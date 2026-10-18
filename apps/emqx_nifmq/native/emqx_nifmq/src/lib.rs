//! NifMQ prototype: QoS 0 local fan-out through native per-subscriber queues.
//!
//! A published message is copied once into a process-independent environment (`Msg`).
//! Each subscriber owns a bounded queue (`Queue`) that holds references to shared `Msg`
//! objects. A producer notifies the queue owner only on the transition from "not notified"
//! to "notified", so wakeups coalesce. The owner drains its queue in batches.
//!
//! Notification state machine. The fields `buf`, `notified` and `congested` of one queue
//! change only under the queue mutex. Invariant: when `congested` is false and `buf` is not
//! empty, `notified` is true, and the owner has an outstanding drain obligation: either a
//! `nifmq_ready` message in its mailbox, or a `more` result from `drain*`/`set_congested`.
//!
//! - push (producer): append; when `!notified && !congested`, set `notified` and send
//!   `nifmq_ready` after the mutex is released.
//! - drain (owner): pop up to `max`; when `buf` becomes empty, clear `notified` and return
//!   `more = false`; otherwise keep `notified` and return `more = true`.
//! - set_congested(true): set `congested`. Producers keep queueing but send nothing.
//! - set_congested(false): clear `congested`; when `buf` is not empty, set `notified` and
//!   return `more`, else clear `notified` and return `ok`.
//!
//! A notification that arrives after its message was drained by an earlier drain is spurious
//! and harmless: the drain returns an empty batch.

use rustler::sys::{
    enif_alloc_env, enif_consume_timeslice, enif_free_env, enif_get_tuple, enif_inspect_binary,
    enif_make_copy, ErlNifBinary, ErlNifEnv, ERL_NIF_TERM,
};
use rustler::{
    Binary, Encoder, Env, Error, LocalPid, Monitor, NifResult, Resource, ResourceArc, Term,
};
use std::collections::{HashMap, VecDeque};
use std::mem::MaybeUninit;
use std::os::raw::c_int;
use std::sync::atomic::{AtomicI64, AtomicU64, Ordering};
use std::sync::{Arc, Mutex, OnceLock, RwLock};

mod atoms {
    rustler::atoms! {
        ok,
        more,
        continue_ = "continue",
        nifmq_ready,
        published,
        enqueued,
        dropped,
        notifications,
        drains,
        drained,
        live_msgs,
        live_entries,
        queues,
        queue_bytes,
        topics,
    }
}

/// Number of queues served by one `publish`/`publish_cont` call.
const CHUNK: usize = 256;
/// Upper bound of one drain batch.
const MAX_DRAIN: usize = 1000;
/// Position of `topic` and `payload` in the `#message{}` tuple (0-based, tag at 0).
const MSG_TOPIC_POS: usize = 6;
const MSG_PAYLOAD_POS: usize = 7;
const MSG_ARITY: usize = 10;
const ENTRY_BYTES: usize = std::mem::size_of::<ResourceArc<Msg>>();

struct Stats {
    published: AtomicU64,
    enqueued: AtomicU64,
    dropped: AtomicU64,
    notifications: AtomicU64,
    drains: AtomicU64,
    drained: AtomicU64,
    live_msgs: AtomicI64,
    live_entries: AtomicI64,
    queues: AtomicI64,
    queue_bytes: AtomicI64,
}

static STATS: Stats = Stats {
    published: AtomicU64::new(0),
    enqueued: AtomicU64::new(0),
    dropped: AtomicU64::new(0),
    notifications: AtomicU64::new(0),
    drains: AtomicU64::new(0),
    drained: AtomicU64::new(0),
    live_msgs: AtomicI64::new(0),
    live_entries: AtomicI64::new(0),
    queues: AtomicI64::new(0),
    queue_bytes: AtomicI64::new(0),
};

type Subs = Arc<Vec<ResourceArc<Queue>>>;

fn registry() -> &'static RwLock<HashMap<Vec<u8>, Subs>> {
    static REG: OnceLock<RwLock<HashMap<Vec<u8>, Subs>>> = OnceLock::new();
    REG.get_or_init(|| RwLock::new(HashMap::new()))
}

/// One published message, stored once.
pub struct Msg {
    env: *mut ErlNifEnv,
    term: ERL_NIF_TERM,
    topic: (*const u8, usize),
    payload: (*const u8, usize),
}

// The environment is only read after construction, and freed once in `drop`.
unsafe impl Send for Msg {}
unsafe impl Sync for Msg {}

impl Drop for Msg {
    fn drop(&mut self) {
        unsafe { enif_free_env(self.env) };
        STATS.live_msgs.fetch_sub(1, Ordering::Relaxed);
    }
}

#[rustler::resource_impl]
impl Resource for Msg {}

impl Msg {
    fn topic(&self) -> &[u8] {
        unsafe { std::slice::from_raw_parts(self.topic.0, self.topic.1) }
    }
    fn payload(&self) -> &[u8] {
        unsafe { std::slice::from_raw_parts(self.payload.0, self.payload.1) }
    }
}

fn inspect_bin(env: *mut ErlNifEnv, term: ERL_NIF_TERM) -> Option<(*const u8, usize)> {
    let mut bin = MaybeUninit::<ErlNifBinary>::zeroed();
    if unsafe { enif_inspect_binary(env, term, bin.as_mut_ptr()) } == 0 {
        return None;
    }
    let bin = unsafe { bin.assume_init() };
    Some((bin.data as *const u8, bin.size))
}

fn store_msg(msg: Term) -> NifResult<ResourceArc<Msg>> {
    let env = unsafe { enif_alloc_env() };
    let term = unsafe { enif_make_copy(env, msg.as_c_arg()) };
    let mut arity: c_int = 0;
    let mut elems: *const ERL_NIF_TERM = std::ptr::null();
    let ok = unsafe { enif_get_tuple(env, term, &mut arity, &mut elems) } != 0;
    let parts = if ok && arity as usize == MSG_ARITY {
        let elems = unsafe { std::slice::from_raw_parts(elems, MSG_ARITY) };
        inspect_bin(env, elems[MSG_TOPIC_POS]).zip(inspect_bin(env, elems[MSG_PAYLOAD_POS]))
    } else {
        None
    };
    match parts {
        Some((topic, payload)) => {
            STATS.live_msgs.fetch_add(1, Ordering::Relaxed);
            Ok(ResourceArc::new(Msg {
                env,
                term,
                topic,
                payload,
            }))
        }
        None => {
            unsafe { enif_free_env(env) };
            Err(Error::BadArg)
        }
    }
}

struct QInner {
    buf: VecDeque<ResourceArc<Msg>>,
    notified: bool,
    congested: bool,
    closed: bool,
    topics: Vec<Vec<u8>>,
    dropped: u64,
}

/// One subscriber queue. The owner is the process that created it.
pub struct Queue {
    owner: LocalPid,
    cap: usize,
    inner: Mutex<QInner>,
}

#[rustler::resource_impl]
impl Resource for Queue {
    const IMPLEMENTS_DOWN: bool = true;

    fn down<'a>(&'a self, _env: Env<'a>, _pid: LocalPid, _mon: Monitor) {
        self.close();
    }
}

impl Drop for Queue {
    fn drop(&mut self) {
        STATS.queues.fetch_sub(1, Ordering::Relaxed);
        let cap = self.inner.get_mut().map(|i| i.buf.capacity()).unwrap_or(0);
        STATS
            .queue_bytes
            .fetch_sub((cap * ENTRY_BYTES) as i64, Ordering::Relaxed);
    }
}

impl Queue {
    fn close(&self) {
        let (topics, freed) = {
            let mut inner = self.inner.lock().unwrap();
            inner.closed = true;
            let freed = inner.buf.len();
            inner.buf.clear();
            (std::mem::take(&mut inner.topics), freed)
        };
        STATS
            .live_entries
            .fetch_sub(freed as i64, Ordering::Relaxed);
        let mut reg = registry().write().unwrap();
        for t in topics {
            remove_sub(&mut reg, &t, self);
        }
    }
}

enum Push {
    Queued,
    QueuedNotify,
    Dropped,
}

fn push(q: &Queue, msg: &ResourceArc<Msg>) -> Push {
    let mut inner = q.inner.lock().unwrap();
    if inner.closed || inner.buf.len() >= q.cap {
        inner.dropped += 1;
        return Push::Dropped;
    }
    let cap0 = inner.buf.capacity();
    inner.buf.push_back(msg.clone());
    let cap1 = inner.buf.capacity();
    if cap1 != cap0 {
        STATS
            .queue_bytes
            .fetch_add(((cap1 - cap0) * ENTRY_BYTES) as i64, Ordering::Relaxed);
    }
    if !inner.notified && !inner.congested {
        inner.notified = true;
        Push::QueuedNotify
    } else {
        Push::Queued
    }
}

fn remove_sub(reg: &mut HashMap<Vec<u8>, Subs>, topic: &[u8], q: &Queue) {
    if let Some(subs) = reg.get(topic) {
        let rest: Vec<ResourceArc<Queue>> = subs
            .iter()
            .filter(|s| !std::ptr::eq(&***s, q))
            .cloned()
            .collect();
        if rest.is_empty() {
            reg.remove(topic);
        } else {
            reg.insert(topic.to_vec(), Arc::new(rest));
        }
    }
}

/// Continuation of a fan-out that exceeds `CHUNK` subscribers.
pub struct Fanout {
    msg: ResourceArc<Msg>,
    subs: Subs,
    next: Mutex<usize>,
}

#[rustler::resource_impl]
impl Resource for Fanout {}

/// Enqueue `msg` to `subs[from..]`, at most `CHUNK` queues. Return the next index.
fn fan_chunk(env: Env, msg: &ResourceArc<Msg>, subs: &Subs, from: usize) -> (usize, u64) {
    let to = usize::min(from + CHUNK, subs.len());
    let mut n_enq = 0u64;
    let mut n_drop = 0u64;
    let mut n_notify = 0u64;
    for q in &subs[from..to] {
        match push(q, msg) {
            Push::Queued => n_enq += 1,
            Push::QueuedNotify => {
                n_enq += 1;
                n_notify += 1;
                let _ = env.send(&q.owner, atoms::nifmq_ready());
            }
            Push::Dropped => n_drop += 1,
        }
    }
    STATS.enqueued.fetch_add(n_enq, Ordering::Relaxed);
    STATS
        .live_entries
        .fetch_add(n_enq as i64, Ordering::Relaxed);
    STATS.notifications.fetch_add(n_notify, Ordering::Relaxed);
    if n_drop > 0 {
        STATS.dropped.fetch_add(n_drop, Ordering::Relaxed);
    }
    // One chunk of CHUNK pushes and sends is well below 10% of a 1 ms timeslice.
    let pct = ((to - from) * 10 / CHUNK).max(1) as c_int;
    unsafe { enif_consume_timeslice(env.as_c_arg(), pct) };
    (to, n_enq)
}

#[rustler::nif]
fn new_queue(env: Env, cap: usize) -> NifResult<ResourceArc<Queue>> {
    let owner = env.pid();
    let q = ResourceArc::new(Queue {
        owner,
        cap,
        inner: Mutex::new(QInner {
            buf: VecDeque::new(),
            notified: false,
            congested: false,
            closed: false,
            topics: Vec::new(),
            dropped: 0,
        }),
    });
    STATS.queues.fetch_add(1, Ordering::Relaxed);
    if q.monitor(Some(env), &owner).is_none() {
        return Err(Error::BadArg);
    }
    Ok(q)
}

#[rustler::nif]
fn subscribe(q: ResourceArc<Queue>, topic: Binary) -> NifResult<rustler::Atom> {
    let topic = topic.as_slice().to_vec();
    {
        let mut inner = q.inner.lock().unwrap();
        if inner.closed {
            return Err(Error::BadArg);
        }
        if inner.topics.contains(&topic) {
            return Ok(atoms::ok());
        }
        inner.topics.push(topic.clone());
    }
    let mut reg = registry().write().unwrap();
    let mut subs: Vec<ResourceArc<Queue>> = reg
        .get(&topic)
        .map(|s| s.as_ref().clone())
        .unwrap_or_default();
    subs.push(q.clone());
    reg.insert(topic, Arc::new(subs));
    Ok(atoms::ok())
}

#[rustler::nif]
fn unsubscribe(q: ResourceArc<Queue>, topic: Binary) -> rustler::Atom {
    let topic = topic.as_slice();
    q.inner.lock().unwrap().topics.retain(|t| t != topic);
    let mut reg = registry().write().unwrap();
    remove_sub(&mut reg, topic, &q);
    atoms::ok()
}

/// Store the `#message{}` once and enqueue it to the subscribers of `topic`.
/// Return `{ok, Enqueued}` or `{continue, Fanout, Enqueued}`.
#[rustler::nif]
fn publish<'a>(env: Env<'a>, topic: Binary<'a>, msg: Term<'a>) -> NifResult<Term<'a>> {
    let subs = match registry().read().unwrap().get(topic.as_slice()) {
        Some(s) => s.clone(),
        None => return Ok((atoms::ok(), 0u64).encode(env)),
    };
    let msg = store_msg(msg)?;
    STATS.published.fetch_add(1, Ordering::Relaxed);
    let (next, n) = fan_chunk(env, &msg, &subs, 0);
    if next < subs.len() {
        let cont = ResourceArc::new(Fanout {
            msg,
            subs,
            next: Mutex::new(next),
        });
        Ok((atoms::continue_(), cont, n).encode(env))
    } else {
        Ok((atoms::ok(), n).encode(env))
    }
}

#[rustler::nif]
fn publish_cont<'a>(env: Env<'a>, cont: ResourceArc<Fanout>) -> Term<'a> {
    let mut next = cont.next.lock().unwrap();
    let (to, n) = fan_chunk(env, &cont.msg, &cont.subs, *next);
    *next = to;
    if to < cont.subs.len() {
        (atoms::continue_(), cont.clone(), n).encode(env)
    } else {
        (atoms::ok(), n).encode(env)
    }
}

/// Pop up to `max` messages. Return the batch and whether the queue still has messages.
fn take(q: &Queue, max: usize) -> (Vec<ResourceArc<Msg>>, bool) {
    let max = max.min(MAX_DRAIN);
    let mut inner = q.inner.lock().unwrap();
    let n = max.min(inner.buf.len());
    let batch: Vec<ResourceArc<Msg>> = inner.buf.drain(..n).collect();
    let more = !inner.buf.is_empty();
    if !more {
        inner.notified = false;
    }
    drop(inner);
    STATS.drains.fetch_add(1, Ordering::Relaxed);
    STATS.drained.fetch_add(n as u64, Ordering::Relaxed);
    STATS.live_entries.fetch_sub(n as i64, Ordering::Relaxed);
    (batch, more)
}

/// Report the drain work to the scheduler, so a process that drains in a loop is
/// preempted like one that runs Erlang code. `per_pct` is the number of messages that
/// cost about 1% of a 1 ms timeslice.
fn consume(env: Env, n: usize, per_pct: usize) {
    let pct = (1 + n / per_pct).min(100) as c_int;
    unsafe { enif_consume_timeslice(env.as_c_arg(), pct) };
}

/// Drain as full `#message{}` terms. Each term is copied into the caller heap.
#[rustler::nif]
fn drain<'a>(env: Env<'a>, q: ResourceArc<Queue>, max: usize) -> Term<'a> {
    let (batch, more) = take(&q, max);
    consume(env, batch.len(), 20);
    let terms: Vec<Term<'a>> = batch
        .iter()
        .map(|m| unsafe { Term::new(env, enif_make_copy(env.as_c_arg(), m.term)) })
        .collect();
    (terms, more).encode(env)
}

/// Drain as `{Topic, Payload}` pairs. Both are resource binaries that point into the
/// stored message; nothing is copied.
#[rustler::nif]
fn drain_view<'a>(env: Env<'a>, q: ResourceArc<Queue>, max: usize) -> Term<'a> {
    let (batch, more) = take(&q, max);
    consume(env, batch.len(), 50);
    let terms: Vec<Term<'a>> = batch
        .iter()
        .map(|m| {
            let topic = unsafe { m.make_binary_unsafe(env, |x| x.topic()) };
            let payload = unsafe { m.make_binary_unsafe(env, |x| x.payload()) };
            (topic, payload).encode(env)
        })
        .collect();
    (terms, more).encode(env)
}

#[rustler::nif]
fn set_congested(q: ResourceArc<Queue>, congested: bool) -> rustler::Atom {
    let mut inner = q.inner.lock().unwrap();
    inner.congested = congested;
    if congested {
        return atoms::ok();
    }
    if inner.buf.is_empty() {
        inner.notified = false;
        atoms::ok()
    } else {
        inner.notified = true;
        atoms::more()
    }
}

#[rustler::nif]
fn queue_info(q: ResourceArc<Queue>) -> (usize, bool, bool, u64) {
    let inner = q.inner.lock().unwrap();
    (
        inner.buf.len(),
        inner.notified,
        inner.congested,
        inner.dropped,
    )
}

#[rustler::nif]
fn info(env: Env) -> NifResult<Term> {
    let r = |c: &AtomicU64| c.load(Ordering::Relaxed);
    let s = |c: &AtomicI64| c.load(Ordering::Relaxed);
    let n_topics = registry().read().unwrap().len();
    let pairs: Vec<(Term, Term)> = vec![
        (
            atoms::published().encode(env),
            r(&STATS.published).encode(env),
        ),
        (
            atoms::enqueued().encode(env),
            r(&STATS.enqueued).encode(env),
        ),
        (atoms::dropped().encode(env), r(&STATS.dropped).encode(env)),
        (
            atoms::notifications().encode(env),
            r(&STATS.notifications).encode(env),
        ),
        (atoms::drains().encode(env), r(&STATS.drains).encode(env)),
        (atoms::drained().encode(env), r(&STATS.drained).encode(env)),
        (
            atoms::live_msgs().encode(env),
            s(&STATS.live_msgs).encode(env),
        ),
        (
            atoms::live_entries().encode(env),
            s(&STATS.live_entries).encode(env),
        ),
        (atoms::queues().encode(env), s(&STATS.queues).encode(env)),
        (
            atoms::queue_bytes().encode(env),
            s(&STATS.queue_bytes).encode(env),
        ),
        (atoms::topics().encode(env), n_topics.encode(env)),
    ];
    Term::map_from_pairs(env, &pairs)
}

rustler::init!("emqx_nifmq_nif");
