//! Native socket writer with a QoS 0/1 outgoing session (prototype).
//!
//! A `Conn` owns a `dup()` copy of a connected TCP socket. Every outgoing byte of the
//! connection goes through its output queue `out`, so packets never interleave. A dispatch
//! encodes one MQTT PUBLISH per subscriber and appends it to the subscriber's `out`. The
//! payload is shared: an `Out::Pub` item points into the stored message.
//!
//! Who calls `writev` depends on the mode:
//! - `inline`: the caller of dispatch, puback or send flushes `out` in its own call.
//! - `threads`: the caller marks the connection ready, and a poller thread flushes it.
//!
//! When the socket buffer is full (`EAGAIN`), the connection is armed for `EPOLLOUT` on
//! its poller thread, which flushes it when the socket becomes writable. While armed, no
//! other caller writes. All fields of one connection change only under its mutex.
//! Lock order: connection mutex, then a poller's `armed` map. A poller thread never holds
//! the `armed` map while it takes a connection mutex.
//!
//! With `conn_read_native/1`, the poller thread also reads the socket. It handles each
//! PUBACK in place (free the window slot, refill, write) and sends all other bytes to the
//! owner as `{nifmq_data, Conn, Bytes}`. At end of stream or on a read error it sends
//! `{nifmq_closed, Conn, Reason}`. The reader uses a second `dup()` copy of the socket,
//! registered for `EPOLLIN` on the same poller.

use rustler::sys::enif_consume_timeslice;
use rustler::{
    Binary, Encoder, Env, Error, LocalPid, Monitor, NifResult, OwnedBinary, OwnedEnv, Resource,
    ResourceArc, Term,
};
use std::collections::{HashMap, VecDeque};
use std::os::raw::c_int;
use std::sync::atomic::{AtomicU64, AtomicUsize, Ordering};
use std::sync::{Arc, Mutex, MutexGuard, OnceLock, RwLock};
use std::time::Instant;

mod atoms {
    rustler::atoms! {
        ok,
        error,
        continue_ = "continue",
        inline,
        threads,
        not_found,
        closed,
        packets,
        bytes,
        writev_calls,
        eagain,
        dropped_qos0,
        dropped_qos1,
        dropped_size,
        dropped_nl,
        queued_qos1,
        thread_flushes,
        conns,
        inflight,
        mqueue,
        out_bytes,
        armed,
        reads,
        native_pubacks,
        unknown_pubacks,
        nifmq_data,
        nifmq_closed,
        eof,
        malformed,
        read_error,
        native_read,
    }
}

const MAX_POLLERS: usize = 4;
const MAX_IOV: usize = 256;
const CHUNK_INLINE: usize = 64;
const CHUNK_THREADS: usize = 256;
const EVFD_TOKEN: u64 = u64::MAX;
/// Set in the epoll token of a reader fd.
const READ_BIT: u64 = 1 << 32;
const READ_BUF: usize = 64 * 1024;

struct Stats {
    packets: AtomicU64,
    bytes: AtomicU64,
    writev_calls: AtomicU64,
    eagain: AtomicU64,
    dropped_qos0: AtomicU64,
    dropped_qos1: AtomicU64,
    dropped_size: AtomicU64,
    dropped_nl: AtomicU64,
    queued_qos1: AtomicU64,
    thread_flushes: AtomicU64,
    conns: AtomicU64,
    reads: AtomicU64,
    native_pubacks: AtomicU64,
    unknown_pubacks: AtomicU64,
}

static STATS: Stats = Stats {
    packets: AtomicU64::new(0),
    bytes: AtomicU64::new(0),
    writev_calls: AtomicU64::new(0),
    eagain: AtomicU64::new(0),
    dropped_qos0: AtomicU64::new(0),
    dropped_qos1: AtomicU64::new(0),
    dropped_size: AtomicU64::new(0),
    dropped_nl: AtomicU64::new(0),
    queued_qos1: AtomicU64::new(0),
    thread_flushes: AtomicU64::new(0),
    conns: AtomicU64::new(0),
    reads: AtomicU64::new(0),
    native_pubacks: AtomicU64::new(0),
    unknown_pubacks: AtomicU64::new(0),
};

/// 0 = inline, otherwise the number of active poller threads that flush.
static MODE_THREADS: AtomicUsize = AtomicUsize::new(0);
static CONN_SEQ: AtomicUsize = AtomicUsize::new(0);

fn inc(c: &AtomicU64, n: u64) {
    c.fetch_add(n, Ordering::Relaxed);
}

/// A published message, stored once and shared by all its deliveries.
pub struct MsgData {
    topic: Vec<u8>,
    payload: Vec<u8>,
    /// MQTT 5 properties, encoded without the length prefix and without
    /// Subscription-Identifier.
    props: Vec<u8>,
    from: Vec<u8>,
    qos: u8,
    retain: bool,
    /// The message comes from the retained store, so the retain flag is always kept.
    retained: bool,
}

type MsgRef = Arc<MsgData>;

#[derive(Clone, Copy)]
struct SubOpts {
    qos: u8,
    nl: bool,
    rap: bool,
    subid: u32,
}

/// Per-delivery choices that the encoder needs besides the packet identifier.
#[derive(Clone, Copy)]
struct PubOpts {
    qos: u8,
    retain: bool,
    subid: u32,
}

struct Pending {
    msg: MsgRef,
    po: PubOpts,
}

enum Out {
    Raw(Vec<u8>),
    /// head = fixed header, remaining length, topic length; mid = packet identifier,
    /// property length, Subscription-Identifier.
    Pub {
        head: [u8; 8],
        head_len: u8,
        mid: [u8; 12],
        mid_len: u8,
        topic_off: u32,
        /// MQTT 5 only: include the stored properties.
        props: bool,
        msg: MsgRef,
    },
}

impl Out {
    fn slices(&self) -> [&[u8]; 5] {
        match self {
            Out::Raw(b) => [b, &[], &[], &[], &[]],
            Out::Pub {
                head,
                head_len,
                mid,
                mid_len,
                topic_off,
                props,
                msg,
            } => [
                &head[..*head_len as usize],
                &msg.topic[*topic_off as usize..],
                &mid[..*mid_len as usize],
                if *props { &msg.props } else { &[] },
                &msg.payload,
            ],
        }
    }

    fn len(&self) -> usize {
        self.slices().iter().map(|s| s.len()).sum()
    }
}

fn varint_len(mut n: usize) -> usize {
    let mut l = 1;
    while n >= 128 {
        n >>= 7;
        l += 1;
    }
    l
}

fn put_varint(buf: &mut [u8], mut at: usize, mut n: usize) -> usize {
    loop {
        let b = (n % 128) as u8;
        n /= 128;
        if n > 0 {
            buf[at] = b | 0x80;
            at += 1;
        } else {
            buf[at] = b;
            return at + 1;
        }
    }
}

struct ConnOpts {
    proto_ver: u8,
    clientid: Vec<u8>,
    mountpoint: Vec<u8>,
    max_packet_size: usize,
    window: usize,
    max_mqueue: usize,
    max_buf: usize,
}

fn encode(opts: &ConnOpts, msg: &MsgRef, po: PubOpts, pkt_id: u16) -> Option<Out> {
    let topic_off = if !opts.mountpoint.is_empty() && msg.topic.starts_with(&opts.mountpoint) {
        opts.mountpoint.len()
    } else {
        0
    };
    let tlen = msg.topic.len() - topic_off;
    let mut mid = [0u8; 12];
    let mut m = 0;
    if po.qos > 0 {
        mid[0] = (pkt_id >> 8) as u8;
        mid[1] = pkt_id as u8;
        m = 2;
    }
    let mut props_total = 0;
    if opts.proto_ver == 5 {
        let subid_len = if po.subid > 0 {
            1 + varint_len(po.subid as usize)
        } else {
            0
        };
        let plen = msg.props.len() + subid_len;
        m = put_varint(&mut mid, m, plen);
        if po.subid > 0 {
            mid[m] = 0x0B;
            m = put_varint(&mut mid, m + 1, po.subid as usize);
        }
        props_total = msg.props.len();
    }
    let rl = 2 + tlen + m + props_total + msg.payload.len();
    if 1 + varint_len(rl) + rl > opts.max_packet_size {
        return None;
    }
    let mut head = [0u8; 8];
    head[0] = 0x30 | (po.qos << 1) | (po.retain as u8);
    let mut h = put_varint(&mut head, 1, rl);
    head[h] = (tlen >> 8) as u8;
    head[h + 1] = tlen as u8;
    h += 2;
    Some(Out::Pub {
        head,
        head_len: h as u8,
        mid,
        mid_len: m as u8,
        topic_off: topic_off as u32,
        props: opts.proto_ver == 5,
        msg: msg.clone(),
    })
}

struct ConnInner {
    fd: c_int,
    closed: bool,
    out: VecDeque<Out>,
    front_off: usize,
    out_bytes: usize,
    /// Registered for EPOLLOUT on the poller. Only the poller writes while armed.
    armed: bool,
    /// In the ready list of the poller (threads mode).
    queued: bool,
    /// Outgoing QoS 1 window in send order; `None` marks an acknowledged hole.
    inflight: VecDeque<(u16, Option<MsgRef>)>,
    inflight_cnt: usize,
    next_id: u16,
    mqueue: VecDeque<Pending>,
    filters: Vec<Vec<u8>>,
    /// Reader copy of the socket, or -1 when the owner reads.
    rfd: c_int,
    /// Received bytes that do not make a full packet yet.
    rbuf: Vec<u8>,
}

pub struct Conn {
    owner: LocalPid,
    opts: ConnOpts,
    poller: usize,
    inner: Mutex<ConnInner>,
}

#[rustler::resource_impl]
impl Resource for Conn {
    const IMPLEMENTS_DOWN: bool = true;

    fn down<'a>(&'a self, _env: Env<'a>, _pid: LocalPid, _mon: Monitor) {
        self.close();
    }
}

impl Drop for Conn {
    fn drop(&mut self) {
        if let Ok(inner) = self.inner.get_mut() {
            if inner.fd >= 0 {
                unsafe { libc::close(inner.fd) };
                inner.fd = -1;
            }
            if inner.rfd >= 0 {
                unsafe { libc::close(inner.rfd) };
                inner.rfd = -1;
            }
        }
        STATS.conns.fetch_sub(1, Ordering::Relaxed);
    }
}

enum Flush {
    Done,
    Again,
    Failed,
}

impl Conn {
    fn close(&self) {
        let filters = {
            let mut inner = self.inner.lock().unwrap();
            if inner.armed {
                pollers()[self.poller].disarm(inner.fd);
                inner.armed = false;
            }
            if inner.fd >= 0 {
                unsafe { libc::close(inner.fd) };
                inner.fd = -1;
            }
            Self::stop_reader(self.poller, &mut inner);
            inner.closed = true;
            inner.out.clear();
            inner.out_bytes = 0;
            inner.mqueue.clear();
            inner.inflight.clear();
            inner.inflight_cnt = 0;
            std::mem::take(&mut inner.filters)
        };
        let mut reg = registry().write().unwrap();
        for f in filters {
            remove_sub(&mut reg, &f, self);
        }
    }

    fn push_out(inner: &mut ConnInner, item: Out) {
        inner.out_bytes += item.len();
        inner.out.push_back(item);
    }

    fn next_pkt_id(inner: &mut ConnInner) -> u16 {
        loop {
            let id = inner.next_id;
            inner.next_id = if id == u16::MAX { 1 } else { id + 1 };
            if !inner.inflight.iter().any(|(i, m)| *i == id && m.is_some()) {
                return id;
            }
        }
    }

    /// Send a QoS 1 message now when the window has room, else queue it.
    fn deliver_qos1(&self, inner: &mut ConnInner, msg: &MsgRef, po: PubOpts) -> bool {
        if inner.inflight_cnt < self.opts.window {
            let id = Self::next_pkt_id(inner);
            match encode(&self.opts, msg, po, id) {
                Some(item) => {
                    inner.inflight.push_back((id, Some(msg.clone())));
                    inner.inflight_cnt += 1;
                    Self::push_out(inner, item);
                    true
                }
                None => {
                    inc(&STATS.dropped_size, 1);
                    false
                }
            }
        } else if inner.mqueue.len() < self.opts.max_mqueue {
            inner.mqueue.push_back(Pending {
                msg: msg.clone(),
                po,
            });
            inc(&STATS.queued_qos1, 1);
            true
        } else {
            // Drop the oldest queued message, as emqx_mqueue does by default.
            inner.mqueue.pop_front();
            inner.mqueue.push_back(Pending {
                msg: msg.clone(),
                po,
            });
            inc(&STATS.dropped_qos1, 1);
            true
        }
    }

    /// Deregister and close the reader copy of the socket. The caller holds the mutex.
    fn stop_reader(poller: usize, inner: &mut ConnInner) {
        if inner.rfd >= 0 {
            pollers()[poller].del_reader(inner.rfd);
            unsafe { libc::close(inner.rfd) };
            inner.rfd = -1;
            inner.rbuf = Vec::new();
        }
    }

    /// Free the window slot of `pkt_id` and refill the window from the queue.
    fn ack(&self, inner: &mut ConnInner, pkt_id: u16) -> bool {
        let pos = inner
            .inflight
            .iter()
            .position(|(i, m)| *i == pkt_id && m.is_some());
        let Some(pos) = pos else {
            return false;
        };
        inner.inflight[pos].1 = None;
        inner.inflight_cnt -= 1;
        while matches!(inner.inflight.front(), Some((_, None))) {
            inner.inflight.pop_front();
        }
        self.refill(inner);
        true
    }

    fn refill(&self, inner: &mut ConnInner) {
        while inner.inflight_cnt < self.opts.window {
            match inner.mqueue.pop_front() {
                Some(p) => {
                    self.deliver_qos1(inner, &p.msg, p.po);
                }
                None => break,
            }
        }
    }

    /// Write `out` until it is empty or the socket buffer is full.
    fn flush_locked(inner: &mut ConnInner) -> Flush {
        loop {
            if inner.out.is_empty() {
                return Flush::Done;
            }
            let mut iov: [libc::iovec; MAX_IOV] = unsafe { std::mem::zeroed() };
            let mut n_iov = 0;
            let mut want = 0usize;
            let mut skip = inner.front_off;
            'items: for item in inner.out.iter() {
                for s in item.slices() {
                    if s.is_empty() {
                        continue;
                    }
                    let s = if skip >= s.len() {
                        skip -= s.len();
                        continue;
                    } else {
                        let r = &s[skip..];
                        skip = 0;
                        r
                    };
                    if n_iov == MAX_IOV {
                        break 'items;
                    }
                    iov[n_iov] = libc::iovec {
                        iov_base: s.as_ptr() as *mut libc::c_void,
                        iov_len: s.len(),
                    };
                    n_iov += 1;
                    want += s.len();
                }
            }
            let n = unsafe { libc::writev(inner.fd, iov.as_ptr(), n_iov as c_int) };
            inc(&STATS.writev_calls, 1);
            if n < 0 {
                match std::io::Error::last_os_error().raw_os_error() {
                    Some(libc::EAGAIN) => {
                        inc(&STATS.eagain, 1);
                        return Flush::Again;
                    }
                    Some(libc::EINTR) => continue,
                    _ => {
                        inner.out.clear();
                        inner.out_bytes = 0;
                        inner.front_off = 0;
                        return Flush::Failed;
                    }
                }
            }
            let mut left = n as usize;
            inc(&STATS.bytes, left as u64);
            inner.out_bytes -= left;
            while left > 0 {
                let front_len = inner.out.front().map(|i| i.len()).unwrap_or(0) - inner.front_off;
                if left >= front_len {
                    left -= front_len;
                    inner.out.pop_front();
                    inner.front_off = 0;
                    inc(&STATS.packets, 1);
                } else {
                    inner.front_off += left;
                    left = 0;
                }
            }
            if (n as usize) < want {
                return Flush::Again;
            }
        }
    }
}

/// Make sure the queued bytes get written, by this caller or by the poller.
/// Return `true` when the connection must be pushed to its poller's ready list.
fn kick(conn: &ResourceArc<Conn>, inner: &mut MutexGuard<ConnInner>) -> bool {
    if inner.closed || inner.armed || inner.out.is_empty() {
        return false;
    }
    if MODE_THREADS.load(Ordering::Relaxed) == 0 {
        let r = Conn::flush_locked(inner);
        after_flush(conn, inner, r);
        false
    } else if !inner.queued {
        inner.queued = true;
        true
    } else {
        false
    }
}

/// Arm the connection when the socket is full; stop writing when it failed. The
/// owner sees the failure on its read side and closes the connection.
fn after_flush(conn: &ResourceArc<Conn>, inner: &mut ConnInner, r: Flush) {
    match r {
        Flush::Done => {}
        Flush::Again => {
            inner.armed = true;
            pollers()[conn.poller].arm(conn.clone(), inner.fd);
        }
        Flush::Failed => inner.closed = true,
    }
}

type Subs = Arc<Vec<(ResourceArc<Conn>, SubOpts)>>;

fn registry() -> &'static RwLock<HashMap<Vec<u8>, Subs>> {
    static REG: OnceLock<RwLock<HashMap<Vec<u8>, Subs>>> = OnceLock::new();
    REG.get_or_init(|| RwLock::new(HashMap::new()))
}

fn remove_sub(reg: &mut HashMap<Vec<u8>, Subs>, filter: &[u8], c: &Conn) {
    if let Some(subs) = reg.get(filter) {
        let rest: Vec<(ResourceArc<Conn>, SubOpts)> = subs
            .iter()
            .filter(|(s, _)| !std::ptr::eq(&**s, c))
            .cloned()
            .collect();
        if rest.is_empty() {
            reg.remove(filter);
        } else {
            reg.insert(filter.to_vec(), Arc::new(rest));
        }
    }
}

//------------------------------------------------------------------------------
// Poller threads
//------------------------------------------------------------------------------

struct Poller {
    epfd: c_int,
    evfd: c_int,
    ready: Mutex<Vec<ResourceArc<Conn>>>,
    armed: Mutex<HashMap<c_int, ResourceArc<Conn>>>,
    readers: Mutex<HashMap<c_int, ResourceArc<Conn>>>,
}

/// Why the native reader stopped.
enum ReadEnd {
    Eof,
    Malformed,
    Error,
}

/// Split complete packets off `buf`: collect PUBACK packet identifiers in `acks` and copy
/// all other packets to `fwd`. Return the number of bytes consumed.
fn parse_in(buf: &[u8], acks: &mut Vec<u16>, fwd: &mut Vec<u8>) -> Result<usize, ReadEnd> {
    let mut at = 0;
    loop {
        let rest = &buf[at..];
        if rest.len() < 2 {
            return Ok(at);
        }
        let mut len = 0usize;
        let mut mult = 1usize;
        let mut i = 1;
        loop {
            if i > 4 {
                return Err(ReadEnd::Malformed);
            }
            let Some(&b) = rest.get(i) else {
                return Ok(at);
            };
            len += (b & 0x7f) as usize * mult;
            mult *= 128;
            i += 1;
            if b & 0x80 == 0 {
                break;
            }
        }
        let total = i + len;
        if rest.len() < total {
            return Ok(at);
        }
        if rest[0] >> 4 == 4 && len >= 2 {
            acks.push(u16::from_be_bytes([rest[i], rest[i + 1]]));
        } else {
            fwd.extend_from_slice(&rest[..total]);
        }
        at += total;
    }
}

impl Poller {
    fn arm(&self, conn: ResourceArc<Conn>, fd: c_int) {
        self.armed.lock().unwrap().insert(fd, conn);
        let mut ev = libc::epoll_event {
            events: (libc::EPOLLOUT | libc::EPOLLONESHOT) as u32,
            u64: fd as u64,
        };
        let r = unsafe { libc::epoll_ctl(self.epfd, libc::EPOLL_CTL_ADD, fd, &mut ev) };
        if r < 0 {
            unsafe { libc::epoll_ctl(self.epfd, libc::EPOLL_CTL_MOD, fd, &mut ev) };
        }
    }

    fn add_reader(&self, conn: ResourceArc<Conn>, rfd: c_int) {
        self.readers.lock().unwrap().insert(rfd, conn);
        let mut ev = libc::epoll_event {
            events: libc::EPOLLIN as u32,
            u64: rfd as u64 | READ_BIT,
        };
        unsafe { libc::epoll_ctl(self.epfd, libc::EPOLL_CTL_ADD, rfd, &mut ev) };
    }

    fn del_reader(&self, rfd: c_int) {
        unsafe { libc::epoll_ctl(self.epfd, libc::EPOLL_CTL_DEL, rfd, std::ptr::null_mut()) };
        self.readers.lock().unwrap().remove(&rfd);
    }

    /// Read once from the socket. Handle PUBACKs here; send the rest to the owner.
    fn read_one(&self, conn: &ResourceArc<Conn>, rfd: c_int) {
        let mut acks = Vec::new();
        let mut fwd = Vec::new();
        let end = {
            let mut inner = conn.inner.lock().unwrap();
            if inner.closed || inner.rfd != rfd {
                return;
            }
            let old = inner.rbuf.len();
            inner.rbuf.resize(old + READ_BUF, 0);
            let n = unsafe {
                libc::read(
                    rfd,
                    inner.rbuf[old..].as_mut_ptr() as *mut libc::c_void,
                    READ_BUF,
                )
            };
            inc(&STATS.reads, 1);
            let end = if n < 0 {
                inner.rbuf.truncate(old);
                match std::io::Error::last_os_error().raw_os_error() {
                    Some(libc::EAGAIN) | Some(libc::EINTR) => return,
                    _ => Some(ReadEnd::Error),
                }
            } else if n == 0 {
                inner.rbuf.truncate(old);
                Some(ReadEnd::Eof)
            } else {
                inner.rbuf.truncate(old + n as usize);
                match parse_in(&inner.rbuf, &mut acks, &mut fwd) {
                    Ok(used) => {
                        inner.rbuf.drain(..used);
                        None
                    }
                    Err(e) => Some(e),
                }
            };
            for id in &acks {
                if conn.ack(&mut inner, *id) {
                    inc(&STATS.native_pubacks, 1);
                } else {
                    inc(&STATS.unknown_pubacks, 1);
                }
            }
            if !acks.is_empty() && !inner.closed && !inner.armed && !inner.out.is_empty() {
                let r = Conn::flush_locked(&mut inner);
                after_flush(conn, &mut inner, r);
            }
            if end.is_some() {
                Conn::stop_reader(conn.poller, &mut inner);
            }
            end
        };
        let mut env = OwnedEnv::new();
        if !fwd.is_empty() {
            let _ = env.send_and_clear(&conn.owner, |env| {
                let mut b = OwnedBinary::new(fwd.len()).expect("alloc");
                b.as_mut_slice().copy_from_slice(&fwd);
                (atoms::nifmq_data(), conn.clone(), b.release(env)).encode(env)
            });
        }
        if let Some(e) = end {
            let reason = match e {
                ReadEnd::Eof => atoms::eof(),
                ReadEnd::Malformed => atoms::malformed(),
                ReadEnd::Error => atoms::read_error(),
            };
            let _ = env.send_and_clear(&conn.owner, |env| {
                (atoms::nifmq_closed(), conn.clone(), reason).encode(env)
            });
        }
    }

    fn disarm(&self, fd: c_int) {
        unsafe { libc::epoll_ctl(self.epfd, libc::EPOLL_CTL_DEL, fd, std::ptr::null_mut()) };
        self.armed.lock().unwrap().remove(&fd);
    }

    fn push(&self, conn: ResourceArc<Conn>) {
        let was_empty = {
            let mut r = self.ready.lock().unwrap();
            r.push(conn);
            r.len() == 1
        };
        if was_empty {
            let one: u64 = 1;
            unsafe { libc::write(self.evfd, &one as *const u64 as *const libc::c_void, 8) };
        }
    }

    fn flush_one(&self, conn: &ResourceArc<Conn>) {
        let mut inner = conn.inner.lock().unwrap();
        if inner.closed || inner.armed {
            return;
        }
        inc(&STATS.thread_flushes, 1);
        let r = Conn::flush_locked(&mut inner);
        after_flush(conn, &mut inner, r);
    }

    fn run(&self) {
        let mut events: [libc::epoll_event; 256] = unsafe { std::mem::zeroed() };
        loop {
            let n = unsafe { libc::epoll_wait(self.epfd, events.as_mut_ptr(), 256, -1) };
            if n < 0 {
                continue;
            }
            for ev in &events[..n as usize] {
                let token = ev.u64;
                if token != EVFD_TOKEN && token & READ_BIT != 0 {
                    let rfd = (token & 0xffff_ffff) as c_int;
                    let conn = self.readers.lock().unwrap().get(&rfd).cloned();
                    if let Some(conn) = conn {
                        self.read_one(&conn, rfd);
                    }
                } else if token == EVFD_TOKEN {
                    let mut buf: u64 = 0;
                    unsafe { libc::read(self.evfd, &mut buf as *mut u64 as *mut libc::c_void, 8) };
                    let batch = std::mem::take(&mut *self.ready.lock().unwrap());
                    for conn in batch {
                        conn.inner.lock().unwrap().queued = false;
                        self.flush_one(&conn);
                    }
                } else {
                    let fd = token as c_int;
                    unsafe {
                        libc::epoll_ctl(self.epfd, libc::EPOLL_CTL_DEL, fd, std::ptr::null_mut())
                    };
                    let conn = self.armed.lock().unwrap().remove(&fd);
                    if let Some(conn) = conn {
                        let mut inner = conn.inner.lock().unwrap();
                        if inner.fd != fd || inner.closed {
                            continue;
                        }
                        inner.armed = false;
                        drop(inner);
                        self.flush_one(&conn);
                    }
                }
            }
        }
    }
}

fn pollers() -> &'static Vec<Poller> {
    static P: OnceLock<Vec<Poller>> = OnceLock::new();
    P.get_or_init(|| {
        (0..MAX_POLLERS)
            .map(|_| {
                let epfd = unsafe { libc::epoll_create1(libc::EPOLL_CLOEXEC) };
                let evfd = unsafe { libc::eventfd(0, libc::EFD_NONBLOCK | libc::EFD_CLOEXEC) };
                let mut ev = libc::epoll_event {
                    events: libc::EPOLLIN as u32,
                    u64: EVFD_TOKEN,
                };
                unsafe { libc::epoll_ctl(epfd, libc::EPOLL_CTL_ADD, evfd, &mut ev) };
                Poller {
                    epfd,
                    evfd,
                    ready: Mutex::new(Vec::new()),
                    armed: Mutex::new(HashMap::new()),
                    readers: Mutex::new(HashMap::new()),
                }
            })
            .collect()
    })
}

fn start_pollers() {
    static STARTED: OnceLock<()> = OnceLock::new();
    STARTED.get_or_init(|| {
        for i in 0..MAX_POLLERS {
            std::thread::Builder::new()
                .name(format!("nifmq_poller_{i}"))
                .spawn(move || pollers()[i].run())
                .expect("spawn poller");
        }
    });
}

//------------------------------------------------------------------------------
// Dispatch
//------------------------------------------------------------------------------

pub struct DispatchCont {
    msg: MsgRef,
    subs: Subs,
    next: Mutex<usize>,
}

#[rustler::resource_impl]
impl Resource for DispatchCont {}

fn deliver_one(conn: &ResourceArc<Conn>, msg: &MsgRef, so: SubOpts) -> (bool, bool) {
    if so.nl && msg.from == conn.opts.clientid {
        inc(&STATS.dropped_nl, 1);
        return (false, false);
    }
    let qos = msg.qos.min(so.qos);
    let po = PubOpts {
        qos,
        retain: msg.retained || (so.rap && msg.retain),
        subid: so.subid,
    };
    let mut inner = conn.inner.lock().unwrap();
    if inner.closed {
        return (false, false);
    }
    let ok = if qos == 0 {
        if inner.out_bytes > conn.opts.max_buf {
            inc(&STATS.dropped_qos0, 1);
            false
        } else {
            match encode(&conn.opts, msg, po, 0) {
                Some(item) => {
                    Conn::push_out(&mut inner, item);
                    true
                }
                None => {
                    inc(&STATS.dropped_size, 1);
                    false
                }
            }
        }
    } else {
        conn.deliver_qos1(&mut inner, msg, po)
    };
    let wake = kick(conn, &mut inner);
    (ok, wake)
}

fn dispatch_chunk(env: Env, msg: &MsgRef, subs: &Subs, from: usize) -> (usize, u64) {
    let threads = MODE_THREADS.load(Ordering::Relaxed);
    let chunk = if threads == 0 {
        CHUNK_INLINE
    } else {
        CHUNK_THREADS
    };
    let to = usize::min(from + chunk, subs.len());
    let t0 = Instant::now();
    let mut n = 0u64;
    for (conn, so) in &subs[from..to] {
        let (ok, wake) = deliver_one(conn, msg, *so);
        if ok {
            n += 1;
        }
        if wake {
            pollers()[conn.poller].push(conn.clone());
        }
    }
    let pct = (t0.elapsed().as_micros() / 10).clamp(1, 100) as c_int;
    unsafe { enif_consume_timeslice(env.as_c_arg(), pct) };
    (to, n)
}

//------------------------------------------------------------------------------
// NIFs
//------------------------------------------------------------------------------

type AttachOpts<'a> = (u8, Binary<'a>, Binary<'a>, usize, usize, usize, usize);

#[rustler::nif]
fn conn_attach(env: Env, fd: c_int, opts: AttachOpts) -> NifResult<ResourceArc<Conn>> {
    let (proto_ver, clientid, mountpoint, max_packet_size, window, max_mqueue, max_buf) = opts;
    start_pollers();
    let dfd = unsafe { libc::dup(fd) };
    if dfd < 0 {
        return Err(Error::BadArg);
    }
    unsafe {
        let fl = libc::fcntl(dfd, libc::F_GETFL);
        libc::fcntl(dfd, libc::F_SETFL, fl | libc::O_NONBLOCK);
        libc::fcntl(dfd, libc::F_SETFD, libc::FD_CLOEXEC);
    }
    let owner = env.pid();
    let seq = CONN_SEQ.fetch_add(1, Ordering::Relaxed);
    let active = MODE_THREADS.load(Ordering::Relaxed).max(1);
    let conn = ResourceArc::new(Conn {
        owner,
        opts: ConnOpts {
            proto_ver,
            clientid: clientid.as_slice().to_vec(),
            mountpoint: mountpoint.as_slice().to_vec(),
            max_packet_size,
            window,
            max_mqueue,
            max_buf,
        },
        poller: seq % active,
        inner: Mutex::new(ConnInner {
            fd: dfd,
            closed: false,
            out: VecDeque::new(),
            front_off: 0,
            out_bytes: 0,
            armed: false,
            queued: false,
            inflight: VecDeque::new(),
            inflight_cnt: 0,
            next_id: 1,
            mqueue: VecDeque::new(),
            filters: Vec::new(),
            rfd: -1,
            rbuf: Vec::new(),
        }),
    });
    inc(&STATS.conns, 1);
    if conn.monitor(Some(env), &conn.owner).is_none() {
        conn.close();
        return Err(Error::BadArg);
    }
    Ok(conn)
}

#[rustler::nif]
fn conn_subscribe(
    conn: ResourceArc<Conn>,
    filter: Binary,
    so: (u8, bool, bool, u32),
) -> NifResult<rustler::Atom> {
    let filter = filter.as_slice().to_vec();
    let so = SubOpts {
        qos: so.0,
        nl: so.1,
        rap: so.2,
        subid: so.3,
    };
    {
        let mut inner = conn.inner.lock().unwrap();
        if inner.closed {
            return Err(Error::BadArg);
        }
        if !inner.filters.contains(&filter) {
            inner.filters.push(filter.clone());
        }
    }
    let mut reg = registry().write().unwrap();
    let mut subs: Vec<(ResourceArc<Conn>, SubOpts)> = reg
        .get(&filter)
        .map(|s| {
            s.iter()
                .filter(|(c, _)| !std::ptr::eq(&**c, &*conn))
                .cloned()
                .collect()
        })
        .unwrap_or_default();
    subs.push((conn.clone(), so));
    reg.insert(filter, Arc::new(subs));
    Ok(atoms::ok())
}

#[rustler::nif]
fn conn_unsubscribe(conn: ResourceArc<Conn>, filter: Binary) -> rustler::Atom {
    let filter = filter.as_slice();
    conn.inner.lock().unwrap().filters.retain(|f| f != filter);
    let mut reg = registry().write().unwrap();
    remove_sub(&mut reg, filter, &conn);
    atoms::ok()
}

type Parts<'a> = (
    Binary<'a>,
    Binary<'a>,
    u8,
    bool,
    bool,
    Binary<'a>,
    Binary<'a>,
);

/// Encode and send (or queue) `parts` to every native subscriber of `filter`.
/// Return `{ok, N}` or `{continue, Cont, N}`.
#[rustler::nif]
fn conn_dispatch<'a>(env: Env<'a>, filter: Binary<'a>, parts: Parts<'a>) -> Term<'a> {
    let subs = match registry().read().unwrap().get(filter.as_slice()) {
        Some(s) => s.clone(),
        None => return (atoms::ok(), 0u64).encode(env),
    };
    let (topic, payload, qos, retain, retained, from, props) = parts;
    let msg = Arc::new(MsgData {
        topic: topic.as_slice().to_vec(),
        payload: payload.as_slice().to_vec(),
        props: props.as_slice().to_vec(),
        from: from.as_slice().to_vec(),
        qos,
        retain,
        retained,
    });
    let (next, n) = dispatch_chunk(env, &msg, &subs, 0);
    if next < subs.len() {
        let cont = ResourceArc::new(DispatchCont {
            msg,
            subs,
            next: Mutex::new(next),
        });
        (atoms::continue_(), cont, n).encode(env)
    } else {
        (atoms::ok(), n).encode(env)
    }
}

#[rustler::nif]
fn conn_dispatch_cont<'a>(env: Env<'a>, cont: ResourceArc<DispatchCont>) -> Term<'a> {
    let mut next = cont.next.lock().unwrap();
    let (to, n) = dispatch_chunk(env, &cont.msg, &cont.subs, *next);
    *next = to;
    if to < cont.subs.len() {
        (atoms::continue_(), cont.clone(), n).encode(env)
    } else {
        (atoms::ok(), n).encode(env)
    }
}

/// Acknowledge a QoS 1 packet identifier and refill the window from the queue.
#[rustler::nif]
fn conn_puback<'a>(env: Env<'a>, conn: ResourceArc<Conn>, pkt_id: u16) -> Term<'a> {
    let mut inner = conn.inner.lock().unwrap();
    if !conn.ack(&mut inner, pkt_id) {
        return (atoms::error(), atoms::not_found()).encode(env);
    }
    let wake = kick(&conn, &mut inner);
    drop(inner);
    if wake {
        pollers()[conn.poller].push(conn.clone());
    }
    atoms::ok().encode(env)
}

/// Queue raw bytes (an already encoded packet) behind the packets queued before.
#[rustler::nif]
fn conn_send<'a>(env: Env<'a>, conn: ResourceArc<Conn>, data: Binary<'a>) -> Term<'a> {
    let mut inner = conn.inner.lock().unwrap();
    if inner.closed {
        return (atoms::error(), atoms::closed()).encode(env);
    }
    Conn::push_out(&mut inner, Out::Raw(data.as_slice().to_vec()));
    let wake = kick(&conn, &mut inner);
    drop(inner);
    if wake {
        pollers()[conn.poller].push(conn.clone());
    }
    atoms::ok().encode(env)
}

/// Let the poller thread read the socket from now on. The owner must stop reading it.
#[rustler::nif]
fn conn_read_native(conn: ResourceArc<Conn>) -> NifResult<rustler::Atom> {
    let mut inner = conn.inner.lock().unwrap();
    if inner.closed || inner.rfd >= 0 {
        return Err(Error::BadArg);
    }
    let rfd = unsafe { libc::dup(inner.fd) };
    if rfd < 0 {
        return Err(Error::BadArg);
    }
    unsafe {
        libc::fcntl(rfd, libc::F_SETFD, libc::FD_CLOEXEC);
    }
    inner.rfd = rfd;
    pollers()[conn.poller].add_reader(conn.clone(), rfd);
    Ok(atoms::ok())
}

#[rustler::nif]
fn conn_close(conn: ResourceArc<Conn>) -> rustler::Atom {
    conn.close();
    atoms::ok()
}

/// Select the writer: `inline` (K ignored) or `threads` with K poller threads (1..4).
/// Affects connections attached afterwards.
#[rustler::nif]
fn conn_set_mode(mode: rustler::Atom, k: usize) -> NifResult<rustler::Atom> {
    if mode == atoms::inline() {
        MODE_THREADS.store(0, Ordering::Relaxed);
    } else if mode == atoms::threads() && (1..=MAX_POLLERS).contains(&k) {
        MODE_THREADS.store(k, Ordering::Relaxed);
    } else {
        return Err(Error::BadArg);
    }
    start_pollers();
    Ok(atoms::ok())
}

#[rustler::nif]
fn conn_info(env: Env, conn: ResourceArc<Conn>) -> NifResult<Term> {
    let inner = conn.inner.lock().unwrap();
    let pairs: Vec<(Term, Term)> = vec![
        (
            atoms::inflight().encode(env),
            inner.inflight_cnt.encode(env),
        ),
        (atoms::mqueue().encode(env), inner.mqueue.len().encode(env)),
        (atoms::out_bytes().encode(env), inner.out_bytes.encode(env)),
        (atoms::armed().encode(env), inner.armed.encode(env)),
        (atoms::closed().encode(env), inner.closed.encode(env)),
        (
            atoms::native_read().encode(env),
            (inner.rfd >= 0).encode(env),
        ),
    ];
    Term::map_from_pairs(env, &pairs)
}

#[rustler::nif]
fn conn_stats(env: Env) -> NifResult<Term> {
    let r = |c: &AtomicU64| c.load(Ordering::Relaxed);
    let pairs: Vec<(Term, Term)> = vec![
        (atoms::packets().encode(env), r(&STATS.packets).encode(env)),
        (atoms::bytes().encode(env), r(&STATS.bytes).encode(env)),
        (
            atoms::writev_calls().encode(env),
            r(&STATS.writev_calls).encode(env),
        ),
        (atoms::eagain().encode(env), r(&STATS.eagain).encode(env)),
        (
            atoms::dropped_qos0().encode(env),
            r(&STATS.dropped_qos0).encode(env),
        ),
        (
            atoms::dropped_qos1().encode(env),
            r(&STATS.dropped_qos1).encode(env),
        ),
        (
            atoms::dropped_size().encode(env),
            r(&STATS.dropped_size).encode(env),
        ),
        (
            atoms::dropped_nl().encode(env),
            r(&STATS.dropped_nl).encode(env),
        ),
        (
            atoms::queued_qos1().encode(env),
            r(&STATS.queued_qos1).encode(env),
        ),
        (
            atoms::thread_flushes().encode(env),
            r(&STATS.thread_flushes).encode(env),
        ),
        (atoms::conns().encode(env), r(&STATS.conns).encode(env)),
        (atoms::reads().encode(env), r(&STATS.reads).encode(env)),
        (
            atoms::native_pubacks().encode(env),
            r(&STATS.native_pubacks).encode(env),
        ),
        (
            atoms::unknown_pubacks().encode(env),
            r(&STATS.unknown_pubacks).encode(env),
        ),
    ];
    Term::map_from_pairs(env, &pairs)
}
