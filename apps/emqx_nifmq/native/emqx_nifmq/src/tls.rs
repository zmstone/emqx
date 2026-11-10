//! TLS server state machine for a connection process (prototype).
//!
//! The NIF does no socket I/O. The connection process feeds the bytes that it reads from the
//! socket, and sends the bytes that the NIF returns. One `Tls` belongs to one connection
//! process, so its mutex is never contended.
//!
//! `tls_feed`:
//! - `{ok, Out, App}`: send `Out`, then parse `App` (decrypted bytes). Both can be empty.
//! - `{handshake_done, Out, App, Info}`: as `ok`, once, when the handshake completes.
//! - `{close, Out, Reason}`: send `Out` (an alert), then close the socket.
//! - `{error, Reason}`: close the socket.
//!
//! A close_notify from the peer is not an error: the data before it is returned with `ok`,
//! and the socket sees end of stream next.

use rustler::{Binary, Encoder, Env, NifResult, OwnedBinary, Resource, ResourceArc, Term};
use rustls::pki_types::pem::PemObject;
use rustls::pki_types::{CertificateDer, PrivateKeyDer};
use rustls::server::WebPkiClientVerifier;
use rustls::{RootCertStore, ServerConfig, ServerConnection};
use std::io::{Read, Write};
use std::sync::{Arc, Mutex};

mod atoms {
    rustler::atoms! {
        ok,
        error,
        close,
        handshake_done,
        certfile,
        keyfile,
        cacertfile,
        verify,
        verify_peer,
        fail_if_no_peer_cert,
        version,
        cipher,
        sni,
        alpn,
        peercert,
        undefined,
    }
}

pub struct TlsConf {
    config: Arc<ServerConfig>,
}

#[rustler::resource_impl]
impl Resource for TlsConf {}

pub struct Tls {
    conn: Mutex<ServerConnection>,
}

#[rustler::resource_impl]
impl Resource for Tls {}

fn get_path(opts: Term, key: rustler::Atom) -> NifResult<Option<String>> {
    match opts.map_get(key) {
        // A binary or a charlist.
        Ok(v) => {
            let b = Binary::from_iolist(v)?;
            let s = std::str::from_utf8(b.as_slice()).map_err(|_| rustler::Error::BadArg)?;
            Ok(Some(s.to_string()))
        }
        Err(_) => Ok(None),
    }
}

fn reason<'a>(env: Env<'a>, s: String) -> Term<'a> {
    s.encode(env)
}

fn build_config(opts: Term) -> Result<ServerConfig, String> {
    let need = |k| -> Result<String, String> {
        get_path(opts, k)
            .map_err(|_| "bad option".to_string())?
            .ok_or_else(|| "missing certfile or keyfile".to_string())
    };
    let certfile = need(atoms::certfile())?;
    let keyfile = need(atoms::keyfile())?;
    let certs = CertificateDer::pem_file_iter(&certfile)
        .and_then(|it| it.collect::<Result<Vec<_>, _>>())
        .map_err(|e| format!("certfile: {e}"))?;
    let key = PrivateKeyDer::from_pem_file(&keyfile).map_err(|e| format!("keyfile: {e}"))?;
    let provider = Arc::new(rustls::crypto::ring::default_provider());
    let verify_peer = matches!(opts.map_get(atoms::verify()), Ok(v) if v.decode::<rustler::Atom>().ok() == Some(atoms::verify_peer()));
    let fail_if_no_cert = matches!(opts.map_get(atoms::fail_if_no_peer_cert()), Ok(v) if v.decode::<bool>().unwrap_or(false));
    let builder = ServerConfig::builder_with_provider(provider.clone())
        .with_safe_default_protocol_versions()
        .map_err(|e| e.to_string())?;
    let builder = if verify_peer {
        let cafile = get_path(opts, atoms::cacertfile())
            .map_err(|_| "bad cacertfile".to_string())?
            .ok_or_else(|| "verify_peer needs cacertfile".to_string())?;
        let mut roots = RootCertStore::empty();
        for c in CertificateDer::pem_file_iter(&cafile).map_err(|e| format!("cacertfile: {e}"))? {
            roots
                .add(c.map_err(|e| format!("cacertfile: {e}"))?)
                .map_err(|e| format!("cacertfile: {e}"))?;
        }
        let vb = WebPkiClientVerifier::builder_with_provider(Arc::new(roots), provider);
        let vb = if fail_if_no_cert {
            vb
        } else {
            vb.allow_unauthenticated()
        };
        builder.with_client_cert_verifier(vb.build().map_err(|e| e.to_string())?)
    } else {
        builder.with_no_client_auth()
    };
    let mut config = builder
        .with_single_cert(certs, key)
        .map_err(|e| e.to_string())?;
    // OTP ssl servers send no session tickets by default; match it.
    config.send_tls13_tickets = 0;
    Ok(config)
}

#[rustler::nif(schedule = "DirtyIo")]
fn tls_config<'a>(env: Env<'a>, opts: Term<'a>) -> Term<'a> {
    match build_config(opts) {
        Ok(config) => (
            atoms::ok(),
            ResourceArc::new(TlsConf {
                config: Arc::new(config),
            }),
        )
            .encode(env),
        Err(e) => (atoms::error(), reason(env, e)).encode(env),
    }
}

#[rustler::nif]
fn tls_new(conf: ResourceArc<TlsConf>) -> NifResult<ResourceArc<Tls>> {
    let conn = ServerConnection::new(conf.config.clone())
        .map_err(|e| rustler::Error::Term(Box::new(e.to_string())))?;
    Ok(ResourceArc::new(Tls {
        conn: Mutex::new(conn),
    }))
}

fn to_bin<'a>(env: Env<'a>, v: &[u8]) -> Binary<'a> {
    let mut ob = OwnedBinary::new(v.len()).expect("alloc");
    ob.as_mut_slice().copy_from_slice(v);
    ob.release(env)
}

fn drain_out(conn: &mut ServerConnection, out: &mut Vec<u8>) {
    while conn.wants_write() {
        if conn.write_tls(out).is_err() {
            break;
        }
    }
}

fn info<'a>(env: Env<'a>, conn: &ServerConnection) -> Term<'a> {
    let undef = atoms::undefined().encode(env);
    let version = conn
        .protocol_version()
        .map(|v| match v {
            rustls::ProtocolVersion::TLSv1_3 => "tlsv1.3".encode(env),
            rustls::ProtocolVersion::TLSv1_2 => "tlsv1.2".encode(env),
            other => format!("{other:?}").encode(env),
        })
        .unwrap_or(undef);
    let cipher = conn
        .negotiated_cipher_suite()
        .map(|s| format!("{:?}", s.suite()).encode(env))
        .unwrap_or(undef);
    let sni = conn.server_name().map(|s| s.encode(env)).unwrap_or(undef);
    let alpn = conn
        .alpn_protocol()
        .map(|p| to_bin(env, p).encode(env))
        .unwrap_or(undef);
    let peercert = conn
        .peer_certificates()
        .and_then(|c| c.first())
        .map(|c| to_bin(env, c.as_ref()).encode(env))
        .unwrap_or(undef);
    let pairs = [
        (atoms::version().encode(env), version),
        (atoms::cipher().encode(env), cipher),
        (atoms::sni().encode(env), sni),
        (atoms::alpn().encode(env), alpn),
        (atoms::peercert().encode(env), peercert),
    ];
    Term::map_from_pairs(env, &pairs).unwrap_or(undef)
}

fn feed<'a>(env: Env<'a>, tls: &Tls, data: Binary<'a>) -> Term<'a> {
    let mut conn = tls.conn.lock().unwrap();
    let was_handshaking = conn.is_handshaking();
    let mut rd: &[u8] = data.as_slice();
    let mut app: Vec<u8> = Vec::new();
    let mut out: Vec<u8> = Vec::new();
    while !rd.is_empty() {
        if let Err(e) = conn.read_tls(&mut rd) {
            return (atoms::error(), reason(env, e.to_string())).encode(env);
        }
        match conn.process_new_packets() {
            Err(e) => {
                drain_out(&mut conn, &mut out);
                return if out.is_empty() {
                    (atoms::error(), reason(env, e.to_string())).encode(env)
                } else {
                    (
                        atoms::close(),
                        to_bin(env, &out),
                        reason(env, e.to_string()),
                    )
                        .encode(env)
                };
            }
            Ok(st) => {
                let n = st.plaintext_bytes_to_read();
                if n > 0 {
                    let old = app.len();
                    app.resize(old + n, 0);
                    if let Err(e) = conn.reader().read_exact(&mut app[old..]) {
                        return (atoms::error(), reason(env, e.to_string())).encode(env);
                    }
                }
                if st.peer_has_closed() {
                    break;
                }
            }
        }
    }
    drain_out(&mut conn, &mut out);
    let out = to_bin(env, &out);
    let app = to_bin(env, &app);
    if was_handshaking && !conn.is_handshaking() {
        (atoms::handshake_done(), out, app, info(env, &conn)).encode(env)
    } else {
        (atoms::ok(), out, app).encode(env)
    }
}

/// Feed during the handshake: the key exchange and signature run here.
#[rustler::nif(schedule = "DirtyCpu")]
fn tls_feed_hs<'a>(env: Env<'a>, tls: ResourceArc<Tls>, data: Binary<'a>) -> Term<'a> {
    feed(env, &tls, data)
}

/// Feed after the handshake: record decryption only.
#[rustler::nif]
fn tls_feed<'a>(env: Env<'a>, tls: ResourceArc<Tls>, data: Binary<'a>) -> Term<'a> {
    feed(env, &tls, data)
}

#[rustler::nif]
fn tls_encrypt<'a>(env: Env<'a>, tls: ResourceArc<Tls>, data: Term<'a>) -> Term<'a> {
    let bin = match Binary::from_iolist(data) {
        Ok(b) => b,
        Err(_) => return (atoms::error(), atoms::error()).encode(env),
    };
    let mut conn = tls.conn.lock().unwrap();
    if let Err(e) = conn.writer().write_all(bin.as_slice()) {
        return (atoms::error(), reason(env, e.to_string())).encode(env);
    }
    let mut out = Vec::with_capacity(bin.len() + 64);
    drain_out(&mut conn, &mut out);
    (atoms::ok(), to_bin(env, &out)).encode(env)
}

#[rustler::nif]
fn tls_close<'a>(env: Env<'a>, tls: ResourceArc<Tls>) -> Term<'a> {
    let mut conn = tls.conn.lock().unwrap();
    conn.send_close_notify();
    let mut out = Vec::new();
    drain_out(&mut conn, &mut out);
    to_bin(env, &out).encode(env)
}
