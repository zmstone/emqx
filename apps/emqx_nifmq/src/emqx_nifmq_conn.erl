%%--------------------------------------------------------------------
%% Copyright (c) 2026 EMQ Technologies Co., Ltd. All Rights Reserved.
%%--------------------------------------------------------------------
-module(emqx_nifmq_conn).

-moduledoc """
Native socket writer with a QoS 0 and QoS 1 outgoing session (experimental).

`attach/2` gives the NIF a `dup()` copy of a connected TCP socket. After that, every
outgoing byte of the connection must go through this module: `dispatch/2` for PUBLISH
packets to subscribers, `send/2` for other packets. The caller keeps reading the socket and
calls `puback/2` for each PUBACK it receives.

A dispatch encodes one MQTT PUBLISH per subscriber, applies the subscription options, and
writes it to the socket. No Erlang message goes to the subscriber's process. QoS 1
deliveries use a native window of `window` packets; the rest wait in a native queue of at
most `max_mqueue` messages.

After `read_native/1`, a native poller thread also reads the socket. It handles PUBACKs
itself and sends every other packet to the owner as `{nifmq_data, Conn, Bytes}`. At end of
stream or on an error it sends `{nifmq_closed, Conn, Reason}`.

The connection closes its socket copy when `close/1` is called or when the owner (the
process that called `attach/2`) exits.
""".

-export([
    set_mode/2,
    attach/2,
    subscribe/3,
    unsubscribe/2,
    dispatch/2,
    puback/2,
    read_native/1,
    send/2,
    close/1,
    info/1,
    stats/0
]).

-export_type([conn/0, attach_opts/0, sub_opts/0, msg_parts/0]).

-opaque conn() :: reference().

-type attach_opts() :: #{
    proto_ver := 3 | 4 | 5,
    clientid := binary(),
    mountpoint => binary(),
    max_packet_size => pos_integer(),
    window => pos_integer(),
    max_mqueue => non_neg_integer(),
    max_buffer_bytes => pos_integer()
}.

-type sub_opts() :: #{qos := 0 | 1, nl => 0 | 1, rap => 0 | 1, subid => pos_integer()}.

-type msg_parts() :: #{
    topic := binary(),
    payload := binary(),
    qos := 0 | 1,
    from := binary(),
    retain => boolean(),
    retained => boolean(),
    props => binary()
}.

-define(MAX_PACKET_SIZE, 268435455).

-doc """
Select who writes to the sockets: `inline` (the caller of `dispatch/2`, `puback/2` or
`send/2`) or `threads` with `K` native poller threads (1 to 4). Affects connections that are
attached afterwards.
""".
-spec set_mode(inline | threads, 1..4) -> ok.
set_mode(Mode, K) ->
    emqx_nifmq_nif:conn_set_mode(Mode, K).

-doc "Attach a connected TCP socket, given as its file descriptor. The caller is the owner.".
-spec attach(non_neg_integer(), attach_opts()) -> conn().
attach(Fd, Opts) ->
    emqx_nifmq_nif:conn_attach(Fd, {
        maps:get(proto_ver, Opts),
        maps:get(clientid, Opts),
        maps:get(mountpoint, Opts, <<>>),
        maps:get(max_packet_size, Opts, ?MAX_PACKET_SIZE),
        maps:get(window, Opts, 32),
        maps:get(max_mqueue, Opts, 1000),
        maps:get(max_buffer_bytes, Opts, 1 bsl 20)
    }).

-doc "Subscribe the connection to a topic filter. A second call replaces the options.".
-spec subscribe(conn(), binary(), sub_opts()) -> ok.
subscribe(Conn, Filter, SubOpts) ->
    emqx_nifmq_nif:conn_subscribe(Conn, Filter, {
        maps:get(qos, SubOpts),
        maps:get(nl, SubOpts, 0) =:= 1,
        maps:get(rap, SubOpts, 0) =:= 1,
        maps:get(subid, SubOpts, 0)
    }).

-spec unsubscribe(conn(), binary()) -> ok.
unsubscribe(Conn, Filter) ->
    emqx_nifmq_nif:conn_unsubscribe(Conn, Filter).

-doc """
Deliver a message to every native subscriber of `Filter`. Return the number of subscribers
that sent or queued it. Each NIF call serves at most 64 (inline) or 256 (threads)
subscribers, so a large fan-out takes several calls.
""".
-spec dispatch(binary(), msg_parts()) -> non_neg_integer().
dispatch(Filter, Parts) ->
    T = {
        maps:get(topic, Parts),
        maps:get(payload, Parts),
        maps:get(qos, Parts),
        maps:get(retain, Parts, false),
        maps:get(retained, Parts, false),
        maps:get(from, Parts),
        maps:get(props, Parts, <<>>)
    },
    case emqx_nifmq_nif:conn_dispatch(Filter, T) of
        {ok, N} -> N;
        {continue, Cont, N} -> dispatch_cont(Cont, N)
    end.

dispatch_cont(Cont, Acc) ->
    case emqx_nifmq_nif:conn_dispatch_cont(Cont) of
        {ok, N} -> Acc + N;
        {continue, Cont, N} -> dispatch_cont(Cont, Acc + N)
    end.

-doc "Acknowledge a QoS 1 packet identifier. Queued messages fill the freed window slot.".
-spec puback(conn(), 1..65535) -> ok | {error, not_found}.
puback(Conn, PacketId) ->
    emqx_nifmq_nif:conn_puback(Conn, PacketId).

-doc """
Read the socket natively from now on. The owner must not read the socket after this call.
""".
-spec read_native(conn()) -> ok.
read_native(Conn) ->
    emqx_nifmq_nif:conn_read_native(Conn).

-doc "Send an encoded packet after the packets that are already queued.".
-spec send(conn(), iodata()) -> ok | {error, closed}.
send(Conn, Data) ->
    emqx_nifmq_nif:conn_send(Conn, iolist_to_binary(Data)).

-spec close(conn()) -> ok.
close(Conn) ->
    emqx_nifmq_nif:conn_close(Conn).

-doc "Return the window use, queue length and unsent bytes of one connection.".
-spec info(conn()) -> #{atom() => term()}.
info(Conn) ->
    emqx_nifmq_nif:conn_info(Conn).

-doc "Return the global counters of the native writer.".
-spec stats() -> #{atom() => non_neg_integer()}.
stats() ->
    emqx_nifmq_nif:conn_stats().
