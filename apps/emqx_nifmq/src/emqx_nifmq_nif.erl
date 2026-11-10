%%--------------------------------------------------------------------
%% Copyright (c) 2026 EMQ Technologies Co., Ltd. All Rights Reserved.
%%--------------------------------------------------------------------
-module(emqx_nifmq_nif).

-moduledoc """
NIF stubs for the NifMQ prototype. Use `emqx_nifmq` instead of calling this module.
""".

-export([
    new_queue/1,
    subscribe/2,
    unsubscribe/2,
    publish/2,
    publish_cont/1,
    drain/2,
    drain_view/2,
    set_congested/2,
    queue_info/1,
    conn_attach/2,
    conn_subscribe/3,
    conn_unsubscribe/2,
    conn_dispatch/2,
    conn_dispatch_cont/1,
    conn_puback/2,
    conn_read_native/1,
    conn_send/2,
    conn_close/1,
    conn_set_mode/2,
    conn_info/1,
    conn_stats/0,
    info/0,
    tls_config/1,
    tls_new/1,
    tls_feed_hs/2,
    tls_feed/2,
    tls_encrypt/2,
    tls_close/1
]).

-nifs([
    new_queue/1,
    subscribe/2,
    unsubscribe/2,
    publish/2,
    publish_cont/1,
    drain/2,
    drain_view/2,
    set_congested/2,
    queue_info/1,
    conn_attach/2,
    conn_subscribe/3,
    conn_unsubscribe/2,
    conn_dispatch/2,
    conn_dispatch_cont/1,
    conn_puback/2,
    conn_read_native/1,
    conn_send/2,
    conn_close/1,
    conn_set_mode/2,
    conn_info/1,
    conn_stats/0,
    info/0,
    tls_config/1,
    tls_new/1,
    tls_feed_hs/2,
    tls_feed/2,
    tls_encrypt/2,
    tls_close/1
]).

-on_load(init/0).

init() ->
    Lib = filename:join(code:priv_dir(emqx_nifmq), "libemqx_nifmq"),
    erlang:load_nif(Lib, 0).

-define(NOT_LOADED, erlang:nif_error(nif_not_loaded)).

new_queue(_Cap) -> ?NOT_LOADED.
subscribe(_Q, _Topic) -> ?NOT_LOADED.
unsubscribe(_Q, _Topic) -> ?NOT_LOADED.
publish(_Topic, _Msg) -> ?NOT_LOADED.
publish_cont(_Fanout) -> ?NOT_LOADED.
drain(_Q, _Max) -> ?NOT_LOADED.
drain_view(_Q, _Max) -> ?NOT_LOADED.
set_congested(_Q, _Bool) -> ?NOT_LOADED.
queue_info(_Q) -> ?NOT_LOADED.
info() -> ?NOT_LOADED.
conn_attach(_Fd, _Opts) -> ?NOT_LOADED.
conn_subscribe(_Conn, _Filter, _SubOpts) -> ?NOT_LOADED.
conn_unsubscribe(_Conn, _Filter) -> ?NOT_LOADED.
conn_dispatch(_Filter, _Parts) -> ?NOT_LOADED.
conn_dispatch_cont(_Cont) -> ?NOT_LOADED.
conn_puback(_Conn, _PacketId) -> ?NOT_LOADED.
conn_read_native(_Conn) -> ?NOT_LOADED.
conn_send(_Conn, _Data) -> ?NOT_LOADED.
conn_close(_Conn) -> ?NOT_LOADED.
conn_set_mode(_Mode, _K) -> ?NOT_LOADED.
conn_info(_Conn) -> ?NOT_LOADED.
conn_stats() -> ?NOT_LOADED.
tls_config(_Opts) -> ?NOT_LOADED.
tls_new(_Conf) -> ?NOT_LOADED.
tls_feed_hs(_Tls, _Data) -> ?NOT_LOADED.
tls_feed(_Tls, _Data) -> ?NOT_LOADED.
tls_encrypt(_Tls, _IoData) -> ?NOT_LOADED.
tls_close(_Tls) -> ?NOT_LOADED.
