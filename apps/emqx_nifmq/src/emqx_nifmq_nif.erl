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
    info/0
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
    info/0
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
