%%--------------------------------------------------------------------
%% Copyright (c) 2026 EMQ Technologies Co., Ltd. All Rights Reserved.
%%--------------------------------------------------------------------
-module(emqx_niftls).

-moduledoc """
TLS server state machine in a NIF (prototype). The caller does the socket I/O: it feeds the
bytes read from the socket and sends the bytes returned.

`feed/2` returns:
- `{ok, Out, App}`: send `Out`, then parse `App` (decrypted bytes). Both can be empty.
- `{handshake_done, Out, App, Info}`: as `ok`, once, when the handshake completes.
- `{close, Out, Reason}`: send `Out` (an alert), then close the socket.
- `{error, Reason}`: close the socket.

Enable it on a TCP listener with the `socket` backend:
`emqx_niftls:enable({tcp, default}, #{certfile => F, keyfile => K})`.
""".

-export([config/1, new/1, feed/2, encrypt/2, close/1]).
-export([enable/2, disable/1, listener_conf/1]).

-type conf() :: reference().
-type tls() :: {hs | up, reference()}.
-export_type([conf/0, tls/0]).

-spec config(map()) -> {ok, conf()} | {error, binary()}.
config(Opts) ->
    emqx_nifmq_nif:tls_config(Opts).

-spec new(conf()) -> tls().
new(Conf) ->
    {hs, emqx_nifmq_nif:tls_new(Conf)}.

%% The handshake runs on a dirty CPU scheduler, the record layer on a normal one.
-spec feed(tls(), binary()) ->
    {ok, iodata(), binary(), tls()}
    | {handshake_done, iodata(), binary(), map(), tls()}
    | {close, iodata(), term()}
    | {error, term()}.
feed({hs, R} = Tls, Data) ->
    case emqx_nifmq_nif:tls_feed_hs(R, Data) of
        {ok, Out, App} -> {ok, Out, App, Tls};
        {handshake_done, Out, App, Info} -> {handshake_done, Out, App, Info, {up, R}};
        Other -> Other
    end;
feed({up, R} = Tls, Data) ->
    case emqx_nifmq_nif:tls_feed(R, Data) of
        {ok, Out, App} -> {ok, Out, App, Tls};
        Other -> Other
    end.

-spec encrypt(tls(), iodata()) -> {ok, binary()} | {error, term()}.
encrypt({_, R}, IoData) ->
    emqx_nifmq_nif:tls_encrypt(R, IoData).

-spec close(tls()) -> binary().
close({_, R}) ->
    emqx_nifmq_nif:tls_close(R).

-spec enable(emqx_listeners:listener_id() | {atom(), atom()}, map()) -> ok | {error, binary()}.
enable(Listener, Opts) ->
    case config(Opts) of
        {ok, Conf} -> persistent_term:put({?MODULE, Listener}, Conf);
        Error -> Error
    end.

disable(Listener) ->
    _ = persistent_term:erase({?MODULE, Listener}),
    ok.

-spec listener_conf(term()) -> conf() | undefined.
listener_conf(Listener) ->
    persistent_term:get({?MODULE, Listener}, undefined).
