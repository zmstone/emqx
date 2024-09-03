%%--------------------------------------------------------------------
%% Copyright (c) 2022-2024 EMQ Technologies Co., Ltd. All Rights Reserved.
%%--------------------------------------------------------------------

-module(emqx_eviction_agent).

-include_lib("emqx/include/emqx_mqtt.hrl").
-include_lib("emqx/include/logger.hrl").
-include_lib("emqx/include/types.hrl").
-include_lib("emqx/include/emqx_hooks.hrl").
-include_lib("emqx/include/emqx.hrl").

-include_lib("stdlib/include/qlc.hrl").
-include_lib("snabbkaffe/include/snabbkaffe.hrl").

-export([
    start_link/0,
    enable/2,
    enable/3,
    default_options/0,
    disable/1,
    status/0,
    enable_status/0,
    connection_count/0,
    all_channels_count/0,
    session_count/0,
    session_count/1,
    durable_session_count/0,
    evict_connections/1,
    evict_sessions/2,
    evict_sessions/3,
    purge_sessions/1,
    purge_durable_sessions/1
]).

%% RPC targets
-export([
    all_local_channels_count/0,
    evict_session_channel/3,
    do_evict_session_channel_v3/4
]).

-behaviour(gen_server).

-export([
    init/1,
    handle_call/3,
    handle_info/2,
    handle_cast/2,
    code_change/3
]).

-export([
    on_connect/2,
    on_connack/3
]).

-export([
    hook/0,
    unhook/0
]).

-export_type([server_reference/0, kind/0, options/0]).

-define(CONN_MODULES, [
    emqx_connection, emqx_ws_connection, emqx_quic_connection, emqx_eviction_agent_channel
]).

%%--------------------------------------------------------------------
%% APIs
%%--------------------------------------------------------------------

-type server_reference() :: binary() | undefined.
-type status() :: {enabled, conn_stats()} | disabled.
-type conn_stats() :: #{
    connections := non_neg_integer(),
    sessions := non_neg_integer()
}.

%% kind() is any() because it was not exported previously
%% and bpapi checker remembered it as any()
-type kind() :: any().
-type options() :: #{
    allow_connections => boolean()
}.

-spec start_link() -> startlink_ret().
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec default_options() -> options().
default_options() ->
    #{
        allow_connections => false
    }.

-spec enable(kind(), server_reference()) -> ok_or_error(eviction_agent_busy).
enable(Kind, ServerReference) ->
    gen_server:call(?MODULE, {enable, Kind, ServerReference, default_options()}).

-spec enable(kind(), server_reference(), options()) -> ok_or_error(eviction_agent_busy).
enable(Kind, ServerReference, #{} = Options) ->
    gen_server:call(?MODULE, {enable, Kind, ServerReference, Options}).

-spec disable(kind()) -> ok.
disable(Kind) ->
    gen_server:call(?MODULE, {disable, Kind}).

-spec status() -> status().
status() ->
    case enable_status() of
        {enabled, _Kind, _ServerReference, _Options} ->
            {enabled, stats()};
        disabled ->
            disabled
    end.

-spec enable_status() -> disabled | {enabled, kind(), server_reference(), options()}.
enable_status() ->
    persistent_term:get(?MODULE, disabled).

-spec evict_connections(pos_integer()) -> ok_or_error(disabled).
evict_connections(N) ->
    case enable_status() of
        {enabled, _Kind, ServerReference, _Options} ->
            ok = do_evict_connections(N, ServerReference);
        disabled ->
            {error, disabled}
    end.

-spec evict_sessions(pos_integer(), node() | [node()]) -> ok_or_error(disabled).
evict_sessions(N, Node) when is_atom(Node) ->
    evict_sessions(N, [Node]);
evict_sessions(N, Nodes) when is_list(Nodes) andalso length(Nodes) > 0 ->
    evict_sessions(N, Nodes, any).

-spec evict_sessions(pos_integer(), node() | [node()], atom()) -> ok_or_error(disabled).
evict_sessions(N, Node, ConnState) when is_atom(Node) ->
    evict_sessions(N, [Node], ConnState);
evict_sessions(N, Nodes, ConnState) when
    is_list(Nodes) andalso length(Nodes) > 0
->
    case enable_status() of
        {enabled, _Kind, _ServerReference, _Options} ->
            ok = do_evict_sessions(N, Nodes, ConnState);
        disabled ->
            {error, disabled}
    end.

-spec purge_sessions(non_neg_integer()) -> ok_or_error(disabled).
purge_sessions(N) ->
    case enable_status() of
        {enabled, _Kind, _ServerReference, _Options} ->
            ok = do_purge_sessions(N);
        disabled ->
            {error, disabled}
    end.

-spec purge_durable_sessions(non_neg_integer()) -> ok | done | {error, disabled}.
purge_durable_sessions(N) ->
    PersistenceEnabled = emqx_persistent_message:is_persistence_enabled(),
    case enable_status() of
        {enabled, _Kind, _ServerReference, _Options} when PersistenceEnabled ->
            do_purge_durable_sessions(N);
        {enabled, _Kind, _ServerReference, _Options} ->
            done;
        disabled ->
            {error, disabled}
    end.

%%--------------------------------------------------------------------
%% gen_server callbacks
%%--------------------------------------------------------------------

init([]) ->
    _ = persistent_term:erase(?MODULE),
    {ok, #{}}.

%% enable
handle_call({enable, Kind, ServerReference, Options}, _From, St) ->
    Reply =
        case enable_status() of
            disabled ->
                ok = persistent_term:put(?MODULE, {enabled, Kind, ServerReference, Options});
            {enabled, Kind, _ServerReference, _Options} ->
                ok = persistent_term:put(?MODULE, {enabled, Kind, ServerReference, Options});
            {enabled, _OtherKind, _ServerReference, _Options} ->
                {error, eviction_agent_busy}
        end,
    {reply, Reply, St};
%% disable
handle_call({disable, Kind}, _From, St) ->
    Reply =
        case enable_status() of
            disabled ->
                {error, disabled};
            {enabled, Kind, _ServerReference, _Options} ->
                _ = persistent_term:erase(?MODULE),
                ok;
            {enabled, _OtherKind, _ServerReference, _Options} ->
                {error, eviction_agent_busy}
        end,
    {reply, Reply, St};
handle_call(Msg, _From, St) ->
    ?SLOG(warning, #{msg => "unknown_call", call => Msg, state => St}),
    {reply, {error, unknown_call}, St}.

handle_info(Msg, St) ->
    ?SLOG(warning, #{msg => "unknown_msg", info => Msg, state => St}),
    {noreply, St}.

handle_cast(Msg, St) ->
    ?SLOG(warning, #{msg => "unknown_cast", cast => Msg, state => St}),
    {noreply, St}.

code_change(_Vsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% Hook callbacks
%%--------------------------------------------------------------------

on_connect(_ConnInfo, _Props) ->
    case enable_status() of
        {enabled, _Kind, _ServerReference, #{allow_connections := false}} ->
            {stop, {error, ?RC_USE_ANOTHER_SERVER}};
        {enabled, _Kind, _ServerReference, _Options} ->
            ignore;
        disabled ->
            ignore
    end.

on_connack(
    #{proto_name := <<"MQTT">>, proto_ver := ?MQTT_PROTO_V5},
    use_another_server,
    Props
) ->
    case enable_status() of
        {enabled, _Kind, ServerReference, _Options} ->
            {ok, Props#{'Server-Reference' => ServerReference}};
        disabled ->
            {ok, Props}
    end;
on_connack(_ClientInfo, _Reason, Props) ->
    {ok, Props}.

%%--------------------------------------------------------------------
%% Hook funcs
%%--------------------------------------------------------------------

hook() ->
    ?tp(debug, eviction_agent_hook, #{}),
    ok = emqx_hooks:put('client.connack', {?MODULE, on_connack, []}, ?HP_NODE_REBALANCE),
    ok = emqx_hooks:put('client.connect', {?MODULE, on_connect, []}, ?HP_NODE_REBALANCE).

unhook() ->
    ?tp(debug, eviction_agent_unhook, #{}),
    ok = emqx_hooks:del('client.connect', {?MODULE, on_connect}),
    ok = emqx_hooks:del('client.connack', {?MODULE, on_connack}).

%%--------------------------------------------------------------------
%% Internal funcs
%%--------------------------------------------------------------------

stats() ->
    #{
        connections => connection_count(),
        sessions => session_count()
    }.

connection_table() ->
    emqx_cm:live_connection_table(?CONN_MODULES).

connection_count() ->
    table_count(connection_table()).

channel_table(any) ->
    qlc:q([
        {ClientId, ConnInfo, ClientInfo}
     || {ClientId, _, ConnInfo, ClientInfo} <-
            emqx_cm:all_channels_table(?CONN_MODULES)
    ]);
channel_table(RequiredConnState) ->
    qlc:q([
        {ClientId, ConnInfo, ClientInfo}
     || {ClientId, ConnState, ConnInfo, ClientInfo} <-
            emqx_cm:all_channels_table(?CONN_MODULES),
        RequiredConnState =:= ConnState
    ]).

-spec all_channels_count() -> non_neg_integer().
all_channels_count() ->
    Nodes = emqx:running_nodes(),
    Timeout = 15_000,
    Results = emqx_eviction_agent_proto_v2:all_channels_count(Nodes, Timeout),
    NodeResults = lists:zip(Nodes, Results),
    Errors = lists:filter(
        fun
            ({_Node, {ok, _}}) -> false;
            ({_Node, _Err}) -> true
        end,
        NodeResults
    ),
    Errors =/= [] andalso
        ?SLOG(
            warning,
            #{
                msg => "error_collecting_all_channels_count",
                errors => maps:from_list(Errors)
            }
        ),
    lists:sum([N || {ok, N} <- Results]).

-spec all_local_channels_count() -> non_neg_integer().
all_local_channels_count() ->
    table_count(channel_table(any)).

session_count() ->
    session_count(any) + durable_session_count().

durable_session_count() ->
    emqx_persistent_session_bookkeeper:get_disconnected_session_count().

session_count(ConnState) ->
    table_count(channel_table(ConnState)).

table_count(QH) ->
    qlc:fold(fun(_, Acc) -> Acc + 1 end, 0, QH).

take_connections(N) ->
    ChanQH = qlc:q([ChanPid || {_ClientId, ChanPid} <- connection_table()]),
    ChanPidCursor = qlc:cursor(ChanQH),
    ChanPids = qlc:next_answers(ChanPidCursor, N),
    ok = qlc:delete_cursor(ChanPidCursor),
    ChanPids.

take_channels(N) ->
    QH = qlc:q([
        {ClientId, ConnInfo, ClientInfo}
     || {ClientId, _, ConnInfo, ClientInfo} <-
            emqx_cm:all_channels_table(?CONN_MODULES)
    ]),
    ChanPidCursor = qlc:cursor(QH),
    Channels = qlc:next_answers(ChanPidCursor, N),
    ok = qlc:delete_cursor(ChanPidCursor),
    Channels.

take_channels(N, ConnState) ->
    ChanPidCursor = qlc:cursor(channel_table(ConnState)),
    Channels = qlc:next_answers(ChanPidCursor, N),
    ok = qlc:delete_cursor(ChanPidCursor),
    Channels.

do_evict_connections(N, ServerReference) when N > 0 ->
    ChanPids = take_connections(N),
    ok = lists:foreach(
        fun(ChanPid) ->
            disconnect_channel(ChanPid, ServerReference)
        end,
        ChanPids
    ).

do_evict_sessions(N, Nodes, ConnState) when N > 0 ->
    Channels = take_channels(N, ConnState),
    ok = lists:foreach(
        fun({ClientId, ConnInfo, ClientInfo}) ->
            evict_session_channel(Nodes, ClientId, ConnInfo, ClientInfo)
        end,
        Channels
    ).

evict_session_channel(Nodes, ClientId, ConnInfo, ClientInfo) ->
    Node = select_random(Nodes),
    ?SLOG(
        info,
        #{
            msg => "evict_session_channel",
            client_id => ClientId,
            node => Node,
            conn_info => ConnInfo,
            client_info => ClientInfo
        }
    ),
    case emqx_eviction_agent_proto_v2:evict_session_channel(Node, ClientId, ConnInfo, ClientInfo) of
        {badrpc, Reason} ->
            ?SLOG(
                error,
                #{
                    msg => "evict_session_channel_rpc_error",
                    client_id => ClientId,
                    node => Node,
                    reason => Reason
                }
            ),
            {error, Reason};
        {error, {no_session, _}} = Error ->
            ?SLOG(
                warning,
                #{
                    msg => "evict_session_channel_no_session",
                    client_id => ClientId,
                    node => Node
                }
            ),
            Error;
        {error, Reason} = Error ->
            ?SLOG(
                error,
                #{
                    msg => "evict_session_channel_error",
                    client_id => ClientId,
                    node => Node,
                    reason => Reason
                }
            ),
            Error;
        Res ->
            Res
    end.

%% RPC target for `emqx_eviction_agent_proto_v2'
-spec evict_session_channel(
    emqx_types:clientid(),
    emqx_types:conninfo(),
    emqx_types:clientinfo()
) -> supervisor:startchild_ret().
evict_session_channel(ClientId, ConnInfo, ClientInfo) ->
    do_evict_session_channel_v3(ClientId, ConnInfo, ClientInfo, _WillMsg = undefined).

%% RPC target for `emqx_eviction_agent_proto_v3'
-spec do_evict_session_channel_v3(
    emqx_types:clientid(),
    emqx_types:conninfo(),
    emqx_types:clientinfo(),
    emqx_maybe:t(emqx_types:message())
) -> supervisor:startchild_ret().
do_evict_session_channel_v3(ClientId, ConnInfo, ClientInfo, MaybeWillMsg) ->
    ?SLOG(info, #{
        msg => "evict_session_channel",
        client_id => ClientId,
        conn_info => ConnInfo,
        client_info => ClientInfo
    }),
    Result = emqx_eviction_agent_channel:start_supervised(
        #{
            conninfo => ConnInfo,
            clientinfo => ClientInfo,
            will_message => MaybeWillMsg
        }
    ),
    ?SLOG(
        info,
        #{
            msg => "evict_session_channel_result",
            client_id => ClientId,
            result => Result
        }
    ),
    Result.

disconnect_channel(ChanPid, ServerReference) ->
    ChanPid !
        {disconnect, ?RC_USE_ANOTHER_SERVER, use_another_server, #{
            'Server-Reference' => ServerReference
        }}.

do_purge_sessions(N) when N > 0 ->
    Channels = take_channels(N),
    ok = lists:foreach(
        fun
            ({{Mtns, ClientId}, _ConnInfo, _ClientInfo}) ->
                emqx_cm:discard_session(Mtns, ClientId);
            ({ClientId, _ConnInfo, _ClientInfo}) when is_binary(ClientId) ->
                emqx_cm:discard_session(?GBNS, ClientId)
        end,
        Channels
    ).

do_purge_durable_sessions(N) when N > 0 ->
    Iterator = emqx_persistent_session_ds_state:make_session_iterator(),
    {Sessions, _NewIterator} = emqx_persistent_session_ds_state:session_iterator_next(Iterator, N),
    lists:foreach(
        fun({ClientId, _Metadata}) ->
            emqx_persistent_session_ds:destroy_session(ClientId)
        end,
        Sessions
    ),
    case Sessions of
        [] ->
            done;
        _ ->
            ok
    end.

select_random(List) when length(List) > 0 ->
    lists:nth(rand:uniform(length(List)), List).
