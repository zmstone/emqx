%%--------------------------------------------------------------------
%% Copyright (c) 2020-2024 EMQ Technologies Co., Ltd. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%--------------------------------------------------------------------

-module(emqx_mgmt).

-include("emqx_mgmt.hrl").
-include_lib("emqx/include/emqx.hrl").
-include_lib("emqx/include/emqx_cm.hrl").
-include_lib("emqx/include/logger.hrl").

-elvis([{elvis_style, invalid_dynamic_call, disable}]).
-elvis([{elvis_style, god_modules, disable}]).

-include_lib("stdlib/include/qlc.hrl").

%% Nodes and Brokers API
-export([
    list_nodes/0,
    lookup_node/1,
    list_brokers/0,
    lookup_broker/1,
    node_info/0,
    node_info/1,
    broker_info/0,
    broker_info/1
]).

%% Metrics and Stats
-export([
    get_metrics/0,
    get_metrics/1,
    get_stats/0,
    get_stats/1
]).

%% Clients, Sessions
-export([
    lookup_client/3,
    lookup_client/4,
    kickout_client/2,
    kickout_clients/2,
    list_authz_cache/2,
    list_client_subscriptions/2,
    list_client_msgs/4,
    client_subscriptions/3,
    clean_authz_cache/2,
    clean_authz_cache/3,
    clean_authz_cache_all/0,
    clean_authz_cache_all/1,
    clean_pem_cache_all/0,
    clean_pem_cache_all/1,
    set_ratelimit_policy/3,
    set_quota_policy/3,
    set_keepalive/3,

    %% @deprecated
    do_kickout_clients/1,
    do_kickout_clients/2
]).

%% Internal exports
-export([lookup_running_client/3]).

%% Internal functions
-export([do_call_client/2, do_call_client/3]).

%% Subscriptions
-export([
    list_subscriptions/1,
    list_subscriptions_via_topic/2,
    list_subscriptions_via_topic/3,

    do_list_subscriptions/0
]).

%% PubSub
-export([
    subscribe/3,
    %% @deprecated
    do_subscribe/2,
    do_subscribe/3,
    publish/1,
    unsubscribe/3,
    %% @deprecated
    do_unsubscribe/2,
    do_unsubscribe/3,
    unsubscribe_batch/3,
    %% @deprecated
    do_unsubscribe_batch/2,
    do_unsubscribe_batch/3
]).

%% Alarms
-export([
    get_alarms/1,
    get_alarms/2,
    deactivate/2,
    delete_all_deactivated_alarms/0,
    delete_all_deactivated_alarms/1
]).

%% Banned
-export([
    create_banned/1,
    delete_banned/1
]).

%% Common Table API
-export([
    default_row_limit/0,
    vm_stats/0,
    vm_stats/1
]).

-elvis([{elvis_style, god_modules, disable}]).

-define(maybe_log_node_errors(LogData, Errors),
    case Errors of
        [] -> ok;
        _ -> ?SLOG(error, (LogData)#{node_errors => Errors})
    end
).

%%--------------------------------------------------------------------
%% Node Info
%%--------------------------------------------------------------------

list_nodes() ->
    Running = emqx:cluster_nodes(running),
    %% all stopped core nodes
    Stopped = emqx:cluster_nodes(stopped),
    DownNodes = lists:map(fun stopped_node_info/1, Stopped),
    [{Node, Info} || #{node := Node} = Info <- node_info(Running)] ++ DownNodes.

lookup_node(Node) ->
    [Info] = node_info([Node]),
    Info.

node_info() ->
    {UsedRatio, Total} = get_sys_memory(),
    Info = maps:from_list(emqx_vm:loads()),
    BrokerInfo = emqx_sys:info(),
    Info#{
        node => node(),
        otp_release => otp_rel(),
        memory_total => Total,
        memory_used => erlang:round(Total * UsedRatio),
        process_available => erlang:system_info(process_limit),
        process_used => erlang:system_info(process_count),

        max_fds => proplists:get_value(
            max_fds, lists:usort(lists:flatten(erlang:system_info(check_io)))
        ),
        connections => ets:info(?CHAN_TAB, size),
        live_connections => ets:info(?CHAN_LIVE_TAB, size),
        cluster_sessions => ets:info(?CHAN_REG_TAB, size),
        node_status => 'running',
        uptime => proplists:get_value(uptime, BrokerInfo),
        version => iolist_to_binary(proplists:get_value(version, BrokerInfo)),
        edition => emqx_release:edition_longstr(),
        role => mria_rlog:role(),
        log_path => log_path(),
        sys_path => iolist_to_binary(code:root_dir())
    }.

log_path() ->
    RootDir = code:root_dir(),
    Configs = logger:get_handler_config(),
    case get_log_path(Configs) of
        undefined ->
            <<"log.file.enable is false, not logging to file.">>;
        Path ->
            iolist_to_binary(filename:join(RootDir, Path))
    end.

get_log_path([#{config := #{file := Path}} | _LoggerConfigs]) ->
    filename:dirname(Path);
get_log_path([_LoggerConfig | LoggerConfigs]) ->
    get_log_path(LoggerConfigs);
get_log_path([]) ->
    undefined.

get_sys_memory() ->
    case os:type() of
        {unix, linux} ->
            emqx_mgmt_cache:get_sys_memory();
        _ ->
            {0, 0}
    end.

node_info(Nodes) ->
    emqx_rpc:unwrap_erpc(emqx_management_proto_v5:node_info(Nodes)).

stopped_node_info(Node) ->
    {Node, #{node => Node, node_status => 'stopped', role => core}}.

%% Hide cpu stats if os_check is not supported.
vm_stats() ->
    {MemUsedRatio, MemTotal} = get_sys_memory(),
    cpu_stats() ++
        [
            {run_queue, vm_stats('run.queue')},
            {total_memory, MemTotal},
            {used_memory, erlang:round(MemTotal * MemUsedRatio)}
        ].

cpu_stats() ->
    case emqx_os_mon:is_os_check_supported() of
        false ->
            [];
        true ->
            vm_stats('cpu')
    end.

vm_stats('cpu') ->
    CpuUtilArg = [],
    case emqx_vm:cpu_util([CpuUtilArg]) of
        %% return 0.0 when `emqx_cpu_sup_worker` is not started
        {all, Use, Idle, _} ->
            NUse = floor(Use * 100) / 100,
            NIdle = ceil(Idle * 100) / 100,
            [{cpu_use, NUse}, {cpu_idle, NIdle}];
        _ ->
            [{cpu_use, 0}, {cpu_idle, 0}]
    end;
vm_stats('total.memory') ->
    {_, MemTotal} = get_sys_memory(),
    MemTotal;
vm_stats('used.memory') ->
    {MemUsedRatio, MemTotal} = get_sys_memory(),
    erlang:round(MemTotal * MemUsedRatio);
vm_stats('run.queue') ->
    erlang:statistics(run_queue).

%%--------------------------------------------------------------------
%% Brokers
%%--------------------------------------------------------------------

list_brokers() ->
    Running = emqx:running_nodes(),
    [{Node, Broker} || #{node := Node} = Broker <- broker_info(Running)].

lookup_broker(Node) ->
    [Broker] = broker_info([Node]),
    Broker.

broker_info() ->
    Info = lists:foldl(fun convert_broker_info/2, #{}, emqx_sys:info()),
    Info#{node => node(), otp_release => otp_rel(), node_status => 'running'}.

convert_broker_info({uptime, Uptime}, M) ->
    M#{uptime => emqx_utils_calendar:human_readable_duration_string(Uptime)};
convert_broker_info({K, V}, M) ->
    M#{K => iolist_to_binary(V)}.

broker_info(Nodes) ->
    emqx_rpc:unwrap_erpc(emqx_management_proto_v5:broker_info(Nodes)).

%%--------------------------------------------------------------------
%% Metrics and Stats
%%--------------------------------------------------------------------

get_metrics() ->
    nodes_info_count([get_metrics(Node) || Node <- emqx:running_nodes()]).

get_metrics(Node) ->
    unwrap_rpc(emqx_proto_v1:get_metrics(Node)).

aggregated_only_keys() ->
    [
        'durable_subscriptions.count',
        'durable_subscriptions.max'
    ].

get_stats() ->
    GlobalStatsKeys =
        [
            'retained.count',
            'retained.max',
            'topics.count',
            'topics.max',
            'subscriptions.shared.count',
            'subscriptions.shared.max'
        ],
    CountStats = nodes_info_count(
        lists:foldl(
            fun(Node, Acc) ->
                case get_stats(Node) of
                    {error, _} ->
                        Acc;
                    Stats ->
                        [delete_keys(Stats, GlobalStatsKeys) | Acc]
                end
            end,
            [],
            emqx:running_nodes()
        )
    ),
    GlobalStats = maps:with(GlobalStatsKeys, maps:from_list(emqx_stats:getstats())),
    maps:merge(CountStats, GlobalStats).

delete_keys(List, []) ->
    List;
delete_keys(List, [Key | Keys]) ->
    delete_keys(proplists:delete(Key, List), Keys).

get_stats(Node) ->
    case unwrap_rpc(emqx_proto_v1:get_stats(Node)) of
        {error, _} = Error ->
            Error;
        Stats when is_list(Stats) ->
            delete_keys(Stats, aggregated_only_keys())
    end.

nodes_info_count(PropList) ->
    NodeCount =
        fun({Key, Value}, Result) ->
            Count = maps:get(Key, Result, 0),
            Result#{Key => Count + Value}
        end,
    AllCount =
        fun(StatsMap, Result) ->
            lists:foldl(NodeCount, Result, StatsMap)
        end,
    lists:foldl(AllCount, #{}, PropList).

%%--------------------------------------------------------------------
%% Clients
%%--------------------------------------------------------------------

lookup_client(Mtns, {clientid, ClientId}, FormatFun) ->
    IsPersistenceEnabled = emqx_persistent_message:is_persistence_enabled(),
    case lookup_running_client(Mtns, ClientId, FormatFun) of
        [] when IsPersistenceEnabled ->
            case emqx_persistent_session_ds_state:print_session(ClientId) of
                undefined -> [];
                Session -> [maybe_format(FormatFun, {ClientId, Session})]
            end;
        Res ->
            Res
    end;
lookup_client(Mtns, {username, Username}, FormatFun) ->
    lists:append([
        lookup_client(Node, Mtns, {username, Username}, FormatFun)
     || Node <- emqx:running_nodes()
    ]).

lookup_client(Node, Mtns, Key, FormatFun) ->
    case unwrap_rpc(emqx_cm_proto_v4:lookup_client(Node, Mtns, Key)) of
        {error, Err} ->
            {error, Err};
        L ->
            lists:map(
                fun({Chan, Info0, Stats}) ->
                    Info = Info0#{node => Node},
                    maybe_format(FormatFun, {Chan, Info, Stats})
                end,
                L
            )
    end.

maybe_format(undefined, A) ->
    A;
maybe_format({M, F}, A) ->
    M:F(A).

kickout_client(Mtns, ClientId) ->
    case lookup_client(Mtns, {clientid, ClientId}, undefined) of
        [] ->
            {error, not_found};
        [{ClientId, _}] ->
            %% Offline durable session (client ID is a plain binary
            %% without channel pid):
            emqx_persistent_session_ds:kick_offline_session(ClientId);
        _ ->
            Results = [kickout_client(Node, Mtns, ClientId) || Node <- emqx:running_nodes()],
            check_results(Results)
    end.

kickout_client(Node, Mtns, ClientId) ->
    unwrap_rpc(emqx_cm_proto_v4:kickout_client(Node, Mtns, ClientId)).

kickout_clients(Mtns, ClientIds) when is_list(ClientIds) ->
    F = fun(Node) ->
        emqx_management_proto_v6:kickout_clients(Node, Mtns, ClientIds)
    end,
    Results = lists:map(F, emqx:running_nodes()),
    lists:foreach(fun emqx_persistent_session_ds:kick_offline_session/1, ClientIds),
    case lists:filter(fun(Res) -> Res =/= ok end, Results) of
        [] ->
            ok;
        [Result | _] ->
            unwrap_rpc(Result)
    end.

%% @deprecated Used by emqx_management_proto_v5 and before version
-spec do_kickout_clients([emqx_types:clientid()]) -> ok.
do_kickout_clients(ClientIds) when is_list(ClientIds) ->
    do_kickout_clients(undefined, ClientIds).

-spec do_kickout_clients(emqx_types:mtns(), [emqx_types:clientid()]) -> ok.
do_kickout_clients(Mtns, ClientIds) when is_list(ClientIds) ->
    F = fun(ClientId) ->
        ChanPids = emqx_cm:lookup_channels(local, Mtns, ClientId),
        lists:foreach(
            fun(ChanPid) -> emqx_cm:kick_session(Mtns, ClientId, ChanPid) end,
            ChanPids
        )
    end,
    lists:foreach(F, ClientIds).

list_authz_cache(Mtns, ClientId) ->
    call_client(Mtns, ClientId, list_authz_cache).

list_client_subscriptions(Mtns, ClientId) ->
    case emqx_persistent_session_ds:list_client_subscriptions(Mtns, ClientId) of
        {error, not_found} ->
            list_client_subscriptions_mem(Mtns, ClientId);
        Result ->
            Result
    end.

%% List subscriptions of an in-memory session:
list_client_subscriptions_mem(Mtns, ClientId) ->
    case lookup_client(Mtns, {clientid, ClientId}, undefined) of
        [] ->
            {error, not_found};
        _ ->
            Results = [client_subscriptions(Node, Mtns, ClientId) || Node <- emqx:running_nodes()],
            Filter =
                fun
                    ({error, _}) ->
                        false;
                    ({_Node, List}) ->
                        erlang:is_list(List) andalso 0 < erlang:length(List)
                end,
            case lists:filter(Filter, Results) of
                [] -> [];
                [Result | _] -> Result
            end
    end.

list_client_msgs(MsgsType, Mtns, ClientId, PagerParams) when
    MsgsType =:= inflight_msgs;
    MsgsType =:= mqueue_msgs
->
    call_client(Mtns, ClientId, {MsgsType, PagerParams}).

client_subscriptions(Node, Mtns, ClientId) ->
    {Node, unwrap_rpc(emqx_broker_proto_v2:list_client_subscriptions(Node, Mtns, ClientId))}.

clean_authz_cache(Mtns, ClientId) ->
    Results = [clean_authz_cache(Node, Mtns, ClientId) || Node <- emqx:running_nodes()],
    check_results(Results).

clean_authz_cache(Node, Mtns, ClientId) ->
    unwrap_rpc(emqx_proto_v3:clean_authz_cache_v3(Node, Mtns, ClientId)).

clean_authz_cache_all() ->
    Results = [{Node, clean_authz_cache_all(Node)} || Node <- emqx:running_nodes()],
    wrap_results(Results).

clean_pem_cache_all() ->
    Results = [{Node, clean_pem_cache_all(Node)} || Node <- emqx:running_nodes()],
    wrap_results(Results).

wrap_results(Results) ->
    case lists:filter(fun({_Node, Item}) -> Item =/= ok end, Results) of
        [] -> ok;
        BadNodes -> {error, BadNodes}
    end.

clean_authz_cache_all(Node) ->
    unwrap_rpc(emqx_proto_v2:clean_authz_cache(Node)).

clean_pem_cache_all(Node) ->
    unwrap_rpc(emqx_proto_v2:clean_pem_cache(Node)).

set_ratelimit_policy(Mtns, ClientId, Policy) ->
    call_client(Mtns, ClientId, {ratelimit, Policy}).

set_quota_policy(Mtns, ClientId, Policy) ->
    call_client(Mtns, ClientId, {quota, Policy}).

set_keepalive(Mtns, ClientId, Interval) when Interval >= 0 andalso Interval =< 65535 ->
    call_client(Mtns, ClientId, {keepalive, Interval});
set_keepalive(_Mtns, _ClientId, _Interval) ->
    {error, <<"mqtt3.1.1 specification: keepalive must between 0~65535">>}.

%% @private
call_client(Mtns, ClientId, Req) ->
    case emqx_cm_registry:is_enabled() of
        true ->
            do_call_client(Mtns, ClientId, Req);
        false ->
            call_client_on_all_nodes(Mtns, ClientId, Req)
    end.

call_client_on_all_nodes(Mtns, ClientId, Req) ->
    Nodes = emqx:running_nodes(),
    Results = call_client(Nodes, Mtns, ClientId, Req),
    {Expected, Errs} = lists:foldr(
        fun
            ({_N, {error, not_found}}, Acc) -> Acc;
            ({_N, {error, _}} = Err, {OkAcc, ErrAcc}) -> {OkAcc, [Err | ErrAcc]};
            ({_N, OkRes}, {OkAcc, ErrAcc}) -> {[OkRes | OkAcc], ErrAcc}
        end,
        {[], []},
        lists:zip(Nodes, Results)
    ),
    ?maybe_log_node_errors(#{msg => "call_client_failed", request => Req}, Errs),
    case Expected of
        [] ->
            case Errs of
                [] -> {error, not_found};
                [{_Node, FirstErr} | _] -> FirstErr
            end;
        [Result | _] ->
            Result
    end.

%% @deprecated
-spec do_call_client(emqx_types:clientid(), term()) -> term().
do_call_client(ClientId, Req) ->
    do_call_client(undefined, ClientId, Req).

%% @private
-spec do_call_client(emqx_types:mtns(), emqx_types:clientid(), term()) -> term().
do_call_client(Mtns, ClientId, Req) ->
    %% FIXME:
    case emqx_cm:lookup_channels(Mtns, ClientId) of
        [] ->
            {error, not_found};
        Pids when is_list(Pids) ->
            Pid = lists:last(Pids),
            case emqx_cm:get_chan_info(Mtns, ClientId, Pid) of
                #{conninfo := #{conn_mod := ConnMod}} ->
                    call_conn(ConnMod, Pid, Req);
                undefined ->
                    {error, not_found}
            end
    end.

%% @private
call_client(Nodes, Mtns, ClientId, Req) ->
    emqx_rpc:unwrap_erpc(emqx_management_proto_v6:call_client(Nodes, Mtns, ClientId, Req)).

%%--------------------------------------------------------------------
%% Subscriptions
%%--------------------------------------------------------------------

-spec do_list_subscriptions() -> no_return().
do_list_subscriptions() ->
    %% [FIXME] Add function to `emqx_broker` that returns list of subscriptions
    %% and either redirect from here or bpapi directly (EMQX-8993).
    throw(not_implemented).

list_subscriptions(Node) ->
    unwrap_rpc(emqx_management_proto_v5:list_subscriptions(Node)).

list_subscriptions_via_topic(Topic, FormatFun) ->
    lists:append([
        list_subscriptions_via_topic(Node, Topic, FormatFun)
     || Node <- emqx:running_nodes()
    ]).

list_subscriptions_via_topic(Node, Topic, _FormatFun = {M, F}) ->
    case unwrap_rpc(emqx_broker_proto_v1:list_subscriptions_via_topic(Node, Topic)) of
        {error, Reason} -> {error, Reason};
        Result -> M:F(Result)
    end.

%%--------------------------------------------------------------------
%% PubSub
%%--------------------------------------------------------------------

subscribe(Mtns, ClientId, TopicTables) ->
    case emqx_cm_registry:is_enabled() of
        false ->
            subscribe(emqx:running_nodes(), Mtns, ClientId, TopicTables);
        true ->
            with_client_node(
                Mtns,
                ClientId,
                {error, channel_not_found},
                fun(Node) ->
                    subscribe([Node], Mtns, ClientId, TopicTables)
                end
            )
    end.

subscribe([Node | Nodes], Mtns, ClientId, TopicTables) ->
    case unwrap_rpc(emqx_management_proto_v6:subscribe(Node, Mtns, ClientId, TopicTables)) of
        {error, _} -> subscribe(Nodes, Mtns, ClientId, TopicTables);
        {subscribe, Res} -> {subscribe, Res, Node}
    end;
subscribe([], _Mtns, _ClientId, _TopicTables) ->
    {error, channel_not_found}.

%% @deprecated Used by emqx_management_proto_v5 and before version
-spec do_subscribe(emqx_types:clientid(), emqx_types:topic_filters()) ->
    {subscribe, _} | {error, atom()}.
do_subscribe(ClientId, TopicTables) ->
    do_subscribe(?GBNS, ClientId, TopicTables).

-spec do_subscribe(emqx_types:mtns(), emqx_types:clientid(), emqx_types:topic_filters()) ->
    {subscribe, _} | {error, atom()}.
do_subscribe(Mtns, ClientId, TopicTables) ->
    case ets:lookup(?CHAN_TAB, {Mtns, ClientId}) of
        [] -> {error, channel_not_found};
        [{_, Pid}] -> Pid ! {subscribe, TopicTables}
    end.

publish(Msg) ->
    emqx_metrics:inc_msg(Msg),
    emqx:publish(Msg).

-spec unsubscribe(emqx_types:mtns(), emqx_types:clientid(), emqx_types:topic()) ->
    {unsubscribe, _} | {error, channel_not_found}.
unsubscribe(Mtns, ClientId, Topic) ->
    unsubscribe(emqx:running_nodes(), Mtns, ClientId, Topic).

-spec unsubscribe([node()], emqx_types:mtns(), emqx_types:clientid(), emqx_types:topic()) ->
    {unsubscribe, _} | {error, channel_not_found}.
unsubscribe([Node | Nodes], Mtns, ClientId, Topic) ->
    case unwrap_rpc(emqx_management_proto_v6:unsubscribe(Node, Mtns, ClientId, Topic)) of
        {error, _} -> unsubscribe(Nodes, Mtns, ClientId, Topic);
        Re -> Re
    end;
unsubscribe([], _Mtns, _ClientId, _Topic) ->
    {error, channel_not_found}.

-spec do_unsubscribe(emqx_types:clientid(), emqx_types:topic()) ->
    {unsubscribe, _} | {error, _}.
do_unsubscribe(ClientId, Topic) ->
    do_unsubscribe(?GBNS, ClientId, Topic).

-spec do_unsubscribe(emqx_types:mtns(), emqx_types:clientid(), emqx_types:topic()) ->
    {unsubscribe, _} | {error, _}.
do_unsubscribe(Mtns, ClientId, Topic) ->
    case ets:lookup(?CHAN_TAB, {Mtns, ClientId}) of
        [] -> {error, channel_not_found};
        [{_, Pid}] -> Pid ! {unsubscribe, [emqx_topic:parse(Topic)]}
    end.

-spec unsubscribe_batch(emqx_types:mtns(), emqx_types:clientid(), [emqx_types:topic()]) ->
    {unsubscribe, _} | {error, channel_not_found}.
unsubscribe_batch(Mtns, ClientId, Topics) ->
    case emqx_cm_registry:is_enabled() of
        false ->
            unsubscribe_batch(emqx:running_nodes(), Mtns, ClientId, Topics);
        true ->
            with_client_node(
                Mtns,
                ClientId,
                {error, channel_not_found},
                fun(Node) ->
                    unsubscribe_batch([Node], Mtns, ClientId, Topics)
                end
            )
    end.

-spec unsubscribe_batch([node()], emqx_types:mtns(), emqx_types:clientid(), [emqx_types:topic()]) ->
    {unsubscribe_batch, _} | {error, channel_not_found}.
unsubscribe_batch([Node | Nodes], Mtns, ClientId, Topics) ->
    case unwrap_rpc(emqx_management_proto_v6:unsubscribe_batch(Node, Mtns, ClientId, Topics)) of
        {error, _} -> unsubscribe_batch(Nodes, Mtns, ClientId, Topics);
        Re -> Re
    end;
unsubscribe_batch([], _Mtns, _ClientId, _Topics) ->
    {error, channel_not_found}.

-spec do_unsubscribe_batch(emqx_types:clientid(), [emqx_types:topic()]) ->
    {unsubscribe_batch, _} | {error, _}.
do_unsubscribe_batch(ClientId, Topics) ->
    do_unsubscribe_batch(?GBNS, ClientId, Topics).

-spec do_unsubscribe_batch(emqx_types:mtns(), emqx_types:clientid(), [emqx_types:topic()]) ->
    {unsubscribe_batch, _} | {error, _}.
do_unsubscribe_batch(Mtns, ClientId, Topics) ->
    case ets:lookup(?CHAN_TAB, {Mtns, ClientId}) of
        [] -> {error, channel_not_found};
        [{_, Pid}] -> Pid ! {unsubscribe, [emqx_topic:parse(Topic) || Topic <- Topics]}
    end.

%%--------------------------------------------------------------------
%% Get Alarms
%%--------------------------------------------------------------------

get_alarms(Type) ->
    [{Node, get_alarms(Node, Type)} || Node <- emqx:running_nodes()].

get_alarms(Node, Type) ->
    add_duration_field(unwrap_rpc(emqx_proto_v1:get_alarms(Node, Type))).

deactivate(Node, Name) ->
    unwrap_rpc(emqx_proto_v1:deactivate_alarm(Node, Name)).

delete_all_deactivated_alarms() ->
    [delete_all_deactivated_alarms(Node) || Node <- emqx:running_nodes()].

delete_all_deactivated_alarms(Node) ->
    unwrap_rpc(emqx_proto_v1:delete_all_deactivated_alarms(Node)).

add_duration_field(Alarms) ->
    Now = erlang:system_time(microsecond),
    add_duration_field(Alarms, Now, []).

add_duration_field([], _Now, Acc) ->
    Acc;
add_duration_field([Alarm = #{activated := true, activate_at := ActivateAt} | Rest], Now, Acc) ->
    add_duration_field(Rest, Now, [Alarm#{duration => Now - ActivateAt} | Acc]);
add_duration_field(
    [
        Alarm = #{
            activated := false,
            activate_at := ActivateAt,
            deactivate_at := DeactivateAt
        }
        | Rest
    ],
    Now,
    Acc
) ->
    add_duration_field(Rest, Now, [Alarm#{duration => DeactivateAt - ActivateAt} | Acc]).

%%--------------------------------------------------------------------
%% Banned API
%%--------------------------------------------------------------------

create_banned(Banned) ->
    emqx_banned:create(Banned).

delete_banned(Who) ->
    emqx_banned:delete(Who).

%%--------------------------------------------------------------------
%% Internal exports
%%--------------------------------------------------------------------

lookup_running_client(Mtns, ClientId, FormatFun) ->
    case emqx_cm_registry:is_enabled() of
        false ->
            lists:append([
                lookup_client(Node, Mtns, {clientid, ClientId}, FormatFun)
             || Node <- emqx:running_nodes()
            ]);
        true ->
            with_client_node(
                Mtns,
                ClientId,
                _WhenNotFound = [],
                fun(Node) -> lookup_client(Node, Mtns, {clientid, ClientId}, FormatFun) end
            )
    end.

%%--------------------------------------------------------------------
%% Internal Functions.
%%--------------------------------------------------------------------

with_client_node(Mtns, ClientId, WhenNotFound, Fn) ->
    case emqx_cm_registry:lookup_channels(Mtns, ClientId) of
        [ChanPid | _] ->
            Node = node(ChanPid),
            Fn(Node);
        [] ->
            WhenNotFound
    end.

unwrap_rpc({badrpc, Reason}) ->
    {error, Reason};
unwrap_rpc(Res) ->
    Res.

otp_rel() ->
    iolist_to_binary([emqx_vm:get_otp_version(), "/", erlang:system_info(version)]).

check_results(Results) ->
    case lists:any(fun(Item) -> Item =:= ok end, Results) of
        true -> ok;
        false -> unwrap_rpc(lists:last(Results))
    end.

default_row_limit() ->
    ?DEFAULT_ROW_LIMIT.

call_conn(ConnMod, Pid, Req) ->
    try
        erlang:apply(ConnMod, call, [Pid, Req])
    catch
        exit:R when R =:= shutdown; R =:= normal ->
            {error, shutdown};
        exit:{R, _} when R =:= shutdown; R =:= noproc ->
            {error, shutdown};
        exit:{{shutdown, _OOMInfo}, _Location} ->
            {error, shutdown};
        exit:timeout ->
            LogData = #{
                msg => "call_client_connection_process_timeout",
                request => Req,
                pid => Pid,
                module => ConnMod
            },
            LogData1 =
                case node(Pid) =:= node() of
                    true ->
                        LogData#{stacktrace => erlang:process_info(Pid, current_stacktrace)};
                    false ->
                        LogData
                end,

            ?SLOG(warning, LogData1),
            {error, timeout}
    end.
