%%--------------------------------------------------------------------
%% Copyright (c) 2026 EMQ Technologies Co., Ltd. All Rights Reserved.
%%--------------------------------------------------------------------

%% Clean-down of disconnected channels on a replicant: the global registry
%% rows are removed in batches, with one call to a core node per batch.
-module(emqx_cm_clean_down_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("snabbkaffe/include/snabbkaffe.hrl").
-include("emqx_cm.hrl").

-define(N_CLIENTS, 100).
-define(CORE_RPC, {mria_lib, rpc_call_nothrow, 4}).

suite() -> [{timetrap, {minutes, 3}}].

all() -> emqx_common_test_helpers:all(?MODULE).

init_per_suite(Config) ->
    Apps = [emqx],
    Specs = emqx_cth_cluster:mk_nodespecs(
        [
            {cm_clean_down_core1, #{role => core, apps => Apps}},
            {cm_clean_down_repl1, #{role => replicant, apps => Apps}}
        ],
        #{work_dir => emqx_cth_suite:work_dir(Config)}
    ),
    [Core, Repl] = emqx_cth_cluster:start(Specs),
    [{core, Core}, {repl, Repl}, {specs, Specs} | Config].

end_per_suite(Config) ->
    emqx_cth_cluster:stop(?config(specs, Config)).

init_per_testcase(_TestCase, Config) ->
    snabbkaffe:start_trace(),
    Config.

end_per_testcase(_TestCase, Config) ->
    snabbkaffe:stop(),
    ok = resume_pool(?config(repl, Config)),
    ok = set_history_retain(Config, 0),
    ok = erpc:call(?config(core, Config), emqx_cm_registry_keeper, purge, []).

%%--------------------------------------------------------------------
%% Test cases
%%--------------------------------------------------------------------

-doc """
The clean-down tasks that are queued in an `emqx_cm_pool` worker on a replicant
remove their channels from the global registry in one batch, not one call to a
core node for each channel.
""".
t_batch_unregister(Config) ->
    Core = ?config(core, Config),
    Repl = ?config(repl, Config),
    ClientIds = client_ids(<<"batch">>),
    Workers = disconnect_with_pool_suspended(Repl, ClientIds),
    %% Count the calls from the replicant to a core node during the clean-down.
    _ = erpc:call(Repl, erlang, trace_pattern, [?CORE_RPC, true, [call_count]]),
    ok = wait_clean_down(Repl, Core),
    {call_count, Rpcs} = erpc:call(Repl, erlang, trace_info, [?CORE_RPC, call_count]),
    _ = erpc:call(Repl, erlang, trace_pattern, [?CORE_RPC, false, [call_count]]),
    Batches = ?of_kind(emqx_cm_registry_unregister_channels, snabbkaffe:collect_trace()),
    ct:pal("core RPCs: ~p, batches: ~p, pool workers: ~p", [Rpcs, length(Batches), Workers]),
    ?assert(Rpcs < ?N_CLIENTS div 2, #{rpcs => Rpcs, clients => ?N_CLIENTS}),
    ?assertEqual(?N_CLIENTS, lists:sum([C || #{count := C} <- Batches])),
    ?assert(length(Batches) =< Workers, #{batches => length(Batches), workers => Workers}).

-doc """
A client that reconnects to another node while its old channel on the
replicant waits for clean-down keeps its new registry entry.
""".
t_reconnect_to_other_node(Config) ->
    Core = ?config(core, Config),
    Repl = ?config(repl, Config),
    [ClientId] = ClientIds = [<<"reconnect-1">>],
    _ = disconnect_with_pool_suspended(Repl, ClientIds),
    [OldPid] = erpc:call(Core, emqx_cm_registry, lookup_all_channels, [ClientId]),
    ?assertEqual(Repl, node(OldPid)),
    {ok, C} = emqtt:start_link([
        {clientid, ClientId}, {port, emqx_cth_cluster:get_tcp_mqtt_port(Core)}
    ]),
    {ok, _} = emqtt:connect(C),
    ok = resume_pool(Repl),
    ?retry(100, 50, ?assertEqual(0, erpc:call(Repl, ets, info, [?CHAN_TAB, size]))),
    [NewPid] = erpc:call(Core, emqx_cm_registry, lookup_all_channels, [ClientId]),
    ?assertEqual(Core, node(NewPid)),
    ok = emqtt:stop(C).

-doc """
With the session registration history on, the batched clean-down leaves one
history row and no channel row for each client.
""".
t_batch_unregister_history(Config) ->
    Core = ?config(core, Config),
    Repl = ?config(repl, Config),
    ok = set_history_retain(Config, 3600),
    ClientIds = client_ids(<<"hist">>),
    _ = disconnect_with_pool_suspended(Repl, ClientIds),
    ok = resume_pool(Repl),
    ?retry(
        100,
        50,
        ?assertEqual(
            [[hist] || _ <- ClientIds],
            [
                [row_kind(R) || R <- erpc:call(Core, mnesia, dirty_read, [?CHAN_REG_TAB, Id])]
             || Id <- ClientIds
            ]
        )
    ).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

client_ids(Prefix) ->
    [<<Prefix/binary, "-", (integer_to_binary(I))/binary>> || I <- lists:seq(1, ?N_CLIENTS)].

%% Connect the clients to the replicant, suspend the clean-down workers, and
%% disconnect the clients. Return when every clean-down task is queued.
disconnect_with_pool_suspended(Repl, ClientIds) ->
    Port = emqx_cth_cluster:get_tcp_mqtt_port(Repl),
    Clients = [connect(Id, Port) || Id <- ClientIds],
    N = length(ClientIds),
    ?retry(100, 50, ?assertEqual(N, erpc:call(Repl, ets, info, [?CHAN_LIVE_TAB, size]))),
    Workers = pool_workers(Repl),
    ok = lists:foreach(fun(W) -> ok = erpc:call(Repl, sys, suspend, [W]) end, Workers),
    lists:foreach(fun emqtt:stop/1, Clients),
    ?retry(100, 50, ?assertEqual(0, erpc:call(Repl, ets, info, [?CHAN_LIVE_TAB, size]))),
    %% emqx_cm submits the tasks in the same callback that marks the channels
    %% disconnected. This call returns after that callback.
    _ = erpc:call(Repl, sys, get_state, [emqx_cm]),
    ?assertEqual(N, erpc:call(Repl, ets, info, [?CHAN_TAB, size])),
    length(Workers).

connect(ClientId, Port) ->
    {ok, C} = emqtt:start_link([{clientid, ClientId}, {port, Port}]),
    unlink(C),
    {ok, _} = emqtt:connect(C),
    C.

pool_workers(Node) ->
    [Pid || {_, Pid} <- erpc:call(Node, gproc_pool, active_workers, [?CM_POOL])].

resume_pool(Node) ->
    lists:foreach(
        fun(W) ->
            case erpc:call(Node, sys, get_status, [W]) of
                {status, _, _, [_, suspended | _]} -> ok = erpc:call(Node, sys, resume, [W]);
                _ -> ok
            end
        end,
        pool_workers(Node)
    ).

wait_clean_down(Repl, Core) ->
    ok = resume_pool(Repl),
    ?retry(100, 50, ?assertEqual(0, erpc:call(Repl, ets, info, [?CHAN_TAB, size]))),
    ?retry(100, 50, ?assertEqual(0, erpc:call(Core, emqx_cm_registry, table_size, []))),
    ok.

row_kind(#channel{pid = Pid}) when is_pid(Pid) -> Pid;
row_kind(#channel{pid = Ts}) when is_integer(Ts) -> hist.

set_history_retain(Config, Seconds) ->
    lists:foreach(
        fun(Node) ->
            ok = erpc:call(Node, emqx_config, put, [[broker, session_history_retain], Seconds])
        end,
        [?config(core, Config), ?config(repl, Config)]
    ).
