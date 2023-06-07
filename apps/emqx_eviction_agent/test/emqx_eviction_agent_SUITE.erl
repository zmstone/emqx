%%--------------------------------------------------------------------
%% Copyright (c) 2022-2023 EMQ Technologies Co., Ltd. All Rights Reserved.
%%--------------------------------------------------------------------

-module(emqx_eviction_agent_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("emqx/include/emqx_mqtt.hrl").
-include_lib("emqx/include/asserts.hrl").
-include_lib("emqx/include/emqx_cm.hrl").

-import(
    emqx_eviction_agent_test_helpers,
    [emqtt_connect/0, emqtt_connect/1, emqtt_connect/2]
).

-define(assertPrinted(Printed, Code),
    ?assertMatch(
        {match, _},
        re:run(Code, Printed)
    )
).

all() ->
    emqx_common_test_helpers:all(?MODULE).

init_per_suite(Config) ->
    emqx_common_test_helpers:start_apps([emqx_eviction_agent]),
    Config.

end_per_suite(_Config) ->
    emqx_common_test_helpers:stop_apps([emqx_eviction_agent]).

init_per_testcase(Case, Config) ->
    _ = emqx_eviction_agent:disable(test_eviction),
    ok = snabbkaffe:start_trace(),
    start_slave(Case, Config).

start_slave(t_explicit_session_takeover, Config) ->
    ClusterNodes = emqx_eviction_agent_test_helpers:start_cluster(
        [{evacuate_test1, 2883}, {evacuate_test2, 3883}],
        [emqx_eviction_agent]
    ),
    [{evacuate_nodes, ClusterNodes} | Config];
start_slave(_Case, Config) ->
    Config.

end_per_testcase(TestCase, Config) ->
    emqx_eviction_agent:disable(test_eviction),
    ok = snabbkaffe:stop(),
    stop_slave(TestCase, Config).

stop_slave(t_explicit_session_takeover, Config) ->
    emqx_eviction_agent_test_helpers:stop_cluster(
        ?config(evacuate_nodes, Config),
        [emqx_eviction_agent]
    );
stop_slave(_Case, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% Tests
%%--------------------------------------------------------------------

t_enable_disable(_Config) ->
    erlang:process_flag(trap_exit, true),

    ?assertMatch(
        disabled,
        emqx_eviction_agent:status()
    ),

    {ok, C0} = emqtt_connect(),
    ok = emqtt:disconnect(C0),

    ok = emqx_eviction_agent:enable(test_eviction, undefined),

    ?assertMatch(
        {error, eviction_agent_busy},
        emqx_eviction_agent:enable(bar, undefined)
    ),

    ?assertMatch(
        ok,
        emqx_eviction_agent:enable(test_eviction, <<"srv">>)
    ),

    ?assertMatch(
        {enabled, #{}},
        emqx_eviction_agent:status()
    ),

    ?assertMatch(
        {error, {use_another_server, #{}}},
        emqtt_connect()
    ),

    ?assertMatch(
        {error, eviction_agent_busy},
        emqx_eviction_agent:disable(bar)
    ),

    ?assertMatch(
        ok,
        emqx_eviction_agent:disable(test_eviction)
    ),

    ?assertMatch(
        {error, disabled},
        emqx_eviction_agent:disable(test_eviction)
    ),

    ?assertMatch(
        disabled,
        emqx_eviction_agent:status()
    ),

    {ok, C1} = emqtt_connect(),
    ok = emqtt:disconnect(C1).

t_evict_connections_status(_Config) ->
    erlang:process_flag(trap_exit, true),

    {ok, _C} = emqtt_connect(),

    {error, disabled} = emqx_eviction_agent:evict_connections(1),

    ok = emqx_eviction_agent:enable(test_eviction, undefined),

    ?assertMatch(
        {enabled, #{connections := 1, sessions := _}},
        emqx_eviction_agent:status()
    ),

    ok = emqx_eviction_agent:evict_connections(1),

    ct:sleep(100),

    ?assertMatch(
        {enabled, #{connections := 0, sessions := _}},
        emqx_eviction_agent:status()
    ),

    ok = emqx_eviction_agent:disable(test_eviction).

t_explicit_session_takeover(Config) ->
    _ = erlang:process_flag(trap_exit, true),
    ok = restart_emqx(),

    [{Node1, Port1}, {Node2, _Port2}] = ?config(evacuate_nodes, Config),

    {ok, C0} = emqtt_connect([
        {clientid, <<"client_with_session">>},
        {clean_start, false},
        {port, Port1}
    ]),
    {ok, _, _} = emqtt:subscribe(C0, <<"t1">>),

    ?assertEqual(
        1,
        rpc:call(Node1, emqx_eviction_agent, connection_count, [])
    ),

    [ChanPid] = rpc:call(Node1, emqx_cm, lookup_channels, [<<"client_with_session">>]),

    ok = rpc:call(Node1, emqx_eviction_agent, enable, [test_eviction, undefined]),

    ?assertWaitEvent(
        begin
            ok = rpc:call(Node1, emqx_eviction_agent, evict_connections, [1]),
            receive
                {'EXIT', C0, {disconnected, ?RC_USE_ANOTHER_SERVER, _}} -> ok
            after 1000 ->
                ?assert(false, "Connection not evicted")
            end
        end,
        #{?snk_kind := emqx_cm_connected_client_count_dec_done, chan_pid := ChanPid},
        2000
    ),

    ?assertEqual(
        0,
        rpc:call(Node1, emqx_eviction_agent, connection_count, [])
    ),

    ?assertEqual(
        1,
        rpc:call(Node1, emqx_eviction_agent, session_count, [])
    ),

    %% First, evacuate to the same node

    ?assertWaitEvent(
        rpc:call(Node1, emqx_eviction_agent, evict_sessions, [1, Node1]),
        #{?snk_kind := emqx_channel_takeover_end, clientid := <<"client_with_session">>},
        1000
    ),

    ok = rpc:call(Node1, emqx_eviction_agent, disable, [test_eviction]),

    {ok, C1} = emqtt_connect([{port, Port1}]),
    emqtt:publish(C1, <<"t1">>, <<"MessageToEvictedSession1">>),
    ok = emqtt:disconnect(C1),

    ok = rpc:call(Node1, emqx_eviction_agent, enable, [test_eviction, undefined]),

    %% Evacuate to another node

    ?assertWaitEvent(
        rpc:call(Node1, emqx_eviction_agent, evict_sessions, [1, Node2]),
        #{?snk_kind := emqx_channel_takeover_end, clientid := <<"client_with_session">>},
        1000
    ),

    ?assertEqual(
        0,
        rpc:call(Node1, emqx_eviction_agent, session_count, [])
    ),

    ?assertEqual(
        1,
        rpc:call(Node2, emqx_eviction_agent, session_count, [])
    ),

    ok = rpc:call(Node1, emqx_eviction_agent, disable, [test_eviction]),

    %% Session is on Node2, but we connect to Node1
    {ok, C2} = emqtt_connect([{port, Port1}]),
    emqtt:publish(C2, <<"t1">>, <<"MessageToEvictedSession2">>),
    ok = emqtt:disconnect(C2),

    ct:sleep(100),

    %% Session is on Node2, but we connect the subscribed client to Node1
    %% It should take over the session for the third time and recieve
    %% previously published messages
    {ok, C3} = emqtt_connect([
        {clientid, <<"client_with_session">>},
        {clean_start, false},
        {port, Port1}
    ]),

    ok = assert_receive_publish(
        [
            #{payload => <<"MessageToEvictedSession1">>, topic => <<"t1">>},
            #{payload => <<"MessageToEvictedSession2">>, topic => <<"t1">>}
        ]
    ),
    ok = emqtt:disconnect(C3).

t_disable_on_restart(_Config) ->
    ok = emqx_eviction_agent:enable(test_eviction, undefined),

    ok = supervisor:terminate_child(emqx_eviction_agent_sup, emqx_eviction_agent),
    {ok, _} = supervisor:restart_child(emqx_eviction_agent_sup, emqx_eviction_agent),

    ?assertEqual(
        disabled,
        emqx_eviction_agent:status()
    ).

t_session_serialization(_Config) ->
    _ = erlang:process_flag(trap_exit, true),
    ok = restart_emqx(),

    {ok, C0} = emqtt_connect(<<"client_with_session">>, false),
    {ok, _, _} = emqtt:subscribe(C0, <<"t1">>),
    ok = emqtt:disconnect(C0),

    ok = emqx_eviction_agent:enable(test_eviction, undefined),

    ?assertEqual(
        1,
        emqx_eviction_agent:session_count()
    ),

    %% Evacuate to the same node

    ?assertWaitEvent(
        emqx_eviction_agent:evict_sessions(1, node()),
        #{?snk_kind := emqx_channel_takeover_end, clientid := <<"client_with_session">>},
        1000
    ),

    ok = emqx_eviction_agent:disable(test_eviction),

    ?assertEqual(
        1,
        emqx_eviction_agent:session_count()
    ),

    ?assertMatch(
        #{data := [#{clientid := <<"client_with_session">>}]},
        emqx_mgmt_api:cluster_query(
            ?CHAN_INFO_TAB,
            #{},
            [],
            fun emqx_mgmt_api_clients:qs2ms/2,
            fun emqx_mgmt_api_clients:format_channel_info/2
        )
    ),

    mock_print(),

    ?assertPrinted(
        "client_with_session",
        emqx_mgmt_cli:clients(["list"])
    ),

    ?assertPrinted(
        "client_with_session",
        emqx_mgmt_cli:clients(["show", "client_with_session"])
    ),

    ?assertWaitEvent(
        emqx_cm:kick_session(<<"client_with_session">>),
        #{?snk_kind := emqx_cm_clean_down, client_id := <<"client_with_session">>},
        1000
    ),

    ?assertEqual(
        0,
        emqx_eviction_agent:session_count()
    ).

t_will_msg(_Config) ->
    erlang:process_flag(trap_exit, true),

    WillMsg = <<"will_msg">>,
    WillTopic = <<"will_topic">>,
    ClientId = <<"client_with_will">>,

    _ = emqtt_connect([
        {clean_start, false},
        {clientid, ClientId},
        {will_payload, WillMsg},
        {will_topic, WillTopic}
    ]),

    {ok, C} = emqtt_connect(),
    {ok, _, _} = emqtt:subscribe(C, WillTopic),

    [ChanPid] = emqx_cm:lookup_channels(ClientId),

    ChanPid !
        {disconnect, ?RC_USE_ANOTHER_SERVER, use_another_server, #{
            'Server-Reference' => <<>>
        }},

    receive
        {publish, #{
            payload := WillMsg,
            topic := WillTopic
        }} ->
            ok
    after 1000 ->
        ct:fail("Will message not received")
    end,

    ok = emqtt:disconnect(C).

t_ws_conn(_Config) ->
    erlang:process_flag(trap_exit, true),

    ClientId = <<"ws_client">>,
    {ok, C} = emqtt:start_link([
        {proto_ver, v5},
        {clientid, ClientId},
        {port, 8083},
        {ws_path, "/mqtt"}
    ]),
    {ok, _} = emqtt:ws_connect(C),

    ok = emqx_eviction_agent:enable(test_eviction, undefined),

    ?assertEqual(
        1,
        emqx_eviction_agent:connection_count()
    ),

    ?assertWaitEvent(
        ok = emqx_eviction_agent:evict_connections(1),
        #{?snk_kind := emqx_cm_connected_client_count_dec_done},
        1000
    ),

    ?assertEqual(
        0,
        emqx_eviction_agent:connection_count()
    ).

-ifndef(BUILD_WITHOUT_QUIC).

t_quic_conn(_Config) ->
    erlang:process_flag(trap_exit, true),

    QuicPort = emqx_common_test_helpers:select_free_port(quic),
    application:ensure_all_started(quicer),
    emqx_common_test_helpers:ensure_quic_listener(?MODULE, QuicPort),

    ClientId = <<"quic_client">>,
    {ok, C} = emqtt:start_link([
        {proto_ver, v5},
        {clientid, ClientId},
        {port, QuicPort}
    ]),
    {ok, _} = emqtt:quic_connect(C),

    ok = emqx_eviction_agent:enable(test_eviction, undefined),

    ?assertEqual(
        1,
        emqx_eviction_agent:connection_count()
    ),

    ?assertWaitEvent(
        ok = emqx_eviction_agent:evict_connections(1),
        #{?snk_kind := emqx_cm_connected_client_count_dec_done},
        1000
    ),

    ?assertEqual(
        0,
        emqx_eviction_agent:connection_count()
    ).

-endif.

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

assert_receive_publish([]) ->
    ok;
assert_receive_publish([#{payload := Msg, topic := Topic} | Rest]) ->
    receive
        {publish, #{
            payload := Msg,
            topic := Topic
        }} ->
            assert_receive_publish(Rest)
    after 1000 ->
        ?assert(false, "Message `" ++ binary_to_list(Msg) ++ "` is lost")
    end.

connect_and_publish(Topic, Message) ->
    {ok, C} = emqtt_connect(),
    emqtt:publish(C, Topic, Message),
    ok = emqtt:disconnect(C).

restart_emqx() ->
    _ = application:stop(emqx),
    _ = application:start(emqx),
    _ = application:stop(emqx_eviction_agent),
    _ = application:start(emqx_eviction_agent),
    ok.

mock_print() ->
    catch meck:unload(emqx_ctl),
    meck:new(emqx_ctl, [non_strict, passthrough]),
    meck:expect(emqx_ctl, print, fun(Arg) -> emqx_ctl:format(Arg, []) end),
    meck:expect(emqx_ctl, print, fun(Msg, Arg) -> emqx_ctl:format(Msg, Arg) end),
    meck:expect(emqx_ctl, usage, fun(Usages) -> emqx_ctl:format_usage(Usages) end),
    meck:expect(emqx_ctl, usage, fun(Cmd, Descr) -> emqx_ctl:format_usage(Cmd, Descr) end).
