%%--------------------------------------------------------------------
%% Copyright (c) 2026 EMQ Technologies Co., Ltd. All Rights Reserved.
%%--------------------------------------------------------------------
-module(emqx_nifmq_conn_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include_lib("emqx/include/emqx_mqtt.hrl").
-include_lib("snabbkaffe/include/snabbkaffe.hrl").

suite() -> [{timetrap, {minutes, 2}}].

all() -> emqx_common_test_helpers:all(?MODULE).

init_per_testcase(_Case, Config) ->
    ok = emqx_nifmq_conn:set_mode(inline, 1),
    Config.

end_per_testcase(_Case, _Config) ->
    ok = emqx_nifmq_conn:set_mode(inline, 1).

%%------------------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------------------

filter() ->
    iolist_to_binary(io_lib:format("t/~p", [erlang:unique_integer([positive])])).

%% Connect a client socket to a server socket that a new owner process attaches.
%% Return `{Conn, Owner, Client}`. The client socket is passive.
pair(AttachOpts) ->
    pair(AttachOpts, []).

pair(AttachOpts, ClientOpts) ->
    {ok, L} = gen_tcp:listen(0, [binary, {active, false}, {reuseaddr, true}]),
    {ok, Port} = inet:port(L),
    {ok, Client} = gen_tcp:connect(
        {127, 0, 0, 1}, Port, [binary, {active, false}, {packet, raw} | ClientOpts]
    ),
    {ok, Server} = gen_tcp:accept(L),
    ok = gen_tcp:close(L),
    Parent = self(),
    Owner = spawn(fun() ->
        receive
            go -> ok
        end,
        {ok, Fd} = inet:getfd(Server),
        Conn = emqx_nifmq_conn:attach(Fd, AttachOpts),
        Parent ! {attached, self(), Conn},
        owner_loop(Parent, Server)
    end),
    ok = gen_tcp:controlling_process(Server, Owner),
    Owner ! go,
    receive
        {attached, Owner, Conn} -> {Conn, Owner, Client}
    end.

%% Forward the messages of the native reader to the test process.
owner_loop(Parent, Server) ->
    receive
        stop ->
            gen_tcp:close(Server);
        {nifmq_data, _, _} = M ->
            Parent ! {owner, M},
            owner_loop(Parent, Server);
        {nifmq_closed, _, _} = M ->
            Parent ! {owner, M},
            owner_loop(Parent, Server)
    end.

v5() ->
    #{proto_ver => 5, clientid => <<"sub">>}.

parts(Topic, Payload, QoS) ->
    #{topic => Topic, payload => Payload, qos => QoS, from => <<"pub">>}.

parse_state(Ver) ->
    emqx_frame:initial_parse_state(#{version => Ver, expect_connect => false}).

%% Receive exactly N packets.
recv_packets(Sock, N, Ver) ->
    recv_packets(Sock, N, parse_state(Ver), <<>>, []).

recv_packets(_Sock, 0, _PS, _Buf, Acc) ->
    lists:reverse(Acc);
recv_packets(Sock, N, PS, Buf, Acc) ->
    case emqx_frame:parse(Buf, PS) of
        {#mqtt_packet{} = Pkt, Rest, PS1} ->
            recv_packets(Sock, N - 1, PS1, Rest, [Pkt | Acc]);
        {_More, PS1} ->
            {ok, Data} = gen_tcp:recv(Sock, 0, 5000),
            recv_packets(Sock, N, PS1, Data, Acc)
    end.

%% Assert that no data arrives within Ms milliseconds.
assert_silent(Sock, Ms) ->
    ?assertEqual({error, timeout}, gen_tcp:recv(Sock, 0, Ms)).

pub_fields(#mqtt_packet{
    header = #mqtt_packet_header{type = ?PUBLISH, qos = QoS, retain = Retain},
    variable = #mqtt_packet_publish{topic_name = T, packet_id = Id, properties = Props},
    payload = P
}) ->
    #{topic => T, qos => QoS, retain => Retain, id => Id, props => Props, payload => P}.

%%------------------------------------------------------------------------------
%% Cases
%%------------------------------------------------------------------------------

-doc "A QoS 0 MQTT 5 PUBLISH from the native encoder parses with emqx_frame.".
t_qos0_v5_encoding(_Config) ->
    F = filter(),
    {Conn, Owner, C} = pair(v5()),
    ok = emqx_nifmq_conn:subscribe(Conn, F, #{qos => 1, subid => 300}),
    %% Content-Type "a/b" as pre-encoded properties.
    Props = <<16#03, 3:16, "a/b">>,
    Payload = binary:copy(<<"x">>, 5000),
    ?assertEqual(1, emqx_nifmq_conn:dispatch(F, (parts(F, Payload, 0))#{props => Props})),
    [P] = recv_packets(C, 1, 5),
    ?assertMatch(
        #{
            topic := F,
            qos := 0,
            retain := false,
            payload := Payload,
            props := #{'Content-Type' := <<"a/b">>, 'Subscription-Identifier' := 300}
        },
        pub_fields(P)
    ),
    Owner ! stop.

-doc "An MQTT 3.1.1 subscriber gets no property section.".
t_qos0_v4_encoding(_Config) ->
    F = filter(),
    {Conn, Owner, C} = pair(#{proto_ver => 4, clientid => <<"sub">>}),
    ok = emqx_nifmq_conn:subscribe(Conn, F, #{qos => 0, subid => 7}),
    Props = <<16#03, 3:16, "a/b">>,
    1 = emqx_nifmq_conn:dispatch(F, (parts(F, <<"hello">>, 1))#{props => Props}),
    [P] = recv_packets(C, 1, 4),
    ?assertMatch(#{topic := F, qos := 0, payload := <<"hello">>}, pub_fields(P)),
    Owner ! stop.

-doc "No Local drops a message from the subscriber's own client ID.".
t_no_local(_Config) ->
    F = filter(),
    {Conn, Owner, C} = pair(v5()),
    ok = emqx_nifmq_conn:subscribe(Conn, F, #{qos => 0, nl => 1}),
    ?assertEqual(0, emqx_nifmq_conn:dispatch(F, (parts(F, <<"own">>, 0))#{from => <<"sub">>})),
    ?assertEqual(1, emqx_nifmq_conn:dispatch(F, parts(F, <<"other">>, 0))),
    [P] = recv_packets(C, 1, 5),
    ?assertMatch(#{payload := <<"other">>}, pub_fields(P)),
    Owner ! stop.

-doc "Retain As Published keeps the retain flag; without it the flag is cleared.".
t_retain_as_published(_Config) ->
    F = filter(),
    {ConnA, OwnerA, A} = pair(v5()),
    {ConnB, OwnerB, B} = pair(v5()),
    ok = emqx_nifmq_conn:subscribe(ConnA, F, #{qos => 0, rap => 1}),
    ok = emqx_nifmq_conn:subscribe(ConnB, F, #{qos => 0}),
    2 = emqx_nifmq_conn:dispatch(F, (parts(F, <<"r">>, 0))#{retain => true}),
    [PA] = recv_packets(A, 1, 5),
    [PB] = recv_packets(B, 1, 5),
    ?assertMatch(#{retain := true}, pub_fields(PA)),
    ?assertMatch(#{retain := false}, pub_fields(PB)),
    OwnerA ! stop,
    OwnerB ! stop.

-doc "The mountpoint prefix is removed from the topic.".
t_mountpoint(_Config) ->
    F = <<"mp/", (filter())/binary>>,
    {Conn, Owner, C} = pair((v5())#{mountpoint => <<"mp/">>}),
    ok = emqx_nifmq_conn:subscribe(Conn, F, #{qos => 0}),
    1 = emqx_nifmq_conn:dispatch(F, parts(F, <<"m">>, 0)),
    [P] = recv_packets(C, 1, 5),
    <<"mp/", Rest/binary>> = F,
    ?assertMatch(#{topic := Rest}, pub_fields(P)),
    Owner ! stop.

-doc "A message larger than the client's Maximum Packet Size is dropped.".
t_max_packet_size(_Config) ->
    F = filter(),
    {Conn, Owner, C} = pair((v5())#{max_packet_size => 100}),
    ok = emqx_nifmq_conn:subscribe(Conn, F, #{qos => 0}),
    0 = emqx_nifmq_conn:dispatch(F, parts(F, binary:copy(<<"x">>, 200), 0)),
    1 = emqx_nifmq_conn:dispatch(F, parts(F, <<"small">>, 0)),
    [P] = recv_packets(C, 1, 5),
    ?assertMatch(#{payload := <<"small">>}, pub_fields(P)),
    Owner ! stop.

-doc """
QoS 1 deliveries stop at the window size; each PUBACK releases one queued message, and all
messages arrive in order with distinct packet identifiers.
""".
t_qos1_window(_Config) ->
    F = filter(),
    {Conn, Owner, C} = pair((v5())#{window => 4}),
    ok = emqx_nifmq_conn:subscribe(Conn, F, #{qos => 1}),
    [1 = emqx_nifmq_conn:dispatch(F, parts(F, <<I:32>>, 1)) || I <- lists:seq(1, 10)],
    First = [pub_fields(P) || P <- recv_packets(C, 4, 5)],
    assert_silent(C, 200),
    ?assertMatch(#{inflight := 4, mqueue := 6}, emqx_nifmq_conn:info(Conn)),
    {Outstanding, Rest} = lists:foldl(
        fun(_, {[#{id := Id} | Out], Acc}) ->
            ok = emqx_nifmq_conn:puback(Conn, Id),
            [P] = [pub_fields(X) || X <- recv_packets(C, 1, 5)],
            {Out ++ [P], Acc ++ [P]}
        end,
        {First, []},
        lists:seq(1, 6)
    ),
    [ok = emqx_nifmq_conn:puback(Conn, Id) || #{id := Id} <- Outstanding],
    assert_silent(C, 200),
    ?assertMatch(#{inflight := 0, mqueue := 0}, emqx_nifmq_conn:info(Conn)),
    All = First ++ Rest,
    ?assertEqual([<<I:32>> || I <- lists:seq(1, 10)], [maps:get(payload, P) || P <- All]),
    ?assert(lists:all(fun(#{qos := Q}) -> Q =:= 1 end, All)),
    Ids = [maps:get(id, P) || P <- All],
    ?assertEqual(Ids, lists:usort(Ids)),
    Owner ! stop.

-doc "A PUBACK for a packet identifier that is not in flight returns not_found.".
t_puback_unknown(_Config) ->
    {Conn, Owner, _C} = pair(v5()),
    ?assertEqual({error, not_found}, emqx_nifmq_conn:puback(Conn, 42)),
    Owner ! stop.

-doc """
A subscriber that does not read loses QoS 0 messages once its buffer is full; it does not
block the dispatch, and another subscriber gets every message.
""".
t_slow_reader_drops_qos0(_Config) ->
    F = filter(),
    {SlowConn, SlowOwner, _Slow} = pair((v5())#{max_buffer_bytes => 65536}, [{recbuf, 4096}]),
    {FastConn, FastOwner, Fast} = pair(v5()),
    ok = emqx_nifmq_conn:subscribe(SlowConn, F, #{qos => 0}),
    ok = emqx_nifmq_conn:subscribe(FastConn, F, #{qos => 0}),
    Drop0 = maps:get(dropped_qos0, emqx_nifmq_conn:stats()),
    Payload = binary:copy(<<"y">>, 4096),
    N = 2000,
    Reader = spawn_link(fun() -> exit({received, length(recv_packets(Fast, N, 5))}) end),
    process_flag(trap_exit, true),
    {T, _} = timer:tc(fun() ->
        [emqx_nifmq_conn:dispatch(F, parts(F, Payload, 0)) || _ <- lists:seq(1, N)]
    end),
    receive
        {'EXIT', Reader, {received, Got}} -> ?assertEqual(N, Got)
    after 10000 -> ct:fail(fast_reader_timeout)
    end,
    ?assert(maps:get(dropped_qos0, emqx_nifmq_conn:stats()) > Drop0),
    ?assert(T < 5000000, T),
    SlowOwner ! stop,
    FastOwner ! stop.

-doc "Packets sent with send/2 and PUBLISH packets keep their order on the wire.".
t_send_order(_Config) ->
    F = filter(),
    {Conn, Owner, C} = pair(v5()),
    ok = emqx_nifmq_conn:subscribe(Conn, F, #{qos => 0}),
    %% The test parses with the broker-side parser, which accepts PUBACK.
    ok = emqx_nifmq_conn:send(Conn, emqx_frame:serialize(?PUBACK_PACKET(1), 5)),
    1 = emqx_nifmq_conn:dispatch(F, parts(F, <<"a">>, 0)),
    ok = emqx_nifmq_conn:send(Conn, emqx_frame:serialize(?PUBACK_PACKET(2), 5)),
    [P1, P2, P3] = recv_packets(C, 3, 5),
    ?assertMatch(#mqtt_packet{variable = #mqtt_packet_puback{packet_id = 1}}, P1),
    ?assertMatch(#{payload := <<"a">>}, pub_fields(P2)),
    ?assertMatch(#mqtt_packet{variable = #mqtt_packet_puback{packet_id = 2}}, P3),
    Owner ! stop.

-doc "When the owner exits, the native copy is closed and the client sees end of stream.".
t_owner_exit_closes(_Config) ->
    F = filter(),
    {Conn, Owner, C} = pair(v5()),
    ok = emqx_nifmq_conn:subscribe(Conn, F, #{qos => 0}),
    Owner ! stop,
    ?assertEqual({error, closed}, gen_tcp:recv(C, 0, 5000)),
    ?assertMatch(#{closed := true}, emqx_nifmq_conn:info(Conn)),
    ?assertEqual(0, emqx_nifmq_conn:dispatch(F, parts(F, <<"late">>, 0))).

-doc "In threads mode, a poller thread writes the packets.".
t_threads_mode(_Config) ->
    ok = emqx_nifmq_conn:set_mode(threads, 2),
    F = filter(),
    Pairs = [pair(v5()) || _ <- lists:seq(1, 4)],
    [ok = emqx_nifmq_conn:subscribe(Conn, F, #{qos => 1}) || {Conn, _, _} <- Pairs],
    Flushes0 = maps:get(thread_flushes, emqx_nifmq_conn:stats()),
    [4 = emqx_nifmq_conn:dispatch(F, parts(F, <<I:32>>, 1)) || I <- lists:seq(1, 3)],
    lists:foreach(
        fun({_Conn, Owner, C}) ->
            Ps = [pub_fields(P) || P <- recv_packets(C, 3, 5)],
            ?assertEqual([<<1:32>>, <<2:32>>, <<3:32>>], [maps:get(payload, P) || P <- Ps]),
            Owner ! stop
        end,
        Pairs
    ),
    ?assert(maps:get(thread_flushes, emqx_nifmq_conn:stats()) > Flushes0).

-doc """
With the native reader, a PUBACK frees the window slot without the owner, also when it
arrives one byte at a time. Other packets go to the owner.
""".
t_native_read_puback(_Config) ->
    F = filter(),
    {Conn, Owner, C} = pair((v5())#{window => 2}),
    ok = emqx_nifmq_conn:subscribe(Conn, F, #{qos => 1}),
    ok = emqx_nifmq_conn:read_native(Conn),
    ?assertMatch(#{native_read := true}, emqx_nifmq_conn:info(Conn)),
    [1 = emqx_nifmq_conn:dispatch(F, parts(F, <<I:32>>, 1)) || I <- lists:seq(1, 5)],
    [#{id := Id1}, #{id := Id2}] = [pub_fields(P) || P <- recv_packets(C, 2, 5)],
    assert_silent(C, 200),
    PingReq = iolist_to_binary(emqx_frame:serialize(?PACKET(?PINGREQ), 5)),
    ok = gen_tcp:send(C, [emqx_frame:serialize(?PUBACK_PACKET(Id1), 5), PingReq]),
    [#{id := Id3, payload := <<3:32>>}] = [pub_fields(P) || P <- recv_packets(C, 1, 5)],
    receive
        {owner, {nifmq_data, Conn, Data}} -> ?assertEqual(PingReq, iolist_to_binary(Data))
    after 5000 -> ct:fail(no_forwarded_data)
    end,
    Acks = iolist_to_binary([emqx_frame:serialize(?PUBACK_PACKET(Id), 5) || Id <- [Id2, Id3]]),
    [ok = gen_tcp:send(C, <<B>>) || <<B>> <= Acks],
    Rest = [pub_fields(P) || P <- recv_packets(C, 2, 5)],
    ?assertEqual([<<4:32>>, <<5:32>>], [maps:get(payload, P) || P <- Rest]),
    ok = gen_tcp:send(C, [emqx_frame:serialize(?PUBACK_PACKET(Id), 5) || #{id := Id} <- Rest]),
    ?retry(50, 100, ?assertMatch(#{inflight := 0, mqueue := 0}, emqx_nifmq_conn:info(Conn))),
    ok = gen_tcp:close(C),
    receive
        {owner, {nifmq_closed, Conn, eof}} -> ok
    after 5000 -> ct:fail(no_close)
    end,
    ?assertMatch(#{native_read := false}, emqx_nifmq_conn:info(Conn)),
    Owner ! stop.

-doc "The native reader stops on a malformed remaining length and tells the owner.".
t_native_read_malformed(_Config) ->
    {Conn, Owner, C} = pair(v5()),
    ok = emqx_nifmq_conn:read_native(Conn),
    ok = gen_tcp:send(C, <<16#40, 255, 255, 255, 255, 1>>),
    receive
        {owner, {nifmq_closed, Conn, malformed}} -> ok
    after 5000 -> ct:fail(no_close)
    end,
    Owner ! stop.
