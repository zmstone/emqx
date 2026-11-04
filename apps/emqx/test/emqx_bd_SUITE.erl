%%--------------------------------------------------------------------
%% Copyright (c) 2026 EMQ Technologies Co., Ltd. All Rights Reserved.
%%--------------------------------------------------------------------
-module(emqx_bd_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

all() -> emqx_common_test_helpers:all(?MODULE).

init_per_suite(Config) ->
    Apps = emqx_cth_suite:start(
        [{emqx, "listeners.tcp.chunk { bind = \"127.0.0.1:21883\", parse_unit = chunk }"}],
        #{work_dir => emqx_cth_suite:work_dir(Config)}
    ),
    [{apps, Apps} | Config].

end_per_suite(Config) ->
    ok = emqx_bd:set_mode(off, off, 1),
    emqx_cth_suite:stop(?config(apps, Config)).

t_off(_Config) -> check_order(off).
t_push(_Config) -> check_order(push).
t_poll(_Config) -> check_order(poll).
t_ets(_Config) -> check_order(ets).
t_chunk(_Config) -> check_order(chunk).
t_chunkr(_Config) -> check_order(chunkr).

%% Several PUBLISH packets in one write, on a listener with parse_unit = chunk. The subscriber
%% gets them in order and in fewer sends.
t_chunk_one_write(_Config) ->
    {Sub, Sock} = chunk_setup(chunk),
    ok = gen_tcp:send(Sock, [publish_frame(I) || I <- lists:seq(1, 16)]),
    ?assertEqual(lists:seq(1, 16), lists:reverse(maps:get(Sub, recv(16, #{})))),
    ?assertMatch(#{rounds := 1, sends := 1, deliveries := 16}, emqx_bd:stats()),
    chunk_teardown(Sub, Sock).

%% With a cap of 4, 16 publishes in one write reach the subscriber in 4 batches, in order.
t_chunk_cap(_Config) ->
    {Sub, Sock} = chunk_setup(chunkr4),
    ok = gen_tcp:send(Sock, [publish_frame(I) || I <- lists:seq(1, 16)]),
    ?assertEqual(lists:seq(1, 16), lists:reverse(maps:get(Sub, recv(16, #{})))),
    ?assertMatch(#{local := chunkr, rounds := 1, sends := 4, deliveries := 16}, emqx_bd:stats()),
    chunk_teardown(Sub, Sock).

%% A DISCONNECT at the end of the chunk must not lose what the chunk collected.
t_chunk_disconnect_in_write(_Config) ->
    {Sub, Sock} = chunk_setup(chunk),
    ok = gen_tcp:send(Sock, [[publish_frame(I) || I <- lists:seq(1, 4)], <<16#E0, 0>>]),
    ?assertEqual(lists:seq(1, 4), lists:reverse(maps:get(Sub, recv(4, #{})))),
    ?assertMatch(#{rounds := 1, sends := 1, deliveries := 4}, emqx_bd:stats()),
    chunk_teardown(Sub, Sock).

chunk_setup(Mode) ->
    ok = emqx_bd:set_mode(Mode, off, 1),
    Sub = connect(<<"chunk_sub">>),
    {ok, _, [0]} = emqtt:subscribe(Sub, <<"bd/c">>, 0),
    {ok, Sock} = gen_tcp:connect("127.0.0.1", 21883, [binary, {active, false}, {nodelay, true}]),
    ok = gen_tcp:send(Sock, <<16#10, 14, 4:16, "MQTT", 4, 2, 60:16, 2:16, "cp">>),
    {ok, <<16#20, 2, 0, 0>>} = gen_tcp:recv(Sock, 4, 5000),
    {Sub, Sock}.

chunk_teardown(Sub, Sock) ->
    gen_tcp:close(Sock),
    ok = emqtt:stop(Sub),
    ok.

publish_frame(I) ->
    Payload = integer_to_binary(I),
    Body = <<4:16, "bd/c", Payload/binary>>,
    <<16#30, (byte_size(Body)), Body/binary>>.

check_order(Mode) ->
    ok = emqx_bd:set_mode(Mode, off, 2),
    Subs = [connect(<<"sub", (integer_to_binary(I))/binary>>) || I <- [1, 2, 3]],
    [{ok, _, [0]} = emqtt:subscribe(C, <<"bd/t">>, 0) || C <- Subs],
    Pub = connect(<<"pub">>),
    N = 2000,
    [ok = emqtt:publish(Pub, <<"bd/t">>, integer_to_binary(I), 0) || I <- lists:seq(1, N)],
    Got = recv(3 * N, #{}),
    ?assertEqual(3, map_size(Got)),
    [?assertEqual(lists:seq(1, N), lists:reverse(L)) || L <- maps:values(Got)],
    [ok = emqtt:stop(C) || C <- [Pub | Subs]].

connect(ClientId) ->
    {ok, C} = emqtt:start_link([{clientid, ClientId}, {proto_ver, v5}]),
    {ok, _} = emqtt:connect(C),
    C.

recv(0, Acc) ->
    Acc;
recv(N, Acc) ->
    receive
        {publish, #{client_pid := C, payload := P}} ->
            I = binary_to_integer(P),
            recv(N - 1, maps:update_with(C, fun(L) -> [I | L] end, [I], Acc))
    after 10000 ->
        ct:fail({missing, N, maps:map(fun(_, L) -> length(L) end, Acc)})
    end.
