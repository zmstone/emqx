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
    Apps = emqx_cth_suite:start([emqx], #{work_dir => emqx_cth_suite:work_dir(Config)}),
    [{apps, Apps} | Config].

end_per_suite(Config) ->
    ok = emqx_bd:set_mode(off, off, 1),
    emqx_cth_suite:stop(?config(apps, Config)).

t_off(_Config) -> check_order(off).
t_push(_Config) -> check_order(push).
t_poll(_Config) -> check_order(poll).
t_ets(_Config) -> check_order(ets).

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
