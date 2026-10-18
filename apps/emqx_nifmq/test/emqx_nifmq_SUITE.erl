%%--------------------------------------------------------------------
%% Copyright (c) 2026 EMQ Technologies Co., Ltd. All Rights Reserved.
%%--------------------------------------------------------------------
-module(emqx_nifmq_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include_lib("emqx_utils/include/emqx_message.hrl").

suite() -> [{timetrap, {minutes, 5}}].

all() -> emqx_common_test_helpers:all(?MODULE).

init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(_Case, _Config) ->
    ok.

%%------------------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------------------

msg(Topic, Payload) ->
    msg(Topic, Payload, #{}).

msg(Topic, Payload, Headers) ->
    #message{
        id = <<0:128>>,
        qos = 0,
        from = <<"pub">>,
        flags = #{},
        headers = Headers,
        topic = Topic,
        payload = Payload,
        timestamp = 0,
        extra = #{}
    }.

topic() ->
    iolist_to_binary(io_lib:format("t/~p", [erlang:unique_integer([positive])])).

%% Start a process that owns a queue subscribed to Topic, and wait until it is ready.
start_sub(Topic, Cap, Fun) ->
    Parent = self(),
    Pid = spawn_link(fun() ->
        Q = emqx_nifmq:new_queue(Cap),
        ok = emqx_nifmq:subscribe(Q, Topic),
        Parent ! {ready, self()},
        Fun(Q)
    end),
    receive
        {ready, Pid} -> Pid
    end.

%% Drain until the queue has nothing left. Return the drained messages.
drain_all(Q, Max) ->
    drain_all(Q, Max, []).

drain_all(Q, Max, Acc) ->
    case emqx_nifmq:drain(Q, Max) of
        {Msgs, true} -> drain_all(Q, Max, [Msgs | Acc]);
        {Msgs, false} -> lists:append(lists:reverse([Msgs | Acc]))
    end.

flush_ready() ->
    receive
        nifmq_ready -> 1 + flush_ready()
    after 0 -> 0
    end.

wait_info(Key, Expected) ->
    wait_info(Key, Expected, 100).

wait_info(Key, Expected, 0) ->
    ?assertEqual(Expected, maps:get(Key, emqx_nifmq:info()));
wait_info(Key, Expected, N) ->
    case maps:get(Key, emqx_nifmq:info()) of
        Expected ->
            ok;
        _ ->
            timer:sleep(10),
            wait_info(Key, Expected, N - 1)
    end.

%%------------------------------------------------------------------------------
%% Cases
%%------------------------------------------------------------------------------

-doc "A drained message is equal to the published one, and order is kept.".
t_publish_drain_order(_Config) ->
    T = topic(),
    Q = emqx_nifmq:new_queue(10_000),
    ok = emqx_nifmq:subscribe(Q, T),
    Msgs = [msg(T, integer_to_binary(I), #{seq => I}) || I <- lists:seq(1, 1000)],
    lists:foreach(fun(M) -> ?assertEqual(1, emqx_nifmq:publish(M)) end, Msgs),
    ?assertEqual(1, flush_ready()),
    ?assertEqual(Msgs, drain_all(Q, 7)),
    ?assertMatch({0, false, false, 0}, emqx_nifmq:queue_info(Q)).

-doc "A publish to a topic without subscribers enqueues nothing.".
t_no_subscriber(_Config) ->
    ?assertEqual(0, emqx_nifmq:publish(msg(topic(), <<"x">>))).

-doc "Many publishes to an idle owner produce one notification.".
t_coalesced_wakeup(_Config) ->
    T = topic(),
    Q = emqx_nifmq:new_queue(1000),
    ok = emqx_nifmq:subscribe(Q, T),
    [1 = emqx_nifmq:publish(msg(T, <<"x">>)) || _ <- lists:seq(1, 100)],
    ?assertEqual(1, flush_ready()),
    {Batch, true} = emqx_nifmq:drain(Q, 60),
    ?assertEqual(60, length(Batch)),
    %% The owner still owes a drain, so a new publish sends nothing.
    1 = emqx_nifmq:publish(msg(T, <<"x">>)),
    ?assertEqual(0, flush_ready()),
    {Rest, false} = emqx_nifmq:drain(Q, 100),
    ?assertEqual(41, length(Rest)),
    %% The queue is empty and armed again.
    1 = emqx_nifmq:publish(msg(T, <<"x">>)),
    ?assertEqual(1, flush_ready()).

-doc "drain_view returns topic and payload as references to the stored message.".
t_drain_view_no_copy(_Config) ->
    T = topic(),
    Q = emqx_nifmq:new_queue(10),
    ok = emqx_nifmq:subscribe(Q, T),
    Small = <<"0123456789abcdef">>,
    Large = binary:copy(<<"x">>, 4096),
    1 = emqx_nifmq:publish(msg(T, Small)),
    1 = emqx_nifmq:publish(msg(T, Large)),
    {[{T1, P1} = V1, {T2, P2} = V2], false} = emqx_nifmq:drain_view(Q, 10),
    ?assertEqual({T, Small}, {T1, P1}),
    ?assertEqual({T, Large}, {T2, P2}),
    %% Off-heap binaries only: the heap part does not grow with the payload size.
    ?assertEqual(erts_debug:flat_size(V1), erts_debug:flat_size(V2)).

-doc "A full queue drops new messages for that queue only.".
t_cap_drop(_Config) ->
    T = topic(),
    Q1 = emqx_nifmq:new_queue(10),
    Q2 = emqx_nifmq:new_queue(100),
    ok = emqx_nifmq:subscribe(Q1, T),
    ok = emqx_nifmq:subscribe(Q2, T),
    Res = [emqx_nifmq:publish(msg(T, integer_to_binary(I))) || I <- lists:seq(1, 15)],
    ?assertEqual(lists:duplicate(10, 2) ++ lists:duplicate(5, 1), Res),
    ?assertMatch({10, true, false, 5}, emqx_nifmq:queue_info(Q1)),
    Payloads = [P || #message{payload = P} <- drain_all(Q1, 100)],
    ?assertEqual([integer_to_binary(I) || I <- lists:seq(1, 10)], Payloads),
    ?assertEqual(15, length(drain_all(Q2, 100))).

-doc "A congested queue accepts messages without notification, and re-arms on clear.".
t_congested(_Config) ->
    T = topic(),
    Q = emqx_nifmq:new_queue(100),
    ok = emqx_nifmq:subscribe(Q, T),
    ok = emqx_nifmq:set_congested(Q, true),
    [1 = emqx_nifmq:publish(msg(T, <<"x">>)) || _ <- lists:seq(1, 5)],
    ?assertEqual(0, flush_ready()),
    ?assertEqual(more, emqx_nifmq:set_congested(Q, false)),
    ?assertEqual(5, length(drain_all(Q, 2))),
    ?assertEqual(ok, emqx_nifmq:set_congested(Q, false)),
    1 = emqx_nifmq:publish(msg(T, <<"x">>)),
    ?assertEqual(1, flush_ready()).

-doc "When the owner exits, the queue unsubscribes and frees its messages.".
t_owner_down_cleanup(_Config) ->
    T = topic(),
    #{live_msgs := Live0, live_entries := Entries0} = emqx_nifmq:info(),
    Pid = start_sub(T, 100, fun(_Q) ->
        receive
            stop -> ok
        end
    end),
    [1 = emqx_nifmq:publish(msg(T, <<"x">>)) || _ <- lists:seq(1, 10)],
    unlink(Pid),
    MRef = monitor(process, Pid),
    Pid ! stop,
    receive
        {'DOWN', MRef, process, Pid, _} -> ok
    end,
    wait_info(live_entries, Entries0),
    wait_info(live_msgs, Live0),
    ?assertEqual(0, emqx_nifmq:publish(msg(T, <<"x">>))).

-doc "A fan-out larger than one NIF chunk reaches every subscriber.".
t_large_fanout(_Config) ->
    T = topic(),
    N = 1000,
    Self = self(),
    Pids = [
        start_sub(T, 10, fun(Q) ->
            receive
                nifmq_ready -> ok
            end,
            Self ! {got, self(), drain_all(Q, 10)}
        end)
     || _ <- lists:seq(1, N)
    ],
    M = msg(T, <<"x">>),
    ?assertEqual(N, emqx_nifmq:publish(M)),
    [
        receive
            {got, P, Got} -> ?assertEqual([M], Got)
        end
     || P <- Pids
    ].

-doc """
Stress the notification protocol. In each round, several producers publish a short burst
concurrently while the owner drains in small batches and sometimes toggles the congested
flag. The round ends only when the owner has every message of the burst, and the owner
blocks only in `receive nifmq_ready`. A lost wakeup strands the tail of a burst, so it
shows as a timeout. Per-producer order is checked across all rounds.
""".
t_no_lost_wakeup(_Config) ->
    T = topic(),
    NProd = 8,
    Rounds = 5000,
    Self = self(),
    Owner = start_sub(T, 100_000, fun(Q) ->
        rand:seed(exsss, {1, 2, 3}),
        consume(Q, Self, #{}, 0)
    end),
    Prods = [spawn_link(fun() -> producer(T, P, 0) end) || P <- lists:seq(1, NProd)],
    lists:foldl(
        fun(Round, Expected0) ->
            Burst = rand:uniform(4),
            [Pid ! {burst, Burst} || Pid <- Prods],
            Expected = Expected0 + NProd * Burst,
            wait_progress(Owner, Round, Expected),
            Expected
        end,
        0,
        lists:seq(1, Rounds)
    ).

producer(T, P, S) ->
    receive
        {burst, N} ->
            lists:foreach(
                fun(I) -> 1 = emqx_nifmq:publish(msg(T, <<>>, #{p => P, s => S + I})) end,
                lists:seq(1, N)
            ),
            producer(T, P, S + N)
    end.

wait_progress(Owner, Round, Expected) ->
    receive
        {progress, Expected} ->
            ok;
        {progress, N} when N < Expected ->
            wait_progress(Owner, Round, Expected)
    after 2_000 ->
        ct:fail({lost_wakeup, Round, Expected, process_info(Owner, current_function)})
    end.

consume(Q, Parent, Last, Count) ->
    receive
        nifmq_ready -> consume_more(Q, Parent, Last, Count)
    end.

consume_more(Q, Parent, Last0, Count0) ->
    {Msgs, More0} = emqx_nifmq:drain(Q, rand:uniform(3)),
    Last = check_order(Msgs, Last0),
    Count = Count0 + length(Msgs),
    Msgs =/= [] andalso (Parent ! {progress, Count}),
    More =
        case rand:uniform(20) of
            1 ->
                ok = emqx_nifmq:set_congested(Q, true),
                erlang:yield(),
                emqx_nifmq:set_congested(Q, false) =:= more orelse More0;
            _ ->
                More0
        end,
    case More of
        true -> consume_more(Q, Parent, Last, Count);
        false -> consume(Q, Parent, Last, Count)
    end.

check_order([], Last) ->
    Last;
check_order([#message{headers = #{p := P, s := S}} | Rest], Last) ->
    Prev = maps:get(P, Last, 0),
    S =:= Prev + 1 orelse exit({out_of_order, P, Prev, S}),
    check_order(Rest, Last#{P => S}).
