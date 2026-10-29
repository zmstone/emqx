-module(v4bench).
-export([run/1, workload/1, make_table/2, op/2]).

%% Usage: erl ... -run v4bench run <Mode> [Workload...]
%% Mode: mem | time | garbage
%%
%% Variants:
%%   v3      emqx_trie_search (dev-63), keys {Words, {Dest}}
%%   pr19558 emqx_trie_search of #19558 (binary topic), keys {Words, {Dest}}
%%   v4scan / v4lens / v4words  emqx_trie_search_v4, keys {Packed, Dest}
%% Each op is a router-like match: search + convert each key to {route, Topic, Dest}.

-define(VARIANTS, [v3, pr19558, v4scan, v4lens, v4words_dec, v4words_scan, v4words]).
-define(L,
    <<"abcdefghijabcdefghijabcdefghijabcdefghijabcdefghijabcdefghijabcdefghijabcdefghijabcdefghijabcdefghij">>
).
-define(T, <<"site/building/floor/device/temperature">>).

run([Mode | Ws]) ->
    Workloads =
        case Ws of
            [] -> all_workloads();
            _ -> [list_to_atom(W) || W <- Ws]
        end,
    _ = [run_mode(list_to_atom(Mode), W) || W <- Workloads],
    halt().

all_workloads() ->
    [
        hash,
        selective,
        overlap,
        duplicate_dests,
        long_plus,
        long_overlap,
        long_literal,
        sparse_1000,
        empty_levels,
        sys,
        big_dev_hit,
        big_dev_miss,
        big_mix
    ].

dest(I) -> list_to_atom("emqx-" ++ integer_to_list(I) ++ "@10.0.0." ++ integer_to_list(I rem 250)).
dest() -> dest(1).

combos([]) -> [[]];
combos([{A, B} | Rest]) -> [[X | T] || X <- [A, B], T <- combos(Rest)].

join(Ls) -> iolist_to_binary(lists:join(<<"/">>, Ls)).

%% {Routes :: [{Filter, Dest}], Topics :: [binary()]}
workload(hash) ->
    {[{<<"#">>, dest()}], [?T]};
workload(selective) ->
    {
        [
            {<<"site/+/floor/+/temperature">>, dest()},
            {<<"other/#">>, dest()},
            {<<"site/building/#">>, dest()}
        ],
        [?T]
    };
workload(overlap) ->
    Fs = [
        join(C ++ [<<"+">>])
     || C <- combos([
            {<<"site">>, <<"+">>},
            {<<"building">>, <<"+">>},
            {<<"floor">>, <<"+">>},
            {<<"device">>, <<"+">>}
        ])
    ],
    {[{F, dest()} || F <- Fs], [?T]};
workload(duplicate_dests) ->
    {[{<<"site/+/+/+/+">>, dest(I)} || I <- lists:seq(0, 63)], [?T]};
workload(long_plus) ->
    {[{<<"+/+/+/+">>, dest(I)} || I <- lists:seq(0, 15)], [join([?L, ?L, ?L, ?L])]};
workload(long_overlap) ->
    Fs = [
        join(C ++ [<<"+">>])
     || C <- combos([{?L, <<"+">>}, {?L, <<"+">>}, {?L, <<"+">>}, {?L, <<"+">>}])
    ],
    {[{F, dest()} || F <- Fs], [join([?L, ?L, ?L, ?L, <<"temperature">>])]};
workload(long_literal) ->
    {[{join([?L, ?L, ?L, <<"+">>]), dest()}], [join([?L, ?L, ?L, <<"x">>])]};
workload(sparse_1000) ->
    {[{join([<<"site">>, integer_to_binary(I), <<"+">>]), dest()} || I <- lists:seq(1, 1000)], [
        <<"site/500/temperature">>
    ]};
workload(empty_levels) ->
    {[{F, dest()} || F <- [<<"+/+/+/+/+">>, <<"/a/#">>, <<"/+/+/b/+">>, <<"/a//b/">>]], [
        <<"/a//b/">>
    ]};
workload(sys) ->
    {[{F, dest()} || F <- [<<"#">>, <<"+/#">>, <<"$SYS/broker/+/+">>]], [
        <<"$SYS/broker/node/uptime">>
    ]};
%% 100k per-device filters, 1 match per topic.
workload(big_dev_hit) ->
    {
        [
            {<<"devices/dev-", (integer_to_binary(I))/binary, "/+/cmd">>, dest()}
         || I <- lists:seq(1, 100000)
        ],
        [
            <<"devices/dev-", (integer_to_binary(I * 97 rem 100000 + 1))/binary, "/sensor-1/cmd">>
         || I <- lists:seq(1, 1000)
        ]
    };
%% Same table, topics that match nothing.
workload(big_dev_miss) ->
    {element(1, workload(big_dev_hit)), [
        <<"devices/dev-", (integer_to_binary(I * 97 rem 100000 + 1))/binary, "/sensor-1/status">>
     || I <- lists:seq(1, 1000)
    ]};
%% 100k mixed filters: tenants with 2 wildcard levels and a trailing #, plus shared subs.
workload(big_mix) ->
    Tenant = [
        {
            <<"tenant-", (integer_to_binary(T))/binary, "/site-", (integer_to_binary(S))/binary,
                "/+/+/#">>,
            dest(T rem 3)
        }
     || T <- lists:seq(1, 1000), S <- lists:seq(1, 50)
    ],
    Shared = [
        {<<"fleet/", (integer_to_binary(F))/binary, "/+/telemetry/#">>, {
            <<"g", (integer_to_binary(F rem 7))/binary>>, dest(F rem 3)
        }}
     || F <- lists:seq(1, 50000)
    ],
    Topics = [
        case I rem 2 of
            0 ->
                <<"tenant-", (integer_to_binary(I rem 1000 + 1))/binary, "/site-",
                    (integer_to_binary(I rem 50 + 1))/binary, "/line-3/dev-",
                    (integer_to_binary(I))/binary, "/temp">>;
            1 ->
                <<"fleet/", (integer_to_binary(I * 31 rem 50000 + 1))/binary, "/truck-",
                    (integer_to_binary(I))/binary, "/telemetry/gps">>
        end
     || I <- lists:seq(1, 1000)
    ],
    {Tenant ++ Shared, Topics}.

%% Table rows look like router rows: {routeidx, Key, []}.
make_table(Variant, Routes) ->
    Tab = ets:new(?MODULE, [
        ordered_set,
        public,
        {keypos, 2},
        {read_concurrency, true},
        {write_concurrency, true},
        {decentralized_counters, true}
    ]),
    MakeKey =
        case family(Variant) of
            v3 -> fun emqx_trie_search:make_key/2;
            v4 -> fun emqx_trie_search_v4:make_key/2
        end,
    true = ets:insert(Tab, [{routeidx, MakeKey(F, D), []} || {F, D} <- Routes]),
    Tab.

family(v3) -> v3;
family(pr19558) -> v3;
family(_) -> v4.

op(Variant, Tab) ->
    NextF = fun(K) -> ets:next(Tab, K) end,
    case Variant of
        v3 ->
            fun(T) ->
                [
                    {route, emqx_trie_search:get_topic(K), emqx_trie_search:get_id(K)}
                 || K <- emqx_trie_search:matches(T, NextF, [])
                ]
            end;
        pr19558 ->
            fun(T) ->
                [
                    {route, ts19558:get_topic(K), ts19558:get_id(K)}
                 || K <- ts19558:matches(T, NextF, [])
                ]
            end;
        v4words ->
            fun(T) ->
                [{route, F, D} || {F, D} <- emqx_trie_search_v4:match_routes(T, NextF, rebuild)]
            end;
        v4words_scan ->
            fun(T) ->
                [{route, F, D} || {F, D} <- emqx_trie_search_v4:match_routes(T, NextF, scan)]
            end;
        _ ->
            Opts = [{topic_index, strategy(Variant)}],
            fun(T) ->
                [
                    {route, emqx_trie_search_v4:decode(P), D}
                 || {P, D} <- emqx_trie_search_v4:matches(T, NextF, Opts)
                ]
            end
    end.

strategy(v4scan) -> scan;
strategy(v4lens) -> lens;
strategy(v4words_dec) -> words.

run_mode(mem, W) ->
    {Routes, _} = workload(W),
    Res = [
        begin
            Tab = make_table(V, Routes),
            Bytes = ets:info(Tab, memory) * erlang:system_info(wordsize),
            ets:delete(Tab),
            {V, Bytes}
        end
     || V <- [v3, v4scan]
    ],
    [{v3, B3}, {v4scan, B4}] = Res,
    N = length(Routes),
    io:format(
        "mem ~p routes=~p v3=~p bytes (~.1f/route) v4=~p bytes (~.1f/route) saved=~.1f%~n",
        [W, N, B3, B3 / N, B4, B4 / N, 100 * (B3 - B4) / B3]
    );
run_mode(garbage, W) ->
    {Routes, Topics} = workload(W),
    TopicsT = list_to_tuple(Topics),
    Ref = check_same(W, Routes, Topics),
    Res = [
        begin
            Tab = make_table(V, Routes),
            Op = op(V, Tab),
            {Words, GCs} = garbage(Op, TopicsT, 20000),
            ets:delete(Tab),
            {V, Words, GCs}
        end
     || V <- ?VARIANTS
    ],
    io:format(
        "garbage ~p matches=~p ~s~n",
        [W, Ref, lists:join(" ", [io_lib:format("~p=~w", [V, round(Wd)]) || {V, Wd, _G} <- Res])]
    );
run_mode(time, W) ->
    {Routes, Topics} = workload(W),
    TopicsT = list_to_tuple(Topics),
    Tabs = [{V, make_table(V, Routes)} || V <- ?VARIANTS],
    Ops = [{V, op(V, T)} || {V, T} <- Tabs],
    N = calibrate(element(2, hd(Ops)), TopicsT),
    Rounds = [round_times(rotate(Ops, R), TopicsT, N) || R <- lists:seq(0, 2)],
    Med = [
        {V, median([proplists:get_value(V, Rd) || Rd <- Rounds]),
            lists:min([proplists:get_value(V, Rd) || Rd <- Rounds]),
            lists:max([proplists:get_value(V, Rd) || Rd <- Rounds])}
     || V <- ?VARIANTS
    ],
    [ets:delete(T) || {_, T} <- Tabs],
    io:format(
        "time ~p n=~p ~s~n",
        [
            W,
            N,
            lists:join(" ", [
                io_lib:format("~p=~w(~w-~w)", [V, round(M), round(Lo), round(Hi)])
             || {V, M, Lo, Hi} <- Med
            ])
        ]
    ).

rotate(L, 0) -> L;
rotate([H | T], N) -> rotate(T ++ [H], N - 1).

check_same(W, Routes, Topics) ->
    Results = [
        begin
            Tab = make_table(V, Routes),
            Op = op(V, Tab),
            R = [lists:sort(Op(T)) || T <- Topics],
            ets:delete(Tab),
            R
        end
     || V <- ?VARIANTS
    ],
    case lists:usort(Results) of
        [One] -> lists:sum([length(R) || R <- One]) / length(Topics);
        _ -> error({results_differ, W})
    end.

%% Words of garbage per op and minor GCs, in a fresh process with a default heap.
garbage(Op, TopicsT, N) ->
    Self = self(),
    {Pid, MRef} = spawn_monitor(fun() ->
        loop(Op, TopicsT, 200),
        erlang:garbage_collect(),
        {_, W0, _} = erlang:statistics(garbage_collection),
        {garbage_collection, G0} = process_info(self(), garbage_collection),
        loop(Op, TopicsT, N),
        {garbage_collection, G1} = process_info(self(), garbage_collection),
        erlang:garbage_collect(),
        {_, W1, _} = erlang:statistics(garbage_collection),
        Self !
            {
                self(),
                (W1 - W0) / N,
                proplists:get_value(minor_gcs, G1) - proplists:get_value(minor_gcs, G0)
            }
    end),
    receive
        {Pid, Words, GCs} ->
            demonitor(MRef, [flush]),
            {Words, GCs};
        {'DOWN', MRef, _, _, R} ->
            error(R)
    end.

calibrate(Op, TopicsT) ->
    T0 = erlang:monotonic_time(nanosecond),
    loop(Op, TopicsT, 2000),
    Ns = (erlang:monotonic_time(nanosecond) - T0) / 2000,
    max(2000, round(20000000 / max(Ns, 1))).

round_times(Ops, TopicsT, N) ->
    [{V, time_op(Op, TopicsT, N)} || {V, Op} <- Ops].

%% Median of 7 runs, ns per op.
time_op(Op, TopicsT, N) ->
    loop(Op, TopicsT, N div 4),
    median([
        begin
            T0 = erlang:monotonic_time(nanosecond),
            loop(Op, TopicsT, N),
            (erlang:monotonic_time(nanosecond) - T0) / N
        end
     || _ <- lists:seq(1, 7)
    ]).

median(L) -> lists:nth((length(L) + 1) div 2, lists:sort(L)).

loop(_Op, _TopicsT, 0) ->
    ok;
loop(Op, TopicsT, I) ->
    _ = Op(element(I rem tuple_size(TopicsT) + 1, TopicsT)),
    loop(Op, TopicsT, I - 1).
