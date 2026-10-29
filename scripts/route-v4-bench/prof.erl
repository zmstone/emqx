-module(prof).
-export([run/1]).
run([WStr, VStr]) ->
    {Routes, Topics} = v4bench:workload(list_to_atom(WStr)),
    V = list_to_atom(VStr),
    Tab = v4bench:make_table(V, Routes),
    Op = v4bench:op(V, Tab),
    T = hd(Topics),
    eprof:start(),
    eprof:profile([], fun() -> [Op(T) || _ <- lists:seq(1, 2000)] end),
    eprof:analyze(total, [{sort, time}]),
    NextCalls = count_next(V, Tab, T),
    io:format("ets:next calls per op: ~p~n", [NextCalls]),
    halt().
count_next(V, Tab, T) ->
    C = counters:new(1, []),
    NextF = fun(K) ->
        counters:add(C, 1, 1),
        ets:next(Tab, K)
    end,
    _ =
        case V of
            v3 -> emqx_trie_search:matches(T, NextF, []);
            v4words -> emqx_trie_search_v4:match_routes(T, NextF)
        end,
    counters:get(C, 1).
