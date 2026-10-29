-module(v4check).
-export([run/1]).

%% Differential check: v4 search (3 topic strategies) vs v3 search vs brute force.
run([NStr]) ->
    N = list_to_integer(NStr),
    rand:seed(exsss, {1, 2, 3}),
    ok = order_check(20000),
    io:format("order check ok~n"),
    Bad = lists:foldl(fun(I, Acc) -> Acc + one_round(I) end, 0, lists:seq(1, N)),
    io:format("rounds ~p, mismatches ~p~n", [N, Bad]),
    halt().

levels() ->
    [
        <<"a">>,
        <<"b">>,
        <<"ab">>,
        <<"abc">>,
        <<>>,
        <<"$SYS">>,
        <<"$a">>,
        <<"z">>,
        <<"abcdefghij">>,
        <<"abcdefghijk">>,
        <<"abcdefg">>,
        <<"0">>,
        <<"-">>,
        <<"é"/utf8>>,
        <<"abcdefghijabcdefghijabcdefghij">>
    ].

rand_level(Wild) ->
    case Wild andalso rand:uniform(3) =:= 1 of
        true -> <<"+">>;
        false -> lists:nth(rand:uniform(length(levels())), levels())
    end.

rand_topic() ->
    L = [rand_level(false) || _ <- lists:seq(1, rand:uniform(5))],
    iolist_to_binary(lists:join(<<"/">>, L)).

rand_filter() ->
    L = [rand_level(true) || _ <- lists:seq(1, rand:uniform(5))],
    L2 =
        case rand:uniform(3) of
            1 -> L ++ [<<"#">>];
            _ -> L
        end,
    L3 =
        case rand:uniform(20) of
            1 -> [<<"#">>];
            _ -> L2
        end,
    iolist_to_binary(lists:join(<<"/">>, L3)).

dests() -> ['n1@h', 'n2@h', {<<"g">>, 'n1@h'}, {<<"g2">>, 'n2@h'}].

one_round(_I) ->
    NF = rand:uniform(40),
    Routes = lists:usort([
        {rand_filter(), lists:nth(rand:uniform(4), dests())}
     || _ <- lists:seq(1, NF)
    ]),
    T3 = ets:new(t3, [ordered_set]),
    T4 = ets:new(t4, [ordered_set]),
    [ets:insert(T3, {emqx_trie_search:make_key(F, D), []}) || {F, D} <- Routes],
    [ets:insert(T4, {emqx_trie_search_v4:make_key(F, D), []}) || {F, D} <- Routes],
    Next3 = fun(K) -> ets:next(T3, K) end,
    Next4 = fun(K) -> ets:next(T4, K) end,
    Bad = lists:sum([check_topic(rand_topic(), Routes, Next3, Next4) || _ <- lists:seq(1, 20)]),
    ets:delete(T3),
    ets:delete(T4),
    Bad.

check_topic(Topic, Routes, Next3, Next4) ->
    Brute = lists:sort([{F, D} || {F, D} <- Routes, emqx_topic:match(Topic, F)]),
    V3 = lists:sort([
        {emqx_trie_search:get_topic(K), emqx_trie_search:get_id(K)}
     || K <- emqx_trie_search:matches(Topic, Next3, [])
    ]),
    V4 = [
        lists:sort([
            {emqx_trie_search_v4:get_topic(K), emqx_trie_search_v4:get_id(K)}
         || K <- emqx_trie_search_v4:matches(Topic, Next4, [{topic_index, S}])
        ])
     || S <- [scan, lens, words]
    ],
    Routes4 = lists:sort(emqx_trie_search_v4:match_routes(Topic, Next4)),
    First = [
        emqx_trie_search_v4:match(Topic, Next4, [{topic_index, S}]) =/= false
     || S <- [scan, lens, words]
    ],
    case
        [V3, Routes4 | V4] =:= [Brute, Brute, Brute, Brute, Brute] andalso
            First =:= [Brute =/= [] || _ <- First]
    of
        true ->
            0;
        false ->
            io:format(
                "MISMATCH topic ~p~n brute ~p~n v3 ~p~n v4 ~p~n routes ~p~n first ~p~n",
                [Topic, Brute, V3, V4, Routes4, First]
            ),
            1
    end.

%% Packed filters sort in the same order as v3 word lists.
order_check(N) ->
    Fs = lists:usort([rand_filter() || _ <- lists:seq(1, N)]),
    ByWords = [F || {_, F} <- lists:sort([{words(F), F} || F <- Fs])],
    ByPacked = [F || {_, F} <- lists:sort([{emqx_trie_search_v4:encode(F), F} || F <- Fs])],
    RoundTrip = lists:all(
        fun(F) -> emqx_trie_search_v4:decode(emqx_trie_search_v4:encode(F)) =:= F end, Fs
    ),
    case {ByWords =:= ByPacked, RoundTrip} of
        {true, true} ->
            ok;
        Other ->
            io:format("order/roundtrip failed: ~p~n", [Other]),
            error(order)
    end.

words(F) ->
    {W, _} = emqx_trie_search:make_key(F, x),
    case is_list(W) of
        true ->
            W;
        false ->
            [
                case L of
                    <<"+">> -> '+';
                    <<"#">> -> '#';
                    _ -> L
                end
             || L <- binary:split(F, <<"/">>, [global])
            ]
    end.
