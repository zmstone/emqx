%%--------------------------------------------------------------------
%% Copyright (c) 2026 EMQ Technologies Co., Ltd. All Rights Reserved.
%%--------------------------------------------------------------------

-module(emqx_trie_search_v4_tests).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(DESTS, ['n1@h', 'n2@h', {<<"g">>, 'n1@h'}, {<<"g2">>, 'n2@h'}]).

%% Packed filters sort like the word lists of `emqx_trie_search', and round-trip.
order_test() ->
    ?assert(
        proper:quickcheck(
            ?FORALL(
                Fs,
                list(filter_gen()),
                begin
                    U = lists:usort(Fs),
                    ByWords = [F || {_, F} <- lists:sort([{words(F), F} || F <- U])],
                    ByPacked = [
                        F
                     || {_, F} <- lists:sort([{emqx_trie_search_v4:encode(F), F} || F <- U])
                    ],
                    RoundTrip = [
                        emqx_trie_search_v4:decode(emqx_trie_search_v4:encode(F))
                     || F <- U
                    ],
                    ByWords =:= ByPacked andalso RoundTrip =:= U
                end
            ),
            [{numtests, 2000}, {to_file, user}]
        )
    ).

%% Every search function gives the same result as a match of each route.
search_test() ->
    ?assert(
        proper:quickcheck(
            ?FORALL(
                {Routes, Topics},
                {list({filter_gen(), oneof(?DESTS)}), non_empty(list(topic_gen()))},
                begin
                    Tab = ets:new(?MODULE, [ordered_set]),
                    [ets:insert(Tab, {emqx_trie_search_v4:make_key(F, D), []}) || {F, D} <- Routes],
                    NextF = fun(K) -> ets:next(Tab, K) end,
                    Res = lists:all(fun(T) -> same(T, lists:usort(Routes), NextF) end, Topics),
                    ets:delete(Tab),
                    Res
                end
            ),
            [{numtests, 3000}, {to_file, user}]
        )
    ).

same(Topic, Routes, NextF) ->
    Expected = lists:sort([R || {F, _} = R <- Routes, emqx_topic:match(Topic, F)]),
    Keys = fun(Opts) ->
        lists:sort([
            {emqx_trie_search_v4:get_topic(K), emqx_trie_search_v4:get_id(K)}
         || K <- emqx_trie_search_v4:matches(Topic, NextF, Opts)
        ])
    end,
    Got =
        [Keys([{topic_index, S}]) || S <- [scan, lens, words]] ++
            [lists:sort(emqx_trie_search_v4:match_routes(Topic, NextF, D)) || D <- [rebuild, scan]],
    First = emqx_trie_search_v4:match(Topic, NextF) =/= false,
    lists:all(fun(G) -> G =:= Expected end, Got) andalso First =:= (Expected =/= []).

words(F) ->
    [
        case L of
            <<"+">> -> '+';
            <<"#">> -> '#';
            _ -> L
        end
     || L <- binary:split(F, <<"/">>, [global])
    ].

level_gen() ->
    oneof([
        <<"a">>,
        <<"ab">>,
        <<"abc">>,
        <<"z">>,
        <<>>,
        <<"$SYS">>,
        <<"0">>,
        <<"-">>,
        <<"abcdefg">>,
        <<"abcdefghij">>,
        <<"é"/utf8>>
    ]).

topic_gen() ->
    ?LET(Ls, non_empty(list(level_gen())), iolist_to_binary(lists:join(<<"/">>, Ls))).

filter_gen() ->
    ?LET(
        {Ls, Hash},
        {non_empty(list(oneof([level_gen(), <<"+">>]))), boolean()},
        iolist_to_binary(
            lists:join(
                <<"/">>,
                case Hash of
                    true -> Ls ++ [<<"#">>];
                    false -> Ls
                end
            )
        )
    ).
