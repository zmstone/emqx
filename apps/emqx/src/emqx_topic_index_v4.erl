%%--------------------------------------------------------------------
%% Copyright (c) 2026 EMQ Technologies Co., Ltd. All Rights Reserved.
%%--------------------------------------------------------------------

%% @doc Experimental topic index with packed filter keys, see `emqx_trie_search_v4'.
-module(emqx_topic_index_v4).

-export([new/0, insert/4, delete/3, make_key/2]).
-export([match/2, matches/3, match_routes/2, get_id/1, get_topic/1]).

-spec new() -> ets:table().
new() ->
    ets:new(?MODULE, [ordered_set, public, {read_concurrency, true}]).

-spec insert(emqx_types:topic(), Dest, _Record, ets:table()) -> true when Dest :: term().
insert(Filter, Dest, Record, Tab) ->
    true = ets:insert(Tab, {make_key(Filter, Dest), Record}).

-spec delete(emqx_types:topic(), _Dest, ets:table()) -> true.
delete(Filter, Dest, Tab) ->
    ets:delete(Tab, make_key(Filter, Dest)).

-spec make_key(emqx_types:topic(), Dest) -> emqx_trie_search_v4:key(Dest).
make_key(Filter, Dest) ->
    emqx_trie_search_v4:make_key(Filter, Dest).

-spec match(emqx_types:topic(), ets:table()) -> emqx_trie_search_v4:key(_) | false.
match(Topic, Tab) ->
    emqx_trie_search_v4:match(Topic, make_nextf(Tab)).

-spec matches(emqx_types:topic(), ets:table(), emqx_trie_search_v4:opts()) ->
    [emqx_trie_search_v4:key(_)].
matches(Topic, Tab, Opts) ->
    emqx_trie_search_v4:matches(Topic, make_nextf(Tab), Opts).

%% @doc Return `{Filter, Dest}' for each key that matches the topic.
-spec match_routes(emqx_types:topic(), ets:table()) -> [{emqx_types:topic(), _Dest}].
match_routes(Topic, Tab) ->
    emqx_trie_search_v4:match_routes(Topic, make_nextf(Tab)).

get_id(Key) ->
    emqx_trie_search_v4:get_id(Key).

get_topic(Key) ->
    emqx_trie_search_v4:get_topic(Key).

make_nextf(Tab) ->
    fun(Key) -> ets:next(Tab, Key) end.
