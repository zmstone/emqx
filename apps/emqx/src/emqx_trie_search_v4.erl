%%--------------------------------------------------------------------
%% Copyright (c) 2026 EMQ Technologies Co., Ltd. All Rights Reserved.
%%--------------------------------------------------------------------

%% @doc Experimental trie search over packed topic filter keys.
%%
%% Same search algorithm as `emqx_trie_search', but each key is
%% `{PackedFilter, Dest}': the filter is one binary instead of a list of words,
%% and the destination is not wrapped in a tuple.
%%
%% Packed filter: the levels joined with byte 0. A `#' level is byte 1, a `+'
%% level is byte 2, an empty level is byte 3. A literal level keeps its bytes.
%% Literal levels must not contain bytes 0..3. The binary order of packed
%% filters is the same as the order of `emqx_trie_search' word lists:
%% '#' < '+' < empty level < literal level, and a filter sorts before its
%% extensions.
%%
%% The topic is not packed. Its literal bytes are the same as in the packed
%% filter, and its separator `/' never equals a packed separator 0, so the
%% search compares the topic with the packed filter directly.
%%
%% Every stored destination must compare greater than the atom `'''.
%%
%% The topic side has three strategies, for the experiment:
%% * `scan': walk the binary topic, find a level end when needed;
%% * `lens': compute the list of level lengths once per search;
%% * `words': split the topic into levels once per search.
-module(emqx_trie_search_v4).

-export([encode/1, decode/1, make_key/2, get_id/1, get_topic/1]).
-export([match/2, match/3, matches/3, match_routes/2, match_routes/3]).

-export_type([key/1, nextf/0, opts/0]).

-define(END, '$end_of_table').
-define(SEP, 0).
-define(HASH, 1).
-define(PLUS, 2).
-define(EMPTY, 3).
-define(MIN_DEST, '').

-define(SLASH_BYTES7, 16#2F2F2F2F2F2F2F).
-define(LOW7_BYTES7, 16#7F7F7F7F7F7F7F).
-define(HIGH_BYTES7, 16#80808080808080).
-define(HAS_ZERO_BYTE7(V),
    (((((V) band ?LOW7_BYTES7) + ?LOW7_BYTES7) bor (V)) band ?HIGH_BYTES7 =/= ?HIGH_BYTES7)
).

-type key(Dest) :: {binary(), Dest}.
-type nextf() :: fun((key(_)) -> ?END | key(_)).
-type opts() :: [unique | return_first | {topic_index, scan | lens | words}].

%%--------------------------------------------------------------------
%% Codec
%%--------------------------------------------------------------------

%% @doc Pack a topic or topic filter.
-spec encode(emqx_types:topic()) -> binary().
encode(Filter) ->
    iolist_to_binary(
        lists:join(<<?SEP>>, [encode_level(L) || L <- binary:split(Filter, <<"/">>, [global])])
    ).

encode_level(<<"#">>) -> <<?HASH>>;
encode_level(<<"+">>) -> <<?PLUS>>;
encode_level(<<>>) -> <<?EMPTY>>;
encode_level(L) -> L.

%% @doc Unpack a packed topic filter.
-spec decode(binary()) -> emqx_types:topic().
decode(Packed) ->
    decode(Packed, <<>>).

%% Copy 7 bytes at once when none of them is below 4.
decode(<<W:56, Rest/binary>>, Acc) when not ?HAS_ZERO_BYTE7(W band 16#FCFCFCFCFCFCFC) ->
    decode(Rest, <<Acc/binary, W:56>>);
decode(<<?SEP, Rest/binary>>, Acc) ->
    decode(Rest, <<Acc/binary, $/>>);
decode(<<?HASH, Rest/binary>>, Acc) ->
    decode(Rest, <<Acc/binary, $#>>);
decode(<<?PLUS, Rest/binary>>, Acc) ->
    decode(Rest, <<Acc/binary, $+>>);
decode(<<?EMPTY, Rest/binary>>, Acc) ->
    decode(Rest, Acc);
decode(<<C, Rest/binary>>, Acc) ->
    decode(Rest, <<Acc/binary, C>>);
decode(<<>>, Acc) ->
    Acc.

-spec make_key(emqx_types:topic(), Dest) -> key(Dest).
make_key(Filter, Dest) ->
    {encode(Filter), Dest}.

-spec get_id(key(Dest)) -> Dest.
get_id({_Packed, Dest}) ->
    Dest.

-spec get_topic(key(_)) -> emqx_types:topic().
get_topic({Packed, _Dest}) ->
    decode(Packed).

%%--------------------------------------------------------------------
%% Search
%%--------------------------------------------------------------------

%% @doc Return the first key that matches the topic, or `false'.
-spec match(emqx_types:topic(), nextf()) -> false | key(_).
match(Topic, NextF) ->
    match(Topic, NextF, []).

-spec match(emqx_types:topic(), nextf(), opts()) -> false | key(_).
match(Topic, NextF, Opts) ->
    try search(Topic, NextF, [return_first | Opts]) of
        _ -> false
    catch
        throw:{first, Res} -> Res
    end.

%% @doc Return all keys that match the topic. The topic must be a valid topic name.
-spec matches(emqx_types:topic(), nextf(), opts()) -> [key(_)].
matches(Topic, NextF, Opts) ->
    search(Topic, NextF, Opts).

search(Topic, NextF, Opts) ->
    Acc0 =
        case proplists:get_bool(return_first, Opts) of
            true ->
                first;
            false ->
                case proplists:get_bool(unique, Opts) of
                    true -> #{};
                    false -> []
                end
        end,
    T =
        case proplists:get_value(topic_index, Opts, words) of
            scan -> {scan, Topic};
            lens -> {lens, Topic, level_lens(Topic)};
            words -> {words, topic_words(Topic)}
        end,
    case search_new(T, base_init(Topic), NextF, Acc0) of
        Acc when is_map(Acc) -> maps:values(Acc);
        Acc -> Acc
    end.

-compile({inline, [base/1, match_add/2]}).

base(Prefix) ->
    {Prefix, ?MIN_DEST}.

%% A topic that starts with `$' does not match a filter that starts with a wildcard.
base_init(<<"$", _/binary>> = Topic) ->
    base(first_level(Topic));
base_init(_) ->
    base(<<>>).

search_new(T, Base, NextF, Acc) ->
    case NextF(Base) of
        ?END -> Acc;
        Cursor -> search_up(T, Cursor, NextF, Acc)
    end.

search_up(T, {Filter, _} = Cursor, NextF, Acc) ->
    case compare(Filter, T) of
        match_full ->
            search_adjacent(T, Cursor, NextF, match_add(Cursor, Acc));
        match_prefix ->
            search_new(T, Cursor, NextF, Acc);
        lower ->
            Acc;
        {seek, FOff, Level} ->
            %% Take the filter levels before FOff, and the topic level after them.
            search_new(T, base(<<Filter:FOff/binary, Level/binary>>), NextF, Acc)
    end.

%% Adjacent keys can have the same filter and a different destination.
search_adjacent(T, {Filter, _} = Cursor, NextF, Acc) ->
    case NextF(Cursor) of
        ?END -> Acc;
        {Filter, _} = Next -> search_adjacent(T, Next, NextF, match_add(Next, Acc));
        Next -> search_up(T, Next, NextF, Acc)
    end.

match_add({_, Dest} = K, Acc) when is_map(Acc) ->
    Acc#{Dest => K};
match_add(K, Acc) when is_list(Acc) ->
    [K | Acc];
match_add(K, first) ->
    throw({first, K}).

%% @doc Return `{Filter, Dest}' for each key that matches the topic. A literal level of a
%% matching filter equals the topic level at the same position, so the filter is rebuilt
%% from the topic levels instead of a scan of the packed bytes. Keys with the same filter
%% share one filter binary.
-spec match_routes(emqx_types:topic(), nextf()) -> [{emqx_types:topic(), _Dest}].
match_routes(Topic, NextF) ->
    match_routes(Topic, NextF, rebuild).

%% Decode: `rebuild' from the topic levels, or `scan' the packed bytes.
-spec match_routes(emqx_types:topic(), nextf(), rebuild | scan) -> [{emqx_types:topic(), _Dest}].
match_routes(Topic, NextF, Decode) ->
    routes_new({Decode, Topic, undefined}, base_init(Topic), NextF, []).

routes_new(T, Base, NextF, Acc) ->
    case NextF(Base) of
        ?END -> Acc;
        Cursor -> routes_up(T, Cursor, NextF, Acc)
    end.

%% A filter that starts with # matches without the topic levels. The topic is split only
%% when a filter needs its levels, and at most once.
routes_up(T, {<<?HASH, _/binary>> = Filter, Dest} = Cursor, NextF, Acc) ->
    Decoded = decode(Filter),
    routes_adjacent(T, Cursor, Decoded, NextF, [{Decoded, Dest} | Acc]);
routes_up({Decode, Topic, undefined}, Cursor, NextF, Acc) ->
    routes_up({Decode, Topic, topic_words(Topic)}, Cursor, NextF, Acc);
routes_up({Decode, _, Words} = T, {Filter, Dest} = Cursor, NextF, Acc) ->
    case cmp_w(Filter, Words, 0) of
        match_full ->
            Decoded =
                case Decode of
                    rebuild -> iolist_to_binary(rebuild(Filter, Words));
                    scan -> decode(Filter)
                end,
            routes_adjacent(T, Cursor, Decoded, NextF, [{Decoded, Dest} | Acc]);
        match_prefix ->
            routes_new(T, Cursor, NextF, Acc);
        lower ->
            Acc;
        {seek, FOff, Level} ->
            routes_new(T, base(<<Filter:FOff/binary, Level/binary>>), NextF, Acc)
    end.

routes_adjacent(T, {Filter, _} = Cursor, Decoded, NextF, Acc) ->
    case NextF(Cursor) of
        ?END -> Acc;
        {Filter, Dest} = Next -> routes_adjacent(T, Next, Decoded, NextF, [{Decoded, Dest} | Acc]);
        Next -> routes_up(T, Next, NextF, Acc)
    end.

%% The iolist of a packed filter that matches the topic levels Words.
rebuild(<<?HASH, _/binary>>, _Words) ->
    [$#];
rebuild(<<?PLUS, ?SEP, FR/binary>>, [_ | Words]) ->
    [$+, $/ | rebuild(FR, Words)];
rebuild(<<?PLUS>>, _Words) ->
    [$+];
rebuild(<<?EMPTY, ?SEP, FR/binary>>, [_ | Words]) ->
    [$/ | rebuild(FR, Words)];
rebuild(<<?EMPTY>>, _Words) ->
    [];
rebuild(F, [W | Words]) ->
    Size = byte_size(W),
    case F of
        <<_:Size/binary, ?SEP, FR/binary>> -> [W, $/ | rebuild(FR, Words)];
        _ -> [W]
    end.

%% Compare a packed filter with the topic. Return:
%% * `match_full': the filter matches the topic;
%% * `match_prefix': the filter matches the first levels of the topic;
%% * `lower': the topic sorts before the filter, and no filter after it can match;
%% * `{seek, FOff, Level}': seek to the filter bytes before FOff followed by Level.
compare(Filter, {scan, Topic}) ->
    case cmp_s(Filter, Topic, 0, 0) of
        {seek, FOff, TOff} -> {seek, FOff, seek_level(level_at(Topic, TOff))};
        Other -> Other
    end;
compare(Filter, {lens, Topic, Lens}) ->
    case cmp_l(Filter, Topic, Lens, 0, 0) of
        {seek, FOff, TOff, Len} -> {seek, FOff, seek_level(binary:part(Topic, TOff, Len))};
        Other -> Other
    end;
compare(Filter, {words, Words}) ->
    cmp_w(Filter, Words, 0).

seek_level(<<>>) -> <<?EMPTY>>;
seek_level(Level) -> Level.

%%--------------------------------------------------------------------
%% Strategy `scan'
%%--------------------------------------------------------------------

%% F and T start at a level. FOff and TOff are their offsets.
cmp_s(<<?HASH, _/binary>>, _T, _FOff, _TOff) ->
    match_full;
cmp_s(<<?PLUS, FR/binary>>, T, FOff, TOff) ->
    case after_level_s(FR, skip_level_s(T, TOff + 1), FOff + 2) of
        lower -> {seek, FOff, TOff};
        Other -> Other
    end;
cmp_s(<<?EMPTY, FR/binary>>, T, FOff, TOff) ->
    case T of
        <<$/, TR/binary>> -> after_level_s(FR, {TR, TOff + 1}, FOff + 2);
        <<>> -> after_level_s(FR, done, FOff + 2);
        _ -> {seek, FOff, TOff}
    end;
cmp_s(F, T, FOff, TOff) ->
    lit_s(F, T, FOff, TOff, 0).

%% Compare a literal filter level. Equal bytes are literal bytes of both: the
%% filter has no `/' and the topic has no byte below 4.
lit_s(<<W:56, FR/binary>>, <<W:56, TR/binary>>, FOff, TOff, N) ->
    lit_s(FR, TR, FOff, TOff, N + 7);
lit_s(<<C, FR/binary>>, <<C, TR/binary>>, FOff, TOff, N) ->
    lit_s(FR, TR, FOff, TOff, N + 1);
lit_s(F, T, FOff, TOff, N) ->
    case F of
        <<FB, _/binary>> when FB > ?SEP ->
            %% The filter level goes on.
            case T of
                <<TB, _/binary>> when TB =/= $/, TB > FB -> {seek, FOff, TOff};
                _ -> lower
            end;
        _ ->
            %% The filter level ends.
            case T of
                <<$/, TR/binary>> -> after_level_s(F, {TR, TOff + N + 1}, FOff + N + 1);
                <<>> -> after_level_s(F, done, FOff + N + 1);
                _ -> {seek, FOff, TOff}
            end
    end.

%% FR is the filter after a level: empty, or a separator and more levels.
after_level_s(<<>>, done, _FOff) ->
    match_full;
after_level_s(<<>>, _, _FOff) ->
    match_prefix;
after_level_s(<<?SEP, ?HASH>>, done, _FOff) ->
    match_full;
after_level_s(<<?SEP, _/binary>>, done, _FOff) ->
    lower;
after_level_s(<<?SEP, FR/binary>>, {TR, TOff}, FOff) ->
    cmp_s(FR, TR, FOff, TOff).

skip_level_s(<<W:56, Rest/binary>>, Off) when not ?HAS_ZERO_BYTE7(W bxor ?SLASH_BYTES7) ->
    skip_level_s(Rest, Off + 7);
skip_level_s(<<$/, Rest/binary>>, Off) ->
    {Rest, Off};
skip_level_s(<<_, Rest/binary>>, Off) ->
    skip_level_s(Rest, Off + 1);
skip_level_s(<<>>, _Off) ->
    done.

level_at(Topic, Offset) ->
    <<_:Offset/binary, Tail/binary>> = Topic,
    binary:part(Topic, Offset, level_len(Tail, 0)).

first_level(Topic) ->
    level_at(Topic, 0).

level_len(<<W:56, Rest/binary>>, N) when not ?HAS_ZERO_BYTE7(W bxor ?SLASH_BYTES7) ->
    level_len(Rest, N + 7);
level_len(<<$/, _/binary>>, N) ->
    N;
level_len(<<_, Rest/binary>>, N) ->
    level_len(Rest, N + 1);
level_len(<<>>, N) ->
    N.

%%--------------------------------------------------------------------
%% Strategy `lens'
%%--------------------------------------------------------------------

level_lens(Topic) ->
    level_lens(Topic, 0, []).

level_lens(<<W:56, Rest/binary>>, N, Acc) when not ?HAS_ZERO_BYTE7(W bxor ?SLASH_BYTES7) ->
    level_lens(Rest, N + 7, Acc);
level_lens(<<$/, Rest/binary>>, N, Acc) ->
    level_lens(Rest, 0, [N | Acc]);
level_lens(<<_, Rest/binary>>, N, Acc) ->
    level_lens(Rest, N + 1, Acc);
level_lens(<<>>, N, Acc) ->
    lists:reverse(Acc, [N]).

%% Same as cmp_s/4, with the length of each topic level known.
cmp_l(<<?HASH, _/binary>>, _T, _Lens, _FOff, _TOff) ->
    match_full;
cmp_l(<<?PLUS, FR/binary>>, T, [Len | Lens], FOff, TOff) ->
    case after_level_l(FR, T, Len, Lens, FOff + 2, TOff) of
        lower -> {seek, FOff, TOff, Len};
        Other -> Other
    end;
cmp_l(<<?EMPTY, FR/binary>>, T, [Len | Lens], FOff, TOff) ->
    case Len of
        0 -> after_level_l(FR, T, 0, Lens, FOff + 2, TOff);
        _ -> {seek, FOff, TOff, Len}
    end;
cmp_l(F, T, [Len | Lens], FOff, TOff) ->
    lit_l(F, T, T, Len, Lens, FOff, TOff, 0).

lit_l(<<W:56, FR/binary>>, <<W:56, TR/binary>>, T0, Len, Lens, FOff, TOff, N) ->
    lit_l(FR, TR, T0, Len, Lens, FOff, TOff, N + 7);
lit_l(<<C, FR/binary>>, <<C, TR/binary>>, T0, Len, Lens, FOff, TOff, N) ->
    lit_l(FR, TR, T0, Len, Lens, FOff, TOff, N + 1);
lit_l(F, T, T0, Len, Lens, FOff, TOff, N) ->
    case F of
        <<FB, _/binary>> when FB > ?SEP ->
            case T of
                <<TB, _/binary>> when TB =/= $/, TB > FB -> {seek, FOff, TOff, Len};
                _ -> lower
            end;
        _ when N =:= Len ->
            after_level_l(F, T0, Len, Lens, FOff + N + 1, TOff);
        _ ->
            {seek, FOff, TOff, Len}
    end.

%% T0 is the topic at the start of the level that was matched, Len its length.
after_level_l(<<>>, _T0, _Len, [], _FOff, _TOff) ->
    match_full;
after_level_l(<<>>, _T0, _Len, _Lens, _FOff, _TOff) ->
    match_prefix;
after_level_l(<<?SEP, ?HASH>>, _T0, _Len, [], _FOff, _TOff) ->
    match_full;
after_level_l(<<?SEP, _/binary>>, _T0, _Len, [], _FOff, _TOff) ->
    lower;
after_level_l(<<?SEP, FR/binary>>, T0, Len, Lens, FOff, TOff) ->
    <<_:Len/binary, $/, TR/binary>> = T0,
    cmp_l(FR, TR, Lens, FOff, TOff + Len + 1).

%%--------------------------------------------------------------------
%% Strategy `words'
%%--------------------------------------------------------------------

%% The levels of the topic. One scan, without `binary:split/3`.
topic_words(Topic) ->
    split(Topic, Topic, 0, 0).

split(<<W:56, Rest/binary>>, Topic, Start, I) when not ?HAS_ZERO_BYTE7(W bxor ?SLASH_BYTES7) ->
    split(Rest, Topic, Start, I + 7);
split(<<$/, Rest/binary>>, Topic, Start, I) ->
    [binary_part(Topic, Start, I - Start) | split(Rest, Topic, I + 1, I + 1)];
split(<<_, Rest/binary>>, Topic, Start, I) ->
    split(Rest, Topic, Start, I + 1);
split(<<>>, Topic, Start, I) ->
    [binary_part(Topic, Start, I - Start)].

%% F is the filter at the start of a level, Words the topic levels left.
cmp_w(<<?HASH, _/binary>>, _Words, _FOff) ->
    match_full;
cmp_w(_F, [], _FOff) ->
    %% The filter has more levels than the topic.
    lower;
cmp_w(<<?PLUS, ?SEP, FR/binary>>, [W | Words], FOff) ->
    case cmp_w(FR, Words, FOff + 2) of
        lower -> {seek, FOff, seek_level(W)};
        Other -> Other
    end;
cmp_w(<<?PLUS>>, [_ | Words], _FOff) ->
    filter_end_w(Words);
cmp_w(<<?EMPTY, ?SEP, FR/binary>>, [<<>> | Words], FOff) ->
    cmp_w(FR, Words, FOff + 2);
cmp_w(<<?EMPTY>>, [<<>> | Words], _FOff) ->
    filter_end_w(Words);
cmp_w(<<?EMPTY, _/binary>>, [W | _], FOff) ->
    {seek, FOff, W};
cmp_w(_F, [<<>> | _], _FOff) ->
    %% A literal filter level sorts after an empty topic level.
    lower;
cmp_w(F, [W | Words], FOff) ->
    Size = byte_size(W),
    case F of
        <<W:Size/binary, ?SEP, FR/binary>> -> cmp_w(FR, Words, FOff + Size + 1);
        <<W:Size/binary>> -> filter_end_w(Words);
        _ when F > W -> lower;
        _ -> {seek, FOff, W}
    end.

filter_end_w([]) ->
    match_full;
filter_end_w(_Words) ->
    match_prefix.
