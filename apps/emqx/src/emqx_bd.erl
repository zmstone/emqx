%%--------------------------------------------------------------------
%% Copyright (c) 2026 EMQ Technologies Co., Ltd. All Rights Reserved.
%%--------------------------------------------------------------------
-module(emqx_bd).

-moduledoc """
Experiment: batched delivery. Runtime modes select how a message reaches subscribers.

Local modes:
- `off`: the publisher sends one `#deliver{}` to each subscriber.
- `push`: the publisher sends the message to a batcher process, chosen by topic. The batcher
  drains its mailbox and sends each subscriber one `{bd, undefined, Delivers}`.
- `poll`: as `push`, but a subscriber has at most one batch outstanding per batcher. The
  batcher holds later deliveries until the subscriber sends `{bd_poll, Pid}`. There is no timer.
- `ets`: no batcher. The publisher writes deliveries to ETS and wakes an idle subscriber with
  `bd_ready`. The subscriber takes its deliveries from ETS.
- `chunk`: no batcher and no ETS. When one TCP chunk holds more than one packet, the publisher's
  connection collects the deliveries per subscriber, and sends each subscriber one
  `{bd, undefined, Delivers}` at the end of the chunk.
- `chunkr`: as `chunk`, and the publisher also looks up routes and subscribers once per topic
  in a chunk.

Remote mode `on` sends forwards through the batcher, one RPC per batcher round and node.

Limits of this experiment: only `emqx_connection` understands these messages, deliveries held
in a batcher or in ETS are lost when the subscriber exits, and the RPC is not in BPAPI. In the
chunk modes, a QoS 1 PUBACK can leave before the end of the chunk.
""".

-include("emqx.hrl").

-export([set_mode/3, stats/0, local_mode/0, remote_mode/0]).
-export([dispatch/3, forward/3, dispatch_batch/1, drain/1, ets_take/1]).
-export([chunk_begin/1, chunk_flush/0, chunk_send/2, chunk_routes/2, chunk_subs/2]).

-define(PT, emqx_bd_conf).
-define(CHANS, emqx_bd_entries).
-define(COUNTS, emqx_bd_pending).
-define(MAX_ROUND, 100).
-define(MAX_HELD, 1000).
-define(SWEEP_MS, 5000).
-define(CHUNK, emqx_bd_chunk).
-define(MEMO_ROUTES, emqx_bd_routes).
-define(MEMO_SUBS, emqx_bd_subs).

-define(C_MSGS, 1).
-define(C_DELIVERIES, 2).
-define(C_SENDS, 3).
-define(C_ROUNDS, 4).
-define(C_POLLS, 5).
-define(C_FORWARDS, 6).
-define(C_RPCS, 7).

%%--------------------------------------------------------------------
%% Control
%%--------------------------------------------------------------------

-doc "Set the local mode, the remote mode and the number of batchers. Call this with no load.".
-spec set_mode(off | push | poll | ets | chunk | chunkr, off | on, pos_integer()) -> ok.
set_mode(Local, Remote, K) ->
    case persistent_term:get(?PT, undefined) of
        undefined -> ok;
        {_, _, OldPool, _} -> [exit(P, kill) || P <- tuple_to_list(OldPool)]
    end,
    ok = ensure_tabs(),
    Cnt = counters:new(7, [write_concurrency]),
    Pool =
        case Local =:= push orelse Local =:= poll orelse Remote =:= on of
            true -> list_to_tuple([start_batcher(Local, I, Cnt) || I <- lists:seq(1, K)]);
            false -> {}
        end,
    persistent_term:put(?PT, {Local, Remote, Pool, Cnt}),
    ok.

-spec stats() -> map().
stats() ->
    {Local, Remote, Pool, Cnt} = persistent_term:get(?PT),
    G = fun(I) -> counters:get(Cnt, I) end,
    #{
        local => Local,
        remote => Remote,
        batchers => tuple_size(Pool),
        msgs => G(?C_MSGS),
        deliveries => G(?C_DELIVERIES),
        sends => G(?C_SENDS),
        rounds => G(?C_ROUNDS),
        polls => G(?C_POLLS),
        forwards => G(?C_FORWARDS),
        rpcs => G(?C_RPCS)
    }.

-compile({inline, [conf/0]}).
conf() ->
    persistent_term:get(?PT, {off, off, {}, undefined}).

-spec local_mode() -> off | push | poll | ets | chunk | chunkr.
local_mode() ->
    element(1, conf()).

-spec remote_mode() -> off | on.
remote_mode() ->
    element(2, conf()).

%%--------------------------------------------------------------------
%% Publisher side
%%--------------------------------------------------------------------

-doc "Dispatch through the local mode, which must not be `off`.".
-spec dispatch(emqx_types:topic(), #deliver{}, [pid()] | lookup_in_batcher) -> ok.
dispatch(Topic, Deliver, Subs) ->
    case conf() of
        {ets, _, _, _} ->
            ets_put(Deliver, Subs);
        {_, _, Pool, _} ->
            pick(Pool, Topic) ! {d, Deliver},
            ok
    end.

-spec forward(node(), emqx_types:topic(), emqx_types:delivery()) -> true.
forward(Node, To, Delivery) ->
    {_, _, Pool, _} = conf(),
    pick(Pool, To) ! {f, Node, To, Delivery},
    true.

pick(Pool, Topic) ->
    element(erlang:phash2(Topic, tuple_size(Pool)) + 1, Pool).

%%--------------------------------------------------------------------
%% Receiver node side
%%--------------------------------------------------------------------

-spec dispatch_batch([{emqx_types:topic(), emqx_types:delivery()}]) -> ok.
dispatch_batch(Items) ->
    lists:foreach(fun({To, Delivery}) -> emqx_broker:dispatch(To, Delivery) end, Items).

%%--------------------------------------------------------------------
%% Subscriber side
%%--------------------------------------------------------------------

-doc "Take up to `Max` more deliveries from the mailbox, in `push` mode.".
-spec drain(integer()) -> [#deliver{}].
drain(Max) when Max =< 0 ->
    [];
drain(Max) ->
    receive
        {bd, undefined, Delivers} -> Delivers ++ drain(Max - length(Delivers))
    after 0 -> []
    end.

-doc "Take up to `Max` deliveries of the calling process from ETS.".
-spec ets_take(pos_integer()) -> [#deliver{}].
ets_take(Max) ->
    Self = self(),
    Delivers = ets_take(Self, {Self, 0}, Max),
    case ets:update_counter(?COUNTS, Self, -length(Delivers)) of
        0 -> ok;
        _ -> Self ! bd_ready
    end,
    Delivers.

ets_take(_Self, _Key, 0) ->
    [];
ets_take(Self, Key0, N) ->
    case ets:next(?CHANS, Key0) of
        {Self, _} = Key ->
            [{_, Deliver}] = ets:take(?CHANS, Key),
            [Deliver | ets_take(Self, Key, N - 1)];
        _ ->
            []
    end.

%%--------------------------------------------------------------------
%% Chunk modes, publisher side
%%--------------------------------------------------------------------

-doc "Start to collect deliveries when the chunk holds more than one packet. True if it started.".
-spec chunk_begin(non_neg_integer()) -> boolean().
chunk_begin(N) when N > 1 ->
    case local_mode() of
        off ->
            false;
        Mode when Mode =:= chunk; Mode =:= chunkr ->
            put(?CHUNK, #{}),
            {_, _, _, Cnt} = conf(),
            counters:add(Cnt, ?C_ROUNDS, 1),
            counters:add(Cnt, ?C_MSGS, N),
            true;
        _ ->
            false
    end;
chunk_begin(_) ->
    false.

-doc "Send what the chunk collected. Later packets of the chunk send directly.".
-spec chunk_flush() -> ok.
chunk_flush() ->
    case erase(?CHUNK) of
        undefined ->
            ok;
        Buf ->
            erase(?MEMO_ROUTES),
            erase(?MEMO_SUBS),
            {_, _, _, Cnt} = conf(),
            maps:foreach(
                fun(Sub, Rev) ->
                    Sub ! {bd, undefined, lists:reverse(Rev)},
                    counters:add(Cnt, ?C_SENDS, 1),
                    counters:add(Cnt, ?C_DELIVERIES, length(Rev))
                end,
                Buf
            )
    end.

-doc """
Deliver in a chunk mode. Always in the batch form, because a plain `#deliver{}` drain can pass
a `{bd, ...}` message in the mailbox.
""".
-spec chunk_send(#deliver{}, [pid()]) -> ok.
chunk_send(Deliver, Subs) ->
    case get(?CHUNK) of
        undefined ->
            {_, _, _, Cnt} = conf(),
            N = lists:foldl(
                fun(Sub, I) ->
                    Sub ! {bd, undefined, [Deliver]},
                    I + 1
                end,
                0,
                Subs
            ),
            counters:add(Cnt, ?C_SENDS, N),
            counters:add(Cnt, ?C_DELIVERIES, N);
        Buf ->
            put(
                ?CHUNK,
                lists:foldl(
                    fun(Sub, Acc) ->
                        case Acc of
                            #{Sub := Rev} -> Acc#{Sub := [Deliver | Rev]};
                            #{} -> Acc#{Sub => [Deliver]}
                        end
                    end,
                    Buf,
                    Subs
                )
            ),
            ok
    end.

-doc "Return `Lookup()`, kept for the rest of the chunk in `chunkr` mode.".
-spec chunk_routes(emqx_types:topic(), fun(() -> Routes)) -> Routes.
chunk_routes(Topic, Lookup) ->
    memo(?MEMO_ROUTES, Topic, Lookup).

-doc "Return `Lookup()`, kept for the rest of the chunk in `chunkr` mode.".
-spec chunk_subs(emqx_types:topic(), fun(() -> Subs)) -> Subs.
chunk_subs(Topic, Lookup) ->
    memo(?MEMO_SUBS, Topic, Lookup).

memo(Slot, Topic, Lookup) ->
    case get(Slot) of
        {Topic, Value} ->
            Value;
        _ ->
            Value = Lookup(),
            case local_mode() =:= chunkr andalso get(?CHUNK) =/= undefined of
                true -> put(Slot, {Topic, Value});
                false -> ok
            end,
            Value
    end.

%%--------------------------------------------------------------------
%% ETS mode, publisher side
%%--------------------------------------------------------------------

ets_put(Deliver, Subs) ->
    lists:foreach(
        fun(Sub) ->
            Seq = erlang:unique_integer([monotonic, positive]),
            true = ets:insert(?CHANS, {{Sub, Seq}, Deliver}),
            case ets:update_counter(?COUNTS, Sub, 1, {Sub, 0}) of
                1 -> Sub ! bd_ready;
                _ -> ok
            end
        end,
        Subs
    ).

ensure_tabs() ->
    Opts = [named_table, public, {write_concurrency, true}, {read_concurrency, false}],
    ensure_tab(?CHANS, [ordered_set | Opts]),
    ensure_tab(?COUNTS, [set | Opts]),
    ok.

ensure_tab(Name, Opts) ->
    case ets:whereis(Name) of
        undefined ->
            Self = self(),
            %% The table owner must outlive the caller of `set_mode/3'.
            spawn(fun() ->
                ets:new(Name, Opts),
                Self ! {tab, Name},
                receive
                    stop -> ok
                end
            end),
            receive
                {tab, Name} -> ok
            end;
        _ ->
            ok
    end.

%%--------------------------------------------------------------------
%% Batcher
%%--------------------------------------------------------------------

start_batcher(Local, I, Cnt) ->
    S = #{mode => Local, key => I, cnt => Cnt, busy => #{}},
    spawn_opt(
        fun() ->
            _ = erlang:send_after(?SWEEP_MS, self(), sweep),
            loop(S)
        end,
        [{message_queue_data, off_heap}, {min_heap_size, 4096}]
    ).

loop(S) ->
    receive
        First ->
            loop(round([First | take_round(?MAX_ROUND - 1)], S))
    end.

take_round(0) ->
    [];
take_round(N) ->
    receive
        Item -> [Item | take_round(N - 1)]
    after 0 -> []
    end.

round(Items, #{cnt := Cnt} = S) ->
    counters:add(Cnt, ?C_ROUNDS, 1),
    {Forwards, Rest, S1} = split(Items, [], [], S),
    ok = send_forwards(Forwards, S1),
    deliver_round(Rest, S1).

split([{f, Node, To, Delivery} | T], Fwd, Rest, S) ->
    split(T, [{Node, To, Delivery} | Fwd], Rest, S);
split([sweep | T], Fwd, Rest, #{busy := Busy} = S) ->
    _ = erlang:send_after(?SWEEP_MS, self(), sweep),
    Live = maps:filter(fun(P, _) -> is_process_alive(P) end, Busy),
    split(T, Fwd, Rest, S#{busy := Live});
split([Item | T], Fwd, Rest, S) ->
    split(T, Fwd, [Item | Rest], S);
split([], Fwd, Rest, S) ->
    {lists:reverse(Fwd), lists:reverse(Rest), S}.

send_forwards([], _S) ->
    ok;
send_forwards(Forwards, #{key := Key, cnt := Cnt}) ->
    ByNode = lists:foldl(
        fun({Node, To, Delivery}, Acc) ->
            maps:update_with(Node, fun(L) -> [{To, Delivery} | L] end, [{To, Delivery}], Acc)
        end,
        #{},
        Forwards
    ),
    counters:add(Cnt, ?C_FORWARDS, length(Forwards)),
    counters:add(Cnt, ?C_RPCS, map_size(ByNode)),
    maps:foreach(
        fun(Node, Rev) ->
            emqx_rpc:cast(Key, Node, emqx_bd, dispatch_batch, [lists:reverse(Rev)])
        end,
        ByNode
    ).

deliver_round([], S) ->
    S;
deliver_round(Items, #{mode := poll} = S) ->
    round_poll(Items, S);
deliver_round([{d, Deliver}], #{cnt := Cnt} = S) ->
    %% One message in this round: no map is needed. Keep the batch form, because a
    %% plain `#deliver{}' can overtake a queued batch in the subscriber's mailbox drain.
    Subs = subscribers(Deliver),
    send_each(Subs, {bd, undefined, [Deliver]}),
    N = length(Subs),
    counters:add(Cnt, ?C_MSGS, 1),
    counters:add(Cnt, ?C_DELIVERIES, N),
    counters:add(Cnt, ?C_SENDS, N),
    S;
deliver_round(Items, #{cnt := Cnt} = S) ->
    {Out, N} = lists:foldl(fun collect/2, {#{}, 0}, Items),
    maps:foreach(fun(P, Rev) -> P ! {bd, undefined, lists:reverse(Rev)} end, Out),
    counters:add(Cnt, ?C_MSGS, length(Items)),
    counters:add(Cnt, ?C_DELIVERIES, N),
    counters:add(Cnt, ?C_SENDS, map_size(Out)),
    S.

subscribers(#deliver{topic = Topic}) ->
    emqx_broker:subscribers(Topic).

send_each([P | T], Batch) ->
    P ! Batch,
    send_each(T, Batch);
send_each([], _) ->
    ok.

collect({d, Deliver}, Acc) ->
    lists:foldl(
        fun(P, {Out, N}) ->
            {maps:update_with(P, fun(L) -> [Deliver | L] end, [Deliver], Out), N + 1}
        end,
        Acc,
        subscribers(Deliver)
    );
collect(_Stray, Acc) ->
    Acc.

round_poll(Items, #{busy := Busy0, cnt := Cnt} = S) ->
    {Busy1, Ready, Msgs, N, Polls} = lists:foldl(fun poll_step/2, {Busy0, #{}, 0, 0, 0}, Items),
    Self = self(),
    Busy2 = maps:fold(
        fun(P, _, Acc) ->
            #{P := {Rev, _}} = Acc,
            P ! {bd, Self, lists:reverse(Rev)},
            Acc#{P := {[], 0}}
        end,
        Busy1,
        Ready
    ),
    counters:add(Cnt, ?C_MSGS, Msgs),
    counters:add(Cnt, ?C_DELIVERIES, N),
    counters:add(Cnt, ?C_SENDS, map_size(Ready)),
    counters:add(Cnt, ?C_POLLS, Polls),
    S#{busy := Busy2}.

poll_step({d, Deliver}, {Busy, Ready, Msgs, N, Polls}) ->
    {Busy1, Ready1, Cnt} = lists:foldl(
        fun(P, {B, R, C}) ->
            case B of
                #{P := {Rev, Len}} when Len + 1 >= ?MAX_HELD ->
                    {B#{P := {[Deliver | Rev], Len + 1}}, R#{P => true}, C + 1};
                #{P := {Rev, Len}} ->
                    {B#{P := {[Deliver | Rev], Len + 1}}, R, C + 1};
                _ ->
                    {B#{P => {[Deliver], 1}}, R#{P => true}, C + 1}
            end
        end,
        {Busy, Ready, 0},
        subscribers(Deliver)
    ),
    {Busy1, Ready1, Msgs + 1, N + Cnt, Polls};
poll_step({bd_poll, P}, {Busy, Ready, Msgs, N, Polls}) ->
    case Busy of
        #{P := {[], 0}} -> {maps:remove(P, Busy), Ready, Msgs, N, Polls + 1};
        #{P := _} -> {Busy, Ready#{P => true}, Msgs, N, Polls + 1};
        _ -> {Busy, Ready, Msgs, N, Polls + 1}
    end;
poll_step(_Stray, Acc) ->
    Acc.
