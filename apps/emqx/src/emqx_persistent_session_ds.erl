%%--------------------------------------------------------------------
%% Copyright (c) 2021-2024 EMQ Technologies Co., Ltd. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%--------------------------------------------------------------------

-module(emqx_persistent_session_ds).

-behaviour(emqx_session).

-include("emqx.hrl").
-include_lib("emqx/include/logger.hrl").
-include_lib("snabbkaffe/include/trace.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-include("emqx_mqtt.hrl").

-include("emqx_session.hrl").
-include("emqx_persistent_session_ds/session_internals.hrl").

-ifdef(TEST).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-endif.

%% Session API
-export([
    create/4,
    open/4,
    destroy/1,
    kick_offline_session/1
]).

-export([
    info/2,
    stats/1
]).

-export([
    subscribe/3,
    unsubscribe/2,
    get_subscription/2
]).

-export([
    publish/3,
    puback/3,
    pubrec/2,
    pubrel/2,
    pubcomp/3
]).

-export([
    deliver/3,
    replay/3,
    handle_timeout/3,
    handle_info/2,
    disconnect/2,
    terminate/2
]).

%% Will message handling
-export([
    clear_will_message/1,
    publish_will_message_now/2
]).

%% Managment APIs:
-export([
    list_client_subscriptions/2,
    get_client_subscription/2
]).

%% session table operations
-export([sync/1]).
-ifndef(STORE_STATE_IN_DS).
-export([create_tables/0]).
%% END ifndef(STORE_STATE_IN_DS).
-endif.

%% internal export used by session GC process
-export([destroy_session/1]).

%% Remove me later (satisfy checks for an unused BPAPI)
-export([
    do_open_iterator/3,
    do_ensure_iterator_closed/1,
    do_ensure_all_iterators_closed/1
]).

-export([print_session/1, seqno_diff/4]).

-ifdef(TEST).
-export([
    session_open/4,
    list_all_sessions/0
]).
-endif.

-export_type([
    id/0,
    seqno/0,
    timestamp/0,
    topic_filter/0,
    share_topic_filter/0,
    subscription_id/0,
    subscription/0,
    session/0,
    stream_state/0
]).

-type seqno() :: non_neg_integer().

%% Currently, this is the clientid.  We avoid `emqx_types:clientid()' because that can be
%% an atom, in theory (?).
-type id() :: binary().
-type share_topic_filter() :: #share{}.
-type topic_filter() :: emqx_types:topic() | share_topic_filter().

%% Subscription and subscription states:
%%
%% Persistent sessions cannot simply update or delete subscriptions,
%% since subscription parameters must be exactly the same during
%% replay.
%%
%% To solve this problem, we store subscriptions in a twofold manner:
%%
%% - `subscription' is an object that holds up-to-date information
%% about the client's subscription and a reference to the latest
%% subscription state id
%%
%% - `subscription_state' is an immutable object that holds
%% information about the subcription parameters at a certain point of
%% time
%%
%% New subscription states are created whenever the client subscribes
%% to a topics, or updates an existing subscription.
%%
%% Stream replay states contain references to the subscription states.
%%
%% Outdated subscription states are discarded when they are not
%% referenced by either subscription or stream replay state objects.

-type subscription_id() :: integer().

%% This type is a result of merging
%% `emqx_persistent_session_ds_subs:subscription()' with its current
%% state.
-type subscription() :: #{
    id := subscription_id(),
    start_time := emqx_ds:time(),
    current_state := emqx_persistent_session_ds_subs:subscription_state_id(),
    subopts := map()
}.

-type shared_sub_state() :: term().

-define(TIMER_PULL, timer_pull).
-define(TIMER_GET_STREAMS, timer_get_streams).
-define(TIMER_BUMP_LAST_ALIVE_AT, timer_bump_last_alive_at).
-define(TIMER_RETRY_REPLAY, timer_retry_replay).

-type timer() :: ?TIMER_PULL | ?TIMER_GET_STREAMS | ?TIMER_BUMP_LAST_ALIVE_AT | ?TIMER_RETRY_REPLAY.

%% TODO: Needs configuration?
-define(TIMEOUT_RETRY_REPLAY, 1000).

-type session() :: #{
    %% Client ID
    id := id(),
    %% Configuration:
    props := map(),
    %% Persistent state:
    s := emqx_persistent_session_ds_state:t(),
    %% Shared subscription state:
    shared_sub_s := shared_sub_state(),
    %% Buffer:
    inflight := emqx_persistent_session_ds_inflight:t(),
    %% Last fetched stream:
    %% Used as a continuation point for fair stream scheduling.
    last_fetched_stream => emqx_persistent_session_ds_state:stream_key(),
    %% In-progress replay:
    %% List of stream replay states to be added to the inflight buffer.
    replay => [{_StreamKey, stream_state()}, ...],
    %% Timers:
    timer() => reference()
}.

-define(IS_REPLAY_ONGOING(SESS), is_map_key(replay, SESS)).

-record(req_sync, {
    from :: pid(),
    ref :: reference()
}).

-type stream_state() :: #srs{}.

-type message() :: emqx_types:message().
-type timestamp() :: emqx_utils_calendar:epoch_millisecond().
-type millisecond() :: non_neg_integer().
-type clientinfo() :: emqx_types:clientinfo().
-type conninfo() :: emqx_session:conninfo().
-type replies() :: emqx_session:replies().

-define(STATS_KEYS, [
    durable,
    subscriptions_cnt,
    subscriptions_max,
    inflight_cnt,
    inflight_max,
    mqueue_len,
    mqueue_dropped,
    seqno_q1_comm,
    seqno_q1_dup,
    seqno_q1_next,
    seqno_q2_comm,
    seqno_q2_dup,
    seqno_q2_rec,
    seqno_q2_next,
    n_streams,
    awaiting_rel_cnt,
    awaiting_rel_max
]).

%%

-spec create(clientinfo(), conninfo(), emqx_maybe:t(message()), emqx_session:conf()) ->
    session().
create(#{clientid := ClientID} = ClientInfo, ConnInfo, MaybeWillMsg, Conf) ->
    ensure_timers(session_ensure_new(ClientID, ClientInfo, ConnInfo, MaybeWillMsg, Conf)).

-spec open(clientinfo(), conninfo(), emqx_maybe:t(message()), emqx_session:conf()) ->
    {_IsPresent :: true, session(), []} | false.
open(#{clientid := ClientID} = ClientInfo, ConnInfo, MaybeWillMsg, Conf) ->
    %% NOTE
    %% The fact that we need to concern about discarding all live channels here
    %% is essentially a consequence of the in-memory session design, where we
    %% have disconnected channels holding onto session state. Ideally, we should
    %% somehow isolate those idling not-yet-expired sessions into a separate process
    %% space, and move this call back into `emqx_cm` where it belongs.
    ok = emqx_cm:takeover_kick(ClientID),
    case session_open(ClientID, ClientInfo, ConnInfo, MaybeWillMsg) of
        Session0 = #{} ->
            Session1 = Session0#{props => Conf},
            Session = do_expire(ClientInfo, Session1),
            {true, ensure_timers(Session), []};
        false ->
            false
    end.

-spec destroy(session() | clientinfo()) -> ok.
destroy(#{id := ClientID}) ->
    destroy_session(ClientID);
destroy(#{clientid := ClientID}) ->
    destroy_session(ClientID).

destroy_session(ClientID) ->
    session_drop(ClientID, destroy).

-spec kick_offline_session(emqx_types:clientid()) -> ok.
kick_offline_session(ClientID) ->
    case emqx_persistent_message:is_persistence_enabled() of
        true ->
            session_drop(ClientID, kicked);
        false ->
            ok
    end.

%%--------------------------------------------------------------------
%% Info, Stats
%%--------------------------------------------------------------------

info(Keys, Session) when is_list(Keys) ->
    [{Key, info(Key, Session)} || Key <- Keys];
info(id, #{id := ClientID}) ->
    ClientID;
info(clientid, #{id := ClientID}) ->
    ClientID;
info(durable, _) ->
    true;
info(created_at, #{s := S}) ->
    emqx_persistent_session_ds_state:get_created_at(S);
info(is_persistent, #{}) ->
    true;
info(subscriptions, #{s := S, shared_sub_s := SharedSubS}) ->
    maps:merge(
        emqx_persistent_session_ds_subs:to_map(S),
        emqx_persistent_session_ds_shared_subs:to_map(S, SharedSubS)
    );
info(subscriptions_cnt, #{s := S}) ->
    emqx_persistent_session_ds_state:n_subscriptions(S);
info(subscriptions_max, #{props := Conf}) ->
    maps:get(max_subscriptions, Conf);
info(upgrade_qos, #{props := Conf}) ->
    maps:get(upgrade_qos, Conf);
info(inflight, #{inflight := Inflight}) ->
    Inflight;
info(inflight_cnt, #{inflight := Inflight}) ->
    emqx_persistent_session_ds_inflight:n_inflight(Inflight);
info(inflight_max, #{inflight := Inflight}) ->
    emqx_persistent_session_ds_inflight:receive_maximum(Inflight);
info(retry_interval, #{props := Conf}) ->
    maps:get(retry_interval, Conf);
info(mqueue_len, #{inflight := Inflight}) ->
    emqx_persistent_session_ds_inflight:n_buffered(all, Inflight);
info(mqueue_dropped, _Session) ->
    0;
%% info(next_pkt_id, #{s := S}) ->
%%     {PacketId, _} = emqx_persistent_message_ds_replayer:next_packet_id(S),
%%     PacketId;
info(awaiting_rel, #{s := S}) ->
    emqx_persistent_session_ds_state:fold_awaiting_rel(fun maps:put/3, #{}, S);
info(awaiting_rel_max, #{props := Conf}) ->
    maps:get(max_awaiting_rel, Conf);
info(awaiting_rel_cnt, #{s := S}) ->
    emqx_persistent_session_ds_state:n_awaiting_rel(S);
info(await_rel_timeout, #{props := Conf}) ->
    maps:get(await_rel_timeout, Conf);
info(seqno_q1_comm, #{s := S}) ->
    emqx_persistent_session_ds_state:get_seqno(?committed(?QOS_1), S);
info(seqno_q1_dup, #{s := S}) ->
    emqx_persistent_session_ds_state:get_seqno(?dup(?QOS_1), S);
info(seqno_q1_next, #{s := S}) ->
    emqx_persistent_session_ds_state:get_seqno(?next(?QOS_1), S);
info(seqno_q2_comm, #{s := S}) ->
    emqx_persistent_session_ds_state:get_seqno(?committed(?QOS_2), S);
info(seqno_q2_dup, #{s := S}) ->
    emqx_persistent_session_ds_state:get_seqno(?dup(?QOS_2), S);
info(seqno_q2_rec, #{s := S}) ->
    emqx_persistent_session_ds_state:get_seqno(?rec, S);
info(seqno_q2_next, #{s := S}) ->
    emqx_persistent_session_ds_state:get_seqno(?next(?QOS_2), S);
info(n_streams, #{s := S}) ->
    emqx_persistent_session_ds_state:n_streams(S);
info({MsgsQ, _PagerParams}, _Session) when MsgsQ =:= mqueue_msgs; MsgsQ =:= inflight_msgs ->
    {error, not_implemented}.

-spec stats(session()) -> emqx_types:stats().
stats(Session) ->
    info(?STATS_KEYS, Session).

%% Used by management API
-spec print_session(emqx_types:clientid()) -> map() | undefined.
print_session(ClientId) ->
    case try_get_live_session(ClientId) of
        {Pid, SessionState} ->
            maps:update_with(
                s, fun emqx_persistent_session_ds_state:format/1, SessionState#{
                    '_alive' => {true, Pid}
                }
            );
        not_found ->
            case emqx_persistent_session_ds_state:print_session(ClientId) of
                undefined ->
                    undefined;
                S ->
                    #{s => S, '_alive' => false}
            end;
        not_persistent ->
            undefined
    end.

%%--------------------------------------------------------------------
%% Client -> Broker: SUBSCRIBE / UNSUBSCRIBE
%%--------------------------------------------------------------------

%% Suppress warnings about clauses handling unimplemented results
%% of `emqx_persistent_session_ds_shared_subs:on_subscribe/3`
-dialyzer({nowarn_function, subscribe/3}).
-spec subscribe(topic_filter(), emqx_types:subopts(), session()) ->
    {ok, session()} | {error, emqx_types:reason_code()}.
subscribe(
    #share{} = TopicFilter,
    SubOpts,
    Session
) ->
    case emqx_persistent_session_ds_shared_subs:on_subscribe(TopicFilter, SubOpts, Session) of
        {ok, S0, SharedSubS} ->
            S = emqx_persistent_session_ds_state:commit(S0),
            {ok, Session#{s => S, shared_sub_s => SharedSubS}};
        Error = {error, _} ->
            Error
    end;
subscribe(
    TopicFilter,
    SubOpts,
    Session
) ->
    case emqx_persistent_session_ds_subs:on_subscribe(TopicFilter, SubOpts, Session) of
        {ok, S1} ->
            S = emqx_persistent_session_ds_state:commit(S1),
            {ok, Session#{s => S}};
        Error = {error, _} ->
            Error
    end.

%% Suppress warnings about clauses handling unimplemented results
%% of `emqx_persistent_session_ds_shared_subs:on_unsubscribe/4`
-dialyzer({nowarn_function, unsubscribe/2}).
-spec unsubscribe(topic_filter(), session()) ->
    {ok, session(), emqx_types:subopts()} | {error, emqx_types:reason_code()}.
unsubscribe(
    #share{} = TopicFilter,
    Session = #{id := SessionId, s := S0, shared_sub_s := SharedSubS0}
) ->
    case
        emqx_persistent_session_ds_shared_subs:on_unsubscribe(
            SessionId, TopicFilter, S0, SharedSubS0
        )
    of
        {ok, S1, SharedSubS1, #{id := SubId, subopts := SubOpts}} ->
            S2 = emqx_persistent_session_ds_stream_scheduler:on_unsubscribe(SubId, S1),
            S = emqx_persistent_session_ds_state:commit(S2),
            {ok, Session#{s => S, shared_sub_s => SharedSubS1}, SubOpts};
        Error = {error, _} ->
            Error
    end;
unsubscribe(
    TopicFilter,
    Session = #{id := SessionId, s := S0}
) ->
    case emqx_persistent_session_ds_subs:on_unsubscribe(SessionId, TopicFilter, S0) of
        {ok, S1, #{id := SubId, subopts := SubOpts}} ->
            S2 = emqx_persistent_session_ds_stream_scheduler:on_unsubscribe(SubId, S1),
            S = emqx_persistent_session_ds_state:commit(S2),
            {ok, Session#{s => S}, SubOpts};
        Error = {error, _} ->
            Error
    end.

-spec get_subscription(topic_filter(), session()) ->
    emqx_types:subopts() | undefined.
get_subscription(#share{}, _) ->
    %% TODO: shared subscriptions are not supported yet:
    undefined;
get_subscription(TopicFilter, #{s := S}) ->
    case emqx_persistent_session_ds_subs:lookup(TopicFilter, S) of
        #{subopts := SubOpts} ->
            SubOpts;
        undefined ->
            undefined
    end.

%%--------------------------------------------------------------------
%% Client -> Broker: PUBLISH
%%--------------------------------------------------------------------

-spec publish(emqx_types:packet_id(), emqx_types:message(), session()) ->
    {ok, emqx_types:publish_result(), session()}
    | {error, emqx_types:reason_code()}.
publish(
    PacketId,
    Msg = #message{qos = ?QOS_2, timestamp = Ts},
    Session = #{s := S0}
) ->
    case is_awaiting_full(Session) of
        false ->
            case emqx_persistent_session_ds_state:get_awaiting_rel(PacketId, S0) of
                undefined ->
                    Results = emqx_broker:publish(Msg),
                    S = emqx_persistent_session_ds_state:put_awaiting_rel(PacketId, Ts, S0),
                    {ok, Results, Session#{s => S}};
                _Ts ->
                    {error, ?RC_PACKET_IDENTIFIER_IN_USE}
            end;
        true ->
            {error, ?RC_RECEIVE_MAXIMUM_EXCEEDED}
    end;
publish(_PacketId, Msg, Session) ->
    Result = emqx_broker:publish(Msg),
    {ok, Result, Session}.

is_awaiting_full(#{s := S, props := Props}) ->
    emqx_persistent_session_ds_state:n_awaiting_rel(S) >=
        maps:get(max_awaiting_rel, Props, infinity).

-spec expire(emqx_types:clientinfo(), session()) ->
    {ok, [], timeout(), session()} | {ok, [], session()}.
expire(ClientInfo, Session0 = #{props := Props}) ->
    Session = #{s := S} = do_expire(ClientInfo, Session0),
    case emqx_persistent_session_ds_state:n_awaiting_rel(S) of
        0 ->
            {ok, [], Session};
        _ ->
            AwaitRelTimeout = maps:get(await_rel_timeout, Props),
            {ok, [], AwaitRelTimeout, Session}
    end.

do_expire(ClientInfo, Session = #{s := S0, props := Props}) ->
    %% 1. Find expired packet IDs:
    Now = erlang:system_time(millisecond),
    AwaitRelTimeout = maps:get(await_rel_timeout, Props),
    ExpiredPacketIds =
        emqx_persistent_session_ds_state:fold_awaiting_rel(
            fun(PacketId, Ts, Acc) ->
                Age = Now - Ts,
                case Age > AwaitRelTimeout of
                    true ->
                        [PacketId | Acc];
                    false ->
                        Acc
                end
            end,
            [],
            S0
        ),
    %% 2. Perform side effects:
    _ = emqx_session_events:handle_event(ClientInfo, {expired_rel, length(ExpiredPacketIds)}),
    %% 3. Update state:
    S = lists:foldl(
        fun emqx_persistent_session_ds_state:del_awaiting_rel/2,
        S0,
        ExpiredPacketIds
    ),
    Session#{s => S}.

%%--------------------------------------------------------------------
%% Client -> Broker: PUBACK
%%--------------------------------------------------------------------

-spec puback(clientinfo(), emqx_types:packet_id(), session()) ->
    {ok, emqx_types:message(), replies(), session()}
    | {error, emqx_types:reason_code()}.
puback(_ClientInfo, PacketId, Session0) ->
    case update_seqno(puback, PacketId, Session0) of
        {ok, Msg, Session} ->
            {ok, Msg, [], pull_now(Session)};
        Error ->
            Error
    end.

%%--------------------------------------------------------------------
%% Client -> Broker: PUBREC
%%--------------------------------------------------------------------

-spec pubrec(emqx_types:packet_id(), session()) ->
    {ok, emqx_types:message(), session()}
    | {error, emqx_types:reason_code()}.
pubrec(PacketId, Session0) ->
    case update_seqno(pubrec, PacketId, Session0) of
        {ok, Msg, Session} ->
            {ok, Msg, Session};
        Error = {error, _} ->
            Error
    end.

%%--------------------------------------------------------------------
%% Client -> Broker: PUBREL
%%--------------------------------------------------------------------

-spec pubrel(emqx_types:packet_id(), session()) ->
    {ok, session()} | {error, emqx_types:reason_code()}.
pubrel(PacketId, Session = #{s := S0}) ->
    case emqx_persistent_session_ds_state:get_awaiting_rel(PacketId, S0) of
        undefined ->
            {error, ?RC_PACKET_IDENTIFIER_NOT_FOUND};
        _TS ->
            S = emqx_persistent_session_ds_state:del_awaiting_rel(PacketId, S0),
            {ok, Session#{s => S}}
    end.

%%--------------------------------------------------------------------
%% Client -> Broker: PUBCOMP
%%--------------------------------------------------------------------

-spec pubcomp(clientinfo(), emqx_types:packet_id(), session()) ->
    {ok, emqx_types:message(), replies(), session()}
    | {error, emqx_types:reason_code()}.
pubcomp(_ClientInfo, PacketId, Session0) ->
    case update_seqno(pubcomp, PacketId, Session0) of
        {ok, Msg, Session} ->
            {ok, Msg, [], pull_now(Session)};
        Error = {error, _} ->
            Error
    end.

%%--------------------------------------------------------------------
%% Delivers
%%--------------------------------------------------------------------

-spec deliver(clientinfo(), [emqx_types:deliver()], session()) ->
    {ok, replies(), session()}.
deliver(ClientInfo, Delivers, Session0) ->
    %% Durable sessions still have to handle some transient messages.
    %% For example, retainer sends messages to the session directly.
    Session = lists:foldl(
        fun(Msg, Acc) -> enqueue_transient(ClientInfo, Msg, Acc) end, Session0, Delivers
    ),
    {ok, [], pull_now(Session)}.

%%--------------------------------------------------------------------
%% Timeouts
%%--------------------------------------------------------------------

-spec handle_timeout(clientinfo(), _Timeout, session()) ->
    {ok, replies(), session()} | {ok, replies(), timeout(), session()}.
handle_timeout(ClientInfo, ?TIMER_PULL, Session0) ->
    {Publishes, Session1} =
        case ?IS_REPLAY_ONGOING(Session0) of
            false ->
                drain_buffer(fetch_new_messages(Session0, ClientInfo));
            true ->
                {[], Session0}
        end,
    Timeout =
        case Publishes of
            [] ->
                get_config(ClientInfo, [idle_poll_interval]);
            [_ | _] ->
                0
        end,
    Session = emqx_session:ensure_timer(?TIMER_PULL, Timeout, Session1),
    {ok, Publishes, Session};
handle_timeout(ClientInfo, ?TIMER_RETRY_REPLAY, Session0) ->
    Session = replay_streams(Session0, ClientInfo),
    {ok, [], Session};
handle_timeout(ClientInfo, ?TIMER_GET_STREAMS, Session0 = #{s := S0, shared_sub_s := SharedSubS0}) ->
    %% `gc` and `renew_streams` methods may drop unsubscribed streams.
    %% Shared subscription handler must have a chance to see unsubscribed streams
    %% in the fully replayed state.
    {S1, SharedSubS1} = emqx_persistent_session_ds_shared_subs:pre_renew_streams(S0, SharedSubS0),
    S2 = emqx_persistent_session_ds_subs:gc(S1),
    S3 = emqx_persistent_session_ds_stream_scheduler:renew_streams(S2),
    {S, SharedSubS} = emqx_persistent_session_ds_shared_subs:renew_streams(S3, SharedSubS1),
    Interval = get_config(ClientInfo, [renew_streams_interval]),
    Session = emqx_session:ensure_timer(
        ?TIMER_GET_STREAMS,
        Interval,
        Session0#{s => S, shared_sub_s => SharedSubS}
    ),
    {ok, [], Session};
handle_timeout(_ClientInfo, ?TIMER_BUMP_LAST_ALIVE_AT, Session0 = #{s := S0}) ->
    S = emqx_persistent_session_ds_state:commit(bump_last_alive(S0)),
    Session = emqx_session:ensure_timer(
        ?TIMER_BUMP_LAST_ALIVE_AT,
        bump_interval(),
        Session0#{s => S}
    ),
    {ok, [], Session};
handle_timeout(_ClientInfo, #req_sync{from = From, ref = Ref}, Session = #{s := S0}) ->
    S = emqx_persistent_session_ds_state:commit(S0),
    From ! Ref,
    {ok, [], Session#{s => S}};
handle_timeout(ClientInfo, expire_awaiting_rel, Session) ->
    expire(ClientInfo, Session);
handle_timeout(_ClientInfo, Timeout, Session) ->
    ?SLOG(warning, #{msg => "unknown_ds_timeout", timeout => Timeout}),
    {ok, [], Session}.

%%--------------------------------------------------------------------
%% Generic messages
%%--------------------------------------------------------------------

-spec handle_info(term(), session()) -> session().
handle_info(?shared_sub_message(Msg), Session = #{s := S0, shared_sub_s := SharedSubS0}) ->
    {S, SharedSubS} = emqx_persistent_session_ds_shared_subs:on_info(S0, SharedSubS0, Msg),
    Session#{s => S, shared_sub_s => SharedSubS}.

%%--------------------------------------------------------------------
%% Shared subscription outgoing messages
%%--------------------------------------------------------------------

shared_sub_opts(SessionId) ->
    #{session_id => SessionId}.

bump_last_alive(S0) ->
    %% Note: we take a pessimistic approach here and assume that the client will be alive
    %% until the next bump timeout.  With this, we avoid garbage collecting this session
    %% too early in case the session/connection/node crashes earlier without having time
    %% to commit the time.
    EstimatedLastAliveAt = now_ms() + bump_interval(),
    emqx_persistent_session_ds_state:set_last_alive_at(EstimatedLastAliveAt, S0).

-spec replay(clientinfo(), [], session()) ->
    {ok, replies(), session()}.
replay(ClientInfo, [], Session0 = #{s := S0}) ->
    Streams = emqx_persistent_session_ds_stream_scheduler:find_replay_streams(S0),
    Session = replay_streams(Session0#{replay => Streams}, ClientInfo),
    {ok, [], Session}.

replay_streams(Session0 = #{replay := [{StreamKey, Srs0} | Rest]}, ClientInfo) ->
    case replay_batch(Srs0, Session0, ClientInfo) of
        Session = #{} ->
            replay_streams(Session#{replay := Rest}, ClientInfo);
        {error, recoverable, Reason} ->
            RetryTimeout = ?TIMEOUT_RETRY_REPLAY,
            ?SLOG(debug, #{
                msg => "failed_to_fetch_replay_batch",
                stream => StreamKey,
                reason => Reason,
                class => recoverable,
                retry_in_ms => RetryTimeout
            }),
            emqx_session:ensure_timer(?TIMER_RETRY_REPLAY, RetryTimeout, Session0);
        {error, unrecoverable, Reason} ->
            Session1 = skip_batch(StreamKey, Srs0, Session0, ClientInfo, Reason),
            replay_streams(Session1#{replay := Rest}, ClientInfo)
    end;
replay_streams(Session0 = #{replay := []}, _ClientInfo) ->
    Session = maps:remove(replay, Session0),
    %% Note: we filled the buffer with the historical messages, and
    %% from now on we'll rely on the normal inflight/flow control
    %% mechanisms to replay them:
    pull_now(Session).

-spec replay_batch(stream_state(), session(), clientinfo()) -> session() | emqx_ds:error(_).
replay_batch(Srs0, Session0, ClientInfo) ->
    #srs{batch_size = BatchSize} = Srs0,
    case enqueue_batch(true, BatchSize, Srs0, Session0, ClientInfo) of
        {ok, Srs, Session} ->
            %% Assert:
            Srs =:= Srs0 orelse
                ?tp(warning, emqx_persistent_session_ds_replay_inconsistency, #{
                    expected => Srs0,
                    got => Srs
                }),
            Session;
        {error, _, _} = Error ->
            Error
    end.

%% Handle `{error, unrecoverable, _}' returned by `enqueue_batch'.
%% Most likely they mean that the generation containing the messages
%% has been removed.
-spec skip_batch(_StreamKey, stream_state(), session(), clientinfo(), _Reason) -> session().
skip_batch(StreamKey, SRS0, Session = #{s := S0}, ClientInfo, Reason) ->
    ?SLOG(info, #{
        msg => "session_ds_replay_unrecoverable_error",
        reason => Reason,
        srs => SRS0
    }),
    GenEvents = fun
        F(QoS, SeqNo, LastSeqNo) when SeqNo < LastSeqNo ->
            FakeMsg = #message{
                id = <<>>,
                qos = QoS,
                payload = <<>>,
                topic = <<>>,
                timestamp = 0
            },
            _ = emqx_session_events:handle_event(ClientInfo, {expired, FakeMsg}),
            F(QoS, inc_seqno(QoS, SeqNo), LastSeqNo);
        F(_, _, _) ->
            ok
    end,
    %% Treat messages as expired:
    GenEvents(?QOS_1, SRS0#srs.first_seqno_qos1, SRS0#srs.last_seqno_qos1),
    GenEvents(?QOS_2, SRS0#srs.first_seqno_qos2, SRS0#srs.last_seqno_qos2),
    SRS = SRS0#srs{it_end = end_of_stream, batch_size = 0},
    %% That's it for the iterator. Mark SRS as reached the
    %% `end_of_stream', and let stream scheduler do the rest:
    S = emqx_persistent_session_ds_state:put_stream(StreamKey, SRS, S0),
    Session#{s := S}.

%%--------------------------------------------------------------------

-spec disconnect(session(), emqx_types:conninfo()) -> {shutdown, session()}.
disconnect(Session = #{id := Id, s := S0, shared_sub_s := SharedSubS0}, ConnInfo) ->
    S1 = maybe_set_offline_info(S0, Id),
    S2 = emqx_persistent_session_ds_state:set_last_alive_at(now_ms(), S1),
    S3 =
        case ConnInfo of
            #{expiry_interval := EI} when is_number(EI) ->
                emqx_persistent_session_ds_state:set_expiry_interval(EI, S2);
            _ ->
                S2
        end,
    {S4, SharedSubS} = emqx_persistent_session_ds_shared_subs:on_disconnect(S3, SharedSubS0),
    S = emqx_persistent_session_ds_state:commit(S4),
    {shutdown, Session#{s => S, shared_sub_s => SharedSubS}}.

-spec terminate(Reason :: term(), session()) -> ok.
terminate(_Reason, Session = #{id := Id, s := S}) ->
    maybe_set_will_message_timer(Session),
    _ = emqx_persistent_session_ds_state:commit(S),
    ?tp(debug, persistent_session_ds_terminate, #{id => Id}),
    ok.

%%--------------------------------------------------------------------
%% Management APIs (dashboard)
%%--------------------------------------------------------------------

-spec list_client_subscriptions(emqx_types:mtns(), emqx_types:clientid()) ->
    {node() | undefined, [{emqx_types:topic() | emqx_types:share(), emqx_types:subopts()}]}
    | {error, not_found}.
list_client_subscriptions(_Mtns, ClientId) ->
    %% XXX: Mtns
    case emqx_persistent_message:is_persistence_enabled() of
        true ->
            %% TODO: this is not the most optimal implementation, since it
            %% should be possible to avoid reading extra data (streams, etc.)
            case print_session(ClientId) of
                Sess = #{s := #{subscriptions := Subs, subscription_states := SStates}} ->
                    Node =
                        case Sess of
                            #{'_alive' := {true, Pid}} ->
                                node(Pid);
                            _ ->
                                undefined
                        end,
                    SubList =
                        maps:fold(
                            fun(Topic, #{current_state := CS}, Acc) ->
                                #{subopts := SubOpts} = maps:get(CS, SStates),
                                Elem = {Topic, SubOpts#{durable => true}},
                                [Elem | Acc]
                            end,
                            [],
                            Subs
                        ),
                    {Node, SubList};
                undefined ->
                    {error, not_found}
            end;
        false ->
            {error, not_found}
    end.

-spec get_client_subscription(emqx_types:clientid(), topic_filter() | share_topic_filter()) ->
    subscription() | undefined.
get_client_subscription(ClientId, #share{} = ShareTopicFilter) ->
    emqx_persistent_session_ds_shared_subs:cold_get_subscription(ClientId, ShareTopicFilter);
get_client_subscription(ClientId, TopicFilter) ->
    emqx_persistent_session_ds_subs:cold_get_subscription(ClientId, TopicFilter).

%%--------------------------------------------------------------------
%% Session tables operations
%%--------------------------------------------------------------------

-ifndef(STORE_STATE_IN_DS).
create_tables() ->
    emqx_persistent_session_ds_state:create_tables().
%% END ifndef(STORE_STATE_IN_DS).
-endif.

%% @doc Force syncing of the transient state to persistent storage
sync(ClientId) ->
    case emqx_cm:lookup_channels(?GBNS, ClientId) of
        [Pid] ->
            Ref = monitor(process, Pid),
            Pid ! {emqx_session, #req_sync{from = self(), ref = Ref}},
            receive
                {'DOWN', Ref, process, _Pid, Reason} ->
                    {error, Reason};
                Ref ->
                    demonitor(Ref, [flush]),
                    ok
            end;
        [] ->
            {error, noproc}
    end.

%% @doc Called when a client connects. This function looks up a
%% session or returns `false` if previous one couldn't be found.
%%
%% Note: session API doesn't handle session takeovers, it's the job of
%% the broker.
-spec session_open(id(), emqx_types:clientinfo(), emqx_types:conninfo(), emqx_maybe:t(message())) ->
    session() | false.
session_open(
    SessionId,
    ClientInfo,
    NewConnInfo = #{proto_name := ProtoName, proto_ver := ProtoVer},
    MaybeWillMsg
) ->
    NowMS = now_ms(),
    case emqx_persistent_session_ds_state:open(SessionId) of
        {ok, S0} ->
            EI = emqx_persistent_session_ds_state:get_expiry_interval(S0),
            LastAliveAt = emqx_persistent_session_ds_state:get_last_alive_at(S0),
            case NowMS >= LastAliveAt + EI of
                true ->
                    session_drop(SessionId, expired),
                    false;
                false ->
                    ?tp(open_session, #{ei => EI, now => NowMS, laa => LastAliveAt}),
                    %% New connection being established
                    S1 = emqx_persistent_session_ds_state:set_expiry_interval(EI, S0),
                    S2 = emqx_persistent_session_ds_state:set_last_alive_at(NowMS, S1),
                    S3 = emqx_persistent_session_ds_state:set_peername(
                        maps:get(peername, NewConnInfo), S2
                    ),
                    S4 = emqx_persistent_session_ds_state:set_will_message(MaybeWillMsg, S3),
                    S5 = set_clientinfo(ClientInfo, S4),
                    S6 = emqx_persistent_session_ds_state:set_protocol({ProtoName, ProtoVer}, S5),
                    {ok, S7, SharedSubS} = emqx_persistent_session_ds_shared_subs:open(
                        S6, shared_sub_opts(SessionId)
                    ),
                    S = emqx_persistent_session_ds_state:commit(S7),
                    Inflight = emqx_persistent_session_ds_inflight:new(
                        receive_maximum(NewConnInfo)
                    ),
                    #{
                        id => SessionId,
                        s => S,
                        shared_sub_s => SharedSubS,
                        inflight => Inflight,
                        props => #{}
                    }
            end;
        undefined ->
            false
    end.

-spec session_ensure_new(
    id(),
    emqx_types:clientinfo(),
    emqx_types:conninfo(),
    emqx_maybe:t(message()),
    emqx_session:conf()
) ->
    session().
session_ensure_new(
    Id, ClientInfo, ConnInfo = #{proto_name := ProtoName, proto_ver := ProtoVer}, MaybeWillMsg, Conf
) ->
    ?tp(debug, persistent_session_ds_ensure_new, #{id => Id}),
    Now = now_ms(),
    S0 = emqx_persistent_session_ds_state:create_new(Id),
    S1 = emqx_persistent_session_ds_state:set_expiry_interval(expiry_interval(ConnInfo), S0),
    S2 = bump_last_alive(S1),
    S3 = emqx_persistent_session_ds_state:set_created_at(Now, S2),
    S4 = lists:foldl(
        fun(Track, Acc) ->
            emqx_persistent_session_ds_state:put_seqno(Track, 0, Acc)
        end,
        S3,
        [
            ?next(?QOS_1),
            ?dup(?QOS_1),
            ?committed(?QOS_1),
            ?next(?QOS_2),
            ?dup(?QOS_2),
            ?rec,
            ?committed(?QOS_2)
        ]
    ),
    S5 = emqx_persistent_session_ds_state:set_will_message(MaybeWillMsg, S4),
    S6 = set_clientinfo(ClientInfo, S5),
    S7 = emqx_persistent_session_ds_state:set_protocol({ProtoName, ProtoVer}, S6),
    S = emqx_persistent_session_ds_state:commit(S7, #{ensure_new => true}),
    #{
        id => Id,
        props => Conf,
        s => S,
        shared_sub_s => emqx_persistent_session_ds_shared_subs:new(shared_sub_opts(Id)),
        inflight => emqx_persistent_session_ds_inflight:new(receive_maximum(ConnInfo))
    }.

%% @doc Called when a client reconnects with `clean session=true' or
%% during session GC
-spec session_drop(id(), _Reason) -> ok.
session_drop(SessionId, Reason) ->
    case emqx_persistent_session_ds_state:open(SessionId) of
        {ok, S0} ->
            ?tp(debug, drop_persistent_session, #{client_id => SessionId, reason => Reason}),
            ok = emqx_persistent_session_ds_subs:on_session_drop(SessionId, S0),
            ok = emqx_persistent_session_ds_state:delete(SessionId);
        undefined ->
            ok
    end.

now_ms() ->
    erlang:system_time(millisecond).

set_clientinfo(ClientInfo0, S) ->
    %% Remove unnecessary fields from the clientinfo:
    ClientInfo = maps:without([cn, dn, auth_result], ClientInfo0),
    emqx_persistent_session_ds_state:set_clientinfo(ClientInfo, S).

%%--------------------------------------------------------------------
%% RPC targets (v1)
%%--------------------------------------------------------------------

%% RPC target.
-spec do_open_iterator(emqx_types:words(), emqx_ds:time(), emqx_ds:iterator_id()) ->
    {ok, emqx_ds_storage_layer:iterator()} | {error, _Reason}.
do_open_iterator(_TopicFilter, _StartMS, _IteratorID) ->
    {error, not_implemented}.

%% RPC target.
-spec do_ensure_iterator_closed(emqx_ds:iterator_id()) -> ok.
do_ensure_iterator_closed(_IteratorID) ->
    ok.

%% RPC target.
-spec do_ensure_all_iterators_closed(id()) -> ok.
do_ensure_all_iterators_closed(_DSSessionID) ->
    ok.

%%--------------------------------------------------------------------
%% Normal replay:
%%--------------------------------------------------------------------

fetch_new_messages(Session0 = #{s := S0, shared_sub_s := SharedSubS0}, ClientInfo) ->
    {S1, SharedSubS1} = emqx_persistent_session_ds_shared_subs:on_streams_replay(S0, SharedSubS0),
    Session1 = Session0#{s => S1, shared_sub_s => SharedSubS1},
    LFS = maps:get(last_fetched_stream, Session1, beginning),
    ItStream = emqx_persistent_session_ds_stream_scheduler:iter_next_streams(LFS, S1),
    BatchSize = get_config(ClientInfo, [batch_size]),
    Session2 = fetch_new_messages(ItStream, BatchSize, Session1, ClientInfo),
    Session2#{shared_sub_s => SharedSubS1}.

fetch_new_messages(ItStream0, BatchSize, Session0, ClientInfo) ->
    #{inflight := Inflight} = Session0,
    case emqx_persistent_session_ds_inflight:n_buffered(all, Inflight) >= BatchSize of
        true ->
            %% Buffer is full:
            Session0;
        false ->
            case emqx_persistent_session_ds_stream_scheduler:next_stream(ItStream0) of
                {StreamKey, Srs, ItStream} ->
                    Session1 = new_batch(StreamKey, Srs, BatchSize, Session0, ClientInfo),
                    Session = Session1#{last_fetched_stream => StreamKey},
                    fetch_new_messages(ItStream, BatchSize, Session, ClientInfo);
                none ->
                    Session0
            end
    end.

new_batch(StreamKey, Srs0, BatchSize, Session0 = #{s := S0}, ClientInfo) ->
    SN1 = emqx_persistent_session_ds_state:get_seqno(?next(?QOS_1), S0),
    SN2 = emqx_persistent_session_ds_state:get_seqno(?next(?QOS_2), S0),
    Srs1 = Srs0#srs{
        first_seqno_qos1 = SN1,
        first_seqno_qos2 = SN2,
        batch_size = 0,
        last_seqno_qos1 = SN1,
        last_seqno_qos2 = SN2
    },
    case enqueue_batch(false, BatchSize, Srs1, Session0, ClientInfo) of
        {ok, Srs, Session} ->
            S1 = emqx_persistent_session_ds_state:put_seqno(
                ?next(?QOS_1),
                Srs#srs.last_seqno_qos1,
                S0
            ),
            S2 = emqx_persistent_session_ds_state:put_seqno(
                ?next(?QOS_2),
                Srs#srs.last_seqno_qos2,
                S1
            ),
            S = emqx_persistent_session_ds_state:put_stream(StreamKey, Srs, S2),
            Session#{s => S};
        {error, recoverable, Reason} ->
            ?SLOG(debug, #{
                msg => "failed_to_fetch_batch",
                stream => StreamKey,
                reason => Reason,
                class => recoverable
            }),
            Session0;
        {error, unrecoverable, Reason} ->
            skip_batch(StreamKey, Srs1, Session0, ClientInfo, Reason)
    end.

enqueue_batch(IsReplay, BatchSize, Srs0, Session = #{inflight := Inflight0, s := S}, ClientInfo) ->
    #srs{
        it_begin = ItBegin0,
        it_end = ItEnd0,
        first_seqno_qos1 = FirstSeqnoQos1,
        first_seqno_qos2 = FirstSeqnoQos2,
        sub_state_id = SubStateId
    } = Srs0,
    ItBegin =
        case IsReplay of
            true -> ItBegin0;
            false -> ItEnd0
        end,
    SubState = #{} = emqx_persistent_session_ds_state:get_subscription_state(SubStateId, S),
    case emqx_ds:next(?PERSISTENT_MESSAGE_DB, ItBegin, BatchSize) of
        {ok, ItEnd, Messages} ->
            {Inflight, LastSeqnoQos1, LastSeqnoQos2} = process_batch(
                IsReplay,
                Session,
                SubState,
                ClientInfo,
                FirstSeqnoQos1,
                FirstSeqnoQos2,
                Messages,
                Inflight0
            ),
            Srs = Srs0#srs{
                it_begin = ItBegin,
                it_end = ItEnd,
                %% TODO: it should be possible to avoid calling
                %% length here by diffing size of inflight before
                %% and after inserting messages:
                batch_size = length(Messages),
                last_seqno_qos1 = LastSeqnoQos1,
                last_seqno_qos2 = LastSeqnoQos2
            },
            {ok, Srs, Session#{inflight := Inflight}};
        {ok, end_of_stream} ->
            %% No new messages; just update the end iterator:
            Srs = Srs0#srs{it_begin = ItBegin, it_end = end_of_stream, batch_size = 0},
            {ok, Srs, Session#{inflight := Inflight0}};
        {error, _, _} = Error ->
            Error
    end.

%% key_of_iter(#{3 := #{3 := #{5 := K}}}) ->
%%     K.

process_batch(
    _IsReplay, _Session, _SubState, _ClientInfo, LastSeqNoQos1, LastSeqNoQos2, [], Inflight
) ->
    {Inflight, LastSeqNoQos1, LastSeqNoQos2};
process_batch(
    IsReplay,
    Session,
    SubState,
    ClientInfo,
    FirstSeqNoQos1,
    FirstSeqNoQos2,
    [KV | Messages],
    Inflight0
) ->
    #{s := S} = Session,
    #{upgrade_qos := UpgradeQoS, subopts := SubOpts} = SubState,
    {_DsMsgKey, Msg0} = KV,
    Comm1 = emqx_persistent_session_ds_state:get_seqno(?committed(?QOS_1), S),
    Comm2 = emqx_persistent_session_ds_state:get_seqno(?committed(?QOS_2), S),
    Dup1 = emqx_persistent_session_ds_state:get_seqno(?dup(?QOS_1), S),
    Dup2 = emqx_persistent_session_ds_state:get_seqno(?dup(?QOS_2), S),
    Rec = emqx_persistent_session_ds_state:get_seqno(?rec, S),
    Msgs = emqx_session:enrich_message(ClientInfo, Msg0, SubOpts, UpgradeQoS),
    {Inflight, LastSeqNoQos1, LastSeqNoQos2} = lists:foldl(
        fun(Msg = #message{qos = Qos}, {Acc, SeqNoQos10, SeqNoQos20}) ->
            case Qos of
                ?QOS_0 ->
                    SeqNoQos1 = SeqNoQos10,
                    SeqNoQos2 = SeqNoQos20;
                ?QOS_1 ->
                    SeqNoQos1 = inc_seqno(?QOS_1, SeqNoQos10),
                    SeqNoQos2 = SeqNoQos20;
                ?QOS_2 ->
                    SeqNoQos1 = SeqNoQos10,
                    SeqNoQos2 = inc_seqno(?QOS_2, SeqNoQos20)
            end,
            {
                case Qos of
                    ?QOS_0 when IsReplay ->
                        %% We ignore QoS 0 messages during replay:
                        Acc;
                    ?QOS_0 ->
                        emqx_persistent_session_ds_inflight:push({undefined, Msg}, Acc);
                    ?QOS_1 when SeqNoQos1 =< Comm1 ->
                        %% QoS1 message has been acked by the client, ignore:
                        Acc;
                    ?QOS_1 when SeqNoQos1 =< Dup1 ->
                        %% QoS1 message has been sent but not
                        %% acked. Retransmit:
                        Msg1 = emqx_message:set_flag(dup, true, Msg),
                        emqx_persistent_session_ds_inflight:push({SeqNoQos1, Msg1}, Acc);
                    ?QOS_1 ->
                        emqx_persistent_session_ds_inflight:push({SeqNoQos1, Msg}, Acc);
                    ?QOS_2 when SeqNoQos2 =< Comm2 ->
                        %% QoS2 message has been PUBCOMP'ed by the client, ignore:
                        Acc;
                    ?QOS_2 when SeqNoQos2 =< Rec ->
                        %% QoS2 message has been PUBREC'ed by the client, resend PUBREL:
                        emqx_persistent_session_ds_inflight:push({pubrel, SeqNoQos2}, Acc);
                    ?QOS_2 when SeqNoQos2 =< Dup2 ->
                        %% QoS2 message has been sent, but we haven't received PUBREC.
                        %%
                        %% TODO: According to the MQTT standard 4.3.3:
                        %% DUP flag is never set for QoS2 messages? We
                        %% do so for mem sessions, though.
                        Msg1 = emqx_message:set_flag(dup, true, Msg),
                        emqx_persistent_session_ds_inflight:push({SeqNoQos2, Msg1}, Acc);
                    ?QOS_2 ->
                        emqx_persistent_session_ds_inflight:push({SeqNoQos2, Msg}, Acc)
                end,
                SeqNoQos1,
                SeqNoQos2
            }
        end,
        {Inflight0, FirstSeqNoQos1, FirstSeqNoQos2},
        Msgs
    ),
    process_batch(
        IsReplay, Session, SubState, ClientInfo, LastSeqNoQos1, LastSeqNoQos2, Messages, Inflight
    ).

%%--------------------------------------------------------------------
%% Transient messages
%%--------------------------------------------------------------------

enqueue_transient(
    _ClientInfo, Msg = #message{qos = Qos}, Session = #{inflight := Inflight0, s := S0}
) ->
    %% TODO: Such messages won't be retransmitted, should the session
    %% reconnect before transient messages are acked.
    %%
    %% Proper solution could look like this: session publishes
    %% transient messages to a separate DS DB that serves as a queue,
    %% then subscribes to a special system topic that contains the
    %% queued messages. Since streams in this DB are exclusive to the
    %% session, messages from the queue can be dropped as soon as they
    %% are acked.
    case Qos of
        ?QOS_0 ->
            S = S0,
            Inflight = emqx_persistent_session_ds_inflight:push({undefined, Msg}, Inflight0);
        QoS when QoS =:= ?QOS_1; QoS =:= ?QOS_2 ->
            SeqNo = inc_seqno(
                QoS, emqx_persistent_session_ds_state:get_seqno(?next(QoS), S0)
            ),
            S = emqx_persistent_session_ds_state:put_seqno(?next(QoS), SeqNo, S0),
            Inflight = emqx_persistent_session_ds_inflight:push({SeqNo, Msg}, Inflight0)
    end,
    Session#{
        inflight => Inflight,
        s => S
    }.

%%--------------------------------------------------------------------
%% Buffer drain
%%--------------------------------------------------------------------

drain_buffer(Session = #{inflight := Inflight0, s := S0}) ->
    {Publishes, Inflight, S} = do_drain_buffer(Inflight0, S0, []),
    {Publishes, Session#{inflight => Inflight, s := S}}.

do_drain_buffer(Inflight0, S0, Acc) ->
    case emqx_persistent_session_ds_inflight:pop(Inflight0) of
        undefined ->
            {lists:reverse(Acc), Inflight0, S0};
        {{pubrel, SeqNo}, Inflight} ->
            Publish = {pubrel, seqno_to_packet_id(?QOS_2, SeqNo)},
            do_drain_buffer(Inflight, S0, [Publish | Acc]);
        {{SeqNo, Msg}, Inflight} ->
            case Msg#message.qos of
                ?QOS_0 ->
                    do_drain_buffer(Inflight, S0, [{undefined, Msg} | Acc]);
                Qos ->
                    S = emqx_persistent_session_ds_state:put_seqno(?dup(Qos), SeqNo, S0),
                    Publish = {seqno_to_packet_id(Qos, SeqNo), Msg},
                    do_drain_buffer(Inflight, S, [Publish | Acc])
            end
    end.

%%--------------------------------------------------------------------------------

%% TODO: find a more reliable way to perform actions that have side
%% effects. Add `CBM:init' callback to the session behavior?
-spec ensure_timers(session()) -> session().
ensure_timers(Session0) ->
    Session1 = emqx_session:ensure_timer(?TIMER_PULL, 100, Session0),
    Session2 = emqx_session:ensure_timer(?TIMER_GET_STREAMS, 100, Session1),
    emqx_session:ensure_timer(?TIMER_BUMP_LAST_ALIVE_AT, 100, Session2).

-spec pull_now(session()) -> session().
pull_now(Session) ->
    emqx_session:reset_timer(?TIMER_PULL, 0, Session).

-spec receive_maximum(conninfo()) -> pos_integer().
receive_maximum(ConnInfo) ->
    %% Note: the default value should be always set by the channel
    %% with respect to the zone configuration, but the type spec
    %% indicates that it's optional.
    maps:get(receive_maximum, ConnInfo, 65_535).

-spec expiry_interval(conninfo()) -> millisecond().
expiry_interval(ConnInfo) ->
    maps:get(expiry_interval, ConnInfo, 0).

%% Note: we don't allow overriding `heartbeat_interval' per
%% zone, since the GC process is responsible for all sessions
%% regardless of the zone.
bump_interval() ->
    emqx_config:get([durable_sessions, heartbeat_interval]).

get_config(#{zone := Zone}, Key) ->
    emqx_config:get_zone_conf(Zone, [durable_sessions | Key]).

-spec try_get_live_session(emqx_types:clientid()) ->
    {pid(), session()} | not_found | not_persistent.
try_get_live_session(ClientId) ->
    case emqx_cm:lookup_channels(local, ?GBNS, ClientId) of
        [Pid] ->
            try
                #{channel := ChanState} = emqx_connection:get_state(Pid),
                case emqx_channel:info(impl, ChanState) of
                    ?MODULE ->
                        {Pid, emqx_channel:info(session_state, ChanState)};
                    _ ->
                        not_persistent
                end
            catch
                _:_ ->
                    not_found
            end;
        _ ->
            not_found
    end.

-spec maybe_set_offline_info(emqx_persistent_session_ds_state:t(), emqx_types:clientid()) ->
    emqx_persistent_session_ds_state:t().
maybe_set_offline_info(S, Id) ->
    case emqx_cm:lookup_client({clientid, Id}) of
        [{_Key, ChannelInfo, Stats}] ->
            emqx_persistent_session_ds_state:set_offline_info(
                #{
                    chan_info => ChannelInfo,
                    stats => Stats,
                    disconnected_at => erlang:system_time(millisecond),
                    last_connected_to => node()
                },
                S
            );
        _ ->
            S
    end.

%%--------------------------------------------------------------------
%% SeqNo tracking
%% --------------------------------------------------------------------

-spec update_seqno(puback | pubrec | pubcomp, emqx_types:packet_id(), session()) ->
    {ok, emqx_types:message(), session()} | {error, _}.
update_seqno(Track, PacketId, Session = #{id := SessionId, s := S, inflight := Inflight0}) ->
    SeqNo = packet_id_to_seqno(PacketId, S),
    case Track of
        puback ->
            SeqNoKey = ?committed(?QOS_1),
            Result = emqx_persistent_session_ds_inflight:puback(SeqNo, Inflight0);
        pubrec ->
            SeqNoKey = ?rec,
            Result = emqx_persistent_session_ds_inflight:pubrec(SeqNo, Inflight0);
        pubcomp ->
            SeqNoKey = ?committed(?QOS_2),
            Result = emqx_persistent_session_ds_inflight:pubcomp(SeqNo, Inflight0)
    end,
    case Result of
        {ok, Inflight} ->
            %% TODO: we pass a bogus message into the hook:
            Msg = emqx_message:make(SessionId, <<>>, <<>>),
            {ok, Msg, Session#{
                s => emqx_persistent_session_ds_state:put_seqno(SeqNoKey, SeqNo, S),
                inflight => Inflight
            }};
        {error, Expected} ->
            ?SLOG(warning, #{
                msg => "out-of-order_commit",
                track => Track,
                packet_id => PacketId,
                seqno => SeqNo,
                expected => Expected
            }),
            {error, ?RC_PACKET_IDENTIFIER_NOT_FOUND}
    end.

%%--------------------------------------------------------------------
%% Functions for dealing with the sequence number and packet ID
%% generation
%% --------------------------------------------------------------------

-define(EPOCH_BITS, 15).
-define(PACKET_ID_MASK, 2#111_1111_1111_1111).

%% Epoch size = `16#10000 div 2' since we generate different sets of
%% packet IDs for QoS1 and QoS2:
-define(EPOCH_SIZE, 16#8000).

%% Reconstruct session counter by adding most significant bits from
%% the current counter to the packet id:
-spec packet_id_to_seqno(emqx_types:packet_id(), emqx_persistent_session_ds_state:t()) ->
    seqno().
packet_id_to_seqno(PacketId, S) ->
    NextSeqNo = emqx_persistent_session_ds_state:get_seqno(?next(packet_id_to_qos(PacketId)), S),
    Epoch = NextSeqNo bsr ?EPOCH_BITS,
    SeqNo = (Epoch bsl ?EPOCH_BITS) + (PacketId band ?PACKET_ID_MASK),
    case SeqNo =< NextSeqNo of
        true ->
            SeqNo;
        false ->
            SeqNo - ?EPOCH_SIZE
    end.

-spec inc_seqno(?QOS_1 | ?QOS_2, seqno()) -> emqx_types:packet_id().
inc_seqno(Qos, SeqNo) ->
    NextSeqno = SeqNo + 1,
    case seqno_to_packet_id(Qos, NextSeqno) of
        0 ->
            %% We skip sequence numbers that lead to PacketId = 0 to
            %% simplify math. Note: it leads to occasional gaps in the
            %% sequence numbers.
            NextSeqno + 1;
        _ ->
            NextSeqno
    end.

%% Note: we use the most significant bit to store the QoS.
seqno_to_packet_id(?QOS_1, SeqNo) ->
    SeqNo band ?PACKET_ID_MASK;
seqno_to_packet_id(?QOS_2, SeqNo) ->
    SeqNo band ?PACKET_ID_MASK bor ?EPOCH_SIZE.

packet_id_to_qos(PacketId) ->
    PacketId bsr ?EPOCH_BITS + 1.

seqno_diff(Qos, A, B, S) ->
    seqno_diff(
        Qos,
        emqx_persistent_session_ds_state:get_seqno(A, S),
        emqx_persistent_session_ds_state:get_seqno(B, S)
    ).

%% Dialyzer complains about the second clause, since it's currently
%% unused, shut it up:
-dialyzer({nowarn_function, seqno_diff/3}).
seqno_diff(?QOS_1, A, B) ->
    %% For QoS1 messages we skip a seqno every time the epoch changes,
    %% we need to substract that from the diff:
    EpochA = A bsr ?EPOCH_BITS,
    EpochB = B bsr ?EPOCH_BITS,
    A - B - (EpochA - EpochB);
seqno_diff(?QOS_2, A, B) ->
    A - B.

%%--------------------------------------------------------------------
%% Will message handling
%%--------------------------------------------------------------------

-spec clear_will_message(session()) -> session().
clear_will_message(#{s := S0} = Session) ->
    S = emqx_persistent_session_ds_state:clear_will_message(S0),
    Session#{s := S}.

-spec publish_will_message_now(session(), message()) -> session().
publish_will_message_now(#{} = Session, WillMsg = #message{}) ->
    _ = emqx_broker:publish(WillMsg),
    clear_will_message(Session).

maybe_set_will_message_timer(#{id := SessionId, s := S}) ->
    case emqx_persistent_session_ds_state:get_will_message(S) of
        #message{} = WillMsg ->
            WillDelayInterval = emqx_channel:will_delay_interval(WillMsg),
            WillDelayInterval > 0 andalso
                emqx_persistent_session_ds_gc_worker:check_session_after(
                    SessionId,
                    timer:seconds(WillDelayInterval)
                ),
            ok;
        _ ->
            ok
    end.

%%--------------------------------------------------------------------
%% Tests
%%--------------------------------------------------------------------

-ifdef(TEST).

%% Warning: the below functions may return out-of-date results because
%% the sessions commit data to mria asynchronously.

list_all_sessions() ->
    maps:from_list(
        [
            {Id, print_session(Id)}
         || Id <- emqx_persistent_session_ds_state:list_sessions()
        ]
    ).

%%%% Proper generators:

%% Generate a sequence number that smaller than the given `NextSeqNo'
%% number by at most `?EPOCH_SIZE':
seqno_gen(NextSeqNo) ->
    WindowSize = ?EPOCH_SIZE - 1,
    Min = max(0, NextSeqNo - WindowSize),
    Max = max(0, NextSeqNo - 1),
    range(Min, Max).

%% Generate a sequence number:
next_seqno_gen() ->
    ?LET(
        {Epoch, Offset},
        {non_neg_integer(), range(0, ?EPOCH_SIZE)},
        Epoch bsl ?EPOCH_BITS + Offset
    ).

%%%% Property-based tests:

%% erlfmt-ignore
packet_id_to_seqno_prop() ->
    ?FORALL(
        {Qos, NextSeqNo}, {oneof([?QOS_1, ?QOS_2]), next_seqno_gen()},
        ?FORALL(
            ExpectedSeqNo, seqno_gen(NextSeqNo),
            begin
                PacketId = seqno_to_packet_id(Qos, ExpectedSeqNo),
                SeqNo = packet_id_to_seqno(PacketId, NextSeqNo),
                ?WHENFAIL(
                    begin
                        io:format(user, " *** PacketID = ~p~n", [PacketId]),
                        io:format(user, " *** SeqNo = ~p -> ~p~n", [ExpectedSeqNo, SeqNo]),
                        io:format(user, " *** NextSeqNo = ~p~n", [NextSeqNo])
                    end,
                    PacketId < 16#10000 andalso SeqNo =:= ExpectedSeqNo
                )
            end)).

inc_seqno_prop() ->
    ?FORALL(
        {Qos, SeqNo},
        {oneof([?QOS_1, ?QOS_2]), next_seqno_gen()},
        begin
            NewSeqNo = inc_seqno(Qos, SeqNo),
            PacketId = seqno_to_packet_id(Qos, NewSeqNo),
            ?WHENFAIL(
                begin
                    io:format(user, " *** QoS = ~p~n", [Qos]),
                    io:format(user, " *** SeqNo = ~p -> ~p~n", [SeqNo, NewSeqNo]),
                    io:format(user, " *** PacketId = ~p~n", [PacketId])
                end,
                PacketId > 0 andalso PacketId < 16#10000
            )
        end
    ).

seqno_diff_prop() ->
    ?FORALL(
        {Qos, SeqNo, N},
        {oneof([?QOS_1, ?QOS_2]), next_seqno_gen(), range(0, 100)},
        ?IMPLIES(
            seqno_to_packet_id(Qos, SeqNo) > 0,
            begin
                NewSeqNo = apply_n_times(N, fun(A) -> inc_seqno(Qos, A) end, SeqNo),
                Diff = seqno_diff(Qos, NewSeqNo, SeqNo),
                ?WHENFAIL(
                    begin
                        io:format(user, " *** QoS = ~p~n", [Qos]),
                        io:format(user, " *** SeqNo = ~p -> ~p~n", [SeqNo, NewSeqNo]),
                        io:format(user, " *** N : ~p == ~p~n", [N, Diff])
                    end,
                    N =:= Diff
                )
            end
        )
    ).

seqno_proper_test_() ->
    Props = [packet_id_to_seqno_prop(), inc_seqno_prop(), seqno_diff_prop()],
    Opts = [{numtests, 1000}, {to_file, user}],
    {timeout, 30,
        {setup,
            fun() ->
                meck:new(emqx_persistent_session_ds_state, [no_history]),
                ok = meck:expect(emqx_persistent_session_ds_state, get_seqno, fun(_Track, Seqno) ->
                    Seqno
                end)
            end,
            fun(_) ->
                meck:unload(emqx_persistent_session_ds_state)
            end,
            [?_assert(proper:quickcheck(Prop, Opts)) || Prop <- Props]}}.

apply_n_times(0, _Fun, A) ->
    A;
apply_n_times(N, Fun, A) when N > 0 ->
    apply_n_times(N - 1, Fun, Fun(A)).

-endif.
