%%--------------------------------------------------------------------
%% Copyright (c) 2017-2023 EMQ Technologies Co., Ltd. All Rights Reserved.
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

-module(emqx_broker).

-behaviour(gen_server).

-include("emqx.hrl").
-include("emqx_router.hrl").

-include("logger.hrl").
-include("types.hrl").
-include("emqx_mqtt.hrl").

-export([start_link/2]).

%% PubSub
-export([
    subscribe/1,
    subscribe/2,
    subscribe/3
]).

-export([unsubscribe/1]).

-export([subscriber_down/1]).

-export([
    publish/1,
    safe_publish/1
]).

-export([dispatch/2]).

%% PubSub Infos
-export([
    subscriptions/1,
    subscriptions_via_topic/1,
    subscribers/1,
    subscribed/2
]).

-export([
    get_subopts/2,
    set_subopts/2
]).

-export([topics/0]).

%% Stats fun
-export([stats_fun/0]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-import(emqx_utils_ets, [lookup_value/2, lookup_value/3]).

-ifdef(TEST).
-compile(export_all).
-compile(nowarn_export_all).
-endif.

-define(BROKER, ?MODULE).

%% Guards
-define(IS_SUBID(Id), (is_binary(Id) orelse is_atom(Id))).

-define(cast_or_eval(PICK, Msg, Expr),
    case {PICK, self()} of
        {Self, Self} ->
            _ = Expr,
            ok;
        {NotSelf, _Self} ->
            cast(NotSelf, Msg)
    end
).

-spec start_link(atom(), pos_integer()) -> startlink_ret().
start_link(Pool, Id) ->
    ok = create_tabs(),
    gen_server:start_link(
        {local, emqx_utils:proc_name(?BROKER, Id)},
        ?MODULE,
        [Pool, Id],
        []
    ).

%%------------------------------------------------------------------------------
%% Create tabs
%%------------------------------------------------------------------------------

-spec create_tabs() -> ok.
create_tabs() ->
    TabOpts = [public, {read_concurrency, true}, {write_concurrency, true}],

    %% SubOption: {TopicFilter, SubPid} -> SubOption
    ok = emqx_utils_ets:new(?SUBOPTION, [ordered_set | TabOpts]),

    %% Subscription: SubPid -> TopicFilter1, TopicFilter2, TopicFilter3, ...
    %% duplicate_bag: o(1) insert
    ok = emqx_utils_ets:new(?SUBSCRIPTION, [duplicate_bag | TabOpts]),

    %% Subscriber: Topic -> SubPid1, SubPid2, SubPid3, ...
    %% bag: o(n) insert:(
    ok = emqx_utils_ets:new(?SUBSCRIBER, [bag | TabOpts]).

%%------------------------------------------------------------------------------
%% Subscribe API
%%------------------------------------------------------------------------------

-spec subscribe(emqx_types:topic() | emqx_types:share()) -> ok.
subscribe(Topic) when ?IS_TOPIC(Topic) ->
    subscribe(Topic, undefined).

-spec subscribe(emqx_types:topic() | emqx_types:share(), emqx_types:subid() | emqx_types:subopts()) ->
    ok.
subscribe(Topic, SubId) when ?IS_TOPIC(Topic), ?IS_SUBID(SubId) ->
    subscribe(Topic, SubId, ?DEFAULT_SUBOPTS);
subscribe(Topic, SubOpts) when ?IS_TOPIC(Topic), is_map(SubOpts) ->
    subscribe(Topic, undefined, SubOpts).

-spec subscribe(emqx_types:topic() | emqx_types:share(), emqx_types:subid(), emqx_types:subopts()) ->
    ok.
subscribe(Topic, SubId, SubOpts0) when ?IS_TOPIC(Topic), ?IS_SUBID(SubId), is_map(SubOpts0) ->
    SubOpts = maps:merge(?DEFAULT_SUBOPTS, SubOpts0),
    _ = emqx_trace:subscribe(Topic, SubId, SubOpts),
    SubPid = self(),
    case subscribed(SubPid, Topic) of
        %% New
        false ->
            ok = emqx_broker_helper:register_sub(SubPid, SubId),
            true = ets:insert(?SUBSCRIPTION, {SubPid, Topic}),
            do_subscribe(Topic, SubPid, with_subid(SubId, SubOpts));
        %% Existed
        true ->
            set_subopts(SubPid, Topic, with_subid(SubId, SubOpts)),
            %% ensure to return 'ok'
            ok
    end.

-compile({inline, [with_subid/2]}).
with_subid(undefined, SubOpts) ->
    SubOpts;
with_subid(SubId, SubOpts) ->
    maps:put(subid, SubId, SubOpts).

do_subscribe(Topic, SubPid, SubOpts) when is_binary(Topic) ->
    %% FIXME: subscribe shard bug
    %% https://emqx.atlassian.net/browse/EMQX-10214
    I = emqx_broker_helper:get_sub_shard(SubPid, Topic),
    true = ets:insert(?SUBOPTION, {{Topic, SubPid}, with_shard_idx(I, SubOpts)}),
    Sync = call(pick({Topic, I}), {subscribe, Topic, SubPid, I}),
    case Sync of
        ok ->
            ok;
        Ref when is_reference(Ref) ->
            emqx_router_syncer:wait(Ref)
    end;
do_subscribe(Topic = #share{group = Group, topic = RealTopic}, SubPid, SubOpts) when
    is_binary(RealTopic)
->
    true = ets:insert(?SUBOPTION, {{Topic, SubPid}, SubOpts}),
    emqx_shared_sub:subscribe(Group, RealTopic, SubPid).

with_shard_idx(0, SubOpts) ->
    SubOpts;
with_shard_idx(I, SubOpts) ->
    maps:put(shard, I, SubOpts).

%%--------------------------------------------------------------------
%% Unsubscribe API
%%--------------------------------------------------------------------

-spec unsubscribe(emqx_types:topic() | emqx_types:share()) -> ok.
unsubscribe(Topic) when ?IS_TOPIC(Topic) ->
    SubPid = self(),
    case ets:lookup(?SUBOPTION, {Topic, SubPid}) of
        [{_, SubOpts}] ->
            _ = emqx_trace:unsubscribe(Topic, SubOpts),
            do_unsubscribe(Topic, SubPid, SubOpts);
        [] ->
            ok
    end.

-spec do_unsubscribe(emqx_types:topic() | emqx_types:share(), pid(), emqx_types:subopts()) ->
    ok.
do_unsubscribe(Topic, SubPid, SubOpts) ->
    true = ets:delete(?SUBOPTION, {Topic, SubPid}),
    true = ets:delete_object(?SUBSCRIPTION, {SubPid, Topic}),
    do_unsubscribe2(Topic, SubPid, SubOpts).

-spec do_unsubscribe2(emqx_types:topic() | emqx_types:share(), pid(), emqx_types:subopts()) ->
    ok.
do_unsubscribe2(Topic, SubPid, SubOpts) when
    is_binary(Topic), is_pid(SubPid), is_map(SubOpts)
->
    _ = emqx_broker_helper:reclaim_seq(Topic),
    I = maps:get(shard, SubOpts, 0),
    case I of
        0 -> emqx_exclusive_subscription:unsubscribe(Topic, SubOpts);
        _ -> ok
    end,
    cast(pick({Topic, I}), {unsubscribed, Topic, SubPid, I});
do_unsubscribe2(#share{group = Group, topic = Topic}, SubPid, _SubOpts) when
    is_binary(Group), is_binary(Topic), is_pid(SubPid)
->
    emqx_shared_sub:unsubscribe(Group, Topic, SubPid).

%%--------------------------------------------------------------------
%% Publish
%%--------------------------------------------------------------------

-spec publish(emqx_types:message()) -> emqx_types:publish_result().
publish(Msg) when is_record(Msg, message) ->
    _ = emqx_trace:publish(Msg),
    emqx_message:is_sys(Msg) orelse emqx_metrics:inc('messages.publish'),
    case emqx_hooks:run_fold('message.publish', [], emqx_message:clean_dup(Msg)) of
        #message{headers = #{allow_publish := false}, topic = Topic} ->
            ?TRACE("MQTT", "msg_publish_not_allowed", #{
                message => emqx_message:to_log_map(Msg),
                topic => Topic
            }),
            [];
        Msg1 = #message{topic = Topic} ->
            PersistRes = persist_publish(Msg1),
            PersistRes ++ route(aggre(emqx_router:match_routes(Topic)), delivery(Msg1))
    end.

persist_publish(Msg) ->
    case emqx_persistent_message:persist(Msg) of
        ok ->
            [persisted];
        {_SkipOrError, _Reason} ->
            % TODO: log errors?
            []
    end.

%% Called internally
-spec safe_publish(emqx_types:message()) -> emqx_types:publish_result().
safe_publish(Msg) when is_record(Msg, message) ->
    try
        publish(Msg)
    catch
        Error:Reason:Stk ->
            ?SLOG(
                error,
                #{
                    msg => "publishing_error",
                    exception => Error,
                    reason => Reason,
                    payload => emqx_message:to_log_map(Msg),
                    stacktrace => Stk
                },
                #{topic => Msg#message.topic}
            ),
            []
    end.

-compile({inline, [delivery/1]}).
delivery(Msg) -> #delivery{sender = self(), message = Msg}.

%%--------------------------------------------------------------------
%% Route
%%--------------------------------------------------------------------

-spec route([emqx_types:route_entry()], emqx_types:delivery()) ->
    emqx_types:publish_result().
route([], #delivery{message = Msg}) ->
    ok = emqx_hooks:run('message.dropped', [Msg, #{node => node()}, no_subscribers]),
    ok = inc_dropped_cnt(Msg),
    [];
route(Routes, Delivery) ->
    lists:foldl(
        fun(Route, Acc) ->
            [do_route(Route, Delivery) | Acc]
        end,
        [],
        Routes
    ).

do_route({To, Node}, Delivery) when Node =:= node() ->
    {Node, To, dispatch(To, Delivery)};
do_route({To, Node}, Delivery) when is_atom(Node) ->
    {Node, To, forward(Node, To, Delivery, emqx:get_config([rpc, mode]))};
do_route({To, Group}, Delivery) when is_tuple(Group); is_binary(Group) ->
    {share, To, emqx_shared_sub:dispatch(Group, To, Delivery)}.

aggre([]) ->
    [];
aggre([#route{topic = To, dest = Node}]) when is_atom(Node) ->
    [{To, Node}];
aggre([#route{topic = To, dest = {Group, _Node}}]) ->
    [{To, Group}];
aggre(Routes) ->
    aggre(Routes, false, []).

aggre([#route{topic = To, dest = Node} | Rest], Dedup, Acc) when is_atom(Node) ->
    aggre(Rest, Dedup, [{To, Node} | Acc]);
aggre([#route{topic = To, dest = {Group, _Node}} | Rest], _Dedup, Acc) ->
    aggre(Rest, true, [{To, Group} | Acc]);
aggre([], false, Acc) ->
    Acc;
aggre([], true, Acc) ->
    lists:usort(Acc).

%% @doc Forward message to another node.
-spec forward(
    node(), emqx_types:topic() | emqx_types:share(), emqx_types:delivery(), RpcMode :: sync | async
) ->
    emqx_types:deliver_result().
forward(Node, To, Delivery, async) ->
    true = emqx_broker_proto_v1:forward_async(Node, To, Delivery),
    emqx_metrics:inc('messages.forward');
forward(Node, To, Delivery, sync) ->
    case emqx_broker_proto_v1:forward(Node, To, Delivery) of
        {Err, Reason} when Err =:= badrpc; Err =:= badtcp ->
            ?SLOG(
                error,
                #{
                    msg => "sync_forward_msg_to_node_failed",
                    node => Node,
                    Err => Reason
                },
                #{topic => To}
            ),
            {error, badrpc};
        Result ->
            emqx_metrics:inc('messages.forward'),
            Result
    end.

-spec dispatch(emqx_types:topic() | emqx_types:share(), emqx_types:delivery()) ->
    emqx_types:deliver_result().
dispatch(Topic, Delivery = #delivery{}) when is_binary(Topic) ->
    case emqx:is_running() of
        true ->
            do_dispatch(Topic, Delivery);
        false ->
            %% In a rare case emqx_router_helper process may delay
            %% cleanup of the routing table and the peers will
            %% dispatch messages to a node that is not fully
            %% initialized. Handle this case gracefully:
            {error, not_running}
    end.

-compile({inline, [inc_dropped_cnt/1]}).
inc_dropped_cnt(Msg) ->
    case emqx_message:is_sys(Msg) of
        true ->
            ok;
        false ->
            ok = emqx_metrics:inc('messages.dropped'),
            emqx_metrics:inc('messages.dropped.no_subscribers')
    end.

-compile({inline, [subscribers/1]}).
-spec subscribers(
    emqx_types:topic()
    | emqx_types:share()
    | {shard, emqx_types:topic() | emqx_types:share(), non_neg_integer()}
) ->
    [pid()].
subscribers(Topic) when is_binary(Topic) ->
    lookup_value(?SUBSCRIBER, Topic, []);
subscribers(Shard = {shard, _Topic, _I}) ->
    lookup_value(?SUBSCRIBER, Shard, []).

%%--------------------------------------------------------------------
%% Subscriber is down
%%--------------------------------------------------------------------

-spec subscriber_down(pid()) -> true.
subscriber_down(SubPid) ->
    lists:foreach(
        fun(Topic) ->
            case lookup_value(?SUBOPTION, {Topic, SubPid}) of
                SubOpts when is_map(SubOpts) ->
                    true = ets:delete(?SUBOPTION, {Topic, SubPid}),
                    do_unsubscribe2(Topic, SubPid, SubOpts);
                undefined ->
                    ok
            end
        end,
        lookup_value(?SUBSCRIPTION, SubPid, [])
    ),
    ets:delete(?SUBSCRIPTION, SubPid).

%%--------------------------------------------------------------------
%% Management APIs
%%--------------------------------------------------------------------

-spec subscriptions(pid() | emqx_types:subid()) ->
    [{emqx_types:topic() | emqx_types:share(), emqx_types:subopts()}].
subscriptions(SubPid) when is_pid(SubPid) ->
    [
        {Topic, lookup_value(?SUBOPTION, {Topic, SubPid}, #{})}
     || Topic <- lookup_value(?SUBSCRIPTION, SubPid, [])
    ];
subscriptions(SubId) ->
    case emqx_broker_helper:lookup_subpid(SubId) of
        SubPid when is_pid(SubPid) ->
            subscriptions(SubPid);
        undefined ->
            []
    end.

-spec subscriptions_via_topic(emqx_types:topic() | emqx_types:share()) -> [emqx_types:subopts()].
subscriptions_via_topic(Topic) ->
    MatchSpec = [{{{Topic, '_'}, '_'}, [], ['$_']}],
    ets:select(?SUBOPTION, MatchSpec).

-spec subscribed(
    pid() | emqx_types:subid(), emqx_types:topic() | emqx_types:share()
) -> boolean().
subscribed(SubPid, Topic) when is_pid(SubPid) ->
    ets:member(?SUBOPTION, {Topic, SubPid});
subscribed(SubId, Topic) when ?IS_SUBID(SubId) ->
    SubPid = emqx_broker_helper:lookup_subpid(SubId),
    ets:member(?SUBOPTION, {Topic, SubPid}).

-spec get_subopts(pid(), emqx_types:topic() | emqx_types:share()) -> maybe(emqx_types:subopts()).
get_subopts(SubPid, Topic) when is_pid(SubPid), ?IS_TOPIC(Topic) ->
    lookup_value(?SUBOPTION, {Topic, SubPid});
get_subopts(SubId, Topic) when ?IS_SUBID(SubId) ->
    case emqx_broker_helper:lookup_subpid(SubId) of
        SubPid when is_pid(SubPid) ->
            get_subopts(SubPid, Topic);
        undefined ->
            undefined
    end.

-spec set_subopts(emqx_types:topic() | emqx_types:share(), emqx_types:subopts()) -> boolean().
set_subopts(Topic, NewOpts) when is_binary(Topic), is_map(NewOpts) ->
    set_subopts(self(), Topic, NewOpts).

%% @private
set_subopts(SubPid, Topic, NewOpts) ->
    Sub = {Topic, SubPid},
    case ets:lookup(?SUBOPTION, Sub) of
        [{_, OldOpts}] ->
            ets:insert(?SUBOPTION, {Sub, maps:merge(OldOpts, NewOpts)});
        [] ->
            false
    end.

-spec topics() -> [emqx_types:topic() | emqx_types:share()].
topics() ->
    emqx_router:topics().

%%--------------------------------------------------------------------
%% Stats fun
%%--------------------------------------------------------------------

stats_fun() ->
    safe_update_stats(?SUBSCRIBER, 'subscribers.count', 'subscribers.max'),
    safe_update_stats(?SUBSCRIPTION, 'subscriptions.count', 'subscriptions.max'),
    safe_update_stats(?SUBOPTION, 'suboptions.count', 'suboptions.max').

safe_update_stats(Tab, Stat, MaxStat) ->
    case ets:info(Tab, size) of
        undefined -> ok;
        Size -> emqx_stats:setstat(Stat, MaxStat, Size)
    end.

%%--------------------------------------------------------------------
%% call, cast, pick
%%--------------------------------------------------------------------

-compile({inline, [call/2, cast/2, pick/1]}).

call(Broker, Req) ->
    gen_server:call(Broker, Req, infinity).

cast(Broker, Req) ->
    gen_server:cast(Broker, Req).

%% Pick a broker
pick(TopicShard) ->
    gproc_pool:pick_worker(broker_pool, TopicShard).

%%--------------------------------------------------------------------
%% gen_server callbacks
%%--------------------------------------------------------------------

init([Pool, Id]) ->
    true = gproc_pool:connect_worker(Pool, {Pool, Id}),
    {ok, #{pool => Pool, id => Id}}.

handle_call({subscribe, Topic, SubPid, 0}, {From, _Tag}, State) ->
    Existed = ets:member(?SUBSCRIBER, Topic),
    Result = maybe_add_route(Existed, Topic, From),
    assert_ok_result(Result),
    true = ets:insert(?SUBSCRIBER, {Topic, SubPid}),
    {reply, Result, State};
handle_call({subscribe, Topic, SubPid, I}, _From, State) ->
    Existed = ets:member(?SUBSCRIBER, {shard, Topic, I}),
    Recs = [{{shard, Topic, I}, SubPid}],
    Recs1 =
        case Existed of
            false ->
                %% This will attempt to add a route per each new shard.
                %% The overhead must be negligible, but the consistency in general
                %% and race conditions safety is expected to be stronger.
                %% The main purpose is to solve the race when
                %% `{shard, Topic, N}` (where N > 0)
                %% is the first ever processed subscribe request per `Topic`.
                %% It inserts `{Topic, {shard, I}}` to `?SUBSCRIBER` tab.
                %% After that, another broker worker starts processing
                %% `{shard, Topic, 0}` sub and already observers `{shard, Topic, N}`,
                %% i.e. `ets:member(?SUBSCRIBER, Topic)` returns false,
                %% so it doesn't add the route.
                %% Even if this happens, this cast is expected to be processed eventually
                %% and the route should be added (unless the worker restarts...)
                ?cast_or_eval(
                    pick({Topic, 0}),
                    {subscribed, Topic, shard, I},
                    sync_route(add, Topic, #{})
                ),
                [{Topic, {shard, I}} | Recs];
            true ->
                Recs
        end,
    true = ets:insert(?SUBSCRIBER, Recs1),
    {reply, ok, State};
handle_call(Req, _From, State) ->
    ?SLOG(error, #{msg => "unexpected_call", call => Req}),
    {reply, ignored, State}.

handle_cast({subscribed, Topic, shard, _I}, State) ->
    %% Do not need to 'maybe add' (i.e. to check if the route exists).
    %% It was already checked that this shard is newely added.
    _ = sync_route(add, Topic, #{}),
    {noreply, State};
handle_cast({unsubscribed, Topic, shard, _I}, State) ->
    _ = maybe_delete_route(Topic),
    {noreply, State};
handle_cast({unsubscribed, Topic, SubPid, 0}, State) ->
    true = ets:delete_object(?SUBSCRIBER, {Topic, SubPid}),
    _ = maybe_delete_route(Topic),
    {noreply, State};
handle_cast({unsubscribed, Topic, SubPid, I}, State) ->
    true = ets:delete_object(?SUBSCRIBER, {{shard, Topic, I}, SubPid}),
    case ets:member(?SUBSCRIBER, {shard, Topic, I}) of
        false ->
            ets:delete_object(?SUBSCRIBER, {Topic, {shard, I}}),
            %% Do not attempt to delete any routes here,
            %% let it be handled only by the same pool worker per topic (0 shard),
            %% so that all route deletes are serialized.
            ?cast_or_eval(
                pick({Topic, 0}),
                {unsubscribed, Topic, shard, I},
                maybe_delete_route(Topic)
            );
        true ->
            ok
    end,
    {noreply, State};
handle_cast(Msg, State) ->
    ?SLOG(error, #{msg => "unexpected_cast", cast => Msg}),
    {noreply, State}.

handle_info(Info, State) ->
    ?SLOG(error, #{msg => "unexpected_info", info => Info}),
    {noreply, State}.

terminate(_Reason, #{pool := Pool, id := Id}) ->
    gproc_pool:disconnect_worker(Pool, {Pool, Id}).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

-spec do_dispatch(emqx_types:topic() | emqx_types:share(), emqx_types:delivery()) ->
    emqx_types:deliver_result().
do_dispatch(Topic, #delivery{message = Msg}) ->
    DispN = lists:foldl(
        fun(Sub, N) ->
            N + do_dispatch(Sub, Topic, Msg)
        end,
        0,
        subscribers(Topic)
    ),
    case DispN of
        0 ->
            ok = emqx_hooks:run('message.dropped', [Msg, #{node => node()}, no_subscribers]),
            ok = inc_dropped_cnt(Msg),
            {error, no_subscribers};
        _ ->
            {ok, DispN}
    end.

%% Don't dispatch to share subscriber here.
%% we do it in `emqx_shared_sub.erl` with configured strategy
do_dispatch(SubPid, Topic, Msg) when is_pid(SubPid) ->
    case erlang:is_process_alive(SubPid) of
        true ->
            SubPid ! {deliver, Topic, Msg},
            1;
        false ->
            0
    end;
do_dispatch({shard, I}, Topic, Msg) ->
    lists:foldl(
        fun(SubPid, N) ->
            N + do_dispatch(SubPid, Topic, Msg)
        end,
        0,
        subscribers({shard, Topic, I})
    ).

%%

assert_ok_result(ok) -> ok;
assert_ok_result(Ref) when is_reference(Ref) -> ok.

maybe_add_route(_Existed = false, Topic, ReplyTo) ->
    sync_route(add, Topic, #{reply => ReplyTo});
maybe_add_route(_Existed = true, _Topic, _ReplyTo) ->
    ok.

maybe_delete_route(Topic) ->
    case ets:member(?SUBSCRIBER, Topic) of
        true -> ok;
        false -> sync_route(delete, Topic, #{})
    end.

sync_route(Action, Topic, ReplyTo) ->
    EnabledOn = emqx_config:get([broker, routing, batch_sync, enable_on]),
    case EnabledOn of
        all ->
            push_sync_route(Action, Topic, ReplyTo);
        none ->
            regular_sync_route(Action, Topic);
        Role ->
            case Role =:= mria_config:whoami() of
                true ->
                    push_sync_route(Action, Topic, ReplyTo);
                false ->
                    regular_sync_route(Action, Topic)
            end
    end.

push_sync_route(Action, Topic, Opts) ->
    emqx_router_syncer:push(Action, Topic, node(), Opts).

regular_sync_route(add, Topic) ->
    emqx_router:do_add_route(Topic, node());
regular_sync_route(delete, Topic) ->
    emqx_router:do_delete_route(Topic, node()).
