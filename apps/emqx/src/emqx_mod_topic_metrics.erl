%%--------------------------------------------------------------------
%% Copyright (c) 2020 EMQ Technologies Co., Ltd. All Rights Reserved.
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

-module(emqx_mod_topic_metrics).

-behaviour(gen_server).
-behaviour(emqx_gen_mod).

-include_lib("emqx_libs/include/emqx.hrl").
-include_lib("emqx_libs/include/logger.hrl").
-include_lib("emqx_libs/include/emqx_mqtt.hrl").

-logger_header("[TOPIC_METRICS]").

-export([ load/1
        , unload/1
        , description/0
        ]).

-export([ on_message_publish/1
        , on_message_delivered/2
        , on_message_dropped/3
        ]).

%% API functions
-export([ start_link/0
        , stop/0
        ]).

-export([ inc/2
        , inc/3
        , val/2
        , rate/2
        , metrics/1
        , register/1
        , unregister/1
        , unregister_all/0
        , is_registered/1
        , all_registered_topics/0
        ]).

%% gen_server callbacks
-export([ init/1
        , handle_call/3
        , handle_info/2
        , handle_cast/2
        , terminate/2
        ]).

-define(CRefID(Topic), {?MODULE, Topic}).

-define(MAX_TOPICS, 512).
-define(TAB, ?MODULE).

-define(TOPIC_METRICS,
        ['messages.in',
         'messages.out',
         'messages.qos0.in',
         'messages.qos0.out',
         'messages.qos1.in',
         'messages.qos1.out',
         'messages.qos2.in',
         'messages.qos2.out',
         'messages.dropped'
        ]).

-define(TICKING_INTERVAL, 1).

-record(speed, {
            last = 0 :: number(),
            tick = 1 :: number(),
            last_v = 0 :: number(),
            acc = 0 :: number(),
            samples = [] :: list()
        }).

-record(state, {
            speeds :: #{{binary(), atom()} => #speed{}}
        }).

%%------------------------------------------------------------------------------
%% APIs
%%------------------------------------------------------------------------------

load(_Env) ->
    emqx_mod_sup:start_child(?MODULE, worker),
    emqx_hooks:put('message.publish',   {?MODULE, on_message_publish, []}),
    emqx_hooks:put('message.dropped',   {?MODULE, on_message_dropped, []}),
    emqx_hooks:put('message.delivered', {?MODULE, on_message_delivered, []}).

unload(_Env) ->
    emqx_hooks:del('message.publish',   {?MODULE, on_message_publish}),
    emqx_hooks:del('message.dropped',   {?MODULE, on_message_dropped}),
    emqx_hooks:del('message.delivered', {?MODULE, on_message_delivered}),
    emqx_mod_sup:stop_child(?MODULE).

description() ->
    "EMQ X Topic Metrics Module".

on_message_publish(#message{topic = Topic, qos = QoS}) ->
    case is_registered(Topic) of
        true ->
            try_inc(Topic, 'messages.in'),
            case QoS of
                ?QOS_0 -> inc(Topic, 'messages.qos0.in');
                ?QOS_1 -> inc(Topic, 'messages.qos1.in');
                ?QOS_2 -> inc(Topic, 'messages.qos2.in')
            end;
        false ->
            ok
    end.

on_message_delivered(_, #message{topic = Topic, qos = QoS}) ->
    case is_registered(Topic) of
        true ->
            try_inc(Topic, 'messages.out'),
            case QoS of
                ?QOS_0 -> inc(Topic, 'messages.qos0.out');
                ?QOS_1 -> inc(Topic, 'messages.qos1.out');
                ?QOS_2 -> inc(Topic, 'messages.qos2.out')
            end;
        false ->
            ok
    end.

on_message_dropped(#message{topic = Topic}, _, _) ->
    case is_registered(Topic) of
        true ->
            inc(Topic, 'messages.dropped');
        false ->
            ok
    end.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:stop(?MODULE).

try_inc(Topic, Metric) ->
    _ = inc(Topic, Metric),
    ok.

inc(Topic, Metric) ->
    inc(Topic, Metric, 1).

inc(Topic, Metric, Val) ->
    case get_counters(Topic) of
        {error, topic_not_found} ->
            {error, topic_not_found};
        CRef ->
            case metric_idx(Metric) of
                {error, invalid_metric} ->
                    {error, invalid_metric};
                Idx ->
                    counters:add(CRef, Idx, Val)
            end
    end.

val(Topic, Metric) ->
    case ets:lookup(?TAB, Topic) of
        [] ->
            {error, topic_not_found};
        [{Topic, CRef}] ->
            case metric_idx(Metric) of
                {error, invalid_metric} ->
                    {error, invalid_metric};
                Idx ->
                    counters:get(CRef, Idx)
            end
    end.

rate(Topic, Metric) ->
    gen_server:call(?MODULE, {get_rate, Topic, Metric}).

metrics(Topic) ->
    case ets:lookup(?TAB, Topic) of
        [] ->
            {error, topic_not_found};
        [{Topic, CRef}] ->
            lists:foldl(fun(Metric, Acc) ->
                            [{to_count(Metric), counters:get(CRef, metric_idx(Metric))},
                             {to_rate(Metric), rate(Topic, Metric)} | Acc]
                        end, [], ?TOPIC_METRICS)
    end.

register(Topic) when is_binary(Topic) ->
    gen_server:call(?MODULE, {register, Topic}).

unregister(Topic) when is_binary(Topic) ->
    gen_server:call(?MODULE, {unregister, Topic}).

unregister_all() ->
    gen_server:call(?MODULE, {unregister, all}).

is_registered(Topic) ->
    ets:member(?TAB, Topic).

all_registered_topics() ->
    [Topic || {Topic, _CRef} <- ets:tab2list(?TAB)].

%%--------------------------------------------------------------------
%% gen_server callbacks
%%--------------------------------------------------------------------

init([]) ->
    erlang:process_flag(trap_exit, true),
    ok = emqx_tables:new(?TAB, [{read_concurrency, true}]),
    erlang:send_after(timer:seconds(?TICKING_INTERVAL), self(), ticking),
    {ok, #state{speeds = #{}}, hibernate}.

handle_call({register, Topic}, _From, State = #state{speeds = Speeds}) ->
    case is_registered(Topic) of
        true ->
            {reply, {error, already_existed}, State};
        false ->
            case number_of_registered_topics() < ?MAX_TOPICS of
                true ->
                    CRef = counters:new(counters_size(), [write_concurrency]),
                    true = ets:insert(?TAB, {Topic, CRef}),
                    [counters:put(CRef, Idx, 0) || Idx <- lists:seq(1, counters_size())],
                    NSpeeds = lists:foldl(fun(Metric, Acc) ->
                                              maps:put({Topic, Metric}, #speed{}, Acc)
                                          end, Speeds, ?TOPIC_METRICS),
                    {reply, ok, State#state{speeds = NSpeeds}};
                false ->
                    {reply, {error, quota_exceeded}, State}
            end
    end;

handle_call({unregister, all}, _From, State) ->
    [delete_counters(Topic) || {Topic, _CRef} <- ets:tab2list(?TAB)],
    {reply, ok, State#state{speeds = #{}}};

handle_call({unregister, Topic}, _From, State = #state{speeds = Speeds}) ->
    case is_registered(Topic) of
        false ->
            {reply, ok, State};
        true ->
            ok = delete_counters(Topic),
            NSpeeds = lists:foldl(fun(Metric, Acc) ->
                                      maps:remove({Topic, Metric}, Acc)
                                  end, Speeds, ?TOPIC_METRICS),
            {reply, ok, State#state{speeds = NSpeeds}}
    end;

handle_call({get_rate, Topic, Metric}, _From, State = #state{speeds = Speeds}) ->
    case is_registered(Topic) of
        false ->
            {reply, {error, topic_not_found}, State};
        true ->
            case maps:get({Topic, Metric}, Speeds, undefined) of
                undefined ->
                    {reply, {error, invalid_metric}, State};
                #speed{last = Last} ->
                    {reply, Last, State}
            end
    end.

handle_cast(Msg, State) ->
    ?LOG(error, "Unexpected cast: ~p", [Msg]),
    {noreply, State}.

handle_info(ticking, State = #state{speeds = Speeds}) ->
    NSpeeds = maps:map(
            fun({Topic, Metric}, Speed) ->
                case val(Topic, Metric) of
                    {error, topic_not_found} -> maps:remove({Topic, Metric}, Speeds);
                    Val -> calculate_speed(Val, Speed)
                end
            end, Speeds),
    erlang:send_after(timer:seconds(?TICKING_INTERVAL), self(), ticking),
    {noreply, State#state{speeds = NSpeeds}};

handle_info(Info, State) ->
    ?LOG(error, "Unexpected info: ~p", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%%------------------------------------------------------------------------------
%% Internal Functions
%%------------------------------------------------------------------------------

metric_idx('messages.in') ->       01;
metric_idx('messages.out') ->      02;
metric_idx('messages.qos0.in') ->  03;
metric_idx('messages.qos0.out') -> 04;
metric_idx('messages.qos1.in') ->  05;
metric_idx('messages.qos1.out') -> 06;
metric_idx('messages.qos2.in') ->  07;
metric_idx('messages.qos2.out') -> 08;
metric_idx('messages.dropped') ->  09;
metric_idx(_) ->
    {error, invalid_metric}.

to_count('messages.in') ->
    'messages.in.count';
to_count('messages.out') ->
    'messages.out.count';
to_count('messages.qos0.in') ->
    'messages.qos0.in.count';
to_count('messages.qos0.out') ->
    'messages.qos0.out.count';
to_count('messages.qos1.in') ->
    'messages.qos1.in.count';
to_count('messages.qos1.out') ->
    'messages.qos1.out.count';
to_count('messages.qos2.in') ->
    'messages.qos2.in.count';
to_count('messages.qos2.out') ->
    'messages.qos2.out.count';
to_count('messages.dropped') ->
    'messages.dropped.count'.

to_rate('messages.in') ->
    'messages.in.rate';
to_rate('messages.out') ->
    'messages.out.rate';
to_rate('messages.qos0.in') ->
    'messages.qos0.in.rate';
to_rate('messages.qos0.out') ->
    'messages.qos0.out.rate';
to_rate('messages.qos1.in') ->
    'messages.qos1.in.rate';
to_rate('messages.qos1.out') ->
    'messages.qos1.out.rate';
to_rate('messages.qos2.in') ->
    'messages.qos2.in.rate';
to_rate('messages.qos2.out') ->
    'messages.qos2.out.rate';
to_rate('messages.dropped') ->
    'messages.dropped.rate'.

delete_counters(Topic) ->
    true = ets:delete(?TAB, Topic),
    ok.

get_counters(Topic) ->
    case ets:lookup(?TAB, Topic) of
        [] -> {error, topic_not_found};
        [{Topic, CRef}] -> CRef
    end.

counters_size() ->
    length(?TOPIC_METRICS).

number_of_registered_topics() ->
    proplists:get_value(size, ets:info(?TAB)).

calculate_speed(CurVal, #speed{last_v = LastVal, tick = Tick, acc = Acc, samples = Samples}) ->
    %% calculate the current speed based on the last value of the counter
    CurSpeed = (CurVal - LastVal) / ?TICKING_INTERVAL,

    %% calculate the average speed in last 5 seconds
    case Tick < 5 of
        true ->
            Acc1 = Acc + CurSpeed,
            #speed{last = Acc1 / Tick,
                   last_v = CurVal,
                   acc = Acc1,
                   samples = Samples ++ [CurSpeed],
                   tick = Tick + 1};
        false ->
            [FirstSpeed | Speeds] = Samples,
            Acc1 =  Acc + CurSpeed - FirstSpeed,
            #speed{last = Acc1 / Tick,
                   last_v = CurVal,
                   acc = Acc1,
                   samples = Speeds ++ [CurSpeed],
                   tick = Tick}
    end.
