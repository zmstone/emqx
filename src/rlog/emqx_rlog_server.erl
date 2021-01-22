%%--------------------------------------------------------------------
%% Copyright (c) 2021 EMQ Technologies Co., Ltd. All Rights Reserved.
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

%% @doc This module accepts watch requests, and spawns workers processes
%% to feed transaction logs to watchers.

-module(emqx_rlog_server).

-behaviour(gen_server).

-export([ start_link/2
        , watch/3
        , unwatch/2
        ]).

%% gen_server callbacks
-export([ init/1
        , terminate/2
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , code_change/3
        ]).

-export_type([ watcher/0
             , checkpoint/0
             , watch_opts/0
             ]).

-type shard() :: emqx_tx:shard().
-type watcher() :: node() | pid().
-type rlog_ts() :: emqx_rlog:rlog_ts().
-type checkpoint() :: no_checkpoint | rlog_ts().
-type watch_opts() :: #{checkpoint := checkpoint()}.

start_link(Shard, Config) ->
    gen_server:start_link({local, Shard}, ?MODULE, {Shard, Config}).

-spec watch(shard(), watcher(), watch_opts()) -> ok.
watch(Shard, Watcher, Opts) ->
    gen_server:call(Shard, {watch, Watcher, Opts}, infinity).

-spec unwatch(shard(), watcher()) -> ok.
unwatch(Shard, Watcher) ->
    gen_server:call(Shard, {unwatch, Watcher}, infinity).

init({Shard, Config}) ->
    process_flag(trap_exit, true),
    {ok, #{ agents => #{}
          , shard => Shard
          , cleaner => start_cleaner(Shard, Config)
          }}.

handle_info(_Info, St) ->
    %% TODO handle watcher EXIT:s
    {noreply, St}.

handle_cast(_Cast, St) ->
    {noreply, St}.

handle_call(_From, {watch, Watcher, Opts}, St) ->
    {reply, ok, do_watch(St, Watcher, Opts)};
handle_call(_From, {unwatch, Watcher}, St) ->
    {reply, ok, do_unwatch(St, Watcher)};
handle_call(_From, Call, St) ->
    {reply, {error, {unknown_call, Call}}, St}.

code_change(_OldVsn, St, _Extra) ->
    {ok, St}.

terminate(_Reason, #{cleaner := Cleaner} = St) ->
    ok = emqx_rlog_cleaner:stop(Cleaner),
    %% TODO: stop agents
    {ok, St}.

do_watch(#{agents := Agents0, shard := Shard} = St, Watcher, Opts) ->
    Agents =
        case maps:is_key(Watcher, Agents0) of
            true  -> Agents0;
            false -> Agents0#{Watcher => start_agent(Shard, Watcher, Opts)}
        end,
    St#{agents := Agents}.

do_unwatch(#{agents := Agents} = St, Watcher) ->
    case maps:get(Watcher, Agents, false) of
        false -> ok;
        Pid   -> emqx_rlog_agent:stop(Pid)
    end,
    St#{agents := maps:withoug([Watcher], Agents)}.

start_agent(Shard, Watcher, Opts) ->
    {ok, Pid} = emqx_rlog_agent:start_link(Shard, Watcher, Opts),
    Pid.

start_cleaner(Shard, Config) ->
    {ok, Pid} = emqx_rlog_cleaner:start_link(Shard, Config),
    Pid.

