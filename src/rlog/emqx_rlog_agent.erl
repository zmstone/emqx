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

%% @doc This module implements a gen_server which pushes rlogs to
%% a remote node.

-module(emqx_rlog_agent).

-behaviour(gen_server).

-export([start_link/3, stop/1]).

-export([ init/1
        , terminate/2
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , code_change/3
        ]).

start_link(Shard, Watcher, Opts) ->
    gen_server:start_link(?MODULE, {Shard, Watcher, Opts}).

stop(Pid) ->
    try
        gen_server:call(Pid, stop, infinity)
    catch
        exit : {noproc, _} ->
            %% race condition, the process exited
            %% before or during this gen_server:call
            ok
    end.

init({Shard, Watcher, Opts}) ->
    process_flag(trap_exit, true),
    {ok, #{ shard => Shard
          , watcher => Watcher
          , opts => Opts
          }}.

handle_info(_Info, St) ->
    {noreply, St}.

handle_cast(_Cast, St) ->
    {noreply, St}.

handle_call(_From, stop, St) ->
    {stop, normal, ok, St};
handle_call(_From, Call, St) ->
    {reply, {error, {unknown_call, Call}}, St}.

code_change(_OldVsn, St, _Extra) ->
    {ok, St}.

terminate(_Reason, St) ->
    {ok, St}.
