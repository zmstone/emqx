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

%% @doc This module implements a gen_statem which pushes rlogs to
%% a remote node.
%%
%% The state machine consists of 3 states:
%% `copybase', `catchup', `insync', which are explained in detail below:
%%
%% 1. `copybase'
%%    Tn copybase state, agent process tries to copy all records
%%    in the shard's tables to the watcher node.
%%    This state is skipped over to `catchup' state
%%    when the checkpoint (provided in start option) is found in the rlog
%%    table.
%% 2. `catchup'
%%    In this state, agent process sends the recorded changes in rlog
%%    table to watcher node.
%% 3. `insync'
%%    In this state, agent process subscribes to rlog table and sends
%%    the realtime updates to the watcher node.
%%
%% All sends are done as `gen_rpc' calls to the watcher node.

-module(emqx_rlog_agent).

-behaviour(gen_statem).

-export([start_link/3, stop/1]).

-export([init/1, terminate/3, code_change/4, callback_mode/0]).

-export([copybase/3, catchup/3, insync/3]).

%% Concurrent transactions (rlog entries) may each other and result in
%% disorder. e.g. transaction A having timestamp 1 is logged *after*
%% transaction B with timestamp 2.
-define(CHECKPOINT_MARGIN_SECCONDS, 60).

start_link(Shard, Watcher, Opts) ->
    gen_statem:start_link(?MODULE, {Shard, Watcher, Opts}, []).

stop(Pid) ->
    try
        gen_statem:call(Pid, stop, infinity)
    catch
        exit : {noproc, _} ->
            %% race condition, the process exited
            %% before or during this call
            ok
    end.

callback_mode() -> [state_functions, state_enter].

init({Shard, Watcher, Opts}) ->
    process_flag(trap_exit, true),
    Checkpoint = safe_margin(maps:get(checkpoint, Opts)),
    Data = #{ shard => Shard
            , watcher => Watcher
            , checkpoint => Checkpoint
            },
    {ok, copybase, Data}.

copybase(enter, _OldState, _Data) ->
    keep_state_and_data;
copybase(Type, Event, Data) ->
    handle_event(Type, Event, ?FUNCTION_NAME, Data).

catchup(enter, _OldState, _Data) ->
    keep_state_and_data;
catchup(Type, Event, Data) ->
    handle_event(Type, Event, ?FUNCTION_NAME, Data).

insync(enter, _OldState, _Data) ->
    keep_state_and_data;
insync(Type, Event, Data) ->
    handle_event(Type, Event, ?FUNCTION_NAME, Data).

handle_event({call, From}, stop, _State, _Data) ->
    {stop_and_reply, normal, {reply, From, ok}};
handle_event(_Type, _Event, _State, _Data) ->
    keep_state_and_data.

code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.

terminate(_Reason, _State, _Data) ->
    ok.

safe_margin(no_checkpoint) -> no_checkpoint;
safe_margin(Ts) ->
    R = Ts - ?CHECKPOINT_MARGIN_SECCONDS * 1000000000,
    case R < 0 of
        true -> 0;
        _ -> R
    end.
