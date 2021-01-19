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

%% emtrypoint for replicated transactions
-module(emqx_tx).

-export([transaction/3]).
-export([async_dirty/3]).

-export_type([ args/0
             , shard/0
             , func/0
             , reason/0
             , result/0
             ]).

-include("emqx_rlog.hrl").
-include_lib("mnesia/src/mnesia.hrl").

-type shard() :: ?SHARD_ROUTING.
-type args() :: [term()].
-type result() :: term(). %% transaction function result
-type reason() :: term(). %% transaction abort reason
-type func() :: fun((...) -> result()).

%% @doc Perform a transaction and log changes.
%% the logged changes are to be replicated to other nodes.
-spec transaction(shard(), func(), args()) -> {atomic, result()} | {aborted, reason()}.
transaction(Shard, F, Args) -> do(transaction, Shard, F, Args).

%% @doc TODO
-spec async_dirty(shard(), func(), args()) -> result().
async_dirty(Shard, F, Args) -> do(async_dirty, Shard, F, Args).

do(Type, Shard, F, Args) ->
    TxFun =
        fun() ->
                Result = apply(F, Args),
                Ops = get_tx_ops(Shard, F, Args),
                case Ops =:= [] of
                    true ->
                        %% nothing to log, avoid creating a key
                        ok;
                    false ->
                        Key = emqx_rlog:make_key(),
                        ok = emqx_rlog_tab:write(Shard, Key, Ops)
                end,
                Result
        end,
    case Type of
        transaction -> mnesia:transaction(TxFun);
        async_dirty -> mnesia:async_dirty(TxFun)
    end.

get_tx_ops(Shard, F, Args) ->
    {_, _, Store} = mnesia:get_activity_id(),
    case Store of
        non_transaction ->
            args_as_op(F, Args);
        #tidstore{store = Ets} ->
            AllOps = ets:tab2list(Ets),
            filter(Shard, AllOps)
    end.

filter(Shard, Ops) -> filter(Shard, Ops, []).

filter(_Shard, [], Acc) ->
    lists:reverse(Acc);
filter(Shard, [Op | Ops], Acc) ->
    case is_change_type_for_logging(Op) of
        true -> filter(Shard, Ops, ops_for_shard(Shard, Op, Acc));
        false -> filter(Shard, Ops, Acc)
    end.

%% return true if the mnesia op is a write or delete (delete_object for bags)
is_change_type_for_logging({{_Tab, _Key}, _Record, C}) ->
    C =:= write orelse C =:= delete orelse C =:= delete_object;
is_change_type_for_logging(_) ->
    false.

%% filter (and maybe transform) a table op for the given shard for logging.
ops_for_shard(?SHARD_ROUTING, Op, Acc) ->
    {{Tab, _Key}, _Record, _C} = Op,
    case is_routing_table(Tab) of
        true -> [Op | Acc];
        false -> Acc
    end.

is_routing_table(emqx_trie) -> true;
is_routing_table(emqx_trie_node) -> true;
is_routing_table(emqx_route) -> true;
is_routing_table(_) -> false.

%% we can only hope that this is not an anonymous function
%% add the function is idempotent.
args_as_op(F, Args) -> [{F, Args, apply}].

