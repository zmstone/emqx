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

-export([do/3]).

-export_type([ args/0
             , shard/0
             ]).

-include("emqx_rlog.hrl").
-include_lib("mnesia/src/mnesia.hrl").

-type shard() :: ?SHARD_ROUTING.
-type args() :: [term()].

%% @doc Perform a transaction and log changes.
%% the logged changes are to be replicated to other nodes.
-spec do(shard(), function(), args()) ->
        {atomic, term()} | {aborted, term()}.
do(Shard, F, Args) ->
    TxFun =
        fun() ->
                Result = apply(F, Args),
                Ops = get_tx_ops(Shard),
                ok = emqx_rlog_tab:write(Shard, emqx_rlog:make_key(), Ops),
                Result
        end,
    mnesia:transaction(TxFun).

get_tx_ops(Shard) ->
    {_, _, Store} = mnesia:get_activity_id(),
    filter(Shard, ets:tab2list(Store#tidstore.store)).

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
