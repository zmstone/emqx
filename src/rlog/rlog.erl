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

%% replicated logs
%% mnesia (with rocksdb backend) is for replication
-module(rlog).

-export([mnesia/1]).

-export([tx/3]).

%% for tests
-export([write/2]).

-boot_mnesia({mnesia, [boot]}).

-define(TAB, rlog).

mnesia(boot) ->
    %% mnesia_rocksdb:register() should be called immediately after mnesia is started.
    %% TODO: move to a common place, maybe ekka
    mnesia_rocksdb:register(),
    %% TODO optimize for logging. e.g. there should be no compaction required because
    %% all keys are unique
    RocksProps = [{max_open_files, 1024}],
    UserProps = [{rocksdb_opts, RocksProps}],
    ekka_mnesia:create_table(?TAB, [{rocksdb_copies, [node()]},
                                    {attributes, [key, value]},
                                    {type, ordered_set},
                                    {user_properties, UserProps}]).

%% call apply(M, F, A) in an mnesia transaction, and log A in the rocksdb backend.
-spec tx(module(), atom(), [term()]) -> {atomic, any()} | {aborted, any()}.
tx(M, F, Args) ->
    NodeId = node_id(),
    TxFun =
        fun() ->
                Time = erlang:system_time(),
                Result = apply(M, F, Args),
                ok = do_write({Time, NodeId, M, F}, Args),
                Result
        end,
    mnesia:transaction(TxFun).

%% for tests only
-spec write(term(), term()) -> {atomic, any()} | {aborted, any()}.
write(Key, Args) ->
    mnesia:transaction(fun() -> do_write(Key, Args) end).

do_write(Key, Args) ->
    ok = mnesia:write(?TAB, {?TAB, Key, Args}, write).

%% TODO read this from config
node_id() -> 0.

