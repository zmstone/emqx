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

%% transaction log replication
-module(emqx_rlog).

-export([ make_key/0
        ]).

-export_type([ key/0
             , node_id/0
             ]).

-type key() :: {integer(), node_id()}.
-type node_id() :: integer().

%% Log key should be monotoic and globally unique.
%% it is a tuple of a timestamp (ts) and the node id (node_id),
%% where ts is at nanosecond precesion to ensure it is locally
%% monotoic and unique, and node_id, which identifies the node which
%% initiated the transaction, should ensure global uniqueness.
%%
%% NOTE: In case of two or more concurrent transactions,
%% it is possible to have a later transction logged with
%% an earlier ts if there is no conflict in the content
%% the concurrent transactions updated, otherwise we rely on
%% mnesia locks to ensure linearbility.
make_key() -> {erlang:system_time(nanosecond), node_id()}.

%% TODO: configurable
node_id() -> 0.
