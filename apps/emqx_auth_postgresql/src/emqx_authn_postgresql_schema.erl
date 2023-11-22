%%--------------------------------------------------------------------
%% Copyright (c) 2020-2023 EMQ Technologies Co., Ltd. All Rights Reserved.
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

-module(emqx_authn_postgresql_schema).

-include("emqx_auth_postgresql.hrl").
-include_lib("hocon/include/hoconsc.hrl").

-behaviour(emqx_authn_schema).

-export([
    fields/1,
    desc/1,
    refs/0,
    select_union_member/1,
    namespace/0
]).

namespace() -> authn.

select_union_member(
    #{
        <<"mechanism">> := ?AUTHN_MECHANISM_BIN, <<"backend">> := ?AUTHN_BACKEND_BIN
    }
) ->
    refs();
select_union_member(#{<<"backend">> := ?AUTHN_BACKEND_BIN}) ->
    throw(#{
        reason => "unknown_mechanism",
        expected => ?AUTHN_MECHANISM
    });
select_union_member(_) ->
    undefined.

fields(postgresql) ->
    [
        {mechanism, emqx_authn_schema:mechanism(?AUTHN_MECHANISM)},
        {backend, emqx_authn_schema:backend(?AUTHN_BACKEND)},
        {password_hash_algorithm, fun emqx_authn_password_hashing:type_ro/1},
        {query, fun query/1}
    ] ++
        emqx_authn_schema:common_fields() ++
        proplists:delete(prepare_statement, emqx_connector_pgsql:fields(config)).

desc(postgresql) ->
    ?DESC(postgresql);
desc(_) ->
    undefined.

query(type) -> string();
query(desc) -> ?DESC(?FUNCTION_NAME);
query(required) -> true;
query(_) -> undefined.

refs() ->
    [hoconsc:ref(?MODULE, postgresql)].
