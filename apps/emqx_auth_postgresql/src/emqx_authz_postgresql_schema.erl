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

-module(emqx_authz_postgresql_schema).

-include("emqx_auth_postgresql.hrl").
-include_lib("hocon/include/hoconsc.hrl").

-behaviour(emqx_authz_schema).

-export([
    type/0,
    fields/1,
    desc/1,
    source_refs/0,
    select_union_member/1,
    namespace/0
]).

namespace() -> authz.

type() -> ?AUTHZ_TYPE.

fields(postgresql) ->
    emqx_authz_schema:authz_common_fields(?AUTHZ_TYPE) ++
        emqx_connector_pgsql:fields(config) ++
        [{query, query()}].

desc(postgresql) ->
    ?DESC(postgresql);
desc(_) ->
    undefined.

source_refs() ->
    [?R_REF(postgresql)].

select_union_member(#{<<"type">> := ?AUTHZ_TYPE_BIN}) ->
    ?R_REF(postgresql);
select_union_member(_Value) ->
    undefined.

%%--------------------------------------------------------------------
%% Internal Functions
%%--------------------------------------------------------------------

query() ->
    ?HOCON(binary(), #{
        desc => ?DESC(query),
        required => true,
        validator => fun(S) ->
            case size(S) > 0 of
                true -> ok;
                _ -> {error, "Request query"}
            end
        end
    }).
