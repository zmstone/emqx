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

-module(emqx_authz_redis_schema).

-include("emqx_auth_redis.hrl").
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

fields(redis_single) ->
    emqx_authz_schema:authz_common_fields(?AUTHZ_TYPE) ++
        emqx_redis:fields(single) ++
        [{cmd, cmd()}];
fields(redis_sentinel) ->
    emqx_authz_schema:authz_common_fields(?AUTHZ_TYPE) ++
        emqx_redis:fields(sentinel) ++
        [{cmd, cmd()}];
fields(redis_cluster) ->
    emqx_authz_schema:authz_common_fields(?AUTHZ_TYPE) ++
        emqx_redis:fields(cluster) ++
        [{cmd, cmd()}].

desc(redis_single) ->
    ?DESC(redis_single);
desc(redis_sentinel) ->
    ?DESC(redis_sentinel);
desc(redis_cluster) ->
    ?DESC(redis_cluster);
desc(_) ->
    undefined.

source_refs() ->
    [?R_REF(redis_single), ?R_REF(redis_sentinel), ?R_REF(redis_cluster)].

select_union_member(#{<<"type">> := ?AUTHZ_TYPE_BIN} = Value) ->
    RedisType = maps:get(<<"redis_type">>, Value, undefined),
    case RedisType of
        <<"single">> ->
            ?R_REF(redis_single);
        <<"cluster">> ->
            ?R_REF(redis_cluster);
        <<"sentinel">> ->
            ?R_REF(redis_sentinel);
        Else ->
            throw(#{
                reason => "unknown_redis_type",
                expected => "single | cluster | sentinel",
                got => Else
            })
    end;
select_union_member(_Value) ->
    undefined.

%%--------------------------------------------------------------------
%% Internal Functions
%%--------------------------------------------------------------------

cmd() ->
    ?HOCON(binary(), #{
        desc => ?DESC(cmd),
        required => true,
        validator => fun(S) ->
            case size(S) > 0 of
                true -> ok;
                _ -> {error, "Request query"}
            end
        end,
        example => <<"HGETALL mqtt_authz">>
    }).
