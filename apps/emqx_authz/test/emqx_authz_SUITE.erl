%%--------------------------------------------------------------------
%% Copyright (c) 2020-2021 EMQ Technologies Co., Ltd. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%--------------------------------------------------------------------

-module(emqx_authz_SUITE).

-compile(nowarn_export_all).
-compile(export_all).

-include("emqx_authz.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-define(CONF_DEFAULT, <<"authorization: {sources: []}">>).

all() ->
    emqx_ct:all(?MODULE).

groups() ->
    [].

init_per_suite(Config) ->
    meck:new(emqx_schema, [non_strict, passthrough, no_history, no_link]),
    meck:expect(emqx_schema, fields, fun("authorization") ->
                                             meck:passthrough(["authorization"]) ++
                                             emqx_authz_schema:fields("authorization");
                                        (F) -> meck:passthrough([F])
                                     end),

    meck:new(emqx_resource, [non_strict, passthrough, no_history, no_link]),
    meck:expect(emqx_resource, create, fun(_, _, _) -> {ok, meck_data} end),
    meck:expect(emqx_resource, update, fun(_, _, _, _) -> {ok, meck_data} end),
    meck:expect(emqx_resource, remove, fun(_) -> ok end ),

    ok = emqx_config:init_load(emqx_authz_schema, ?CONF_DEFAULT),
    ok = emqx_ct_helpers:start_apps([emqx_authz]),
    {ok, _} = emqx:update_config([authorization, cache, enable], false),
    {ok, _} = emqx:update_config([authorization, no_match], deny),
    Config.

end_per_suite(_Config) ->
    {ok, _} = emqx_authz:update(replace, []),
    emqx_ct_helpers:stop_apps([emqx_authz, emqx_resource]),
    meck:unload(emqx_resource),
    meck:unload(emqx_schema),
    ok.

init_per_testcase(_, Config) ->
    {ok, _} = emqx_authz:update(replace, []),
    Config.

-define(SOURCE1, #{<<"type">> => <<"http">>,
                   <<"enable">> => true,
                   <<"config">> => #{
                      <<"url">> => <<"https://fake.com:443/">>,
                      <<"headers">> => #{},
                      <<"method">> => <<"get">>,
                      <<"request_timeout">> => 5000}
                  }).
-define(SOURCE2, #{<<"type">> => <<"mongo">>,
                   <<"enable">> => true,
                   <<"config">> => #{
                          <<"mongo_type">> => <<"single">>,
                          <<"server">> => <<"127.0.0.1:27017">>,
                          <<"pool_size">> => 1,
                          <<"database">> => <<"mqtt">>,
                          <<"ssl">> => #{<<"enable">> => false}},
                   <<"collection">> => <<"fake">>,
                   <<"find">> => #{<<"a">> => <<"b">>}
                  }).
-define(SOURCE3, #{<<"type">> => <<"mysql">>,
                   <<"enable">> => true,
                   <<"config">> => #{
                       <<"server">> => <<"127.0.0.1:27017">>,
                       <<"pool_size">> => 1,
                       <<"database">> => <<"mqtt">>,
                       <<"username">> => <<"xx">>,
                       <<"password">> => <<"ee">>,
                       <<"auto_reconnect">> => true,
                       <<"ssl">> => #{<<"enable">> => false}},
                   <<"sql">> => <<"abcb">>
                  }).
-define(SOURCE4, #{<<"type">> => <<"pgsql">>,
                   <<"enable">> => true,
                   <<"config">> => #{
                       <<"server">> => <<"127.0.0.1:27017">>,
                       <<"pool_size">> => 1,
                       <<"database">> => <<"mqtt">>,
                       <<"username">> => <<"xx">>,
                       <<"password">> => <<"ee">>,
                       <<"auto_reconnect">> => true,
                       <<"ssl">> => #{<<"enable">> => false}},
                   <<"sql">> => <<"abcb">>
                  }).
-define(SOURCE5, #{<<"type">> => <<"redis">>,
                   <<"enable">> => true,
                   <<"config">> => #{
                       <<"server">> => <<"127.0.0.1:27017">>,
                       <<"pool_size">> => 1,
                       <<"database">> => 0,
                       <<"password">> => <<"ee">>,
                       <<"auto_reconnect">> => true,
                       <<"ssl">> => #{<<"enable">> => false}},
                   <<"cmd">> => <<"HGETALL mqtt_authz:%u">>
                  }).
-define(SOURCE6, #{<<"type">> => <<"file">>,
                   <<"enable">> => true,
                   <<"path">> => emqx_ct_helpers:deps_path(emqx_authz, "etc/authorization_rules.conf")
                  }).


%%------------------------------------------------------------------------------
%% Testcases
%%------------------------------------------------------------------------------

t_update_source(_) ->
    {ok, _} = emqx_authz:update(replace, [?SOURCE3]),
    {ok, _} = emqx_authz:update(head, [?SOURCE2]),
    {ok, _} = emqx_authz:update(head, [?SOURCE1]),
    {ok, _} = emqx_authz:update(tail, [?SOURCE4]),
    {ok, _} = emqx_authz:update(tail, [?SOURCE5]),
    {ok, _} = emqx_authz:update(tail, [?SOURCE6]),

    ?assertMatch([ #{type := http,  enable := true}
                 , #{type := mongo, enable := true}
                 , #{type := mysql, enable := true}
                 , #{type := pgsql, enable := true}
                 , #{type := redis, enable := true}
                 , #{type := file,  enable := true}
                 ], emqx:get_config([authorization, sources], [])),

    {ok, _} = emqx_authz:update({replace_once, http},  ?SOURCE1#{<<"enable">> := false}),
    {ok, _} = emqx_authz:update({replace_once, mongo}, ?SOURCE2#{<<"enable">> := false}),
    {ok, _} = emqx_authz:update({replace_once, mysql}, ?SOURCE3#{<<"enable">> := false}),
    {ok, _} = emqx_authz:update({replace_once, pgsql}, ?SOURCE4#{<<"enable">> := false}),
    {ok, _} = emqx_authz:update({replace_once, redis}, ?SOURCE5#{<<"enable">> := false}),
    {ok, _} = emqx_authz:update({replace_once, file},  ?SOURCE6#{<<"enable">> := false}),

    ?assertMatch([ #{type := http,  enable := false}
                 , #{type := mongo, enable := false}
                 , #{type := mysql, enable := false}
                 , #{type := pgsql, enable := false}
                 , #{type := redis, enable := false}
                 , #{type := file,  enable := false}
                 ], emqx:get_config([authorization, sources], [])),

    {ok, _} = emqx_authz:update(replace, []).

t_move_source(_) ->
    {ok, _} = emqx_authz:update(replace, [?SOURCE1, ?SOURCE2, ?SOURCE3, ?SOURCE4, ?SOURCE5, ?SOURCE6]),
    ?assertMatch([ #{type := http}
                 , #{type := mongo}
                 , #{type := mysql}
                 , #{type := pgsql}
                 , #{type := redis}
                 , #{type := file}
                 ], emqx_authz:lookup()),

    {ok, _} = emqx_authz:move(pgsql, <<"top">>),
    ?assertMatch([ #{type := pgsql}
                 , #{type := http}
                 , #{type := mongo}
                 , #{type := mysql}
                 , #{type := redis}
                 , #{type := file}
                 ], emqx_authz:lookup()),

    {ok, _} = emqx_authz:move(http, <<"bottom">>),
    ?assertMatch([ #{type := pgsql}
                 , #{type := mongo}
                 , #{type := mysql}
                 , #{type := redis}
                 , #{type := file}
                 , #{type := http}
                 ], emqx_authz:lookup()),

    {ok, _} = emqx_authz:move(mysql, #{<<"before">> => pgsql}),
    ?assertMatch([ #{type := mysql}
                 , #{type := pgsql}
                 , #{type := mongo}
                 , #{type := redis}
                 , #{type := file}
                 , #{type := http}
                 ], emqx_authz:lookup()),

    {ok, _} = emqx_authz:move(mongo, #{<<"after">> => http}),
    ?assertMatch([ #{type := mysql}
                 , #{type := pgsql}
                 , #{type := redis}
                 , #{type := file}
                 , #{type := http}
                 , #{type := mongo}
                 ], emqx_authz:lookup()),

    ok.
