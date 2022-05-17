%%--------------------------------------------------------------------
%% Copyright (c) 2020-2022 EMQ Technologies Co., Ltd. All Rights Reserved.
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

-module(emqx_rule_engine_schema).

-include_lib("typerefl/include/types.hrl").
-include_lib("hocon/include/hoconsc.hrl").

-type duration_ms() :: integer().

-typerefl_from_string({duration_ms/0, emqx_schema, to_duration_ms}).

-behaviour(hocon_schema).

-reflect_type([
    duration_ms/0
]).

-export([
    namespace/0,
    roots/0,
    fields/1,
    desc/1
]).

-export([validate_sql/1, validate_rule_name/1]).

% workaround: prevent being recognized as unused functions
-export([
    to_duration_ms/1
]).

namespace() -> rule_engine.

roots() -> ["rule_engine"].

fields("rule_engine") ->
    [
        {ignore_sys_message,
            sc(boolean(), #{default => true, desc => ?DESC("rule_engine_ignore_sys_message")})},
        {rules,
            sc(hoconsc:map("id", ref("rules")), #{
                desc => ?DESC("rule_engine_rules"), default => #{}
            })},
        {jq_function_default_timeout,
            sc(
                duration_ms(),
                #{
                    default => "10s",
                    desc => ?DESC("rule_engine_jq_function_default_timeout")
                }
            )}
    ];
fields("rules") ->
    [
        rule_name(),
        {"sql",
            sc(
                binary(),
                #{
                    desc => ?DESC("rules_sql"),
                    example => "SELECT * FROM \"test/topic\" WHERE payload.x = 1",
                    required => true,
                    validator => fun ?MODULE:validate_sql/1
                }
            )},
        {"outputs",
            sc(
                hoconsc:array(hoconsc:union(outputs())),
                #{
                    desc => ?DESC("rules_outputs"),
                    default => [],
                    example => [
                        <<"http:my_http_bridge">>,
                        #{
                            function => republish,
                            args => #{
                                topic => <<"t/1">>, payload => <<"${payload}">>
                            }
                        },
                        #{function => console}
                    ]
                }
            )},
        {"enable", sc(boolean(), #{desc => ?DESC("rules_enable"), default => true})},
        {"description",
            sc(
                binary(),
                #{
                    desc => ?DESC("rules_description"),
                    example => "Some description",
                    default => <<>>
                }
            )}
    ];
fields("builtin_output_republish") ->
    [
        {function, sc(republish, #{desc => ?DESC("republish_function")})},
        {args, sc(ref("republish_args"), #{default => #{}})}
    ];
fields("builtin_output_console") ->
    [
        {function, sc(console, #{desc => ?DESC("console_function")})}
        %% we may support some args for the console output in the future
        %, {args, sc(map(), #{desc => "The arguments of the built-in 'console' output",
        %    default => #{}})}
    ];
fields("user_provided_function") ->
    [
        {function,
            sc(
                binary(),
                #{
                    desc => ?DESC("user_provided_function_function"),
                    required => true,
                    example => "module:function"
                }
            )},
        {args,
            sc(
                map(),
                #{
                    desc => ?DESC("user_provided_function_args"),
                    default => #{}
                }
            )}
    ];
fields("republish_args") ->
    [
        {topic,
            sc(
                binary(),
                #{
                    desc => ?DESC("republish_args_topic"),
                    required => true,
                    example => <<"a/1">>
                }
            )},
        {qos,
            sc(
                qos(),
                #{
                    desc => ?DESC("republish_args_qos"),
                    default => <<"${qos}">>,
                    example => <<"${qos}">>
                }
            )},
        {retain,
            sc(
                hoconsc:union([binary(), boolean()]),
                #{
                    desc => ?DESC("republish_args_retain"),
                    default => <<"${retain}">>,
                    example => <<"${retain}">>
                }
            )},
        {payload,
            sc(
                binary(),
                #{
                    desc => ?DESC("republish_args_payload"),
                    default => <<"${payload}">>,
                    example => <<"${payload}">>
                }
            )}
    ].

desc("rule_engine") ->
    ?DESC("desc_rule_engine");
desc("rules") ->
    ?DESC("desc_rules");
desc("builtin_output_republish") ->
    ?DESC("desc_builtin_output_republish");
desc("builtin_output_console") ->
    ?DESC("desc_builtin_output_console");
desc("user_provided_function") ->
    ?DESC("desc_user_provided_function");
desc("republish_args") ->
    ?DESC("desc_republish_args");
desc(_) ->
    undefined.

rule_name() ->
    {"name",
        sc(
            binary(),
            #{
                desc => ?DESC("rules_name"),
                default => "",
                required => true,
                example => "foo",
                validator => fun ?MODULE:validate_rule_name/1
            }
        )}.

validate_rule_name(Name) ->
    RE = "^[A-Za-z0-9]+[A-Za-z0-9-_]*$",
    try re:run(Name, RE) of
        {match, _} ->
            ok;
        _Nomatch ->
            Reason = list_to_binary(io_lib:format("Bad rule name ~p, expect ~p", [Name, RE])),
            {error, Reason}
    catch
        _:_ ->
            Reason = list_to_binary(io_lib:format("Bad rule name ~p, expect ~p", [Name, RE])),
            {error, Reason}
    end.

outputs() ->
    [
        binary(),
        ref("builtin_output_republish"),
        ref("builtin_output_console"),
        ref("user_provided_function")
    ].

qos() ->
    hoconsc:union([emqx_schema:qos(), binary()]).

validate_sql(Sql) ->
    case emqx_rule_sqlparser:parse(Sql) of
        {ok, _Result} -> ok;
        {error, Reason} -> {error, Reason}
    end.

sc(Type, Meta) -> hoconsc:mk(Type, Meta).
ref(Field) -> hoconsc:ref(?MODULE, Field).


-spec ceiling(number()) -> integer().
ceiling(X) ->
    T = erlang:trunc(X),
    case (X - T) of
        Neg when Neg < 0 -> T;
        Pos when Pos > 0 -> T + 1;
        _ -> T
    end.

-spec to_duration_ms(Input) -> {ok, integer()} | {error, Input} when
    Input :: string() | binary().
to_duration_ms(Str) ->
    case hocon_postprocess:duration(Str) of
        I when is_number(I) -> {ok, ceiling(I)};
        _ -> {error, Str}
    end.
