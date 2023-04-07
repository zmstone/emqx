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
-module(emqx_conf).

-compile({no_auto_import, [get/1, get/2]}).
-include_lib("emqx/include/logger.hrl").
-include_lib("hocon/include/hoconsc.hrl").

-export([add_handler/2, remove_handler/1]).
-export([get/1, get/2, get_raw/1, get_raw/2, get_all/1]).
-export([get_by_node/2, get_by_node/3]).
-export([update/3, update/4]).
-export([remove/2, remove/3]).
-export([reset/2, reset/3]).
-export([dump_schema/2]).
-export([schema_module/0]).
-export([gen_example_conf/2]).

%% for rpc
-export([get_node_and_config/1]).

%% API
%% @doc Adds a new config handler to emqx_config_handler.
-spec add_handler(emqx_config:config_key_path(), module()) -> ok.
add_handler(ConfKeyPath, HandlerName) ->
    emqx_config_handler:add_handler(ConfKeyPath, HandlerName).

%% @doc remove config handler from emqx_config_handler.
-spec remove_handler(emqx_config:config_key_path()) -> ok.
remove_handler(ConfKeyPath) ->
    emqx_config_handler:remove_handler(ConfKeyPath).

-spec get(emqx_map_lib:config_key_path()) -> term().
get(KeyPath) ->
    emqx:get_config(KeyPath).

-spec get(emqx_map_lib:config_key_path(), term()) -> term().
get(KeyPath, Default) ->
    emqx:get_config(KeyPath, Default).

-spec get_raw(emqx_map_lib:config_key_path(), term()) -> term().
get_raw(KeyPath, Default) ->
    emqx_config:get_raw(KeyPath, Default).

-spec get_raw(emqx_map_lib:config_key_path()) -> term().
get_raw(KeyPath) ->
    emqx_config:get_raw(KeyPath).

%% @doc Returns all values in the cluster.
-spec get_all(emqx_map_lib:config_key_path()) -> #{node() => term()}.
get_all(KeyPath) ->
    {ResL, []} = emqx_conf_proto_v2:get_all(KeyPath),
    maps:from_list(ResL).

%% @doc Returns the specified node's KeyPath, or exception if not found
-spec get_by_node(node(), emqx_map_lib:config_key_path()) -> term().
get_by_node(Node, KeyPath) when Node =:= node() ->
    emqx:get_config(KeyPath);
get_by_node(Node, KeyPath) ->
    emqx_conf_proto_v2:get_config(Node, KeyPath).

%% @doc Returns the specified node's KeyPath, or the default value if not found
-spec get_by_node(node(), emqx_map_lib:config_key_path(), term()) -> term().
get_by_node(Node, KeyPath, Default) when Node =:= node() ->
    emqx:get_config(KeyPath, Default);
get_by_node(Node, KeyPath, Default) ->
    emqx_conf_proto_v2:get_config(Node, KeyPath, Default).

%% @doc Returns the specified node's KeyPath, or config_not_found if key path not found
-spec get_node_and_config(emqx_map_lib:config_key_path()) -> term().
get_node_and_config(KeyPath) ->
    {node(), emqx:get_config(KeyPath, config_not_found)}.

%% @doc Update all value of key path in cluster-override.conf or local-override.conf.
-spec update(
    emqx_map_lib:config_key_path(),
    emqx_config:update_request(),
    emqx_config:update_opts()
) ->
    {ok, emqx_config:update_result()} | {error, emqx_config:update_error()}.
update(KeyPath, UpdateReq, Opts) ->
    emqx_conf_proto_v2:update(KeyPath, UpdateReq, Opts).

%% @doc Update the specified node's key path in local-override.conf.
-spec update(
    node(),
    emqx_map_lib:config_key_path(),
    emqx_config:update_request(),
    emqx_config:update_opts()
) ->
    {ok, emqx_config:update_result()} | {error, emqx_config:update_error()} | emqx_rpc:badrpc().
update(Node, KeyPath, UpdateReq, Opts0) when Node =:= node() ->
    emqx:update_config(KeyPath, UpdateReq, Opts0#{override_to => local});
update(Node, KeyPath, UpdateReq, Opts) ->
    emqx_conf_proto_v2:update(Node, KeyPath, UpdateReq, Opts).

%% @doc remove all value of key path in cluster-override.conf or local-override.conf.
-spec remove(emqx_map_lib:config_key_path(), emqx_config:update_opts()) ->
    {ok, emqx_config:update_result()} | {error, emqx_config:update_error()}.
remove(KeyPath, Opts) ->
    emqx_conf_proto_v2:remove_config(KeyPath, Opts).

%% @doc remove the specified node's key path in local-override.conf.
-spec remove(node(), emqx_map_lib:config_key_path(), emqx_config:update_opts()) ->
    {ok, emqx_config:update_result()} | {error, emqx_config:update_error()}.
remove(Node, KeyPath, Opts) when Node =:= node() ->
    emqx:remove_config(KeyPath, Opts#{override_to => local});
remove(Node, KeyPath, Opts) ->
    emqx_conf_proto_v2:remove_config(Node, KeyPath, Opts).

%% @doc reset all value of key path in cluster-override.conf or local-override.conf.
-spec reset(emqx_map_lib:config_key_path(), emqx_config:update_opts()) ->
    {ok, emqx_config:update_result()} | {error, emqx_config:update_error()}.
reset(KeyPath, Opts) ->
    emqx_conf_proto_v2:reset(KeyPath, Opts).

%% @doc reset the specified node's key path in local-override.conf.
-spec reset(node(), emqx_map_lib:config_key_path(), emqx_config:update_opts()) ->
    {ok, emqx_config:update_result()} | {error, emqx_config:update_error()}.
reset(Node, KeyPath, Opts) when Node =:= node() ->
    emqx:reset_config(KeyPath, Opts#{override_to => local});
reset(Node, KeyPath, Opts) ->
    emqx_conf_proto_v2:reset(Node, KeyPath, Opts).

%% @doc Called from build script.
dump_schema(Dir, SchemaModule) ->
    _ = application:load(emqx_dashboard),
    ok = emqx_dashboard_desc_cache:init(),
    lists:foreach(
        fun(Lang) ->
            ok = gen_config_md(Dir, SchemaModule, Lang),
            ok = gen_api_schema_json(Dir, Lang),
            ok = gen_schema_json(Dir, SchemaModule, Lang)
        end,
        ["en", "zh"]
    ),
    ok = gen_example_conf(Dir, SchemaModule).

%% for scripts/spellcheck.
gen_schema_json(Dir, SchemaModule, Lang) ->
    SchemaJsonFile = filename:join([Dir, "schema-" ++ Lang ++ ".json"]),
    io:format(user, "===< Generating: ~s~n", [SchemaJsonFile]),
    %% EMQX_SCHEMA_FULL_DUMP is quite a hidden API
    %% it is used to dump the full schema for EMQX developers and supporters
    IncludeImportance =
        case os:getenv("EMQX_SCHEMA_FULL_DUMP") =:= "1" of
            true -> ?IMPORTANCE_HIDDEN;
            false -> ?IMPORTANCE_LOW
        end,
    io:format(user, "===< Including fields from importance level: ~p~n", [IncludeImportance]),
    Opts = #{
        include_importance_up_from => IncludeImportance,
        desc_resolver => make_desc_resolver(Lang)
    },
    JsonMap = hocon_schema_json:gen(SchemaModule, Opts),
    IoData = jsx:encode(JsonMap, [space, {indent, 4}]),
    ok = file:write_file(SchemaJsonFile, IoData).

gen_api_schema_json(Dir, Lang) ->
    gen_api_schema_json_hotconf(Dir, Lang),
    gen_api_schema_json_bridge(Dir, Lang).

gen_api_schema_json_hotconf(Dir, Lang) ->
    SchemaInfo = #{title => <<"EMQX Hot Conf API Schema">>, version => <<"0.1.0">>},
    File = schema_filename(Dir, "hot-config-schema-", Lang),
    ok = do_gen_api_schema_json(File, emqx_mgmt_api_configs, SchemaInfo, Lang).

gen_api_schema_json_bridge(Dir, Lang) ->
    SchemaInfo = #{title => <<"EMQX Data Bridge API Schema">>, version => <<"0.1.0">>},
    File = schema_filename(Dir, "bridge-api-", Lang),
    ok = do_gen_api_schema_json(File, emqx_bridge_api, SchemaInfo, Lang).

schema_filename(Dir, Prefix, Lang) ->
    Filename = Prefix ++ Lang ++ ".json",
    filename:join([Dir, Filename]).

%% TODO: remove it and also remove hocon_md.erl and friends.
%% markdown generation from schema is a failure and we are moving to an interactive
%% viewer like swagger UI.
gen_config_md(Dir, SchemaModule, Lang) ->
    SchemaMdFile = filename:join([Dir, "config-" ++ Lang ++ ".md"]),
    io:format(user, "===< Generating: ~s~n", [SchemaMdFile]),
    ok = gen_doc(SchemaMdFile, SchemaModule, Lang).

gen_example_conf(Dir, SchemaModule) ->
    SchemaMdFile = filename:join([Dir, "emqx.conf.example"]),
    io:format(user, "===< Generating: ~s~n", [SchemaMdFile]),
    ok = gen_example(SchemaMdFile, SchemaModule).

%% @doc return the root schema module.
-spec schema_module() -> module().
schema_module() ->
    case os:getenv("SCHEMA_MOD") of
        false -> emqx_conf_schema;
        Value -> list_to_existing_atom(Value)
    end.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

%% @doc Make a resolver function that can be used to lookup the description by hocon_schema_json dump.
make_desc_resolver(Lang) ->
    fun
        ({desc, Namespace, Id}) ->
            emqx_dashboard_desc_cache:lookup(Lang, Namespace, Id, desc);
        (Desc) ->
            Desc
    end.

-spec gen_doc(file:name_all(), module(), string()) -> ok.
gen_doc(File, SchemaModule, Lang) ->
    Version = emqx_release:version(),
    Title =
        "# " ++ emqx_release:description() ++ " Configuration\n\n" ++
            "<!--" ++ Version ++ "-->",
    BodyFile = filename:join([rel, "emqx_conf.template." ++ Lang ++ ".md"]),
    {ok, Body} = file:read_file(BodyFile),
    Resolver = make_desc_resolver(Lang),
    Opts = #{title => Title, body => Body, desc_resolver => Resolver},
    Doc = hocon_schema_md:gen(SchemaModule, Opts),
    file:write_file(File, Doc).

gen_example(File, SchemaModule) ->
    %% we do not generate description in example files
    %% so there is no need for a desc_resolver
    Opts = #{
        title => <<"EMQX Configuration Example">>,
        body => <<"">>,
        include_importance_up_from => ?IMPORTANCE_MEDIUM
    },
    Example = hocon_schema_example:gen(SchemaModule, Opts),
    file:write_file(File, Example).

%% Only gen hot_conf schema, not all configuration fields.
do_gen_api_schema_json(File, SchemaMod, SchemaInfo, Lang) ->
    io:format(user, "===< Generating: ~s~n", [File]),
    {ApiSpec0, Components0} = emqx_dashboard_swagger:spec(
        SchemaMod,
        #{
            schema_converter => fun hocon_schema_to_spec/2,
            i18n_lang => Lang
        }
    ),
    ApiSpec = lists:foldl(
        fun({Path, Spec, _, _}, Acc) ->
            NewSpec = maps:fold(
                fun(Method, #{responses := Responses}, SubAcc) ->
                    case Responses of
                        #{
                            <<"200">> :=
                                #{
                                    <<"content">> := #{
                                        <<"application/json">> := #{<<"schema">> := Schema}
                                    }
                                }
                        } ->
                            SubAcc#{Method => Schema};
                        _ ->
                            SubAcc
                    end
                end,
                #{},
                Spec
            ),
            Acc#{list_to_atom(Path) => NewSpec}
        end,
        #{},
        ApiSpec0
    ),
    Components = lists:foldl(fun(M, Acc) -> maps:merge(M, Acc) end, #{}, Components0),
    IoData = jsx:encode(
        #{
            info => SchemaInfo,
            paths => ApiSpec,
            components => #{schemas => Components}
        },
        [space, {indent, 4}]
    ),
    file:write_file(File, IoData).

-define(TO_REF(_N_, _F_), iolist_to_binary([to_bin(_N_), ".", to_bin(_F_)])).
-define(TO_COMPONENTS_SCHEMA(_M_, _F_),
    iolist_to_binary([
        <<"#/components/schemas/">>,
        ?TO_REF(emqx_dashboard_swagger:namespace(_M_), _F_)
    ])
).

hocon_schema_to_spec(?R_REF(Module, StructName), _LocalModule) ->
    {#{<<"$ref">> => ?TO_COMPONENTS_SCHEMA(Module, StructName)}, [{Module, StructName}]};
hocon_schema_to_spec(?REF(StructName), LocalModule) ->
    {#{<<"$ref">> => ?TO_COMPONENTS_SCHEMA(LocalModule, StructName)}, [{LocalModule, StructName}]};
hocon_schema_to_spec(Type, LocalModule) when ?IS_TYPEREFL(Type) ->
    {typename_to_spec(typerefl:name(Type), LocalModule), []};
hocon_schema_to_spec(?ARRAY(Item), LocalModule) ->
    {Schema, Refs} = hocon_schema_to_spec(Item, LocalModule),
    {#{type => array, items => Schema}, Refs};
hocon_schema_to_spec(?ENUM(Items), _LocalModule) ->
    {#{type => enum, symbols => Items}, []};
hocon_schema_to_spec(?MAP(Name, Type), LocalModule) ->
    {Schema, SubRefs} = hocon_schema_to_spec(Type, LocalModule),
    {
        #{
            <<"type">> => object,
            <<"properties">> => #{<<"$", (to_bin(Name))/binary>> => Schema}
        },
        SubRefs
    };
hocon_schema_to_spec(?UNION(Types), LocalModule) ->
    {OneOf, Refs} = lists:foldl(
        fun(Type, {Acc, RefsAcc}) ->
            {Schema, SubRefs} = hocon_schema_to_spec(Type, LocalModule),
            {[Schema | Acc], SubRefs ++ RefsAcc}
        end,
        {[], []},
        hoconsc:union_members(Types)
    ),
    {#{<<"oneOf">> => OneOf}, Refs};
hocon_schema_to_spec(Atom, _LocalModule) when is_atom(Atom) ->
    {#{type => enum, symbols => [Atom]}, []}.

typename_to_spec("user_id_type()", _Mod) ->
    #{type => enum, symbols => [clientid, username]};
typename_to_spec("term()", _Mod) ->
    #{type => string};
typename_to_spec("boolean()", _Mod) ->
    #{type => boolean};
typename_to_spec("binary()", _Mod) ->
    #{type => string};
typename_to_spec("float()", _Mod) ->
    #{type => number};
typename_to_spec("integer()", _Mod) ->
    #{type => number};
typename_to_spec("non_neg_integer()", _Mod) ->
    #{type => number, minimum => 1};
typename_to_spec("number()", _Mod) ->
    #{type => number};
typename_to_spec("string()", _Mod) ->
    #{type => string};
typename_to_spec("atom()", _Mod) ->
    #{type => string};
typename_to_spec("duration()", _Mod) ->
    #{type => duration};
typename_to_spec("duration_s()", _Mod) ->
    #{type => duration};
typename_to_spec("duration_ms()", _Mod) ->
    #{type => duration};
typename_to_spec("percent()", _Mod) ->
    #{type => percent};
typename_to_spec("file()", _Mod) ->
    #{type => string};
typename_to_spec("ip_port()", _Mod) ->
    #{type => ip_port};
typename_to_spec("url()", _Mod) ->
    #{type => url};
typename_to_spec("bytesize()", _Mod) ->
    #{type => 'byteSize'};
typename_to_spec("wordsize()", _Mod) ->
    #{type => 'byteSize'};
typename_to_spec("qos()", _Mod) ->
    #{type => enum, symbols => [0, 1, 2]};
typename_to_spec("comma_separated_list()", _Mod) ->
    #{type => comma_separated_string};
typename_to_spec("comma_separated_atoms()", _Mod) ->
    #{type => comma_separated_string};
typename_to_spec("pool_type()", _Mod) ->
    #{type => enum, symbols => [random, hash]};
typename_to_spec("log_level()", _Mod) ->
    #{
        type => enum,
        symbols => [
            debug,
            info,
            notice,
            warning,
            error,
            critical,
            alert,
            emergency,
            all
        ]
    };
typename_to_spec("rate()", _Mod) ->
    #{type => string};
typename_to_spec("capacity()", _Mod) ->
    #{type => string};
typename_to_spec("burst_rate()", _Mod) ->
    #{type => string};
typename_to_spec("failure_strategy()", _Mod) ->
    #{type => enum, symbols => [force, drop, throw]};
typename_to_spec("initial()", _Mod) ->
    #{type => string};
typename_to_spec("map()", _Mod) ->
    #{type => object};
typename_to_spec("#{" ++ _, Mod) ->
    typename_to_spec("map()", Mod);
typename_to_spec(Name, Mod) ->
    Spec = range(Name),
    Spec1 = remote_module_type(Spec, Name, Mod),
    Spec2 = typerefl_array(Spec1, Name, Mod),
    Spec3 = integer(Spec2, Name),
    default_type(Spec3).

default_type(nomatch) -> #{type => string};
default_type(Type) -> Type.

range(Name) ->
    case string:split(Name, "..") of
        %% 1..10 1..inf -inf..10
        [MinStr, MaxStr] ->
            Schema = #{type => number},
            Schema1 = add_integer_prop(Schema, minimum, MinStr),
            add_integer_prop(Schema1, maximum, MaxStr);
        _ ->
            nomatch
    end.

%% Module:Type
remote_module_type(nomatch, Name, Mod) ->
    case string:split(Name, ":") of
        [_Module, Type] -> typename_to_spec(Type, Mod);
        _ -> nomatch
    end;
remote_module_type(Spec, _Name, _Mod) ->
    Spec.

%% [string()] or [integer()] or [xxx].
typerefl_array(nomatch, Name, Mod) ->
    case string:trim(Name, leading, "[") of
        Name ->
            nomatch;
        Name1 ->
            case string:trim(Name1, trailing, "]") of
                Name1 ->
                    notmatch;
                Name2 ->
                    Schema = typename_to_spec(Name2, Mod),
                    #{type => array, items => Schema}
            end
    end;
typerefl_array(Spec, _Name, _Mod) ->
    Spec.

%% integer(1)
integer(nomatch, Name) ->
    case string:to_integer(Name) of
        {Int, []} -> #{type => enum, symbols => [Int], default => Int};
        _ -> nomatch
    end;
integer(Spec, _Name) ->
    Spec.

add_integer_prop(Schema, Key, Value) ->
    case string:to_integer(Value) of
        {error, no_integer} -> Schema;
        {Int, []} when Key =:= minimum -> Schema#{Key => Int};
        {Int, []} -> Schema#{Key => Int}
    end.

to_bin(List) when is_list(List) ->
    case io_lib:printable_list(List) of
        true -> unicode:characters_to_binary(List);
        false -> List
    end;
to_bin(Boolean) when is_boolean(Boolean) -> Boolean;
to_bin(Atom) when is_atom(Atom) -> atom_to_binary(Atom, utf8);
to_bin(X) ->
    X.
