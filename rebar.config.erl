-module('rebar.config').

-export([do/2]).

do(Dir, CONFIG) ->
    ok = assert_otp(),
    ok = warn_profile_env(),
    case iolist_to_binary(Dir) of
        <<".">> ->
            C1 = deps(CONFIG),
            Config = dialyzer(C1),
            maybe_dump(Config ++ [{overrides, overrides()}] ++ coveralls() ++ config());
        _ ->
            CONFIG
    end.

assert_otp() ->
    Oldest = 25,
    Latest = 26,
    OtpRelease = list_to_integer(erlang:system_info(otp_release)),
    case OtpRelease < Oldest orelse OtpRelease > Latest of
        true ->
            io:format(
                standard_error,
                "ERROR: Erlang/OTP version ~p found. min=~p, recommended=~p~n",
                [OtpRelease, Oldest, Latest]
            ),
            halt(1);
        false when OtpRelease =/= Latest ->
            io:format(
                "WARNING: Erlang/OTP version ~p found, recommended==~p~n",
                [OtpRelease, Latest]
            );
        false ->
            ok
    end.

bcrypt() ->
    {bcrypt, {git, "https://github.com/emqx/erlang-bcrypt.git", {tag, "0.6.1"}}}.

quicer() ->
    {quicer, {git, "https://github.com/emqx/quic.git", {tag, "0.0.308"}}}.

jq() ->
    {jq, {git, "https://github.com/emqx/jq", {tag, "v0.3.12"}}}.

deps(Config) ->
    {deps, OldDeps} = lists:keyfind(deps, 1, Config),
    MoreDeps =
        [bcrypt() || provide_bcrypt_dep()] ++
            [jq() || is_jq_supported()] ++
            [quicer() || is_quicer_supported()],
    lists:keystore(deps, 1, Config, {deps, OldDeps ++ MoreDeps}).

overrides() ->
    [
        {add, [{extra_src_dirs, [{"etc", [{recursive, true}]}]}]},
        {add, jesse, [{erl_opts, [nowarn_match_float_zero]}]}
    ] ++ snabbkaffe_overrides().

%% Temporary workaround for a rebar3 erl_opts duplication
%% bug. Ideally, we want to set this define globally
snabbkaffe_overrides() ->
    Apps = [snabbkaffe, ekka, mria, gen_rpc],
    [{add, App, [{erl_opts, [{d, snk_kind, msg}]}]} || App <- Apps].

config() ->
    [
        {cover_enabled, is_cover_enabled()},
        {profiles, profiles()},
        {plugins, plugins()}
    ].

is_cover_enabled() ->
    case os:getenv("ENABLE_COVER_COMPILE") of
        "1" -> true;
        "true" -> true;
        _ -> false
    end.

is_enterprise(ce) -> false;
is_enterprise(ee) -> true.

is_community_umbrella_app("apps/emqx_bridge_kafka") -> false;
is_community_umbrella_app("apps/emqx_bridge_confluent") -> false;
is_community_umbrella_app("apps/emqx_bridge_gcp_pubsub") -> false;
is_community_umbrella_app("apps/emqx_bridge_cassandra") -> false;
is_community_umbrella_app("apps/emqx_bridge_opents") -> false;
is_community_umbrella_app("apps/emqx_bridge_clickhouse") -> false;
is_community_umbrella_app("apps/emqx_bridge_dynamo") -> false;
is_community_umbrella_app("apps/emqx_bridge_greptimedb") -> false;
is_community_umbrella_app("apps/emqx_bridge_hstreamdb") -> false;
is_community_umbrella_app("apps/emqx_bridge_influxdb") -> false;
is_community_umbrella_app("apps/emqx_bridge_iotdb") -> false;
is_community_umbrella_app("apps/emqx_bridge_matrix") -> false;
is_community_umbrella_app("apps/emqx_bridge_mongodb") -> false;
is_community_umbrella_app("apps/emqx_bridge_mysql") -> false;
is_community_umbrella_app("apps/emqx_bridge_pgsql") -> false;
is_community_umbrella_app("apps/emqx_bridge_pulsar") -> false;
is_community_umbrella_app("apps/emqx_bridge_redis") -> false;
is_community_umbrella_app("apps/emqx_bridge_rocketmq") -> false;
is_community_umbrella_app("apps/emqx_bridge_tdengine") -> false;
is_community_umbrella_app("apps/emqx_bridge_timescale") -> false;
is_community_umbrella_app("apps/emqx_bridge_oracle") -> false;
is_community_umbrella_app("apps/emqx_bridge_sqlserver") -> false;
is_community_umbrella_app("apps/emqx_oracle") -> false;
is_community_umbrella_app("apps/emqx_bridge_rabbitmq") -> false;
is_community_umbrella_app("apps/emqx_ft") -> false;
is_community_umbrella_app("apps/emqx_s3") -> false;
is_community_umbrella_app("apps/emqx_schema_registry") -> false;
is_community_umbrella_app("apps/emqx_enterprise") -> false;
is_community_umbrella_app("apps/emqx_bridge_kinesis") -> false;
is_community_umbrella_app("apps/emqx_bridge_azure_event_hub") -> false;
is_community_umbrella_app("apps/emqx_gcp_device") -> false;
is_community_umbrella_app("apps/emqx_dashboard_rbac") -> false;
is_community_umbrella_app("apps/emqx_dashboard_sso") -> false;
is_community_umbrella_app("apps/emqx_audit") -> false;
is_community_umbrella_app("apps/emqx_gateway_gbt32960") -> false;
is_community_umbrella_app("apps/emqx_gateway_ocpp") -> false;
is_community_umbrella_app("apps/emqx_gateway_jt808") -> false;
is_community_umbrella_app("apps/emqx_bridge_syskeeper") -> false;
is_community_umbrella_app(_) -> true.

is_jq_supported() ->
    not (false =/= os:getenv("BUILD_WITHOUT_JQ") orelse
        is_win32()) orelse
        "1" == os:getenv("BUILD_WITH_JQ").

is_quicer_supported() ->
    not (false =/= os:getenv("BUILD_WITHOUT_QUIC") orelse
        is_macos() orelse
        is_win32() orelse is_centos_6()) orelse
        "1" == os:getenv("BUILD_WITH_QUIC").

is_rocksdb_supported() ->
    not (false =/= os:getenv("BUILD_WITHOUT_ROCKSDB") orelse
        is_raspbian()) orelse
        "1" == os:getenv("BUILD_WITH_ROCKSDB").

is_macos() ->
    {unix, darwin} =:= os:type().

is_centos_6() ->
    %% reason:
    %% glibc is too old
    case file:read_file("/etc/centos-release") of
        {ok, <<"CentOS release 6", _/binary>>} ->
            true;
        _ ->
            false
    end.

is_raspbian() ->
    case os_cmd("./scripts/get-distro.sh") of
        "raspbian" ++ _ ->
            true;
        _ ->
            false
    end.

is_win32() ->
    win32 =:= element(1, os:type()).

project_app_dirs() ->
    project_app_dirs(get_edition_from_profile_env()).

project_app_dirs(Edition) ->
    IsEnterprise = is_enterprise(Edition),
    UmbrellaApps = [
        Path
     || Path <- filelib:wildcard("apps/*"),
        is_community_umbrella_app(Path) orelse IsEnterprise
    ],
    UmbrellaApps.

plugins() ->
    [
        %{relup_helper, {git, "https://github.com/emqx/relup_helper", {tag, "2.1.0"}}},
        %% emqx main project does not require port-compiler
        %% pin at root level for deterministic
        {pc, "v1.14.0"}
    ] ++
        %% test plugins are concatenated to default profile plugins
        %% otherwise rebar3 test profile runs are super slow
        test_plugins().

test_plugins() ->
    [
        {rebar3_proper, "0.12.1"},
        {coveralls, {git, "https://github.com/emqx/coveralls-erl", {tag, "v2.2.0-emqx-1"}}}
    ].

test_deps() ->
    [
        {bbmustache, "1.10.0"},
        {meck, "0.9.2"},
        {proper, "1.4.0"},
        {er_coap_client, {git, "https://github.com/emqx/er_coap_client", {tag, "v1.0.5"}}},
        {erl_csv, "0.2.0"},
        {eministat, "0.10.1"}
    ].

common_compile_opts() ->
    common_compile_opts(get_edition_from_profile_env(), undefined).

common_compile_opts(Edition, Vsn) ->
    % always include debug_info
    [
        debug_info,
        {compile_info, [{emqx_vsn, Vsn} || Vsn /= undefined]},
        {d, 'EMQX_RELEASE_EDITION', Edition}
    ] ++
        [{d, 'EMQX_BENCHMARK'} || os:getenv("EMQX_BENCHMARK") =:= "1"] ++
        [{d, 'BUILD_WITHOUT_QUIC'} || not is_quicer_supported()].

warn_profile_env() ->
    case os:getenv("PROFILE") of
        false ->
            io:format(
                standard_error,
                "WARN: environment variable PROFILE is not set, using 'emqx-enterprise'~n",
                []
            );
        _ ->
            ok
    end.

%% this function is only used for test/check profiles
get_edition_from_profile_env() ->
    case os:getenv("PROFILE") of
        "emqx-enterprise" ++ _ ->
            ee;
        "emqx" ++ _ ->
            ce;
        false ->
            ee;
        V ->
            io:format(standard_error, "ERROR: bad_PROFILE ~p~n", [V]),
            exit(bad_PROFILE)
    end.

prod_compile_opts(Edition, Vsn) ->
    [
        compressed,
        deterministic,
        warnings_as_errors
        | common_compile_opts(Edition, Vsn)
    ].

prod_overrides() ->
    [{add, [{erl_opts, [deterministic]}]}].

profiles() ->
    case get_edition_from_profile_env() of
        ee ->
            profiles_ee();
        ce ->
            profiles_ce()
    end ++ profiles_dev().

profiles_ce() ->
    Vsn = get_vsn(emqx),
    [
        {'emqx', [
            {erl_opts, prod_compile_opts(ce, Vsn)},
            {relx, relx(Vsn, cloud, bin, ce)},
            {overrides, prod_overrides()},
            {project_app_dirs, project_app_dirs(ce)}
        ]},
        {'emqx-pkg', [
            {erl_opts, prod_compile_opts(ce, Vsn)},
            {relx, relx(Vsn, cloud, pkg, ce)},
            {overrides, prod_overrides()},
            {project_app_dirs, project_app_dirs(ce)}
        ]}
    ].

profiles_ee() ->
    Vsn = get_vsn('emqx-enterprise'),
    [
        {'emqx-enterprise', [
            {erl_opts, prod_compile_opts(ee, Vsn)},
            {relx, relx(Vsn, cloud, bin, ee)},
            {overrides, prod_overrides()},
            {project_app_dirs, project_app_dirs(ee)}
        ]},
        {'emqx-enterprise-pkg', [
            {erl_opts, prod_compile_opts(ee, Vsn)},
            {relx, relx(Vsn, cloud, pkg, ee)},
            {overrides, prod_overrides()},
            {project_app_dirs, project_app_dirs(ee)}
        ]}
    ].

%% EE has more files than CE, always test/check with EE options.
profiles_dev() ->
    [
        {check, [
            {erl_opts, common_compile_opts()},
            {project_app_dirs, project_app_dirs()}
        ]},
        {test, [
            {deps, test_deps()},
            {erl_opts, common_compile_opts() ++ erl_opts_i()},
            {extra_src_dirs, [{"test", [{recursive, true}]}]},
            {project_app_dirs, project_app_dirs()}
        ]}
    ].

%% RelType: cloud (full size)
%% PkgType: bin | pkg
%% Edition: ce (opensource) | ee (enterprise)
relx(Vsn, RelType, PkgType, Edition) ->
    [
        {include_src, false},
        {include_erts, true},
        {extended_start_script, false},
        {generate_start_script, false},
        {sys_config, false},
        {vm_args, false},
        {release, {emqx, Vsn}, relx_apps(RelType, Edition)},
        {overlay, relx_overlay(RelType, Edition)},
        {overlay_vars_values,
            build_info() ++
                [
                    {emqx_description, emqx_description(RelType, Edition)}
                    | overlay_vars(RelType, PkgType, Edition)
                ]}
    ].

%% Make a HOCON compatible format
build_info() ->
    Os = os_cmd("./scripts/get-distro.sh"),
    [
        {build_info_arch, erlang:system_info(system_architecture)},
        {build_info_wordsize, rebar_utils:wordsize()},
        {build_info_os, Os},
        {build_info_erlang, rebar_utils:otp_release()},
        {build_info_elixir, none},
        {build_info_relform, relform()}
    ].

relform() ->
    case os:getenv("EMQX_REL_FORM") of
        false -> "tgz";
        Other -> Other
    end.

emqx_description(cloud, ee) -> "EMQX Enterprise";
emqx_description(cloud, ce) -> "EMQX".

overlay_vars(cloud, PkgType, Edition) ->
    [
        {emqx_default_erlang_cookie, "emqxsecretcookie"}
    ] ++
        overlay_vars_pkg(PkgType) ++
        overlay_vars_edition(Edition).

overlay_vars_edition(ce) ->
    [
        {emqx_schema_mod, emqx_conf_schema},
        {emqx_configuration_doc,
            "https://www.emqx.io/docs/en/v5.0/configuration/configuration.html"},
        {is_enterprise, "no"}
    ];
overlay_vars_edition(ee) ->
    [
        {emqx_schema_mod, emqx_enterprise_schema},
        {emqx_configuration_doc,
            "https://docs.emqx.com/en/enterprise/v5.0/configuration/configuration.html"},
        {is_enterprise, "yes"}
    ].

%% vars per packaging type, bin(zip/tar.gz/docker) or pkg(rpm/deb)
overlay_vars_pkg(bin) ->
    [
        {platform_data_dir, "data"},
        {platform_etc_dir, "etc"},
        {platform_plugins_dir, "plugins"},
        {runner_bin_dir, "$RUNNER_ROOT_DIR/bin"},
        {emqx_etc_dir, "$RUNNER_ROOT_DIR/etc"},
        {runner_lib_dir, "$RUNNER_ROOT_DIR/lib"},
        {runner_log_dir, "$RUNNER_ROOT_DIR/log"},
        {runner_user, ""},
        {is_elixir, "no"}
    ];
overlay_vars_pkg(pkg) ->
    [
        {platform_data_dir, "/var/lib/emqx"},
        {platform_etc_dir, "/etc/emqx"},
        {platform_plugins_dir, "/var/lib/emqx/plugins"},
        {runner_bin_dir, "/usr/bin"},
        {emqx_etc_dir, "/etc/emqx"},
        {runner_lib_dir, "$RUNNER_ROOT_DIR/lib"},
        {runner_log_dir, "/var/log/emqx"},
        {runner_user, "emqx"},
        {is_elixir, "no"}
    ].

relx_apps(ReleaseType, Edition) ->
    {ok, [
        #{
            db_apps := DBApps,
            system_apps := SystemApps,
            common_business_apps := CommonBusinessApps,
            ee_business_apps := EEBusinessApps,
            ce_business_apps := CEBusinessApps
        }
    ]} = file:consult("apps/emqx_machine/priv/reboot_lists.eterm"),
    EditionSpecificApps =
        case Edition of
            ee -> EEBusinessApps;
            ce -> CEBusinessApps
        end,
    BusinessApps = CommonBusinessApps ++ EditionSpecificApps,
    Apps =
        (SystemApps ++
            %% EMQX starts the DB and the business applications:
            [{App, load} || App <- DBApps] ++
            [emqx_machine] ++
            [{App, load} || App <- BusinessApps]),
    lists:foldl(fun proplists:delete/2, Apps, excluded_apps(ReleaseType)).

excluded_apps(ReleaseType) ->
    OptionalApps = [
        {quicer, is_quicer_supported()},
        {bcrypt, provide_bcrypt_release(ReleaseType)},
        {jq, is_jq_supported()},
        {observer, is_app(observer)},
        {mnesia_rocksdb, is_rocksdb_supported()},
        {os_mon, provide_os_mon_release()}
    ],
    [App || {App, false} <- OptionalApps].

is_app(Name) ->
    case application:load(Name) of
        ok -> true;
        {error, {already_loaded, _}} -> true;
        _ -> false
    end.

relx_overlay(ReleaseType, Edition) ->
    [
        {mkdir, "log/"},
        {mkdir, "data/"},
        {mkdir, "plugins"},
        {mkdir, "data/mnesia"},
        {mkdir, "data/configs"},
        {mkdir, "data/patches"},
        {mkdir, "data/scripts"},
        {template, "rel/emqx_vars", "releases/emqx_vars"},
        {template, "rel/BUILD_INFO", "releases/{{release_version}}/BUILD_INFO"},
        {copy, "bin/emqx", "bin/emqx"},
        {copy, "bin/emqx_ctl", "bin/emqx_ctl"},
        {copy, "bin/emqx_cluster_rescue", "bin/emqx_cluster_rescue"},
        {copy, "bin/node_dump", "bin/node_dump"},
        {copy, "bin/install_upgrade.escript", "bin/install_upgrade.escript"},
        {copy, "bin/emqx", "bin/emqx-{{release_version}}"},
        {copy, "bin/emqx_ctl", "bin/emqx_ctl-{{release_version}}"},
        {copy, "bin/install_upgrade.escript", "bin/install_upgrade.escript-{{release_version}}"},
        {copy, "apps/emqx_gateway_lwm2m/lwm2m_xml", "etc/lwm2m_xml"},
        {copy, "apps/emqx_auth/etc/acl.conf", "etc/acl.conf"},
        {template, "bin/emqx.cmd", "bin/emqx.cmd"},
        {template, "bin/emqx_ctl.cmd", "bin/emqx_ctl.cmd"},
        {copy, "bin/nodetool", "bin/nodetool"},
        {copy, "bin/nodetool", "bin/nodetool-{{release_version}}"}
    ] ++ etc_overlay(ReleaseType, Edition).

etc_overlay(ReleaseType, Edition) ->
    Templates = emqx_etc_overlay(ReleaseType),
    [
        {mkdir, "etc/"},
        {copy, "{{base_dir}}/lib/emqx/etc/certs", "etc/"}
        | copy_examples(Edition)
    ] ++
        lists:map(
            fun
                ({From, To}) -> {template, From, To};
                (FromTo) -> {template, FromTo, FromTo}
            end,
            Templates
        ).

copy_examples(ce) ->
    [{copy, "rel/config/examples", "etc/"}];
copy_examples(ee) ->
    [
        {copy, "rel/config/examples", "etc/"},
        {copy, "rel/config/ee-examples/*", "etc/examples/"}
    ].

emqx_etc_overlay(ReleaseType) ->
    emqx_etc_overlay_per_rel(ReleaseType) ++
        emqx_etc_overlay().

emqx_etc_overlay_per_rel(cloud) ->
    [{"{{base_dir}}/lib/emqx/etc/vm.args.cloud", "etc/vm.args"}].

emqx_etc_overlay() ->
    [
        {"{{base_dir}}/lib/emqx/etc/ssl_dist.conf", "etc/ssl_dist.conf"},
        {"{{base_dir}}/lib/emqx_conf/etc/emqx.conf.all", "etc/emqx.conf"}
    ].

get_vsn(Profile) ->
    case os:getenv("PKG_VSN") of
        false ->
            os_cmd("pkg-vsn.sh " ++ atom_to_list(Profile));
        Vsn ->
            Vsn
    end.

%% to make it compatible to Linux and Windows,
%% we must use bash to execute the bash file
%% because "./" will not be recognized as an internal or external command
os_cmd(Cmd) ->
    Output = os:cmd("bash " ++ Cmd),
    re:replace(Output, "\n", "", [{return, list}]).

maybe_dump(Config) ->
    is_debug() andalso
        file:write_file("rebar.config.rendered", [io_lib:format("~p.\n", [I]) || I <- Config]),
    Config.

is_debug() -> is_debug("DEBUG") orelse is_debug("DIAGNOSTIC").

is_debug(VarName) ->
    case os:getenv(VarName) of
        false -> false;
        "" -> false;
        _ -> true
    end.

provide_bcrypt_dep() ->
    not is_win32().

provide_os_mon_release() ->
    not is_win32().

provide_bcrypt_release(ReleaseType) ->
    provide_bcrypt_dep() andalso ReleaseType =:= cloud.

erl_opts_i() ->
    [{i, "apps"}] ++
        [{i, Dir} || Dir <- filelib:wildcard(filename:join(["apps", "*", "include"]))].

dialyzer(Config) ->
    {dialyzer, OldDialyzerConfig} = lists:keyfind(dialyzer, 1, Config),

    AppsToAnalyse =
        case os:getenv("DIALYZER_ANALYSE_APP") of
            false ->
                [];
            Value ->
                [list_to_atom(App) || App <- string:tokens(Value, ",")]
        end,

    AppNames = app_names(),

    KnownApps = [Name || Name <- AppsToAnalyse, lists:member(Name, AppNames)],

    AppsToExclude = AppNames -- KnownApps,

    Extra =
        [os_mon, system_monitor, tools, covertool] ++
            [bcrypt || provide_bcrypt_dep()] ++
            [jq || is_jq_supported()] ++
            [quicer || is_quicer_supported()],
    NewDialyzerConfig =
        OldDialyzerConfig ++
            [{exclude_apps, AppsToExclude} || length(AppsToAnalyse) > 0] ++
            [{plt_extra_apps, Extra} || length(Extra) > 0],
    lists:keystore(
        dialyzer,
        1,
        Config,
        {dialyzer, NewDialyzerConfig}
    ).

coveralls() ->
    case {os:getenv("GITHUB_ACTIONS"), os:getenv("GITHUB_TOKEN")} of
        {"true", Token} when is_list(Token) ->
            Cfgs = [
                {coveralls_repo_token, Token},
                {coveralls_service_job_id, os:getenv("GITHUB_RUN_ID")},
                {coveralls_commit_sha, os:getenv("GITHUB_SHA")},
                {coveralls_coverdata, "_build/test/cover/*.coverdata"},
                {coveralls_service_name, "github"}
            ],
            case
                os:getenv("GITHUB_EVENT_NAME") =:= "pull_request" andalso
                    string:tokens(os:getenv("GITHUB_REF"), "/")
            of
                [_, "pull", PRNO, _] ->
                    [{coveralls_service_pull_request, PRNO} | Cfgs];
                _ ->
                    Cfgs
            end;
        _ ->
            []
    end.

app_names() -> list_dir("apps").

list_dir(Dir) ->
    case filelib:is_dir(Dir) of
        true ->
            {ok, Names} = file:list_dir(Dir),
            [list_to_atom(Name) || Name <- Names, filelib:is_dir(filename:join([Dir, Name]))];
        false ->
            []
    end.
