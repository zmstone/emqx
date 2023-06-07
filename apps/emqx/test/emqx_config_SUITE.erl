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

-module(emqx_config_SUITE).

-compile(export_all).
-compile(nowarn_export_all).
-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("snabbkaffe/include/snabbkaffe.hrl").

all() -> emqx_common_test_helpers:all(?MODULE).

init_per_suite(Config) ->
    emqx_common_test_helpers:boot_modules(all),
    emqx_common_test_helpers:start_apps([]),
    Config.

end_per_suite(_Config) ->
    emqx_common_test_helpers:stop_apps([]).

init_per_testcase(TestCase, Config) ->
    try
        ?MODULE:TestCase({init, Config})
    catch
        error:function_clause ->
            Config
    end.

end_per_testcase(TestCase, Config) ->
    try
        ?MODULE:TestCase({'end', Config})
    catch
        error:function_clause ->
            ok
    end.

t_fill_default_values(C) when is_list(C) ->
    Conf = #{
        <<"broker">> => #{
            <<"perf">> => #{},
            <<"route_batch_clean">> => false
        }
    },
    WithDefaults = emqx_config:fill_defaults(Conf),
    ?assertMatch(
        #{
            <<"broker">> :=
                #{
                    <<"enable_session_registry">> := true,
                    <<"perf">> :=
                        #{
                            <<"route_lock_type">> := key,
                            <<"trie_compaction">> := true
                        },
                    <<"route_batch_clean">> := false,
                    <<"session_locking_strategy">> := quorum,
                    <<"shared_subscription_strategy">> := round_robin
                }
        },
        WithDefaults
    ),
    %% ensure JSON compatible
    _ = emqx_utils_json:encode(WithDefaults),
    ok.

t_init_load(C) when is_list(C) ->
    ConfFile = "./test_emqx.conf",
    ok = file:write_file(ConfFile, <<"">>),
    ExpectRootNames = lists:sort(hocon_schema:root_names(emqx_schema)),
    emqx_config:erase_all(),
    {ok, DeprecatedFile} = application:get_env(emqx, cluster_override_conf_file),
    ?assertEqual(false, filelib:is_regular(DeprecatedFile), DeprecatedFile),
    %% Don't has deprecated file
    ok = emqx_config:init_load(emqx_schema, [ConfFile]),
    ?assertEqual(ExpectRootNames, lists:sort(emqx_config:get_root_names())),
    ?assertMatch({ok, #{raw_config := 256}}, emqx:update_config([mqtt, max_topic_levels], 256)),
    emqx_config:erase_all(),
    %% Has deprecated file
    ok = file:write_file(DeprecatedFile, <<"{}">>),
    ok = emqx_config:init_load(emqx_schema, [ConfFile]),
    ?assertEqual(ExpectRootNames, lists:sort(emqx_config:get_root_names())),
    ?assertMatch({ok, #{raw_config := 128}}, emqx:update_config([mqtt, max_topic_levels], 128)),
    ok = file:delete(DeprecatedFile).

t_unknown_root_keys(C) when is_list(C) ->
    ?check_trace(
        #{timetrap => 1000},
        begin
            ok = emqx_config:init_load(
                emqx_schema, <<"test_1 {}\n test_2 {sub = 100}\n listeners {}">>
            ),
            ?block_until(#{?snk_kind := unknown_config_keys})
        end,
        fun(Trace) ->
            ?assertMatch(
                [#{unknown_config_keys := "test_1,test_2"}],
                ?of_kind(unknown_config_keys, Trace)
            )
        end
    ),
    ok.

t_cluster_hocon_backup({init, C}) ->
    C;
t_cluster_hocon_backup({'end', _C}) ->
    File = "backup-test.hocon",
    Files = [File | filelib:wildcard(File ++ ".*.bak")],
    lists:foreach(fun file:delete/1, Files);
t_cluster_hocon_backup(C) when is_list(C) ->
    Write = fun(Path, Content) ->
        %% avoid name clash
        timer:sleep(1),
        emqx_config:backup_and_write(Path, Content)
    end,
    File = "backup-test.hocon",
    %% write 12 times, 10 backups should be kept
    %% the latest one is File itself without suffix
    %% the oldest one is expected to be deleted
    N = 12,
    Inputs = lists:seq(1, N),
    Backups = lists:seq(N - 10, N - 1),
    InputContents = [integer_to_binary(I) || I <- Inputs],
    BackupContents = [integer_to_binary(I) || I <- Backups],
    lists:foreach(
        fun(Content) ->
            Write(File, Content)
        end,
        InputContents
    ),
    LatestContent = integer_to_binary(N),
    ?assertEqual({ok, LatestContent}, file:read_file(File)),
    Re = "\\.[0-9]{4}\\.[0-9]{2}\\.[0-9]{2}\\.[0-9]{2}\\.[0-9]{2}\\.[0-9]{2}\\.[0-9]{3}\\.bak$",
    Files = filelib:wildcard(File ++ ".*.bak"),
    ?assert(lists:all(fun(F) -> re:run(F, Re) =/= nomatch end, Files)),
    %% keep only the latest 10
    ?assertEqual(10, length(Files)),
    FilesSorted = lists:zip(lists:sort(Files), BackupContents),
    lists:foreach(
        fun({BackupFile, ExpectedContent}) ->
            ?assertEqual({ok, ExpectedContent}, file:read_file(BackupFile))
        end,
        FilesSorted
    ),
    ok.

t_init_load_emqx_schema(Config) when is_list(Config) ->
    emqx_config:erase_all(),
    %% Given empty config file
    ConfFile = prepare_conf_file(?FUNCTION_NAME, <<"">>, Config),
    application:set_env(emqx, config_files, [ConfFile]),
    %% When load emqx_schema
    ?assertEqual(ok, emqx_config:init_load(emqx_schema)),
    %% Then default zone is injected with all global defaults
    Default = emqx_config:get([zones, default]),
    MQTT = emqx_config:get([mqtt]),
    Stats = emqx_config:get([stats]),
    FD = emqx_config:get([flapping_detect]),
    FS = emqx_config:get([force_shutdown]),
    CC = emqx_config:get([conn_congestion]),
    FG = emqx_config:get([force_gc]),
    OP = emqx_config:get([overload_protection]),
    ?assertMatch(
        #{
            mqtt := MQTT,
            stats := Stats,
            flapping_detect := FD,
            force_shutdown := FS,
            conn_congestion := CC,
            force_gc := FG,
            overload_protection := OP
        },
        Default
    ).

t_init_zones_load_emqx_schema_no_default_for_none_existing(Config) when is_list(Config) ->
    emqx_config:erase_all(),
    %% Given empty config file
    ConfFile = prepare_conf_file(?FUNCTION_NAME, <<"">>, Config),
    application:set_env(emqx, config_files, [ConfFile]),
    %% When emqx_schema is loaded
    ?assertEqual(ok, emqx_config:init_load(emqx_schema)),
    %% Then read for none existing zone should throw error
    ?assertError(
        {config_not_found, [zones, no_exists]},
        emqx_config:get([zones, no_exists])
    ).

t_init_zones_load_other_schema(Config) when is_list(Config) ->
    emqx_config:erase_all(),
    %% Given empty config file
    ConfFile = prepare_conf_file(?FUNCTION_NAME, <<"">>, Config),
    application:set_env(emqx, config_files, [ConfFile]),
    %% When load emqx_limiter_schema, not emqx_schema
    %% Then load should success
    ?assertEqual(ok, emqx_config:init_load(emqx_limiter_schema)),
    %% Then no zones is loaded.
    ?assertError(
        {config_not_found, [zones]},
        emqx_config:get([zones])
    ),
    %% Then no default zone is loaded.
    ?assertError(
        {config_not_found, [zones, default]},
        emqx_config:get([zones, default])
    ).

t_init_zones_with_user_defined_default_zone(Config) when is_list(Config) ->
    emqx_config:erase_all(),
    %% Given user defined config for default zone
    ConfFile = prepare_conf_file(
        ?FUNCTION_NAME, <<"zones.default.mqtt.max_topic_alias=1024">>, Config
    ),
    application:set_env(emqx, config_files, [ConfFile]),
    %% When schema is loaded
    ?assertEqual(ok, emqx_config:init_load(emqx_schema)),

    %% Then user defined value is set
    {MqttV, Others} = maps:take(mqtt, emqx_config:get([zones, default])),
    {ZGDMQTT, ExpectedOthers} = maps:take(mqtt, zone_global_defaults()),
    ?assertEqual(ZGDMQTT#{max_topic_alias := 1024}, MqttV),
    %% Then others are defaults
    ?assertEqual(ExpectedOthers, Others).

t_init_zones_with_user_defined_other_zone(Config) when is_list(Config) ->
    emqx_config:erase_all(),
    %% Given user defined config for default zone
    ConfFile = prepare_conf_file(
        ?FUNCTION_NAME, <<"zones.myzone.mqtt.max_topic_alias=1024">>, Config
    ),
    application:set_env(emqx, config_files, [ConfFile]),
    %% When schema is loaded
    ?assertEqual(ok, emqx_config:init_load(emqx_schema)),
    %% Then user defined value is set and others are defaults

    %% Then user defined value is set
    {MqttV, Others} = maps:take(mqtt, emqx_config:get([zones, myzone])),
    {ZGDMQTT, ExpectedOthers} = maps:take(mqtt, zone_global_defaults()),
    ?assertEqual(ZGDMQTT#{max_topic_alias := 1024}, MqttV),
    %% Then others are defaults
    ?assertEqual(ExpectedOthers, Others),
    %% Then default zone still have the defaults
    ?assertEqual(zone_global_defaults(), emqx_config:get([zones, default])).

t_init_zones_with_cust_root_mqtt(Config) when is_list(Config) ->
    emqx_config:erase_all(),
    %% Given config file with mqtt user overrides
    ConfFile = prepare_conf_file(?FUNCTION_NAME, <<"mqtt.retry_interval=10m">>, Config),
    application:set_env(emqx, config_files, [ConfFile]),
    %% When emqx_schema is loaded
    ?assertEqual(ok, emqx_config:init_load(emqx_schema)),
    %% Then the value is reflected as internal representation in default `zone'
    %% and other fields under mqtt are defaults.
    GDefaultMqtt = maps:get(mqtt, zone_global_defaults()),
    ?assertEqual(
        GDefaultMqtt#{retry_interval := 600000},
        emqx_config:get([zones, default, mqtt])
    ).

t_default_zone_is_updated_after_global_defaults_updated(Config) when is_list(Config) ->
    emqx_config:erase_all(),
    %% Given empty emqx conf
    ConfFile = prepare_conf_file(?FUNCTION_NAME, <<"">>, Config),
    application:set_env(emqx, config_files, [ConfFile]),
    ?assertEqual(ok, emqx_config:init_load(emqx_schema)),
    ?assertNotEqual(900000, emqx_config:get([zones, default, mqtt, retry_interval])),
    %% When emqx_schema is loaded
    emqx_config:put([mqtt, retry_interval], 900000),
    %% Then the value is reflected in default `zone' and other fields under mqtt are defaults.
    GDefaultMqtt = maps:get(mqtt, zone_global_defaults()),
    ?assertEqual(
        GDefaultMqtt#{retry_interval := 900000},
        emqx_config:get([zones, default, mqtt])
    ).

t_myzone_is_updated_after_global_defaults_updated(Config) when is_list(Config) ->
    emqx_config:erase_all(),
    %% Given emqx conf file with user override in myzone (none default zone)
    ConfFile = prepare_conf_file(?FUNCTION_NAME, <<"zones.myzone.mqtt.max_inflight=32">>, Config),
    application:set_env(emqx, config_files, [ConfFile]),
    ?assertEqual(ok, emqx_config:init_load(emqx_schema)),
    ?assertNotEqual(900000, emqx_config:get([zones, myzone, mqtt, retry_interval])),
    %% When update another value of global default
    emqx_config:put([mqtt, retry_interval], 900000),
    %% Then the value is reflected in myzone and the user defined value unchanged.
    GDefaultMqtt = maps:get(mqtt, zone_global_defaults()),
    ?assertEqual(
        GDefaultMqtt#{
            retry_interval := 900000,
            max_inflight := 32
        },
        emqx_config:get([zones, myzone, mqtt])
    ),
    %% Then the value is reflected in default zone as well.
    ?assertEqual(
        GDefaultMqtt#{retry_interval := 900000},
        emqx_config:get([zones, default, mqtt])
    ).

t_zone_no_user_defined_overrides(Config) when is_list(Config) ->
    emqx_config:erase_all(),
    %% Given emqx conf file with user specified myzone
    ConfFile = prepare_conf_file(
        ?FUNCTION_NAME, <<"zones.myzone.mqtt.retry_interval=10m">>, Config
    ),
    application:set_env(emqx, config_files, [ConfFile]),
    ?assertEqual(ok, emqx_config:init_load(emqx_schema)),
    ?assertEqual(600000, emqx_config:get([zones, myzone, mqtt, retry_interval])),
    %% When there is an update in global default
    emqx_config:put([mqtt, max_inflight], 2),
    %% Then the value is reflected in both default and myzone
    ?assertMatch(2, emqx_config:get([zones, default, mqtt, max_inflight])),
    ?assertMatch(2, emqx_config:get([zones, myzone, mqtt, max_inflight])),
    %% Then user defined value from config is not overwritten
    ?assertMatch(600000, emqx_config:get([zones, myzone, mqtt, retry_interval])).

t_zone_no_user_defined_overrides_internal_represent(Config) when is_list(Config) ->
    emqx_config:erase_all(),
    %% Given emqx conf file with user specified myzone
    ConfFile = prepare_conf_file(?FUNCTION_NAME, <<"zones.myzone.mqtt.max_inflight=1">>, Config),
    application:set_env(emqx, config_files, [ConfFile]),
    ?assertEqual(ok, emqx_config:init_load(emqx_schema)),
    ?assertEqual(1, emqx_config:get([zones, myzone, mqtt, max_inflight])),
    %% When there is an update in global default
    emqx_config:put([mqtt, max_inflight], 2),
    %% Then the value is reflected in default `zone' but not user-defined zone
    ?assertMatch(2, emqx_config:get([zones, default, mqtt, max_inflight])),
    ?assertMatch(1, emqx_config:get([zones, myzone, mqtt, max_inflight])).

t_update_global_defaults_no_updates_on_user_overrides(Config) when is_list(Config) ->
    emqx_config:erase_all(),
    %% Given default zone config in conf file.
    ConfFile = prepare_conf_file(?FUNCTION_NAME, <<"zones.default.mqtt.max_inflight=1">>, Config),
    application:set_env(emqx, config_files, [ConfFile]),
    ?assertEqual(ok, emqx_config:init_load(emqx_schema)),
    ?assertEqual(1, emqx_config:get([zones, default, mqtt, max_inflight])),
    %% When there is an update in global default
    emqx_config:put([mqtt, max_inflight], 20),
    %% Then the value is not reflected in default `zone'
    ?assertMatch(1, emqx_config:get([zones, default, mqtt, max_inflight])).

t_zone_update_with_new_zone(Config) when is_list(Config) ->
    emqx_config:erase_all(),
    %% Given loaded an empty conf file
    ConfFile = prepare_conf_file(?FUNCTION_NAME, <<"">>, Config),
    application:set_env(emqx, config_files, [ConfFile]),
    ?assertEqual(ok, emqx_config:init_load(emqx_schema)),
    %% When there is an update for creating new zone config
    ok = emqx_config:put([zones, myzone, mqtt, max_inflight], 2),
    %% Then the value is set and other roots are created with defaults.
    GDefaultMqtt = maps:get(mqtt, zone_global_defaults()),
    ?assertEqual(
        GDefaultMqtt#{max_inflight := 2},
        emqx_config:get([zones, myzone, mqtt])
    ).

t_init_zone_with_global_defaults(Config) when is_list(Config) ->
    %% Given uninitialized empty config
    emqx_config:erase_all(),
    Zones = #{myzone => #{mqtt => #{max_inflight => 3}}},
    %% when put zones with global default with emqx_config:put/1
    GlobalDefaults = zone_global_defaults(),
    AllConf = maps:put(zones, Zones, GlobalDefaults),
    %% Then put sucess
    ?assertEqual(ok, emqx_config:put(AllConf)),
    %% Then GlobalDefaults are set
    ?assertEqual(GlobalDefaults, maps:with(maps:keys(GlobalDefaults), emqx_config:get([]))),
    %% Then my zone and default zone are set
    {MqttV, Others} = maps:take(mqtt, emqx_config:get([zones, myzone])),
    {ZGDMQTT, ExpectedOthers} = maps:take(mqtt, GlobalDefaults),
    ?assertEqual(ZGDMQTT#{max_inflight := 3}, MqttV),
    %% Then others are defaults
    ?assertEqual(ExpectedOthers, Others).

%%%
%%% Helpers
%%%
prepare_conf_file(Name, Content, CTConfig) ->
    Filename = tc_conf_file(Name, CTConfig),
    filelib:ensure_dir(Filename),
    ok = file:write_file(Filename, Content),
    Filename.

tc_conf_file(TC, Config) ->
    DataDir = ?config(data_dir, Config),
    filename:join([DataDir, TC, 'emqx.conf']).

zone_global_defaults() ->
    #{
        conn_congestion =>
            #{enable_alarm => true, min_alarm_sustain_duration => 60000},
        flapping_detect =>
            #{ban_time => 300000, max_count => 15, window_time => disabled},
        force_gc =>
            #{bytes => 16777216, count => 16000, enable => true},
        force_shutdown =>
            #{
                enable => true,
                max_heap_size => 4194304,
                max_mailbox_size => 1000
            },
        mqtt =>
            #{
                await_rel_timeout => 300000,
                exclusive_subscription => false,
                idle_timeout => 15000,
                ignore_loop_deliver => false,
                keepalive_backoff => 0.75,
                keepalive_multiplier => 1.5,
                max_awaiting_rel => 100,
                max_clientid_len => 65535,
                max_inflight => 32,
                max_mqueue_len => 1000,
                max_packet_size => 1048576,
                max_qos_allowed => 2,
                max_subscriptions => infinity,
                max_topic_alias => 65535,
                max_topic_levels => 128,
                mqueue_default_priority => lowest,
                mqueue_priorities => disabled,
                mqueue_store_qos0 => true,
                peer_cert_as_clientid => disabled,
                peer_cert_as_username => disabled,
                response_information => [],
                retain_available => true,
                retry_interval => 30000,
                server_keepalive => disabled,
                session_expiry_interval => 7200000,
                shared_subscription => true,
                strict_mode => false,
                upgrade_qos => false,
                use_username_as_clientid => false,
                wildcard_subscription => true
            },
        overload_protection =>
            #{
                backoff_delay => 1,
                backoff_gc => false,
                backoff_hibernation => true,
                backoff_new_conn => true,
                enable => false
            },
        stats => #{enable => true}
    }.
