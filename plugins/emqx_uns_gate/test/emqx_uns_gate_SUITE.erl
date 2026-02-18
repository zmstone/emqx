%%--------------------------------------------------------------------
%% Copyright (c) 2026 EMQ Technologies Co., Ltd. All Rights Reserved.
%%--------------------------------------------------------------------
-module(emqx_uns_gate_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() ->
    emqx_common_test_helpers:all(?MODULE).

init_per_suite(Config) ->
    Apps = emqx_cth_suite:start([mria], #{
        work_dir => emqx_cth_suite:work_dir(Config)
    }),
    [{apps, Apps} | Config].

end_per_suite(Config) ->
    case whereis(emqx_uns_gate_store) of
        undefined -> ok;
        Pid -> exit(Pid, shutdown)
    end,
    case whereis(emqx_uns_gate_metrics) of
        undefined -> ok;
        Pid2 -> exit(Pid2, shutdown)
    end,
    ok = emqx_cth_suite:stop(?config(apps, Config)),
    Config.

init_per_testcase(_Case, Config) ->
    case whereis(emqx_uns_gate_store) of
        undefined ->
            {ok, Pid} = emqx_uns_gate_store:start_link(),
            true = unlink(Pid),
            ok;
        _ ->
            ok
    end,
    case whereis(emqx_uns_gate_metrics) of
        undefined ->
            {ok, Pid2} = emqx_uns_gate_metrics:start_link(),
            true = unlink(Pid2),
            ok;
        _ ->
            ok
    end,
    ok = emqx_uns_gate_store:reset(),
    ok = emqx_uns_gate_metrics:reset(),
    ok = emqx_uns_gate_config:update(#{
        <<"enabled">> => true,
        <<"on_mismatch">> => <<"deny">>,
        <<"exempt_topics">> => [<<"$SYS/#">>]
    }),
    Config.

t_topic_validation(_Config) ->
    {ok, _} = emqx_uns_gate_store:put_model(sample_model(), true),
    ?assertEqual(
        {allow, <<"model-v1">>},
        emqx_uns_gate_store:validate_topic(
            <<"default/Plant1/BatchHouse/Furnaces/F1/Lines/Line1/LineControl">>
        )
    ),
    ?assertEqual(
        {deny, topic_invalid, [{<<"model-v1">>, topic_invalid}]},
        emqx_uns_gate_store:validate_topic(
            <<"default/Plant1/BatchHouse/Furnaces/F1/Lines/BadLine/LineControl">>
        )
    ),
    ?assertEqual(
        {deny, not_endpoint, [{<<"model-v1">>, not_endpoint}]},
        emqx_uns_gate_store:validate_topic(
            <<"default/Plant1/BatchHouse/Furnaces/F1/Lines/Line1">>
        )
    ),
    ?assertEqual(
        {deny, payload_invalid, [{<<"model-v1">>, payload_invalid}]},
        emqx_uns_gate_store:validate_message(
            <<"default/Plant1/BatchHouse/Furnaces/F1/Lines/Line1/LineControl">>,
            <<"{\"Status\":\"running\"}">>
        )
    ).

t_plugin_api_get_put_post(_Config) ->
    {ok, 200, _, #{id := <<"model-v1">>, active := true}} = emqx_uns_gate_api:handle(
        post, [<<"models">>], #{body => #{<<"activate">> => true, <<"model">> => sample_model()}}
    ),
    Model2 = sample_model_v2(),
    {ok, 200, _, #{id := <<"model-v2">>, active := false}} = emqx_uns_gate_api:handle(
        post, [<<"models">>], #{body => #{<<"activate">> => false, <<"model">> => Model2}}
    ),
    {ok, 200, _, #{data := Data}} = emqx_uns_gate_api:handle(get, [<<"models">>], #{}),
    ?assert(length(Data) >= 2),
    {ok, 200, _, #{id := <<"model-v2">>, active := true}} = emqx_uns_gate_api:handle(
        post, [<<"models">>, <<"model-v2">>, <<"activate">>], #{}
    ),
    {ok, 200, _, #{id := <<"model-v2">>, active := false}} = emqx_uns_gate_api:handle(
        post, [<<"models">>, <<"model-v2">>, <<"deactivate">>], #{}
    ),
    {ok, 200, _, #{id := <<"model-v2">>, active := true}} = emqx_uns_gate_api:handle(
        post, [<<"models">>, <<"model-v2">>, <<"activate">>], #{}
    ),
    Model3 = (sample_model())#{<<"id">> => <<"model-v3">>, <<"name">> => <<"UNS Model V3">>},
    {ok, 200, _, #{id := <<"model-v3">>, active := false}} = emqx_uns_gate_api:handle(
        post, [<<"models">>], #{body => #{<<"activate">> => false, <<"model">> => Model3}}
    ),
    {error, 400, _, #{code := <<"BAD_MODEL">>}} = emqx_uns_gate_api:handle(
        post, [<<"models">>, <<"model-v3">>, <<"activate">>], #{}
    ),
    {ok, 200, _, #{id := <<"model-v2">>, active := true}} = emqx_uns_gate_api:handle(
        get, [<<"model">>], #{}
    ),
    {ok, 200, #{<<"content-type">> := <<"text/html; charset=utf-8">>}, UiBody} =
        emqx_uns_gate_api:handle(get, [<<"ui">>], #{}),
    ?assertMatch(<<_/binary>>, UiBody),
    ?assertNotEqual(nomatch, binary:match(UiBody, <<"UNS Gate">>)),
    {ok, 200, _, #{result := #{valid := true}}} = emqx_uns_gate_api:handle(
        post,
        [<<"validate">>, <<"topic">>],
        #{
            body => #{
                <<"topic">> => <<"default/Plant1/BatchHouse/Furnaces/F1/Lines/Line1/LineControl">>
            }
        }
    ),
    {ok, 200, _, #{messages_total := _, messages_allowed := _, messages_dropped := _}} =
        emqx_uns_gate_api:handle(get, [<<"stats">>], #{}).

t_multi_active_models_and_per_model_stats(_Config) ->
    Model1 = sample_model(),
    Model2 = sample_model_v2(),
    {ok, _} = emqx_uns_gate_store:put_model(Model1, true),
    {ok, _} = emqx_uns_gate_store:put_model(Model2, true),
    {ok, Entries} = emqx_uns_gate_store:list_models(),
    ActiveIds = [maps:get(id, E) || E <- Entries, maps:get(active, E, false) =:= true],
    ?assert(lists:member(<<"model-v1">>, ActiveIds)),
    ?assert(lists:member(<<"model-v2">>, ActiveIds)),

    %% Topic matching model-v1 should be allowed by model-v1.
    ?assertEqual(
        {allow, <<"model-v1">>},
        emqx_uns_gate_store:validate_message(
            <<"default/Plant1/BatchHouse/Furnaces/F1/Lines/Line1/LineControl">>,
            <<"{\"Status\":\"running\",\"Mode\":\"auto\"}">>
        )
    ),
    %% Topic matching model-v2 should be allowed by model-v2.
    ?assertEqual(
        {allow, <<"model-v2">>},
        emqx_uns_gate_store:validate_message(
            <<"enterprise/Plant1/BatchHouse/Furnaces/F1/Lines/Line1/LineControl">>,
            <<"{\"Status\":\"running\",\"Mode\":\"auto\"}">>
        )
    ),

    %% Record allow/drop per model and assert stats snapshot shape.
    emqx_uns_gate_metrics:record_allowed(<<"model-v1">>),
    emqx_uns_gate_metrics:record_allowed(<<"model-v2">>),
    emqx_uns_gate_metrics:record_drop(
        <<"bad/topic">>,
        topic_invalid,
        topic_invalid
    ),
    emqx_uns_gate_metrics:record_drop_model(<<"model-v1">>, topic_invalid, topic_invalid),
    emqx_uns_gate_metrics:record_drop_model(<<"model-v2">>, topic_invalid, topic_invalid),
    timer:sleep(10),
    {ok, 200, _, #{per_model := PerModel}} = emqx_uns_gate_api:handle(get, [<<"stats">>], #{}),
    ?assert(maps:is_key(<<"model-v1">>, PerModel)),
    ?assert(maps:is_key(<<"model-v2">>, PerModel)),
    M1 = maps:get(<<"model-v1">>, PerModel),
    M2 = maps:get(<<"model-v2">>, PerModel),
    ?assertEqual(2, maps:get(messages_total, M1)),
    ?assertEqual(1, maps:get(messages_allowed, M1)),
    ?assertEqual(1, maps:get(messages_dropped, M1)),
    ?assertEqual(1, maps:get(topic_invalid, M1)),
    ?assertEqual(2, maps:get(messages_total, M2)),
    ?assertEqual(1, maps:get(messages_allowed, M2)),
    ?assertEqual(1, maps:get(messages_dropped, M2)),
    ?assertEqual(1, maps:get(topic_invalid, M2)).

t_models_ordered_by_id_for_ui_and_runtime(_Config) ->
    ModelA = (sample_model())#{<<"id">> => <<"a-model">>, <<"name">> => <<"Model A">>},
    ModelB = (sample_model_v2())#{<<"id">> => <<"b-model">>, <<"name">> => <<"Model B">>},
    %% Insert in reverse lexical order to verify deterministic ID ordering.
    {ok, _} = emqx_uns_gate_store:put_model(ModelA, true),
    {ok, _} = emqx_uns_gate_store:put_model(ModelB, true),

    {ok, Entries} = emqx_uns_gate_store:list_models(),
    Ids = [maps:get(id, E) || E <- Entries],
    ?assertEqual([<<"a-model">>, <<"b-model">>], Ids),

    %% Both models match; runtime should pick the lowest ID first.
    ?assertEqual(
        {allow, <<"a-model">>},
        emqx_uns_gate_store:validate_message(
            <<"default/Plant1/BatchHouse/Furnaces/F1/Lines/Line1/LineControl">>,
            <<"{\"Status\":\"running\",\"Mode\":\"auto\"}">>
        )
    ).

t_multi_active_deny_reason_priority_and_details(_Config) ->
    {ok, _} = emqx_uns_gate_store:put_model(sample_model(), true),
    {ok, _} = emqx_uns_gate_store:put_model(sample_model_v2(), true),
    {deny, payload_invalid, ModelReasons} = emqx_uns_gate_store:validate_message(
        <<"default/Plant1/BatchHouse/Furnaces/F1/Lines/Line1/LineControl">>,
        <<"{\"Status\":\"running\"}">>
    ),
    ?assertEqual(1, length(ModelReasons)),
    ?assertEqual([{<<"model-v1">>, payload_invalid}], ModelReasons).

t_screen_topic_filter_skips_unrelated_models(_Config) ->
    {ok, _} = emqx_uns_gate_store:put_model(sample_model(), true),
    {ok, _} = emqx_uns_gate_store:put_model(sample_model_v2(), true),
    {deny, topic_invalid, Reasons1} = emqx_uns_gate_store:validate_topic(
        <<"default/Plant1/BatchHouse/Furnaces/F1/Lines/BadLine/LineControl">>
    ),
    ?assertEqual([{<<"model-v1">>, topic_invalid}], Reasons1),
    {deny, topic_invalid, Reasons2} = emqx_uns_gate_store:validate_topic(
        <<"enterprise/Plant1/BatchHouse/Furnaces/F1/Lines/BadLine/LineControl">>
    ),
    ?assertEqual([{<<"model-v2">>, topic_invalid}], Reasons2).

t_first_screened_model_is_final_choice(_Config) ->
    {ok, _} = emqx_uns_gate_store:put_model(sample_model(), true),
    {error, {conflicting_topic_filter, _, <<"model-v1">>}} = emqx_uns_gate_store:put_model(
        sample_model_lenient_topic(), true
    ),
    %% Conflicting active model is rejected; existing active model still validates.
    {deny, topic_invalid, Reasons} = emqx_uns_gate_store:validate_topic(
        <<"default/Plant1/BatchHouse/Furnaces/F1/Lines/BadLine/LineControl">>
    ),
    ?assertEqual([{<<"model-v1">>, topic_invalid}], Reasons).

t_topic_nomatch_counter(_Config) ->
    {ok, _} = emqx_uns_gate_store:put_model(sample_model(), true),
    ?assertEqual(
        {deny, topic_nomatch, []},
        emqx_uns_gate_store:validate_topic(
            <<"enterprise/Plant1/BatchHouse/Furnaces/F1/Lines/Line1/LineControl">>
        )
    ),
    emqx_uns_gate_metrics:record_drop(
        <<"enterprise/Plant1/BatchHouse/Furnaces/F1/Lines/Line1/LineControl">>,
        topic_nomatch,
        topic_nomatch
    ),
    timer:sleep(10),
    {ok, 200, _, Stats} = emqx_uns_gate_api:handle(get, [<<"stats">>], #{}),
    ?assertEqual(1, maps:get(topic_nomatch, Stats)),
    ?assertEqual(1, maps:get(messages_dropped, Stats)),
    ?assertEqual(1, maps:get(messages_total, Stats)).

t_single_model_multi_branch_topic_filters(_Config) ->
    {ok, _} = emqx_uns_gate_store:put_model(sample_model_multi_branch(), true),
    %% Branch A
    ?assertEqual(
        {allow, <<"model-multi-branch">>},
        emqx_uns_gate_store:validate_topic(
            <<"default/Plant1/BatchHouse/Furnaces/F1/Lines/Line1/LineControl">>
        )
    ),
    %% Branch B
    ?assertEqual(
        {allow, <<"model-multi-branch">>},
        emqx_uns_gate_store:validate_topic(
            <<"default/Plant1/BatchHouse/Utilities/U1/Status">>
        )
    ),
    %% No branch match -> topic_nomatch
    ?assertEqual(
        {deny, topic_nomatch, []},
        emqx_uns_gate_store:validate_topic(
            <<"default/Plant1/BatchHouse/Unknown/X">>
        )
    ).

t_simplified_tree_without_type_and_var_type(_Config) ->
    {ok, _} = emqx_uns_gate_store:put_model(sample_model_simplified_tree(), true),
    ?assertEqual(
        {allow, <<"model-simple-tree">>},
        emqx_uns_gate_store:validate_topic(<<"default/Plant1/Status">>)
    ),
    ?assertEqual(
        {deny, topic_nomatch, []},
        emqx_uns_gate_store:validate_topic(<<"default/unknown/path">>)
    ).

t_tree_plus_and_hash_wildcards(_Config) ->
    {ok, _} = emqx_uns_gate_store:put_model(sample_model_with_wildcards(), true),
    ?assertEqual(
        {allow, <<"model-wildcards">>},
        emqx_uns_gate_store:validate_topic(<<"default/Plant1/Status">>)
    ),
    ?assertEqual(
        {allow, <<"model-wildcards">>},
        emqx_uns_gate_store:validate_topic(<<"default/stream">>)
    ),
    ?assertEqual(
        {allow, <<"model-wildcards">>},
        emqx_uns_gate_store:validate_topic(<<"default/stream/a/b/c">>)
    ).

t_per_model_stats_breakdown_counters(_Config) ->
    emqx_uns_gate_metrics:record_allowed(<<"model-v1">>),
    emqx_uns_gate_metrics:record_drop_model(<<"model-v1">>, payload_invalid, payload_invalid),
    emqx_uns_gate_metrics:record_drop_model(<<"model-v1">>, not_endpoint, not_endpoint),
    emqx_uns_gate_metrics:record_drop_model(<<"model-v2">>, topic_invalid, topic_invalid),
    timer:sleep(10),
    {ok, 200, _, #{per_model := PerModel}} = emqx_uns_gate_api:handle(get, [<<"stats">>], #{}),
    M1 = maps:get(<<"model-v1">>, PerModel),
    M2 = maps:get(<<"model-v2">>, PerModel),
    ?assertEqual(3, maps:get(messages_total, M1)),
    ?assertEqual(1, maps:get(messages_allowed, M1)),
    ?assertEqual(2, maps:get(messages_dropped, M1)),
    ?assertEqual(1, maps:get(payload_invalid, M1)),
    ?assertEqual(1, maps:get(not_endpoint, M1)),
    ?assertEqual(0, maps:get(topic_invalid, M1)),
    ?assertEqual(1, maps:get(messages_total, M2)),
    ?assertEqual(0, maps:get(messages_allowed, M2)),
    ?assertEqual(1, maps:get(messages_dropped, M2)),
    ?assertEqual(1, maps:get(topic_invalid, M2)).

t_delete_model_cleans_per_model_metrics(_Config) ->
    {ok, _} = emqx_uns_gate_store:put_model(sample_model(), true),
    emqx_uns_gate_metrics:record_allowed(<<"model-v1">>),
    emqx_uns_gate_metrics:record_drop_model(<<"model-v1">>, topic_invalid, topic_invalid),
    timer:sleep(10),
    {ok, 200, _, #{per_model := PerModel0}} = emqx_uns_gate_api:handle(get, [<<"stats">>], #{}),
    ?assert(maps:is_key(<<"model-v1">>, PerModel0)),
    ok = emqx_uns_gate_store:delete_model(<<"model-v1">>),
    timer:sleep(10),
    {ok, 200, _, #{per_model := PerModel1}} = emqx_uns_gate_api:handle(get, [<<"stats">>], #{}),
    ?assertEqual(false, maps:is_key(<<"model-v1">>, PerModel1)),
    {error, not_found} = emqx_uns_gate_store:get_model(<<"model-v1">>),
    {error, not_found} = emqx_uns_gate_store:active_model().

t_plugin_api_delete_model(_Config) ->
    {ok, 200, _, #{id := <<"model-v1">>, active := true}} = emqx_uns_gate_api:handle(
        post, [<<"models">>], #{body => #{<<"activate">> => true, <<"model">> => sample_model()}}
    ),
    {ok, 200, _, #{id := <<"model-v1">>, deleted := true}} = emqx_uns_gate_api:handle(
        delete, [<<"models">>, <<"model-v1">>], #{}
    ),
    {error, 404, _, _} = emqx_uns_gate_api:handle(get, [<<"models">>, <<"model-v1">>], #{}),
    {error, 404, _, _} = emqx_uns_gate_api:handle(delete, [<<"models">>, <<"model-v1">>], #{}).

t_message_payload_invalid_rejects_immediately(_Config) ->
    {ok, _} = emqx_uns_gate_store:put_model(sample_model(), true),
    {error, {conflicting_topic_filter, _, <<"model-v1">>}} = emqx_uns_gate_store:put_model(
        sample_model_lenient_payload(), true
    ),
    %% Conflicting active model is rejected, so model-v1 remains selected.
    ?assertEqual(
        {deny, payload_invalid, [{<<"model-v1">>, payload_invalid}]},
        emqx_uns_gate_store:validate_message(
            <<"default/Plant1/BatchHouse/Furnaces/F1/Lines/Line1/LineControl">>,
            <<"{\"Status\":\"running\"}">>
        )
    ),
    ?assertMatch(
        {deny, payload_invalid, _},
        emqx_uns_gate_store:validate_message(
            <<"default/Plant1/BatchHouse/Furnaces/F1/Lines/Line1/LineControl">>,
            <<"{\"Status\":\"running\"}">>
        )
    ).

sample_model() ->
    #{
        <<"id">> => <<"model-v1">>,
        <<"name">> => <<"UNS Model V1">>,
        <<"variable_types">> => #{
            <<"site_id">> => #{
                <<"type">> => <<"string">>,
                <<"pattern">> => <<"^[A-Za-z][A-Za-z0-9_]{0,31}$">>
            },
            <<"area_id">> => #{
                <<"type">> => <<"string">>,
                <<"pattern">> => <<"^[A-Za-z][A-Za-z0-9_]{0,31}$">>
            },
            <<"process_cell_id">> => #{
                <<"type">> => <<"string">>,
                <<"pattern">> => <<"^[A-Za-z][A-Za-z0-9_]{0,31}$">>
            },
            <<"line_id">> => #{
                <<"type">> => <<"string">>,
                <<"pattern">> => <<"^Line[0-9]{1,4}$">>
            }
        },
        <<"payload_types">> => #{
            <<"line_control">> => #{
                <<"type">> => <<"object">>,
                <<"required">> => [<<"Status">>, <<"Mode">>],
                <<"properties">> => #{
                    <<"Status">> => #{
                        <<"type">> => <<"string">>,
                        <<"enum">> => [<<"running">>, <<"stopped">>]
                    },
                    <<"Mode">> => #{
                        <<"type">> => <<"string">>,
                        <<"enum">> => [<<"auto">>, <<"manual">>]
                    }
                },
                <<"additionalProperties">> => false
            }
        },
        <<"tree">> => #{
            <<"default">> => #{
                <<"_type">> => <<"namespace">>,
                <<"children">> => #{
                    <<"{site_id}">> => #{
                        <<"_type">> => <<"variable">>,
                        <<"_var_type">> => <<"site_id">>,
                        <<"children">> => #{
                            <<"{area_id}">> => #{
                                <<"_type">> => <<"variable">>,
                                <<"_var_type">> => <<"area_id">>,
                                <<"children">> => #{
                                    <<"Furnaces">> => #{
                                        <<"_type">> => <<"namespace">>,
                                        <<"children">> => #{
                                            <<"{process_cell_id}">> => #{
                                                <<"_type">> => <<"variable">>,
                                                <<"_var_type">> => <<"process_cell_id">>,
                                                <<"children">> => #{
                                                    <<"Lines">> => #{
                                                        <<"_type">> => <<"namespace">>,
                                                        <<"children">> => #{
                                                            <<"{line_id}">> => #{
                                                                <<"_type">> => <<"variable">>,
                                                                <<"_var_type">> => <<"line_id">>,
                                                                <<"children">> => #{
                                                                    <<"LineControl">> => #{
                                                                        <<"_type">> =>
                                                                            <<"endpoint">>,
                                                                        <<"_payload">> =>
                                                                            <<"line_control">>
                                                                    }
                                                                }
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }.

sample_model_v2() ->
    Model = sample_model(),
    Tree1 = maps:get(<<"tree">>, Model),
    Root = maps:get(<<"default">>, Tree1),
    Tree2 = #{<<"enterprise">> => Root},
    Model#{
        <<"id">> => <<"model-v2">>,
        <<"name">> => <<"UNS Model V2">>,
        <<"tree">> => Tree2
    }.

sample_model_lenient_payload() ->
    Model = sample_model(),
    PayloadTypes0 = maps:get(<<"payload_types">>, Model),
    LineControl0 = maps:get(<<"line_control">>, PayloadTypes0),
    LineControl = LineControl0#{<<"required">> => [<<"Status">>]},
    PayloadTypes = PayloadTypes0#{<<"line_control">> => LineControl},
    Model#{
        <<"id">> => <<"model-v3">>,
        <<"name">> => <<"UNS Model V3 Lenient Payload">>,
        <<"payload_types">> => PayloadTypes
    }.

sample_model_lenient_topic() ->
    Model = sample_model(),
    VarTypes0 = maps:get(<<"variable_types">>, Model),
    LineType0 = maps:get(<<"line_id">>, VarTypes0),
    LineType = LineType0#{<<"pattern">> => <<"^[A-Za-z][A-Za-z0-9_]{0,31}$">>},
    VarTypes = VarTypes0#{<<"line_id">> => LineType},
    Model#{
        <<"id">> => <<"model-v9">>,
        <<"name">> => <<"UNS Model V9 Lenient Topic">>,
        <<"variable_types">> => VarTypes
    }.

sample_model_multi_branch() ->
    #{
        <<"id">> => <<"model-multi-branch">>,
        <<"name">> => <<"UNS Model Multi Branch">>,
        <<"variable_types">> => #{
            <<"site_id">> => #{
                <<"type">> => <<"string">>,
                <<"pattern">> => <<"^[A-Za-z][A-Za-z0-9_]{0,31}$">>
            },
            <<"area_id">> => #{
                <<"type">> => <<"string">>,
                <<"pattern">> => <<"^[A-Za-z][A-Za-z0-9_]{0,31}$">>
            },
            <<"process_cell_id">> => #{
                <<"type">> => <<"string">>,
                <<"pattern">> => <<"^[A-Za-z][A-Za-z0-9_]{0,31}$">>
            },
            <<"line_id">> => #{
                <<"type">> => <<"string">>,
                <<"pattern">> => <<"^Line[0-9]{1,4}$">>
            },
            <<"unit_id">> => #{
                <<"type">> => <<"string">>,
                <<"pattern">> => <<"^[A-Za-z][A-Za-z0-9_]{0,31}$">>
            }
        },
        <<"payload_types">> => #{
            <<"line_control">> => #{
                <<"type">> => <<"object">>,
                <<"required">> => [<<"Status">>, <<"Mode">>],
                <<"properties">> => #{
                    <<"Status">> => #{
                        <<"type">> => <<"string">>,
                        <<"enum">> => [<<"running">>, <<"stopped">>]
                    },
                    <<"Mode">> => #{
                        <<"type">> => <<"string">>,
                        <<"enum">> => [<<"auto">>, <<"manual">>]
                    }
                },
                <<"additionalProperties">> => false
            },
            <<"unit_status">> => #{
                <<"type">> => <<"object">>,
                <<"required">> => [<<"State">>],
                <<"properties">> => #{
                    <<"State">> => #{
                        <<"type">> => <<"string">>,
                        <<"enum">> => [<<"ok">>, <<"warn">>, <<"error">>]
                    }
                },
                <<"additionalProperties">> => false
            }
        },
        <<"tree">> => #{
            <<"default">> => #{
                <<"_type">> => <<"namespace">>,
                <<"children">> => #{
                    <<"{site_id}">> => #{
                        <<"_type">> => <<"variable">>,
                        <<"_var_type">> => <<"site_id">>,
                        <<"children">> => #{
                            <<"{area_id}">> => #{
                                <<"_type">> => <<"variable">>,
                                <<"_var_type">> => <<"area_id">>,
                                <<"children">> => #{
                                    <<"Furnaces">> => #{
                                        <<"_type">> => <<"namespace">>,
                                        <<"children">> => #{
                                            <<"{process_cell_id}">> => #{
                                                <<"_type">> => <<"variable">>,
                                                <<"_var_type">> => <<"process_cell_id">>,
                                                <<"children">> => #{
                                                    <<"Lines">> => #{
                                                        <<"_type">> => <<"namespace">>,
                                                        <<"children">> => #{
                                                            <<"{line_id}">> => #{
                                                                <<"_type">> => <<"variable">>,
                                                                <<"_var_type">> => <<"line_id">>,
                                                                <<"children">> => #{
                                                                    <<"LineControl">> => #{
                                                                        <<"_type">> =>
                                                                            <<"endpoint">>,
                                                                        <<"_payload">> =>
                                                                            <<"line_control">>
                                                                    }
                                                                }
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    },
                                    <<"Utilities">> => #{
                                        <<"_type">> => <<"namespace">>,
                                        <<"children">> => #{
                                            <<"{unit_id}">> => #{
                                                <<"_type">> => <<"variable">>,
                                                <<"_var_type">> => <<"unit_id">>,
                                                <<"children">> => #{
                                                    <<"Status">> => #{
                                                        <<"_type">> => <<"endpoint">>,
                                                        <<"_payload">> => <<"unit_status">>
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }.

sample_model_simplified_tree() ->
    #{
        <<"id">> => <<"model-simple-tree">>,
        <<"name">> => <<"UNS Model Simple Tree">>,
        <<"variable_types">> => #{
            <<"site_id">> => #{
                <<"type">> => <<"string">>,
                <<"pattern">> => <<"^[A-Za-z][A-Za-z0-9_]{0,31}$">>
            }
        },
        <<"tree">> => #{
            <<"default">> => #{
                <<"children">> => #{
                    <<"{site_id}">> => #{
                        <<"children">> => #{
                            <<"Status">> => #{
                                <<"_payload">> => <<"any">>
                            }
                        }
                    }
                }
            }
        }
    }.

sample_model_with_wildcards() ->
    #{
        <<"id">> => <<"model-wildcards">>,
        <<"name">> => <<"UNS Model Wildcards">>,
        <<"tree">> => #{
            <<"default">> => #{
                <<"children">> => #{
                    <<"+">> => #{
                        <<"children">> => #{
                            <<"Status">> => #{
                                <<"_payload">> => <<"any">>
                            }
                        }
                    },
                    <<"stream">> => #{
                        <<"children">> => #{
                            <<"#">> => #{
                                <<"_payload">> => <<"any">>
                            }
                        }
                    }
                }
            }
        }
    }.
