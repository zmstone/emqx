%%--------------------------------------------------------------------
%% Copyright (c) 2026 EMQ Technologies Co., Ltd. All Rights Reserved.
%%--------------------------------------------------------------------
-module(emqx_uns_gate_model_schema).

-export([compile_var_matcher/1, match_segment/2, validate_payload/2]).

compile_var_matcher(#{type := <<"enum">>, values := Values}) when is_list(Values) ->
    {enum, maps:from_list([{to_bin(V), true} || V <- Values])};
compile_var_matcher(#{type := <<"string">>, pattern := Pattern}) ->
    compile_regex_matcher(Pattern);
compile_var_matcher(_) ->
    any.

match_segment(any, _Segment) ->
    true;
match_segment({enum, ValuesSet}, Segment) ->
    maps:is_key(Segment, ValuesSet);
match_segment({regex, RE}, Segment) ->
    case re:run(Segment, RE, [{capture, none}]) of
        match -> true;
        _ -> false
    end.

validate_payload(Node, PayloadBin) ->
    case maps:get(payload, Node, <<"any">>) of
        <<"any">> ->
            ok;
        _ ->
            do_validate_payload(Node, PayloadBin)
    end.

do_validate_payload(Node, PayloadBin) ->
    Schema = maps:get(payload_schema, Node, #{}),
    try emqx_utils_json:decode(PayloadBin, [return_maps]) of
        PayloadMap ->
            case validate_schema(PayloadMap, Schema) of
                ok -> ok;
                {error, _} -> {error, payload_invalid}
            end
    catch
        _:_ ->
            {error, payload_invalid}
    end.

validate_schema(Value, Schema) when is_map(Schema) ->
    Type = read_any(Schema, [type], undefined),
    case Type of
        <<"object">> ->
            validate_object_schema(Value, Schema);
        <<"string">> ->
            validate_string_schema(Value, Schema);
        <<"integer">> ->
            validate_integer_schema(Value, Schema);
        <<"number">> ->
            validate_number_schema(Value, Schema);
        <<"boolean">> ->
            validate_boolean_schema(Value, Schema);
        _ ->
            ok
    end.

validate_object_schema(Value, Schema) when is_map(Value) ->
    Required0 = read_list(Schema, [required], []),
    Required = [to_bin(K) || K <- Required0],
    case lists:all(fun(K) -> maps:is_key(K, Value) end, Required) of
        false ->
            {error, missing_required};
        true ->
            Properties0 = read_map(Schema, [properties], #{}),
            Properties = normalize_nested_map_keys(Properties0),
            case validate_object_properties(Value, Properties) of
                ok ->
                    Additional = read_any(Schema, [additionalProperties], true),
                    case Additional of
                        false ->
                            Keys = maps:keys(Value),
                            Allowed = maps:keys(Properties),
                            case lists:all(fun(K) -> lists:member(K, Allowed) end, Keys) of
                                true -> ok;
                                false -> {error, additional_properties}
                            end;
                        _ ->
                            ok
                    end;
                {error, _} = Error ->
                    Error
            end
    end;
validate_object_schema(_Value, _Schema) ->
    {error, expected_object}.

validate_object_properties(Value, Properties) ->
    maps:fold(
        fun
            (Key, PropSchema0, ok) ->
                case maps:find(Key, Value) of
                    error ->
                        ok;
                    {ok, PropValue} ->
                        PropSchema = normalize_nested_map_keys(PropSchema0),
                        validate_schema(PropValue, PropSchema)
                end;
            (_Key, _PropSchema, Error) ->
                Error
        end,
        ok,
        Properties
    ).

validate_string_schema(Value, Schema) when is_binary(Value) ->
    case read_list(Schema, [enum], undefined) of
        undefined ->
            ok;
        EnumValues ->
            Enum = [to_bin(V) || V <- EnumValues],
            case lists:member(Value, Enum) of
                true -> ok;
                false -> {error, enum_mismatch}
            end
    end;
validate_string_schema(_Value, _Schema) ->
    {error, expected_string}.

validate_integer_schema(Value, _Schema) when is_integer(Value) ->
    ok;
validate_integer_schema(_Value, _Schema) ->
    {error, expected_integer}.

validate_number_schema(Value, _Schema) when is_number(Value) ->
    ok;
validate_number_schema(_Value, _Schema) ->
    {error, expected_number}.

validate_boolean_schema(true, _Schema) ->
    ok;
validate_boolean_schema(false, _Schema) ->
    ok;
validate_boolean_schema(_, _) ->
    {error, expected_boolean}.

compile_regex_matcher(Pattern0) ->
    Pattern = to_bin(Pattern0),
    case re:compile(Pattern) of
        {ok, RE} -> {regex, RE};
        _ -> any
    end.

normalize_nested_map_keys(Map) when is_map(Map) ->
    maps:fold(
        fun
            (<<"_type">>, V, Acc) -> Acc#{type => V};
            (<<"_var_type">>, V, Acc) -> Acc#{var_type => V};
            (<<"_payload">>, V, Acc) -> Acc#{payload => V};
            (<<"children">>, V, Acc) -> Acc#{children => V};
            (<<"type">>, V, Acc) -> Acc#{type => V};
            (<<"pattern">>, V, Acc) -> Acc#{pattern => V};
            (<<"values">>, V, Acc) -> Acc#{values => V};
            (<<"required">>, V, Acc) -> Acc#{required => V};
            (<<"properties">>, V, Acc) -> Acc#{properties => V};
            (<<"additionalProperties">>, V, Acc) -> Acc#{additionalProperties => V};
            (<<"enum">>, V, Acc) -> Acc#{enum => V};
            (K, V, Acc) when is_binary(K) -> Acc#{K => V}
        end,
        #{},
        Map
    );
normalize_nested_map_keys(V) ->
    V.

read_map(Map, [K | Ks], Default) ->
    case maps:find(K, Map) of
        {ok, V} when is_map(V) -> V;
        {ok, _} -> Default;
        error -> read_map(Map, Ks, Default)
    end;
read_map(_Map, [], Default) ->
    Default.

read_any(Map, [K | Ks], Default) ->
    case maps:find(K, Map) of
        {ok, V} -> V;
        error -> read_any(Map, Ks, Default)
    end;
read_any(_Map, [], Default) ->
    Default.

read_list(Map, [K | Ks], Default) ->
    case maps:find(K, Map) of
        {ok, V} when is_list(V) -> V;
        {ok, _} -> Default;
        error -> read_list(Map, Ks, Default)
    end;
read_list(_Map, [], Default) ->
    Default.

to_bin(V) when is_binary(V) -> V;
to_bin(V) when is_integer(V) -> integer_to_binary(V);
to_bin(V) -> iolist_to_binary(io_lib:format("~p", [V])).
