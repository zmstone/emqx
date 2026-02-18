%%--------------------------------------------------------------------
%% Copyright (c) 2026 EMQ Technologies Co., Ltd. All Rights Reserved.
%%--------------------------------------------------------------------
-module(emqx_uns_gate_model_message).

-export([validate_message/4]).

validate_message(Compiled, Topic, Payload, ValidatePayload) ->
    case emqx_uns_gate_model_topic:walk_to_node(Compiled, Topic) of
        {ok, Node} ->
            case maps:get(kind, Node) of
                endpoint ->
                    case maybe_validate_payload(Node, Payload, ValidatePayload) of
                        ok -> allow;
                        {error, Reason} -> {deny, Reason}
                    end;
                _ ->
                    {deny, not_endpoint}
            end;
        {error, Reason} ->
            {deny, Reason}
    end.

maybe_validate_payload(_Node, _Payload, false) ->
    ok;
maybe_validate_payload(Node, Payload, true) ->
    emqx_uns_gate_model_schema:validate_payload(Node, Payload).
