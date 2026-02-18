%%--------------------------------------------------------------------
%% Copyright (c) 2026 EMQ Technologies Co., Ltd. All Rights Reserved.
%%--------------------------------------------------------------------
-module(emqx_uns_gate_api).

-export([handle/3]).

handle(get, [<<"status">>], _Request) ->
    {ok, 200, #{}, #{
        plugin => <<"emqx_uns_gate">>,
        enabled => emqx_uns_gate_config:enabled(),
        on_mismatch => emqx_uns_gate_config:on_mismatch(),
        exempt_topics => emqx_uns_gate_config:exempt_topics()
    }};
handle(get, [<<"stats">>], Request) ->
    _ = Request,
    Stats = emqx_uns_gate_metrics:snapshot(),
    {ok, 200, #{}, Stats};
handle(get, [<<"model">>], _Request) ->
    case emqx_uns_gate_store:active_model() of
        {ok, Entry} ->
            {ok, 200, #{}, Entry};
        {error, not_found} ->
            {error, 404, #{}, #{code => <<"NOT_FOUND">>, message => <<"No active model">>}}
    end;
handle(get, [<<"ui">>], _Request) ->
    {ok, 200,
        #{
            <<"content-type">> => <<"text/html; charset=utf-8">>,
            <<"cache-control">> => <<"no-store, no-cache, must-revalidate">>,
            <<"pragma">> => <<"no-cache">>,
            <<"expires">> => <<"0">>
        },
        ui_html()};
handle(get, [<<"models">>], _Request) ->
    {ok, Entries} = emqx_uns_gate_store:list_models(),
    {ok, 200, #{}, #{data => Entries}};
handle(get, [<<"models">>, Id], _Request) ->
    case emqx_uns_gate_store:get_model(Id) of
        {ok, Entry} ->
            {ok, 200, #{}, Entry};
        {error, not_found} ->
            {error, 404, #{}, #{code => <<"NOT_FOUND">>, message => <<"Model not found">>}}
    end;
handle(post, [<<"models">>], Request) ->
    Body = maps:get(body, Request, #{}),
    Activate = get_activate_flag(Body),
    Model = get_model_body(Body),
    case emqx_uns_gate_store:put_model(Model, Activate) of
        {ok, Entry} ->
            {ok, 200, #{}, Entry};
        {error, Reason} ->
            bad_model(Reason)
    end;
handle(post, [<<"models">>, Id, <<"activate">>], _Request) ->
    case emqx_uns_gate_store:activate(Id) of
        ok ->
            {ok, 200, #{}, #{id => Id, active => true}};
        {error, Reason} when Reason =/= not_found ->
            bad_model(Reason);
        {error, not_found} ->
            {error, 404, #{}, #{code => <<"NOT_FOUND">>, message => <<"Model not found">>}}
    end;
handle(post, [<<"models">>, Id, <<"deactivate">>], _Request) ->
    case emqx_uns_gate_store:deactivate(Id) of
        ok ->
            {ok, 200, #{}, #{id => Id, active => false}};
        {error, not_found} ->
            {error, 404, #{}, #{code => <<"NOT_FOUND">>, message => <<"Model not found">>}}
    end;
handle(delete, [<<"models">>, Id], _Request) ->
    case emqx_uns_gate_store:delete_model(Id) of
        ok ->
            {ok, 200, #{}, #{id => Id, deleted => true}};
        {error, not_found} ->
            {error, 404, #{}, #{code => <<"NOT_FOUND">>, message => <<"Model not found">>}}
    end;
handle(post, [<<"validate">>, <<"topic">>], Request) ->
    Topic = get_topic(maps:get(body, Request, #{})),
    case Topic of
        <<>> ->
            {error, 400, #{}, #{
                code => <<"BAD_REQUEST">>,
                message => <<"topic is required">>
            }};
        _ ->
            Result = emqx_uns_gate_store:validate_topic(Topic),
            {ok, 200, #{}, #{
                topic => Topic,
                result => format_validate_result(Result)
            }}
    end;
handle(_Method, _Path, _Request) ->
    {error, not_found}.

bad_model(Reason) ->
    {error, 400, #{}, #{
        code => <<"BAD_MODEL">>,
        message => iolist_to_binary(io_lib:format("~p", [Reason]))
    }}.

get_activate_flag(#{<<"activate">> := V}) ->
    normalize_bool(V);
get_activate_flag(_) ->
    false.

get_model_body(#{<<"model">> := Model}) when is_map(Model) ->
    Model;
get_model_body(Body) ->
    Body.

get_topic(#{<<"topic">> := Topic}) ->
    Topic;
get_topic(_) ->
    <<>>.

format_validate_result({allow, _ModelId}) ->
    #{valid => true};
format_validate_result({deny, Reason, _ModelReasons}) ->
    #{valid => false, reason => Reason}.

normalize_bool(true) -> true;
normalize_bool(_) -> false.

ui_html() ->
    case code:priv_dir(emqx_uns_gate) of
        {error, _} ->
            ui_not_available();
        Dir when is_list(Dir) ->
            Path = filename:join(Dir, "ui.html"),
            case file:read_file(Path) of
                {ok, Bin} -> Bin;
                _ -> ui_not_available()
            end
    end.

ui_not_available() ->
    <<
        "<!doctype html><html><body><h1>UNS Gate UI unavailable</h1>"
        "<p>Missing priv/ui.html</p></body></html>"
    >>.
