%%--------------------------------------------------------------------
%% Copyright (c) 2026 EMQ Technologies Co., Ltd. All Rights Reserved.
%%--------------------------------------------------------------------
-module(emqx_unsgov_store).

-moduledoc """
Persists UNS models, maintains runtime indexes, and executes validation.
""".

-behaviour(gen_server).

-export([
    create_tables/0,
    start_link/0,
    reset/0,
    active_model/0,
    list_models/0,
    get_model/1,
    put_model/2,
    delete_model/1,
    activate/1,
    deactivate/1,
    validate_topic/1,
    validate_message/2,
    apply_put_local/2,
    apply_put_local/3,
    apply_delete_local/1,
    apply_delete_local/2,
    apply_activate_local/2,
    apply_activate_local/3,
    apply_deactivate_local/1,
    apply_deactivate_local/2
]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-include("emqx_unsgov.hrl").
-define(TOPIC_INDEX_TAB, emqx_unsgov_topic_index).

create_tables() ->
    ok = mria:create_table(?MODEL_TAB, [
        {type, ordered_set},
        {rlog_shard, ?DB_SHARD},
        {storage, disc_copies},
        {attributes, record_info(fields, ?MODEL_TAB)}
    ]),
    ok = mria:create_table(?META_TAB, [
        {type, ordered_set},
        {rlog_shard, ?DB_SHARD},
        {storage, disc_copies},
        {attributes, record_info(fields, ?META_TAB)}
    ]),
    ok = mria:wait_for_tables([?MODEL_TAB, ?META_TAB]),
    ok.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

reset() ->
    gen_server:call(?MODULE, reset).

active_model() ->
    gen_server:call(?MODULE, active_model).

list_models() ->
    gen_server:call(?MODULE, list_models).

get_model(Id) ->
    gen_server:call(?MODULE, {get_model, to_bin(Id)}).

put_model(Model, Activate) ->
    gen_server:call(?MODULE, {put_model, Model, Activate}).

delete_model(Id) ->
    gen_server:call(?MODULE, {delete_model, to_bin(Id)}).

activate(Id) ->
    gen_server:call(?MODULE, {activate, to_bin(Id)}).

deactivate(Id) ->
    gen_server:call(?MODULE, {deactivate, to_bin(Id)}).

validate_topic(Topic) ->
    do_validate_topic(Topic).

validate_message(Topic, Payload) ->
    do_validate_message(Topic, Payload).

apply_put_local(Model, Activate) ->
    apply_put_local(Model, Activate, #{}).

apply_put_local(_Model, _Activate, #{kind := initiate}) ->
    ok;
apply_put_local(Model, Activate, _ClusterOpts) ->
    gen_server:call(?MODULE, {runtime_apply_put, Model, Activate}).

apply_delete_local(Id) ->
    apply_delete_local(Id, #{}).

apply_delete_local(_Id, #{kind := initiate}) ->
    ok;
apply_delete_local(Id, _ClusterOpts) ->
    gen_server:call(?MODULE, {runtime_apply_delete, to_bin(Id)}).

apply_activate_local(Id, Model) ->
    apply_activate_local(Id, Model, #{}).

apply_activate_local(_Id, _Model, #{kind := initiate}) ->
    ok;
apply_activate_local(Id, Model, _ClusterOpts) ->
    gen_server:call(?MODULE, {runtime_apply_activate, to_bin(Id), Model}).

apply_deactivate_local(Id) ->
    apply_deactivate_local(Id, #{}).

apply_deactivate_local(_Id, #{kind := initiate}) ->
    ok;
apply_deactivate_local(Id, _ClusterOpts) ->
    gen_server:call(?MODULE, {runtime_apply_deactivate, to_bin(Id)}).

init([]) ->
    ok = create_tables(),
    ensure_topic_index_table(),
    ok = load_bootstrap_models(),
    {ok, build_runtime_state(#{runtime_filter_owner => #{}})}.

handle_call(reset, _From, _State) ->
    _ = mria:clear_table(?MODEL_TAB),
    _ = mria:clear_table(?META_TAB),
    clear_all_compiled_models(),
    clear_topic_index_table(),
    {reply, ok, #{runtime_active_ids => [], runtime_filter_owner => #{}}};
handle_call(active_model, _From, State) ->
    case current_active_id() of
        undefined ->
            {reply, {error, not_found}, State};
        ActiveId ->
            case get_model_record(ActiveId) of
                {ok, Record} ->
                    {reply, {ok, format_entry(Record, true)}, State};
                error ->
                    {reply, {error, not_found}, State}
            end
    end;
handle_call(list_models, _From, State) ->
    ActiveIds = active_ids(),
    Entries = [
        format_entry(Record, lists:member(model_id(Record), ActiveIds))
     || Record <- list_model_records()
    ],
    Sorted = lists:sort(
        fun(#{id := A}, #{id := B}) -> A =< B end,
        Entries
    ),
    {reply, {ok, Sorted}, State};
handle_call({get_model, Id}, _From, State) ->
    ActiveIds = active_ids(),
    case get_model_record(Id) of
        {ok, Record} ->
            {reply, {ok, format_entry(Record, lists:member(Id, ActiveIds))}, State};
        error ->
            {reply, {error, not_found}, State}
    end;
handle_call({put_model, Model, Activate}, _From, State) ->
    case emqx_unsgov_model:compile(Model) of
        {ok, Compiled} ->
            Id = maps:get(id, Compiled),
            ActiveIds0 = active_ids(),
            WillBeActive = Activate orelse lists:member(Id, ActiveIds0),
            case maybe_validate_filter_conflict(Compiled, Id, WillBeActive, State) of
                ok ->
                    do_put_model(Compiled, Model, Activate, State);
                {error, _} = Error ->
                    {reply, Error, State}
            end;
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;
handle_call({activate, Id}, _From, State) ->
    case get_model_record(Id) of
        {ok, Record} ->
            Model = model_raw(Record),
            case emqx_unsgov_model:compile(Model) of
                {ok, Compiled} ->
                    case
                        check_filter_conflict(
                            Compiled, Id, maps:get(runtime_filter_owner, State, #{})
                        )
                    of
                        ok ->
                            ok = set_active_id(Id),
                            ok = set_active_ids(add_active_id(Id, active_ids())),
                            State1 = runtime_apply_activate(Id, Model, State),
                            _ = multicall_apply_activate(Id, Model),
                            {reply, ok, State1};
                        {error, _} = Error ->
                            {reply, Error, State}
                    end;
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end;
        error ->
            {reply, {error, not_found}, State}
    end;
handle_call({delete_model, Id}, _From, State0) ->
    do_delete_model(Id, State0);
handle_call({deactivate, Id}, _From, State) ->
    case get_model_record(Id) of
        {ok, _} ->
            ActiveIds0 = active_ids(),
            ActiveIds = remove_active_id(Id, ActiveIds0),
            ok = set_active_ids(ActiveIds),
            case current_active_id() of
                Id ->
                    NewCurrent =
                        case ActiveIds of
                            [First | _] -> First;
                            [] -> undefined
                        end,
                    ok = set_active_id(NewCurrent);
                _ ->
                    ok
            end,
            State1 = runtime_apply_deactivate(Id, State),
            _ = multicall_apply_deactivate(Id),
            {reply, ok, State1};
        error ->
            {reply, {error, not_found}, State}
    end;
handle_call({runtime_apply_put, Model, Activate}, _From, State0) ->
    State =
        case emqx_unsgov_model:compile(Model) of
            {ok, Compiled} -> runtime_apply_put_compiled(Compiled, Activate, State0);
            {error, _} -> State0
        end,
    {reply, ok, State};
handle_call({runtime_apply_delete, Id}, _From, State0) ->
    {reply, ok, runtime_apply_delete(Id, State0)};
handle_call({runtime_apply_activate, Id, Model}, _From, State0) ->
    {reply, ok, runtime_apply_activate(Id, Model, State0)};
handle_call({runtime_apply_deactivate, Id}, _From, State0) ->
    {reply, ok, runtime_apply_deactivate(Id, State0)};
handle_call({validate_topic, Topic}, _From, State0) ->
    {reply, do_validate_topic(Topic), State0};
handle_call({validate_message, Topic, Payload}, _From, State0) ->
    {reply, do_validate_message(Topic, Payload), State0};
handle_call(_Call, _From, State) ->
    {reply, {error, bad_request}, State}.

do_put_model(Compiled, Model, Activate, State) ->
    Id = maps:get(id, Compiled),
    Ts = erlang:system_time(millisecond),
    Record = #?MODEL_TAB{
        id = Id,
        model = maps:get(raw, Compiled),
        summary = emqx_unsgov_model:summary(Compiled),
        updated_at_ms = Ts
    },
    ok = mria:dirty_write(?MODEL_TAB, Record),
    ActiveIds0 = active_ids(),
    {ActiveIds, ActiveId} =
        case Activate of
            true ->
                ok = set_active_id(Id),
                ActiveIds1 = add_active_id(Id, ActiveIds0),
                ok = set_active_ids(ActiveIds1),
                {ActiveIds1, Id};
            false ->
                {ActiveIds0, current_active_id()}
        end,
    State1 = runtime_apply_put_compiled(Compiled, Activate, State),
    _ = multicall_apply_put(Model, Activate),
    {reply, {ok, format_entry(Record, lists:member(Id, ActiveIds) orelse ActiveId =:= Id)}, State1}.

do_delete_model(Id, State0) ->
    case get_model_record(Id) of
        {ok, _Record} ->
            ok = mria:dirty_delete(?MODEL_TAB, Id),
            ActiveIds0 = active_ids(),
            ActiveIds = remove_active_id(Id, ActiveIds0),
            ok = set_active_ids(ActiveIds),
            case current_active_id() of
                Id ->
                    NewCurrent =
                        case ActiveIds of
                            [First | _] -> First;
                            [] -> undefined
                        end,
                    ok = set_active_id(NewCurrent);
                _ ->
                    ok
            end,
            ok = emqx_unsgov_metrics:delete_model(Id),
            State1 = runtime_apply_delete(Id, State0),
            _ = multicall_apply_delete(Id),
            {reply, ok, State1};
        error ->
            {reply, {error, not_found}, State0}
    end.

handle_cast({runtime_apply_put, Model, Activate}, State0) ->
    State =
        case emqx_unsgov_model:compile(Model) of
            {ok, Compiled} -> runtime_apply_put_compiled(Compiled, Activate, State0);
            {error, _} -> State0
        end,
    {noreply, State};
handle_cast({runtime_apply_delete, Id}, State) ->
    {noreply, runtime_apply_delete(Id, State)};
handle_cast({runtime_apply_activate, Id, Model}, State0) ->
    {noreply, runtime_apply_activate(Id, Model, State0)};
handle_cast({runtime_apply_deactivate, Id}, State) ->
    {noreply, runtime_apply_deactivate(Id, State)};
handle_cast(_Cast, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

format_entry(Record, Active) ->
    #{
        id => model_id(Record),
        active => Active,
        updated_at_ms => model_updated_at_ms(Record),
        summary => model_summary(Record),
        model => model_raw(Record)
    }.

current_active_id() ->
    case ets:lookup(?META_TAB, ?META_KEY_ACTIVE_ID) of
        [#?META_TAB{value = Id}] -> Id;
        _ -> undefined
    end.

set_active_id(Id) ->
    mria:dirty_write(?META_TAB, #?META_TAB{key = ?META_KEY_ACTIVE_ID, value = Id}).

active_ids() ->
    case ets:lookup(?META_TAB, ?META_KEY_ACTIVE_IDS) of
        [#?META_TAB{value = Ids}] when is_list(Ids) ->
            sort_model_ids(lists:filter(fun(I) -> is_binary(I) end, Ids));
        _ ->
            case current_active_id() of
                undefined -> [];
                Id -> [Id]
            end
    end.

set_active_ids(Ids) ->
    mria:dirty_write(?META_TAB, #?META_TAB{key = ?META_KEY_ACTIVE_IDS, value = sort_model_ids(Ids)}).

add_active_id(Id, Ids) ->
    sort_model_ids([Id | [X || X <- Ids, X =/= Id]]).

remove_active_id(Id, Ids) ->
    sort_model_ids([X || X <- Ids, X =/= Id]).

sort_model_ids(Ids) ->
    lists:sort(fun(A, B) -> A =< B end, Ids).

list_model_records() ->
    ets:tab2list(?MODEL_TAB).

get_model_record(Id) ->
    case ets:lookup(?MODEL_TAB, Id) of
        [Record] -> {ok, Record};
        _ -> error
    end.

model_id(#?MODEL_TAB{id = Id}) ->
    Id.

model_raw(#?MODEL_TAB{model = Model}) ->
    Model.

model_summary(#?MODEL_TAB{summary = Summary}) ->
    Summary.

model_updated_at_ms(#?MODEL_TAB{updated_at_ms = Ts}) ->
    Ts.

do_validate_topic(Topic) ->
    with_selected_model(
        Topic,
        fun(Selected) ->
            do_validate_topic_selected(Selected, Topic)
        end
    ).

do_validate_topic_selected({Id, Compiled}, Topic) ->
    case emqx_unsgov_model:validate_topic(Compiled, Topic) of
        allow ->
            {allow, Id};
        {deny, Reason} ->
            {deny, Reason, [{Id, Reason}]}
    end.

do_validate_message(Topic, Payload) ->
    ValidatePayload = emqx_unsgov_config:validate_payload(),
    with_selected_model(
        Topic,
        fun(Selected) ->
            do_validate_message_selected(Selected, Topic, Payload, ValidatePayload)
        end
    ).

with_selected_model(Topic, OnSelected) ->
    case select_screened_model(Topic) of
        undefined ->
            {deny, topic_nomatch, []};
        Selected ->
            OnSelected(Selected)
    end.

do_validate_message_selected({Id, Compiled}, Topic, Payload, ValidatePayload) ->
    case emqx_unsgov_model:validate_message(Compiled, Topic, Payload, ValidatePayload) of
        allow ->
            {allow, Id};
        {deny, Reason} ->
            {deny, Reason, [{Id, Reason}]}
    end.

select_screened_model(Topic) ->
    Matches = topic_index_matches(Topic),
    Ids0 = [emqx_topic_index:get_id(M) || M <- Matches],
    Ids = lists:usort([Id || Id <- Ids0, is_binary(Id)]),
    find_first_compiled(Ids).

find_first_compiled([]) ->
    undefined;
find_first_compiled([Id | Rest]) ->
    case get_compiled_model(Id) of
        undefined -> find_first_compiled(Rest);
        Compiled -> {Id, Compiled}
    end.

to_bin(V) when is_binary(V) -> V;
to_bin(V) when is_atom(V) -> atom_to_binary(V, utf8);
to_bin(V) when is_list(V) -> unicode:characters_to_binary(V).

load_bootstrap_models() ->
    Files = bootstrap_model_files(),
    lists:foreach(fun load_bootstrap_model_file/1, Files),
    ok.

bootstrap_model_files() ->
    case code:priv_dir(emqx_unsgov) of
        {error, _} ->
            [];
        Dir when is_list(Dir) ->
            lists:sort(filelib:wildcard(filename:join(Dir, "bootstrap_models/*.json")))
    end.

load_bootstrap_model_file(Path) ->
    case file:read_file(Path) of
        {ok, Bin} ->
            try emqx_utils_json:decode(Bin, [return_maps]) of
                Model when is_map(Model) ->
                    ensure_bootstrap_model(Model, Path);
                _Other ->
                    ?LOG(error, #{
                        msg => "skip_bootstrap_model_invalid_json",
                        path => Path
                    })
            catch
                Class:Reason:Stacktrace ->
                    ?LOG(error, #{
                        msg => "skip_bootstrap_model_decode_failed",
                        path => Path,
                        kind => Class,
                        reason => Reason,
                        stacktrace => Stacktrace
                    })
            end;
        {error, Reason} ->
            ?LOG(error, #{
                msg => "skip_bootstrap_model_read_failed",
                path => Path,
                reason => Reason
            })
    end.

ensure_bootstrap_model(Model, Path) ->
    case emqx_unsgov_model:compile(Model) of
        {ok, Compiled} ->
            Id = maps:get(id, Compiled),
            case get_model_record(Id) of
                {ok, _} ->
                    ?LOG(info, #{
                        msg => "skip_bootstrap_model_already_exists",
                        path => Path,
                        model_id => Id
                    });
                error ->
                    Ts = erlang:system_time(millisecond),
                    Record = #?MODEL_TAB{
                        id = Id,
                        model = maps:get(raw, Compiled),
                        summary = emqx_unsgov_model:summary(Compiled),
                        updated_at_ms = Ts
                    },
                    ok = mria:dirty_write(?MODEL_TAB, Record),
                    ActiveIds = add_active_id(Id, active_ids()),
                    ok = set_active_ids(ActiveIds),
                    case current_active_id() of
                        undefined -> ok = set_active_id(Id);
                        _ -> ok
                    end,
                    ?LOG(info, #{
                        msg => "loaded_bootstrap_model",
                        path => Path,
                        model_id => Id
                    })
            end;
        {error, Reason} ->
            ?LOG(error, #{
                msg => "skip_bootstrap_model_compile_failed",
                path => Path,
                reason => Reason
            })
    end.

build_runtime_state(State0) ->
    clear_all_compiled_models(),
    clear_topic_index_table(),
    ActiveIds0 = active_ids(),
    Records0 = list_model_records(),
    Records = lists:sort(fun(A, B) -> model_id(A) =< model_id(B) end, Records0),
    {LoadedActiveIds, FilterOwner0} =
        lists:foldl(
            fun(Record, {ActiveAcc, OwnerAcc}) ->
                Id = model_id(Record),
                case emqx_unsgov_model:compile(model_raw(Record)) of
                    {ok, Compiled} ->
                        ok = put_compiled_model(Id, Compiled),
                        case lists:member(Id, ActiveIds0) of
                            true ->
                                case check_filter_conflict(Compiled, Id, OwnerAcc) of
                                    ok ->
                                        add_model_filters_to_index(Id, Compiled),
                                        {
                                            add_active_id(Id, ActiveAcc),
                                            add_filter_owner(Id, Compiled, OwnerAcc)
                                        };
                                    {error, #{cause := conflicting_topic_filter} = Reason} ->
                                        ?LOG(error, #{
                                            msg =>
                                                "skip_active_model_on_boot_due_to_filter_conflict",
                                            model_id => Id,
                                            reason => Reason
                                        }),
                                        {ActiveAcc, OwnerAcc}
                                end;
                            false ->
                                {ActiveAcc, OwnerAcc}
                        end;
                    {error, Reason} ->
                        ?LOG(error, #{
                            msg => "skip_model_on_boot_compile_failed",
                            model_id => Id,
                            reason => Reason
                        }),
                        {ActiveAcc, OwnerAcc}
                end
            end,
            {[], maps:get(runtime_filter_owner, State0, #{})},
            Records
        ),
    State0#{
        runtime_active_ids => sort_model_ids(LoadedActiveIds),
        runtime_filter_owner => FilterOwner0
    }.

runtime_apply_put_compiled(Compiled, Activate, State0) ->
    Id = maps:get(id, Compiled),
    OldCompiled = get_compiled_model(Id),
    ok = put_compiled_model(Id, Compiled),
    RuntimeActiveIds0 = maps:get(runtime_active_ids, State0, []),
    FilterOwner0 = maps:get(runtime_filter_owner, State0, #{}),
    WasActive = lists:member(Id, RuntimeActiveIds0),
    RuntimeActiveIds =
        case Activate of
            true -> add_active_id(Id, RuntimeActiveIds0);
            false -> RuntimeActiveIds0
        end,
    IsActive = lists:member(Id, RuntimeActiveIds),
    FilterOwner1 =
        case WasActive andalso OldCompiled =/= undefined of
            true -> remove_filter_owner(Id, OldCompiled, FilterOwner0);
            false -> FilterOwner0
        end,
    case WasActive andalso OldCompiled =/= undefined of
        true -> delete_model_filters_from_index(Id, OldCompiled);
        false -> ok
    end,
    {RuntimeActiveIds1, FilterOwner2} =
        case IsActive of
            true ->
                case check_filter_conflict(Compiled, Id, FilterOwner1) of
                    ok ->
                        add_model_filters_to_index(Id, Compiled),
                        {RuntimeActiveIds, add_filter_owner(Id, Compiled, FilterOwner1)};
                    {error, #{cause := conflicting_topic_filter} = Reason} ->
                        ?LOG(error, #{
                            msg => "skip_runtime_apply_due_to_filter_conflict",
                            model_id => Id,
                            reason => Reason
                        }),
                        {remove_active_id(Id, RuntimeActiveIds), FilterOwner1}
                end;
            false ->
                {RuntimeActiveIds, FilterOwner1}
        end,
    State0#{runtime_active_ids => RuntimeActiveIds1, runtime_filter_owner => FilterOwner2}.

runtime_apply_delete(Id, State0) ->
    OldCompiled = get_compiled_model(Id),
    RuntimeActiveIds0 = maps:get(runtime_active_ids, State0, []),
    RuntimeActiveIds = remove_active_id(Id, RuntimeActiveIds0),
    FilterOwner0 = maps:get(runtime_filter_owner, State0, #{}),
    case OldCompiled of
        undefined -> ok;
        _ -> delete_model_filters_from_index(Id, OldCompiled)
    end,
    erase_compiled_model(Id),
    FilterOwner =
        case OldCompiled of
            undefined -> FilterOwner0;
            _ -> remove_filter_owner(Id, OldCompiled, FilterOwner0)
        end,
    State0#{runtime_active_ids => RuntimeActiveIds, runtime_filter_owner => FilterOwner}.

runtime_apply_activate(Id, Model, State0) ->
    case emqx_unsgov_model:compile(Model) of
        {ok, Compiled} ->
            runtime_apply_put_compiled(Compiled, true, State0);
        {error, _} ->
            RuntimeActiveIds0 = maps:get(runtime_active_ids, State0, []),
            RuntimeActiveIds = add_active_id(Id, RuntimeActiveIds0),
            State0#{runtime_active_ids => RuntimeActiveIds}
    end.

runtime_apply_deactivate(Id, State0) ->
    RuntimeActiveIds0 = maps:get(runtime_active_ids, State0, []),
    RuntimeActiveIds = remove_active_id(Id, RuntimeActiveIds0),
    FilterOwner0 = maps:get(runtime_filter_owner, State0, #{}),
    Compiled0 = get_compiled_model(Id),
    case Compiled0 of
        undefined -> ok;
        Compiled1 -> delete_model_filters_from_index(Id, Compiled1)
    end,
    FilterOwner =
        case Compiled0 of
            undefined -> FilterOwner0;
            Compiled2 -> remove_filter_owner(Id, Compiled2, FilterOwner0)
        end,
    State0#{runtime_active_ids => RuntimeActiveIds, runtime_filter_owner => FilterOwner}.

multicall_apply_put(Model, Activate) ->
    multicall_apply(apply_put_local, [Model, Activate]).

multicall_apply_delete(Id) ->
    multicall_apply(apply_delete_local, [Id]).

multicall_apply_activate(Id, Model) ->
    multicall_apply(apply_activate_local, [Id, Model]).

multicall_apply_deactivate(Id) ->
    multicall_apply(apply_deactivate_local, [Id]).

multicall_apply(F, A) ->
    try emqx_cluster_rpc:multicall(?MODULE, F, A, 1, 5000) of
        _ -> ok
    catch
        _:_ -> ok
    end.

compiled_model_pt_key(Id) ->
    {?MODULE, compiled_model, Id}.

compiled_model_ids_pt_key() ->
    {?MODULE, compiled_model_ids}.

get_compiled_model(Id) ->
    persistent_term:get(compiled_model_pt_key(Id), undefined).

put_compiled_model(Id, Compiled) ->
    persistent_term:put(compiled_model_pt_key(Id), Compiled),
    Ids0 = persistent_term:get(compiled_model_ids_pt_key(), []),
    persistent_term:put(compiled_model_ids_pt_key(), lists:usort([Id | Ids0])),
    ok.

erase_compiled_model(Id) ->
    persistent_term:erase(compiled_model_pt_key(Id)),
    Ids0 = persistent_term:get(compiled_model_ids_pt_key(), []),
    persistent_term:put(
        compiled_model_ids_pt_key(),
        [X || X <- Ids0, X =/= Id]
    ),
    ok.

clear_all_compiled_models() ->
    Ids = persistent_term:get(compiled_model_ids_pt_key(), []),
    lists:foreach(fun(Id) -> persistent_term:erase(compiled_model_pt_key(Id)) end, Ids),
    persistent_term:erase(compiled_model_ids_pt_key()),
    ok.

add_model_filters_to_index(Id, Compiled) ->
    Filters = maps:get(topic_filters, Compiled, []),
    lists:foreach(
        fun(FilterWords) ->
            emqx_topic_index:insert(filter_words_to_topic(FilterWords), Id, Id, ?TOPIC_INDEX_TAB)
        end,
        Filters
    ),
    ok.

delete_model_filters_from_index(Id, Compiled) ->
    Filters = maps:get(topic_filters, Compiled, []),
    lists:foreach(
        fun(FilterWords) ->
            emqx_topic_index:delete(filter_words_to_topic(FilterWords), Id, ?TOPIC_INDEX_TAB)
        end,
        Filters
    ),
    ok.

filter_words_to_topic(FilterWords) ->
    iolist_to_binary(lists:join(<<"/">>, FilterWords)).

maybe_validate_filter_conflict(_Compiled, _Id, false, _State) ->
    ok;
maybe_validate_filter_conflict(Compiled, Id, true, State) ->
    check_filter_conflict(Compiled, Id, maps:get(runtime_filter_owner, State, #{})).

check_filter_conflict(Compiled, SelfId, FilterOwner) ->
    Filters = maps:get(topic_filters, Compiled, []),
    lists:foldl(
        fun(FilterWords, Acc) ->
            case Acc of
                ok ->
                    Filter = filter_words_to_topic(FilterWords),
                    case maps:get(Filter, FilterOwner, undefined) of
                        undefined ->
                            ok;
                        SelfId ->
                            ok;
                        ExistingId ->
                            {error, #{
                                cause => conflicting_topic_filter,
                                screening_filter => Filter,
                                conflict_with => ExistingId
                            }}
                    end;
                Error ->
                    Error
            end
        end,
        ok,
        Filters
    ).

add_filter_owner(Id, Compiled, FilterOwner0) ->
    Filters = maps:get(topic_filters, Compiled, []),
    lists:foldl(
        fun(FilterWords, Acc) ->
            Acc#{filter_words_to_topic(FilterWords) => Id}
        end,
        FilterOwner0,
        Filters
    ).

remove_filter_owner(Id, Compiled, FilterOwner0) ->
    Filters = maps:get(topic_filters, Compiled, []),
    lists:foldl(
        fun(FilterWords, Acc) ->
            Filter = filter_words_to_topic(FilterWords),
            case maps:get(Filter, Acc, undefined) of
                Id -> maps:remove(Filter, Acc);
                _ -> Acc
            end
        end,
        FilterOwner0,
        Filters
    ).

topic_index_matches(Topic) ->
    try emqx_topic_index:matches(Topic, ?TOPIC_INDEX_TAB, [unique]) of
        Matches -> Matches
    catch
        error:badarg -> []
    end.

ensure_topic_index_table() ->
    case ets:info(?TOPIC_INDEX_TAB) of
        undefined ->
            _ = ets:new(?TOPIC_INDEX_TAB, [
                named_table,
                ordered_set,
                protected,
                {read_concurrency, true}
            ]),
            ok;
        _ ->
            ok
    end.

clear_topic_index_table() ->
    case ets:info(?TOPIC_INDEX_TAB) of
        undefined ->
            ok;
        _ ->
            true = ets:delete_all_objects(?TOPIC_INDEX_TAB),
            ok
    end.
