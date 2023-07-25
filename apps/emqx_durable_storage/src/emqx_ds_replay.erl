%%--------------------------------------------------------------------
%% Copyright (c) 2022-2023 EMQ Technologies Co., Ltd. All Rights Reserved.
%%--------------------------------------------------------------------
-module(emqx_ds_replay).

%% API:
-export([]).

-export_type([replay_id/0, replay/0]).

%%================================================================================
%% Type declarations
%%================================================================================

-type replay_id() :: binary().

-type replay() :: {
    _TopicFilter :: emqx_ds:words(),
    _StartTime :: emqx_ds:time()
}.

%%================================================================================
%% API funcions
%%================================================================================

%%================================================================================
%% behaviour callbacks
%%================================================================================

%%================================================================================
%% Internal exports
%%================================================================================

%%================================================================================
%% Internal functions
%%================================================================================
