%%--------------------------------------------------------------------
%% Copyright (c) 2026 EMQ Technologies Co., Ltd. All Rights Reserved.
%%--------------------------------------------------------------------

-ifndef(EMQX_CHAN_INFO_HRL).
-define(EMQX_CHAN_INFO_HRL, true).

%% The row of the channel info table, flat. `emqx_channel:chan_info/1` builds it from the
%% channel state, `emqx_cm:chan_info_to_map/1` rebuilds the nested info map for readers.
%% `clientinfo` stays a map: its key set depends on the authenticator.
-record(chan_info, {
    conn_state :: atom(),
    clientinfo :: emqx_types:clientinfo(),
    %% conninfo
    clientid :: emqx_types:clientid(),
    username :: emqx_types:username(),
    peername :: emqx_types:peername(),
    sockname :: emqx_types:peername(),
    socktype :: atom(),
    conn_mod :: module(),
    proto_name :: binary(),
    proto_ver :: non_neg_integer(),
    clean_start :: boolean(),
    keepalive :: non_neg_integer(),
    receive_maximum :: non_neg_integer(),
    expiry_interval :: non_neg_integer(),
    connected_at :: non_neg_integer(),
    disconnected_at = undefined :: undefined | non_neg_integer(),
    %% session; `session_impl = undefined` means no session
    session_impl = undefined :: undefined | module(),
    created_at :: undefined | non_neg_integer(),
    is_persistent :: undefined | boolean(),
    durable :: undefined | boolean(),
    subscriptions_max :: undefined | non_neg_integer() | infinity,
    awaiting_rel_max :: undefined | non_neg_integer() | infinity,
    inflight_max :: undefined | non_neg_integer() | infinity,
    %% implementation specific; `undefined` is not stored in the map
    mqueue_max = undefined :: undefined | non_neg_integer() | infinity,
    n_streams = undefined :: undefined | non_neg_integer(),
    %% spare, for fields added later
    extra = #{} :: map()
}).

-endif.
