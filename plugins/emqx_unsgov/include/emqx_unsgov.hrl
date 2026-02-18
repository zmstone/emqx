-ifndef(EMQX_UNS_GOV_HRL).
-define(EMQX_UNS_GOV_HRL, true).

-include_lib("emqx_plugin_helper/include/logger.hrl").

-define(DEFAULT_ENABLED, true).
-define(DEFAULT_ON_MISMATCH, deny).
-define(DEFAULT_VALIDATE_PAYLOAD, true).
-define(DEFAULT_EXEMPT_TOPICS, [<<"$SYS/#">>, <<"$share/#">>]).

-define(MODEL_TAB, emqx_unsgov_model).
-define(META_TAB, emqx_unsgov_meta).
-define(DB_SHARD, emqx_unsgov_shard).
-define(META_KEY_ACTIVE_ID, active_id).
-define(META_KEY_ACTIVE_IDS, active_ids).
%% Unified plugin logging macro with fixed tag/domain.
-define(LOG(Level, Data), ?SLOG(Level, maps:merge(#{tag => "UNS_GOV", domain => unsgov}, (Data)))).

-record(?MODEL_TAB, {
    id,
    model = #{},
    summary = #{},
    updated_at_ms = 0
}).

-record(?META_TAB, {
    key,
    value
}).

-endif.
