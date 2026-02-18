# UNS Gate

This plugin enforces Unified Namespace topic structure at ACL check time.

## Plugin API

Base path: `/api/v5/plugin_api/emqx_uns_gate`

- `GET /status`
- `GET /stats`
- `GET /ui` (manual model editor UI, no frontend framework)
- `GET /model`
- `GET /models`
- `GET /models/:id`
- `POST /models` (create or update model; optional `activate`)
- `POST /models/:id/activate`
- `POST /validate/topic`

Status (JSON):
- `GET /api/v5/plugin_api/emqx_uns_gate/status`

Statistics (JSON):
- `GET /api/v5/plugin_api/emqx_uns_gate/stats`

## UNS Model Schema

This section defines the complete model JSON format accepted by UNS Gate.

### Top-Level Keys

- `id` (optional, string): model ID. If omitted, server assigns one.
- `name` (optional, string): model display name. Defaults to `id`.
- `variable_types` (optional, object): reusable variable constraints.
- `tree` (required, object): topic tree definition.
- `payload_types` (optional, object): reusable payload schemas.

### `variable_types`

Map of variable type name to constraint object.

Supported forms:
- String regex matcher:
  - `{"type":"string","pattern":"^...$"}`
- Enum matcher:
  - `{"type":"enum","values":["A","B","C"]}`

If a variable type is missing or invalid, matcher falls back to permissive `any`.

### `payload_types`

Map of payload schema name to schema object.

Supported validation subset:
- Primitive/object `type`: `object | string | integer | number | boolean`
- `required` (object only)
- `properties` (object only)
- `additionalProperties` (object only, `false` supported)
- `enum` (string only)

Endpoint payload binding:
- Endpoint `_payload` can reference a key in `payload_types`, or `"any"` to skip payload validation.

### `tree`

`tree` is an object where each key is a root topic segment and each value is a node object.

Node object keys:
- `children` (optional, object): child segment map.
- `_payload` (optional, string): payload type name for endpoint node, default `"any"`.
- `_type` (optional, compatibility): explicit `namespace | variable | endpoint`.
- `_var_type` (optional, compatibility): variable type name.

Node type inference:
- If `children` exists: node is non-endpoint.
- If `children` is absent: node is endpoint.
- For non-endpoint keys:
  - key `{name}` => variable node
  - key `+` => variable wildcard node
  - any other key => namespace node

Variable type resolution:
- For key `{name}`:
  - use `_var_type` if provided
  - otherwise use inferred type name `name`
- For key `+`:
  - matcher is `any` (matches one segment)

Wildcard keys in tree:
- `+`: match exactly one topic segment.
- `#`: match remaining topic segments (including zero remaining segments).

### Full Example

```json
{
  "id": "model-v1",
  "name": "UNS Model V1",
  "variable_types": {
    "site_id": { "type": "string", "pattern": "^[A-Za-z][A-Za-z0-9_]{0,31}$" },
    "line_id": { "type": "string", "pattern": "^Line[0-9]{1,4}$" },
    "mode": { "type": "enum", "values": ["auto", "manual"] }
  },
  "payload_types": {
    "line_control": {
      "type": "object",
      "required": ["Status", "Mode"],
      "properties": {
        "Status": { "type": "string", "enum": ["running", "stopped"] },
        "Mode": { "type": "string", "enum": ["auto", "manual"] }
      },
      "additionalProperties": false
    }
  },
  "tree": {
    "default": {
      "children": {
        "{site_id}": {
          "children": {
            "Lines": {
              "children": {
                "{line_id}": {
                  "children": {
                    "LineControl": { "_payload": "line_control" }
                  }
                }
              }
            },
            "stream": {
              "children": {
                "#": { "_payload": "any" }
              }
            }
          }
        }
      }
    }
  }
}
```

## Enforcement Behavior

UNS Gate validates both topic structure and (optionally) payload schema.

- Topic violations (`topic_nomatch`, `topic_invalid`, `not_endpoint`):
  - `topic_nomatch`: no active model topic filter matched the topic.
    (No model-specific validation is run.)
  - `topic_invalid`: selected model filter matched, but topic failed selected
    model structure/segment constraints.
  - `not_endpoint`: selected model matched topic path, but target node is not an
    endpoint.
  - QoS 0: message is ignored.
  - QoS 1/2: publish is rejected and a protocol reason code is returned to client
    (`Not Authorized`).
  - If EMQX `authorization.deny_action` is set to `disconnect`, the client is
    disconnected on topic authorization failure (the setting is `disconnect`,
    not `drop`).
  - If `authorization.deny_action` is `ignore` (default), no disconnect is done;
    QoS 1/2 still receive reject reason codes.
  - Observable counters: `messages_dropped`, `topic_nomatch`,
    `topic_invalid`, `not_endpoint`, and per-model counters in `per_model`.

- Payload violations (`payload_invalid`):
  - Message is dropped by UNS Gate in publish processing.
  - No auth reject/disconnect is required for this path.
  - Observable counters: `messages_dropped`, `payload_invalid`,
    and per-model counters in `per_model`.

## Topic-Filter Pre-Check

When multiple models are active, UNS Gate pre-screens models before full
validation:

- Each model is compiled into topic-filter patterns derived from its tree paths.
- Variable segments are converted to single-level wildcards (`+`).
  - Example: `foo/{bar}/x` becomes `foo/+/x`.
- Active models are ordered by model ID.
- UNS Gate selects the first model (by ID order) whose compiled filter matches
  the publish topic.
- Only that selected model is fully validated; UNS Gate does not continue to the
  next model.
- Models that fail this pre-check are skipped and do not contribute per-model
  drop counters.

This avoids unrelated active models inflating counters and keeps model behavior
deterministic. It also means overlapping topic trees across models should be
avoided.

## Counters

`GET /stats` returns cluster-aggregated counters.

Top-level counters:
- `messages_total`: total handled messages (`messages_allowed + messages_dropped`);
  exempt traffic is included.
- `messages_allowed`: allowed messages plus exempt messages.
- `messages_dropped`: dropped/rejected messages due to UNS validation failures.
- `topic_nomatch`: dropped/rejected because no active model filter matched.
- `topic_invalid`: dropped/rejected due to selected-model topic mismatch.
- `not_endpoint`: dropped/rejected because topic matched a non-endpoint node.
- `payload_invalid`: dropped due to payload schema mismatch.
- `exempt`: messages skipped by `exempt_topics`.
- `per_model`: per-model breakdown map keyed by model ID.
- `recent_drops`: recent drop events (`topic`, `error_type`, `error_detail`,
  `timestamp_ms`).

Per-model counters (`per_model.<model_id>`):
- `messages_total`
- `messages_allowed`
- `messages_dropped`
- `topic_invalid`
- `not_endpoint`
- `payload_invalid`

Counter semantics:
- `record_allowed` bumps `messages_total` and `messages_allowed` for the matched model.
- Topic/payload drops bump `messages_total`, `messages_dropped`, and the specific
  reason counter for the selected model.
- If no model passes the topic-filter pre-check, `topic_nomatch` is bumped
  globally and no per-model drop counter is bumped.
