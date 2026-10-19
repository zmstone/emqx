#!/usr/bin/env bash
## Run the native delivery socket ceiling test: a broker node and a client node.
## Usage: emqx_nifmq_sockbench.sh OUT_DIR REPS MODES FANOUTS PAYLOADS QOSS RATES
## Example: emqx_nifmq_sockbench.sh tmp/sock 3 erlang,inline,threads2,threads4 1000,10000 16,4096 0,1 300000
## Build first: env PROFILE=emqx-enterprise-test ./mix compile
set -euo pipefail

OUT_DIR="$(mkdir -p "$1" && cd "$1" && pwd)"; shift
REPS="$1"; MODES="$2"; FANOUTS="$3"; PAYLOADS="$4"; QOSS="$5"; RATES="$6"
ROOT="$(cd "$(dirname "$0")/../../.." && pwd)"
LIB="$ROOT/_build/emqx-enterprise-test/lib"
## asdf selects the version by directory, so resolve erl from the repo root.
ERL="$(cd "$ROOT" && asdf which erl 2>/dev/null || command -v erl)"
ELIXIR_LIBS="$(cd "$ROOT" && elixir -e 'IO.puts(Path.dirname(:code.lib_dir(:elixir)))')"
BROKER="nifmq_sockb_$$@127.0.0.1"
CLIENT="nifmq_sockc_$$@127.0.0.1"
COOKIE="nifmq-sock-$$"
BROKER_CPUS="${BROKER_CPUS:-0-7}"
CLIENT_CPUS="${CLIENT_CPUS:-8-15}"
## The EMQX release sets these in vm.args.
VM_FLAGS=(+sbwt none +sbwtdcpu none +sbwtdio none +S 8:8 +Q 1048576 +P 2097152)

HOST_LOG="$OUT_DIR/host-state.log"
{
    echo "=== $(date -Is) broker=$BROKER client=$CLIENT args=$REPS $MODES $FANOUTS $PAYLOADS $QOSS $RATES"
    free -g
    uptime
    epmd -names || true
    ss -ltn | grep ':10' || true
} >> "$HOST_LOG"

export ERL_LIBS="$LIB:$ELIXIR_LIBS"
cd "$OUT_DIR"
taskset -c "$CLIENT_CPUS" "$ERL" -name "$CLIENT" -setcookie "$COOKIE" -noshell -noinput \
    "${VM_FLAGS[@]}" -kernel logger_level warning \
    -eval "emqx_nifmq_sockbench:client_main()" > "$OUT_DIR/client.log" 2>&1 &
CLIENT_PID=$!
trap 'kill $CLIENT_PID 2>/dev/null || true' EXIT

taskset -c "$BROKER_CPUS" "$ERL" -name "$BROKER" -setcookie "$COOKIE" -noshell -noinput \
    "${VM_FLAGS[@]}" -kernel logger_level warning \
    -eval "emqx_nifmq_sockbench:main([\"$OUT_DIR/results.csv\", \"$REPS\", \"$MODES\", \"$FANOUTS\", \"$PAYLOADS\", \"$QOSS\", \"$RATES\", \"$CLIENT\"])"
