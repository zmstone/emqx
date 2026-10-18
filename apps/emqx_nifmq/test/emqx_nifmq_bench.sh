#!/usr/bin/env bash
## Run the NifMQ fan-out benchmark on a dedicated node.
## Usage: emqx_nifmq_bench.sh OUT_DIR REPS SCENARIOS MODES FANOUTS PAYLOADS
## Example: emqx_nifmq_bench.sh tmp/nifmq 3 idle,loaded,backlog baseline,nifmq_copy,nifmq_view 10,100,1000,10000 16,4096
## Build first: env PROFILE=emqx-enterprise-test ./mix compile
set -euo pipefail

OUT_DIR="$1"; REPS="$2"; SCENARIOS="$3"; MODES="$4"; FANOUTS="$5"; PAYLOADS="$6"
ROOT="$(cd "$(dirname "$0")/../../.." && pwd)"
LIB="$ROOT/_build/emqx-enterprise-test/lib"
ELIXIR_LIBS="$(cd "$ROOT" && elixir -e 'IO.puts(Path.dirname(:code.lib_dir(:elixir)))')"
NODE="nifmq_bench_$$@127.0.0.1"
SCHEDULERS="${SCHEDULERS:-8}"
## EMQX sets no busy-wait flags, so the default is the OTP default. SBWT=none disables it.
SBWT_FLAGS=()
if [ -n "${SBWT:-}" ]; then
    SBWT_FLAGS=(+sbwt "$SBWT" +sbwtdcpu "$SBWT" +sbwtdio "$SBWT")
fi

mkdir -p "$OUT_DIR"
HOST_LOG="$OUT_DIR/host-state.log"
{
    echo "=== $(date -Is) node=$NODE schedulers=$SCHEDULERS sbwt=${SBWT:-default} args=$*"
    free -g
    uptime
    epmd -names || true
    ss -ltn | grep ':10' || true
} >> "$HOST_LOG"

cd "$OUT_DIR"
ERL_LIBS="$LIB:$ELIXIR_LIBS" ERL_CRASH_DUMP="$OUT_DIR/erl_crash.dump" erl \
    -name "$NODE" -setcookie "nifmq-bench-$$" -noshell -noinput \
    +S "$SCHEDULERS:$SCHEDULERS" "${SBWT_FLAGS[@]}" \
    -kernel logger_level warning \
    -eval "emqx_nifmq_bench:main([\"$OUT_DIR/results.csv\", \"$REPS\", \"$SCENARIOS\", \"$MODES\", \"$FANOUTS\", \"$PAYLOADS\"])"
