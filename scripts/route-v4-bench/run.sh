#!/bin/bash
## Benchmark v3 / #19558 / v4 route filter search. Run from the repo root after
## `make test-compile` (or a CT run) has built _build/emqx-enterprise-test.
## Usage: scripts/route-v4-bench/run.sh [mem|garbage|time|check] [Workload...]
set -euo pipefail
cd "$(git rev-parse --show-toplevel)"
OUT=$(mktemp -d)
P=_build/emqx-enterprise-test/lib/emqx/include
erlc -I "$P" -o "$OUT" apps/emqx/src/emqx_trie_search_v4.erl apps/emqx/src/emqx_topic_index_v4.erl \
    scripts/route-v4-bench/ts19558.erl scripts/route-v4-bench/v4bench.erl \
    scripts/route-v4-bench/v4check.erl scripts/route-v4-bench/prof.erl
MODE=${1:-time}
shift || true
if [ "$MODE" = check ]; then
    exec erl -noshell -pa _build/emqx-enterprise-test/lib/*/ebin -pa "$OUT" -run v4check run 3000
fi
## Wait for a quiet host: 1-minute load below 1.5.
for _ in $(seq 1 240); do
    awk -v l="$(cut -d' ' -f1 /proc/loadavg)" 'BEGIN{exit !(l < 1.5)}' && break
    sleep 15
done
uptime; free -g
erl +S 1:1 +sbt db -noshell -pa _build/emqx-enterprise-test/lib/*/ebin -pa "$OUT" -run v4bench run "$MODE" "$@"
uptime; free -g
