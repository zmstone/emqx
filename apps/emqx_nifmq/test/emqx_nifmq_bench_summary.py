#!/usr/bin/env python3
"""Summarize emqx_nifmq_bench results.csv: median over repetitions, per-delivery figures.

Usage: emqx_nifmq_bench_summary.py results.csv
"""
import csv
import statistics
import sys
from collections import defaultdict

MODES = ["baseline", "nifmq_copy", "nifmq_view"]


def f(x):
    return float(x) if x not in ("", None) else 0.0


def load_csv(path):
    groups = defaultdict(list)
    for r in csv.DictReader(open(path)):
        groups[(r["scenario"], int(r["fanout"]), int(r["payload"]), r["mode"])].append(r)
    return groups


def med(rows, fn):
    return statistics.median(fn(r) for r in rows)


def derived(rows):
    d = {}
    d["n"] = len(rows)
    d["sched"] = med(rows, lambda r: f(r["sched_ns_per_delivery"]))
    d["cpu"] = med(rows, lambda r: f(r["cpu_ns_per_delivery"]))
    d["pub"] = med(rows, lambda r: f(r["pub_ns"]) / f(r["deliveries"]))
    d["tput"] = med(rows, lambda r: f(r["deliveries"]) / f(r["wall_us"]) * 1e6)
    d["p50"] = med(rows, lambda r: f(r["p50_us"]))
    d["p99"] = med(rows, lambda r: f(r["p99_us"]))
    d["p999"] = med(rows, lambda r: f(r["p999_us"]))
    d["batch"] = med(rows, lambda r: f(r["all_deliveries"]) / max(f(r["batches"]), 1))
    d["pub_sched"] = med(rows, lambda r: f(r["pub_sched_ns_per_delivery"]))
    d["drain_sched"] = med(rows, lambda r: f(r["drain_sched_ns_per_delivery"]))
    d["mem"] = med(rows, lambda r: f(r["mem_delta"]) / f(r["deliveries"]))
    d["qbytes"] = med(rows, lambda r: f(r["nif_queue_bytes"]) / f(r["deliveries"]))
    d["msgbytes"] = med(
        rows,
        lambda r: f(r["nif_msgs"])
        * (f(r["msg_words"]) * 8 + (f(r["payload"]) if f(r["payload"]) > 64 else 0))
        / f(r["deliveries"]),
    )
    d["ooo"] = sum(int(r["ooo"]) for r in rows)
    d["load"] = max(f(r["host_load1"]) for r in rows)
    return d


def rel(x, b):
    return f"{(x / b - 1) * 100:+.0f}%" if b else "n/a"


def table(groups, scenario, cols, title):
    keys = sorted({(fo, p) for (s, fo, p, m) in groups if s == scenario})
    if not keys:
        return
    print(f"\n### {title}\n")
    print("| fan-out | payload | mode | " + " | ".join(h for h, _ in cols) + " |")
    print("|---|---|---|" + "---|" * len(cols))
    for fo, p in keys:
        base = derived(groups[(scenario, fo, p, "baseline")])
        for m in MODES:
            rows = groups.get((scenario, fo, p, m))
            if not rows:
                continue
            d = derived(rows)
            cells = [fn(d, base) for _, fn in cols]
            print(f"| {fo} | {p} | {m} | " + " | ".join(cells) + " |")


def main():
    groups = load_csv(sys.argv[1])
    flow = [
        ("sched ns/deliv", lambda d, b: f"{d['sched']:.0f} ({rel(d['sched'], b['sched'])})"),
        ("OS CPU ns/deliv", lambda d, b: f"{d['cpu']:.0f}"),
        ("pub ns/deliv", lambda d, b: f"{d['pub']:.0f}"),
        ("msgs/wakeup", lambda d, b: f"{d['batch']:.2f}"),
        ("P50 µs", lambda d, b: f"{d['p50']:.0f}"),
        ("P99 µs", lambda d, b: f"{d['p99']:.0f}"),
        ("P99.9 µs", lambda d, b: f"{d['p999']:.0f}"),
    ]
    loaded = [
        ("deliv/s", lambda d, b: f"{d['tput'] / 1e6:.2f}M ({rel(d['tput'], b['tput'])})"),
        ("sched ns/deliv", lambda d, b: f"{d['sched']:.0f} ({rel(d['sched'], b['sched'])})"),
        ("pub ns/deliv", lambda d, b: f"{d['pub']:.0f}"),
        ("msgs/wakeup", lambda d, b: f"{d['batch']:.2f}"),
        ("P99 µs", lambda d, b: f"{d['p99']:.0f}"),
    ]
    backlog = [
        ("publish sched ns/deliv", lambda d, b: f"{d['pub_sched']:.0f}"),
        ("drain sched ns/deliv", lambda d, b: f"{d['drain_sched']:.0f} ({rel(d['drain_sched'], b['drain_sched'])})"),
        (
            "total sched ns/deliv",
            lambda d, b: f"{d['pub_sched'] + d['drain_sched']:.0f} "
            f"({rel(d['pub_sched'] + d['drain_sched'], b['pub_sched'] + b['drain_sched'])})",
        ),
        ("erlang:memory B/deliv", lambda d, b: f"{d['mem']:.0f}"),
        ("native queue B/deliv", lambda d, b: f"{d['qbytes']:.1f}"),
        ("stored msg B/deliv", lambda d, b: f"{d['msgbytes']:.1f}"),
    ]
    table(groups, "idle", flow, "idle: one message at a time (window 1)")
    table(groups, "paced", flow, "paced: open loop, 300k deliveries/s")
    table(groups, "paced_encode", flow, "paced_encode: paced, subscribers serialize MQTT PUBLISH")
    table(groups, "loaded", loaded, "loaded: closed loop, window 32 messages")
    table(groups, "loaded_encode", loaded, "loaded_encode: loaded, subscribers serialize MQTT PUBLISH")
    table(groups, "backlog", backlog, "backlog: publish into suspended subscribers, then drain")
    ooo = sum(derived(v)["ooo"] for v in groups.values())
    max_load = max(derived(v)["load"] for v in groups.values())
    print(f"\nOrder violations across all runs: {ooo}. Highest 1-min load at a run start: {max_load:.2f}.")


if __name__ == "__main__":
    main()
