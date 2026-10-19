#!/usr/bin/env python3
"""Summarize emqx_nifmq_sockbench results.csv: median over repetitions per cell.

Usage: emqx_nifmq_sockbench_summary.py results.csv
"""
import csv
import statistics
import sys
from collections import defaultdict

MODES = ["erlang", "inline", "threads2", "threads4", "inline_nr", "threads2_nr", "threads4_nr"]


def load(path):
    groups = defaultdict(list)
    for r in csv.DictReader(open(path)):
        key = (int(r["qos"]), int(r["fanout"]), int(r["payload"]), int(r["rate"]), r["mode"])
        groups[key].append(r)
    return groups


def med(rows, col, default=None):
    if default is not None and col not in rows[0]:
        return default
    return statistics.median(float(r[col]) for r in rows)


def spread(rows, col):
    vals = [float(r[col]) for r in rows]
    m = statistics.median(vals)
    return (max(vals) - min(vals)) / m * 100 if m else 0.0


def main():
    groups = load(sys.argv[1])
    cells = sorted({k[:4] for k in groups})
    print("| QoS | fan-out | payload | rate | mode | CPU ns/deliv | vs erlang | sched ns/deliv "
          "| writes/deliv | wakeups/deliv | vcsw/deliv | achieved/s | P50 µs | P99 µs | missing | ooo | dropped | spread |")
    print("|" + "---|" * 18)
    for c in cells:
        base_rows = groups.get(c + ("erlang",))
        base = med(base_rows, "cpu_ns_per_delivery") if base_rows else None
        for m in MODES:
            rows = groups.get(c + (m,))
            if not rows:
                continue
            cpu = med(rows, "cpu_ns_per_delivery")
            ratio = f"{base / cpu:.2f}x" if base and m != "erlang" else ""
            print(
                f"| {c[0]} | {c[1]} | {c[2]} | {c[3]} | {m} | {cpu:.0f} | {ratio} "
                f"| {med(rows, 'sched_ns_per_delivery'):.0f} | {med(rows, 'syscw_per_delivery'):.2f} "
                f"| {med(rows, 'wakeups_per_delivery'):.2f} | {med(rows, 'vcsw_per_delivery', 0):.2f} "
                f"| {med(rows, 'achieved_rate'):.0f} "
                f"| {med(rows, 'p50_us'):.0f} | {med(rows, 'p99_us'):.0f} "
                f"| {sum(int(r['missing']) for r in rows)} | {sum(int(r['ooo']) for r in rows)} "
                f"| {sum(int(r['native_dropped']) for r in rows)} "
                f"| {spread(rows, 'cpu_ns_per_delivery'):.0f}% |"
            )
    loads = [float(r["host_load1"]) for rows in groups.values() for r in rows]
    print(f"\nRuns: {sum(len(v) for v in groups.values())}. Host 1-min load at cell start: "
          f"min {min(loads):.2f}, median {statistics.median(loads):.2f}, max {max(loads):.2f}.")


if __name__ == "__main__":
    main()
