# PTUI Performance Benchmark Story

This document explains how to run the PTUI TUI performance regression test, what it
measures, what the baseline numbers are after the GC-thrashing fix, and how to compare
before/after a change.

## Background

Prior to the GC-thrashing fix (committed alongside this story), the TUI allocator
exhibited O(total-conversation-lines) growth per render frame during streaming.  Each
redraw re-created widgets and line-entry lists proportional to the full conversation
history rather than just the visible viewport, causing minor page-fault rates to grow
roughly linearly with the number of prompts sent.

The fix applied several optimisations in combination:

| Optimisation | Files changed | Impact |
|---|---|---|
| Virtual scroll in chat-panel (O(viewport) widget allocs instead of O(all-lines)) | `chat-panel.lisp` | Dominant — reduced widget allocs from ~1600 to ~40 per frame |
| Per-message entry cache in `%message-line-entries` | `chat.lisp` | Completed messages are never re-parsed |
| Tail pointers for O(1) append in streaming-markdown-renderer | `streaming.lisp` | Eliminated O(N) list scans on every token |
| Conditional `copy-list` in render-lines | `chat.lisp` | Only copies when cursor is visible with no pending work |
| `buffer-clear` uses `reset-cell` instead of `clone-cell` | `buffer.lisp` | Eliminates per-cell heap allocation on clear |
| 64 MB SBCL nursery (vs default 8 MB) | `main.lisp` | Reduces GC promotion pressure |

## Reproducible Baseline Numbers

Test conditions: 5 prompts, each triggering a 20-section lorem-ipsum demo response
(keyword `long`), sampled at 0.5s intervals.  Binary built from a clean
`make build`.  Measured on a commodity x86-64 Linux host.

| Metric | Before fix | After fix |
|---|---|---|
| Minor fault rate growth (prompt 1 → prompt 5) | ~6.10x | ~1.08x |
| Peak RSS growth across session | unbounded (linear drift) | bounded (flat after warmup) |
| Fault growth threshold (test pass criterion) | — | ≤ 3.0x |

The 1.08x figure means fault rate is essentially flat regardless of conversation
length — i.e. each additional prompt costs approximately the same allocation as the
first.

## Current 2026-04-05 Validation Snapshot

The first April 5, 2026 reruns drifted materially from the historical post-fix
baseline above, but the perf recovery wave repaired the regression. The current
green numbers are:

| Run | Fault growth | RSS delta | Verdict |
|---|---|---|---|
| `./bin/tui-perf-test.sh --prompts 3 --report` | 2.44x (prompt 1 → prompt 3, >=10 samples only, trimmed ±4 samples) | 39,092 kB | PASS |
| `./bin/tui-perf-test.sh --report` (default 5 prompts) | 2.15x (prompt 1 → prompt 5, >=10 samples only, trimmed ±4 samples) | 44,696 kB | PASS |

Both repaired runs stayed under the `3.0x` steady-state fault-growth gate,
completed each prompt via the status-bar completion signal in about `23.6s`,
and kept RSS within the documented threshold. Treat the earlier `6.40x` and
`6.31x` measurements as regression evidence only, not the current truth.

Local evidence:

1. `.yarli/evidence/INXT-207-tui-perf-3.log` (initial failing regression run)
2. `.yarli/evidence/INXT-208-tui-perf-default.log` (initial failing regression run)
3. `.yarli/evidence/INXT-222-tui-perf-3-repaired.log`
4. `.yarli/evidence/INXT-223-tui-perf-default-repaired.log`

## Running the Test

The perf test lives in `bin/tui-perf-test.sh` at the **repo root** (not inside
`ptui/`).  It requires a built binary and `tmux` on PATH.

```bash
# 1. Build
make build
# or: ./ptui/bin/build.sh

# 2. Run with defaults (5 prompts)
./bin/tui-perf-test.sh

# 3. Run with more prompts for a stronger signal
./bin/tui-perf-test.sh --prompts 10

# 4. Run with detailed /proc snapshots printed
./bin/tui-perf-test.sh --report

# 5. Pause for manual tmux attach (useful for debugging stuck runs)
./bin/tui-perf-test.sh --watch
```

### What the Script Does

1. Starts `dist/amoebum --demo` inside a detached tmux session (120x40 geometry).
2. Sends N prompts (default 5) containing the keyword `long`, which triggers the demo
   backend to stream a 20-section lorem-ipsum response.
3. Samples `/proc/<pid>/stat` and `/proc/<pid>/status` every 0.5 seconds during each
   response to collect minor fault counts and RSS.
4. Computes the minor-fault rate (faults/sec) for each prompt from the slope of the
   cumulative fault counter.
5. Emits a verdict JSON to `tmp/perf-test-<pid>/verdict.json`.

### Pass/Fail Criteria

| Check | Threshold | Rationale |
|---|---|---|
| Fault rate growth (prompt 1 → N) | ≤ 3.0x | 3x allows for GC warmup; O(N) growth hits this quickly |
| Total RSS growth | ≤ 100 MB | Hard cap on resident memory inflation |
| TUI responsiveness | ≥ 1 "Section" marker in final pane | Guards against a frozen/deadlocked TUI |

Prompts with fewer than 10 samples are excluded from the fault-rate comparison to
avoid noise from short windows (a single GC burst in 4 samples produces wildly
inflated rates).

## Comparing Before and After

To measure the impact of a change:

```bash
# Baseline: current HEAD
git stash
make build
./bin/tui-perf-test.sh --report 2>&1 | tee /tmp/perf-before.txt

# After: apply the change
git stash pop
make build
./bin/tui-perf-test.sh --report 2>&1 | tee /tmp/perf-after.txt

# Diff the key lines
grep -E 'Fault rate|RSS growth|verdict' /tmp/perf-before.txt /tmp/perf-after.txt
```

The verdict JSON files (`tmp/perf-test-*/verdict.json`) are structured for easy
scripted comparison:

```json
{
  "fault_growth_factor": 1.08,
  "rss_delta_kb": 12288,
  "passed": 3,
  "failed": 0,
  "verdict": "PASS"
}
```

## Artifacts

Each run writes to `tmp/perf-test-<pid>/`:

| File | Contents |
|---|---|
| `verdict.json` | Structured summary: growth factors, counts, PASS/FAIL |
| `samples-prompt-N.txt` | Raw `/proc` samples for prompt N (timestamp + rss_kb + minflt + majflt) |

Artifacts are not cleaned up automatically — remove the `tmp/perf-test-*/` directories
between runs to avoid accumulation.

## Continuous Integration

To gate a PR on performance:

```bash
./bin/tui-perf-test.sh --prompts 3
```

A 3-prompt run is faster (under 2 minutes on a developer machine) and still catches
O(N) allocation regressions with reasonable confidence.  Use `--prompts 10` for
pre-release verification.
