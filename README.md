# amoebum

This repo contains an experimental Common Lisp terminal UI kernel ("PTUI") being built against `PTUI_KERNEL_SPEC.md`.

Licensing: see `LICENSE`.

## PTUI

See `ptui/README.md` for system/module layout and how to load/run.

## Installation

### Prerequisites

From a fresh Linux/macOS environment, install:

1. `sbcl`
2. `make`
3. `quicklisp` (or provide `QUICKLISP_SETUP`)
4. `yarli` (required for `./bin/amoebum` workflow)

### Build from source

From repo root:

1. `make test`
2. `make build`

The binary is produced at `dist/amoebum`.

### Optional local install

To install wrapper + runtime into `~/.local/bin`:

1. `./install.sh`

You can override install location with `INSTALL_PREFIX`, for example:

1. `INSTALL_PREFIX=/usr/local/bin ./install.sh`

## Quick Start

From repo root:

1. `make check`
2. `./bin/amoebum`

## Amoebum CLI

Use the single entrypoint from repo root:

1. `./bin/amoebum`

Behavior:

1. Runs `./bin/yarli-sanitize-continuation.sh`
2. Runs `yarli run --stream`
3. Rejects any subcommands/arguments

## Yarli Usage

Run from repo root:

1. `./bin/yarli-sanitize-continuation.sh`
2. `yarli run --stream`
3. If stream rendering is unavailable, Yarli falls back to headless mode and should continue emitting structured stderr progress.

Authority model:

1. `yarli.toml` is the execution authority for Yarli runtime behavior.
2. `PROMPT.md` is intent-only (objective/context), not an operator runbook.
3. `IMPLEMENTATION_PLAN.md` is tranche scope/state authority.

Run and task triage:

1. `yarli run status <run-id>`
2. `yarli run explain-exit <run-id>`
3. `yarli task list <run-id>`
4. `yarli task explain <task-id>`
5. `yarli task annotate ...` to persist blocker details against a task (see `yarli task --help` for exact args).
6. `./bin/yarli-remediate-run.sh <run-id>` to capture failure context in `.agent/remediation-<run-id>/` and dispatch a separate remediation run.

Audit inspection:

1. `yarli audit tail --lines 100`
2. Expect policy decisions plus command execution entries (command key, exit code, stderr excerpt, duration) when command auditing is enabled.

Post-run memory sync:

1. Run `./bin/yarli-postrun-memory-sync.sh --run-id <run-id>` after a finished run (`RunCompleted`, `RunFailed`, or `RunCancelled`).
2. Haake write path is controlled by `yarli.toml` (`[memory.haake] enabled`, `command`, `project_dir`).
3. If Haake is disabled or unavailable, entries are appended to `.agent/memory-log.md`.

## Local Build and Check Commands

1. `make test-ptui` runs PTUI tests through ASDF.
2. `make test-amoebum` runs Amoebum tests through ASDF.
3. `make test` runs `test-ptui` then `test-amoebum`.
4. `make check-dist-ignore` verifies `dist/` is ignored and still gitignored.
5. `make check` runs `make check-dist-ignore`, then `make test`, then `make build`.
6. `make build` uses `bin/build-binary.sh` and resolves `QUICKLISP_SETUP` with fallback.

Guard script:

- `./bin/check-dist-ignore.sh` checks `.gitignore` for `dist/` and validates `git check-ignore -q dist`.

Payload contract (`yarli_postrun_memory_v1`):

1. Required fields: `timestamp_utc`, `project_id`, `run_id`, `objective`, `outcome`, `run_state`.
2. Verification fields: `verification_failed_gates`, `verification_passed`, task summary counters.
3. Blocker/root-cause fields: `blocker_signatures`, `root_cause_task_id`, `root_cause_reason`, `root_cause_error`.

Tagging convention:

1. Base tags: `yarli`, `postrun-observability`, `haake-postrun-memory-sync`.
2. Outcome/state tags: `outcome-<success|failed>`, `run-state-<normalized-run-state>`.

Deterioration pattern tracking:

1. Run `./bin/yarli-deterioration-report.sh --window-runs <n>` to analyze recent run/task/audit signals and append a report entry to `.agent/deterioration-report.md`.
2. Report format is `YARLI_DETERIORATION_REPORT_V1` with window summary, thresholded classification, `Trend Comparison` deltas (`previous_*`, `delta_*`), top signals, and a per-run trend table.
3. Use `--dry-run` to inspect generated output without writing.
4. Use `--synthetic-profile <observe|retry|remediate|escalate> --assert-action <class>` for deterministic classification probes.

Alert/action classes:

1. `observe`: continue normal execution and monitor trends.
2. `retry`: retry transient failures before broader changes.
3. `remediate`: run targeted remediation (for example `./bin/yarli-remediate-run.sh <run-id>`) before advancing.
4. `escalate`: pause auto-advance and require operator review.
