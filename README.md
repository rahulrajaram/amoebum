# amoebum

This repo contains an experimental Common Lisp terminal UI kernel ("PTUI") being built against `PTUI_KERNEL_SPEC.md`.

Licensing: see `LICENSE`.

## PTUI

See `ptui/README.md` for system/module layout and how to load/run.

## Yarli Usage

Run from repo root:

1. `yarli run --stream`
2. If stream rendering is unavailable, Yarli falls back to headless mode and should continue emitting structured stderr progress.

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

Payload contract (`yarli_postrun_memory_v1`):

1. Required fields: `timestamp_utc`, `project_id`, `run_id`, `objective`, `outcome`, `run_state`.
2. Verification fields: `verification_failed_gates`, `verification_passed`, task summary counters.
3. Blocker/root-cause fields: `blocker_signatures`, `root_cause_task_id`, `root_cause_reason`, `root_cause_error`.

Tagging convention:

1. Base tags: `yarli`, `postrun-observability`, `haake-postrun-memory-sync`.
2. Outcome/state tags: `outcome-<success|failed>`, `run-state-<normalized-run-state>`.
