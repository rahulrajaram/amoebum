# amoebum

This repo contains an experimental Common Lisp terminal UI kernel ("PTUI") being built against `PTUI_KERNEL_SPEC.md`.

Licensing: see `LICENSE`.

## PTUI

See `ptui/README.md` for system/module layout and how to load/run.

## Yarli Usage

Run from repo root:

1. `yarli run --stream`
2. If stream rendering is unavailable, Yarli falls back to headless mode and should continue emitting structured stderr progress.

Run and task triage:

1. `yarli run status <run-id>`
2. `yarli run explain-exit <run-id>`
3. `yarli task list <run-id>`
4. `yarli task explain <task-id>`
5. `yarli task annotate ...` to persist blocker details against a task (see `yarli task --help` for exact args).

Audit inspection:

1. `yarli audit tail --lines 100`
2. Expect policy decisions plus command execution entries (command key, exit code, stderr excerpt, duration) when command auditing is enabled.

