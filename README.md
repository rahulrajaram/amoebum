# amoebum

<p align="center">
  <img src="docs/assets/amoebum-logo.png" alt="amoebum logo" width="320" />
</p>

A Common Lisp monorepo for terminal AI workflows: `amoebum`, `ptui`, `pseudopod`, and `sw4rm-sdk`.

## What is this?
`amoebum` is a Common Lisp workspace for building and running terminal-first AI tooling. The repository combines a TUI kernel (`ptui`), an application/runtime layer (`amoebum`), an LLM provider client (`pseudopod`), and workflow/orchestration helpers (`sw4rm-sdk`).

The primary operator entrypoint is `bin/amoebum`, which sanitizes continuation state and launches `yarli run --stream`. The repo is designed for iterative tranche-based development, local verification loops, and reproducible terminal automation.

This README documents only behavior that is currently implemented in the repository as of February 28, 2026.

## Features
- Monorepo with separately loadable ASDF systems:
  `amoebum`, `ptui`, `pseudopod`, `sw4rm-sdk`.
- Terminal UI kernel (`ptui`) with component/runtime/render/backend layers.
- AI workflow app (`amoebum`) with permissions, sandbox integration, tools, panels, and tests.
- Provider client layer (`pseudopod`) with OpenAI-compatible and Anthropic provider support.
- SW4RM-oriented SDK helpers and local registry/router components.
- Yarli-oriented operational scripts for remediation, post-run memory sync, verification, and deterioration reporting.

## Installation
### Build from source (supported)
Prerequisites:
- `sbcl`
- `quicklisp` (or set `QUICKLISP_SETUP`)
- `yarli`
- `jq`
- `psql` (optional, for sanitize/reconciliation DB path)

Build:
```bash
make build
```

Run tests:
```bash
make test
```

### Cargo install (not available)
This repository is Common Lisp-based and does not provide a Cargo package.

### Docker (not available)
No Docker image or Dockerfile is currently provided in this repository.

## Quick Start
1. Clone the repository and `cd` into it.
2. Ensure dependencies are installed (`sbcl`, `quicklisp`, `yarli`, `jq`).
3. Run verification:
```bash
make check
```
4. Start the main workflow:
```bash
./bin/amoebum
```

## CLI Reference
The outputs below are from actual `--help` executions in this repo.

### `bin/amoebum --help`
```text
Usage:
  bin/amoebum

Behavior:
  1) sanitize continuation state
  2) run `yarli run --stream`

No subcommands or arguments are supported.
```

### `bin/yarli-sanitize-continuation.sh --help`
```text
Usage:
  bin/yarli-sanitize-continuation.sh

Purpose:
  Clears stale continuation state when IMPLEMENTATION_PLAN.md has zero open
  tranches, and optionally reconciles related run rows/events in Postgres.

Notes:
  - Reads settings from ./yarli.toml and objective from ./PROMPT.md.
  - Requires no arguments.
```

### `bin/yarli-remediate-run.sh --help`
```text
Usage:
  bin/yarli-remediate-run.sh <run-id> [--dispatch-cmd <cmd>] [--template <path>] [--dry-run]

Examples:
  bin/yarli-remediate-run.sh 019c4f70-07a7-7703-bfa6-5d7a5f19948c
  bin/yarli-remediate-run.sh 019c4f7007 --dispatch-cmd "bash -lc 'echo remediation stub'"
```

### `bin/yarli-postrun-memory-sync.sh --help`
```text
Usage:
  bin/yarli-postrun-memory-sync.sh --run-id <run-id|short-id>
  bin/yarli-postrun-memory-sync.sh --latest

Options:
  --config <path>         Path to yarli.toml (default: ./yarli.toml)
  --fallback-file <path>  Fallback memory log path (default: ./.agent/memory-log.md)
  --dry-run               Print resolved payload/sink without writing
  -h, --help              Show this help
```

### `bin/yarli-deterioration-report.sh --help`
```text
Usage:
  bin/yarli-deterioration-report.sh [--window-runs <n>] [--output <path>] [--dry-run]
  bin/yarli-deterioration-report.sh --synthetic-profile <observe|retry|remediate|escalate> [--assert-action <class>]

Options:
  --config <path>            Path to yarli.toml (default: ./yarli.toml)
  --window-runs <n>          Number of recent runs to analyze (default: 20)
  --output <path>            Report file path (default: ./.agent/deterioration-report.md)
  --audit-file <path>        Override audit file path (default: from yarli.toml)
  --dry-run                  Print report to stdout without writing output file
  --synthetic-profile <id>   Run synthetic classification probe only
  --assert-action <class>    Expected class for synthetic or real classification
  -h, --help                 Show this help
```

### `bin/yarli-run-verification.sh --help`
```text
Usage:
  bin/yarli-run-verification.sh
  bin/yarli-run-verification.sh --print-commands
```

### External CLI used by this repo: `yarli`
```text
Usage: yarli [OPTIONS] <COMMAND>

Commands:
  run
  task
  gate
  worktree
  merge
  audit
  plan
  debug
  migrate
  init
  info
```

### CLI Help-Text Audit Report (repo scripts)
Commands scanned: 16 (`bin/*` entrypoints)

Issues found:
- `bin/yarli-codex.sh --help` returns a missing prompt-file error instead of usage.
- `bin/yarli-codex-stdin.sh --help` exits with "No prompt provided via stdin." instead of usage.
- `bin/yarli-lint-implementation-plan.sh --help` treats `--help` as file input.
- `bin/check-dist-ignore.sh --help` executes checks instead of printing help.
- `bin/build-binary.sh --help` starts a real build instead of printing help.

Fixed in this run:
- `bin/yarli-sanitize-continuation.sh` now supports `-h/--help` and rejects unexpected args.
- `bin/yarli-verify-gate-parity.sh` now supports `-h/--help` and falls back to `yarli.toml.example` when `yarli.toml` is absent.

## AI Agent Integration
This repo is set up for Yarli-driven agent execution and includes wrappers:
- `bin/yarli-codex.sh`
- `bin/yarli-codex-stdin.sh`
- `bin/yarli-claude-wrapper.sh`

`yarli.toml` controls CLI backend invocation under `[cli]`. Example (current repo default):
```toml
[cli]
backend = "custom"
prompt_mode = "arg"
command = "codex"
args = ["--dangerously-bypass-approvals-and-sandbox", "exec", "--json", "--model", "gpt-5.3-codex-spark", "--config", "model_reasoning_effort=high"]
```

MCP-related code exists under `amoebum/src/mcp/*` (JSON-RPC client/server + tool bridge) for stdio/streamable-http server connectivity.

## gRPC API
No repository-owned gRPC service definition is currently exposed (no `.proto` files in this repo).

## REST API
No repository-owned REST API server is currently exposed (no Axum/REST service module in this repo).

## Configuration
Primary config files:
- `yarli.toml` (runtime orchestration and CLI backend settings)
- `PROMPT.md` (run objective/context)
- `IMPLEMENTATION_PLAN.md` (tranche execution ledger)

Common environment variables read by code/scripts include:
- `QUICKLISP_SETUP`
- `AMOEBUM_STRIP_BINARY`, `AMOEBUM_UPX`
- `AMOEBUM_MODEL`, `AMOEBUM_PERMISSION_MODE`, `AMOEBUM_APPROVAL_POLICY`, `AMOEBUM_SANDBOX_MODE`, `AMOEBUM_SWARM_DELEGATION_MODE`
- `AMOEBUM_EVENT_JOURNAL`, `AMOEBUM_EVENT_JOURNAL_DIR`
- `AMOEBUM_TTS_COMMAND`, `AMOEBUM_TTS_VOICE`, `AMOEBUM_TTS_PYTHON_MODULE`, `AMOEBUM_TTS_AUTO_SPEAK`
- `OPENAI_API_KEY`, `ANTHROPIC_API_KEY`
- `PTUI_EXIT_AFTER_MS`, `PTUI_MAX_IDLE_SLEEP_MS`, `PTUI_LOG_LEVEL`, `PTUI_NATIVE_LIB`, `PTUI_DASHBOARD_MODE`
- `SW4RM_*` service address and tuning variables (see `sw4rm-sdk/src/config.lisp`)

## Architecture
High-level structure:

```text
bin/amoebum
  -> bin/yarli-sanitize-continuation.sh
  -> yarli run --stream
      -> amoebum (app/runtime/tools/panels)
          -> ptui (terminal UI kernel)
          -> pseudopod (provider clients)
          -> sw4rm-sdk (workflow/registry/router helpers)
```

Subsystem locations:
- `amoebum/` application layer, tools, panels, tests
- `ptui/` terminal UI kernel and component systems
- `pseudopod/` provider and model client logic
- `sw4rm-sdk/` orchestration and registry/router helpers

## Security
Security controls present in current codebase:
- Permission modes (`:supervised`, `:auto-edit`, `:full-auto`, `:yolo`, `:no-confirm`)
- Approval policies (`:untrusted`, `:on-failure`, `:on-request`, `:never`)
- Sandbox policies (`:strict`, `:off`)
- Sandbox modes (`:read-only`, `:workspace-write`, `:danger-full-access`)

Current defaults in `amoebum/src/config.lisp`:
- permission mode: `:supervised`
- approval policy: `:on-request`
- sandbox policy: `:strict`
- sandbox mode: `:workspace-write`

## Examples
Run the main entrypoint:
```bash
./bin/amoebum
```

Run full checks:
```bash
make check
```

Run only PTUI tests:
```bash
make test-ptui
```

Run only Amoebum tests:
```bash
make test-amoebum
```

Inspect run/task state with Yarli:
```bash
yarli run list
yarli run status <run-id>
yarli task list <run-id>
yarli task explain <task-id>
```

## License
MIT. See `LICENSE`.
