# PTUI

PTUI is a small terminal UI kernel intended to follow `PTUI_KERNEL_SPEC.md`.

This repo is a monorepo: components live together, but are structured as separately loadable ASDF systems so they can be reused (and eventually open-sourced) independently.

## Systems (Plug-And-Play)

Defined in `ptui/ptui.asd`:

| ASDF system | Purpose | External deps |
| --- | --- | --- |
| `ptui/caps` | Terminal capability probing (pure env parsing) | none |
| `ptui/core` | Core types/events/color policy | none |
| `ptui/util` | Logging + monotonic time | none |
| `ptui/runtime` | Queue + scheduler primitives | `bordeaux-threads` |
| `ptui/term` | TTY/input/signal integration | `cffi` |
| `ptui/text` | Grapheme/width/layout text pipeline | none |
| `ptui/layout` | Layout contracts + deterministic bounds solver | none |
| `ptui/layout/yoga` | Optional Yoga boundary adapter (`:ptui-layout-yoga`) | none |
| `ptui/ui` | Component runtime core (element tree/reconcile/lifecycle/focus) | none |
| `ptui/widgets` | Reusable widgets (text/box/stack/spacer/input/scroll) | none |
| `ptui/components` | Higher-level composable widgets (for PTUI apps) | none |
| `ptui/render` | Cell buffer + diff | none |
| `ptui/backend` | Backend protocol + ANSI backend (ncurses optional) | none |
| `ptui/engine` | Engine loop (`ptui.engine.loop:run`) | none |
| `ptui` | Umbrella system (full kernel) | `cffi`, `bordeaux-threads` |
| `ptui/examples` | Example app(s) | `cffi`, `bordeaux-threads` |
| `ptui/standalone` | Monolithic kernel system (single system, same code) | `cffi`, `bordeaux-threads` |
| `ptui/components-standalone` | Components layer on top of `ptui/standalone` | `cffi`, `bordeaux-threads` |
| `ptui/examples-standalone` | Example app(s) against `ptui/standalone` | `cffi`, `bordeaux-threads` |

Notes:

1. `ptui/standalone` means "one ASDF system loads the kernel". It does not mean "no Quicklisp" or "no native deps".
2. The "External deps" column above is what you need beyond ASDF itself; internal `ptui/*` dependencies are managed by ASDF.

## Architecture Layers

PTUI is intentionally split into three layers:

1. Base kernel/runtime layer:
   `ptui/core`, `ptui/util`, `ptui/runtime`, `ptui/term`, `ptui/text`, `ptui/layout`, `ptui/ui`, `ptui/widgets`, `ptui/render`, `ptui/backend`, `ptui/engine`, `ptui`, `ptui/standalone`.
2. Components layer (separate from base surface, but dependent on PTUI foundations):
   `ptui/components`, `ptui/components-standalone`.
3. Applications/examples:
   `ptui/examples`, `ptui/examples-standalone`.

Dependency rules:

1. `ptui/widgets` is primitive-only (no composite prompt/chat/form components).
2. Composite widgets belong in `ptui/components`.
3. Components may depend on widgets/base; base must not depend on components.

## API Notes (`ptui/text`, `ptui/layout`)

Short API docs for the reusable text and layout systems:

- `ptui/docs/text-layout-api.md`
- `ptui/docs/metrics-dashboard-parity.md`

## Quicklisp + Dependency Setup

PTUI uses a local Quicklisp install under `ptui/.tools/` via:

```bash
./ptui/bin/ensure-quicklisp.sh
```

## Smoke: Load Every System Independently

This is the main modularity check:

```bash
./ptui/bin/check-systems.sh
```

## Build + Run The Example TUI

```bash
./ptui/bin/build.sh
PTUI_EXIT_AFTER_MS=5000 ./ptui/dist/metrics-dashboard
```

Dashboard mode switch:

```bash
# ui/widgets dashboard path (default)
PTUI_EXIT_AFTER_MS=2000 ./ptui/dist/metrics-dashboard

# legacy dashboard path (compat mode)
PTUI_DASHBOARD_MODE=legacy PTUI_EXIT_AFTER_MS=2000 ./ptui/dist/metrics-dashboard
```

## Compliance Gate

```bash
./ptui/bin/compliance-gate.sh
```
