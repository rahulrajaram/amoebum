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
| `ptui/examples` | Example demos and app(s) | `cffi`, `bordeaux-threads` |
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

## Architecture Diagram

A two-tier ASCII diagram showing the kernel layer (buffer, renderer, terminal backend,
engine loop) versus the application layer (panels, widgets, themes, layout engine):

- `ptui/docs/kernel-vs-app.md`

## API Notes (`ptui/text`, `ptui/layout`)

Short API docs for the reusable text and layout systems:

- `ptui/docs/text-layout-api.md`
- `ptui/docs/metrics-dashboard-parity.md`
- `ptui/docs/positioning.md`

## Quicklisp + Dependency Setup

PTUI uses a local Quicklisp install under `ptui/.tools/` via:

```bash
./ptui/bin/ensure-quicklisp.sh
```

## Smoke: Load Every System Independently

This is the main modularity check. It loads the modular and standalone example systems, so it catches both the themed demos and the low-level basics examples:

```bash
./ptui/bin/check-systems.sh
```

## Build + Run The Example TUIs

```bash
./ptui/bin/build.sh
PTUI_EXIT_AFTER_MS=5000 ./ptui/dist/metrics-dashboard
PTUI_EXIT_AFTER_MS=5000 ./ptui/dist/atop-dashboard
```

Minimal kernel demos (loaded via `ptui/examples` and `ptui/examples-standalone`):

- `ptui.examples.buffer-basics:main` for direct cell-buffer drawing, fill-rect, and border primitives
- `ptui.examples.text-layout-basics:main` for width-safe text wrapping into a bordered region
- `ptui.examples.event-handling-basics:main` for a tiny event loop with `:up`, `:down`, `r`, and default quit handling

Additional themed examples (loaded via `ptui/examples`):

- `ptui.examples.ops-wallboard:run-ops-wallboard`
- `ptui.examples.release-tracker:run-release-tracker`
- `ptui.examples.focus-console:run-focus-console`

Minimal example invocations:

```bash
sbcl --load ptui/.tools/quicklisp/setup.lisp \
  --eval '(pushnew (truename "./ptui/") asdf:*central-registry*)' \
  --eval '(asdf:load-system :ptui/examples)' \
  --eval '(ptui.examples.buffer-basics:main)' \
  --quit

sbcl --load ptui/.tools/quicklisp/setup.lisp \
  --eval '(pushnew (truename "./ptui/") asdf:*central-registry*)' \
  --eval '(asdf:load-system :ptui/examples)' \
  --eval '(ptui.examples.text-layout-basics:main)' \
  --quit

sbcl --load ptui/.tools/quicklisp/setup.lisp \
  --eval '(pushnew (truename "./ptui/") asdf:*central-registry*)' \
  --eval '(asdf:load-system :ptui/examples)' \
  --eval '(ptui.examples.event-handling-basics:main)' \
  --quit
```

Themed example invocation:

```bash
sbcl --load ptui/.tools/quicklisp/setup.lisp \
  --eval '(pushnew (truename "./ptui/") asdf:*central-registry*)' \
  --eval '(asdf:load-system :ptui/examples)' \
  --eval '(ptui.examples.release-tracker:run-release-tracker)' \
  --quit
```

## Description-First Definition Files

PTUI now supports loading file-driven UI definitions that compile into existing
`defpanel`/`defapp` macros at runtime via `ptui.ui.definition-loader`.

Supported declarative directives:

- `(:ptui ...)` (wrapper)
- `(:defpackage ...)`, `(:in-package ...)`
- `(:panel ...)` -> `ptui.ui.panel:defpanel`
- `(:app ...)` -> `ptui.ui.app:defapp`
- `(:widget ...)` -> `ptui.widgets.defwidget:defwidget`

Load and run a declarative definition file:

```bash
sbcl --load ptui/.tools/quicklisp/setup.lisp \
  --eval '(pushnew (truename "./ptui/") asdf:*central-registry*)' \
  --eval '(asdf:load-system :ptui)' \
  --eval '(ptui.ui.definition-loader:load-definition-file
            "ptui/examples/declarative-incident-board.lisp")' \
  --eval '(ptui.ui.definition-loader:run-loaded-app
            (find-symbol "INCIDENT-BOARD-APP"
                         "PTUI.EXAMPLES.DECLARATIVE.INCIDENT-BOARD"))' \
  --quit
```

You can also call `ptui.ui.definition-loader:load-definition-file` to only
register panels/apps and invoke the generated runner later.

Manual packet/definition QA assets:

- Playbook: `ptui/docs/packet-manual-test-playbook.md`
- Golden fixtures: `ptui/examples/golden-packets/`

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

## Standalone Packaging

`ptui/standalone` is an ASDF system that loads the full kernel as a single system
rather than a collection of `ptui/*` sub-systems.  This is useful when you want to
vendor PTUI into another project without exposing the internal sub-system split.

### What "standalone" means

- `ptui/standalone` includes the same source files as the umbrella `ptui` system.
- `ptui/components-standalone` layers the components tier on top of `ptui/standalone`.
- `ptui/examples-standalone` exercises both.
- It does **not** mean "no Quicklisp" or "no native deps" — `cffi` and
  `bordeaux-threads` are still required.

### Build a standalone binary

```bash
# Build the metrics-dashboard binary (uses ptui/standalone internally)
./ptui/bin/build.sh

# Verify the binary is self-contained (no Quicklisp needed at runtime)
ldd ptui/dist/metrics-dashboard          # only libc + libpthread + libdl
./ptui/dist/metrics-dashboard --help     # prints usage and exits
```

### Vendoring into another project

```lisp
;; In your project's .asd file, add ptui as a dependency:
(defsystem "my-app"
  :depends-on ("ptui/standalone" "ptui/components-standalone")
  :components (...))
```

Point ASDF at the ptui directory:

```bash
export ASDF_CENTRAL_REGISTRY="$ASDF_CENTRAL_REGISTRY:/path/to/ptui/"
```

Or push it at startup:

```lisp
(pushnew (truename "/path/to/ptui/") asdf:*central-registry*)
```

## Example Workflows

### Smoke-test every system independently

Useful after modifying `*.asd` files or adding a new sub-system:

```bash
./ptui/bin/check-systems.sh
```

### Smoke-test the minimal example entry points

Useful after changing `ptui/examples/*.lisp` or the example system wiring:

```bash
./ptui/bin/smoke-examples.sh
```

### Run kernel unit tests

```bash
make test-ptui
./ptui/bin/test.sh
```

### Run the full compliance gate (build + smoke + PTY tests + ncurses check)

```bash
./ptui/bin/compliance-gate.sh
```

### Run PTUI terminal regression harnesses

```bash
./ptui/bin/tmux-layout-regression.sh
./ptui/bin/tmux-behavior-regression.sh
./ptui/bin/tmux-ansi-regression.sh
./ptui/bin/capture-screenshots.sh
```

Each harness uses tracked references under `ptui/test/snapshots/` and should be
run one at a time when working with tmux-backed PTUI demos.

### Update screenshot-backed visual baselines

```bash
# Refresh one canonical case
./ptui/bin/capture-screenshots.sh --update --demo metrics-dashboard

# Re-check all tracked visual references
./ptui/bin/capture-screenshots.sh
```

### Run the performance regression test

```bash
# Quick (3 prompts) — suitable for CI
./bin/tui-perf-test.sh --prompts 3

# Full (5 prompts, default) — recommended before release
./bin/tui-perf-test.sh

# Detailed /proc snapshots + pause for manual inspection
./bin/tui-perf-test.sh --report --watch
```

See `ptui/docs/benchmark-story.md` for baseline numbers and before/after comparison
instructions.

### Release verification

```bash
./ptui/bin/release-checklist.sh
```

This runs build, tests, tmux layout/behavior/ANSI/visual regressions,
performance smoke, and required-docs checks in sequence and prints a summary.
