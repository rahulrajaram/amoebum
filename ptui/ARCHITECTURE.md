# PTUI Architecture

## 1) Overview

PTUI is a Common Lisp terminal UI framework with a React-like component model:
- components are declared with `defwidget`
- apps are declared with `defapp`
- panels are composed with `defpanel`
- state and lifecycle are driven by hooks (`use-state`, `use-effect`, `use-memo`, `use-context`)

From `ptui/ptui.asd`, PTUI is organized as a core execution chain of 17 runtime systems
(`ptui/caps` through `ptui/panel`), plus higher-layer systems (`ptui/api`, `ptui/components`,
`ptui/standalone`, examples, and test systems). The `ptui/examples` layer now intentionally
mixes low-level kernel demos (`buffer-basics`, `text-layout-basics`, `event-handling-basics`)
with fuller application-style demos (`metrics-dashboard`, `atop-dashboard`, `panel-demo`,
`ops-wallboard`, `release-tracker`, `focus-console`). The PTUI `src/` tree is on the order of
~5k+ LOC for the runtime/render/layout spine and ~10k LOC overall.

## 2) System Map (all systems with deps)

System definitions below are taken from `ptui/ptui.asd`.

| System | Description | Depends on |
| --- | --- | --- |
| `ptui/caps` | Terminal capability probe (env parsing) | `()` |
| `ptui/core` | Core types/events/color | `ptui/caps` |
| `ptui/util` | Logging/time utilities | `()` |
| `ptui/search` | Glob and file-set search primitives | `cl-ppcre` |
| `ptui/runtime` | Queue/scheduler/event-bus primitives | `bordeaux-threads`, `ptui/util` |
| `ptui/term` | TTY/signals/input integration | `cffi`, `ptui/caps`, `ptui/core`, `ptui/util` |
| `ptui/render` | Cell-buffer and diff renderer | `ptui/core`, `ptui/text` |
| `ptui/text` | Grapheme/width/layout text pipeline | `()` |
| `ptui/layout` | Layout foundation API | `ptui/text` |
| `ptui/constraints` | Constraint specs/solver/layout bridge | `ptui/layout` |
| `ptui/layout/yoga` | Optional Yoga adapter boundary | `ptui/layout` |
| `ptui/ui` | Element tree/runtime reconciliation/focus | `ptui/core`, `ptui/layout` |
| `ptui/hooks` | React-like hooks implementation | `ptui/widgets` |
| `ptui/app` | App shell (`defapp`) + paint registry | `ptui/hooks`, `ptui/widgets`, `ptui/engine`, `ptui/render`, `ptui/constraints` |
| `ptui/widgets` | Widget primitives + `defwidget` | `ptui/ui`, `ptui/layout`, `ptui/text` |
| `ptui/views` | View primitives and painter helpers | `ptui/app`, `ptui/widgets`, `ptui/hooks`, `ptui/constraints` |
| `ptui/panel` | `defpanel` DSL + definition loader | `ptui/views`, `ptui/hooks`, `ptui/constraints`, `ptui/app` |
| `ptui/api` | Public API re-export layer | `ptui/app`, `ptui/panel` |
| `ptui/components` | Higher-level composable widgets | `ptui/widgets`, `ptui/search` |
| `ptui/backend` | Backend protocol + ANSI/ncurses backends | `ptui/core`, `ptui/term`, `ptui/render`, `ptui/util`, `cl-charms` (feature-gated) |
| `ptui/engine` | Main frame/event loop | `ptui/backend`, `ptui/runtime`, `ptui/util`, `ptui/render` |
| `ptui/standalone` | Monolithic no-internal-deps PTUI bundle | `cffi`, `bordeaux-threads`, `cl-ppcre`, `cl-charms` (feature-gated) |
| `ptui/components-standalone` | Components atop standalone PTUI | `ptui/standalone` |
| `ptui` | Umbrella system for modular PTUI stack | `ptui/core`, `ptui/util`, `ptui/search`, `ptui/runtime`, `ptui/term`, `ptui/text`, `ptui/layout`, `ptui/constraints`, `ptui/ui`, `ptui/widgets`, `ptui/render`, `ptui/backend`, `ptui/engine`, `ptui/hooks`, `ptui/app`, `ptui/views`, `ptui/panel`, `ptui/api` |
| `ptui/examples` | Example apps (modular stack) | `ptui`, `ptui/components` |
| `ptui/examples-standalone` | Example apps (standalone stack) | `ptui/components-standalone` |
| `ptui/test-support` | Snapshot test backend/harness | `ptui/backend`, `ptui/render`, `ptui/core` |
| `ptui/tests` | FiveAM test suites | `ptui`, `ptui/components`, `ptui/test-support`, `fiveam` |

## 3) Data Flow

The primary interaction/render pipeline is:

`Event -> Runtime -> Hooks -> Widget Tree -> Layout -> Render -> Backend -> Terminal`

1. Event
- `ptui.backend.protocol:backend-poll-events` reads terminal events from the active backend
  (`ptui/backend/ansi` or optional `ptui/backend/ncurses`).

2. Runtime
- `ptui.engine.loop:run` drives the frame loop, event polling, scheduler ticks, redraw timing,
  and event-bus draining.
- `ptui.ui.runtime:route-event` maps input to focused widget targets.

3. Hooks
- Widget handlers and panel logic use hooks in `ptui/src/ui/hooks.lisp` (`use-state`,
  `use-effect`, `use-memo`, `use-callback`, `use-context`) to update runtime state/effects.

4. Widget tree
- `defwidget`/`defpanel` produce immutable `ui-element` trees.
- `ptui.ui.runtime:update-runtime` reconciles old/new trees and runs mount/unmount/effect phases.

5. Layout
- Panel/layout nodes are transformed into constraint specs.
- `ptui.layout.solver:solve-constraints` allocates fixed/percentage/flex regions.

6. Render
- `ptui.ui.app:%paint-element` paints the tree into a cell buffer.
- `ptui.render.diff:diff-buffers` computes incremental draw operations.

7. Backend
- `ptui.backend.protocol:backend-commit` flushes draw operations to the active backend.

8. Terminal
- ANSI escape sequences (or ncurses calls when enabled) update terminal state on screen.

## 4) Key Abstractions

- Widget tree
  - `ptui/src/ui/elements.lisp` defines `ui-element` records (type/props/children/key/id/focusable).
  - `ptui/src/widgets/defwidget.lisp` provides declarative component construction and local widget context.

- Hooks
  - `ptui/src/ui/hooks.lisp` implements `use-state`, `use-effect`, `use-memo`, `use-callback`, and context APIs.
  - Hook state is scoped via runtime widget-context keys.

- Panel DSL
  - `ptui/src/ui/panel.lisp` macro `defpanel` compiles sections: `:state`, `:data`, `:effects`,
    `:layout`, `:keys`, `:context` (plus extension sections `:slots`, `:style`).

- Constraint layout
  - `ptui/src/layout/constraints.lisp`, `ptui/src/layout/solver.lisp`, and
    `ptui/src/layout/constraint-layout.lisp` implement fixed/percentage/flex allocation.

- App shell
  - `ptui/src/ui/app.lisp` macro `defapp` wires backend selection, lifecycle (`on-mount`/
    `on-unmount`), interceptors, routing, render function, and engine loop execution.

## 5) Extension Points

- Backends
  - Implement `ptui.backend.protocol:terminal-backend` methods (`backend-init`, `backend-size`,
    `backend-poll-events`, `backend-commit`, `backend-shutdown`).

- View painters
  - Register custom painters via `ptui.ui.app:register-view-painter` and `*view-paint-registry*`.

- `defpanel` composition
  - Compose reusable panels using `embed-panel`, `:slots`, and packet/definition loading in
    `ptui.ui.definition-loader`.

- Event interceptors
  - `defapp` accepts `:interceptors` (priority/predicate/handler triples) to apply pre-routing
    event transforms or short-circuit handling.

## 6) ASCII Dependency Graph

```text
                               ptui/caps
                                   |
                                ptui/core
                                   |
      +----------------------------+---------------------------+
      |                            |                           |
   ptui/term                    ptui/render                 ptui/ui
      |                       (uses ptui/text)          (uses ptui/layout)
      |                            |                           |
  ptui/backend                  ptui/engine <---------+   ptui/widgets
      |                            ^                 |       |
      +------------+---------------|-----------------+       |
                   |               |                         ptui/hooks
                ptui/runtime       |                            |
                   |               +--------- ptui/app ---------+
                   |                                |
             (event/scheduler)                 ptui/views
                                                    |
                                                 ptui/panel
                                                    |
                                                  ptui/api

Higher layers:
  ptui/components -> (ptui/widgets, ptui/search)
  ptui (umbrella) -> core modular chain above
  ptui/standalone -> monolithic equivalent of modular chain
  ptui/components-standalone -> ptui/standalone
  ptui/examples, ptui/examples-standalone, ptui/test-support, ptui/tests
```
