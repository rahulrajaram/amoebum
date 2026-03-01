# PTUI Architecture

## 1) Overview

PTUI is a Common Lisp terminal-native UI framework with a React-like component model.

- Declarative rendering through `defwidget` and `defpanel`, scoped hooks, and immutable widget trees.
- Core runtime pipeline is split across ASDF systems: execution spine + API/consumer layers.
- Backend abstraction supports ANSI by default and ncurses as optional backend.
- ASDF footprint: **17 core execution systems** plus convenience/test systems, with **27 unique PTUI system names** in `ptui.asd`.
- Estimated core LOC is in the ~5000 range for the spine; full PTUI source is larger with examples/tests.
- Stable consumer entrypoint: `ptui/api` re-exports public app/panel/widget APIs.

Note: `ptui/api` is currently declared twice in `ptui.asd` with identical contents.

## 2) System map (all `ptui.asd` systems with deps)

| System | Depends on |
| --- | --- |
| `ptui/caps` | `()` |
| `ptui/core` | `("ptui/caps")` |
| `ptui/util` | `()` |
| `ptui/search` | `("cl-ppcre")` |
| `ptui/runtime` | `("bordeaux-threads" "ptui/util")` |
| `ptui/term` | `("cffi" "ptui/caps" "ptui/core" "ptui/util")` |
| `ptui/text` | `()` |
| `ptui/render` | `("ptui/core" "ptui/text")` |
| `ptui/layout` | `("ptui/text")` |
| `ptui/constraints` | `("ptui/layout")` |
| `ptui/layout/yoga` | `("ptui/layout")` |
| `ptui/ui` | `("ptui/core" "ptui/layout")` |
| `ptui/widgets` | `("ptui/ui" "ptui/layout" "ptui/text")` |
| `ptui/hooks` | `("ptui/widgets")` |
| `ptui/app` | `("ptui/hooks" "ptui/widgets" "ptui/engine" "ptui/render" "ptui/constraints")` |
| `ptui/views` | `("ptui/app" "ptui/widgets" "ptui/hooks" "ptui/constraints")` |
| `ptui/panel` | `("ptui/views" "ptui/hooks" "ptui/constraints" "ptui/app")` |
| `ptui/api` | `("ptui/app" "ptui/panel")` |
| `ptui/components` | `("ptui/widgets" "ptui/search")` |
| `ptui/backend` | `("ptui/core" "ptui/term" "ptui/render" "ptui/util")` |
| `ptui/engine` | `("ptui/backend" "ptui/runtime" "ptui/util" "ptui/render")` |
| `ptui/standalone` | monolithic source bundle (`caps`, `core`, `runtime`, `term`, `text`, `layout`, `ui`, `widgets`, `render`, `backend`, `engine`, `hooks`, `app`, `views`, `panel`, `definition-loader`, etc.) |
| `ptui/components-standalone` | `("ptui/standalone")` |
| `ptui` | `("ptui/core" "ptui/util" "ptui/search" "ptui/runtime" "ptui/term" "ptui/text" "ptui/layout" "ptui/constraints" "ptui/ui" "ptui/widgets" "ptui/render" "ptui/backend" "ptui/engine" "ptui/hooks" "ptui/app" "ptui/views" "ptui/panel" "ptui/api")` |
| `ptui/examples` | `("ptui" "ptui/components")` |
| `ptui/examples-standalone` | `("ptui/components-standalone")` |
| `ptui/test-support` | `("ptui/backend" "ptui/render" "ptui/core")` |
| `ptui/tests` | `("ptui" "ptui/components" "ptui/test-support" "fiveam")` |

## 3) Data flow

Data path from terminal input to render commit:

1. Backend (`ptui/backend/ansi` or optional `ptui/backend/ncurses`) polls events via `backend-poll-events`.
2. `ptui.engine.loop:run` receives events and executes app-level handling.
3. Optional interceptors run in `defapp` before widget routing.
4. Event routing goes through:
   - focus/routing in `ptui.ui.runtime:route-event`,
   - widget dispatch in `ptui.widgets.core` (`on-event`, `on-event-capture`).
5. Widget event handlers trigger hooks/state updates (`use-state`, `use-effect`, etc.).
6. New tree render builds widget elements and reconciles with `ptui.ui.runtime:update-runtime`.
7. Reconciled tree is painted via `ptui.ui.app` registry and laid out through `ptui.layout` + `ptui.layout.constraints`.
8. `ptui.render.diff:diff-buffers` diffs next/prev cell buffers.
9. Backend receives draw ops and commits with `backend-commit`.
10. Terminal cursor/style/text updates are emitted.

Additional streams:
- `ptui.engine.loop:run` also executes scheduler/event-bus callbacks.
- `ptui.util.log` captures render/runtime/compliance observability data.

## 4) Key abstractions

- Widget tree
  - `ptui.ui.elements`: immutable `ui-element` records with props/children/key/focus.
  - Reconciliation in `ptui.ui.runtime:reconcile-trees`.
- Hooks and lifecycle
  - Context threading via `*current-runtime*` / `*current-widget-context*`.
  - `use-state`, `use-effect`, `use-memo`, `use-callback`, `use-context`.
- `defpanel` DSL
  - Sections: `:state`, `:data`, `:layout`, `:keys`, `:effects`, `:slots`, `:style`, `:context`.
  - Supports composable regions with `:fixed` / `:flex` / `:percentage`.
- Constraint layout
  - `ptui.layout` core API and nodes,
  - `ptui.layout.constraints` + `ptui.layout.solver` for allocation,
  - `ptui.layout.constraint-layout` for region binding.
- App shell
  - `defapp` controls initialization, lifecycle, render loop, event loop, and shutdown.

## 5) Extension Points

- Backend protocol (`ptui.backend.protocol`): `backend-init`, `backend-size`, `backend-poll-events`, `backend-commit`, `backend-shutdown`.
- Painter registry (`ptui.ui.app:*view-paint-registry*`, `register-view-painter`) for custom `ui-element` drawing.
- `defpanel` composition (`embed-panel`, `panel-slot`) and `ptui.ui.definition-loader` allowlist-based DSL loading (`:ptui`, `:panel`, `:app`, `:widget`).
- `defapp` interceptors for pre-routing policy and event transforms.
- Consumer layers (`ptui/components`, `ptui/examples`, `ptui/api`) for extension without touching the core spine.

## 6) ASCII dependency graph

```text
                         ptui/caps
                            |
                      +-----+------+
                      |   ptui/core  |
                      +-----+------+
                            / \
                           /   \ 
                 +--------+     +-----------------+
             ptui/text  ptui/layout<----->ptui/constraints
                 |          |     ^            ^
                 |          |     |            |
                 v          v     |            |
            ptui/render   ptui/ui -----> ptui/widgets ----> ptui/hooks
                              |            ^                |
                              v            |                v
                           ptui/engine <---+------ ptui/app <---- ptui/panel <---- ptui/views
                              |                ^                   ^
                              |                |                   |
                           ptui/backend ------>+------------------->+
                              |                               ^
                     ansi / ncurses (optional)                  |
                                                                 ptui/api
```

`ptui/standalone`, `ptui/components`, `ptui/examples`, and `ptui/tests` are upper-layer
systems built on top of the core execution chain.
