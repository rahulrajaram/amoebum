# PTUI Architecture: Kernel vs Application Layer

This document describes the two-tier architecture of PTUI using ASCII diagrams and
explains which subsystems belong to each tier and what the dependency rules are.

## High-Level Overview

```
  ┌────────────────────────────────────────────────────────────────────┐
  │                      APPLICATION LAYER                             │
  │                                                                    │
  │   ┌──────────────┐  ┌──────────────┐  ┌──────────────────────┐   │
  │   │   Panels     │  │   Widgets    │  │   Theme Engine       │   │
  │   │  (defpanel)  │  │  (composite) │  │  (color + style)     │   │
  │   └──────┬───────┘  └──────┬───────┘  └──────────┬───────────┘   │
  │          │                 │                      │               │
  │   ┌──────┴─────────────────┴──────────────────────┴──────────┐   │
  │   │              Layout Engine  (ptui/layout)                 │   │
  │   │      bounds solver · flex · margin · cross-axis sizing    │   │
  │   └──────────────────────────────┬───────────────────────────┘   │
  │                                  │                               │
  │   ┌──────────────────────────────┴───────────────────────────┐   │
  │   │           Component Runtime  (ptui/ui)                    │   │
  │   │    element tree · reconcile · lifecycle · focus           │   │
  │   └──────────────────────────────────────────────────────────┘   │
  │                                                                    │
  └───────────────────────────────┬────────────────────────────────────┘
                                  │  (application layer depends on kernel
                                  │   kernel must NOT depend on app layer)
  ┌───────────────────────────────┴────────────────────────────────────┐
  │                         KERNEL LAYER                               │
  │                                                                    │
  │   ┌──────────────────────────────────────────────────────────┐   │
  │   │               Engine Loop  (ptui/engine)                  │   │
  │   │    run · tick scheduler · frame-rate control              │   │
  │   └────────────────────┬─────────────────────────────────────┘   │
  │                        │                                          │
  │          ┌─────────────┴──────────────┐                          │
  │          ▼                            ▼                           │
  │   ┌─────────────────┐      ┌─────────────────────────────────┐  │
  │   │   Cell Buffer   │      │      Event Loop / Input          │  │
  │   │  (ptui/render)  │      │        (ptui/term)               │  │
  │   │  buffer · diff  │      │  tty · signals · input · caps    │  │
  │   └────────┬────────┘      └───────────────┬─────────────────┘  │
  │            │                               │                     │
  │            ▼                               │                     │
  │   ┌─────────────────┐                      │                     │
  │   │ Terminal Backend│◄─────────────────────┘                     │
  │   │  (ptui/backend) │                                            │
  │   │  ANSI · ncurses │                                            │
  │   └────────┬────────┘                                            │
  │            │                                                     │
  │   ┌────────┴──────────────────────────────┐                      │
  │   │            Foundation                 │                      │
  │   │  ptui/core  ptui/runtime  ptui/util   │                      │
  │   │  types      queue         logging     │                      │
  │   │  events     scheduler     time        │                      │
  │   │  color      threads                   │                      │
  │   └───────────────────────────────────────┘                      │
  │                                                                    │
  └────────────────────────────────────────────────────────────────────┘
```

## Kernel Layer Subsystems

The kernel provides stable primitives that the application layer and external
consumers can rely on without pulling in application-specific concerns.

### Foundation (`ptui/core`, `ptui/runtime`, `ptui/util`)

| ASDF system | Key exports | Role |
|---|---|---|
| `ptui/core` | `ptui-event`, `ptui-color`, `ptui-cell` | Core types shared by all subsystems |
| `ptui/runtime` | `make-queue`, `schedule-interval`, `scheduler-run-due` | Thread-safe event queue + tick scheduler |
| `ptui/util` | `ptui-log`, `ptui-monotonic-ms` | Structured logging, monotonic clock |

### Terminal Backend (`ptui/term`, `ptui/backend`)

| ASDF system | Key exports | Role |
|---|---|---|
| `ptui/term` | `tty-enter-raw`, `tty-restore`, `read-input-event`, `install-signals` | Low-level TTY control and signal wiring |
| `ptui/caps` | `detect-truecolor-p`, `detect-color-depth` | Read-only terminal capability probing |
| `ptui/backend` | `backend-protocol`, `ansi-backend`, `ncurses-backend` (optional) | Render target abstraction |

The backend protocol decouples the cell-buffer diff algorithm from the specific escape
sequences used to paint the screen.  Switching between ANSI and ncurses is a
configuration flag (`PTUI_ENABLE_NCURSES=1`), not a code change.

### Cell Buffer and Renderer (`ptui/render`)

| ASDF system | Key exports | Role |
|---|---|---|
| `ptui/render` | `make-buffer`, `buffer-set-cell`, `buffer-diff`, `buffer-clear` | Double-buffered cell grid + minimal-diff patch generation |

The buffer is a flat array of `ptui-cell` structs (character + fg + bg + attributes).
On each frame the engine computes a diff between the previous and current buffer and
emits only the changed cells to the backend.

### Engine Loop (`ptui/engine`)

The engine loop owns the main thread.  It:

1. Calls the application's render function to produce a new buffer state.
2. Diffs the new buffer against the previous one.
3. Flushes the patch to the terminal backend.
4. Sleeps until the next tick, honouring `PTUI_MAX_IDLE_SLEEP_MS`.
5. Runs any due scheduler callbacks (timers, animations).

### Text Pipeline (`ptui/text`)

Grapheme segmentation, Unicode width calculation, and the text-layout API (line
wrapping, truncation, alignment) live here.  They are pure computation with no I/O
dependency and can be used outside a running engine loop.

## Application Layer Subsystems

### Layout Engine (`ptui/layout`)

The layout engine computes deterministic bounds for all nodes in the element tree.  It
implements a simplified flex-like algorithm: main-axis sizing from declared widths and
flex-grow, cross-axis sizing from content or parent constraints.  An optional Yoga
adapter (`ptui/layout/yoga`) exposes the same protocol using Facebook Yoga for more
complex layout needs.

### Component Runtime (`ptui/ui`)

`defpanel` and `defapp` macros compile to component-descriptor structs registered in a
global registry.  The component runtime manages:

- The element tree (a DAG of `ptui-element` nodes)
- Reconciliation (diffing old and new element trees before buffer paint)
- Lifecycle callbacks (`on-mount`, `on-unmount`, `on-focus`, `on-blur`)
- Focus traversal (Tab / Shift-Tab, arrow keys within a panel)

### Widgets (`ptui/widgets`, `ptui/components`)

`ptui/widgets` contains primitive leaf widgets: `text-widget`, `box-widget`,
`stack-widget`, `spacer-widget`, `input-widget`, `scroll-widget`.  These are
single-responsibility and have no knowledge of application domain.

`ptui/components` layers composite, reusable patterns on top: chat panels, form
wizards, status bars, progress indicators.  Components may depend on widgets and the
kernel; widgets must not depend on components.

### Theme Engine

Themes are property maps keyed by semantic names (`foreground`, `border`,
`accent-primary`, etc.).  The theme resolver walks the element tree and attaches
resolved `ptui-color` values before layout runs, so panels never embed literal color
values.

## Dependency Rules (enforced by ASDF)

```
kernel-foundation  ──►  kernel-term  ──►  kernel-render  ──►  kernel-engine
                                                                     │
                                                                     ▼
                                                           app-layout  app-ui
                                                                │
                                                                ▼
                                                     app-widgets  app-components
                                                                │
                                                                ▼
                                                          examples / apps
```

Rules:

1. Kernel must not load any `ptui/components` or `ptui/ui` namespaces.
2. `ptui/widgets` must not `(use-package)` or `(import)` from `ptui/components`.
3. Components may depend on widgets and the kernel.
4. Applications and examples may depend on anything.

These rules are verified by `./ptui/bin/check-systems.sh`, which loads each ASDF
system in isolation and confirms no unexpected inter-tier dependency pulls in a higher
tier at load time.

## Further Reading

- `ptui/docs/text-layout-api.md` — Layout property reference
- `ptui/docs/defpanel-guide.md` — Panel/widget definition DSL
- `ptui/docs/tui-taxonomy.md` — Widget taxonomy and naming conventions
- `ptui/docs/kernel-audit.md` — Detailed kernel audit notes
- `ptui/ARCHITECTURE.md` — High-level architecture overview
