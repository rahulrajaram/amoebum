# PTUI Testing Strategy

## Goal

PTUI now has good low-level coverage and a strong compliance gate, but it still
needs a product-level terminal testing stack for three failure classes that unit
tests miss:

1. Layout and reflow regressions that only show up at real terminal sizes or
   after a resize.
2. Interaction regressions in focus, overlays, and keyboard-driven flows.
3. Theme and visual regressions where structure is correct but ANSI/SGR or
   rendered output drifts.

This document defines the recommended layered strategy and the tranche order for
building it.

## Audit Summary

As of 2026-03-28:

- `make test-ptui` passes.
- `./ptui/bin/compliance-gate.sh` passes and remains the required terminal
  safety and backend gate.
- `./ptui/bin/tmux-layout-regression.sh` passes with tracked wide/narrow/live
  resize baselines under `ptui/test/snapshots/tmux-layout/`.
- `./ptui/bin/tmux-behavior-regression.sh` passes with tracked keyboard-flow
  baselines under `ptui/test/snapshots/tmux-behavior/`.
- `./ptui/bin/tmux-ansi-regression.sh` passes and asserts raw truecolor/x256
  SGR fragments for titles, badges, selections, and overlays.
- `./ptui/bin/capture-screenshots.sh` now captures deterministic text, ANSI,
  and PNG references under `ptui/test/snapshots/visual/` for the canonical
  dashboard, wallboard, narrow layout, and overlay cases.
- `bin/tmux-streaming-regression.sh --self-test` passes and is a strong model
  for tmux-driven artifact capture plus machine-readable verdicts.
- `./ptui/bin/release-checklist.sh` and `./bin/yarli-run-verification.sh` now
  run the PTUI layout, behavior, ANSI, and visual harnesses serially so the
  product-level terminal stack is part of normal release verification.

## Source Mapping

This testing wave is promoted from three VISION.md anchors:

- `9.11 Phase 3: Adaptive layout engine (incremental, non-solver)`
  Dirty-flag relayout on resize and graceful wide-to-narrow behavior need a
  real terminal matrix, not only pure data tests.
- `9.12 TUI Visual Design System + .tui-spec.yaml Format`
  Wrap, overflow, width, gutter, alignment, and runtime theme behavior need
  theme-aware capture and visual drift detection.
- `2.3.3 defkeys Edge Cases`
  PTUI still needs confidence around tmux/xterm key normalization, overlay
  precedence, and scripted keyboard flows.

## Layers

### 1. Contract and pure rendering tests

Keep the existing FiveAM and `ptui/test/run.lisp` suites as the first line of
defense. These should continue to own:

- geometry and buffer contracts
- text wrapping and measurement rules
- event normalization and reducer logic
- overlay state transitions that can be exercised without a real terminal

This layer stays fast and deterministic, but it is not sufficient for resize,
tmux key translation, or ANSI fidelity.

### 2. PTY safety and backend compliance

Keep `./ptui/bin/compliance-gate.sh` as the mandatory backend and terminal
health gate. It already proves:

- clear-screen behavior
- clean timed exit and SIGINT terminal restore
- ncurses load path and `:auto` backend resolution
- build and system smoke survivability

This layer protects terminal safety, not product UX correctness.

### 3. tmux layout and resize matrix

Add a PTUI-specific tmux harness that launches deterministic examples, captures
plain text and ANSI panes, and compares them across a width/height matrix.

Recommended first examples:

- `buffer-basics`
- `text-layout-basics`
- `release-tracker`

Recommended follow-on examples:

- `focus-console`
- `ops-wallboard`
- `atop-dashboard`

The first matrix should cover:

- canonical wide layout
- constrained narrow layout
- live resize from wide to narrow inside the same tmux session
- at least one short-height case to catch vertical clipping regressions

This layer is the main defense against reflow regressions like `NXT-155`.

### 4. Behavior and interaction scripting

Build a second tmux harness for keyboard-driven flows that need a real terminal
event path.

Initial focus:

- `focus-console`: selection movement and mode toggle
- `ops-wallboard`: selection movement and filter cycling
- `atop-dashboard`: pause/resume, help overlay, sort changes, process-detail
  overlay

The harness should assert terminal-observable outcomes, not internal state:

- visible selection marker changes
- footer/status text changes
- help/detail overlays appear and disappear
- resize does not break focus or leave stale overlay fragments

### 5. Raw ANSI and SGR assertions

tmux plain-text capture is not enough for theme fidelity. Add a layer that
stores or parses ANSI-preserving captures and asserts:

- expected SGR sequences for badges, headers, selected rows, and emphasis
- capability-tier behavior where truecolor and fallback modes differ
- normalization of dynamic content so color drift is visible without false
  failures from timestamps or counters

This should be PTUI-specific, even if it borrows the capture shape from
`bin/tui-appearance-test.sh`.

### 6. Screenshot-backed visual baselines

Add deterministic image baselines for a small set of canonical layouts and
themes. Keep the set narrow enough that release verification can run it
serially without turning it into the only source of truth.

Recommended order:

- render ANSI captures to PNG with `bin/ansi-to-png.py` for deterministic CI
- keep room for host-native screenshots later when a real terminal emulator
  path is available and stable

Canonical visual cases should stay small:

- one default dashboard
- one narrow/reflow case
- one overlay or focus case

## Example Coverage Map

| Example | Best use |
| --- | --- |
| `buffer-basics` | border and fill structure at fixed sizes |
| `text-layout-basics` | wrap and narrow-width reflow |
| `release-tracker` | column layout and resize behavior |
| `focus-console` | focus navigation and mode toggles |
| `ops-wallboard` | selection plus filter behavior under themed styling |
| `atop-dashboard` | overlay, help, detail, pause, and richer resize stress |
| `metrics-dashboard` | high-density real dashboard smoke and theme assertions |

## Tranche Order

### NXT-175

Build the PTUI tmux layout matrix harness for deterministic examples and add
wide, narrow, and live-resize baselines.

### NXT-176

Add PTUI behavior scripting for focus, overlays, and modal-like flows using
`focus-console`, `ops-wallboard`, and `atop-dashboard`.

### NXT-177

Add raw ANSI/SGR capture utilities and theme assertions so text-structural
greens cannot hide color or emphasis regressions.

### NXT-178

Add screenshot-backed visual baselines for a small canonical set and fix
`ptui/bin/capture-screenshots.sh` so it becomes a real PTUI artifact generator
instead of a placeholder.

### NXT-179

Wire the new PTUI harnesses into release verification and refresh the PTUI docs
to explain the layered test stack.

## Tradeoffs

- tmux text capture is the cheapest and most stable signal for layout, but it
  cannot prove theme fidelity on its own.
- ANSI/SGR capture is precise for colors and emphasis, but it does not prove
  pixel rendering or font-specific appearance.
- screenshot baselines are high-signal, but they are slower and more brittle,
  so they should stay narrow even when included in release verification.
- deterministic example apps are better for baseline goldens than live,
  time-varying dashboards; use the richer dashboards for smoke and selected
  targeted scenarios, not for the whole matrix.
