# PTUI Positioning

PTUI should be presented as its own product surface, not just as "the UI code inside amoebum."

## Core Position

PTUI is a terminal UI kernel for building operational interfaces that need predictable rendering, reusable primitives, and app-level structure without giving up low-level control.

Short version:

- Amoebum is an application.
- PTUI is the reusable terminal UI kernel that can power many applications.

## What PTUI Sells

PTUI is strongest when we sell it around engineering outcomes instead of implementation details:

- Deterministic terminal rendering with a real diff/buffer pipeline.
- A layered architecture: kernel, widgets, components, and apps.
- Reusable example applications that prove the kernel is not toy infrastructure.
- Verification discipline: build, smoke, compliance, and perf gates are part of the story.
- A path from "single dashboard" to "full terminal product" inside one stack.

## Best-Fit Audiences

PTUI is a better fit for these buyers and users than a generic "Lisp UI toolkit" pitch:

- Teams building internal operations dashboards.
- Developers shipping terminal-native control planes or assistants.
- Platform engineers who want testable TUIs, not one-off escape-sequence scripts.
- Common Lisp users who want an application kernel, not only a widget library.

## Message Hierarchy

Lead with outcomes:

1. Build terminal applications that feel structured, not improvised.
2. Keep rendering, input, layout, and backend concerns separated.
3. Prove claims with runnable demos and gates, not screenshots alone.

Support with technical proof:

- ANSI backend plus optional ncurses compatibility path.
- Diff-based rendering and cell-buffer model.
- Input, signal, and raw-TTY handling inside the kernel.
- Example apps: metrics dashboard, ops wallboard, release tracker, focus console.

## Product Split From Amoebum

PTUI should stand on three surfaces:

1. PTUI Kernel
   Market this as the terminal runtime and rendering foundation.
2. PTUI Components
   Market this as the reusable library layer for app builders.
3. PTUI Example Apps
   Market these as proof that PTUI can ship real operator workflows.

Amoebum then becomes one flagship app built on PTUI, not the only reason PTUI exists.

## Proof Assets To Build

The highest-value marketing assets are the ones we can regenerate from the repo:

- Clean screenshots or asciicasts of `metrics-dashboard`, `ops-wallboard`, and `release-tracker`.
- A benchmark/perf note that cites reproducible commands and artifact directories.
- A "kernel vs app" diagram that shows PTUI below amoebum and other possible apps.
- A landing-page section that maps examples to real use cases.
- A release checklist that requires build, smoke, compliance, and perf evidence.

## Suggested Language

Possible headline directions:

- "A terminal UI kernel for serious operator-facing applications."
- "Build structured TUIs with a real kernel, not ad hoc terminal glue."
- "A reusable terminal application stack for dashboards, assistants, and control planes."

Possible supporting sentence:

"PTUI gives Common Lisp teams a tested terminal kernel with rendering, layout, input, widgets, and example apps that can be shipped or adapted."

## Near-Term Repo Actions

To make PTUI easier to market separately, keep the next steps concrete:

- Keep `ptui/README.md` focused on PTUI-only value and runnable entry points.
- Add screenshot generation and capture guidance for each example app.
- Publish one benchmark/perf story with exact commands and saved artifacts.
- Add a short architecture/positioning diagram under `ptui/docs/`.
- Treat amoebum as a case study built on PTUI, not as PTUI's definition.
