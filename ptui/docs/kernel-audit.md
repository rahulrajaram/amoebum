# PTUI Kernel Audit (Spec 01–16)

This document maps `PTUI_KERNEL_SPEC.md` steps 01–16 to the current repo implementation, with evidence commands and PASS/FAIL status.

Notes:

* “PASS” means the step’s functional intent and “Done when” criteria are met.
* “FAIL” means missing, incomplete, or a spec deviation that should be reconciled.
* Step 15 (ncurses) is explicitly optional in the spec; it is tracked separately.

---

## 01) Repo + build spine

**Spec:** `ptui.asd` + `bin/build.sh`, build a single binary using buildapp, provide `ptui.engine.loop:main`, restore terminal on exit and Ctrl-C.

**Implementation:**

* ASDF: `ptui/ptui.asd`
* Build: `ptui/bin/build.sh` (buildapp, produces `ptui/dist/metrics-dashboard`)
* Entrypoint: `ptui/src/engine/loop.lisp` (`ptui.engine.loop:main`)

**Evidence:**

```bash
./ptui/bin/build.sh
PTUI_EXIT_AFTER_MS=500 ./ptui/dist/metrics-dashboard
./ptui/bin/compliance-gate.sh
```

**Status:** PASS (binary builds; terminal restore is gated by compliance PTY checks).

**Spec-compat note:** Spec lists ASDF systems `ptui` + `ptui-examples`. Repo provides `ptui/examples` as the primary examples system and a compatibility alias system `ptui-examples` (defined in `ptui/ptui-examples.asd`).

---

## 02) Dependency discipline

**Spec:** pin Quicklisp dist date and only allow `cffi`, `bordeaux-threads`, and optionally `cl-charms`/`croatoan` behind `:ptui-ncurses`.

**Implementation:**

* Pin: `ptui/deps/quicklisp-dist.txt`
* Bootstrap: `ptui/bin/ensure-quicklisp.sh` installs dist + quickloads `cffi` and `bordeaux-threads`
* Optional ncurses is feature-guarded: `#+ptui-ncurses` in `ptui/ptui.asd`

**Evidence:**

```bash
./ptui/bin/ensure-quicklisp.sh
./ptui/bin/check-deps.sh
```

**Status:** PASS (within allowed dependency set for 01–16).

---

## 03) Core types (`src/core/types.lisp`)

**Spec:** export the exact geometry/style/cell/buffer API and deterministic allocation.

**Implementation:** `ptui/src/core/types.lisp`

**Evidence:**

* API exports match spec: inspect `defpackage :ptui.core.types` in `ptui/src/core/types.lisp`
* Runtime sanity via load:

```bash
./ptui/bin/check-systems.sh
```

**Status:** PASS.

---

## 04) Diagnostics (`src/util/log.lisp` + stats struct)

**Spec:** logging macros + `ptui.util.time`, plus `render-stats` struct, and per-frame debug log keys.

**Implementation:**

* Logging: `ptui/src/util/log.lisp`
* Time: `ptui/src/util/time.lisp`
* Frame debug logging in engine loop: `ptui/src/engine/loop.lisp`

**Evidence:**

```bash
PTUI_LOG_LEVEL=debug PTUI_EXIT_AFTER_MS=250 ./ptui/dist/metrics-dashboard 2>&1 | head
```

**Status:** PASS.

---

## 05) Concurrency primitives (`src/runtime/queue.lisp`)

**Spec:** safe queue with `queue-push` and `queue-pop-all`.

**Implementation:** `ptui/src/runtime/queue.lisp` (mutex-protected list)

**Evidence:**

```bash
./ptui/bin/check-systems.sh
```

**Status:** PASS (simple, serialized correctness).

---

## 06) Task scheduling (`src/runtime/scheduler.lisp`)

**Spec:** interval + once scheduling, cancellation, next-timeout calculation, run-due execution.

**Implementation:** `ptui/src/runtime/scheduler.lisp`

**Evidence:**

* Engine uses scheduler: `ptui/src/engine/loop.lisp`
* Gate checks scheduler hooks: `ptui/bin/compliance-gate.sh`

```bash
./ptui/bin/compliance-gate.sh
```

**Status:** PASS.

---

## 07) Terminal capability probe (`src/term/caps.lisp`)

**Spec:** env-based caps with truecolor heuristic via `COLORTERM`.

**Implementation:** `ptui/src/term/caps.lisp`

**Evidence:**

```bash
./ptui/bin/check-systems.sh
```

**Status:** PASS.

---

## 08) Raw TTY mode — C layer (`native/*` + `src/term/tty.lisp`)

**Spec:** stable C shim API and CL wrapper (`with-raw-tty`, size, read-bytes, fds).

**Implementation:**

* C shim: `ptui/native/ptui_native.c`, `ptui/native/ptui_native.h`
* CL wrapper: `ptui/src/term/tty.lisp`

**Evidence:**

```bash
./ptui/bin/build.sh
./ptui/bin/compliance-gate.sh
```

**Status:** PASS (raw mode + restore is smoke-tested).

---

## 09) Signal handling (`src/term/signals.lisp`)

**Spec:** use signalfd or self-pipe approach; produce resize + ctrl-c clean shutdown.

**Implementation:**

* C self-pipe handler: `ptui/native/ptui_native.c` (pipe + SIGWINCH/SIGINT/SIGTERM)
* CL wrapper: `ptui/src/term/signals.lisp`
* Gate PTY smoke covers SIGINT restore: `ptui/bin/compliance-gate.sh`

**Evidence:**

```bash
./ptui/bin/compliance-gate.sh
```

**Status:** PASS (signal-driven exit + restore gated).

---

## 10) Input decoder (`src/term/input.lisp`)

**Spec:** incremental parser; arrow keys, ctrl-c, UTF-8 printable text; stable events.

**Implementation:**

* Parser: `ptui/src/term/input.lisp`
* Events: `ptui/src/core/events.lisp`

**Evidence:**

* Covered indirectly by running the example; explicit parser regression tests will be added in `ptui/bin/test.sh`.

**Status:** PASS (functional), but needs regression tests (planned).

---

## 11) Backend protocol (`src/backend/protocol.lisp`)

**Spec:** CLOS boundary between engine and terminal backend (init/shutdown/poll/size/commit).

**Implementation:** `ptui/src/backend/protocol.lisp`

**Evidence:**

```bash
./ptui/bin/check-systems.sh
```

**Status:** PASS.

---

## 12) CellBuffer renderer (`src/render/buffer.lisp`)

**Spec:** mutate cell buffers only (no ANSI), clipped drawing, border/text primitives.

**Implementation:** `ptui/src/render/buffer.lisp`

**Evidence:**

```bash
./ptui/bin/build.sh
PTUI_EXIT_AFTER_MS=500 ./ptui/dist/metrics-dashboard
```

**Status:** PASS.

---

## 13) Diff engine (`src/render/diff.lisp`)

**Spec:** draw-ops (`:move`, `:style`, `:write`, `:clear-eol`, cursor/alt toggles), minimal updates, full-redraw threshold.

**Implementation:** `ptui/src/render/diff.lisp`

**Evidence:**

```bash
./ptui/bin/compliance-gate.sh
```

**Status:** PASS (practical), with one deviation:

* Deviation: an internal `:clear-screen` draw-op exists and is emitted on full redraw. The spec describes ED/EL clears as backend responsibilities; this is a small extension that should either be documented as an internal lowering or reconciled with the spec wording.

---

## 14) ANSI backend (`src/backend/ansi.lisp`)

**Spec:** implement protocol; translate draw-ops into ANSI (CUP/SGR/ED/EL/alt/cursor).

**Implementation:** `ptui/src/backend/ansi.lisp`

**Evidence:**

```bash
./ptui/bin/build.sh
./ptui/bin/compliance-gate.sh
```

**Status:** PASS.

---

## 15) ncurses backend (optional) (`src/backend/ncurses.lisp`)

**Spec:** optional compat backend; degraded color; translate draw-ops to ncurses calls; example runs with `:backend :ncurses` when `:ptui-ncurses` enabled.

**Implementation:** `ptui/src/backend/ncurses.lisp` exists, but is currently a compatibility shim that delegates to ANSI.

**Evidence:**

* Not enabled by default; no hard gate today.

**Status:** FAIL (optional) relative to the spec’s “translate to ncurses calls” requirement.

---

## 16) Color policy module (`src/core/color.lisp`)

**Spec:** represent `:default` and RGB colors; resolve mode from caps; generate SGR fragments.

**Implementation:** `ptui/src/core/color.lisp`

**Evidence:**

```bash
./ptui/bin/build.sh
PTUI_EXIT_AFTER_MS=250 ./ptui/dist/metrics-dashboard
```

**Status:** PASS.
