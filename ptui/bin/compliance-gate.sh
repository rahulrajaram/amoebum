#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
if [[ "$(basename "${ROOT_DIR}")" != "ptui" ]]; then
  echo "Expected script to live under ptui/bin (found root: ${ROOT_DIR})" >&2
  exit 1
fi

REPO_DIR="$(cd "${ROOT_DIR}/.." && pwd)"

fail() {
  echo "COMPLIANCE_FAIL: $*" >&2
  exit 1
}

require_cmd() {
  local cmd="$1"
  command -v "${cmd}" >/dev/null 2>&1 || fail "missing required command: ${cmd}"
}

required_files=(
  "ptui.asd"
  "ptui-examples.asd"
  "bin/test.sh"
  "test/run.lisp"
  "src/core/types.lisp"
  "src/core/color.lisp"
  "src/core/events.lisp"
  "src/util/log.lisp"
  "src/util/time.lisp"
  "src/runtime/queue.lisp"
  "src/runtime/scheduler.lisp"
  "src/term/caps.lisp"
  "src/term/tty.lisp"
  "src/term/signals.lisp"
  "src/term/input.lisp"
  "src/render/buffer.lisp"
  "src/render/diff.lisp"
  "src/backend/protocol.lisp"
  "src/backend/ansi.lisp"
  "src/engine/loop.lisp"
  "native/ptui_native.h"
  "native/ptui_native.c"
  "examples/metrics-dashboard.lisp"
  "bin/build.sh"
)

for file in "${required_files[@]}"; do
  if [[ ! -f "${ROOT_DIR}/${file}" ]]; then
    fail "missing required file: ${file}"
  fi
done

if ! rg -n '^\(defun run \(render-fn' "${ROOT_DIR}/src/engine/loop.lisp" >/dev/null; then
  fail "run signature mismatch: expected (defun run (render-fn &key ...))"
fi

if ! rg -n 'ptui\.runtime\.scheduler:schedule-interval' "${ROOT_DIR}/src/engine/loop.lisp" >/dev/null; then
  fail "scheduler integration missing: schedule-interval not used in engine loop"
fi

if ! rg -n 'ptui\.runtime\.scheduler:scheduler-run-due' "${ROOT_DIR}/src/engine/loop.lisp" >/dev/null; then
  fail "scheduler integration missing: scheduler-run-due not used in engine loop"
fi

require_cmd rg
require_cmd script
require_cmd stty
require_cmd sbcl

# Modularity: each ASDF system must load independently.
"${ROOT_DIR}/bin/check-systems.sh" >/dev/null

# Kernel regression tests.
"${ROOT_DIR}/bin/test.sh" >/dev/null

# Activity 1 closure: renderer emits clear-screen op and ANSI backend maps it to ESC[2J.
rg -n 'make-draw-op :clear-screen' "${ROOT_DIR}/src/render/diff.lisp" >/dev/null \
  || fail "activity-1 missing clear-screen draw-op emission"
rg -n ':clear-screen.*2J' "${ROOT_DIR}/src/backend/ansi.lisp" >/dev/null \
  || fail "activity-1 missing ANSI clear-screen escape handling"

"${ROOT_DIR}/bin/build.sh"

TMP_STDOUT="$(mktemp)"
TMP_STDERR="$(mktemp)"
TMP_CAPTURE="$(mktemp)"
TMP_SBCL_ERR="$(mktemp)"
TMP_PTY_TIMED="$(mktemp)"
TMP_PTY_SIGINT="$(mktemp)"
trap 'rm -f "${TMP_STDOUT}" "${TMP_STDERR}" "${TMP_CAPTURE}" "${TMP_SBCL_ERR}" "${TMP_PTY_TIMED}" "${TMP_PTY_SIGINT}"' EXIT

if ! (
  cd "${ROOT_DIR}"
  PTUI_EXIT_AFTER_MS=250 ./dist/metrics-dashboard >"${TMP_STDOUT}" 2>"${TMP_STDERR}"
); then
  echo "smoke run failed" >&2
  sed -n '1,120p' "${TMP_STDERR}" >&2
  fail "timed smoke failed"
fi

# Activity 1 evidence smoke: force at least one full redraw and assert ESC[2J.
if ! (
  cd "${ROOT_DIR}"
  PTUI_EXIT_AFTER_MS=120 ./dist/metrics-dashboard >"${TMP_CAPTURE}" 2>/dev/null
); then
  fail "activity-1 capture run failed"
fi
if ! rg --text -n $'\x1b\\[2J' "${TMP_CAPTURE}" >/dev/null; then
  fail "activity-1 clear-screen smoke missing ESC[2J"
fi

# Activity 2 closure: env-controlled max idle sleep is wired.
rg -n 'PTUI_MAX_IDLE_SLEEP_MS' "${ROOT_DIR}/src/engine/loop.lisp" >/dev/null \
  || fail "activity-2 missing PTUI_MAX_IDLE_SLEEP_MS support"

# Activity 2 evidence: timed PTY exit preserves terminal usability.
if ! script -q -c "cd '${ROOT_DIR}' && PTUI_EXIT_AFTER_MS=120 ./dist/metrics-dashboard; rc=\$?; stty -a >/dev/null; stty_ok=\$?; printf 'TIMED_EXIT_RC=%s STTY_OK=%s\n' \"\$rc\" \"\$([[ \$stty_ok -eq 0 ]] && echo yes || echo no)\"" "${TMP_PTY_TIMED}" >/dev/null 2>&1; then
  fail "activity-2 timed PTY smoke harness failed"
fi
rg -n 'TIMED_EXIT_RC=0 STTY_OK=yes' "${TMP_PTY_TIMED}" >/dev/null \
  || fail "activity-2 timed PTY smoke did not confirm clean exit/stty"

# Activity 2 evidence: injected INT path exits cleanly and preserves terminal.
if ! script -q -c "cd '${ROOT_DIR}' && ( ./dist/metrics-dashboard & pid=\$!; sleep 0.15; kill -INT \$pid; wait \$pid; rc=\$?; stty -a >/dev/null; stty_ok=\$?; printf 'SIGINT_EXIT_RC=%s STTY_OK=%s\n' \"\$rc\" \"\$([[ \$stty_ok -eq 0 ]] && echo yes || echo no)\" )" "${TMP_PTY_SIGINT}" >/dev/null 2>&1; then
  fail "activity-2 SIGINT PTY smoke harness failed"
fi
rg -n 'SIGINT_EXIT_RC=0 STTY_OK=yes' "${TMP_PTY_SIGINT}" >/dev/null \
  || fail "activity-2 SIGINT PTY smoke did not confirm clean exit/stty"

# Activity 3 closure: accept either successful ncurses feature path OR blocker evidence.
if (
  cd "${ROOT_DIR}"
  sbcl --non-interactive \
    --eval '(require :asdf)' \
    --eval "(load \"${ROOT_DIR}/.tools/quicklisp/setup.lisp\")" \
    --eval '(pushnew :ptui-ncurses *features*)' \
    --eval "(asdf:load-asd \"${ROOT_DIR}/ptui.asd\")" \
    --eval '(asdf:load-system :ptui)'
) > /dev/null 2>"${TMP_SBCL_ERR}"; then
  # Implemented path: ncurses-enabled build loads successfully.
  rg -n 'defmethod ptui\.backend\.protocol:backend-commit' "${ROOT_DIR}/src/backend/ncurses.lisp" >/dev/null \
    || fail "activity-3 ncurses load passed but backend-commit method missing"
else
  # Blocker path: require explicit, reproducible blocker evidence.
  BLOCKER_FILE="${REPO_DIR}/.agent/ncurses-backend-blocker.md"
  [[ -f "${BLOCKER_FILE}" ]] || fail "activity-3 blocker evidence missing (.agent/ncurses-backend-blocker.md)"
  rg -n 'Activity 3 Blocker|Exit code: `1`|Command:' "${BLOCKER_FILE}" >/dev/null \
    || fail "activity-3 blocker file missing required structure"
  if rg -n 'cl-charms|Component ".*" not found|Missing dependency' "${TMP_SBCL_ERR}" >/dev/null; then
    rg -n 'cl-charms|Component ".*" not found|Missing dependency' "${BLOCKER_FILE}" >/dev/null \
      || fail "activity-3 blocker file missing matching dependency-failure evidence"
  fi
fi

echo "COMPLIANCE_OK"
