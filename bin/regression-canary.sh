#!/usr/bin/env bash
# regression-canary.sh — sub-30-second pre-commit regression safety net
#
# PURPOSE
#   Fast canary that catches the most common agent-introduced regressions
#   before a commit lands. It loads the amoebum system and runs five
#   load-bearing FiveAM tests picked to cover the interaction surfaces
#   most often broken by automated edits:
#
#     1. chat-snapshot-message-area              (chat UI render path)
#     2. conversation-roundtrip-preserves-tool-call-ids
#                                                (conversation persistence)
#     3. make-amoebum-context-populates-required-slots
#                                                (pipeline context wiring)
#     4. i210-restarts-round-trip                (tool restart semantics)
#     5. cli-resume-by-id-is-deterministic       (session resume)
#
#   Plus three load-bearing PTUI FiveAM tests so that regressions in
#   PTUI's terminal-pane / chat rendering surfaces are caught at
#   pre-commit time (NXT-299):
#
#     6. render-diff-single-change-uses-minimal-ops
#                                                (differential rendering)
#     7. width-classifies-ascii-wide-and-combining
#                                                (text / grapheme width)
#     8. two-region-dock-fill-layout             (constraint layout)
#
#   The PTUI canary block can be skipped in emergencies by setting
#   SKIP_PTUI_CANARY=1 in the environment.
#
# USAGE
#   bin/regression-canary.sh
#
# WHEN AGENTS SHOULD RUN IT
#   - Immediately before attempting a commit when the change touches any
#     of: chat UI, session/conversation persistence, pipeline context,
#     tool restarts, or CLI session resume.
#   - As a cheap smoke pass after a large refactor to confirm nothing
#     load-bearing was broken before running the full suite.
#   - NOT a replacement for `make test`. It is a canary, not a full bar.
#
# EXIT CODES
#   0  all five canary tests passed
#   1  one or more canary tests failed, or SBCL/ASDF load failed
#   2  environment precondition missing (sbcl, quicklisp setup, etc.)
#
# RUNTIME TARGET
#   Under 30 seconds on a warm SBCL fasl cache. First run will be slower
#   while the amoebum system compiles.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

QUICKLISP_SETUP="${QUICKLISP_SETUP:-$HOME/quicklisp/setup.lisp}"
if [ -f "$REPO_ROOT/ptui/.tools/quicklisp/setup.lisp" ]; then
    QUICKLISP_SETUP="$REPO_ROOT/ptui/.tools/quicklisp/setup.lisp"
fi

die_env() { echo "regression-canary: $1" >&2; exit 2; }

command -v sbcl >/dev/null 2>&1 || die_env "sbcl not found on PATH"
[ -f "$QUICKLISP_SETUP" ] || die_env "quicklisp setup not found at $QUICKLISP_SETUP"
[ -f "$REPO_ROOT/amoebum/amoebum.asd" ] || die_env "amoebum.asd not found under $REPO_ROOT/amoebum"

SKIP_PTUI_CANARY="${SKIP_PTUI_CANARY:-0}"

echo "=== regression-canary ==="
echo "Repo:     $REPO_ROOT"
echo "Quicklisp: $QUICKLISP_SETUP"
if [ "$SKIP_PTUI_CANARY" = "1" ]; then
    echo "Running 5 load-bearing FiveAM tests (PTUI canary SKIPPED)..."
else
    echo "Running 5 amoebum + 3 ptui load-bearing FiveAM tests..."
fi
echo ""

START_TS=$(date +%s)

set +e
sbcl --noinform --non-interactive \
  --eval "(require :asdf)" \
  --eval "(let ((*compile-verbose* nil) (*load-verbose* nil)
                (*compile-print* nil)
                (quiet (make-broadcast-stream)))
            (let ((*standard-output* quiet) (*error-output* quiet)
                  (*trace-output* quiet))
              (handler-bind ((warning (lambda (c) (muffle-warning c))))
                (load \"$QUICKLISP_SETUP\")
                (setf asdf:*compile-file-warnings-behaviour* :ignore)
                (asdf:load-asd (truename \"$REPO_ROOT/pseudopod/pseudopod.asd\"))
                (asdf:load-asd (truename \"$REPO_ROOT/sw4rm-sdk/sw4rm-sdk.asd\"))
                (asdf:load-asd (truename \"$REPO_ROOT/ptui/ptui.asd\"))
                (asdf:load-asd (truename \"$REPO_ROOT/amoebum/amoebum.asd\"))
                (asdf:load-system :amoebum/test)
                (unless (equal (uiop:getenv \"SKIP_PTUI_CANARY\") \"1\")
                  (load (merge-pathnames \"ptui/test/render-test.lisp\"
                                         (truename \"$REPO_ROOT/\")))
                  (load (merge-pathnames \"ptui/test/text-test.lisp\"
                                         (truename \"$REPO_ROOT/\")))
                  (load (merge-pathnames \"ptui/test/constraints-test.lisp\"
                                         (truename \"$REPO_ROOT/\")))))))" \
  --eval "(let* ((amoebum-specs '((\"CHAT-SNAPSHOT-MESSAGE-AREA\"                   :amoebum/test)
                                  (\"CONVERSATION-ROUNDTRIP-PRESERVES-TOOL-CALL-IDS\" :amoebum/test)
                                  (\"MAKE-AMOEBUM-CONTEXT-POPULATES-REQUIRED-SLOTS\"  :amoebum/test)
                                  (\"I210-RESTARTS-ROUND-TRIP\"                      :amoebum/test)
                                  (\"CLI-RESUME-BY-ID-IS-DETERMINISTIC\"              :amoebum/test)))
                 (ptui-specs (unless (equal (uiop:getenv \"SKIP_PTUI_CANARY\") \"1\")
                               '((\"RENDER-DIFF-SINGLE-CHANGE-USES-MINIMAL-OPS\" :ptui.test.render)
                                 (\"WIDTH-CLASSIFIES-ASCII-WIDE-AND-COMBINING\"  :ptui.test.text)
                                 (\"TWO-REGION-DOCK-FILL-LAYOUT\"                :ptui.test.constraints))))
                 (specs (append amoebum-specs ptui-specs))
                 (missing nil)
                 (syms (loop for (n p) in specs
                             for pkg = (find-package p)
                             for s = (and pkg (find-symbol n pkg))
                             do (unless s (push (format nil \"~A::~A\" p n) missing))
                             when s collect s)))
            (when missing
              (format *error-output* \"~&regression-canary: missing test symbols: ~{~A~^, ~}~%\" (nreverse missing))
              (uiop:quit 1))
            (let* ((results (loop for s in syms append (fiveam:run s)))
                   (ok (every (lambda (r) (typep r 'fiveam::test-passed)) results))
                   (failed (remove-if (lambda (r) (typep r 'fiveam::test-passed)) results)))
              (fiveam:explain! results)
              (format t \"~&regression-canary: ran ~D test(s), ~D failure(s)~%\"
                      (length results) (length failed))
              (uiop:quit (if ok 0 1))))"
RC=$?
set -e

END_TS=$(date +%s)
ELAPSED=$((END_TS - START_TS))

echo ""
if [ "$RC" -eq 0 ]; then
    echo "regression-canary: PASS (${ELAPSED}s)"
else
    echo "regression-canary: FAIL rc=$RC (${ELAPSED}s)"
fi

exit "$RC"
