#!/usr/bin/env bash
# worktree-cleanup-test.sh — Smoke test for worktree-cleanup.sh
#
# Runs worktree-cleanup.sh in --dry-run mode and asserts:
#   - exit code 0
#   - output contains the summary line
#   - no worktrees were actually removed (count unchanged)

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
CLEANUP="$SCRIPT_DIR/worktree-cleanup.sh"

if [ ! -x "$CLEANUP" ]; then
  echo "worktree-cleanup-test: $CLEANUP is not executable" >&2
  exit 1
fi

REPO_ROOT="$(git rev-parse --show-toplevel)"
before_count="$(git -C "$REPO_ROOT" worktree list --porcelain | grep -c '^worktree ' || true)"

tmpout="$(mktemp)"
trap 'rm -f "$tmpout"' EXIT

set +e
"$CLEANUP" --dry-run >"$tmpout" 2>&1
rc=$?
set -e

if [ "$rc" -ne 0 ]; then
  echo "FAIL: worktree-cleanup.sh --dry-run exited with $rc" >&2
  cat "$tmpout" >&2
  exit 1
fi

if ! grep -q '^worktree-cleanup summary:' "$tmpout"; then
  echo "FAIL: summary line missing from worktree-cleanup.sh output" >&2
  cat "$tmpout" >&2
  exit 1
fi

if ! grep -q 'mode=dry-run' "$tmpout"; then
  echo "FAIL: dry-run mode not reflected in summary line" >&2
  cat "$tmpout" >&2
  exit 1
fi

after_count="$(git -C "$REPO_ROOT" worktree list --porcelain | grep -c '^worktree ' || true)"
if [ "$before_count" != "$after_count" ]; then
  echo "FAIL: worktree count changed during dry-run ($before_count -> $after_count)" >&2
  exit 1
fi

echo "PASS: worktree-cleanup.sh --dry-run exited 0 with summary (worktrees=$before_count)"
echo "---- captured output ----"
cat "$tmpout"
exit 0
