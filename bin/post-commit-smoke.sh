#!/usr/bin/env bash
# post-commit-smoke.sh — smoke test for .githooks/post-commit (NXT-276)
#
# Verifies:
#   1. The hook file exists and is executable.
#   2. Dry-run mode (CULTIVAR_POST_COMMIT_DRY_RUN=1) prints the intended
#      cultivar command and exits 0 without spawning a background process.
#   3. A real invocation backgrounds the cultivar index build and returns
#      quickly (so commit latency is unchanged). The background process
#      writes to .agent/cultivar-index/post-commit.log.
#   4. An unavailable explicit CULTIVAR_BIN records visible skip evidence
#      instead of silently no-oping.
#
# Usage:
#   ./bin/post-commit-smoke.sh

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
HOOK="$REPO_ROOT/.githooks/post-commit"
INDEX_DIR="$REPO_ROOT/.agent/cultivar-index"
LOG_FILE="$INDEX_DIR/post-commit.log"
FALLBACK_CULTIVAR_BIN="/home/rahul/Documents/cultivar/target/release/cultivar"

pass() { echo "  PASS: $*"; }
fail() { echo "  FAIL: $*" >&2; exit 1; }

resolve_expected_cultivar_bin() {
  if [ -n "${CULTIVAR_BIN:-}" ]; then
    if [ -x "$CULTIVAR_BIN" ]; then
      printf '%s\n' "$CULTIVAR_BIN"
    fi
    return 0
  fi

  local path_bin
  path_bin="$(command -v cultivar 2>/dev/null || true)"
  if [ -n "$path_bin" ] && [ -x "$path_bin" ]; then
    printf '%s\n' "$path_bin"
    return 0
  fi

  if [ -x "$FALLBACK_CULTIVAR_BIN" ]; then
    printf '%s\n' "$FALLBACK_CULTIVAR_BIN"
  fi
}

echo "[smoke] post-commit hook: $HOOK"

# (a) Executable
if [ ! -f "$HOOK" ]; then
  fail "hook file missing"
fi
if [ ! -x "$HOOK" ]; then
  fail "hook file is not executable"
fi
pass "hook file exists and is executable"

# (b) Dry run
echo "[smoke] running hook in dry-run mode"
DRY_OUT="$(CULTIVAR_POST_COMMIT_DRY_RUN=1 _CULTIVAR_POST_COMMIT_GUARD= "$HOOK" 2>&1)"
DRY_EC=$?
if [ $DRY_EC -ne 0 ]; then
  echo "$DRY_OUT"
  fail "dry run exited with code $DRY_EC"
fi
echo "$DRY_OUT" | sed 's/^/    | /'
if ! echo "$DRY_OUT" | grep -q "DRY RUN"; then
  fail "dry run did not print a DRY RUN banner"
fi
if ! echo "$DRY_OUT" | grep -q "cultivar"; then
  fail "dry run did not reference cultivar command"
fi
if ! echo "$DRY_OUT" | grep -q -- "--languages"; then
  fail "dry run missing --languages flag"
fi
if ! echo "$DRY_OUT" | grep -q "commonlisp"; then
  fail "dry run missing commonlisp language"
fi
EXPECTED_BIN="$(resolve_expected_cultivar_bin)"
if [ -n "$EXPECTED_BIN" ] && ! echo "$DRY_OUT" | grep -q "$EXPECTED_BIN"; then
  fail "dry run did not use resolved cultivar binary: $EXPECTED_BIN"
fi
pass "dry run prints intended command"

# (c) Real invocation should return quickly (< 3s) and kick off background
echo "[smoke] timing real invocation (expect fast return, background reindex)"
mkdir -p "$INDEX_DIR"
BEFORE_SIZE=0
if [ -f "$LOG_FILE" ]; then
  BEFORE_SIZE=$(stat -c%s "$LOG_FILE" 2>/dev/null || echo 0)
fi

START_NS=$(date +%s%N)
_CULTIVAR_POST_COMMIT_GUARD= "$HOOK"
REAL_EC=$?
END_NS=$(date +%s%N)
ELAPSED_MS=$(( (END_NS - START_NS) / 1000000 ))

if [ $REAL_EC -ne 0 ]; then
  fail "real invocation exited with code $REAL_EC"
fi
pass "real invocation exited 0 (elapsed ${ELAPSED_MS}ms)"

if [ "$ELAPSED_MS" -gt 3000 ]; then
  fail "real invocation took ${ELAPSED_MS}ms (>3000ms); likely not backgrounded"
fi
pass "real invocation returned quickly (< 3000ms)"

# Give the background process a moment to write its start marker.
sleep 1
if [ -f "$LOG_FILE" ]; then
  AFTER_SIZE=$(stat -c%s "$LOG_FILE" 2>/dev/null || echo 0)
  if [ "$AFTER_SIZE" -gt "$BEFORE_SIZE" ]; then
    pass "background process wrote to $LOG_FILE (size ${BEFORE_SIZE} -> ${AFTER_SIZE})"
  else
    # Not fatal: cultivar may be missing on this host, in which case the hook
    # returns 0 after recording skip evidence. That is a valid path.
    if [ -z "$(resolve_expected_cultivar_bin)" ]; then
      pass "cultivar binary absent — hook correctly skipped"
    else
      echo "  WARN: log file did not grow; background reindex may not have started yet" >&2
    fi
  fi
else
  pass "no log file yet (cultivar binary likely absent)"
fi

# (d) Explicitly unavailable binary should leave visible skip evidence.
echo "[smoke] verifying missing-binary skip evidence"
SKIP_BEFORE_SIZE=0
if [ -f "$LOG_FILE" ]; then
  SKIP_BEFORE_SIZE=$(stat -c%s "$LOG_FILE" 2>/dev/null || echo 0)
fi
MISSING_BIN="$REPO_ROOT/.agent/missing-cultivar-for-post-commit-smoke"
CULTIVAR_BIN="$MISSING_BIN" _CULTIVAR_POST_COMMIT_GUARD= "$HOOK"
SKIP_AFTER_SIZE=$(stat -c%s "$LOG_FILE" 2>/dev/null || echo 0)
if [ "$SKIP_AFTER_SIZE" -le "$SKIP_BEFORE_SIZE" ]; then
  fail "missing-binary path did not write skip evidence to $LOG_FILE"
fi
if ! tail -n 8 "$LOG_FILE" | grep -q "post-commit reindex skipped"; then
  fail "skip evidence missing skipped marker"
fi
if ! tail -n 8 "$LOG_FILE" | grep -q "not executable"; then
  fail "skip evidence missing not-executable reason"
fi
pass "missing-binary path records visible skip evidence"

echo "[smoke] all checks passed"
exit 0
