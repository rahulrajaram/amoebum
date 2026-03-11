#!/usr/bin/env bash
# tui-interactive-test.sh — Automated interactive TUI end-to-end test via tmux
#
# Uses tmux to launch dist/amoebum --demo, send keystrokes, and verify
# output without requiring an API key.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
BINARY="$REPO_ROOT/dist/amoebum"
SESSION="amoebum-tui-test-$$"
PASSED=0
FAILED=0
WATCH=false

for arg in "$@"; do
    case "$arg" in
        --watch) WATCH=true ;;
    esac
done

cleanup() {
    tmux kill-session -t "$SESSION" 2>/dev/null || true
}
trap cleanup EXIT

die() { echo "FATAL: $1" >&2; exit 1; }

watch_pause() {
    if $WATCH; then
        echo ""
        echo "  >>> Attach in another terminal:  tmux attach -t $SESSION"
        echo "  >>> Press Enter here to continue..."
        read -r
    fi
}

# --- helpers ---

capture_pane() {
    tmux capture-pane -t "$SESSION" -p -S -100
}

assert_contains() {
    local label="$1" needle="$2" haystack="$3"
    if echo "$haystack" | grep -qiF "$needle"; then
        echo "  PASS: $label (found '$needle')"
        PASSED=$((PASSED + 1))
    else
        echo "  FAIL: $label (expected '$needle' not found)"
        echo "  --- captured pane ---"
        echo "$haystack" | head -30
        echo "  --- end ---"
        FAILED=$((FAILED + 1))
    fi
}

wait_for_text() {
    local needle="$1" timeout="${2:-8}" elapsed=0
    while [ "$elapsed" -lt "$timeout" ]; do
        if capture_pane | grep -qiF "$needle"; then
            return 0
        fi
        sleep 0.5
        elapsed=$((elapsed + 1))
    done
    return 1
}

# --- preconditions ---

[ -x "$BINARY" ] || die "Binary not found or not executable: $BINARY"
command -v tmux >/dev/null 2>&1 || die "tmux is not installed"

echo "=== TUI Interactive Test ==="
echo "Binary: $BINARY"
echo "Session: $SESSION"
echo ""

# ============================================================
# Test 1: Demo default — type "hello", verify "Demo Response"
# ============================================================
echo "Test 1: Demo default response"

tmux new-session -d -s "$SESSION" -x 120 -y 40 "$BINARY --demo"
sleep 3  # wait for TUI to initialize
watch_pause

tmux send-keys -t "$SESSION" "hello" Enter
wait_for_text "Demo Response" 10 || true
watch_pause

CONTENT="$(capture_pane)"
assert_contains "user input echoed"   "hello"         "$CONTENT"
assert_contains "demo response shown" "Demo Response"  "$CONTENT"

tmux kill-session -t "$SESSION" 2>/dev/null || true
sleep 1

# ============================================================
# Test 2: Demo code — type "code", verify "fibonacci"
# ============================================================
echo ""
echo "Test 2: Demo code response"

tmux new-session -d -s "$SESSION" -x 120 -y 40 "$BINARY --demo"
sleep 3
watch_pause

tmux send-keys -t "$SESSION" "code" Enter
wait_for_text "fibonacci" 10 || true
watch_pause

CONTENT="$(capture_pane)"
assert_contains "code keyword echoed" "code"       "$CONTENT"
assert_contains "fibonacci in output" "fibonacci"  "$CONTENT"

tmux kill-session -t "$SESSION" 2>/dev/null || true
sleep 1

# ============================================================
# Test 3: Demo tool — type "tool", verify tool call rendering
# ============================================================
echo ""
echo "Test 3: Demo tool response"

tmux new-session -d -s "$SESSION" -x 120 -y 40 "$BINARY --demo"
sleep 3
watch_pause

tmux send-keys -t "$SESSION" "tool" Enter
wait_for_text "simulated" 10 || true
watch_pause

CONTENT="$(capture_pane)"
assert_contains "tool keyword echoed"    "tool"        "$CONTENT"
assert_contains "tool call shown"        "read-file"   "$CONTENT"
assert_contains "tool result in output"  "simulated"   "$CONTENT"

tmux kill-session -t "$SESSION" 2>/dev/null || true
sleep 1

# ============================================================
# Test 4: Demo error — type "error", verify error rendering
# ============================================================
echo ""
echo "Test 4: Demo error response"

tmux new-session -d -s "$SESSION" -x 120 -y 40 "$BINARY --demo"
sleep 3
watch_pause

tmux send-keys -t "$SESSION" "error" Enter
wait_for_text "stream failed" 10 || true
watch_pause

CONTENT="$(capture_pane)"
assert_contains "error keyword echoed"   "error"          "$CONTENT"
assert_contains "error message shown"    "stream failed"  "$CONTENT"

tmux kill-session -t "$SESSION" 2>/dev/null || true
sleep 1

# ============================================================
# Test 5 (conditional): Real Kimi API
# ============================================================
KIMI_KEY=""
if [ -n "${MOONSHOT_API_KEY:-}" ]; then
    KIMI_KEY="$MOONSHOT_API_KEY"
elif [ -f "$HOME/.moonshotai" ]; then
    KIMI_KEY="$(cat "$HOME/.moonshotai" | tr -d '[:space:]')"
fi

if [ -n "$KIMI_KEY" ]; then
    echo ""
    echo "Test 5: Real Kimi API (MOONSHOT_API_KEY found)"

    tmux new-session -d -s "$SESSION" -x 120 -y 40 "$BINARY"
    sleep 3
    watch_pause

    tmux send-keys -t "$SESSION" "What is 2+2? Answer with just the number." Enter
    # Real API needs more time
    wait_for_text "4" 15 || true
    watch_pause

    CONTENT="$(capture_pane)"
    assert_contains "real API returns answer" "4" "$CONTENT"

    tmux kill-session -t "$SESSION" 2>/dev/null || true
else
    echo ""
    echo "Test 5: Skipped (no MOONSHOT_API_KEY or ~/.moonshotai found)"
fi

# ============================================================
# Summary
# ============================================================
echo ""
TOTAL=$((PASSED + FAILED))
echo "TUI_INTERACTIVE_TEST passed=$PASSED failed=$FAILED total=$TOTAL"

if [ "$FAILED" -gt 0 ]; then
    exit 1
fi
