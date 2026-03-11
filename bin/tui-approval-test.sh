#!/usr/bin/env bash
# tui-approval-test.sh — Interactive tmux tests for the approval dialog UX
#
# Tests the full approval dialog lifecycle: rendering, key handling,
# approve/deny shortcuts, multi-tool sequential approval, and edge cases.
# Uses --demo mode with "approve" and "multi" keywords.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
BINARY="$REPO_ROOT/dist/amoebum"
SESSION="amoebum-approval-test-$$"
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
        echo "  --- captured pane (last 20 lines) ---"
        echo "$haystack" | tail -20
        echo "  --- end ---"
        FAILED=$((FAILED + 1))
    fi
}

assert_not_contains() {
    local label="$1" needle="$2" haystack="$3"
    if echo "$haystack" | grep -qiF "$needle"; then
        echo "  FAIL: $label (unexpected '$needle' found)"
        echo "  --- captured pane (last 20 lines) ---"
        echo "$haystack" | tail -20
        echo "  --- end ---"
        FAILED=$((FAILED + 1))
    else
        echo "  PASS: $label ('$needle' correctly absent)"
        PASSED=$((PASSED + 1))
    fi
}

wait_for_text() {
    local needle="$1" timeout="${2:-10}" elapsed=0
    while [ "$elapsed" -lt "$timeout" ]; do
        if capture_pane | grep -qiF "$needle"; then
            return 0
        fi
        sleep 0.5
        elapsed=$((elapsed + 1))
    done
    return 1
}

wait_for_text_gone() {
    local needle="$1" timeout="${2:-10}" elapsed=0
    while [ "$elapsed" -lt "$timeout" ]; do
        if ! capture_pane | grep -qiF "$needle"; then
            return 0
        fi
        sleep 0.5
        elapsed=$((elapsed + 1))
    done
    return 1
}

start_session() {
    tmux kill-session -t "$SESSION" 2>/dev/null || true
    sleep 0.5
    tmux new-session -d -s "$SESSION" -x 120 -y 40 "$BINARY --demo"
    sleep 3
}

end_session() {
    tmux kill-session -t "$SESSION" 2>/dev/null || true
    sleep 1
}

# --- preconditions ---

[ -x "$BINARY" ] || die "Binary not found or not executable: $BINARY"
command -v tmux >/dev/null 2>&1 || die "tmux is not installed"

echo "=== TUI Approval Dialog Tests ==="
echo "Binary: $BINARY"
echo "Session: $SESSION"
echo ""

# ============================================================
# Test 1: Approval dialog appears with correct content
# ============================================================
echo "Test 1: Approval dialog renders with tool name and options"

start_session
watch_pause

tmux send-keys -t "$SESSION" "approve" Enter
wait_for_text "Tool approval required" 12 || true
watch_pause

CONTENT="$(capture_pane)"
assert_contains "dialog title shown"    "Tool approval required"    "$CONTENT"
assert_contains "tool name shown"       "read-file"                 "$CONTENT"
assert_contains "reason shown"          "sensitive"                 "$CONTENT"
assert_contains "approve option shown"  "[y] Approve"              "$CONTENT"
assert_contains "deny option shown"     "[n] Deny"                 "$CONTENT"
assert_contains "tool preview shown"    "TOOL>"                    "$CONTENT"

# Key leak check: the prompt should NOT have any text in it
# (the 'approve' input was submitted, not leaked)
assert_not_contains "no key leak to prompt" "The user typed" "$CONTENT"

end_session

# ============================================================
# Test 2: Approve with 'y' shortcut — tool proceeds
# ============================================================
echo ""
echo "Test 2: Approve tool with 'y' shortcut"

start_session
watch_pause

tmux send-keys -t "$SESSION" "approve" Enter
wait_for_text "Tool approval required" 12 || true
watch_pause

# Press 'y' to approve
tmux send-keys -t "$SESSION" "y"
wait_for_text "Tool approved" 10 || true
watch_pause

CONTENT="$(capture_pane)"
assert_contains "tool was approved"      "Tool approved"        "$CONTENT"
assert_contains "simulated result"       "simulated"            "$CONTENT"
assert_not_contains "no key leak"        "The user typed"       "$CONTENT"

# Dialog should be gone
assert_not_contains "dialog dismissed" "Tool approval required" "$CONTENT"

end_session

# ============================================================
# Test 3: Deny with 'n' shortcut — tool denied
# ============================================================
echo ""
echo "Test 3: Deny tool with 'n' shortcut"

start_session
watch_pause

tmux send-keys -t "$SESSION" "approve" Enter
wait_for_text "Tool approval required" 12 || true
watch_pause

# Press 'n' to deny
tmux send-keys -t "$SESSION" "n"
wait_for_text "denied" 10 || true
watch_pause

CONTENT="$(capture_pane)"
assert_contains "tool was denied"        "denied"               "$CONTENT"
assert_not_contains "dialog dismissed"   "Tool approval required" "$CONTENT"

end_session

# ============================================================
# Test 4: Deny with Escape key
# ============================================================
echo ""
echo "Test 4: Deny tool with Escape key"

start_session
watch_pause

tmux send-keys -t "$SESSION" "approve" Enter
wait_for_text "Tool approval required" 12 || true
watch_pause

# Press Escape to deny
tmux send-keys -t "$SESSION" Escape
wait_for_text "denied" 10 || true
watch_pause

CONTENT="$(capture_pane)"
assert_contains "escape denied tool"     "denied"               "$CONTENT"

end_session

# ============================================================
# Test 5: Approve with Enter (default selection is Approve)
# ============================================================
echo ""
echo "Test 5: Approve tool with Enter (default selection)"

start_session
watch_pause

tmux send-keys -t "$SESSION" "approve" Enter
wait_for_text "Tool approval required" 12 || true
watch_pause

# Press Enter to confirm default (Approve)
tmux send-keys -t "$SESSION" Enter
wait_for_text "Tool approved" 10 || true
watch_pause

CONTENT="$(capture_pane)"
assert_contains "enter approved tool"    "Tool approved"        "$CONTENT"

end_session

# ============================================================
# Test 6: Navigate with arrow keys then confirm
# ============================================================
echo ""
echo "Test 6: Arrow key navigation + Enter"

start_session
watch_pause

tmux send-keys -t "$SESSION" "approve" Enter
wait_for_text "Tool approval required" 12 || true
watch_pause

# Press Down to move to Deny, then Enter
tmux send-keys -t "$SESSION" Down
sleep 0.3
tmux send-keys -t "$SESSION" Enter
wait_for_text "denied" 10 || true
watch_pause

CONTENT="$(capture_pane)"
assert_contains "arrow+enter denied"    "denied"                "$CONTENT"

end_session

# ============================================================
# Test 7: Key leak prevention — no text goes to prompt
# ============================================================
echo ""
echo "Test 7: Keys don't leak to prompt while dialog is active"

start_session
watch_pause

tmux send-keys -t "$SESSION" "approve" Enter
wait_for_text "Tool approval required" 12 || true
watch_pause

# Type some random keys before approving
tmux send-keys -t "$SESSION" "x"
sleep 0.2
tmux send-keys -t "$SESSION" "z"
sleep 0.2
tmux send-keys -t "$SESSION" "y"  # this approves
wait_for_text "Tool approved" 10 || true
wait_for_text_gone "Tool approval required" 5 || true
sleep 1
watch_pause

CONTENT="$(capture_pane)"
assert_contains "approved after random keys" "Tool approved"    "$CONTENT"
# 'x' and 'z' should not have leaked to the prompt
assert_not_contains "no x leak"  "xz"                           "$CONTENT"

end_session

# ============================================================
# Test 8: Multi-tool sequential approval (approve both)
# ============================================================
echo ""
echo "Test 8: Multi-tool sequential approval (approve, approve)"

start_session
watch_pause

tmux send-keys -t "$SESSION" "multi" Enter
wait_for_text "Tool approval required" 12 || true
watch_pause

# Approve first tool
CONTENT="$(capture_pane)"
assert_contains "first tool name"  "bash-exec"  "$CONTENT"

tmux send-keys -t "$SESSION" "y"
wait_for_text "First tool approved" 10 || true

# Second approval should appear
wait_for_text "Tool approval required" 12 || true
watch_pause

CONTENT="$(capture_pane)"
assert_contains "second tool name"  "read-file"  "$CONTENT"

# Approve second tool
tmux send-keys -t "$SESSION" "y"
wait_for_text "Second tool approved" 10 || true
watch_pause

CONTENT="$(capture_pane)"
assert_contains "both approved"  "Done!"  "$CONTENT"

end_session

# ============================================================
# Test 9: Multi-tool sequential approval (deny first, approve second)
# ============================================================
echo ""
echo "Test 9: Multi-tool sequential approval (deny, approve)"

start_session
watch_pause

tmux send-keys -t "$SESSION" "multi" Enter
wait_for_text "Tool approval required" 12 || true
watch_pause

# Deny first tool
tmux send-keys -t "$SESSION" "n"
wait_for_text "First tool denied" 10 || true

# Second approval should appear
wait_for_text "Tool approval required" 12 || true

# Approve second tool
tmux send-keys -t "$SESSION" "y"
wait_for_text "Second tool approved" 10 || true
watch_pause

CONTENT="$(capture_pane)"
assert_contains "mixed decisions"  "Done!"  "$CONTENT"

end_session

# ============================================================
# Test 10: Streaming hint visible during active stream
# ============================================================
echo ""
echo "Test 10: Streaming hint shows during active stream"

start_session
watch_pause

tmux send-keys -t "$SESSION" "approve" Enter
# Check for streaming hint while stream is active (before approval)
wait_for_text "Streaming" 8 || true
watch_pause

CONTENT="$(capture_pane)"
assert_contains "streaming hint visible"  "Streaming"  "$CONTENT"

# Clean up by approving
tmux send-keys -t "$SESSION" "y"
sleep 2

end_session

# ============================================================
# Summary
# ============================================================
echo ""
TOTAL=$((PASSED + FAILED))
echo "TUI_APPROVAL_TEST passed=$PASSED failed=$FAILED total=$TOTAL"

if [ "$FAILED" -gt 0 ]; then
    exit 1
fi
