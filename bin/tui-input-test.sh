#!/usr/bin/env bash
# tui-input-test.sh — Interactive tmux tests for prompt editing, scrolling,
# stream cancellation, history search, and input navigation.
#
# Uses --demo mode to avoid API key requirements.
# Tests verify observable text in captured tmux panes.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
BINARY="$REPO_ROOT/dist/amoebum"
SESSION="amoebum-input-test-$$"
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

capture_prompt_area() {
    # Capture only the last 4 lines of the pane (prompt box + status bar)
    # to avoid false matches against message history
    tmux capture-pane -t "$SESSION" -p | tail -4
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

echo "=== TUI Input & Navigation Tests ==="
echo "Binary: $BINARY"
echo "Session: $SESSION"
echo ""

# ============================================================
# Group 1: Prompt Text Input & Display
# ============================================================

# --- Test 1: Type text into prompt ---
echo "Test 1: Type text into prompt"

start_session
watch_pause

tmux send-keys -t "$SESSION" "hello world"
sleep 0.5
watch_pause

CONTENT="$(capture_pane)"
assert_contains "typed text visible" "hello world" "$CONTENT"

end_session

# --- Test 2: Backspace deletes text ---
echo ""
echo "Test 2: Backspace deletes text"

start_session
watch_pause

tmux send-keys -t "$SESSION" "hello"
sleep 0.3
tmux send-keys -t "$SESSION" BSpace
sleep 0.2
tmux send-keys -t "$SESSION" BSpace
sleep 0.3
watch_pause

CONTENT="$(capture_prompt_area)"
assert_contains "remaining text after backspace" "hel" "$CONTENT"
assert_not_contains "deleted chars gone" "hello" "$CONTENT"

end_session

# --- Test 3: Delete key (forward delete) ---
echo ""
echo "Test 3: Delete key (forward delete)"

start_session
watch_pause

tmux send-keys -t "$SESSION" "hello"
sleep 0.3
# Move cursor to start with Ctrl-A (Home)
tmux send-keys -t "$SESSION" C-a
sleep 0.2
tmux send-keys -t "$SESSION" DC  # Delete key
sleep 0.2
tmux send-keys -t "$SESSION" DC
sleep 0.3
watch_pause

CONTENT="$(capture_prompt_area)"
assert_contains "remaining text after delete" "llo" "$CONTENT"
assert_not_contains "deleted chars gone" "hello" "$CONTENT"

end_session

# ============================================================
# Group 2: Cursor Movement & Positional Editing
# ============================================================

# --- Test 4: Ctrl-A (Home) + type inserts at beginning ---
echo ""
echo "Test 4: Ctrl-A (Home) + type inserts at beginning"

start_session
watch_pause

tmux send-keys -t "$SESSION" "world"
sleep 0.3
tmux send-keys -t "$SESSION" C-a
sleep 0.2
tmux send-keys -t "$SESSION" "hello "
sleep 0.3
watch_pause

CONTENT="$(capture_pane)"
assert_contains "text inserted at beginning" "hello world" "$CONTENT"

end_session

# --- Test 5: Ctrl-E (End) + type appends ---
echo ""
echo "Test 5: Ctrl-E (End) + type appends"

start_session
watch_pause

tmux send-keys -t "$SESSION" "hello"
sleep 0.3
tmux send-keys -t "$SESSION" C-a
sleep 0.2
tmux send-keys -t "$SESSION" C-e
sleep 0.2
tmux send-keys -t "$SESSION" " world"
sleep 0.3
watch_pause

CONTENT="$(capture_pane)"
assert_contains "text appended at end" "hello world" "$CONTENT"

end_session

# --- Test 6: Left/Right arrow navigation ---
echo ""
echo "Test 6: Left/Right arrow navigation"

start_session
watch_pause

tmux send-keys -t "$SESSION" "helo"
sleep 0.3
tmux send-keys -t "$SESSION" Left
sleep 0.2
tmux send-keys -t "$SESSION" "l"
sleep 0.3
watch_pause

CONTENT="$(capture_pane)"
assert_contains "char inserted via arrow nav" "hello" "$CONTENT"

end_session

# --- Test 7: Ctrl-Left/Ctrl-Right word jump ---
echo ""
echo "Test 7: Ctrl-Left/Ctrl-Right word jump"

start_session
watch_pause

tmux send-keys -t "$SESSION" "one two three"
sleep 0.3
# Ctrl-Left jumps back one word at a time; send twice
# tmux sends Ctrl-Left as the escape sequence for word-left
tmux send-keys -t "$SESSION" C-Left
sleep 0.2
tmux send-keys -t "$SESSION" C-Left
sleep 0.2
tmux send-keys -t "$SESSION" "X"
sleep 0.3
watch_pause

CONTENT="$(capture_pane)"
assert_contains "word jump insert" "Xtwo" "$CONTENT"

end_session

# ============================================================
# Group 3: Line Editing Operations
# ============================================================

# --- Test 8: Ctrl-W (delete word backward) ---
echo ""
echo "Test 8: Ctrl-W (delete word backward)"

start_session
watch_pause

tmux send-keys -t "$SESSION" "hello world"
sleep 0.3
tmux send-keys -t "$SESSION" C-w
sleep 0.3
watch_pause

PROMPT="$(capture_prompt_area)"
assert_contains "remaining word after ctrl-w" "hello" "$PROMPT"
assert_not_contains "deleted word gone" "world" "$PROMPT"

end_session

# --- Test 9: Ctrl-U (kill to start from end) ---
echo ""
echo "Test 9: Ctrl-U (kill to start from end)"

start_session
watch_pause

tmux send-keys -t "$SESSION" "hello world"
sleep 0.3
tmux send-keys -t "$SESSION" C-u
sleep 0.3
watch_pause

PROMPT="$(capture_prompt_area)"
assert_not_contains "text killed by ctrl-u" "hello" "$PROMPT"

end_session

# --- Test 10: Ctrl-K (kill to end from start) ---
echo ""
echo "Test 10: Ctrl-K (kill to end from start)"

start_session
watch_pause

tmux send-keys -t "$SESSION" "hello world"
sleep 0.3
tmux send-keys -t "$SESSION" C-a
sleep 0.2
tmux send-keys -t "$SESSION" C-k
sleep 0.3
watch_pause

PROMPT="$(capture_prompt_area)"
assert_not_contains "text killed by ctrl-k" "hello" "$PROMPT"

end_session

# --- Test 11: Ctrl-U partial (from mid-text) ---
echo ""
echo "Test 11: Ctrl-U partial (from mid-text)"

start_session
watch_pause

tmux send-keys -t "$SESSION" "hello world"
sleep 0.3
# Jump back one word (cursor before "world")
tmux send-keys -t "$SESSION" C-Left
sleep 0.2
tmux send-keys -t "$SESSION" C-u
sleep 0.3
watch_pause

PROMPT="$(capture_prompt_area)"
assert_contains "remaining text after partial ctrl-u" "world" "$PROMPT"
assert_not_contains "killed prefix gone" "hello" "$PROMPT"

end_session

# ============================================================
# Group 4: Multi-line Input
# ============================================================

# --- Test 12: Ctrl-J inserts newline (multi-line prompt) ---
echo ""
echo "Test 12: Ctrl-J inserts newline (multi-line prompt)"

start_session
watch_pause

tmux send-keys -t "$SESSION" "line one"
sleep 0.3
tmux send-keys -t "$SESSION" C-j
sleep 0.2
tmux send-keys -t "$SESSION" "line two"
sleep 0.3
watch_pause

# The prompt box has 1 visible content row (fixed 3 = 2 border + 1 content),
# so "line one" scrolls out when "line two" is typed on a new line.
# Verifying "line two" appears proves Ctrl-J inserted a newline.
CONTENT="$(capture_pane)"
assert_contains "second line visible after newline" "line two" "$CONTENT"

end_session

# --- Test 13: Submit multi-line input with Enter ---
echo ""
echo "Test 13: Submit multi-line input with Enter"

start_session
watch_pause

tmux send-keys -t "$SESSION" "first"
sleep 0.2
tmux send-keys -t "$SESSION" C-j
sleep 0.2
tmux send-keys -t "$SESSION" "second"
sleep 0.3
tmux send-keys -t "$SESSION" Enter
wait_for_text "Demo Response" 10 || true
watch_pause

CONTENT="$(capture_pane)"
assert_contains "multi-line input submitted" "first" "$CONTENT"

end_session

# ============================================================
# Group 5: Message Scrolling
# ============================================================

# --- Test 14: Long response — default at bottom ---
echo ""
echo "Test 14: Long response — default at bottom"

start_session
watch_pause

tmux send-keys -t "$SESSION" "long" Enter
# wait_for_text timeout is in 0.5s iterations, so 80 = 40 seconds
# The long response has 20 sections × ~50 words × 0.02s/word ≈ 20s
wait_for_text "Section 20" 80 || true
# Wait for streaming to finish
wait_for_text_gone "Streaming" 40 || true
sleep 1
watch_pause

CONTENT="$(capture_pane)"
assert_contains "latest section visible at bottom" "Section 20" "$CONTENT"

# --- Test 15: PgUp scrolls to earlier content ---
echo ""
echo "Test 15: PgUp scrolls to earlier content"

# Send PgUp multiple times to scroll up
for i in $(seq 1 10); do
    tmux send-keys -t "$SESSION" PPage
    sleep 0.2
done
sleep 0.5
watch_pause

CONTENT="$(capture_pane)"
assert_contains "early section visible after pgup" "Section 1" "$CONTENT"

# --- Test 16: PgDn scrolls back to bottom ---
echo ""
echo "Test 16: PgDn scrolls back to bottom"

# Send PgDn many times to scroll back down
for i in $(seq 1 20); do
    tmux send-keys -t "$SESSION" NPage
    sleep 0.2
done
sleep 0.5
watch_pause

CONTENT="$(capture_pane)"
assert_contains "back at bottom after pgdn" "Section 20" "$CONTENT"

end_session

# ============================================================
# Group 6: Stream Cancellation
# ============================================================

# --- Test 17: Escape cancels active stream ---
echo ""
echo "Test 17: Escape cancels active stream"

start_session
watch_pause

tmux send-keys -t "$SESSION" "long" Enter
wait_for_text "Streaming" 8 || true
watch_pause

tmux send-keys -t "$SESSION" Escape
wait_for_text_gone "Streaming" 10 || true
sleep 0.5
watch_pause

CONTENT="$(capture_pane)"
assert_not_contains "streaming hint gone after escape" "Streaming" "$CONTENT"
assert_contains "partial response preserved" "Long Response" "$CONTENT"

end_session

# --- Test 18: Prompt usable after stream cancellation ---
echo ""
echo "Test 18: Prompt usable after stream cancellation"

start_session
watch_pause

tmux send-keys -t "$SESSION" "long" Enter
wait_for_text "Streaming" 8 || true

tmux send-keys -t "$SESSION" Escape
wait_for_text_gone "Streaming" 10 || true
sleep 0.5

# Type into prompt after cancellation
tmux send-keys -t "$SESSION" "follow up"
sleep 0.5
watch_pause

CONTENT="$(capture_pane)"
assert_contains "can type after cancellation" "follow up" "$CONTENT"

end_session

# ============================================================
# Group 7: History Search / Fuzzy Picker
# ============================================================

# --- Test 19: Ctrl-R activates fuzzy picker ---
echo ""
echo "Test 19: Ctrl-R activates fuzzy picker"

start_session
watch_pause

# Build some history
tmux send-keys -t "$SESSION" "hello" Enter
wait_for_text "Demo Response" 10 || true
sleep 1
tmux send-keys -t "$SESSION" "code" Enter
wait_for_text "fibonacci" 10 || true
sleep 1

# Activate history search
tmux send-keys -t "$SESSION" C-r
sleep 0.5
watch_pause

CONTENT="$(capture_pane)"
assert_contains "history picker activated" "history" "$CONTENT"

# --- Test 20: Escape closes fuzzy picker ---
echo ""
echo "Test 20: Escape closes fuzzy picker"

tmux send-keys -t "$SESSION" Escape
sleep 0.5
watch_pause

CONTENT="$(capture_pane)"
assert_not_contains "picker dismissed" "history:" "$CONTENT"

# --- Test 21: Type to filter history ---
echo ""
echo "Test 21: Type to filter history"

tmux send-keys -t "$SESSION" C-r
sleep 0.5
tmux send-keys -t "$SESSION" "hello"
sleep 0.5
watch_pause

CONTENT="$(capture_pane)"
assert_contains "filter query or matched entry visible" "hello" "$CONTENT"

# --- Test 22: Enter selects from picker ---
echo ""
echo "Test 22: Enter selects from picker"

tmux send-keys -t "$SESSION" Enter
sleep 0.5
watch_pause

CONTENT="$(capture_pane)"
assert_not_contains "picker closed after selection" "history:" "$CONTENT"

end_session

# ============================================================
# Group 8: Tab Completion
# ============================================================

# --- Test 23: Tab completes slash commands ---
echo ""
echo "Test 23: Tab completes slash commands"

start_session
watch_pause

tmux send-keys -t "$SESSION" "/he"
sleep 0.3
tmux send-keys -t "$SESSION" Tab
sleep 0.5
watch_pause

CONTENT="$(capture_pane)"
assert_contains "tab completed to /help" "/help" "$CONTENT"

end_session

# ============================================================
# Group 9: Input Submission
# ============================================================

# --- Test 24: Enter submits and clears prompt ---
echo ""
echo "Test 24: Enter submits and clears prompt"

start_session
watch_pause

tmux send-keys -t "$SESSION" "hello" Enter
wait_for_text "Demo Response" 20 || true
# Wait for streaming to finish before checking prompt
wait_for_text_gone "Streaming" 20 || true
sleep 1
watch_pause

PROMPT="$(capture_prompt_area)"
assert_not_contains "prompt cleared after submit" "hello" "$PROMPT"

# --- Test 25: Second message after first ---
echo ""
echo "Test 25: Second message after first"

tmux send-keys -t "$SESSION" "code" Enter
wait_for_text "fibonacci" 20 || true
wait_for_text_gone "Streaming" 20 || true
sleep 1
watch_pause

CONTENT="$(capture_pane)"
assert_contains "second response rendered" "fibonacci" "$CONTENT"

end_session

# ============================================================
# Summary
# ============================================================
echo ""
TOTAL=$((PASSED + FAILED))
echo "TUI_INPUT_TEST passed=$PASSED failed=$FAILED total=$TOTAL"

if [ "$FAILED" -gt 0 ]; then
    exit 1
fi
