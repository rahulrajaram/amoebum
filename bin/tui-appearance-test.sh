#!/usr/bin/env bash
# tui-appearance-test.sh — Visual regression tests for the amoebum TUI
#
# Captures ANSI snapshots and pixel screenshots of the TUI in demo mode,
# then compares against reference files.
#
# Usage:
#   ./bin/tui-appearance-test.sh              # compare against references
#   ./bin/tui-appearance-test.sh --update      # save current output as new references
#   ./bin/tui-appearance-test.sh --watch       # pause for manual tmux attach
#
# Reference files are stored in tests/tui-appearance/references/
# Current captures go to tests/tui-appearance/current/

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
BINARY="$REPO_ROOT/dist/amoebum"
SESSION="amoebum-appearance-$$"
REF_DIR="$REPO_ROOT/tests/tui-appearance/references"
CUR_DIR="$REPO_ROOT/tests/tui-appearance/current"
DIFF_DIR="$REPO_ROOT/tests/tui-appearance/diffs"
RENDER_SCRIPT="$SCRIPT_DIR/ansi-to-png.py"

PASSED=0
FAILED=0
UPDATE=false
WATCH=false

for arg in "$@"; do
    case "$arg" in
        --update) UPDATE=true ;;
        --watch)  WATCH=true ;;
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

capture_ansi() {
    # Capture with ANSI escape codes preserved (-e flag)
    tmux capture-pane -t "$SESSION" -p -e -S -100
}

capture_plain() {
    tmux capture-pane -t "$SESSION" -p -S -100
}

wait_for_text() {
    local needle="$1" timeout="${2:-8}" elapsed=0
    while [ "$elapsed" -lt "$timeout" ]; do
        if capture_plain | grep -qiF "$needle"; then
            return 0
        fi
        sleep 0.5
        elapsed=$((elapsed + 1))
    done
    return 1
}

save_snapshot() {
    local name="$1" dest_dir="$2"
    # Save ANSI snapshot
    capture_ansi > "$dest_dir/${name}.ansi"
    # Save plain text snapshot
    capture_plain > "$dest_dir/${name}.txt"
    # Render to PNG if the helper exists
    if [ -f "$RENDER_SCRIPT" ]; then
        python3 "$RENDER_SCRIPT" "$dest_dir/${name}.ansi" "$dest_dir/${name}.png"
    fi
}

normalize_snapshot() {
    # Strip volatile content: status bar stats (tok/s, token counts), trailing blanks
    sed \
        -e 's/Tokens: [0-9]*/Tokens: N/g' \
        -e 's/([0-9]*%)/(__%)/g' \
        -e 's/stream done/stream N tok\/s/g' \
        -e 's/stream [0-9.]* tok\/s/stream N tok\/s/g' \
        -e :a -e '/^[[:space:]]*$/{ $d; N; ba; }' \
        "$1"
}

compare_snapshot() {
    local name="$1" label="$2" fuzzy="${3:-false}"

    if [ ! -f "$REF_DIR/${name}.txt" ]; then
        echo "  SKIP: $label (no reference file — run with --update first)"
        return
    fi

    # --- Text comparison ---
    local ref_txt="$REF_DIR/${name}.txt"
    local cur_txt="$CUR_DIR/${name}.txt"

    if [ "$fuzzy" = "true" ]; then
        # Fuzzy mode: only check that key structural elements exist, not exact viewport
        # The long response scrolls, so the visible window varies between runs
        local fuzzy_pass=true
        for needle in "Section" "Lorem ipsum" "Item" "stream"; do
            if ! grep -qiF "$needle" "$cur_txt"; then
                echo "  FAIL: $label — fuzzy text check missing '$needle'"
                fuzzy_pass=false
                FAILED=$((FAILED + 1))
                break
            fi
        done
        if [ "$fuzzy_pass" = "true" ]; then
            echo "  PASS: $label — fuzzy text check (key elements present)"
            PASSED=$((PASSED + 1))
        fi
    else
        local ref_content cur_content
        ref_content="$(normalize_snapshot "$ref_txt")"
        cur_content="$(normalize_snapshot "$cur_txt")"

        if [ "$ref_content" = "$cur_content" ]; then
            echo "  PASS: $label — text snapshot matches reference"
            PASSED=$((PASSED + 1))
        else
            echo "  FAIL: $label — text snapshot differs from reference"
            diff --color=auto -u <(echo "$ref_content") <(echo "$cur_content") | head -40 || true
            diff -u <(echo "$ref_content") <(echo "$cur_content") > "$DIFF_DIR/${name}.txt.diff" 2>/dev/null || true
            FAILED=$((FAILED + 1))
        fi
    fi

    # --- Pixel diff (if both PNGs exist and ImageMagick is available) ---
    # Mask the bottom status bar (last 2 rows = ~32px) which contains volatile stats
    local ref_png="$REF_DIR/${name}.png"
    local cur_png="$CUR_DIR/${name}.png"
    local pixel_threshold=500
    if [ "$fuzzy" = "true" ]; then
        pixel_threshold=5000  # scroll position may shift a few rows between runs
    fi

    if [ -f "$ref_png" ] && [ -f "$cur_png" ] && command -v compare >/dev/null 2>&1; then
        local diff_png="$DIFF_DIR/${name}.diff.png"
        local ref_cropped="$DIFF_DIR/${name}.ref-cropped.png"
        local cur_cropped="$DIFF_DIR/${name}.cur-cropped.png"
        convert "$ref_png" -gravity South -chop 0x32 "$ref_cropped"
        convert "$cur_png" -gravity South -chop 0x32 "$cur_cropped"

        local metric
        metric="$(compare -metric AE "$ref_cropped" "$cur_cropped" "$diff_png" 2>&1 || true)"
        rm -f "$ref_cropped" "$cur_cropped"

        if [ "$metric" -le "$pixel_threshold" ] 2>/dev/null; then
            echo "  PASS: $label — pixel screenshot matches (${metric} pixels within threshold)"
            PASSED=$((PASSED + 1))
        else
            echo "  FAIL: $label — pixel screenshot differs ($metric pixels changed, threshold=$pixel_threshold)"
            echo "        diff image: $diff_png"
            FAILED=$((FAILED + 1))
        fi
    elif [ -f "$ref_png" ] && [ ! -f "$cur_png" ]; then
        echo "  SKIP: $label — pixel comparison skipped (current PNG not generated)"
    fi
}

# --- preconditions ---

[ -x "$BINARY" ] || die "Binary not found or not executable: $BINARY"
command -v tmux >/dev/null 2>&1 || die "tmux is not installed"

mkdir -p "$REF_DIR" "$CUR_DIR" "$DIFF_DIR"

echo "=== TUI Appearance Test ==="
echo "Binary: $BINARY"
if $UPDATE; then
    echo "Mode: UPDATE (saving new reference snapshots)"
else
    echo "Mode: COMPARE (checking against references)"
fi
echo ""

# ============================================================
# Scenario definitions
# ============================================================

run_scenario() {
    local name="$1" input="$2" wait_text="$3" label="$4" fuzzy="${5:-false}"

    echo "Scenario: $label"

    tmux new-session -d -s "$SESSION" -x 120 -y 40 "$BINARY --demo"
    sleep 3
    watch_pause

    tmux send-keys -t "$SESSION" "$input" Enter
    wait_for_text "$wait_text" 12 || true
    # Extra settle time for streaming + rendering to fully complete
    sleep 3
    watch_pause

    local dest_dir="$CUR_DIR"
    if $UPDATE; then
        dest_dir="$REF_DIR"
    fi
    save_snapshot "$name" "$dest_dir"

    if ! $UPDATE; then
        # Also save to current for diffing
        if [ "$dest_dir" != "$CUR_DIR" ]; then
            save_snapshot "$name" "$CUR_DIR"
        fi
        compare_snapshot "$name" "$label" "$fuzzy"
    else
        echo "  SAVED: reference snapshot $dest_dir/${name}.{ansi,txt,png}"
    fi

    tmux kill-session -t "$SESSION" 2>/dev/null || true
    sleep 1
}

# --- Scenario: Empty startup screen ---
echo "Scenario: Startup screen (no input)"
tmux new-session -d -s "$SESSION" -x 120 -y 40 "$BINARY --demo"
sleep 3
watch_pause

DEST="$CUR_DIR"
if $UPDATE; then DEST="$REF_DIR"; fi
save_snapshot "startup" "$DEST"

if ! $UPDATE; then
    if [ "$DEST" != "$CUR_DIR" ]; then save_snapshot "startup" "$CUR_DIR"; fi
    compare_snapshot "startup" "Startup screen"
else
    echo "  SAVED: reference snapshot $DEST/startup.{ansi,txt,png}"
fi

tmux kill-session -t "$SESSION" 2>/dev/null || true
sleep 1

# --- Scenario: Default demo response ---
# Wait for the last phrase in the default demo response
run_scenario "demo-default" "hello" "other response types" "Demo default response"

# --- Scenario: Code block response ---
# Wait for the last phrase in the code demo response
run_scenario "demo-code" "code" "distinct background" "Demo code block response"

# --- Scenario: Tool call response ---
# Wait for the tail of the tool response
run_scenario "demo-tool" "tool" "simulated" "Demo tool call response"

# --- Scenario: Error response ---
# Error mode triggers stream failure immediately
run_scenario "demo-error" "error" "stream failed" "Demo error response"

# --- Scenario: Long response (scroll test) ---
# Fuzzy match — scroll viewport varies between runs
run_scenario "demo-long" "long" "Item C in section 20" "Demo long response (scroll)" "true"

# ============================================================
# Summary
# ============================================================
echo ""
if $UPDATE; then
    echo "Reference snapshots updated in: $REF_DIR"
    ls -la "$REF_DIR"/ 2>/dev/null
else
    TOTAL=$((PASSED + FAILED))
    echo "TUI_APPEARANCE_TEST passed=$PASSED failed=$FAILED total=$TOTAL"
    if [ "$FAILED" -gt 0 ]; then
        echo "Diff files saved in: $DIFF_DIR"
        exit 1
    fi
fi
