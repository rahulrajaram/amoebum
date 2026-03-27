#!/usr/bin/env bash
# capture-screenshots.sh — Framework for capturing reproducible screenshots of PTUI demo widgets
#
# The PTUI demo widgets (metrics-dashboard, ops-wallboard, release-tracker) run inside a
# real terminal and paint ANSI escape sequences to the TTY.  An actual screenshot requires
# either:
#
#   a) A terminal emulator that supports scripted screenshot export (e.g. xterm with
#      `xwd`, kitty `+kitten icat`, or ghostty's `--screenshot` flag), OR
#   b) A VTE-based harness (e.g. headless VTE + gdk-pixbuf), OR
#   c) A tmux pane captured with `tmux capture-pane -p` (text only, no color rendering).
#
# This script documents the recommended approach for each demo, sets up the correct
# terminal geometry, and shows placeholder commands that must be adapted to the
# screenshot tool available on the host.
#
# Usage:
#   ./ptui/bin/capture-screenshots.sh [--demo metrics-dashboard|ops-wallboard|release-tracker]
#   ./ptui/bin/capture-screenshots.sh --all
#   ./ptui/bin/capture-screenshots.sh --text-only   # tmux pane-capture (no color, always works)

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PTUI_DIR="$(cd "${SCRIPT_DIR}/.." && pwd)"
REPO_DIR="$(cd "${PTUI_DIR}/.." && pwd)"
DIST_DIR="${PTUI_DIR}/dist"
SCREENSHOT_DIR="${PTUI_DIR}/docs/screenshots"

DEMO="all"
TEXT_ONLY=false

while [[ $# -gt 0 ]]; do
    case "$1" in
        --demo) DEMO="$2"; shift 2 ;;
        --all)  DEMO="all"; shift ;;
        --text-only) TEXT_ONLY=true; shift ;;
        *) echo "Unknown arg: $1" >&2; exit 1 ;;
    esac
done

fail() { echo "ERROR: $*" >&2; exit 1; }
info() { echo "  $*"; }

# ---------------------------------------------------------------------------
# Preconditions
# ---------------------------------------------------------------------------

if [[ ! -f "${DIST_DIR}/metrics-dashboard" ]]; then
    fail "Binary not found: ${DIST_DIR}/metrics-dashboard — run 'make build' or './ptui/bin/build.sh' first"
fi

mkdir -p "${SCREENSHOT_DIR}"

# ---------------------------------------------------------------------------
# Terminal geometry for each demo
# ---------------------------------------------------------------------------
#
#   metrics-dashboard  — designed for 160x48 (wide, data-dense grid)
#   ops-wallboard      — designed for 200x56 (extra wide, multi-column ops view)
#   release-tracker    — designed for 140x44 (standard wide, timeline layout)
#
declare -A DEMO_COLS=([metrics-dashboard]=160 [ops-wallboard]=200 [release-tracker]=140)
declare -A DEMO_ROWS=([metrics-dashboard]=48  [ops-wallboard]=56  [release-tracker]=44)
declare -A DEMO_EXIT_MS=([metrics-dashboard]=2000 [ops-wallboard]=2500 [release-tracker]=2000)

# ---------------------------------------------------------------------------
# Text-only capture via tmux (always available, no color)
# ---------------------------------------------------------------------------

capture_text_via_tmux() {
    local demo="$1"
    local cols="${DEMO_COLS[$demo]}"
    local rows="${DEMO_ROWS[$demo]}"
    local exit_ms="${DEMO_EXIT_MS[$demo]}"
    local out="${SCREENSHOT_DIR}/${demo}-text.txt"
    local session="ptui-screenshot-${demo}-$$"

    command -v tmux >/dev/null 2>&1 || fail "tmux is required for --text-only capture"

    echo "Capturing text snapshot: ${demo} (${cols}x${rows})"

    # Launch demo in detached tmux pane at the correct geometry
    tmux new-session -d -s "${session}" -x "${cols}" -y "${rows}" \
        "PTUI_EXIT_AFTER_MS=${exit_ms} COLORTERM=truecolor TERM=xterm-256color ${DIST_DIR}/metrics-dashboard"

    # Wait for the exit-after-ms timer to fire
    local wait_s=$(( (exit_ms / 1000) + 2 ))
    sleep "${wait_s}"

    # Capture the pane content (text only — no ANSI codes)
    tmux capture-pane -t "${session}" -p -S 0 > "${out}" 2>/dev/null || true
    tmux kill-session -t "${session}" 2>/dev/null || true

    info "Text snapshot saved: ${out}"
}

# ---------------------------------------------------------------------------
# Full-color screenshot — PLACEHOLDER
#
# Replace the body of this function with the tool appropriate for your host:
#
#   Option A — kitty terminal:
#     kitty +kitten icat --transfer-mode=file --stdout "${binary}" \
#       | convert - "${out}"
#     (kitty must be the parent terminal)
#
#   Option B — xterm + xwd:
#     DISPLAY=:0 xterm -geometry "${cols}x${rows}" -e "${binary}" &
#     sleep 2
#     DISPLAY=:0 xwd -root -silent | convert xwd:- "${out}"
#     (requires X11 display)
#
#   Option C — ghostty (if it adds --screenshot):
#     ghostty --screenshot="${out}" -- "${binary}"
#
#   Option D — headless Xvfb + ImageMagick import:
#     Xvfb :99 -screen 0 1920x1080x24 &
#     DISPLAY=:99 xterm -geometry "${cols}x${rows}" -e "${binary}" &
#     sleep 2
#     DISPLAY=:99 import -window root "${out}"
# ---------------------------------------------------------------------------

capture_screenshot() {
    local demo="$1"
    local cols="${DEMO_COLS[$demo]}"
    local rows="${DEMO_ROWS[$demo]}"
    local exit_ms="${DEMO_EXIT_MS[$demo]}"
    local out="${SCREENSHOT_DIR}/${demo}.png"

    echo "Screenshot placeholder: ${demo} (${cols}x${rows})"
    info "Output would be: ${out}"
    info ""
    info "Geometry: ${cols} columns x ${rows} rows"
    info "Exit-after: ${exit_ms}ms"
    info ""
    info "To capture a real screenshot, choose one of the methods described in"
    info "the capture_screenshot() function body in this script and replace the"
    info "placeholder block below."
    info ""

    # --- PLACEHOLDER: replace with your screenshot tool ---
    # Example (xvfb + xterm + ImageMagick):
    #
    #   local xvfb_display=":$(( RANDOM % 200 + 100 ))"
    #   Xvfb "${xvfb_display}" -screen 0 1920x1200x24 &
    #   local xvfb_pid=$!
    #   DISPLAY="${xvfb_display}" xterm -fa Mono -fs 10 \
    #       -geometry "${cols}x${rows}" \
    #       -e "PTUI_EXIT_AFTER_MS=${exit_ms} COLORTERM=truecolor \
    #           TERM=xterm-256color ${DIST_DIR}/metrics-dashboard" &
    #   sleep $(( (exit_ms / 1000) + 1 ))
    #   DISPLAY="${xvfb_display}" import -window root "${out}"
    #   kill "${xvfb_pid}" 2>/dev/null || true
    #
    # --- END PLACEHOLDER ---

    info "(Placeholder run — no image written)"
}

# ---------------------------------------------------------------------------
# Main dispatch
# ---------------------------------------------------------------------------

DEMOS=()
if [[ "${DEMO}" == "all" ]]; then
    DEMOS=(metrics-dashboard ops-wallboard release-tracker)
else
    DEMOS=("${DEMO}")
fi

echo "=== PTUI Screenshot Capture ==="
echo "Screenshot dir: ${SCREENSHOT_DIR}"
echo "Text-only mode: ${TEXT_ONLY}"
echo ""

for d in "${DEMOS[@]}"; do
    if [[ -z "${DEMO_COLS[$d]+x}" ]]; then
        fail "Unknown demo: '${d}'.  Valid: metrics-dashboard, ops-wallboard, release-tracker"
    fi

    if "${TEXT_ONLY}"; then
        capture_text_via_tmux "${d}"
    else
        capture_screenshot "${d}"
    fi
    echo ""
done

echo "Done."
echo ""
echo "To get real color screenshots, open this script and replace the PLACEHOLDER"
echo "block in capture_screenshot() with the tool available on your host."
echo "See the comments inside the function for four ready-to-adapt options."
