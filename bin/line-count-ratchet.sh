#!/usr/bin/env bash
#
# line-count-ratchet.sh
#
# Fails (exit 1) when any tracked file grows past its baseline. The baseline
# for each file is "current line count at the time NXT-595 landed + 25 lines
# of headroom for legitimate growth before the ratchet trips".
#
# Raise a baseline only by editing bin/line-count-overrides.txt — never by
# editing this script. That file is the audit trail for "why did we let this
# file grow?"; the script holds only the original NXT-595 baselines.
#
# Usage:  bin/line-count-ratchet.sh
# Exit:   0 if all tracked files are within baseline, 1 otherwise.

set -euo pipefail

# Resolve repo root regardless of where the script is invoked from.
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
OVERRIDES_FILE="$REPO_ROOT/bin/line-count-overrides.txt"

# Hardcoded NXT-595 baselines (current_lines + 25). Do NOT edit these — use
# the overrides file instead.
TRACKED_FILES=(
    "amoebum/src/ui/panels/chat-panel.lisp"
    "amoebum/src/ui/yaml-theme-loader.lisp"
    "amoebum/src/ui/yaml-theme-layout.lisp"
    "amoebum/src/ui/chat-state.lisp"
)
HARDCODED_BASELINES=(
    343
    609
    248
    427
)

# Load overrides into parallel arrays. Format per line:
#   <relative-path> <new-baseline>
# Lines beginning with '#' (after optional whitespace) and blank lines are
# ignored.
OVERRIDE_PATHS=()
OVERRIDE_VALUES=()
if [[ -f "$OVERRIDES_FILE" ]]; then
    while IFS= read -r raw_line || [[ -n "$raw_line" ]]; do
        # Strip leading whitespace.
        trimmed="${raw_line#"${raw_line%%[![:space:]]*}"}"
        # Skip blanks and comments.
        if [[ -z "$trimmed" || "${trimmed:0:1}" == "#" ]]; then
            continue
        fi
        # Split on whitespace into path + baseline.
        # shellcheck disable=SC2206
        parts=( $trimmed )
        if [[ "${#parts[@]}" -lt 2 ]]; then
            echo "WARN: malformed override line ignored: $raw_line" >&2
            continue
        fi
        OVERRIDE_PATHS+=("${parts[0]}")
        OVERRIDE_VALUES+=("${parts[1]}")
    done < "$OVERRIDES_FILE"
fi

# Look up an override for a given relative path; echoes the baseline or empty.
lookup_override() {
    local needle="$1"
    local i
    for i in "${!OVERRIDE_PATHS[@]}"; do
        if [[ "${OVERRIDE_PATHS[$i]}" == "$needle" ]]; then
            echo "${OVERRIDE_VALUES[$i]}"
            return 0
        fi
    done
    echo ""
}

failures=0
echo "line-count-ratchet: checking ${#TRACKED_FILES[@]} tracked files"
echo "----------------------------------------------------------------"

for i in "${!TRACKED_FILES[@]}"; do
    rel_path="${TRACKED_FILES[$i]}"
    hardcoded="${HARDCODED_BASELINES[$i]}"
    abs_path="$REPO_ROOT/$rel_path"

    if [[ ! -f "$abs_path" ]]; then
        echo "FAIL: $rel_path is missing (expected to exist)"
        failures=$((failures + 1))
        continue
    fi

    current=$(wc -l < "$abs_path" | tr -d ' ')
    override="$(lookup_override "$rel_path")"
    if [[ -n "$override" ]]; then
        baseline="$override"
        baseline_source="override"
    else
        baseline="$hardcoded"
        baseline_source="hardcoded"
    fi

    if (( current > baseline )); then
        delta=$((current - baseline))
        echo "FAIL: $rel_path is $current lines (baseline $baseline [$baseline_source], +$delta over)"
        failures=$((failures + 1))
    else
        headroom=$((baseline - current))
        echo "OK:   $rel_path -> $current / $baseline lines [$baseline_source] (headroom: $headroom)"
    fi
done

echo "----------------------------------------------------------------"

if (( failures > 0 )); then
    echo ""
    echo "$failures file(s) over baseline."
    echo "To raise a baseline deliberately, edit bin/line-count-overrides.txt"
    exit 1
fi

echo "OK: ${#TRACKED_FILES[@]} tracked files within baseline"
exit 0
