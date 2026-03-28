#!/usr/bin/env bash
# release-checklist.sh — PTUI release verification checklist
#
# Runs through the required verification steps before tagging a PTUI release:
#   1. Build the binary
#   2. Run ASDF system smoke tests (check-systems)
#   3. Run minimal example smoke tests (smoke-examples.sh)
#   4. Run kernel unit tests (test.sh)
#   5. Run the compliance gate (compliance-gate.sh)
#   6. Run the TUI performance regression test (if tui-perf-test.sh exists)
#   7. Verify required documentation files are present
#   8. Print a summary with PASS/FAIL per step
#
# Usage:
#   ./ptui/bin/release-checklist.sh
#   ./ptui/bin/release-checklist.sh --skip-perf     # skip the perf test (faster)
#   ./ptui/bin/release-checklist.sh --perf-prompts 3  # fewer prompts for perf test

set -uo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PTUI_DIR="$(cd "${SCRIPT_DIR}/.." && pwd)"
REPO_DIR="$(cd "${PTUI_DIR}/.." && pwd)"

SKIP_PERF=false
PERF_PROMPTS=3

while [[ $# -gt 0 ]]; do
    case "$1" in
        --skip-perf)      SKIP_PERF=true; shift ;;
        --perf-prompts)   PERF_PROMPTS="$2"; shift 2 ;;
        *) echo "Unknown argument: $1" >&2; exit 1 ;;
    esac
done

# ---------------------------------------------------------------------------
# Tracking
# ---------------------------------------------------------------------------

declare -a STEP_NAMES=()
declare -a STEP_STATUS=()   # "PASS" | "FAIL" | "SKIP"
declare -a STEP_NOTES=()

record() {
    local name="$1" status="$2" note="${3:-}"
    STEP_NAMES+=("$name")
    STEP_STATUS+=("$status")
    STEP_NOTES+=("$note")
}

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

HR="──────────────────────────────────────────────────────────────"

header() { echo ""; echo "${HR}"; echo "  $*"; echo "${HR}"; }
ok()     { echo "  [PASS] $*"; }
fail()   { echo "  [FAIL] $*"; }
skip()   { echo "  [SKIP] $*"; }
info()   { echo "         $*"; }

run_step() {
    local label="$1"
    shift
    header "Step: ${label}"
    if "$@"; then
        ok "${label}"
        record "${label}" "PASS"
    else
        fail "${label}"
        record "${label}" "FAIL"
    fi
}

# ---------------------------------------------------------------------------
# Step 1: Build
# ---------------------------------------------------------------------------

header "Step 1: Build"
if (cd "${PTUI_DIR}" && ./bin/build.sh); then
    ok "Build succeeded"
    record "Build (make build / build.sh)" "PASS"
else
    fail "Build failed"
    record "Build (make build / build.sh)" "FAIL"
    echo ""
    echo "Cannot continue — build must pass before other checks."
    echo ""
    # Still print a summary if there were earlier partial results.
fi

# ---------------------------------------------------------------------------
# Step 2: run make test-amoebum (repo-level test target)
# ---------------------------------------------------------------------------

header "Step 2: make test-amoebum"
if (cd "${REPO_DIR}" && timeout 300 make test-amoebum); then
    ok "make test-amoebum passed"
    record "make test-amoebum" "PASS"
else
    fail "make test-amoebum failed (exit code $?)"
    record "make test-amoebum" "FAIL"
fi

# ---------------------------------------------------------------------------
# Step 3: ASDF system smoke (check-systems.sh)
# ---------------------------------------------------------------------------

header "Step 3: ASDF system smoke (check-systems.sh)"
if (cd "${PTUI_DIR}" && ./bin/check-systems.sh); then
    ok "All ASDF systems load independently"
    record "ASDF system smoke (check-systems.sh)" "PASS"
else
    fail "check-systems.sh failed"
    record "ASDF system smoke (check-systems.sh)" "FAIL"
fi

# ---------------------------------------------------------------------------
# Step 4: Minimal example smoke tests (smoke-examples.sh)
# ---------------------------------------------------------------------------

header "Step 4: Minimal example smoke tests (smoke-examples.sh)"
if (cd "${PTUI_DIR}" && ./bin/smoke-examples.sh); then
    ok "Minimal examples smoke passed"
    record "Minimal example smoke (smoke-examples.sh)" "PASS"
else
    fail "Minimal examples smoke failed"
    record "Minimal example smoke (smoke-examples.sh)" "FAIL"
fi

# ---------------------------------------------------------------------------
# Step 5: Kernel unit tests (test.sh)
# ---------------------------------------------------------------------------

header "Step 5: Kernel unit tests (test.sh)"
if (cd "${PTUI_DIR}" && ./bin/test.sh); then
    ok "Kernel unit tests passed"
    record "Kernel unit tests (test.sh)" "PASS"
else
    fail "Kernel unit tests failed"
    record "Kernel unit tests (test.sh)" "FAIL"
fi

# ---------------------------------------------------------------------------
# Step 6: Compliance gate
# ---------------------------------------------------------------------------

header "Step 6: Compliance gate (compliance-gate.sh)"
if (cd "${PTUI_DIR}" && ./bin/compliance-gate.sh); then
    ok "Compliance gate passed"
    record "Compliance gate (compliance-gate.sh)" "PASS"
else
    fail "Compliance gate failed"
    record "Compliance gate (compliance-gate.sh)" "FAIL"
fi

# ---------------------------------------------------------------------------
# Step 7: TUI performance regression test
# ---------------------------------------------------------------------------

header "Step 7: TUI performance regression test"
PERF_SCRIPT="${REPO_DIR}/bin/tui-perf-test.sh"
if "${SKIP_PERF}"; then
    skip "Skipped (--skip-perf)"
    record "TUI perf regression (tui-perf-test.sh)" "SKIP" "--skip-perf"
elif [[ ! -x "${PERF_SCRIPT}" ]]; then
    skip "tui-perf-test.sh not found or not executable: ${PERF_SCRIPT}"
    record "TUI perf regression (tui-perf-test.sh)" "SKIP" "not found"
elif ! command -v tmux >/dev/null 2>&1; then
    skip "tmux not installed — cannot run perf test"
    record "TUI perf regression (tui-perf-test.sh)" "SKIP" "tmux missing"
else
    info "Running ${PERF_PROMPTS} prompts..."
    if (cd "${REPO_DIR}" && "${PERF_SCRIPT}" --prompts "${PERF_PROMPTS}"); then
        ok "Perf test passed (${PERF_PROMPTS} prompts)"
        record "TUI perf regression (tui-perf-test.sh)" "PASS"
    else
        fail "Perf test failed (${PERF_PROMPTS} prompts)"
        record "TUI perf regression (tui-perf-test.sh)" "FAIL"
    fi
fi

# ---------------------------------------------------------------------------
# Step 8: Required documentation check
# ---------------------------------------------------------------------------

header "Step 8: Required documentation"

REQUIRED_DOCS=(
    "README.md"
    "ARCHITECTURE.md"
    "docs/text-layout-api.md"
    "docs/defpanel-guide.md"
    "docs/kernel-vs-app.md"
    "docs/benchmark-story.md"
    "docs/kernel-audit.md"
    "docs/packet-manual-test-playbook.md"
    "docs/tui-taxonomy.md"
    "docs/positioning.md"
)

DOCS_OK=true
for doc in "${REQUIRED_DOCS[@]}"; do
    full="${PTUI_DIR}/${doc}"
    if [[ -f "${full}" ]]; then
        info "Found: ${doc}"
    else
        fail "Missing: ${doc}"
        DOCS_OK=false
    fi
done

if "${DOCS_OK}"; then
    ok "All required docs present"
    record "Required documentation" "PASS"
else
    fail "One or more required docs missing"
    record "Required documentation" "FAIL"
fi

# ---------------------------------------------------------------------------
# Summary
# ---------------------------------------------------------------------------

echo ""
echo "${HR}"
echo "  Release Checklist Summary"
echo "${HR}"
echo ""

TOTAL="${#STEP_NAMES[@]}"
PASS_COUNT=0
FAIL_COUNT=0
SKIP_COUNT=0

for i in $(seq 0 $(( TOTAL - 1 ))); do
    name="${STEP_NAMES[$i]}"
    status="${STEP_STATUS[$i]}"
    note="${STEP_NOTES[$i]}"

    case "${status}" in
        PASS) printf "  [PASS]  %s\n" "${name}"; PASS_COUNT=$(( PASS_COUNT + 1 )) ;;
        FAIL) printf "  [FAIL]  %s\n" "${name}"; FAIL_COUNT=$(( FAIL_COUNT + 1 )) ;;
        SKIP) printf "  [SKIP]  %s%s\n" "${name}" "${note:+ (${note})}"; SKIP_COUNT=$(( SKIP_COUNT + 1 )) ;;
    esac
done

echo ""
echo "  Total: ${TOTAL}  Pass: ${PASS_COUNT}  Fail: ${FAIL_COUNT}  Skip: ${SKIP_COUNT}"
echo ""

if [[ "${FAIL_COUNT}" -gt 0 ]]; then
    echo "  VERDICT: RELEASE BLOCKED — ${FAIL_COUNT} step(s) failed."
    echo ""
    exit 1
else
    echo "  VERDICT: RELEASE READY"
    echo ""
    exit 0
fi
