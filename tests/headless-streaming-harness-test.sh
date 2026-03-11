#!/usr/bin/env bash
# I333 targeted regression tests for headless streamed-turn harness.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"
HARNESS="${REPO_ROOT}/bin/headless-streaming-regression.sh"
FIXTURES="${REPO_ROOT}/tests/fixtures/streaming-regression"
ARTIFACT_DIR="${REPO_ROOT}/tmp/i333-harness-test-$$"

die() {
  echo "FATAL: $*" >&2
  exit 1
}

[[ -x "$HARNESS" ]] || die "Harness is not executable: $HARNESS"
command -v jq >/dev/null 2>&1 || die "jq is required"
command -v timeout >/dev/null 2>&1 || die "timeout is required"

mkdir -p "$ARTIFACT_DIR"

run_analyze_case() {
  local name="$1"
  local fixture="$2"
  local expected_outcome="$3"
  local expected_contract_valid="$4"
  local allow_silent="${5:-0}"
  local verdict="${ARTIFACT_DIR}/${name}.json"

  local cmd=(timeout 60 "$HARNESS" --analyze --journal-file "$fixture" --verdict-out "$verdict")
  if [[ "$allow_silent" -eq 1 ]]; then
    cmd+=(--allow-silent)
  fi
  "${cmd[@]}" >/dev/null

  jq -e --arg outcome "$expected_outcome" '.outcome == $outcome' "$verdict" >/dev/null
  jq -e --argjson valid "$expected_contract_valid" '.contract_valid == $valid' "$verdict" >/dev/null
}

run_analyze_case "silent" "${FIXTURES}/silent-completion.jsonl" "silent-completion" false 1
run_analyze_case "healthy" "${FIXTURES}/healthy-tool-continuation.jsonl" "tool-continuation" true
run_analyze_case "error" "${FIXTURES}/explicit-error.jsonl" "explicit-error" true

timeout 60 "$HARNESS" --self-test >/dev/null

# Run-mode smoke: bounded command path + artifact capture + answer verdict.
timeout 60 "$HARNESS" \
  --run \
  --command 'printf "{\"ok\":true,\"mode\":\"json\",\"action\":\"prompt\",\"output\":\"stub answer\",\"error\":null}\n"' \
  --timeout-seconds 10 \
  --artifact-dir "${ARTIFACT_DIR}/run-smoke" \
  --verdict-out "${ARTIFACT_DIR}/run-smoke/verdict.json" >/dev/null

jq -e '.mode == "run" and .outcome == "answer" and .contract_valid == true' \
  "${ARTIFACT_DIR}/run-smoke/verdict.json" >/dev/null
jq -e '.artifacts.stdout_file != "" and .artifacts.stderr_file != ""' \
  "${ARTIFACT_DIR}/run-smoke/verdict.json" >/dev/null

echo "I333_HEADLESS_HARNESS_TEST_OK"
