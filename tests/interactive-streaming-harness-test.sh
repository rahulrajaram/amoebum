#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"
HARNESS="${REPO_ROOT}/bin/tmux-streaming-regression.sh"
FIXTURE_DIR="${REPO_ROOT}/tests/fixtures/streaming-regression"
WORK_DIR="$(mktemp -d -t i334-harness-test-XXXXXX)"

cleanup() {
  rm -rf "${WORK_DIR}"
}
trap cleanup EXIT

[ -x "${HARNESS}" ] || {
  echo "missing harness script: ${HARNESS}" >&2
  exit 1
}

timeout 90 "${HARNESS}" --self-test > "${WORK_DIR}/self-test.log"

timeout 90 "${HARNESS}" --analyze \
  --journal-file "${FIXTURE_DIR}/bad-silent-completion.jsonl" \
  --verdict-out "${WORK_DIR}/bad.json" > "${WORK_DIR}/bad.log"
rg -q '"outcome": "silent-completion"' "${WORK_DIR}/bad.json"
rg -q '"silent_signature_detected": true' "${WORK_DIR}/bad.json"

timeout 90 "${HARNESS}" --analyze \
  --journal-file "${FIXTURE_DIR}/healthy-tool-continuation.jsonl" \
  --verdict-out "${WORK_DIR}/healthy.json" > "${WORK_DIR}/healthy.log"
rg -q '"outcome": "tool-continuation"' "${WORK_DIR}/healthy.json"

timeout 90 "${HARNESS}" --analyze \
  --journal-file "${FIXTURE_DIR}/explicit-error.jsonl" \
  --verdict-out "${WORK_DIR}/error.json" > "${WORK_DIR}/error.log"
rg -q '"outcome": "explicit-error"' "${WORK_DIR}/error.json"

echo "I334_INTERACTIVE_HARNESS_TEST_OK"
