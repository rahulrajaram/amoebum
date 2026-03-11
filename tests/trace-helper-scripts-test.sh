#!/usr/bin/env bash
# I361 targeted smoke test: bpftrace helpers exist and reference implemented probes.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"
TRACE_DIR="${REPO_ROOT}/scripts/trace"
DOC_FILE="${REPO_ROOT}/docs/trace-helper-scripts.md"

die() {
  echo "FATAL: $*" >&2
  exit 1
}

command -v rg >/dev/null 2>&1 || die "rg is required"

TOOL_SCRIPT="${TRACE_DIR}/tool-latency.bt"
STREAM_SCRIPT="${TRACE_DIR}/stream-throughput.bt"

[[ -f "${TOOL_SCRIPT}" ]] || die "missing ${TOOL_SCRIPT}"
[[ -f "${STREAM_SCRIPT}" ]] || die "missing ${STREAM_SCRIPT}"
[[ -f "${DOC_FILE}" ]] || die "missing ${DOC_FILE}"

rg -q '^#!/usr/bin/env bpftrace$' "${TOOL_SCRIPT}" || die "tool-latency.bt missing bpftrace shebang"
rg -q '^#!/usr/bin/env bpftrace$' "${STREAM_SCRIPT}" || die "stream-throughput.bt missing bpftrace shebang"

# Probe references must match implemented logical probe names in amoebum/src/usdt.lisp.
rg -q 'amoebum:tool-enter' "${TOOL_SCRIPT}" || die "tool-latency.bt missing amoebum:tool-enter reference"
rg -q 'amoebum:tool-exit' "${TOOL_SCRIPT}" || die "tool-latency.bt missing amoebum:tool-exit reference"
rg -q 'amoebum:llm-request-start' "${STREAM_SCRIPT}" || die "stream-throughput.bt missing amoebum:llm-request-start reference"
rg -q 'amoebum:llm-request-end' "${STREAM_SCRIPT}" || die "stream-throughput.bt missing amoebum:llm-request-end reference"

rg -q 'scripts/trace/tool-latency.bt' "${DOC_FILE}" || die "docs missing tool-latency.bt usage"
rg -q 'scripts/trace/stream-throughput.bt' "${DOC_FILE}" || die "docs missing stream-throughput.bt usage"

echo "I361_TRACE_HELPERS_SMOKE_OK"
