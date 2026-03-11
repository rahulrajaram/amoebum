#!/usr/bin/env bash
# I334: tmux-driven interactive streaming regression harness.
#
# Modes:
#  - run (default): launch amoebum in tmux, inject prompt, capture artifacts, emit verdict JSON.
#  - analyze: classify an existing journal/pane/debug artifact set.
#  - self-test: fixture-based parser/verdict regression checks.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"

BINARY_DEFAULT="${REPO_ROOT}/dist/amoebum"
PROMPT_DEFAULT="Please inspect the current project and explain what you are doing."
ARTIFACT_ROOT_DEFAULT="${REPO_ROOT}/tmp/i334-interactive"

MODE="run"
COMMAND="${BINARY_DEFAULT}"
PROMPT="${PROMPT_DEFAULT}"
ARTIFACT_ROOT="${ARTIFACT_ROOT_DEFAULT}"
ARTIFACT_DIR=""
VERDICT_OUT=""
JOURNAL_FILE=""
PANE_FILE=""
DEBUG_FILE=""
WAIT_SECONDS=20
STARTUP_SECONDS=3
SESSION_TIMEOUT_SECONDS=45
EXPECT_TOOL_EVENTS=true

TMUX_SESSION=""

usage() {
  cat <<USAGE
Usage:
  $(basename "$0") [options]

Options:
  --run                          Run tmux harness (default).
  --analyze                      Analyze existing artifact files.
  --self-test                    Run fixture-based parser/verdict tests.
  --command <cmd>                Command to launch inside tmux (default: ./dist/amoebum).
  --prompt <text>                Prompt to inject in interactive session.
  --artifact-root <dir>          Root directory for generated runs.
  --artifact-dir <dir>           Explicit artifact directory for this invocation.
  --verdict-out <path>           Verdict JSON output path.
  --journal-file <path>          Journal JSONL file to analyze.
  --pane-file <path>             Pane capture file to analyze.
  --debug-file <path>            Debug/stderr log file to analyze.
  --wait-seconds <n>             Seconds to wait after sending prompt (default: 20).
  --startup-seconds <n>          Seconds to wait after tmux launch (default: 3).
  --session-timeout-seconds <n>  Hard upper bound for interactive wait (default: 45).
  --expect-tool-events <bool>    true/false. true treats no-tool stream as silent signature.
  -h, --help                     Show this help.

Examples:
  $(basename "$0") --command "./dist/amoebum --demo" --prompt "tool" --wait-seconds 8
  $(basename "$0") --analyze --journal-file /tmp/amoebum-journal/journal-20260306-184244.jsonl
  $(basename "$0") --self-test
USAGE
}

cleanup_tmux() {
  if [ -n "${TMUX_SESSION}" ]; then
    tmux kill-session -t "${TMUX_SESSION}" >/dev/null 2>&1 || true
  fi
}

trap cleanup_tmux EXIT

fail() {
  echo "ERROR: $*" >&2
  exit 1
}

normalize_bool() {
  local raw="${1:-}"
  case "$(printf '%s' "${raw}" | tr '[:upper:]' '[:lower:]')" in
    true|1|yes|y|on) echo true ;;
    false|0|no|n|off) echo false ;;
    *) fail "invalid boolean value: ${raw}" ;;
  esac
}

json_escape() {
  printf '%s' "${1:-}" | sed -e 's/\\/\\\\/g' -e 's/"/\\"/g'
}

count_matches() {
  local pattern="$1"
  local file="$2"
  if [ -z "${file}" ] || [ ! -f "${file}" ]; then
    echo 0
    return
  fi
  local matches
  matches="$(rg -o --no-messages "${pattern}" "${file}" 2>/dev/null || true)"
  if [ -z "${matches}" ]; then
    echo 0
  else
    printf '%s\n' "${matches}" | wc -l | tr -d ' '
  fi
}

latest_journal_file() {
  local dir="$1"
  if [ -d "${dir}" ] && ls -1 "${dir}"/journal-*.jsonl >/dev/null 2>&1; then
    ls -1t "${dir}"/journal-*.jsonl | head -n 1
  fi
}

bool_from_count() {
  local count="${1:-0}"
  if [ "${count}" -gt 0 ]; then
    echo true
  else
    echo false
  fi
}

LLM_STREAM_CHUNK_COUNT=0
STREAM_PROGRESS_COMPLETED_COUNT=0
STREAM_PROGRESS_RUNNING_COUNT=0
TOOL_CALL_EVENT_COUNT=0
TOOL_EVENT_COUNT=0
PANE_TOOL_MARKER_COUNT=0
ERROR_EVENT_COUNT=0
HAS_LLM_STREAM_CHUNKS=false
HAS_STREAM_PROGRESS=false
HAS_TOOL_EVENTS=false
HAS_ERROR_EVENTS=false
SILENT_SIGNATURE_DETECTED=false
OUTCOME="retry"

compute_verdict() {
  local journal_file="$1"
  local pane_file="$2"
  local debug_file="$3"

  LLM_STREAM_CHUNK_COUNT="$(count_matches '"type":"LLM:STREAM-CHUNK"' "${journal_file}")"
  STREAM_PROGRESS_COMPLETED_COUNT="$(count_matches '"type":"UI:STREAM-PROGRESS"[^\n]*:STATUS COMPLETED' "${journal_file}")"
  STREAM_PROGRESS_RUNNING_COUNT="$(count_matches '"type":"UI:STREAM-PROGRESS"[^\n]*:STATUS RUNNING' "${journal_file}")"
  TOOL_CALL_EVENT_COUNT="$(count_matches '"type":"TOOL-CALL:[^"]+"' "${journal_file}")"
  TOOL_EVENT_COUNT="$(count_matches '"type":"TOOL:[^"]+"' "${journal_file}")"

  local pane_tool_events
  pane_tool_events="$(count_matches 'TOOL-CALL:|TOOL:' "${pane_file}")"
  local debug_tool_events
  debug_tool_events="$(count_matches 'TOOL-CALL:|TOOL:' "${debug_file}")"
  PANE_TOOL_MARKER_COUNT=$((pane_tool_events + debug_tool_events))

  local journal_errors
  journal_errors="$(count_matches '"severity":"ERROR"|"type":"TOOL:ERROR"' "${journal_file}")"
  local pane_errors
  pane_errors="$(count_matches '(^|[^A-Z])ERROR([^A-Z]|$)|stream failed|exception|traceback' "${pane_file}")"
  local debug_errors
  debug_errors="$(count_matches '(^|[^A-Z])ERROR([^A-Z]|$)|stream failed|exception|traceback' "${debug_file}")"
  ERROR_EVENT_COUNT=$((journal_errors + pane_errors + debug_errors))

  HAS_LLM_STREAM_CHUNKS="$(bool_from_count "${LLM_STREAM_CHUNK_COUNT}")"
  local stream_progress_total
  stream_progress_total=$((STREAM_PROGRESS_COMPLETED_COUNT + STREAM_PROGRESS_RUNNING_COUNT))
  HAS_STREAM_PROGRESS="$(bool_from_count "${stream_progress_total}")"
  local tool_total
  tool_total=$((TOOL_CALL_EVENT_COUNT + TOOL_EVENT_COUNT + PANE_TOOL_MARKER_COUNT))
  HAS_TOOL_EVENTS="$(bool_from_count "${tool_total}")"
  HAS_ERROR_EVENTS="$(bool_from_count "${ERROR_EVENT_COUNT}")"

  SILENT_SIGNATURE_DETECTED=false
  if [ "${EXPECT_TOOL_EVENTS}" = true ] \
    && [ "${HAS_LLM_STREAM_CHUNKS}" = true ] \
    && [ "${HAS_STREAM_PROGRESS}" = true ] \
    && [ "${HAS_TOOL_EVENTS}" = false ]; then
    SILENT_SIGNATURE_DETECTED=true
  fi

  if [ "${HAS_ERROR_EVENTS}" = true ]; then
    OUTCOME="explicit-error"
  elif [ "${HAS_TOOL_EVENTS}" = true ]; then
    OUTCOME="tool-continuation"
  elif [ "${SILENT_SIGNATURE_DETECTED}" = true ]; then
    OUTCOME="silent-completion"
  elif [ "${HAS_LLM_STREAM_CHUNKS}" = true ]; then
    OUTCOME="answer"
  else
    OUTCOME="retry"
  fi
}

write_verdict_json() {
  local mode="$1"
  local verdict_path="$2"
  local journal_file="$3"
  local pane_file="$4"
  local debug_file="$5"

  mkdir -p "$(dirname "${verdict_path}")"

  local generated_at
  generated_at="$(date -u +"%Y-%m-%dT%H:%M:%SZ")"

  cat > "${verdict_path}" <<JSON
{
  "harness": "tmux-streaming-regression",
  "mode": "$(json_escape "${mode}")",
  "generated_at_utc": "${generated_at}",
  "outcome": "${OUTCOME}",
  "silent_signature_detected": ${SILENT_SIGNATURE_DETECTED},
  "expect_tool_events": ${EXPECT_TOOL_EVENTS},
  "signals": {
    "has_llm_stream_chunks": ${HAS_LLM_STREAM_CHUNKS},
    "has_stream_progress": ${HAS_STREAM_PROGRESS},
    "has_tool_events": ${HAS_TOOL_EVENTS},
    "has_error_events": ${HAS_ERROR_EVENTS},
    "llm_stream_chunk_count": ${LLM_STREAM_CHUNK_COUNT},
    "stream_progress_completed_count": ${STREAM_PROGRESS_COMPLETED_COUNT},
    "stream_progress_running_count": ${STREAM_PROGRESS_RUNNING_COUNT},
    "tool_call_event_count": ${TOOL_CALL_EVENT_COUNT},
    "tool_event_count": ${TOOL_EVENT_COUNT},
    "pane_tool_marker_count": ${PANE_TOOL_MARKER_COUNT},
    "error_event_count": ${ERROR_EVENT_COUNT}
  },
  "artifacts": {
    "journal_file": "$(json_escape "${journal_file}")",
    "pane_file": "$(json_escape "${pane_file}")",
    "debug_file": "$(json_escape "${debug_file}")"
  }
}
JSON
}

run_self_test_case() {
  local fixture_file="$1"
  local expected_outcome="$2"

  if [ ! -f "${fixture_file}" ]; then
    fail "missing fixture: ${fixture_file}"
  fi

  compute_verdict "${fixture_file}" "" ""
  if [ "${OUTCOME}" != "${expected_outcome}" ]; then
    echo "SELF_TEST_FAIL fixture=$(basename "${fixture_file}") expected=${expected_outcome} got=${OUTCOME}" >&2
    return 1
  fi

  echo "SELF_TEST_PASS fixture=$(basename "${fixture_file}") outcome=${OUTCOME}"
  return 0
}

run_self_test() {
  local fixture_dir="${REPO_ROOT}/tests/fixtures/streaming-regression"
  local failed=0

  run_self_test_case "${fixture_dir}/bad-silent-completion.jsonl" "silent-completion" || failed=$((failed + 1))
  run_self_test_case "${fixture_dir}/healthy-tool-continuation.jsonl" "tool-continuation" || failed=$((failed + 1))
  run_self_test_case "${fixture_dir}/explicit-error.jsonl" "explicit-error" || failed=$((failed + 1))

  if [ "${failed}" -gt 0 ]; then
    fail "self-test failed for ${failed} fixture(s)"
  fi

  echo "I334_TMUX_HARNESS_SELFTEST_OK cases=3"
}

resolve_analyze_inputs() {
  if [ -n "${ARTIFACT_DIR}" ]; then
    if [ -z "${JOURNAL_FILE}" ]; then
      JOURNAL_FILE="$(latest_journal_file "${ARTIFACT_DIR}/journal")"
    fi
    if [ -z "${PANE_FILE}" ] && [ -f "${ARTIFACT_DIR}/pane.txt" ]; then
      PANE_FILE="${ARTIFACT_DIR}/pane.txt"
    fi
    if [ -z "${DEBUG_FILE}" ] && [ -f "${ARTIFACT_DIR}/debug.log" ]; then
      DEBUG_FILE="${ARTIFACT_DIR}/debug.log"
    fi
    if [ -z "${VERDICT_OUT}" ]; then
      VERDICT_OUT="${ARTIFACT_DIR}/verdict.json"
    fi
  fi

  if [ -z "${VERDICT_OUT}" ]; then
    mkdir -p "${ARTIFACT_ROOT}"
    VERDICT_OUT="${ARTIFACT_ROOT}/analyze-$(date -u +%Y%m%dT%H%M%SZ)-$$.verdict.json"
  fi

  if [ -z "${JOURNAL_FILE}" ] && [ -z "${PANE_FILE}" ] && [ -z "${DEBUG_FILE}" ]; then
    fail "analyze mode requires at least one of --journal-file, --pane-file, --debug-file, or --artifact-dir"
  fi
}

run_analyze_mode() {
  resolve_analyze_inputs
  compute_verdict "${JOURNAL_FILE}" "${PANE_FILE}" "${DEBUG_FILE}"
  write_verdict_json "analyze" "${VERDICT_OUT}" "${JOURNAL_FILE}" "${PANE_FILE}" "${DEBUG_FILE}"
  echo "I334_TMUX_VERDICT mode=analyze outcome=${OUTCOME} verdict=${VERDICT_OUT}"
}

run_tmux_mode() {
  command -v tmux >/dev/null 2>&1 || fail "tmux is required"

  if [ ! -x "${BINARY_DEFAULT}" ] && [ "${COMMAND}" = "${BINARY_DEFAULT}" ]; then
    fail "binary not found/executable at ${BINARY_DEFAULT}; build first (e.g. make build)"
  fi

  mkdir -p "${ARTIFACT_ROOT}"
  if [ -z "${ARTIFACT_DIR}" ]; then
    ARTIFACT_DIR="${ARTIFACT_ROOT}/run-$(date -u +%Y%m%dT%H%M%SZ)-$$"
  fi

  local journal_dir="${ARTIFACT_DIR}/journal"
  mkdir -p "${journal_dir}"

  if [ -z "${PANE_FILE}" ]; then
    PANE_FILE="${ARTIFACT_DIR}/pane.txt"
  fi
  if [ -z "${DEBUG_FILE}" ]; then
    DEBUG_FILE="${ARTIFACT_DIR}/debug.log"
  fi
  if [ -z "${VERDICT_OUT}" ]; then
    VERDICT_OUT="${ARTIFACT_DIR}/verdict.json"
  fi

  local launch_script="${ARTIFACT_DIR}/launch.sh"
  cat > "${launch_script}" <<LAUNCH
#!/usr/bin/env bash
set -euo pipefail
cd "${REPO_ROOT}"
export AMOEBUM_EVENT_JOURNAL=1
export AMOEBUM_EVENT_JOURNAL_DIR="${journal_dir}"
exec bash -lc "${COMMAND}" 2> "${DEBUG_FILE}"
LAUNCH
  chmod +x "${launch_script}"

  TMUX_SESSION="amoebum-i334-$$"
  tmux new-session -d -s "${TMUX_SESSION}" -x 140 -y 42 "${launch_script}"

  local session_alive=true
  if ! tmux has-session -t "${TMUX_SESSION}" >/dev/null 2>&1; then
    session_alive=false
  fi

  if [ "${session_alive}" = true ]; then
    sleep "${STARTUP_SECONDS}"
    if tmux has-session -t "${TMUX_SESSION}" >/dev/null 2>&1; then
      tmux send-keys -t "${TMUX_SESSION}" "${PROMPT}" Enter || true
    else
      session_alive=false
    fi
  fi

  local elapsed=0
  while [ "${elapsed}" -lt "${WAIT_SECONDS}" ] && [ "${elapsed}" -lt "${SESSION_TIMEOUT_SECONDS}" ]; do
    sleep 1
    elapsed=$((elapsed + 1))
    if [ "${session_alive}" = true ] && ! tmux has-session -t "${TMUX_SESSION}" >/dev/null 2>&1; then
      session_alive=false
      break
    fi
  done

  if [ "${session_alive}" = true ] && tmux has-session -t "${TMUX_SESSION}" >/dev/null 2>&1; then
    tmux capture-pane -t "${TMUX_SESSION}" -p -S -12000 > "${PANE_FILE}" 2>/dev/null || true
    tmux send-keys -t "${TMUX_SESSION}" C-c >/dev/null 2>&1 || true
    sleep 1
    tmux capture-pane -t "${TMUX_SESSION}" -p -S -12000 > "${PANE_FILE}" 2>/dev/null || true
  else
    : > "${PANE_FILE}"
  fi

  JOURNAL_FILE="$(latest_journal_file "${journal_dir}")"

  compute_verdict "${JOURNAL_FILE}" "${PANE_FILE}" "${DEBUG_FILE}"
  write_verdict_json "run" "${VERDICT_OUT}" "${JOURNAL_FILE}" "${PANE_FILE}" "${DEBUG_FILE}"

  echo "I334_TMUX_VERDICT mode=run outcome=${OUTCOME} verdict=${VERDICT_OUT} artifact_dir=${ARTIFACT_DIR}"
}

while [ "$#" -gt 0 ]; do
  case "$1" in
    --run)
      MODE="run"
      shift
      ;;
    --analyze)
      MODE="analyze"
      shift
      ;;
    --self-test)
      MODE="self-test"
      shift
      ;;
    --command)
      [ "$#" -ge 2 ] || fail "--command requires a value"
      COMMAND="$2"
      shift 2
      ;;
    --prompt)
      [ "$#" -ge 2 ] || fail "--prompt requires a value"
      PROMPT="$2"
      shift 2
      ;;
    --artifact-root)
      [ "$#" -ge 2 ] || fail "--artifact-root requires a value"
      ARTIFACT_ROOT="$2"
      shift 2
      ;;
    --artifact-dir)
      [ "$#" -ge 2 ] || fail "--artifact-dir requires a value"
      ARTIFACT_DIR="$2"
      shift 2
      ;;
    --verdict-out)
      [ "$#" -ge 2 ] || fail "--verdict-out requires a value"
      VERDICT_OUT="$2"
      shift 2
      ;;
    --journal-file)
      [ "$#" -ge 2 ] || fail "--journal-file requires a value"
      JOURNAL_FILE="$2"
      shift 2
      ;;
    --pane-file)
      [ "$#" -ge 2 ] || fail "--pane-file requires a value"
      PANE_FILE="$2"
      shift 2
      ;;
    --debug-file)
      [ "$#" -ge 2 ] || fail "--debug-file requires a value"
      DEBUG_FILE="$2"
      shift 2
      ;;
    --wait-seconds)
      [ "$#" -ge 2 ] || fail "--wait-seconds requires a value"
      WAIT_SECONDS="$2"
      shift 2
      ;;
    --startup-seconds)
      [ "$#" -ge 2 ] || fail "--startup-seconds requires a value"
      STARTUP_SECONDS="$2"
      shift 2
      ;;
    --session-timeout-seconds)
      [ "$#" -ge 2 ] || fail "--session-timeout-seconds requires a value"
      SESSION_TIMEOUT_SECONDS="$2"
      shift 2
      ;;
    --expect-tool-events)
      [ "$#" -ge 2 ] || fail "--expect-tool-events requires a value"
      EXPECT_TOOL_EVENTS="$(normalize_bool "$2")"
      shift 2
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      fail "unknown argument: $1"
      ;;
  esac
done

case "${MODE}" in
  run)
    run_tmux_mode
    ;;
  analyze)
    run_analyze_mode
    ;;
  self-test)
    run_self_test
    ;;
  *)
    fail "unknown mode: ${MODE}"
    ;;
esac
