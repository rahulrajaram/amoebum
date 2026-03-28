#!/usr/bin/env bash
# tmux-behavior-regression.sh -- PTUI interaction regression harness.
#
# Launches deterministic PTUI examples in tmux, drives keyboard flows, and
# compares the final plain-text panes against tracked references.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PTUI_DIR="$(cd "${SCRIPT_DIR}/.." && pwd)"
REPO_DIR="$(cd "${PTUI_DIR}/.." && pwd)"

REF_DIR="${PTUI_DIR}/test/snapshots/tmux-behavior"
ARTIFACT_ROOT_DEFAULT="${REPO_DIR}/tmp/ptui-tmux-behavior"

ARTIFACT_ROOT="${ARTIFACT_ROOT_DEFAULT}"
ARTIFACT_DIR=""
CURRENT_DIR=""
DIFF_DIR=""
UPDATE=false
SCENARIO_FILTER=""
ACTIVE_SESSION=""

PASS_COUNT=0
FAIL_COUNT=0
UPDATE_COUNT=0
SKIP_COUNT=0

usage() {
  printf '%s\n' \
    "Usage:" \
    "  $(basename "$0") [--update] [--scenario <name>] [--artifact-root <dir>]" \
    "" \
    "Options:" \
    "  --update              Overwrite tracked reference snapshots." \
    "  --scenario <name>     Run only scenarios whose name contains <name>." \
    "  --artifact-root <dir> Write current captures and diffs under <dir>." \
    "  -h, --help            Show this help."
}

fail() {
  printf 'ERROR: %s\n' "$*" >&2
  exit 1
}

info() {
  printf '  %s\n' "$*"
}

require_cmd() {
  local cmd="$1"
  command -v "${cmd}" >/dev/null 2>&1 || fail "missing required command: ${cmd}"
}

cleanup() {
  if [[ -n "${ACTIVE_SESSION}" ]]; then
    tmux kill-session -t "${ACTIVE_SESSION}" >/dev/null 2>&1 || true
  fi
}

trap cleanup EXIT

while [[ $# -gt 0 ]]; do
  case "$1" in
    --update)
      UPDATE=true
      shift
      ;;
    --scenario)
      [[ $# -ge 2 ]] || fail "--scenario requires a value"
      SCENARIO_FILTER="$2"
      shift 2
      ;;
    --artifact-root)
      [[ $# -ge 2 ]] || fail "--artifact-root requires a value"
      ARTIFACT_ROOT="$2"
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

require_cmd tmux
require_cmd sbcl
require_cmd diff
require_cmd cmp
require_cmd rg

"${PTUI_DIR}/bin/ensure-quicklisp.sh" >/dev/null

mkdir -p "${REF_DIR}" "${ARTIFACT_ROOT}"
ARTIFACT_DIR="${ARTIFACT_ROOT}/$(date -u +%Y%m%dT%H%M%SZ)-$$"
CURRENT_DIR="${ARTIFACT_DIR}/current"
DIFF_DIR="${ARTIFACT_DIR}/diffs"
mkdir -p "${CURRENT_DIR}" "${DIFF_DIR}"

write_launch_script() {
  local path="$1"
  local entry_expr="$2"
  local exit_ms="$3"

  printf '%s\n' \
    '#!/usr/bin/env bash' \
    'set -euo pipefail' \
    "cd \"${PTUI_DIR}\"" \
    "export PTUI_EXIT_AFTER_MS=${exit_ms}" \
    'export PTUI_LOG_LEVEL=error' \
    'export COLORTERM=truecolor' \
    'export TERM=xterm-256color' \
    "sbcl --noinform --disable-debugger --eval '(require :asdf)' --eval '(load \"${PTUI_DIR}/.tools/quicklisp/setup.lisp\")' --eval '(asdf:load-asd \"${PTUI_DIR}/ptui.asd\")' --eval '(asdf:load-asd \"${PTUI_DIR}/ptui-examples.asd\")' --eval '(asdf:load-system :ptui/examples)' --eval '${entry_expr}' --quit" \
    > "${path}"
  chmod +x "${path}"
}

capture_plain() {
  local session="$1"
  tmux capture-pane -t "${session}" -p -N -S 0
}

save_step_capture() {
  local session="$1"
  local name="$2"
  local file="${CURRENT_DIR}/${name}.txt"
  capture_plain "${session}" > "${file}"
  normalize_capture "${name}" "${file}"
}

normalize_capture() {
  local name="$1"
  local file="$2"
  local tmp_file="${file}.tmp"

  case "${name}" in
    ops-wallboard-*)
      sed -E 's/LIVE INCIDENT TICKER :: .*/LIVE INCIDENT TICKER :: <dynamic> /' \
        "${file}" > "${tmp_file}"
      mv "${tmp_file}" "${file}"
      ;;
  esac
}

wait_for_contains() {
  local session="$1"
  local needle="$2"
  local timeout_seconds="${3:-10}"
  local max_attempts=$((timeout_seconds * 4))
  local attempt=0

  while (( attempt < max_attempts )); do
    if ! tmux has-session -t "${session}" >/dev/null 2>&1; then
      return 1
    fi
    if capture_plain "${session}" | rg -F "${needle}" >/dev/null 2>&1; then
      return 0
    fi
    sleep 0.25
    attempt=$((attempt + 1))
  done
  return 1
}

wait_for_absence() {
  local session="$1"
  local needle="$2"
  local timeout_seconds="${3:-10}"
  local max_attempts=$((timeout_seconds * 4))
  local attempt=0

  while (( attempt < max_attempts )); do
    if ! tmux has-session -t "${session}" >/dev/null 2>&1; then
      return 1
    fi
    if ! capture_plain "${session}" | rg -F "${needle}" >/dev/null 2>&1; then
      return 0
    fi
    sleep 0.25
    attempt=$((attempt + 1))
  done
  return 1
}

assert_contains() {
  local session="$1"
  local needle="$2"
  local label="$3"

  if ! wait_for_contains "${session}" "${needle}" 12; then
    save_step_capture "${session}" "${label}-failure"
    printf '  FAIL: %s missing "%s"\n' "${label}" "${needle}"
    FAIL_COUNT=$((FAIL_COUNT + 1))
    return 1
  fi
  return 0
}

assert_absence() {
  local session="$1"
  local needle="$2"
  local label="$3"

  if ! wait_for_absence "${session}" "${needle}" 12; then
    save_step_capture "${session}" "${label}-failure"
    printf '  FAIL: %s still shows "%s"\n' "${label}" "${needle}"
    FAIL_COUNT=$((FAIL_COUNT + 1))
    return 1
  fi
  return 0
}

send_keys() {
  local session="$1"
  shift
  tmux send-keys -t "${session}" "$@"
}

compare_capture() {
  local name="$1"
  local capture_file="${CURRENT_DIR}/${name}.txt"
  local ref_file="${REF_DIR}/${name}.txt"

  if "${UPDATE}"; then
    cp "${capture_file}" "${ref_file}"
    UPDATE_COUNT=$((UPDATE_COUNT + 1))
    info "UPDATED ${name}"
    return 0
  fi

  if [[ ! -f "${ref_file}" ]]; then
    printf '  FAIL: %s missing reference snapshot, run with --update\n' "${name}"
    FAIL_COUNT=$((FAIL_COUNT + 1))
    return 1
  fi

  if ! cmp -s "${ref_file}" "${capture_file}"; then
    diff -u "${ref_file}" "${capture_file}" > "${DIFF_DIR}/${name}.txt.diff" || true
    printf '  FAIL: %s snapshot differs (%s)\n' "${name}" "${DIFF_DIR}/${name}.txt.diff"
    FAIL_COUNT=$((FAIL_COUNT + 1))
    return 1
  fi

  PASS_COUNT=$((PASS_COUNT + 1))
  info "PASS ${name}"
  return 0
}

launch_session() {
  local session="$1"
  local launch_script="$2"
  local cols="$3"
  local rows="$4"

  ACTIVE_SESSION="${session}"
  tmux new-session -d -s "${session}" -x "${cols}" -y "${rows}" "${launch_script}"
}

close_session() {
  local session="$1"
  tmux send-keys -t "${session}" C-c >/dev/null 2>&1 || true
  tmux kill-session -t "${session}" >/dev/null 2>&1 || true
  ACTIVE_SESSION=""
}

run_focus_console() {
  local scenario_name="focus-console"
  local session="ptui-behavior-${scenario_name//[^a-zA-Z0-9]/-}-$$"
  local launch_script="${ARTIFACT_DIR}/${scenario_name}.launch.sh"
  local final_capture="focus-console-break-selected-48x14"

  if [[ -n "${SCENARIO_FILTER}" && "${scenario_name}" != *"${SCENARIO_FILTER}"* ]]; then
    SKIP_COUNT=$((SKIP_COUNT + 1))
    info "SKIP ${scenario_name}"
    return 0
  fi

  printf 'Scenario: %s\n' "${scenario_name}"
  write_launch_script "${launch_script}" "(ptui.examples.focus-console:run-focus-console)" "12000"
  launch_session "${session}" "${launch_script}" 60 14

  assert_contains "${session}" "FOCUS CONSOLE :: quiet-room | focus sprint" "${scenario_name}-startup" || { close_session "${session}"; return 1; }
  assert_contains "${session}" ">> Draft rollout memo" "${scenario_name}-initial-selection" || { close_session "${session}"; return 1; }

  send_keys "${session}" Down
  assert_contains "${session}" ">> Review benchmark deltas" "${scenario_name}-move-selection" || { close_session "${session}"; return 1; }

  send_keys "${session}" Enter
  assert_contains "${session}" "mode:break" "${scenario_name}-break-mode" || { close_session "${session}"; return 1; }
  assert_contains "${session}" "break window" "${scenario_name}-break-header" || { close_session "${session}"; return 1; }

  tmux resize-window -t "${session}" -x 48 -y 14
  assert_contains "${session}" "mode:break" "${scenario_name}-resize-mode" || { close_session "${session}"; return 1; }
  assert_contains "${session}" ">> Review benchmark deltas" "${scenario_name}-resize-selection" || { close_session "${session}"; return 1; }

  save_step_capture "${session}" "${final_capture}"
  compare_capture "${final_capture}" || { close_session "${session}"; return 1; }

  close_session "${session}"
}

run_ops_wallboard() {
  local scenario_name="ops-wallboard"
  local session="ptui-behavior-${scenario_name//[^a-zA-Z0-9]/-}-$$"
  local launch_script="${ARTIFACT_DIR}/${scenario_name}.launch.sh"
  local final_capture="ops-wallboard-infra-selected-70x18"

  if [[ -n "${SCENARIO_FILTER}" && "${scenario_name}" != *"${SCENARIO_FILTER}"* ]]; then
    SKIP_COUNT=$((SKIP_COUNT + 1))
    info "SKIP ${scenario_name}"
    return 0
  fi

  printf 'Scenario: %s\n' "${scenario_name}"
  write_launch_script "${launch_script}" "(ptui.examples.ops-wallboard:run-ops-wallboard)" "12000"
  launch_session "${session}" "${launch_script}" 100 18

  assert_contains "${session}" "NOC WALLBOARD" "${scenario_name}-startup" || { close_session "${session}"; return 1; }
  assert_contains "${session}" "filter:ALL" "${scenario_name}-initial-filter" || { close_session "${session}"; return 1; }

  send_keys "${session}" Right
  assert_contains "${session}" "filter:APP" "${scenario_name}-app-filter" || { close_session "${session}"; return 1; }
  assert_contains "${session}" "services: 3" "${scenario_name}-app-count" || { close_session "${session}"; return 1; }

  send_keys "${session}" Down
  assert_contains "${session}" "selected: 1" "${scenario_name}-selected-index" || { close_session "${session}"; return 1; }

  send_keys "${session}" Right
  assert_contains "${session}" "filter:INFRA" "${scenario_name}-infra-filter" || { close_session "${session}"; return 1; }
  assert_contains "${session}" "services: 2" "${scenario_name}-infra-count" || { close_session "${session}"; return 1; }

  tmux resize-window -t "${session}" -x 70 -y 18
  assert_contains "${session}" "filter:INFRA" "${scenario_name}-resize-filter" || { close_session "${session}"; return 1; }
  assert_contains "${session}" "selected: 1" "${scenario_name}-resize-selection" || { close_session "${session}"; return 1; }

  save_step_capture "${session}" "${final_capture}"
  compare_capture "${final_capture}" || { close_session "${session}"; return 1; }

  close_session "${session}"
}

run_atop_dashboard() {
  local scenario_name="atop-dashboard"
  local session="ptui-behavior-${scenario_name//[^a-zA-Z0-9]/-}-$$"
  local launch_script="${ARTIFACT_DIR}/${scenario_name}.launch.sh"
  local final_capture="atop-dashboard-paused-detail-100x24"

  if [[ -n "${SCENARIO_FILTER}" && "${scenario_name}" != *"${SCENARIO_FILTER}"* ]]; then
    SKIP_COUNT=$((SKIP_COUNT + 1))
    info "SKIP ${scenario_name}"
    return 0
  fi

  printf 'Scenario: %s\n' "${scenario_name}"
  write_launch_script "${launch_script}" "(ptui.examples.atop-dashboard:run-behavior-demo)" "16000"
  launch_session "${session}" "${launch_script}" 118 28

  assert_contains "${session}" "status: fixture ready" "${scenario_name}-startup" || { close_session "${session}"; return 1; }
  assert_contains "${session}" "mode: running" "${scenario_name}-initial-mode" || { close_session "${session}"; return 1; }

  send_keys "${session}" p
  assert_contains "${session}" "status: paused by user" "${scenario_name}-paused-status" || { close_session "${session}"; return 1; }
  assert_contains "${session}" "mode: paused" "${scenario_name}-paused-mode" || { close_session "${session}"; return 1; }

  send_keys "${session}" "?"
  assert_contains "${session}" "Controls" "${scenario_name}-help-overlay" || { close_session "${session}"; return 1; }

  send_keys "${session}" h
  assert_absence "${session}" "Controls" "${scenario_name}-hide-help" || { close_session "${session}"; return 1; }

  send_keys "${session}" m
  assert_contains "${session}" "status: process sort: mem" "${scenario_name}-sort-memory" || { close_session "${session}"; return 1; }

  send_keys "${session}" Enter
  assert_contains "${session}" "Focused Process" "${scenario_name}-detail-overlay" || { close_session "${session}"; return 1; }
  assert_contains "${session}" "python pipeline.py" "${scenario_name}-detail-command" || { close_session "${session}"; return 1; }

  tmux resize-window -t "${session}" -x 100 -y 24
  assert_contains "${session}" "Focused Process" "${scenario_name}-resize-detail" || { close_session "${session}"; return 1; }
  assert_contains "${session}" "python pipeline.py" "${scenario_name}-resize-command" || { close_session "${session}"; return 1; }
  assert_contains "${session}" "status: process sort: mem" "${scenario_name}-resize-sort-footer" || { close_session "${session}"; return 1; }
  assert_contains "${session}" "mode: paused" "${scenario_name}-resize-paused-footer" || { close_session "${session}"; return 1; }

  save_step_capture "${session}" "${final_capture}"
  compare_capture "${final_capture}" || { close_session "${session}"; return 1; }

  close_session "${session}"
}

printf '%s\n' '=== PTUI tmux behavior regression ==='
printf 'Reference dir: %s\n' "${REF_DIR}"
printf 'Artifact dir:  %s\n' "${ARTIFACT_DIR}"
printf 'Mode:          %s\n' "$([[ "${UPDATE}" = true ]] && printf 'update' || printf 'compare')"
printf '\n'

run_focus_console
run_ops_wallboard
run_atop_dashboard

printf '\n'
printf 'TMUX_BEHAVIOR_REGRESSION pass=%d fail=%d updated=%d skipped=%d\n' \
  "${PASS_COUNT}" "${FAIL_COUNT}" "${UPDATE_COUNT}" "${SKIP_COUNT}"

if (( FAIL_COUNT > 0 )); then
  printf 'Diffs saved under: %s\n' "${DIFF_DIR}"
  exit 1
fi
