#!/usr/bin/env bash
# tmux-ansi-regression.sh -- PTUI ANSI/SGR regression harness.
#
# Captures raw tmux ANSI panes for deterministic PTUI scenarios and asserts
# that key style fragments remain present across truecolor and x256 modes.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PTUI_DIR="$(cd "${SCRIPT_DIR}/.." && pwd)"
REPO_DIR="$(cd "${PTUI_DIR}/.." && pwd)"

ARTIFACT_ROOT_DEFAULT="${REPO_DIR}/tmp/ptui-tmux-ansi"

ARTIFACT_ROOT="${ARTIFACT_ROOT_DEFAULT}"
ARTIFACT_DIR=""
CURRENT_DIR=""
SCENARIO_FILTER=""
ACTIVE_SESSION=""

PASS_COUNT=0
FAIL_COUNT=0
SKIP_COUNT=0

ESC=$'\033'

usage() {
  printf '%s\n' \
    "Usage:" \
    "  $(basename "$0") [--scenario <name>] [--artifact-root <dir>]" \
    "" \
    "Options:" \
    "  --scenario <name>     Run only scenarios whose name contains <name>." \
    "  --artifact-root <dir> Write ANSI captures under <dir>." \
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
require_cmd rg

"${PTUI_DIR}/bin/ensure-quicklisp.sh" >/dev/null

mkdir -p "${ARTIFACT_ROOT}"
ARTIFACT_DIR="${ARTIFACT_ROOT}/$(date -u +%Y%m%dT%H%M%SZ)-$$"
CURRENT_DIR="${ARTIFACT_DIR}/current"
mkdir -p "${CURRENT_DIR}"

write_launch_script() {
  local path="$1"
  local entry_expr="$2"
  local exit_ms="$3"
  local term="$4"
  local colorterm="$5"

  printf '%s\n' \
    '#!/usr/bin/env bash' \
    'set -euo pipefail' \
    "cd \"${PTUI_DIR}\"" \
    "export PTUI_EXIT_AFTER_MS=${exit_ms}" \
    'export PTUI_LOG_LEVEL=error' \
    "export TERM=${term}" \
    "export COLORTERM=${colorterm}" \
    "sbcl --noinform --disable-debugger --eval '(require :asdf)' --eval '(load \"${PTUI_DIR}/.tools/quicklisp/setup.lisp\")' --eval '(asdf:load-asd \"${PTUI_DIR}/ptui.asd\")' --eval '(asdf:load-asd \"${PTUI_DIR}/ptui-examples.asd\")' --eval '(asdf:load-system :ptui/examples)' --eval '${entry_expr}' --quit" \
    > "${path}"
  chmod +x "${path}"
}

capture_plain() {
  local session="$1"
  tmux capture-pane -t "${session}" -p -N -S 0
}

capture_ansi() {
  local session="$1"
  tmux capture-pane -t "${session}" -p -e -N -S 0
}

wait_for_text() {
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

save_capture() {
  local session="$1"
  local name="$2"
  capture_ansi "${session}" > "${CURRENT_DIR}/${name}.ansi"
}

assert_plain_contains() {
  local session="$1"
  local needle="$2"
  local label="$3"

  if ! wait_for_text "${session}" "${needle}" 12; then
    save_capture "${session}" "${label}-failure"
    printf '  FAIL: %s missing plain-text marker "%s"\n' "${label}" "${needle}"
    FAIL_COUNT=$((FAIL_COUNT + 1))
    return 1
  fi
  return 0
}

assert_ansi_contains() {
  local session="$1"
  local needle="$2"
  local label="$3"

  if ! capture_ansi "${session}" | grep -aF -- "${needle}" >/dev/null 2>&1; then
    save_capture "${session}" "${label}-failure"
    printf '  FAIL: %s missing ANSI fragment %q\n' "${label}" "${needle}"
    FAIL_COUNT=$((FAIL_COUNT + 1))
    return 1
  fi
  return 0
}

assert_ansi_regex() {
  local session="$1"
  local pattern="$2"
  local label="$3"

  if ! capture_ansi "${session}" | grep -aE -- "${pattern}" >/dev/null 2>&1; then
    save_capture "${session}" "${label}-failure"
    printf '  FAIL: %s missing ANSI regex %s\n' "${label}" "${pattern}"
    FAIL_COUNT=$((FAIL_COUNT + 1))
    return 1
  fi
  return 0
}

assert_ansi_absent() {
  local session="$1"
  local needle="$2"
  local label="$3"

  if capture_ansi "${session}" | grep -aF -- "${needle}" >/dev/null 2>&1; then
    save_capture "${session}" "${label}-failure"
    printf '  FAIL: %s unexpectedly contained ANSI fragment %q\n' "${label}" "${needle}"
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

mark_pass() {
  local name="$1"
  PASS_COUNT=$((PASS_COUNT + 1))
  info "PASS ${name}"
}

run_buffer_truecolor() {
  local scenario_name="buffer-basics-truecolor"
  local session="ptui-ansi-${scenario_name//[^a-zA-Z0-9]/-}-$$"
  local launch_script="${ARTIFACT_DIR}/${scenario_name}.launch.sh"

  if [[ -n "${SCENARIO_FILTER}" && "${scenario_name}" != *"${SCENARIO_FILTER}"* ]]; then
    SKIP_COUNT=$((SKIP_COUNT + 1))
    info "SKIP ${scenario_name}"
    return 0
  fi

  printf 'Scenario: %s\n' "${scenario_name}"
  write_launch_script "${launch_script}" "(ptui.examples.buffer-basics:main)" "10000" "xterm-256color" "truecolor"
  launch_session "${session}" "${launch_script}" 48 14

  assert_plain_contains "${session}" "PTUI buffer basics" "${scenario_name}-startup" || { close_session "${session}"; return 1; }
  assert_ansi_contains "${session}" "38;2;120;210;255" "${scenario_name}-title-color" || { close_session "${session}"; return 1; }
  assert_ansi_contains "${session}" "${ESC}[1m${ESC}[38;2;120;210;255mPTUI buffer basics" "${scenario_name}-title-bold" || { close_session "${session}"; return 1; }
  assert_ansi_contains "${session}" "38;2;90;140;210" "${scenario_name}-fill-color" || { close_session "${session}"; return 1; }

  save_capture "${session}" "${scenario_name}"
  mark_pass "${scenario_name}"
  close_session "${session}"
}

run_ops_truecolor() {
  local scenario_name="ops-wallboard-truecolor"
  local session="ptui-ansi-${scenario_name//[^a-zA-Z0-9]/-}-$$"
  local launch_script="${ARTIFACT_DIR}/${scenario_name}.launch.sh"

  if [[ -n "${SCENARIO_FILTER}" && "${scenario_name}" != *"${SCENARIO_FILTER}"* ]]; then
    SKIP_COUNT=$((SKIP_COUNT + 1))
    info "SKIP ${scenario_name}"
    return 0
  fi

  printf 'Scenario: %s\n' "${scenario_name}"
  write_launch_script "${launch_script}" "(ptui.examples.ops-wallboard:run-ops-wallboard)" "12000" "xterm-256color" "truecolor"
  launch_session "${session}" "${launch_script}" 100 18

  assert_plain_contains "${session}" "NOC WALLBOARD" "${scenario_name}-startup" || { close_session "${session}"; return 1; }
  send_keys "${session}" Right
  assert_plain_contains "${session}" "filter:APP" "${scenario_name}-app-filter" || { close_session "${session}"; return 1; }
  send_keys "${session}" Down
  assert_plain_contains "${session}" "selected: 1" "${scenario_name}-selected" || { close_session "${session}"; return 1; }
  send_keys "${session}" Right
  assert_plain_contains "${session}" "filter:INFRA" "${scenario_name}-infra-filter" || { close_session "${session}"; return 1; }

  assert_ansi_contains "${session}" "38;2;196;181;253" "${scenario_name}-filter-fg" || { close_session "${session}"; return 1; }
  assert_ansi_contains "${session}" "48;2;76;29;149" "${scenario_name}-filter-bg" || { close_session "${session}"; return 1; }
  assert_ansi_contains "${session}" "48;2;22;101;52" "${scenario_name}-selected-bg" || { close_session "${session}"; return 1; }
  assert_ansi_contains "${session}" "38;2;125;211;252" "${scenario_name}-selected-prefix" || { close_session "${session}"; return 1; }
  assert_ansi_regex "${session}" '48;2;(153;27;27|220;38;38)' "${scenario_name}-crit-badge" || { close_session "${session}"; return 1; }

  save_capture "${session}" "${scenario_name}"
  mark_pass "${scenario_name}"
  close_session "${session}"
}

run_atop_truecolor() {
  local scenario_name="atop-dashboard-truecolor"
  local session="ptui-ansi-${scenario_name//[^a-zA-Z0-9]/-}-$$"
  local launch_script="${ARTIFACT_DIR}/${scenario_name}.launch.sh"

  if [[ -n "${SCENARIO_FILTER}" && "${scenario_name}" != *"${SCENARIO_FILTER}"* ]]; then
    SKIP_COUNT=$((SKIP_COUNT + 1))
    info "SKIP ${scenario_name}"
    return 0
  fi

  printf 'Scenario: %s\n' "${scenario_name}"
  write_launch_script "${launch_script}" "(ptui.examples.atop-dashboard:run-behavior-demo)" "14000" "xterm-256color" "truecolor"
  launch_session "${session}" "${launch_script}" 118 28

  assert_plain_contains "${session}" "status: fixture ready" "${scenario_name}-startup" || { close_session "${session}"; return 1; }
  send_keys "${session}" p
  assert_plain_contains "${session}" "mode: paused" "${scenario_name}-paused" || { close_session "${session}"; return 1; }
  send_keys "${session}" "?"
  assert_plain_contains "${session}" "Controls" "${scenario_name}-help" || { close_session "${session}"; return 1; }
  send_keys "${session}" h
  send_keys "${session}" m
  assert_plain_contains "${session}" "status: process sort: mem" "${scenario_name}-sort" || { close_session "${session}"; return 1; }
  send_keys "${session}" Enter
  assert_plain_contains "${session}" "Focused Process" "${scenario_name}-detail" || { close_session "${session}"; return 1; }

  assert_ansi_contains "${session}" "38;2;255;220;120" "${scenario_name}-overlay-title" || { close_session "${session}"; return 1; }
  assert_ansi_contains "${session}" "38;2;255;255;170" "${scenario_name}-selected-row" || { close_session "${session}"; return 1; }

  save_capture "${session}" "${scenario_name}"
  mark_pass "${scenario_name}"
  close_session "${session}"
}

run_buffer_x256() {
  local scenario_name="buffer-basics-x256"
  local session="ptui-ansi-${scenario_name//[^a-zA-Z0-9]/-}-$$"
  local launch_script="${ARTIFACT_DIR}/${scenario_name}.launch.sh"

  if [[ -n "${SCENARIO_FILTER}" && "${scenario_name}" != *"${SCENARIO_FILTER}"* ]]; then
    SKIP_COUNT=$((SKIP_COUNT + 1))
    info "SKIP ${scenario_name}"
    return 0
  fi

  printf 'Scenario: %s\n' "${scenario_name}"
  write_launch_script "${launch_script}" "(ptui.examples.buffer-basics:main)" "10000" "xterm-256color" ""
  launch_session "${session}" "${launch_script}" 48 14

  assert_plain_contains "${session}" "PTUI buffer basics" "${scenario_name}-startup" || { close_session "${session}"; return 1; }
  assert_ansi_contains "${session}" "38;5;" "${scenario_name}-x256-present" || { close_session "${session}"; return 1; }
  assert_ansi_absent "${session}" "38;2;" "${scenario_name}-truecolor-absent" || { close_session "${session}"; return 1; }

  save_capture "${session}" "${scenario_name}"
  mark_pass "${scenario_name}"
  close_session "${session}"
}

printf '%s\n' '=== PTUI tmux ANSI regression ==='
printf 'Artifact dir: %s\n' "${ARTIFACT_DIR}"
printf '\n'

run_buffer_truecolor
run_ops_truecolor
run_atop_truecolor
run_buffer_x256

printf '\n'
printf 'TMUX_ANSI_REGRESSION pass=%d fail=%d skipped=%d\n' \
  "${PASS_COUNT}" "${FAIL_COUNT}" "${SKIP_COUNT}"

if (( FAIL_COUNT > 0 )); then
  exit 1
fi
