#!/usr/bin/env bash
# capture-screenshots.sh -- PTUI visual baseline generator and comparer.
#
# Captures deterministic PTUI scenarios in tmux, saves ANSI/plain-text/PNG
# artifacts, and compares them against tracked screenshot baselines.
#
# Usage:
#   ./ptui/bin/capture-screenshots.sh
#   ./ptui/bin/capture-screenshots.sh --update
#   ./ptui/bin/capture-screenshots.sh --demo metrics-dashboard
#   ./ptui/bin/capture-screenshots.sh --text-only

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PTUI_DIR="$(cd "${SCRIPT_DIR}/.." && pwd)"
REPO_DIR="$(cd "${PTUI_DIR}/.." && pwd)"

REF_DIR="${PTUI_DIR}/test/snapshots/visual"
ARTIFACT_ROOT_DEFAULT="${REPO_DIR}/tmp/ptui-visual"
ANSI_TO_PNG="${REPO_DIR}/bin/ansi-to-png.py"

ARTIFACT_ROOT="${ARTIFACT_ROOT_DEFAULT}"
ARTIFACT_DIR=""
CURRENT_DIR=""
DIFF_DIR=""
UPDATE=false
TEXT_ONLY=false
SCENARIO_FILTER=""
ACTIVE_SESSION=""

PASS_COUNT=0
FAIL_COUNT=0
UPDATE_COUNT=0
SKIP_COUNT=0

usage() {
  printf '%s\n' \
    "Usage:" \
    "  $(basename "$0") [--update] [--demo <name>] [--artifact-root <dir>] [--text-only]" \
    "" \
    "Demos:" \
    "  metrics-dashboard    wide dashboard baseline" \
    "  ops-wallboard        deterministic themed wallboard baseline" \
    "  release-tracker      constrained/narrow layout baseline" \
    "  atop-dashboard       deterministic detail-overlay baseline" \
    "" \
    "Options:" \
    "  --update              Overwrite tracked reference baselines." \
    "  --demo <name>         Run only one named scenario." \
    "  --artifact-root <dir> Write current captures and diffs under <dir>." \
    "  --text-only           Skip PNG rendering/comparison and save text only." \
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
    --demo)
      [[ $# -ge 2 ]] || fail "--demo requires a value"
      SCENARIO_FILTER="$2"
      shift 2
      ;;
    --all)
      SCENARIO_FILTER=""
      shift
      ;;
    --artifact-root)
      [[ $# -ge 2 ]] || fail "--artifact-root requires a value"
      ARTIFACT_ROOT="$2"
      shift 2
      ;;
    --text-only)
      TEXT_ONLY=true
      shift
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
require_cmd python3

[[ -f "${ANSI_TO_PNG}" ]] || fail "missing ANSI renderer: ${ANSI_TO_PNG}"
[[ -x "${PTUI_DIR}/dist/metrics-dashboard" ]] || fail "missing metrics-dashboard binary; run ./ptui/bin/build.sh first"

"${PTUI_DIR}/bin/ensure-quicklisp.sh" >/dev/null

mkdir -p "${REF_DIR}" "${ARTIFACT_ROOT}"
ARTIFACT_DIR="${ARTIFACT_ROOT}/$(date -u +%Y%m%dT%H%M%SZ)-$$"
CURRENT_DIR="${ARTIFACT_DIR}/current"
DIFF_DIR="${ARTIFACT_DIR}/diffs"
mkdir -p "${CURRENT_DIR}" "${DIFF_DIR}"

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

write_lisp_launch_script() {
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

write_binary_launch_script() {
  local path="$1"
  local binary="$2"
  local exit_ms="$3"

  printf '%s\n' \
    '#!/usr/bin/env bash' \
    'set -euo pipefail' \
    "cd \"${PTUI_DIR}\"" \
    "export PTUI_EXIT_AFTER_MS=${exit_ms}" \
    'export PTUI_LOG_LEVEL=error' \
    'export COLORTERM=truecolor' \
    'export TERM=xterm-256color' \
    "\"${binary}\"" \
    > "${path}"
  chmod +x "${path}"
}

save_snapshot() {
  local session="$1"
  local name="$2"
  local cols="$3"
  local rows="$4"
  local dest_dir="$5"

  local ansi_file="${dest_dir}/${name}.ansi"
  local txt_file="${dest_dir}/${name}.txt"
  local png_file="${dest_dir}/${name}.png"

  capture_plain "${session}" > "${txt_file}"
  if ! "${TEXT_ONLY}"; then
    capture_ansi "${session}" > "${ansi_file}"
    python3 "${ANSI_TO_PNG}" "${ansi_file}" "${png_file}" --cols "${cols}" --rows "${rows}" >/dev/null
  fi
}

compare_snapshot() {
  local name="$1"
  local ok=true

  local cur_txt="${CURRENT_DIR}/${name}.txt"
  local ref_txt="${REF_DIR}/${name}.txt"
  local cur_ansi="${CURRENT_DIR}/${name}.ansi"
  local ref_ansi="${REF_DIR}/${name}.ansi"
  local cur_png="${CURRENT_DIR}/${name}.png"
  local ref_png="${REF_DIR}/${name}.png"

  if [[ ! -f "${ref_txt}" ]]; then
    printf '  FAIL: %s missing reference text snapshot, run with --update\n' "${name}"
    FAIL_COUNT=$((FAIL_COUNT + 1))
    return 1
  fi

  if ! cmp -s "${ref_txt}" "${cur_txt}"; then
    diff -u "${ref_txt}" "${cur_txt}" > "${DIFF_DIR}/${name}.txt.diff" || true
    printf '  FAIL: %s text snapshot differs (%s)\n' "${name}" "${DIFF_DIR}/${name}.txt.diff"
    ok=false
  fi

  if ! "${TEXT_ONLY}"; then
    if [[ ! -f "${ref_ansi}" || ! -f "${ref_png}" ]]; then
      printf '  FAIL: %s missing reference ANSI/PNG artifacts, run with --update\n' "${name}"
      FAIL_COUNT=$((FAIL_COUNT + 1))
      return 1
    fi

    if ! cmp -s "${ref_ansi}" "${cur_ansi}"; then
      diff -u "${ref_ansi}" "${cur_ansi}" > "${DIFF_DIR}/${name}.ansi.diff" || true
      printf '  FAIL: %s ANSI snapshot differs (%s)\n' "${name}" "${DIFF_DIR}/${name}.ansi.diff"
      ok=false
    fi

    if ! cmp -s "${ref_png}" "${cur_png}"; then
      if command -v compare >/dev/null 2>&1; then
        compare -metric AE "${ref_png}" "${cur_png}" "${DIFF_DIR}/${name}.png.diff.png" >/dev/null 2>"${DIFF_DIR}/${name}.png.diff.txt" || true
      fi
      printf '  FAIL: %s PNG snapshot differs (%s)\n' "${name}" "${DIFF_DIR}/${name}.png.diff.png"
      ok=false
    fi
  fi

  if "${ok}"; then
    PASS_COUNT=$((PASS_COUNT + 1))
    info "PASS ${name}"
    return 0
  fi

  FAIL_COUNT=$((FAIL_COUNT + 1))
  return 1
}

record_and_compare() {
  local session="$1"
  local name="$2"
  local cols="$3"
  local rows="$4"

  if "${UPDATE}"; then
    save_snapshot "${session}" "${name}" "${cols}" "${rows}" "${REF_DIR}"
    UPDATE_COUNT=$((UPDATE_COUNT + 1))
    info "UPDATED ${name}"
    return 0
  fi

  save_snapshot "${session}" "${name}" "${cols}" "${rows}" "${CURRENT_DIR}"
  compare_snapshot "${name}"
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

send_keys() {
  local session="$1"
  shift
  tmux send-keys -t "${session}" "$@"
}

run_metrics_dashboard() {
  local scenario_name="metrics-dashboard"
  local snapshot_name="metrics-dashboard-wide-160x48"
  local session="ptui-visual-${scenario_name//[^a-zA-Z0-9]/-}-$$"
  local launch_script="${ARTIFACT_DIR}/${scenario_name}.launch.sh"
  local cols=160
  local rows=48

  if [[ -n "${SCENARIO_FILTER}" && "${scenario_name}" != *"${SCENARIO_FILTER}"* ]]; then
    SKIP_COUNT=$((SKIP_COUNT + 1))
    info "SKIP ${scenario_name}"
    return 0
  fi

  printf 'Scenario: %s\n' "${scenario_name}"
  write_binary_launch_script "${launch_script}" "${PTUI_DIR}/dist/metrics-dashboard" "2500"
  launch_session "${session}" "${launch_script}" "${cols}" "${rows}"
  if ! wait_for_text "${session}" "PTUI Metrics Dashboard" 16; then
    printf '  FAIL: %s did not render dashboard marker\n' "${scenario_name}"
    FAIL_COUNT=$((FAIL_COUNT + 1))
    close_session "${session}"
    return 1
  fi
  sleep 0.5
  record_and_compare "${session}" "${snapshot_name}" "${cols}" "${rows}" || { close_session "${session}"; return 1; }
  close_session "${session}"
}

run_ops_wallboard() {
  local scenario_name="ops-wallboard"
  local snapshot_name="ops-wallboard-wide-120x24"
  local session="ptui-visual-${scenario_name//[^a-zA-Z0-9]/-}-$$"
  local launch_script="${ARTIFACT_DIR}/${scenario_name}.launch.sh"
  local cols=120
  local rows=24

  if [[ -n "${SCENARIO_FILTER}" && "${scenario_name}" != *"${SCENARIO_FILTER}"* ]]; then
    SKIP_COUNT=$((SKIP_COUNT + 1))
    info "SKIP ${scenario_name}"
    return 0
  fi

  printf 'Scenario: %s\n' "${scenario_name}"
  write_lisp_launch_script "${launch_script}" "(ptui.examples.ops-wallboard:run-visual-demo)" "10000"
  launch_session "${session}" "${launch_script}" "${cols}" "${rows}"
  if ! wait_for_text "${session}" "NOC WALLBOARD" 16; then
    printf '  FAIL: %s did not render wallboard marker\n' "${scenario_name}"
    FAIL_COUNT=$((FAIL_COUNT + 1))
    close_session "${session}"
    return 1
  fi
  sleep 0.5
  record_and_compare "${session}" "${snapshot_name}" "${cols}" "${rows}" || { close_session "${session}"; return 1; }
  close_session "${session}"
}

run_release_tracker() {
  local scenario_name="release-tracker"
  local snapshot_name="release-tracker-narrow-48x16"
  local session="ptui-visual-${scenario_name//[^a-zA-Z0-9]/-}-$$"
  local launch_script="${ARTIFACT_DIR}/${scenario_name}.launch.sh"
  local cols=48
  local rows=16

  if [[ -n "${SCENARIO_FILTER}" && "${scenario_name}" != *"${SCENARIO_FILTER}"* ]]; then
    SKIP_COUNT=$((SKIP_COUNT + 1))
    info "SKIP ${scenario_name}"
    return 0
  fi

  printf 'Scenario: %s\n' "${scenario_name}"
  write_lisp_launch_script "${launch_script}" "(ptui.examples.release-tracker:run-release-tracker)" "10000"
  launch_session "${session}" "${launch_script}" "${cols}" "${rows}"
  if ! wait_for_text "${session}" "[queued]" 16; then
    printf '  FAIL: %s did not render release tracker marker\n' "${scenario_name}"
    FAIL_COUNT=$((FAIL_COUNT + 1))
    close_session "${session}"
    return 1
  fi
  sleep 0.5
  record_and_compare "${session}" "${snapshot_name}" "${cols}" "${rows}" || { close_session "${session}"; return 1; }
  close_session "${session}"
}

run_atop_dashboard() {
  local scenario_name="atop-dashboard"
  local snapshot_name="atop-dashboard-detail-100x24"
  local session="ptui-visual-${scenario_name//[^a-zA-Z0-9]/-}-$$"
  local launch_script="${ARTIFACT_DIR}/${scenario_name}.launch.sh"
  local cols=100
  local rows=24

  if [[ -n "${SCENARIO_FILTER}" && "${scenario_name}" != *"${SCENARIO_FILTER}"* ]]; then
    SKIP_COUNT=$((SKIP_COUNT + 1))
    info "SKIP ${scenario_name}"
    return 0
  fi

  printf 'Scenario: %s\n' "${scenario_name}"
  write_lisp_launch_script "${launch_script}" "(ptui.examples.atop-dashboard:run-behavior-demo)" "14000"
  launch_session "${session}" "${launch_script}" "${cols}" "${rows}"
  if ! wait_for_text "${session}" "status: fixture ready" 16; then
    printf '  FAIL: %s did not render atop fixture marker\n' "${scenario_name}"
    FAIL_COUNT=$((FAIL_COUNT + 1))
    close_session "${session}"
    return 1
  fi

  send_keys "${session}" p
  send_keys "${session}" m
  send_keys "${session}" Enter
  if ! wait_for_text "${session}" "Focused Process" 16; then
    printf '  FAIL: %s did not render focused-process overlay\n' "${scenario_name}"
    FAIL_COUNT=$((FAIL_COUNT + 1))
    close_session "${session}"
    return 1
  fi
  sleep 0.5
  record_and_compare "${session}" "${snapshot_name}" "${cols}" "${rows}" || { close_session "${session}"; return 1; }
  close_session "${session}"
}

printf '%s\n' '=== PTUI Screenshot Capture ==='
printf 'Reference dir: %s\n' "${REF_DIR}"
printf 'Artifact dir:  %s\n' "${ARTIFACT_DIR}"
printf 'Mode:          %s\n' "$([[ "${UPDATE}" = true ]] && printf 'update' || printf 'compare')"
printf 'Text-only:     %s\n' "$([[ "${TEXT_ONLY}" = true ]] && printf 'yes' || printf 'no')"
printf '\n'

run_metrics_dashboard
run_ops_wallboard
run_release_tracker
run_atop_dashboard

printf '\n'
printf 'PTUI_SCREENSHOT_CAPTURE pass=%d fail=%d updated=%d skipped=%d\n' \
  "${PASS_COUNT}" "${FAIL_COUNT}" "${UPDATE_COUNT}" "${SKIP_COUNT}"

if (( FAIL_COUNT > 0 )); then
  exit 1
fi
