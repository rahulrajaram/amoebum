#!/usr/bin/env bash
# tmux-layout-regression.sh -- PTUI layout and resize regression harness.
#
# Captures deterministic PTUI example panes inside tmux, stores plain-text and
# ANSI snapshots, and compares them against tracked references.
#
# Usage:
#   ./ptui/bin/tmux-layout-regression.sh
#   ./ptui/bin/tmux-layout-regression.sh --update
#   ./ptui/bin/tmux-layout-regression.sh --scenario text-layout-basics

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PTUI_DIR="$(cd "${SCRIPT_DIR}/.." && pwd)"
REPO_DIR="$(cd "${PTUI_DIR}/.." && pwd)"

REF_DIR="${PTUI_DIR}/test/snapshots/tmux-layout"
ARTIFACT_ROOT_DEFAULT="${REPO_DIR}/tmp/ptui-tmux-layout"

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

compare_capture() {
  local name="$1"
  local plain_file="$2"
  local ansi_file="$3"
  local ref_plain="${REF_DIR}/${name}.txt"
  local ref_ansi="${REF_DIR}/${name}.ansi"
  local ok=true

  if "${UPDATE}"; then
    cp "${plain_file}" "${ref_plain}"
    cp "${ansi_file}" "${ref_ansi}"
    UPDATE_COUNT=$((UPDATE_COUNT + 1))
    info "UPDATED ${name}"
    return 0
  fi

  if [[ ! -f "${ref_plain}" || ! -f "${ref_ansi}" ]]; then
    printf '  FAIL: %s (missing reference snapshot, run with --update)\n' "${name}"
    FAIL_COUNT=$((FAIL_COUNT + 1))
    return 1
  fi

  if ! cmp -s "${ref_plain}" "${plain_file}"; then
    diff -u "${ref_plain}" "${plain_file}" > "${DIFF_DIR}/${name}.txt.diff" || true
    printf '  FAIL: %s plain-text snapshot differs (%s)\n' "${name}" "${DIFF_DIR}/${name}.txt.diff"
    ok=false
  fi

  if ! cmp -s "${ref_ansi}" "${ansi_file}"; then
    diff -u "${ref_ansi}" "${ansi_file}" > "${DIFF_DIR}/${name}.ansi.diff" || true
    printf '  FAIL: %s ANSI snapshot differs (%s)\n' "${name}" "${DIFF_DIR}/${name}.ansi.diff"
    ok=false
  fi

  if "${ok}"; then
    PASS_COUNT=$((PASS_COUNT + 1))
    info "PASS ${name}"
    return 0
  fi

  FAIL_COUNT=$((FAIL_COUNT + 1))
  return 1
}

record_capture() {
  local session="$1"
  local name="$2"
  local plain_file="${CURRENT_DIR}/${name}.txt"
  local ansi_file="${CURRENT_DIR}/${name}.ansi"

  capture_plain "${session}" > "${plain_file}"
  capture_ansi "${session}" > "${ansi_file}"
  compare_capture "${name}" "${plain_file}" "${ansi_file}"
}

run_scenario() {
  local scenario_name="$1"
  local entry_expr="$2"
  local marker="$3"
  local cols="$4"
  local rows="$5"
  local primary_capture="$6"
  local resize_cols="${7:-}"
  local resize_rows="${8:-}"
  local resized_capture="${9:-}"

  if [[ -n "${SCENARIO_FILTER}" && "${scenario_name}" != *"${SCENARIO_FILTER}"* ]]; then
    SKIP_COUNT=$((SKIP_COUNT + 1))
    info "SKIP ${scenario_name}"
    return 0
  fi

  local session="ptui-layout-${scenario_name//[^a-zA-Z0-9]/-}-$$"
  local launch_script="${ARTIFACT_DIR}/${scenario_name}.launch.sh"
  local exit_ms=8

  if [[ -n "${resize_cols}" && -n "${resize_rows}" ]]; then
    exit_ms=14
  fi

  printf 'Scenario: %s\n' "${scenario_name}"
  write_launch_script "${launch_script}" "${entry_expr}" "${exit_ms}000"
  ACTIVE_SESSION="${session}"
  tmux new-session -d -s "${session}" -x "${cols}" -y "${rows}" "${launch_script}"

  if ! wait_for_text "${session}" "${marker}" 20; then
    printf '  FAIL: %s did not render marker %s\n' "${scenario_name}" "${marker}"
    FAIL_COUNT=$((FAIL_COUNT + 1))
    tmux kill-session -t "${session}" >/dev/null 2>&1 || true
    ACTIVE_SESSION=""
    return 1
  fi

  sleep 0.3
  record_capture "${session}" "${primary_capture}" || true

  if [[ -n "${resize_cols}" && -n "${resize_rows}" && -n "${resized_capture}" ]]; then
    tmux resize-window -t "${session}" -x "${resize_cols}" -y "${resize_rows}"
    if ! wait_for_text "${session}" "${marker}" 20; then
      printf '  FAIL: %s did not recover marker %s after resize\n' "${scenario_name}" "${marker}"
      FAIL_COUNT=$((FAIL_COUNT + 1))
      tmux kill-session -t "${session}" >/dev/null 2>&1 || true
      ACTIVE_SESSION=""
      return 1
    fi
    sleep 0.5
    record_capture "${session}" "${resized_capture}" || true
  fi

  tmux send-keys -t "${session}" C-c >/dev/null 2>&1 || true
  tmux kill-session -t "${session}" >/dev/null 2>&1 || true
  ACTIVE_SESSION=""
}

printf '%s\n' '=== PTUI tmux layout regression ==='
printf 'Reference dir: %s\n' "${REF_DIR}"
printf 'Artifact dir:  %s\n' "${ARTIFACT_DIR}"
printf 'Mode:          %s\n' "$([[ "${UPDATE}" = true ]] && printf 'update' || printf 'compare')"
printf '\n'

run_scenario \
  "buffer-basics" \
  "(ptui.examples.buffer-basics:main)" \
  "PTUI buffer basics" \
  48 \
  14 \
  "buffer-basics-48x14"

run_scenario \
  "text-layout-basics" \
  "(ptui.examples.text-layout-basics:main)" \
  "PTUI text layout basics" \
  60 \
  14 \
  "text-layout-basics-wide-60x14" \
  34 \
  14 \
  "text-layout-basics-resized-34x14"

run_scenario \
  "release-tracker" \
  "(ptui.examples.release-tracker:run-release-tracker)" \
  "[queued]" \
  90 \
  16 \
  "release-tracker-wide-90x16" \
  48 \
  16 \
  "release-tracker-resized-48x16"

printf '\n'
printf 'TMUX_LAYOUT_REGRESSION pass=%d fail=%d updated=%d skipped=%d\n' \
  "${PASS_COUNT}" "${FAIL_COUNT}" "${UPDATE_COUNT}" "${SKIP_COUNT}"

if (( FAIL_COUNT > 0 )); then
  printf 'Diffs saved under: %s\n' "${DIFF_DIR}"
  exit 1
fi
