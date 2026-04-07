#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"
STATE_DIR="${REPO_ROOT}/.yarli"
TRANCHES_FILE="${STATE_DIR}/tranches.toml"
EVIDENCE_DIR="${STATE_DIR}/evidence"

PASS_COUNT=0
FAIL_COUNT=0
HAD_STATE_DIR=false
HAD_EVIDENCE_DIR=false
HAD_TRANCHES_FILE=false
BACKUP_FILE=""

fail() {
  printf 'FATAL: %s\n' "$*" >&2
  exit 1
}

require_cmd() {
  local cmd="$1"
  command -v "${cmd}" >/dev/null 2>&1 || fail "missing required command: ${cmd}"
}

backup_state() {
  if [[ -d "${STATE_DIR}" ]]; then
    HAD_STATE_DIR=true
  fi
  if [[ -d "${EVIDENCE_DIR}" ]]; then
    HAD_EVIDENCE_DIR=true
  fi
  if [[ -f "${TRANCHES_FILE}" ]]; then
    HAD_TRANCHES_FILE=true
    BACKUP_FILE="$(mktemp)"
    cp "${TRANCHES_FILE}" "${BACKUP_FILE}"
  fi
}

restore_state() {
  if [[ -n "${BACKUP_FILE}" && -f "${BACKUP_FILE}" ]]; then
    mkdir -p "${STATE_DIR}"
    mv -f "${BACKUP_FILE}" "${TRANCHES_FILE}"
  else
    rm -f "${TRANCHES_FILE}"
  fi

  if ! ${HAD_EVIDENCE_DIR}; then
    rmdir "${EVIDENCE_DIR}" >/dev/null 2>&1 || true
  fi

  if ! ${HAD_STATE_DIR}; then
    rmdir "${STATE_DIR}" >/dev/null 2>&1 || true
  fi
}

trap restore_state EXIT

reset_missing_tranches() {
  rm -f "${TRANCHES_FILE}"
}

assert_valid_queue() {
  (
    cd "${REPO_ROOT}"
    yarli plan validate >/dev/null
  )
}

assert_bootstrapped_file() {
  [[ -f "${TRANCHES_FILE}" ]] || return 1
  rg -q '^version = 1$' "${TRANCHES_FILE}"
}

run_scenario() {
  local name="$1"
  shift

  printf 'Scenario: %s\n' "${name}"
  if "$@"; then
    PASS_COUNT=$((PASS_COUNT + 1))
    printf '  PASS: %s\n' "${name}"
  else
    FAIL_COUNT=$((FAIL_COUNT + 1))
    printf '  FAIL: %s\n' "${name}" >&2
  fi
}

scenario_bootstrap_script() {
  reset_missing_tranches
  (
    cd "${REPO_ROOT}"
    ./bin/yarli-bootstrap-local-state.sh >/dev/null
  )
  assert_bootstrapped_file
  assert_valid_queue
}

scenario_repo_wrapper_help_bootstrap() {
  reset_missing_tranches
  (
    cd "${REPO_ROOT}"
    ./bin/amoebum --help >/dev/null
  )
  assert_bootstrapped_file
  assert_valid_queue
}

main() {
  require_cmd yarli
  require_cmd rg
  backup_state

  run_scenario \
    "bootstrap recreates missing .yarli/tranches.toml and validates it" \
    scenario_bootstrap_script

  run_scenario \
    "repo wrapper help bootstraps missing local state and leaves a valid queue" \
    scenario_repo_wrapper_help_bootstrap

  printf 'YARLI_LOCAL_STATE_REGRESSION pass=%d fail=%d\n' "${PASS_COUNT}" "${FAIL_COUNT}"
  [[ "${FAIL_COUNT}" -eq 0 ]]
}

main "$@"
