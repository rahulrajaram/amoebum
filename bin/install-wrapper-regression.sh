#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"

TMP_ROOT="$(mktemp -d)"
HOME_DIR="${TMP_ROOT}/home"
RUN_DIR="${TMP_ROOT}/run"
INSTALL_PREFIX="${HOME_DIR}/.local/bin"
INSTALL_LOG="${TMP_ROOT}/install.stdout"
HELP_LOG="${TMP_ROOT}/help.log"
VERSION_LOG="${TMP_ROOT}/version.log"
RUNTIME_LOG="${HOME_DIR}/.amoebum/runtime/runtime.log"
NO_HOOKS_DIR="${TMP_ROOT}/no-hooks"

cleanup() {
  rm -rf "${TMP_ROOT}"
}

trap cleanup EXIT

fail() {
  printf 'FATAL: %s\n' "$*" >&2
  exit 1
}

require_cmd() {
  local cmd="$1"
  command -v "${cmd}" >/dev/null 2>&1 || fail "missing required command: ${cmd}"
}

run_with_timeout() {
  local secs="$1"
  shift

  if command -v timeout >/dev/null 2>&1; then
    timeout "${secs}" "$@"
    return $?
  fi

  "$@"
}

main() {
  require_cmd rg

  mkdir -p "${HOME_DIR}" "${RUN_DIR}" "${NO_HOOKS_DIR}"

  printf 'TMP_ROOT=%s\n' "${TMP_ROOT}"

  (
    cd "${REPO_ROOT}"
    HOME="${HOME_DIR}" \
    INSTALL_PREFIX="${INSTALL_PREFIX}" \
    COMMITHOOKS_DIR="${NO_HOOKS_DIR}" \
    ./install.sh >"${INSTALL_LOG}" 2>&1
  )

  rg -n '\[ok\].*Binary built|Runtime hash matches source-built image|Wrapper syntax check passed' \
    "${INSTALL_LOG}" || true

  [[ -x "${INSTALL_PREFIX}/amoebum" ]] || fail "installed wrapper missing at ${INSTALL_PREFIX}/amoebum"

  (
    cd "${RUN_DIR}"
    HOME="${HOME_DIR}" \
    run_with_timeout 8 "${INSTALL_PREFIX}/amoebum" --help >"${HELP_LOG}" 2>&1
  )
  help_rc=$?
  printf 'HELP_RC=%s\n' "${help_rc}"
  sed -n '1,20p' "${HELP_LOG}"
  [[ "${help_rc}" -eq 0 ]] || fail "installed wrapper --help returned ${help_rc}"
  rg -q '^Usage:' "${HELP_LOG}" || fail "installed wrapper --help did not print usage"
  rg -q 'amoebum --version' "${HELP_LOG}" || fail "installed wrapper --help missing --version usage"

  (
    cd "${RUN_DIR}"
    HOME="${HOME_DIR}" \
    run_with_timeout 8 "${INSTALL_PREFIX}/amoebum" --version >"${VERSION_LOG}" 2>&1
  )
  version_rc=$?
  printf 'VERSION_RC=%s\n' "${version_rc}"
  sed -n '1,20p' "${VERSION_LOG}"
  [[ "${version_rc}" -eq 0 ]] || fail "installed wrapper --version returned ${version_rc}"
  rg -q '^amoebum [0-9]' "${VERSION_LOG}" || fail "installed wrapper --version missing version output"

  [[ -f "${RUNTIME_LOG}" ]] || fail "runtime log missing at ${RUNTIME_LOG}"
  printf 'RUNTIME_LOG=%s\n' "${RUNTIME_LOG}"
  sed -n '1,20p' "${RUNTIME_LOG}"
  rg -q 'wrapper invoke: --help' "${RUNTIME_LOG}" || fail "runtime log missing --help invocation"
  rg -q 'wrapper invoke: --version' "${RUNTIME_LOG}" || fail "runtime log missing --version invocation"

  printf 'INSTALLED_WRAPPER_VALIDATION_OK\n'
}

main "$@"
