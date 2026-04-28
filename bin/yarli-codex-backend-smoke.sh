#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"
WRAPPER="${SCRIPT_DIR}/yarli-codex-stdin.sh"
CONFIG_FILE="${REPO_ROOT}/yarli.toml"
TIMEOUT_SECONDS="${YARLI_CODEX_BACKEND_SMOKE_TIMEOUT_SECONDS:-90}"

TMP_DIR="$(mktemp -d /tmp/yarli-codex-backend-smoke-XXXXXX)"
trap 'rm -rf "${TMP_DIR}"' EXIT

usage() {
  printf '%s\n' \
    "Usage: bin/yarli-codex-backend-smoke.sh" \
    "" \
    "Runs tiny and realistic prompt launches through the configured stdin wrapper." \
    "Set YARLI_CODEX_BACKEND_SMOKE_TIMEOUT_SECONDS to override the per-case timeout."
}

require_wrapper() {
  if [[ ! -x "${WRAPPER}" ]]; then
    printf 'missing executable wrapper: %s\n' "${WRAPPER}" >&2
    exit 2
  fi
}

read_cli_config_value() {
  local key="$1"

  awk -v key="${key}" '
    /^\[cli\]/ {
      in_cli = 1
      next
    }
    /^\[/ {
      if (in_cli) {
        exit
      }
    }
    in_cli && $1 == key {
      sub(/^[^=]*=[[:space:]]*/, "", $0)
      gsub(/^"|"$/, "", $0)
      print
      exit
    }
  ' "${CONFIG_FILE}"
}

verify_yarli_config() {
  local prompt_mode
  local command_path

  if [[ ! -f "${CONFIG_FILE}" ]]; then
    printf 'missing Yarli config: %s\n' "${CONFIG_FILE}" >&2
    exit 2
  fi

  prompt_mode="$(read_cli_config_value prompt_mode)"
  command_path="$(read_cli_config_value command)"

  if [[ "${prompt_mode}" != "stdin" ]]; then
    printf 'yarli.toml [cli] prompt_mode must be stdin, got: %s\n' "${prompt_mode}" >&2
    exit 2
  fi

  if [[ "${command_path}" != "${WRAPPER}" ]]; then
    printf 'yarli.toml [cli] command must be absolute live wrapper path\n' >&2
    printf 'expected: %s\nactual:   %s\n' "${WRAPPER}" "${command_path}" >&2
    exit 2
  fi

  printf 'YARLI_CODEX_BACKEND_CONFIG_OK prompt_mode=%s command=%s\n' "${prompt_mode}" "${command_path}"
}

write_tiny_prompt() {
  local prompt_file="$1"
  printf '%s\n' \
    "You are running a deterministic backend smoke test." \
    "Do not inspect or modify files." \
    "Reply exactly: YARLI_CODEX_BACKEND_SMOKE_TINY_OK" \
    > "${prompt_file}"
}

write_realistic_prompt() {
  local prompt_file="$1"
  local idx

  {
    printf '%s\n' \
      "# Yarli Backend Realistic Prompt Smoke" \
      "" \
      "You are running a deterministic backend smoke test for a Yarli Codex worker." \
      "Do not inspect or modify files. Do not run commands." \
      "This prompt is intentionally multi-kilobyte so wrapper launch shape, stdin handling, and Codex startup are exercised without passing prompt content through argv." \
      "" \
      "Owned files for this simulated tranche:" \
      "- bin/yarli-codex-stdin.sh" \
      "- bin/yarli-codex-backend-smoke.sh" \
      "- yarli.toml" \
      "" \
      "Context:"

    for idx in $(seq 1 96); do
      printf 'Context block %03d: preserve prompt_mode stdin, keep model gpt-5.4, keep model_reasoning_effort medium, sanitize inherited .codex/tmp/arg0 PATH entries, avoid parent CODEX_THREAD_ID reuse, and fail fast with useful stderr before retrying NXT-424.\n' "${idx}"
    done

    printf '%s\n' \
      "" \
      "Exit contract:" \
      "Reply exactly: YARLI_CODEX_BACKEND_SMOKE_REALISTIC_OK"
  } > "${prompt_file}"
}

print_failure_artifacts() {
  local name="$1"
  local rc="$2"
  local stdout_file="$3"
  local stderr_file="$4"
  local last_message_file="$5"

  printf 'YARLI_CODEX_BACKEND_SMOKE_FAIL name=%s rc=%s\n' "${name}" "${rc}" >&2
  printf '%s\n' '--- stderr ---' >&2
  sed -n '1,160p' "${stderr_file}" >&2 || true
  printf '%s\n' '--- stdout ---' >&2
  sed -n '1,80p' "${stdout_file}" >&2 || true
  printf '%s\n' '--- last message ---' >&2
  sed -n '1,80p' "${last_message_file}" >&2 || true
}

run_case() {
  local name="$1"
  local prompt_file="$2"
  local expected="$3"
  local stdout_file="${TMP_DIR}/${name}.stdout.jsonl"
  local stderr_file="${TMP_DIR}/${name}.stderr.log"
  local last_message_file="${TMP_DIR}/${name}.last-message.txt"
  local rc=0

  printf 'YARLI_CODEX_BACKEND_SMOKE_START name=%s prompt_bytes=%s timeout_seconds=%s\n' \
    "${name}" "$(wc -c < "${prompt_file}")" "${TIMEOUT_SECONDS}"

  (
    cd "${REPO_ROOT}"
    timeout "${TIMEOUT_SECONDS}" "${WRAPPER}" \
      --output-last-message "${last_message_file}" \
      < "${prompt_file}" \
      > "${stdout_file}" \
      2> "${stderr_file}"
  ) || rc=$?

  if [[ "${rc}" -ne 0 ]]; then
    print_failure_artifacts "${name}" "${rc}" "${stdout_file}" "${stderr_file}" "${last_message_file}"
    exit "${rc}"
  fi

  if ! grep -Fq "${expected}" "${last_message_file}"; then
    print_failure_artifacts "${name}" "missing-expected-output" "${stdout_file}" "${stderr_file}" "${last_message_file}"
    exit 1
  fi

  printf 'YARLI_CODEX_BACKEND_SMOKE_CASE_OK name=%s\n' "${name}"
}

if [[ "${1:-}" == "--help" ]]; then
  usage
  exit 0
fi

if [[ $# -ne 0 ]]; then
  usage >&2
  exit 2
fi

require_wrapper
verify_yarli_config

tiny_prompt="${TMP_DIR}/tiny.md"
realistic_prompt="${TMP_DIR}/realistic.md"
write_tiny_prompt "${tiny_prompt}"
write_realistic_prompt "${realistic_prompt}"

run_case "tiny" "${tiny_prompt}" "YARLI_CODEX_BACKEND_SMOKE_TINY_OK"
run_case "realistic" "${realistic_prompt}" "YARLI_CODEX_BACKEND_SMOKE_REALISTIC_OK"

printf '%s\n' 'YARLI_CODEX_BACKEND_SMOKE_OK'
