#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"

PLAN_FILE="${1:-${REPO_ROOT}/IMPLEMENTATION_PLAN.md}"
RUNTIME_SCRIPT="${2:-${REPO_ROOT}/bin/yarli-run-verification.sh}"
YARLI_CONFIG="${3:-${REPO_ROOT}/yarli.toml}"
EXPECTED_PACE_CMD="./bin/yarli-run-verification.sh"

fail() {
  echo "VERIFY_GATE_PARITY_ERROR: $*" >&2
  exit 1
}

require_file() {
  local path="$1"
  [[ -f "${path}" ]] || fail "missing file: ${path}"
}

extract_plan_commands() {
  local plan_file="$1"
  awk '
BEGIN {
  in_required = 0
  found_required = 0
  command_count = 0
}

/^Required verification sequence:$/ {
  in_required = 1
  found_required = 1
  next
}

in_required && /^[[:space:]]*$/ {
  in_required = 0
  next
}

in_required {
  if (match($0, /^[[:space:]]*[0-9]+\.[[:space:]]+`[^`]+`[[:space:]]*$/)) {
    line = $0
    sub(/^[[:space:]]*[0-9]+\.[[:space:]]+`/, "", line)
    sub(/`[[:space:]]*$/, "", line)
    print line
    command_count += 1
    next
  }
  printf("VERIFY_GATE_PARITY_ERROR: malformed required verification sequence line: %s\n", $0) > "/dev/stderr"
  exit 2
}

END {
  if (!found_required) {
    print "VERIFY_GATE_PARITY_ERROR: missing Required verification sequence section in plan" > "/dev/stderr"
    exit 2
  }
  if (command_count == 0) {
    print "VERIFY_GATE_PARITY_ERROR: required verification sequence has no commands" > "/dev/stderr"
    exit 2
  }
}
' "${plan_file}"
}

verify_verification_pace() {
  local config_file="$1"
  local expected_cmd="$2"

  awk -v expected_cmd="${expected_cmd}" '
BEGIN {
  in_section = 0
  found_section = 0
  found_cmds = 0
}

/^\[run\.paces\.verification\]$/ {
  in_section = 1
  found_section = 1
  next
}

/^\[[^]]+\]$/ {
  in_section = 0
  next
}

in_section && /^[[:space:]]*cmds[[:space:]]*=/ {
  line = $0
  sub(/^[[:space:]]*cmds[[:space:]]*=[[:space:]]*\[/, "", line)
  sub(/\][[:space:]]*$/, "", line)
  gsub(/[[:space:]]/, "", line)
  expected = "\"" expected_cmd "\""
  if (line == expected) {
    found_cmds = 1
  }
}

END {
  if (!found_section) {
    print "VERIFY_GATE_PARITY_ERROR: missing [run.paces.verification] in yarli config" > "/dev/stderr"
    exit 2
  }
  if (!found_cmds) {
    printf("VERIFY_GATE_PARITY_ERROR: [run.paces.verification].cmds must be [\"%s\"]\n", expected_cmd) > "/dev/stderr"
    exit 2
  }
}
' "${config_file}"
}

require_file "${PLAN_FILE}"
require_file "${RUNTIME_SCRIPT}"
require_file "${YARLI_CONFIG}"
[[ -x "${RUNTIME_SCRIPT}" ]] || fail "runtime verification script is not executable: ${RUNTIME_SCRIPT}"

verify_verification_pace "${YARLI_CONFIG}" "${EXPECTED_PACE_CMD}"

mapfile -t plan_cmds < <(extract_plan_commands "${PLAN_FILE}")
mapfile -t runtime_cmds < <("${RUNTIME_SCRIPT}" --print-commands)

[[ ${#plan_cmds[@]} -gt 0 ]] || fail "no commands parsed from plan"
[[ ${#runtime_cmds[@]} -gt 0 ]] || fail "no commands printed by runtime script"

if [[ ${#plan_cmds[@]} -ne ${#runtime_cmds[@]} ]]; then
  fail "command-count mismatch (plan=${#plan_cmds[@]} runtime=${#runtime_cmds[@]})"
fi

for idx in "${!plan_cmds[@]}"; do
  if [[ "${plan_cmds[$idx]}" != "${runtime_cmds[$idx]}" ]]; then
    fail "command mismatch at position $((idx + 1)): plan='${plan_cmds[$idx]}' runtime='${runtime_cmds[$idx]}'"
  fi
done

echo "VERIFY_GATE_PARITY_OK: command_count=${#plan_cmds[@]} plan=${PLAN_FILE} runtime=${RUNTIME_SCRIPT} config=${YARLI_CONFIG}"
