#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"

CONFIG_FILE="${REPO_ROOT}/yarli.toml"
FALLBACK_FILE="${REPO_ROOT}/.agent/memory-log.md"
RUN_ID_INPUT=""
USE_LATEST=0
DRY_RUN=0

usage() {
  printf '%s\n' "Usage:"
  printf '%s\n' "  bin/yarli-postrun-memory-sync.sh --run-id <run-id|short-id>"
  printf '%s\n' "  bin/yarli-postrun-memory-sync.sh --latest"
  printf '%s\n' ""
  printf '%s\n' "Options:"
  printf '%s\n' "  --config <path>         Path to yarli.toml (default: ./yarli.toml)"
  printf '%s\n' "  --fallback-file <path>  Fallback memory log path (default: ./.agent/memory-log.md)"
  printf '%s\n' "  --dry-run               Print resolved payload/sink without writing"
  printf '%s\n' "  -h, --help              Show this help"
}

fail() {
  printf 'YARLI_POSTRUN_MEMORY_SYNC_ERROR: %s\n' "$*" >&2
  exit 1
}

require_file() {
  local path="$1"
  [[ -f "${path}" ]] || fail "missing file: ${path}"
}

strip_quotes() {
  local value="$1"
  value="${value%\"}"
  value="${value#\"}"
  printf '%s' "${value}"
}

sanitize_field() {
  local value="$1"
  value="$(printf '%s' "${value}" | tr '\n' ' ' | sed -E 's/[[:space:]]+/ /g; s/^ //; s/ $//')"
  printf '%s' "${value}"
}

normalize_bool() {
  local raw="${1:-}"
  raw="${raw,,}"
  case "${raw}" in
    true|1|yes|on)
      printf 'true'
      ;;
    false|0|no|off)
      printf 'false'
      ;;
    *)
      printf ''
      ;;
  esac
}

toml_get_value() {
  local file="$1"
  local section="$2"
  local key="$3"
  awk -v section="${section}" -v key="${key}" '
function trim(s) {
  sub(/^[[:space:]]+/, "", s)
  sub(/[[:space:]]+$/, "", s)
  return s
}
BEGIN {
  in_section = 0
}
{
  line = $0
  sub(/[[:space:]]*#.*/, "", line)
  if (line ~ /^[[:space:]]*\[[^]]+\][[:space:]]*$/) {
    section_name = line
    gsub(/^[[:space:]]*\[/, "", section_name)
    gsub(/\][[:space:]]*$/, "", section_name)
    in_section = (section_name == section)
    next
  }
  if (!in_section) {
    next
  }
  if (line ~ "^[[:space:]]*" key "[[:space:]]*=") {
    sub("^[[:space:]]*" key "[[:space:]]*=[[:space:]]*", "", line)
    print trim(line)
    exit
  }
}
' "${file}"
}

resolve_latest_run_id() {
  yarli run list | awk '
NR <= 2 { next }
$1 ~ /^[0-9a-f]+$/ { print $1; exit }
'
}

extract_run_id_from_status() {
  awk '/^Run / { print $2; exit }'
}

extract_run_state_from_status() {
  awk '/^State:/ { print $2; exit }'
}

extract_objective_from_status() {
  sed -nE 's/^Objective:[[:space:]]+//p' | head -n1
}

extract_tasks_line_from_status() {
  sed -nE 's/^Tasks:[[:space:]]+//p' | head -n1
}

extract_total_count() {
  local line="$1"
  printf '%s\n' "${line}" | sed -nE 's/^([0-9]+) total.*/\1/p'
}

extract_group_count() {
  local line="$1"
  local key="$2"
  local metrics
  metrics="$(printf '%s\n' "${line}" | sed -nE 's/^.*\((.*)\).*$/\1/p')"
  printf '%s\n' "${metrics}" \
    | tr ',' '\n' \
    | sed -nE "s/^[[:space:]]*([0-9]+)[[:space:]]+${key}[[:space:]]*$/\\1/p" \
    | head -n1
}

extract_failed_gates() {
  sed -nE 's/^Failed gates:[[:space:]]+([0-9]+).*/\1/p' | head -n1
}

extract_blocker_lines() {
  awk '
BEGIN {
  in_blockers = 0
}
/^Blocking tasks:$/ {
  in_blockers = 1
  next
}
in_blockers && /^[[:space:]]*$/ {
  in_blockers = 0
  next
}
in_blockers && /^Suggested actions:$/ {
  in_blockers = 0
  next
}
in_blockers && /^Sequence deterioration:/ {
  in_blockers = 0
  next
}
in_blockers {
  line = $0
  sub(/^[[:space:]]+/, "", line)
  if (length(line) > 0) {
    print line
  }
}
'
}

normalize_signature() {
  local raw="$1"
  raw="$(sanitize_field "${raw}")"
  printf '%s' "${raw}" \
    | tr '[:upper:]' '[:lower:]' \
    | sed -E 's/[0-9a-f]{8}-[0-9a-f-]{27}/<id>/g; s/[^a-z0-9]+/-/g; s/^-+//; s/-+$//'
}

append_fallback_entry() {
  local fallback_file="$1"
  local timestamp_utc="$2"
  local run_id="$3"
  local outcome="$4"
  local tags="$5"
  local reason="$6"
  local payload="$7"

  mkdir -p "$(dirname "${fallback_file}")"
  {
    printf '## YARLI_POSTRUN_MEMORY_V1 %s run_id=%s outcome=%s\n' "${timestamp_utc}" "${run_id}" "${outcome}"
    printf 'tags: %s\n' "${tags}"
    printf 'sink: fallback (%s)\n' "${reason}"
    printf '```text\n'
    printf '%s\n' "${payload}"
    printf '```\n\n'
  } >> "${fallback_file}"
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --run-id)
      [[ $# -ge 2 ]] || fail "--run-id requires a value"
      RUN_ID_INPUT="$2"
      shift 2
      ;;
    --latest)
      USE_LATEST=1
      shift
      ;;
    --config)
      [[ $# -ge 2 ]] || fail "--config requires a value"
      CONFIG_FILE="$2"
      shift 2
      ;;
    --fallback-file)
      [[ $# -ge 2 ]] || fail "--fallback-file requires a value"
      FALLBACK_FILE="$2"
      shift 2
      ;;
    --dry-run)
      DRY_RUN=1
      shift
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    --*)
      fail "unknown option: $1"
      ;;
    *)
      if [[ -n "${RUN_ID_INPUT}" ]]; then
        fail "unexpected extra positional argument: $1"
      fi
      RUN_ID_INPUT="$1"
      shift
      ;;
  esac
done

if [[ "${USE_LATEST}" -eq 1 ]]; then
  [[ -z "${RUN_ID_INPUT}" ]] || fail "use either --latest or --run-id, not both"
  RUN_ID_INPUT="$(resolve_latest_run_id)"
  [[ -n "${RUN_ID_INPUT}" ]] || fail "unable to resolve latest run id from 'yarli run list'"
fi

[[ -n "${RUN_ID_INPUT}" ]] || {
  usage >&2
  exit 2
}

require_file "${CONFIG_FILE}"

run_status="$(yarli run status "${RUN_ID_INPUT}")" || fail "unable to read run status for: ${RUN_ID_INPUT}"
run_id="$(printf '%s\n' "${run_status}" | extract_run_id_from_status)"
run_state="$(printf '%s\n' "${run_status}" | extract_run_state_from_status)"
objective_raw="$(printf '%s\n' "${run_status}" | extract_objective_from_status)"
tasks_line="$(printf '%s\n' "${run_status}" | extract_tasks_line_from_status)"

[[ -n "${run_id}" ]] || fail "unable to parse canonical run id from run status"
[[ -n "${run_state}" ]] || fail "unable to parse run state from run status"
[[ -n "${tasks_line}" ]] || fail "unable to parse tasks summary from run status"

case "${run_state}" in
  RunCompleted|RunFailed|RunCancelled)
    ;;
  *)
    fail "run ${run_id} is not finished (state=${run_state}); post-run sync requires terminal state"
    ;;
esac

run_explain="$(yarli run explain-exit "${run_id}")" || fail "unable to explain run exit for: ${run_id}"
task_list="$(yarli task list "${run_id}")" || fail "unable to list tasks for run: ${run_id}"

tasks_total="$(extract_total_count "${tasks_line}")"
tasks_complete="$(extract_group_count "${tasks_line}" "complete")"
tasks_failed="$(extract_group_count "${tasks_line}" "failed")"
tasks_blocked="$(extract_group_count "${tasks_line}" "blocked")"
tasks_active="$(extract_group_count "${tasks_line}" "active")"
failed_gates="$(printf '%s\n' "${run_explain}" | extract_failed_gates)"

tasks_total="${tasks_total:-0}"
tasks_complete="${tasks_complete:-0}"
tasks_failed="${tasks_failed:-0}"
tasks_blocked="${tasks_blocked:-0}"
tasks_active="${tasks_active:-0}"
failed_gates="${failed_gates:-0}"

mapfile -t blocker_lines < <(printf '%s\n' "${run_explain}" | extract_blocker_lines)

outcome="failed"
if [[ "${run_state}" == "RunCompleted" ]]; then
  outcome="success"
fi

verification_passed="no"
if [[ "${outcome}" == "success" && "${failed_gates}" == "0" && "${tasks_failed}" == "0" ]]; then
  verification_passed="yes"
fi

root_cause_task_id="none"
root_cause_reason="none"
root_cause_error="none"

if [[ "${outcome}" != "success" ]]; then
  root_cause_task_id="$(printf '%s\n' "${task_list}" \
    | awk '/^[[:space:]]+[0-9a-f-]+[[:space:]]+TaskFailed[[:space:]]+/ { print $1; exit }')"
  if [[ -z "${root_cause_task_id}" ]]; then
    root_cause_task_id="$(printf '%s\n' "${task_list}" \
      | awk '/^[[:space:]]+[0-9a-f-]+[[:space:]]+/ { print $1; exit }')"
  fi
  if [[ -n "${root_cause_task_id}" ]]; then
    task_explain="$(yarli task explain "${root_cause_task_id}" || true)"
    if [[ -n "${task_explain}" ]]; then
      root_cause_reason="$(printf '%s\n' "${task_explain}" | sed -nE 's/^Last reason:[[:space:]]+//p' | head -n1)"
      root_cause_error="$(printf '%s\n' "${task_explain}" | sed -nE 's/^Last error:[[:space:]]+//p' | head -n1)"
      root_cause_reason="${root_cause_reason:-none}"
      root_cause_error="${root_cause_error:-none}"
    fi
  fi
fi

declare -A signature_seen=()
declare -a blocker_signatures=()

add_signature() {
  local raw="$1"
  local signature
  signature="$(normalize_signature "${raw}")"
  [[ -n "${signature}" ]] || return 0
  if [[ -z "${signature_seen[${signature}]+x}" ]]; then
    blocker_signatures+=("${signature}")
    signature_seen["${signature}"]=1
  fi
}

for line in "${blocker_lines[@]}"; do
  add_signature "${line}"
done

if [[ "${root_cause_reason}" != "none" ]]; then
  add_signature "${root_cause_reason}"
fi

blocker_signature_csv="none"
if [[ ${#blocker_signatures[@]} -gt 0 ]]; then
  blocker_signature_csv="$(IFS=,; printf '%s' "${blocker_signatures[*]}")"
fi

haake_enabled_raw="$(toml_get_value "${CONFIG_FILE}" "memory.haake" "enabled")"
haake_command_raw="$(toml_get_value "${CONFIG_FILE}" "memory.haake" "command")"
haake_project_dir_raw="$(toml_get_value "${CONFIG_FILE}" "memory.haake" "project_dir")"
memory_enabled_raw="$(toml_get_value "${CONFIG_FILE}" "memory" "enabled")"
project_id_raw="$(toml_get_value "${CONFIG_FILE}" "memory" "project_id")"

haake_enabled="$(normalize_bool "${haake_enabled_raw}")"
memory_enabled="$(normalize_bool "${memory_enabled_raw}")"

haake_command="$(strip_quotes "${haake_command_raw:-haake}")"
haake_project_dir="$(strip_quotes "${haake_project_dir_raw:-${REPO_ROOT}}")"
project_id="$(strip_quotes "${project_id_raw:-$(basename "${REPO_ROOT}")}")"

if [[ -z "${haake_command}" ]]; then
  haake_command="haake"
fi
if [[ -z "${haake_project_dir}" ]]; then
  haake_project_dir="${REPO_ROOT}"
fi
if [[ -z "${project_id}" ]]; then
  project_id="$(basename "${REPO_ROOT}")"
fi

haake_effective_enabled="false"
if [[ "${haake_enabled}" == "true" ]]; then
  haake_effective_enabled="true"
fi
if [[ "${memory_enabled}" == "false" ]]; then
  haake_effective_enabled="false"
fi

timestamp_utc="$(date -u +"%Y-%m-%dT%H:%M:%SZ")"
objective="$(sanitize_field "${objective_raw}")"
run_state_tag="$(printf '%s' "${run_state}" | tr '[:upper:]' '[:lower:]' | sed -E 's/[^a-z0-9]+/-/g; s/^-+//; s/-+$//')"
tags="yarli,postrun-observability,haake-postrun-memory-sync,outcome-${outcome},run-state-${run_state_tag}"

root_cause_reason="$(sanitize_field "${root_cause_reason}")"
root_cause_error="$(sanitize_field "${root_cause_error}")"

printf -v payload '%s\n' \
  "yarli_postrun_memory_v1" \
  "timestamp_utc=${timestamp_utc}" \
  "project_id=${project_id}" \
  "run_id=${run_id}" \
  "objective=${objective}" \
  "outcome=${outcome}" \
  "run_state=${run_state}" \
  "verification_failed_gates=${failed_gates}" \
  "verification_passed=${verification_passed}" \
  "tasks_total=${tasks_total}" \
  "tasks_complete=${tasks_complete}" \
  "tasks_failed=${tasks_failed}" \
  "tasks_blocked=${tasks_blocked}" \
  "tasks_active=${tasks_active}" \
  "blocker_signatures=${blocker_signature_csv}" \
  "root_cause_task_id=${root_cause_task_id}" \
  "root_cause_reason=${root_cause_reason}" \
  "root_cause_error=${root_cause_error}" \
  "tags=${tags}"

sink="fallback"
fallback_reason="haake_disabled"
haake_memory_id=""

if [[ "${haake_effective_enabled}" == "true" ]]; then
  if ! command -v "${haake_command}" >/dev/null 2>&1; then
    fallback_reason="haake_command_missing"
  elif [[ ! -d "${haake_project_dir}" ]]; then
    fallback_reason="haake_project_dir_missing"
  else
    set +e
    haake_output="$(
      cd "${haake_project_dir}" && \
      "${haake_command}" memory insert "${project_id}" "${payload}" -t semantic \
        -m source=yarli-postrun-memory-sync \
        -m run_id="${run_id}" \
        -m outcome="${outcome}" \
        -m run_state="${run_state}" \
        -m tags="${tags}" 2>&1
    )"
    haake_rc=$?
    set -e
    if [[ ${haake_rc} -eq 0 ]]; then
      sink="haake"
      fallback_reason=""
      haake_memory_id="$(printf '%s\n' "${haake_output}" | sed -nE 's/.*Inserted memory:[[:space:]]+([^[:space:]]+).*/\1/p' | head -n1)"
    else
      fallback_reason="haake_insert_failed"
    fi
  fi
fi

if [[ "${DRY_RUN}" -eq 1 ]]; then
  printf 'YARLI_POSTRUN_MEMORY_SYNC_DRY_RUN: run_id=%s outcome=%s sink=%s reason=%s\n' \
    "${run_id}" "${outcome}" "${sink}" "${fallback_reason:-none}"
  printf '%s\n' "${payload}"
  exit 0
fi

if [[ "${sink}" == "haake" ]]; then
  printf 'YARLI_POSTRUN_MEMORY_SYNC_OK: run_id=%s outcome=%s sink=haake memory_id=%s\n' \
    "${run_id}" "${outcome}" "${haake_memory_id:-unknown}"
  exit 0
fi

append_fallback_entry "${FALLBACK_FILE}" "${timestamp_utc}" "${run_id}" "${outcome}" "${tags}" "${fallback_reason}" "${payload}"
printf 'YARLI_POSTRUN_MEMORY_SYNC_OK: run_id=%s outcome=%s sink=fallback file=%s reason=%s\n' \
  "${run_id}" "${outcome}" "${FALLBACK_FILE}" "${fallback_reason}"
