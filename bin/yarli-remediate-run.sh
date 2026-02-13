#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"
DEFAULT_TEMPLATE="${REPO_ROOT}/.yarli/prompts/remediation/run-failure-template.md"

usage() {
  printf '%s\n' "Usage:"
  printf '%s\n' "  bin/yarli-remediate-run.sh <run-id> [--dispatch-cmd <cmd>] [--template <path>] [--dry-run]"
  printf '%s\n' ""
  printf '%s\n' "Examples:"
  printf '%s\n' "  bin/yarli-remediate-run.sh 019c4f70-07a7-7703-bfa6-5d7a5f19948c"
  printf '%s\n' "  bin/yarli-remediate-run.sh 019c4f7007 --dispatch-cmd \"bash -lc 'echo remediation stub'\""
}

fail() {
  printf 'YARLI_REMEDIATE_ERROR: %s\n' "$*" >&2
  exit 1
}

require_file() {
  local path="$1"
  [[ -f "${path}" ]] || fail "missing file: ${path}"
}

extract_run_id_from_status() {
  awk '/^Run / {print $2; exit}'
}

extract_run_state_from_status() {
  awk '/^State:/ {print $2; exit}'
}

extract_candidate_task_ids() {
  awk '
/^[[:space:]]+[0-9a-f-]+[[:space:]]+/ {
  if ($2 != "TaskComplete") {
    print $1
  }
}
'
}

extract_all_task_ids() {
  awk '
/^[[:space:]]+[0-9a-f-]+[[:space:]]+/ {
  print $1
}
'
}

extract_run_id_from_run_start_output() {
  awk -F'"' '/"run_id":"[0-9a-f-]+"/ {print $4; exit}'
}

run_id_input=""
template_path="${DEFAULT_TEMPLATE}"
dispatch_cmd=""
dry_run=0

while [[ $# -gt 0 ]]; do
  case "$1" in
    --dispatch-cmd)
      [[ $# -ge 2 ]] || fail "--dispatch-cmd requires a value"
      dispatch_cmd="$2"
      shift 2
      ;;
    --template)
      [[ $# -ge 2 ]] || fail "--template requires a value"
      template_path="$2"
      shift 2
      ;;
    --dry-run)
      dry_run=1
      shift
      ;;
    --help|-h)
      usage
      exit 0
      ;;
    --*)
      fail "unknown option: $1"
      ;;
    *)
      if [[ -n "${run_id_input}" ]]; then
        fail "unexpected extra positional argument: $1"
      fi
      run_id_input="$1"
      shift
      ;;
  esac
done

if [[ -z "${run_id_input}" ]]; then
  usage >&2
  exit 2
fi

require_file "${template_path}"

run_status="$(yarli run status "${run_id_input}")" || fail "unable to read run status for: ${run_id_input}"
run_id="$(printf '%s\n' "${run_status}" | extract_run_id_from_status)"
run_state="$(printf '%s\n' "${run_status}" | extract_run_state_from_status)"

[[ -n "${run_id}" ]] || fail "unable to parse canonical run id from run status"
[[ -n "${run_state}" ]] || fail "unable to parse run state from run status"

if [[ "${run_state}" == "RunCompleted" ]]; then
  fail "run ${run_id} is completed; remediation path expects failed/cancelled/active investigation"
fi

run_explain="$(yarli run explain-exit "${run_id}")" || fail "unable to explain run exit for: ${run_id}"
task_list="$(yarli task list "${run_id}")" || fail "unable to list tasks for run: ${run_id}"

mapfile -t task_ids < <(printf '%s\n' "${task_list}" | extract_candidate_task_ids)
if [[ ${#task_ids[@]} -eq 0 ]]; then
  mapfile -t task_ids < <(printf '%s\n' "${task_list}" | extract_all_task_ids)
fi
[[ ${#task_ids[@]} -gt 0 ]] || fail "no tasks found for run: ${run_id}"

artifact_dir="${REPO_ROOT}/.agent/remediation-${run_id}"
tasks_dir="${artifact_dir}/tasks"
mkdir -p "${tasks_dir}"

run_status_file="${artifact_dir}/run-status.txt"
run_explain_file="${artifact_dir}/run-explain-exit.txt"
task_list_file="${artifact_dir}/task-list.txt"
summary_file="${artifact_dir}/summary.md"
prompt_file="${artifact_dir}/prompt.md"
dispatch_output_file="${artifact_dir}/dispatch-output.txt"

printf '%s\n' "${run_status}" > "${run_status_file}"
printf '%s\n' "${run_explain}" > "${run_explain_file}"
printf '%s\n' "${task_list}" > "${task_list_file}"

for task_id in "${task_ids[@]}"; do
  task_explain="$(yarli task explain "${task_id}")" || fail "unable to explain task: ${task_id}"
  printf '%s\n' "${task_explain}" > "${tasks_dir}/${task_id}.txt"
done

{
  printf '# Yarli Remediation Context\n\n'
  printf '%s\n' "- Source run id: \`${run_id}\`"
  printf '%s\n' "- Source run state: \`${run_state}\`"
  printf '%s\n' "- Captured at (UTC): \`$(date -u +"%Y-%m-%dT%H:%M:%SZ")\`"
  printf '%s\n' "- Candidate task ids:"
  for task_id in "${task_ids[@]}"; do
    printf '  - `%s`\n' "${task_id}"
  done
  printf '\n'
  printf '%s\n' "- Files:"
  printf '%s\n' '  - `run-status.txt`'
  printf '%s\n' '  - `run-explain-exit.txt`'
  printf '%s\n' '  - `task-list.txt`'
  printf '%s\n' '  - `tasks/<task-id>.txt`'
  printf '%s\n' '  - `prompt.md`'
  printf '%s\n' '  - `dispatch-output.txt`'
} > "${summary_file}"

cp "${template_path}" "${prompt_file}"
{
  printf '\n\n## Generated Remediation Context\n\n'
  printf '%s\n' "- Originating run id: \`${run_id}\`"
  printf '%s\n' "- Originating run state: \`${run_state}\`"
  printf '%s\n' "- Artifact directory: \`${artifact_dir}\`"
  printf '%s' '- Candidate tasks:'
  for task_id in "${task_ids[@]}"; do
    printf ' `%s`' "${task_id}"
  done
  printf '\n\n'
  printf '### Run Status\n'
  printf '```text\n'
  printf '%s\n' "${run_status}"
  printf '```\n\n'
  printf '### Run Explain Exit\n'
  printf '```text\n'
  printf '%s\n' "${run_explain}"
  printf '```\n\n'
  printf '### Task List\n'
  printf '```text\n'
  printf '%s\n' "${task_list}"
  printf '```\n\n'
} >> "${prompt_file}"

for task_id in "${task_ids[@]}"; do
  task_file="${tasks_dir}/${task_id}.txt"
  {
    printf '### Task Explain `%s`\n' "${task_id}"
    printf '```text\n'
    sed -n '1,200p' "${task_file}"
    printf '```\n\n'
  } >> "${prompt_file}"
done

if [[ -z "${dispatch_cmd}" ]]; then
  dispatch_cmd="${REPO_ROOT}/bin/yarli-codex.sh ${prompt_file}"
fi

if [[ "${dry_run}" -eq 1 ]]; then
  printf 'YARLI_REMEDIATE_DRY_RUN: source_run=%s prompt=%s artifact_dir=%s dispatch_cmd=%s\n' \
    "${run_id}" "${prompt_file}" "${artifact_dir}" "${dispatch_cmd}"
  exit 0
fi

set +e
dispatch_output="$(yarli run start --stream "remediation for run ${run_id}" --cmd "${dispatch_cmd}" 2>&1)"
dispatch_rc=$?
set -e

printf '%s\n' "${dispatch_output}" > "${dispatch_output_file}"
if [[ ${dispatch_rc} -ne 0 ]]; then
  fail "remediation dispatch failed (source_run=${run_id}, output=${dispatch_output_file})"
fi

remediation_run_id="$(printf '%s\n' "${dispatch_output}" | extract_run_id_from_run_start_output)"
[[ -n "${remediation_run_id}" ]] || fail "unable to parse remediation run id from dispatch output"

{
  printf '\n## Dispatch Result\n\n'
  printf '%s\n' "- Dispatch command: \`${dispatch_cmd}\`"
  printf '%s\n' "- Remediation run id: \`${remediation_run_id}\`"
} >> "${summary_file}"

printf 'YARLI_REMEDIATE_OK: source_run=%s remediation_run=%s prompt=%s artifact_dir=%s\n' \
  "${run_id}" "${remediation_run_id}" "${prompt_file}" "${artifact_dir}"
