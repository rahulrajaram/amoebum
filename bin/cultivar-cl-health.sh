#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"
DEFAULT_CULTIVAR_BINARY="${HOME}/Documents/cultivar/target/release/cultivar"
CULTIVAR_BINARY_PATH="${CULTIVAR_BINARY:-$(command -v cultivar 2>/dev/null || echo "${DEFAULT_CULTIVAR_BINARY}")}"

find_health_root() {
  local candidate

  if [[ -f "${REPO_ROOT}/PROMPT.md" && -f "${REPO_ROOT}/IMPLEMENTATION_PLAN.md" && -d "${REPO_ROOT}/.agent/cultivar-index" ]]; then
    printf '%s\n' "${REPO_ROOT}"
    return 0
  fi

  while IFS= read -r candidate; do
    if [[ -f "${candidate}/PROMPT.md" && -f "${candidate}/IMPLEMENTATION_PLAN.md" && -d "${candidate}/.agent/cultivar-index" ]]; then
      printf '%s\n' "${candidate}"
      return 0
    fi
  done < <(git -C "${REPO_ROOT}" worktree list --porcelain | awk '/^worktree / {print $2}')

  if [[ -d "${REPO_ROOT}/.agent/cultivar-index" ]]; then
    printf '%s\n' "${REPO_ROOT}"
    return 0
  fi

  while IFS= read -r candidate; do
    if [[ -d "${candidate}/.agent/cultivar-index" ]]; then
      printf '%s\n' "${candidate}"
      return 0
    fi
  done < <(git -C "${REPO_ROOT}" worktree list --porcelain | awk '/^worktree / {print $2}')

  return 1
}

write_status_report() {
  local report_path="$1"
  local generated_at="$2"
  local status="$3"
  local reference_mode="$4"
  local index_health_summary="$5"
  local doctor_exit_code="$6"
  local summary="$7"
  local navigation_warning="$8"

  mkdir -p "$(dirname "${report_path}")"
  cat > "${report_path}" <<EOF
schema_version=1
generated_at=${generated_at}
status=${status}
reference_mode=${reference_mode}
index_health_summary=${index_health_summary}
doctor_exit_code=${doctor_exit_code}
summary=${summary}
navigation_warning=${navigation_warning}
EOF
}

write_doctor_log() {
  local log_path="$1"
  local doctor_output="$2"

  mkdir -p "$(dirname "${log_path}")"
  printf '%s\n' "${doctor_output}" > "${log_path}"
}

health_root="$(find_health_root)" || {
  echo "CULTIVAR_CL_HEALTH_FAIL reason=no_health_root_with_cultivar_index" >&2
  exit 1
}

index_path="${health_root}/.agent/cultivar-index"
status_report="${health_root}/.agent/cultivar-cl-health.status"
doctor_log="${health_root}/.agent/cultivar-cl-health.doctor.txt"
mirror_status_report=""
mirror_doctor_log=""
generated_at="$(date +%F)"

if [[ "${REPO_ROOT}" != "${health_root}" ]]; then
  mirror_status_report="${REPO_ROOT}/.agent/cultivar-cl-health.status"
  mirror_doctor_log="${REPO_ROOT}/.agent/cultivar-cl-health.doctor.txt"
fi

if [[ ! -x "${CULTIVAR_BINARY_PATH}" ]]; then
  write_status_report "${status_report}" "${generated_at}" "fail" "missing" "UNKNOWN" "127" \
    "Cultivar binary missing; Common Lisp health could not be checked." \
    "Future decomposition runs should treat Cultivar as unavailable until the binary is restored."
  echo "CULTIVAR_CL_HEALTH_FAIL status=fail binary=${CULTIVAR_BINARY_PATH} root=${health_root} reason=missing_binary" >&2
  exit 1
fi

set +e
doctor_output="$("${CULTIVAR_BINARY_PATH}" doctor --index "${index_path}" 2>&1)"
doctor_exit=$?
set -e

write_doctor_log "${doctor_log}" "${doctor_output}"
if [[ -n "${mirror_doctor_log}" ]]; then
  write_doctor_log "${mirror_doctor_log}" "${doctor_output}"
fi

index_health_summary="$(printf '%s\n' "${doctor_output}" | sed -n 's/^  Index health summary: //p' | head -n1)"
if [[ -z "${index_health_summary}" ]]; then
  index_health_summary="UNKNOWN"
fi

status="fail"
reference_mode="unknown"
summary="Cultivar Common Lisp health check failed."
navigation_warning="Treat Cultivar as unavailable for decomposition decisions until this health check passes."

if printf '%s\n' "${doctor_output}" | grep -Fq "commonlisp [structural_only]"; then
  status="advisory"
  reference_mode="structural_only"
  summary="Common Lisp index degraded but still usable for structural slices."
  navigation_warning="Fresh-symbol navigation is advisory only; use rg plus direct file reads when refactors depend on exact references."
elif [[ ${doctor_exit} -eq 0 ]] && printf '%s\n' "${doctor_output}" | grep -Fq "Checking Common Lisp LSP (cl-lsp)... OK"; then
  status="pass"
  reference_mode="full"
  summary="Common Lisp index healthy for navigation and structural slices."
  navigation_warning=""
elif printf '%s\n' "${doctor_output}" | grep -Fq "snapshot.sqlite... NOT FOUND"; then
  status="fail"
  reference_mode="missing"
  summary="Cultivar index snapshot missing; Common Lisp health cannot be trusted."
  navigation_warning="Run cultivar index build before using Cultivar for decomposition navigation."
fi

write_status_report "${status_report}" "${generated_at}" "${status}" "${reference_mode}" "${index_health_summary}" "${doctor_exit}" "${summary}" "${navigation_warning}"
if [[ -n "${mirror_status_report}" ]]; then
  write_status_report "${mirror_status_report}" "${generated_at}" "${status}" "${reference_mode}" "${index_health_summary}" "${doctor_exit}" "${summary}" "${navigation_warning}"
fi

if [[ "${status}" == "fail" ]]; then
  echo "CULTIVAR_CL_HEALTH_FAIL status=${status} mode=${reference_mode} index_health=${index_health_summary} root=${health_root} report=${status_report} doctor_log=${doctor_log}" >&2
  echo "summary=${summary}" >&2
  echo "warning=${navigation_warning}" >&2
  exit 1
fi

echo "CULTIVAR_CL_HEALTH_OK status=${status} mode=${reference_mode} index_health=${index_health_summary} root=${health_root} report=${status_report} doctor_log=${doctor_log}"
echo "summary=${summary}"
if [[ -n "${navigation_warning}" ]]; then
  echo "warning=${navigation_warning}"
fi
