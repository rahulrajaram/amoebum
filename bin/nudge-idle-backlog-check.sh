#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"
PLAN_FILE="${REPO_ROOT}/IMPLEMENTATION_PLAN.md"
IDLE_THRESHOLD_MINUTES="${NUDGE_AMOEBUM_IDLE_MINUTES:-45}"

if [[ ! "${IDLE_THRESHOLD_MINUTES}" =~ ^[0-9]+$ ]]; then
  IDLE_THRESHOLD_MINUTES=45
fi

find_vision_file() {
  local candidate
  for candidate in "${REPO_ROOT}/VISION.md" "${REPO_ROOT}/visions.md" "${REPO_ROOT}/Vision.md" "${REPO_ROOT}/vision.md"; do
    if [[ -f "${candidate}" ]]; then
      printf '%s\n' "${candidate}"
      return 0
    fi
  done
  return 1
}

count_unchecked_items() {
  local file_path="$1"
  awk '
    /^[[:space:]]*[-*][[:space:]]+\[[[:space:]]\]/ { count += 1 }
    END { printf "%d\n", count + 0 }
  ' "${file_path}"
}

open_tranches_count() {
  local lint_output
  lint_output="$("${SCRIPT_DIR}/yarli-lint-implementation-plan.sh" "${PLAN_FILE}" 2>/dev/null || true)"
  sed -n 's/.*open_tranches=\([0-9][0-9]*\).*/\1/p' <<< "${lint_output}" | head -n1
}

is_local_yarli_run_active() {
  local pid
  local cwd
  local cmdline

  if ! command -v pgrep >/dev/null 2>&1; then
    return 1
  fi

  while read -r pid; do
    [[ -n "${pid}" ]] || continue
    cwd="$(readlink -f "/proc/${pid}/cwd" 2>/dev/null || true)"
    [[ -n "${cwd}" ]] || continue
    if [[ "${cwd}" != "${REPO_ROOT}" && "${cwd}" != "${REPO_ROOT}/"* ]]; then
      continue
    fi
    cmdline="$(tr '\0' ' ' < "/proc/${pid}/cmdline" 2>/dev/null || true)"
    if [[ "${cmdline}" == *"yarli run"* ]]; then
      return 0
    fi
  done < <(pgrep -f "yarli run" || true)

  return 1
}

last_activity_epoch() {
  if [[ -f "${REPO_ROOT}/.yarl/audit.jsonl" ]]; then
    stat -c %Y "${REPO_ROOT}/.yarl/audit.jsonl"
    return 0
  fi

  if git -C "${REPO_ROOT}" rev-parse --is-inside-work-tree >/dev/null 2>&1; then
    git -C "${REPO_ROOT}" log -1 --format=%ct 2>/dev/null || true
    return 0
  fi

  date +%s
}

if [[ "${NUDGE_AMOEBUM_IGNORE_ACTIVE:-0}" != "1" ]] && is_local_yarli_run_active; then
  exit 0
fi

vision_file="$(find_vision_file || true)"
vision_unchecked=0
if [[ -n "${vision_file}" ]]; then
  vision_unchecked="$(count_unchecked_items "${vision_file}")"
fi

open_tranches="$(open_tranches_count)"
if [[ -z "${open_tranches}" ]]; then
  open_tranches=0
fi

if (( vision_unchecked == 0 && open_tranches == 0 )); then
  exit 0
fi

now_epoch="$(date +%s)"
last_epoch="$(last_activity_epoch)"

if [[ ! "${last_epoch}" =~ ^[0-9]+$ ]]; then
  last_epoch="${now_epoch}"
fi

if (( last_epoch > now_epoch )); then
  last_epoch="${now_epoch}"
fi

idle_minutes="$(((now_epoch - last_epoch) / 60))"
if (( idle_minutes < IDLE_THRESHOLD_MINUTES )); then
  exit 0
fi

vision_label="VISION.md"
if [[ -n "${vision_file}" ]]; then
  vision_label="$(basename "${vision_file}")"
fi

printf 'idle %dm; %d open tranches and %d unchecked items in %s\n' \
  "${idle_minutes}" \
  "${open_tranches}" \
  "${vision_unchecked}" \
  "${vision_label}"
