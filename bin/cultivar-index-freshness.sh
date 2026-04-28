#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"

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

stat_mtime_epoch() {
  local path="$1"
  stat -c '%Y' "${path}"
}

stat_mtime_iso() {
  local path="$1"
  date -u -d "@$(stat_mtime_epoch "${path}")" +"%Y-%m-%dT%H:%M:%SZ"
}

write_status_report() {
  local report_path="$1"
  local generated_at="$2"
  local status="$3"
  local snapshot_path="$4"
  local snapshot_mtime="$5"
  local newest_source_path="$6"
  local newest_source_mtime="$7"
  local warning="$8"

  mkdir -p "$(dirname "${report_path}")"
  {
    printf 'schema_version=1\n'
    printf 'generated_at=%s\n' "${generated_at}"
    printf 'freshness_status=%s\n' "${status}"
    printf 'snapshot_path=%s\n' "${snapshot_path}"
    printf 'snapshot_mtime=%s\n' "${snapshot_mtime}"
    printf 'newest_source_path=%s\n' "${newest_source_path}"
    printf 'newest_source_mtime=%s\n' "${newest_source_mtime}"
    printf 'freshness_warning=%s\n' "${warning}"
  } > "${report_path}"
}

health_root="$(find_health_root)" || {
  printf 'CULTIVAR_INDEX_FRESHNESS_FAIL reason=no_health_root_with_cultivar_index\n' >&2
  exit 1
}

index_path="${health_root}/.agent/cultivar-index"
snapshot_path="${index_path}/snapshot.sqlite"
status_report="${index_path}/freshness.status"
generated_at="$(date -u +"%Y-%m-%dT%H:%M:%SZ")"

if [[ ! -f "${snapshot_path}" ]]; then
  write_status_report "${status_report}" "${generated_at}" "missing" "${snapshot_path}" "missing" "unknown" "unknown" \
    "Cultivar snapshot.sqlite is missing; rebuild the index before relying on Cultivar."
  printf 'CULTIVAR_INDEX_FRESHNESS_OK status=missing snapshot=%s report=%s warning=%s\n' \
    "${snapshot_path}" "${status_report}" "snapshot_missing"
  exit 0
fi

newest_source="$(
  find "${health_root}/amoebum" "${health_root}/pseudopod" "${health_root}/sw4rm-sdk" "${health_root}/ptui" \
    -type f \( -name '*.lisp' -o -name '*.asd' \) \
    -printf '%T@ %p\n' 2>/dev/null \
    | sort -nr \
    | sed -n '1s/^[^ ]* //p'
)"

if [[ -z "${newest_source}" ]]; then
  write_status_report "${status_report}" "${generated_at}" "unknown" "${snapshot_path}" "$(stat_mtime_iso "${snapshot_path}")" "unknown" "unknown" \
    "No Lisp or ASD source files were found for freshness comparison."
  printf 'CULTIVAR_INDEX_FRESHNESS_OK status=unknown snapshot=%s report=%s warning=%s\n' \
    "${snapshot_path}" "${status_report}" "no_sources_found"
  exit 0
fi

snapshot_epoch="$(stat_mtime_epoch "${snapshot_path}")"
source_epoch="$(stat_mtime_epoch "${newest_source}")"
snapshot_mtime="$(stat_mtime_iso "${snapshot_path}")"
source_mtime="$(stat_mtime_iso "${newest_source}")"
status="fresh"
warning=""

if [[ "${source_epoch}" -gt "${snapshot_epoch}" ]]; then
  status="stale"
  warning="Cultivar snapshot is older than Lisp/ASD sources; use structural slices cautiously and fall back to rg plus direct file reads for exact references."
fi

write_status_report "${status_report}" "${generated_at}" "${status}" "${snapshot_path}" "${snapshot_mtime}" "${newest_source}" "${source_mtime}" "${warning}"

printf 'CULTIVAR_INDEX_FRESHNESS_OK status=%s snapshot=%s snapshot_mtime=%s newest_source=%s newest_source_mtime=%s report=%s\n' \
  "${status}" "${snapshot_path}" "${snapshot_mtime}" "${newest_source}" "${source_mtime}" "${status_report}"
if [[ -n "${warning}" ]]; then
  printf 'warning=%s\n' "${warning}"
fi
