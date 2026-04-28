#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"
EVIDENCE_DIR="${REPO_ROOT}/.yarli/evidence"
RECOVERY_DIR="${REPO_ROOT}/.agent/auto-commit-recovery"
QUALITY_GATE="${SCRIPT_DIR}/check-commit-message-quality.sh"
VAGUE_PATTERN='^(feat|fix|chore|docs|refactor)\(.+\): update [[:alnum:]_]+$'
DEFAULT_RANGE="HEAD~10..HEAD"
SUBJECT_MAX=72

usage() {
  cat <<EOF
Usage:
  bin/yarli-rewrite-auto-commit-messages.sh [--range RANGE] [--apply] [--evidence-dir DIR]

Detects yarli/Codex auto-commits in RANGE (default ${DEFAULT_RANGE}) whose
subject matches the vague pattern '${VAGUE_PATTERN}', looks up the matching
.yarli/evidence/I<key>.md entry by committer date, and proposes a rewrite
derived from the evidence frontmatter summary.

Modes:
  (default)  dry-run; prints the proposed rewrites to stdout, writes a
             recovery plan under .agent/auto-commit-recovery/, and exits 0.
  --apply    rewrites the matching commits via 'git filter-branch
             --msg-filter' on the local branch only. Requires a clean
             working tree and an explicit confirmation env var:
             YARLI_AUTO_COMMIT_REWRITE_CONFIRM=1.

Idempotent: if no commit in RANGE matches the vague pattern, no rewrite is
proposed and the script exits 0 with count=0.

Final line: YARLI_AUTO_COMMIT_REWRITE_OK count=<N> mode=<dry-run|applied>
EOF
}

fail() {
  printf 'YARLI_AUTO_COMMIT_REWRITE_ERROR: %s\n' "$*" >&2
  exit 1
}

require_cmd() {
  command -v "$1" >/dev/null 2>&1 || fail "missing required command: $1"
}

if [[ "${1:-}" == "-h" || "${1:-}" == "--help" ]]; then
  usage
  exit 0
fi

require_cmd git
require_cmd awk
require_cmd grep

range="${DEFAULT_RANGE}"
mode="dry-run"
evidence_dir="${EVIDENCE_DIR}"

while [[ $# -gt 0 ]]; do
  case "$1" in
    --range) range="$2"; shift 2 ;;
    --apply) mode="applied"; shift ;;
    --evidence-dir) evidence_dir="$2"; shift 2 ;;
    *) fail "unknown argument: $1" ;;
  esac
done

cd "${REPO_ROOT}"
mkdir -p "${RECOVERY_DIR}"

# Date-keyed evidence index: build a list of (YYYY-MM-DD, tranche_key, summary)
# by scanning evidence files. Newer matches sort later.
build_evidence_index() {
  # Index only implementation evidence so audits/postmortems do not win
  # ties when multiple evidence files share a date and scope keyword.
  local f
  for f in "${evidence_dir}"/I*.md; do
    [[ -f "$f" ]] || continue
    local generated_at tranche summary task_type
    generated_at="$(awk -F'"' '/^generated_at = / { print $2; exit }' "$f")"
    tranche="$(awk -F'"' '/^tranche = / { print $2; exit }' "$f")"
    summary="$(awk -F'"' '/^summary = / { print $2; exit }' "$f")"
    task_type="$(awk -F'"' '/^task_type = / { print $2; exit }' "$f")"
    [[ -n "${tranche}" && -n "${summary}" ]] || continue
    [[ "${task_type}" == "implementation" ]] || continue
    [[ -n "${generated_at}" ]] || generated_at="0000-00-00"
    printf '%s\t%s\t%s\n' "${generated_at}" "${tranche}" "${summary}"
  done | sort
}

evidence_index="$(build_evidence_index)"

# Match a commit's date to the closest evidence entry on or before that date
# but after the previously matched evidence entry. Falls back to the last
# evidence entry on or before the commit date.
match_evidence_for_commit_date() {
  local commit_date="$1"
  awk -F'\t' -v cd="${commit_date}" '
    $1 != "" && $1 <= cd {
      key = $2
      summary = $3
    }
    END {
      printf "%s\t%s\n", key, summary
    }
  ' <<<"${evidence_index}"
}

# Refine the match by scoping to evidence files whose summary mentions the
# bad subject's scope hint (e.g., "web" or "status"). Useful when a single
# day landed multiple tranches: the scope hint disambiguates.
match_evidence_for_scope_and_date() {
  local scope_hint="$1"
  local commit_date="$2"
  if [[ -z "${scope_hint}" || "${scope_hint}" == "status" ]]; then
    match_evidence_for_commit_date "${commit_date}"
    return
  fi
  awk -F'\t' -v cd="${commit_date}" -v hint="${scope_hint}" '
    BEGIN { hint_lc = tolower(hint) }
    $1 != "" && $1 <= cd {
      summary_lc = tolower($3)
      if (index(summary_lc, hint_lc) > 0) {
        key = $2
        summary = $3
      }
    }
    END {
      if (key == "") {
        # Fallback: no scope-hint match; let caller fall back.
        exit 1
      }
      printf "%s\t%s\n", key, summary
    }
  ' <<<"${evidence_index}" || match_evidence_for_commit_date "${commit_date}"
}

derive_subject_from_summary() {
  local key="$1"
  local summary="$2"
  local type="refactor"
  local scope="${key,,}"
  local body="${summary}"

  # Truncate to fit Conventional-Commits 72-char limit.
  local prefix="${type}(${scope}): "
  local budget=$((SUBJECT_MAX - ${#prefix}))
  if (( ${#body} > budget )); then
    body="${body:0:$((budget - 3))}..."
  fi
  printf '%s%s\n' "${prefix}" "${body}"
}

# Build the rewrite plan: list of commit_sha + proposed message.
plan_file="${RECOVERY_DIR}/rewrite-plan-$(date -u +%Y%m%dT%H%M%SZ).txt"
mapping_file="${RECOVERY_DIR}/.rewrite-map-$$"
trap 'rm -f "${mapping_file}"' EXIT

count=0
{
  printf '# Yarli Auto-Commit Rewrite Plan\n'
  printf '# Generated: %s\n' "$(date -u +"%Y-%m-%dT%H:%M:%SZ")"
  printf '# Range: %s\n' "${range}"
  printf '# Vague pattern: %s\n' "${VAGUE_PATTERN}"
  printf '#\n'
} >"${plan_file}"

while IFS=$'\t' read -r sha subject committer_date; do
  [[ -z "${sha}" ]] && continue
  if [[ ! "${subject}" =~ ${VAGUE_PATTERN} ]]; then
    continue
  fi

  commit_day="${committer_date%%T*}"
  # Extract scope from the bad subject for disambiguation across same-day landings.
  scope_hint=""
  scope_extract='^[a-z]+\(([^)]+)\): update'
  if [[ "${subject}" =~ ${scope_extract} ]]; then
    scope_hint="${BASH_REMATCH[1]}"
  fi
  ev="$(match_evidence_for_scope_and_date "${scope_hint}" "${commit_day}")"
  ev_key="${ev%%$'\t'*}"
  ev_summary="${ev#*$'\t'}"

  if [[ -z "${ev_key}" || -z "${ev_summary}" ]]; then
    proposed="refactor: rewrite vague auto-commit (no evidence match for ${commit_day})"
  else
    proposed="$(derive_subject_from_summary "${ev_key}" "${ev_summary}")"
  fi

  {
    printf '\n## %s\n' "${sha}"
    printf '%s\n' "- old_subject: ${subject}"
    printf '%s\n' "- new_subject: ${proposed}"
    printf '%s\n' "- evidence: ${ev_key:-none}"
    printf '%s\n' "- committer_date: ${committer_date}"
  } >>"${plan_file}"

  printf '%s\t%s\n' "${sha}" "${proposed}" >>"${mapping_file}"
  count=$((count + 1))
done < <(git log --reverse --format='%H%x09%s%x09%cI' "${range}")

printf '\nGenerated rewrite plan: %s\n' "${plan_file}"

if [[ "${count}" -eq 0 ]]; then
  printf 'YARLI_AUTO_COMMIT_REWRITE_OK count=0 mode=%s\n' "${mode}"
  exit 0
fi

printf '\n## Plan Preview\n\n'
cat "${plan_file}"

if [[ "${mode}" == "applied" ]]; then
  if [[ "${YARLI_AUTO_COMMIT_REWRITE_CONFIRM:-0}" != "1" ]]; then
    fail "--apply requires YARLI_AUTO_COMMIT_REWRITE_CONFIRM=1 in the environment"
  fi
  if ! git diff --quiet || ! git diff --cached --quiet; then
    fail "working tree is dirty; commit or stash before --apply"
  fi

  # Validate every proposed message against the quality gate before rewriting.
  while IFS=$'\t' read -r sha proposed; do
    [[ -z "${sha}" ]] && continue
    if [[ -x "${QUALITY_GATE}" ]]; then
      "${QUALITY_GATE}" --message "${proposed}" >/dev/null \
        || fail "proposed message failed quality gate for ${sha}: ${proposed}"
    fi
  done <"${mapping_file}"

  # Use filter-branch in a tightly scoped way: rewrite messages for SHAs in
  # the mapping file. The msg-filter reads stdin (the message) and writes
  # stdout. We carry the mapping via a temp file in the env.
  export YARLI_REWRITE_MAPPING="${mapping_file}"
  export FILTER_BRANCH_SQUELCH_WARNING=1

  git filter-branch --force --msg-filter '
    sha=$GIT_COMMIT
    new_subject="$(awk -F"\t" -v want="$sha" "\$1==want { print \$2; exit }" "$YARLI_REWRITE_MAPPING")"
    if [[ -n "$new_subject" ]]; then
      printf "%s\n" "$new_subject"
      # Drop original first line (the vague subject), keep the rest.
      tail -n +2
    else
      cat
    fi
  ' "${range}" >/dev/null 2>&1 || fail "git filter-branch failed"

  printf '\nApplied rewrites for %d commits via git filter-branch.\n' "${count}"
fi

printf 'YARLI_AUTO_COMMIT_REWRITE_OK count=%d mode=%s\n' "${count}" "${mode}"
