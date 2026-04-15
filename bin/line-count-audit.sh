#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"

PROFILES=(worktrees packages state policy)
STRICT=0

usage() {
  cat <<'EOF'
Usage:
  bin/line-count-audit.sh [--strict] [profile]
  bin/line-count-audit.sh --list-profiles

Profiles:
  worktrees  Audit worktree/runtime hotspot files.
  packages   Audit package/load-order hotspot files.
  state      Audit conversation/checkpoint hotspot files.
  policy     Audit plan-execution/permissions hotspot files.
  all        Run every profile.

Options:
  --strict   Fail on missing audit targets.
EOF
}

list_profiles() {
  printf '%s\n' "${PROFILES[@]}" all
}

fail() {
  echo "LINE_COUNT_AUDIT_ERROR: $*" >&2
  exit 1
}

line_count() {
  wc -l <"$1" | tr -d ' '
}

profile_rules() {
  case "$1" in
    worktrees)
      cat <<'EOF'
worktrees|amoebum/src/worktrees.lisp|2027
worktrees|amoebum/src/commands-agents.lisp|1294
EOF
      ;;
    packages)
      cat <<'EOF'
packages|amoebum/src/package.lisp|2561
EOF
      ;;
    state)
      cat <<'EOF'
state|amoebum/src/conversation.lisp|1221
state|amoebum/src/checkpoint.lisp|1660
EOF
      ;;
    policy)
      cat <<'EOF'
policy|amoebum/src/plan-execution.lisp|1532
policy|amoebum/src/permissions.lisp|1473
EOF
      ;;
    *)
      fail "unknown profile: $1"
      ;;
  esac
}

selected_profile="${1:-all}"
if [[ $# -gt 0 ]]; then
  case "${1}" in
    --help|-h)
      usage
      exit 0
      ;;
    --list-profiles)
      if [[ $# -ne 1 ]]; then
        usage >&2
        exit 2
      fi
      list_profiles
      exit 0
      ;;
    --strict)
      STRICT=1
      shift
      selected_profile="${1:-all}"
      ;;
  esac
fi

if [[ $# -gt 2 ]]; then
  usage >&2
  exit 2
fi

case "${selected_profile}" in
  all) ;;
  worktrees|packages|state|policy) ;;
  *)
    usage >&2
    exit 2
    ;;
esac

profiles_to_run=()
if [[ "${selected_profile}" == "all" ]]; then
  profiles_to_run=("${PROFILES[@]}")
else
  profiles_to_run=("${selected_profile}")
fi

total_checked=0
total_failed=0
total_missing=0

for profile in "${profiles_to_run[@]}"; do
  while IFS='|' read -r rule_profile relpath max_lines; do
    [[ -z "${relpath}" ]] && continue
    path="${REPO_ROOT}/${relpath}"
    if [[ ! -f "${path}" ]]; then
      total_missing=$((total_missing + 1))
      status="MISSING"
      echo "LINE_COUNT_AUDIT profile=${rule_profile} path=${relpath} max=${max_lines} status=${status}"
      if [[ "${STRICT}" -eq 1 ]]; then
        total_failed=$((total_failed + 1))
      fi
      continue
    fi

    total_checked=$((total_checked + 1))
    actual_lines="$(line_count "${path}")"
    status="PASS"
    if (( actual_lines > max_lines )); then
      total_failed=$((total_failed + 1))
      status="FAIL"
    fi
    echo "LINE_COUNT_AUDIT profile=${rule_profile} path=${relpath} lines=${actual_lines} max=${max_lines} status=${status}"
  done < <(profile_rules "${profile}")
done

if (( total_failed > 0 )); then
  echo "LINE_COUNT_AUDIT_FAIL profiles=${selected_profile} checked=${total_checked} missing=${total_missing} failed=${total_failed}" >&2
  exit 1
fi

echo "LINE_COUNT_AUDIT_OK profiles=${selected_profile} checked=${total_checked} missing=${total_missing}"
