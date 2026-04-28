#!/usr/bin/env bash
set -euo pipefail

readonly VAGUE_SUBJECT_PATTERN='^(feat|fix|chore|docs|refactor)\(.+\): update [[:alnum:]_]+$'

fail() {
  printf 'COMMIT_MESSAGE_QUALITY_FAIL: %s\n' "$*" >&2
  exit 1
}

usage() {
  cat <<'EOF'
Usage:
  check-commit-message-quality.sh --file <commit-msg-file>
  check-commit-message-quality.sh --message <subject-line>
  check-commit-message-quality.sh --range <git-rev-range>
EOF
}

normalize_subject() {
  local subject="$1"
  subject="${subject%$'\r'}"
  printf '%s' "${subject}"
}

validate_subject() {
  local subject
  local source="$2"

  subject="$(normalize_subject "$1")"
  if [[ "${subject}" =~ ${VAGUE_SUBJECT_PATTERN} ]]; then
    fail "${source} uses a vague subject line that matches the blocked rewrite pattern: ${subject}"
  fi
}

check_file() {
  local path="$1"
  local subject=""

  [[ -f "${path}" ]] || fail "commit message file not found: ${path}"
  IFS= read -r subject < "${path}" || true
  validate_subject "${subject}" "${path}"
  printf 'COMMIT_MESSAGE_QUALITY_OK: file=%s subject=%q\n' "${path}" "$(normalize_subject "${subject}")"
}

check_message() {
  local subject="$1"

  validate_subject "${subject}" "inline subject"
  printf 'COMMIT_MESSAGE_QUALITY_OK: subject=%q\n' "$(normalize_subject "${subject}")"
}

check_range() {
  local range="$1"
  local commits_checked=0
  local line=""
  local commit_hash=""
  local subject=""

  while IFS= read -r line; do
    [[ -n "${line}" ]] || continue
    commits_checked=$((commits_checked + 1))
    commit_hash="${line%%$'\t'*}"
    subject="${line#*$'\t'}"
    validate_subject "${subject}" "commit ${commit_hash}"
  done < <(git log --format='%H%x09%s' "${range}")

  if [[ "${commits_checked}" -eq 0 ]]; then
    fail "no commits found for range: ${range}"
  fi

  printf 'COMMIT_MESSAGE_QUALITY_OK: range=%s commits_checked=%s\n' "${range}" "${commits_checked}"
}

main() {
  [[ "$#" -ge 1 ]] || {
    usage >&2
    exit 1
  }

  case "$1" in
    --file)
      [[ "$#" -eq 2 ]] || fail "--file expects exactly one path argument"
      check_file "$2"
      ;;
    --message)
      [[ "$#" -eq 2 ]] || fail "--message expects exactly one subject argument"
      check_message "$2"
      ;;
    --range)
      [[ "$#" -eq 2 ]] || fail "--range expects exactly one rev-range argument"
      check_range "$2"
      ;;
    -h|--help)
      usage
      ;;
    *)
      usage >&2
      fail "unknown mode: $1"
      ;;
  esac
}

main "$@"
