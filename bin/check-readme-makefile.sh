#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"
README_PATH="${REPO_ROOT}/README.md"
MAKEFILE_PATH="${REPO_ROOT}/Makefile"

fail() {
  printf 'README_MAKEFILE_CONGRUENCE_FAIL: %s\n' "$*" >&2
  exit 1
}

require_cmd() {
  local cmd="$1"
  command -v "${cmd}" >/dev/null 2>&1 || fail "missing required command: ${cmd}"
}

quote_inline() {
  printf '`%s`' "$1"
}

format_backtick_list() {
  local -a items=("$@")
  local count="${#items[@]}"
  local i=0

  [[ "${count}" -gt 0 ]] || fail "cannot format empty command list"

  if [[ "${count}" -eq 1 ]]; then
    quote_inline "${items[0]}"
    return 0
  fi

  if [[ "${count}" -eq 2 ]]; then
    printf '%s and %s' "$(quote_inline "${items[0]}")" "$(quote_inline "${items[1]}")"
    return 0
  fi

  printf '%s' "$(quote_inline "${items[0]}")"
  for ((i = 1; i < count - 1; i++)); do
    printf ', %s' "$(quote_inline "${items[i]}")"
  done
  printf ', and %s' "$(quote_inline "${items[count - 1]}")"
}

read_dry_run_commands() {
  local target="$1"

  (
    cd "${REPO_ROOT}"
    make --dry-run --always-make --no-print-directory "${target}"
  ) | awk '
    /^make(\[[0-9]+\])?: (Entering|Leaving) directory/ { next }
    /^[[:space:]]*$/ { next }
    {
      line = $0
      sub(/\r$/, "", line)
      if (pending != "") {
        pending = pending " " line
      } else {
        pending = line
      }
      if (pending ~ /\\$/) {
        sub(/\\$/, "", pending)
        next
      }
      gsub(/[[:space:]]+/, " ", pending)
      sub(/^ /, "", pending)
      sub(/ $/, "", pending)
      print pending
      pending = ""
    }
    END {
      if (pending != "") {
        gsub(/[[:space:]]+/, " ", pending)
        sub(/^ /, "", pending)
        sub(/ $/, "", pending)
        print pending
      }
    }
  '
}

read_readme_section_lines() {
  awk '
    /^## Local Build and Check Commands$/ { in_section = 1; next }
    /^## / && in_section { exit }
    in_section {
      if ($0 ~ /^[0-9]+\.[[:space:]]+/) {
        collecting = 1
        line = $0
        sub(/^[0-9]+\.[[:space:]]+/, "", line)
        print line
        next
      }
      if (collecting && $0 !~ /^[[:space:]]*$/) {
        exit
      }
    }
  ' "${README_PATH}"
}

join_lines() {
  local -a lines=("$@")
  printf '%s\n' "${lines[@]}"
}

require_line() {
  local target="$1"
  local haystack="$2"
  local needle="$3"

  [[ "${haystack}" == *"${needle}"* ]] || fail "target ${target} dry-run missing ${needle}"
}

expected_line_for_target() {
  local target="$1"
  local dry_run_text="$2"
  shift 2
  local -a dry_run_lines=("$@")
  local -a matched=()
  local -a expected_make_lines=()
  local command=""
  local index=0

  case "${target}" in
    test-ptui)
      require_line "${target}" "${dry_run_text}" "ptui/bin/ensure-quicklisp.sh >/dev/null"
      require_line "${target}" "${dry_run_text}" "sbcl --noinform --non-interactive"
      require_line "${target}" "${dry_run_text}" "(asdf:test-system :ptui/tests)"
      printf '%s\n' '`make test-ptui` runs `ptui/bin/ensure-quicklisp.sh` and then `sbcl --noinform --non-interactive` to load/test `:ptui/tests`.'
      ;;
    test-amoebum)
      require_line "${target}" "${dry_run_text}" "ptui/bin/ensure-quicklisp.sh >/dev/null"
      require_line "${target}" "${dry_run_text}" 'tmp/amoebum-test-failures.log'
      require_line "${target}" "${dry_run_text}" "AMOEBUM_TEST_FAILURE_SUMMARY="
      require_line "${target}" "${dry_run_text}" "sbcl --noinform --non-interactive"
      require_line "${target}" "${dry_run_text}" "(asdf:test-system :amoebum/test)"
      printf '%s\n' '`make test-amoebum` runs `ptui/bin/ensure-quicklisp.sh`, clears `tmp/amoebum-test-failures.log`, and then `sbcl --noinform --non-interactive` to load/test `:amoebum/test`.'
      ;;
    test)
      for command in "${dry_run_lines[@]}"; do
        [[ "${command}" == make\ * ]] || continue
        matched+=("${command}")
      done
      [[ "${#matched[@]}" -eq 2 ]] || fail "target ${target} expected 2 nested make commands, found ${#matched[@]}"
      printf '%s\n' "\`make test\` runs $(format_backtick_list "${matched[@]}")."
      ;;
    check-dist-ignore)
      for command in "${dry_run_lines[@]}"; do
        [[ "${command}" == bash\ * ]] || continue
        matched+=("${command#bash }")
      done
      [[ "${#matched[@]}" -eq 1 ]] || fail "target ${target} expected 1 bash command, found ${#matched[@]}"
      printf '%s\n' "\`make check-dist-ignore\` runs $(quote_inline "${matched[0]}")."
      ;;
    check-import-cycles)
      for command in "${dry_run_lines[@]}"; do
        [[ "${command}" == bash\ * ]] || continue
        matched+=("${command#bash }")
      done
      [[ "${#matched[@]}" -eq 1 ]] || fail "target ${target} expected 1 bash command, found ${#matched[@]}"
      printf '%s\n' "\`make check-import-cycles\` runs $(quote_inline "${matched[0]}"). It also runs automatically as a \`make build\` prerequisite."
      ;;
    check-package-export-goldens)
      for command in "${dry_run_lines[@]}"; do
        [[ "${command}" == timeout\ * ]] || continue
        matched+=("${command}")
      done
      [[ "${#matched[@]}" -eq 1 ]] || fail "target ${target} expected 1 timeout command, found ${#matched[@]}"
      printf '%s\n' "\`make check-package-export-goldens\` runs $(quote_inline "${matched[0]}")."
      ;;
    check-readme-makefile)
      for command in "${dry_run_lines[@]}"; do
        [[ "${command}" == bash\ * ]] || continue
        matched+=("${command#bash }")
      done
      [[ "${#matched[@]}" -eq 1 ]] || fail "target ${target} expected 1 bash command, found ${#matched[@]}"
      printf '%s\n' "\`make check-readme-makefile\` runs $(quote_inline "${matched[0]}") to compare this section against \`make --dry-run\` target bodies."
      ;;
    check)
      expected_make_lines=(
        "make check-parens"
        "make check-dist-ignore"
        "make check-import-cycles"
        "make check-readme-makefile"
        "make test"
        "make check-package-export-goldens"
        "make build"
      )
      index=0
      for command in "${dry_run_lines[@]}"; do
        [[ "${index}" -lt "${#expected_make_lines[@]}" ]] || break
        if [[ "${command}" == "${expected_make_lines[index]}" ]]; then
          matched+=("${command}")
          index=$((index + 1))
        fi
      done
      [[ "${#matched[@]}" -eq "${#expected_make_lines[@]}" ]] || fail "target ${target} missing expected top-level make sequence"
      printf '%s\n' "\`make check\` runs $(format_backtick_list "${matched[@]}"), in that order."
      ;;
    build)
      for command in "${dry_run_lines[@]}"; do
        [[ "${command}" == bash\ * ]] || continue
        matched+=("${command#bash }")
      done
      [[ "${#matched[@]}" -eq 2 ]] || fail "target ${target} expected 2 bash commands, found ${#matched[@]}"
      printf '%s\n' "\`make build\` runs $(format_backtick_list "${matched[@]}")."
      ;;
    yarli-bootstrap-validate)
      require_line "${target}" "${dry_run_text}" 'bash ./bin/yarli-bootstrap-local-state.sh'
      require_line "${target}" "${dry_run_text}" 'yarli plan validate'
      printf '%s\n' '`make yarli-bootstrap-validate` changes into repo root, runs `./bin/yarli-bootstrap-local-state.sh`, and then runs `yarli plan validate`.'
      ;;
    *)
      fail "unsupported target: ${target}"
      ;;
  esac
}

print_mismatch() {
  local heading="$1"
  shift
  local -a lines=("$@")
  local i=0

  printf '%s\n' "${heading}" >&2
  for ((i = 0; i < ${#lines[@]}; i++)); do
    printf '  [%02d] %s\n' "$((i + 1))" "${lines[i]}" >&2
  done
}

main() {
  local -a targets=(
    "test-ptui"
    "test-amoebum"
    "test"
    "check-dist-ignore"
    "check-import-cycles"
    "check-package-export-goldens"
    "check-readme-makefile"
    "check"
    "build"
    "yarli-bootstrap-validate"
  )
  local -a expected_lines=()
  local -a actual_lines=()
  local -a dry_run_lines=()
  local target=""
  local dry_run_text=""

  [[ -f "${README_PATH}" ]] || fail "missing README at ${README_PATH}"
  [[ -f "${MAKEFILE_PATH}" ]] || fail "missing Makefile at ${MAKEFILE_PATH}"

  require_cmd awk
  require_cmd make

  for target in "${targets[@]}"; do
    mapfile -t dry_run_lines < <(read_dry_run_commands "${target}")
    [[ "${#dry_run_lines[@]}" -gt 0 ]] || fail "target ${target} produced no dry-run output"
    dry_run_text="$(join_lines "${dry_run_lines[@]}")"
    expected_lines+=("$(expected_line_for_target "${target}" "${dry_run_text}" "${dry_run_lines[@]}")")
  done

  expected_lines+=('`./bin/yarli-local-state-regression.sh` exercises the missing-tranches bootstrap and repo-wrapper help-path smoke cases.')

  mapfile -t actual_lines < <(read_readme_section_lines)

  [[ "${#actual_lines[@]}" -eq "${#expected_lines[@]}" ]] || {
    print_mismatch "Expected README lines:" "${expected_lines[@]}"
    print_mismatch "Actual README lines:" "${actual_lines[@]}"
    fail "README Local Build and Check Commands line count ${#actual_lines[@]} does not match expected ${#expected_lines[@]}"
  }

  for target in "${!expected_lines[@]}"; do
    if [[ "${actual_lines[target]}" != "${expected_lines[target]}" ]]; then
      print_mismatch "Expected README lines:" "${expected_lines[@]}"
      print_mismatch "Actual README lines:" "${actual_lines[@]}"
      fail "README Local Build and Check Commands diverges at line $((target + 1))"
    fi
  done

  printf 'README_MAKEFILE_CONGRUENCE_OK\n'
}

main "$@"
