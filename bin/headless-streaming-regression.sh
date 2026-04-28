#!/usr/bin/env bash
# headless-streaming-regression.sh
# I333: Headless streamed-turn regression harness with bounded execution,
# artifact capture, and machine-readable lifecycle verdicts.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"
TMPDIR="${REPO_ROOT}/tmp"
mkdir -p "${TMPDIR}"
export TMPDIR

MODE=""
BINARY="${REPO_ROOT}/dist/amoebum"
PROMPT="Briefly inspect the repository and use at least one tool."
COMMAND=""
TIMEOUT_SECONDS=180
ARTIFACT_DIR=""
JOURNAL_FILE=""
JOURNAL_DIR=""
STDOUT_FILE=""
STDERR_FILE=""
VERDICT_OUT=""
ALLOW_SILENT=0

usage() {
  cat <<'EOF'
Usage:
  bin/headless-streaming-regression.sh --run [options]
  bin/headless-streaming-regression.sh --analyze [options]
  bin/headless-streaming-regression.sh --self-test

Modes:
  --run
      Execute a bounded headless command (default: ./dist/amoebum --json --prompt ...),
      capture stdout/stderr/journal artifacts, and emit verdict JSON.
  --analyze
      Analyze existing journal/stdout artifacts and emit verdict JSON.
  --self-test
      Run deterministic fixture checks for verdicting behavior.

Options:
  --binary <path>            Binary path for --run (default: ./dist/amoebum)
  --prompt <text>            Prompt for default --run command
  --command <shell-command>  Explicit shell command for --run
  --timeout-seconds <n>      Command timeout for --run (default: 180)
  --artifact-dir <dir>       Artifact directory for --run
  --journal-file <file>      Journal file to analyze
  --journal-dir <dir>        Journal directory to analyze (all *.jsonl files)
  --stdout-file <file>       Captured stdout file to analyze
  --stderr-file <file>       Captured stderr file to analyze
  --verdict-out <file>       Write verdict JSON to file (also printed)
  --allow-silent             Do not fail exit code on :silent-completion verdict
  --help                     Show this help
EOF
}

die() {
  echo "FATAL: $*" >&2
  exit 1
}

need_command() {
  local name="$1"
  command -v "$name" >/dev/stdout || die "Required command not found: $name"
}

json_escape_file_list() {
  local list_file="$1"
  jq -Rc -s 'split("\n") | map(select(length > 0))' < "$list_file"
}

count_matches() {
  local pattern="$1"
  local file="$2"
  if [[ ! -s "$file" ]]; then
    echo "0"
    return
  fi
  grep -E -c "$pattern" "$file" || true
}

contains_pattern() {
  local pattern="$1"
  local file="$2"
  if [[ ! -s "$file" ]]; then
    return 1
  fi
  grep -Eqi "$pattern" "$file"
}

resolve_default_command() {
  local quoted_binary quoted_prompt
  quoted_binary="$(printf '%q' "$BINARY")"
  quoted_prompt="$(printf '%q' "$PROMPT")"
  printf '%s --json --prompt %s' "$quoted_binary" "$quoted_prompt"
}

collect_journal_files() {
  local list_file="$1"
  : > "$list_file"
  if [[ -n "$JOURNAL_FILE" ]]; then
    [[ -f "$JOURNAL_FILE" ]] || die "Journal file not found: $JOURNAL_FILE"
    printf '%s\n' "$JOURNAL_FILE" >> "$list_file"
  fi
  if [[ -n "$JOURNAL_DIR" ]]; then
    [[ -d "$JOURNAL_DIR" ]] || die "Journal dir not found: $JOURNAL_DIR"
    find "$JOURNAL_DIR" -maxdepth 1 -type f -name '*.jsonl' | sort >> "$list_file"
  fi
}

emit_verdict() {
  local mode="$1"
  local command_form="$2"
  local timeout_seconds="$3"
  local timed_out="$4"
  local exit_code="$5"
  local journal_files_list="$6"
  local combined_journal="$7"
  local stdout_file="$8"
  local stderr_file="$9"

  local stream_progress_count stream_complete_count stream_failed_count
  local text_chunk_count tool_signal_count retry_signal_count error_signal_count
  local saw_stream_progress saw_stream_complete saw_text_delta saw_tool_signal
  local saw_retry saw_explicit_error saw_answer
  local stdout_json stdout_ok stdout_output stdout_error stdout_action
  local outcome contract_valid

  stream_progress_count="$(count_matches '"type":"UI:STREAM-PROGRESS"' "$combined_journal")"
  stream_complete_count="$(count_matches '"type":"UI:STREAM-PROGRESS".*:STATUS COMPLETED' "$combined_journal")"
  stream_failed_count="$(count_matches '"type":"UI:STREAM-PROGRESS".*:STATUS (FAILED|CANCELLED)' "$combined_journal")"
  text_chunk_count="$(count_matches '"type":"LLM:STREAM-CHUNK"' "$combined_journal")"
  tool_signal_count="$(count_matches '"type":"(TOOL|TOOL-CALL):' "$combined_journal")"
  retry_signal_count="$(count_matches '"type":"[^"]*RETRY' "$combined_journal")"
  error_signal_count="$(count_matches '"type":"[^"]*(ERROR|FAILED|CANCELLED)' "$combined_journal")"

  stdout_json='{}'
  if [[ -n "$stdout_file" && -f "$stdout_file" ]]; then
    stdout_json="$(jq -Rsc 'split("\n") | map(try fromjson catch empty) | map(select(type=="object")) | last // {}' "$stdout_file" || echo '{}')"
  fi

  stdout_ok="$(jq -r '.ok // false' <<<"$stdout_json" || echo "false")"
  stdout_output="$(jq -r '.output // ""' <<<"$stdout_json" || echo "")"
  stdout_error="$(jq -r '.error // ""' <<<"$stdout_json" || echo "")"
  stdout_action="$(jq -r '.action // ""' <<<"$stdout_json" || echo "")"

  saw_stream_progress=0
  [[ "$stream_progress_count" -gt 0 ]] && saw_stream_progress=1
  saw_stream_complete=0
  [[ "$stream_complete_count" -gt 0 ]] && saw_stream_complete=1
  saw_text_delta=0
  [[ "$text_chunk_count" -gt 0 ]] && saw_text_delta=1
  saw_tool_signal=0
  [[ "$tool_signal_count" -gt 0 ]] && saw_tool_signal=1
  saw_retry=0
  if [[ "$retry_signal_count" -gt 0 ]]; then
    saw_retry=1
  elif [[ "${stdout_output}" =~ [Rr]etry|try[[:space:]]again|re-issue ]] || [[ "${stdout_error}" =~ [Rr]etry|try[[:space:]]again|re-issue ]]; then
    saw_retry=1
  fi

  saw_answer=0
  if [[ -n "${stdout_output//[[:space:]]/}" ]]; then
    saw_answer=1
  fi

  saw_explicit_error=0
  if [[ "$timed_out" == "true" ]]; then
    saw_explicit_error=1
  elif [[ "$stream_failed_count" -gt 0 || "$error_signal_count" -gt 0 ]]; then
    saw_explicit_error=1
  elif [[ -n "${stdout_error//[[:space:]]/}" ]]; then
    saw_explicit_error=1
  elif [[ "$stdout_ok" != "true" && "$stdout_action" == "error" ]]; then
    saw_explicit_error=1
  fi

  if [[ "$saw_explicit_error" -eq 1 ]]; then
    outcome="explicit-error"
  elif [[ "$saw_retry" -eq 1 ]]; then
    outcome="retry"
  elif [[ "$saw_answer" -eq 1 ]]; then
    outcome="answer"
  elif [[ "$saw_tool_signal" -eq 1 ]]; then
    outcome="tool-continuation"
  elif [[ "$saw_stream_complete" -eq 1 && "$saw_stream_progress" -eq 1 && "$saw_text_delta" -eq 1 ]]; then
    outcome="silent-completion"
  else
    outcome="silent-completion"
  fi

  contract_valid="false"
  case "$outcome" in
    answer|tool-continuation|retry|explicit-error)
      contract_valid="true"
      ;;
  esac

  local journal_files_json
  journal_files_json="$(json_escape_file_list "$journal_files_list")"

  local verdict
  verdict="$(jq -n \
    --arg mode "$mode" \
    --arg command_form "$command_form" \
    --argjson timeout_seconds "$timeout_seconds" \
    --arg timed_out "$timed_out" \
    --argjson exit_code "$exit_code" \
    --arg outcome "$outcome" \
    --argjson contract_valid "$contract_valid" \
    --arg stdout_file "${stdout_file:-}" \
    --arg stderr_file "${stderr_file:-}" \
    --argjson journal_files "$journal_files_json" \
    --argjson stream_progress_count "$stream_progress_count" \
    --argjson stream_complete_count "$stream_complete_count" \
    --argjson stream_failed_count "$stream_failed_count" \
    --argjson text_chunk_count "$text_chunk_count" \
    --argjson tool_signal_count "$tool_signal_count" \
    --argjson retry_signal_count "$retry_signal_count" \
    --argjson error_signal_count "$error_signal_count" \
    --argjson saw_stream_progress "$saw_stream_progress" \
    --argjson saw_stream_complete "$saw_stream_complete" \
    --argjson saw_text_delta "$saw_text_delta" \
    --argjson saw_tool_signal "$saw_tool_signal" \
    --argjson saw_retry "$saw_retry" \
    --argjson saw_explicit_error "$saw_explicit_error" \
    --argjson saw_answer "$saw_answer" \
    '{
      mode: $mode,
      command: $command_form,
      timeout_seconds: $timeout_seconds,
      timed_out: ($timed_out == "true"),
      exit_code: $exit_code,
      outcome: $outcome,
      contract_valid: $contract_valid,
      valid_outcomes: ["answer", "tool-continuation", "retry", "explicit-error"],
      artifacts: {
        stdout_file: $stdout_file,
        stderr_file: $stderr_file,
        journal_files: $journal_files
      },
      signals: {
        stream_progress_count: $stream_progress_count,
        stream_complete_count: $stream_complete_count,
        stream_failed_count: $stream_failed_count,
        text_chunk_count: $text_chunk_count,
        tool_signal_count: $tool_signal_count,
        retry_signal_count: $retry_signal_count,
        error_signal_count: $error_signal_count,
        saw_stream_progress: ($saw_stream_progress == 1),
        saw_stream_complete: ($saw_stream_complete == 1),
        saw_text_delta: ($saw_text_delta == 1),
        saw_tool_signal: ($saw_tool_signal == 1),
        saw_retry: ($saw_retry == 1),
        saw_explicit_error: ($saw_explicit_error == 1),
        saw_answer: ($saw_answer == 1)
      }
    }')"

  if [[ -n "$VERDICT_OUT" ]]; then
    mkdir -p "$(dirname "$VERDICT_OUT")"
    printf '%s\n' "$verdict" > "$VERDICT_OUT"
  fi

  printf '%s\n' "$verdict"
  echo "I333_HEADLESS_VERDICT mode=${mode} outcome=${outcome} contract_valid=${contract_valid}"

  if [[ "$outcome" == "silent-completion" && "$ALLOW_SILENT" -ne 1 ]]; then
    return 2
  fi
  return 0
}

analyze_mode() {
  need_command jq
  need_command timeout

  local temp_dir list_file combined_journal
  temp_dir="$(mktemp -d)"
  list_file="${temp_dir}/journal-files.txt"
  combined_journal="${temp_dir}/combined-journal.jsonl"

  collect_journal_files "$list_file"
  : > "$combined_journal"
  if [[ -s "$list_file" ]]; then
    while IFS= read -r path; do
      [[ -z "$path" ]] && continue
      cat "$path" >> "$combined_journal"
      printf '\n' >> "$combined_journal"
    done < "$list_file"
  fi

  local command_form="analyze-only"
  emit_verdict "analyze" "$command_form" 0 "false" 0 "$list_file" "$combined_journal" "$STDOUT_FILE" "$STDERR_FILE"
  local status=$?
  rm -rf "$temp_dir"
  return "$status"
}

run_mode() {
  need_command jq
  need_command timeout

  local timestamp default_artifact command_to_run exit_code timed_out
  timestamp="$(date -u +%Y%m%dT%H%M%SZ)"
  if [[ -z "$ARTIFACT_DIR" ]]; then
    ARTIFACT_DIR="${REPO_ROOT}/tmp/i333-headless-${timestamp}"
  fi

  mkdir -p "$ARTIFACT_DIR/journal"
  STDOUT_FILE="${ARTIFACT_DIR}/stdout.jsonl"
  STDERR_FILE="${ARTIFACT_DIR}/stderr.log"
  JOURNAL_DIR="${ARTIFACT_DIR}/journal"

  if [[ -n "$COMMAND" ]]; then
    command_to_run="$COMMAND"
  else
    [[ -x "$BINARY" ]] || die "Binary not found or not executable: $BINARY"
    command_to_run="$(resolve_default_command)"
  fi

  exit_code=0
  timed_out="false"
  set +e
  AMOEBUM_EVENT_JOURNAL=1 AMOEBUM_EVENT_JOURNAL_DIR="$JOURNAL_DIR" \
    timeout --signal=TERM --kill-after=5 "${TIMEOUT_SECONDS}" \
    bash -lc "$command_to_run" >"$STDOUT_FILE" 2>"$STDERR_FILE"
  exit_code=$?
  set -e

  if [[ "$exit_code" -eq 124 ]]; then
    timed_out="true"
  fi

  local temp_dir list_file combined_journal
  temp_dir="$(mktemp -d)"
  list_file="${temp_dir}/journal-files.txt"
  combined_journal="${temp_dir}/combined-journal.jsonl"

  JOURNAL_FILE=""
  collect_journal_files "$list_file"
  : > "$combined_journal"
  if [[ -s "$list_file" ]]; then
    while IFS= read -r path; do
      [[ -z "$path" ]] && continue
      cat "$path" >> "$combined_journal"
      printf '\n' >> "$combined_journal"
    done < "$list_file"
  fi

  emit_verdict "run" "$command_to_run" "$TIMEOUT_SECONDS" "$timed_out" "$exit_code" "$list_file" "$combined_journal" "$STDOUT_FILE" "$STDERR_FILE"
  local status=$?
  rm -rf "$temp_dir"
  return "$status"
}

## ---------------------------------------------------------------------------
## NXT-400: per-submodule streaming coverage gates
##
## Each ui/streaming/* submodule produced by NXT-382/NXT-383 must exercise its
## intended surface before the wave can close. We invoke one FiveAM suite per
## submodule, parse the "Did N checks. Pass: N" line emitted by `run!`, and
## emit a machine-greppable verdict per submodule. The top-level
## `I333_HEADLESS_HARNESS_SELF_TEST_OK` marker only fires if every submodule
## verdict is OK; otherwise we emit a `..._FAIL` marker and exit non-zero so
## downstream callers (yarli verify, focused-verify, CI) fail loudly.
##
## Suite mapping (intentionally pinned — do not silently re-route):
##   token-state      -> TOKEN-STREAM-TRANSITION-TABLE-SUITE
##   markdown         -> INCREMENTAL-MARKDOWN-SUITE
##   provider-runtime -> STREAMING-PROVIDER-RUNTIME-SUITE
##   event-journal    -> STREAM-HOOKS-SUITE
## ---------------------------------------------------------------------------

resolve_quicklisp_setup() {
  local default_path="${HOME}/quicklisp/setup.lisp"
  if [[ -n "${QUICKLISP_SETUP:-}" && -f "${QUICKLISP_SETUP}" ]]; then
    printf '%s\n' "${QUICKLISP_SETUP}"
    return 0
  fi
  if [[ -f "${REPO_ROOT}/ptui/.tools/quicklisp/setup.lisp" ]]; then
    printf '%s\n' "${REPO_ROOT}/ptui/.tools/quicklisp/setup.lisp"
    return 0
  fi
  if [[ -f "${default_path}" ]]; then
    printf '%s\n' "${default_path}"
    return 0
  fi
  return 1
}

run_focused_streaming_suite() {
  # Args: <suite-name> <log-file>
  # Runs a single AMOEBUM/TEST FiveAM suite via a temp .lisp helper.
  # Avoids `--script /dev/stdin` which silently no-ops on Debian SBCL 2.2.9.
  # Exits with the SBCL process exit code (0 if all checks pass, 1 otherwise).
  local suite_name="$1"
  local log_file="$2"
  local quicklisp_setup
  quicklisp_setup="$(resolve_quicklisp_setup)" || die "quicklisp setup not found (set QUICKLISP_SETUP=...)"
  command -v sbcl >/dev/stdout || die "sbcl not found on PATH"

  local helper_lisp
  local cache_root
  helper_lisp="$(mktemp -t headless-streaming-suite-XXXXXX.lisp)"
  cache_root="${REPO_ROOT}/tmp/headless-streaming-cache"
  mkdir -p "${cache_root}"
  cat >"${helper_lisp}" <<LISP
(let* ((argv (or #+sbcl sb-ext:*posix-argv* #-sbcl nil))
       (suite-spec (or (and argv (second argv)) ""))
       (repo-root-arg (or (and argv (third argv)) ""))
       (quicklisp-arg (or (and argv (fourth argv)) ""))
       (repo-root (and (plusp (length repo-root-arg))
                       (truename repo-root-arg))))
  (unless repo-root
    (error "Unable to resolve repository root from ~S" repo-root-arg))
  (unless (plusp (length suite-spec))
    (error "Suite name argument required."))
  (load quicklisp-arg)
  (require :asdf)
  (let* ((asdf-pkg (or (find-package "ASDF")
                       (error "Missing package ASDF")))
         (load-asd-fn (symbol-function (or (find-symbol "LOAD-ASD" asdf-pkg)
                                           (error "Missing ASDF LOAD-ASD"))))
         (load-system-fn (symbol-function (or (find-symbol "LOAD-SYSTEM" asdf-pkg)
                                              (error "Missing ASDF LOAD-SYSTEM"))))
         (warn-sym (or (find-symbol "*COMPILE-FILE-WARNINGS-BEHAVIOUR*" asdf-pkg)
                       (find-symbol "*COMPILE-FILE-WARNINGS-BEHAVIOR*" asdf-pkg))))
    (when warn-sym
      (setf (symbol-value warn-sym) :ignore))
    (dolist (asd-path '("pseudopod/pseudopod.asd"
                        "sw4rm-sdk/sw4rm-sdk.asd"
                        "ptui/ptui.asd"
                        "amoebum/amoebum.asd"))
      (funcall load-asd-fn (merge-pathnames asd-path repo-root)))
    (funcall load-system-fn :amoebum/test))
  (let* ((run-fn (symbol-function (or (find-symbol "RUN!" "IT.BESE.FIVEAM")
                                      (error "Missing FiveAM RUN!"))))
         (suite-symbol (or (find-symbol suite-spec "AMOEBUM/TEST")
                           (error "Missing AMOEBUM/TEST suite ~S" suite-spec)))
         (status (funcall run-fn suite-symbol)))
    (unless status
      (sb-ext:exit :code 1))))
LISP

  local exit_code=0
  set +e
  XDG_CACHE_HOME="${cache_root}" \
  sbcl --noinform --non-interactive --load "${helper_lisp}" \
    "${suite_name}" "${REPO_ROOT}" "${quicklisp_setup}" \
    >"${log_file}" 2>&1
  exit_code=$?
  set -e

  rm -f "${helper_lisp}"
  return "${exit_code}"
}

extract_suite_check_count() {
  # Parse "Did N check(s)." line emitted by FiveAM detailed-text-explainer.
  local log_file="$1"
  if [[ ! -s "${log_file}" ]]; then
    echo "0"
    return
  fi
  awk '/Did[[:space:]]+[0-9]+[[:space:]]+check/ {
         for (i = 1; i <= NF; i++) {
           if ($i == "Did") {
             gsub(/[^0-9]/, "", $(i+1))
             print $(i+1)
             exit
           }
         }
       }' "${log_file}"
}

extract_suite_pass_status() {
  # Verify "Pass: N (100%)" appears AND no "Fail: <nonzero>" appears.
  # Returns 0 if suite passed cleanly, 1 otherwise.
  local log_file="$1"
  [[ -s "${log_file}" ]] || return 1
  local fail_count
  fail_count="$(awk '/Fail:[[:space:]]+[0-9]+/ {
                       for (i = 1; i <= NF; i++) {
                         if ($i == "Fail:") { gsub(/[^0-9]/, "", $(i+1)); print $(i+1); exit }
                       }
                     }' "${log_file}")"
  if [[ -n "${fail_count}" && "${fail_count}" != "0" ]]; then
    return 1
  fi
  grep -Eq 'Pass:[[:space:]]+[0-9]+[[:space:]]+\(100' "${log_file}"
}

run_submodule_coverage_gate() {
  # Args: <submodule-label> <suite-name> <verdict-marker-prefix>
  # Emits one verdict line on success or failure. Returns 0/1.
  local submodule="$1"
  local suite_name="$2"
  local marker_prefix="$3"
  local log_file
  log_file="$(mktemp -t headless-streaming-${submodule}-XXXXXX.log)"

  local sbcl_status=0
  run_focused_streaming_suite "${suite_name}" "${log_file}" || sbcl_status=$?

  local checks
  checks="$(extract_suite_check_count "${log_file}")"
  : "${checks:=0}"

  if [[ "${sbcl_status}" -ne 0 ]] || ! extract_suite_pass_status "${log_file}"; then
    echo "${marker_prefix}_FAIL submodule=${submodule} suite=${suite_name} checks=${checks} sbcl_exit=${sbcl_status}"
    echo "---- last 40 lines of ${log_file} ----" >&2
    tail -n 40 "${log_file}" >&2 || true
    return 1
  fi

  if [[ "${checks}" -eq 0 ]]; then
    # A suite that passes with zero checks is the exact failure mode this
    # gate is built to catch — refuse to call it green.
    echo "${marker_prefix}_FAIL submodule=${submodule} suite=${suite_name} checks=0 reason=zero-checks-passed"
    return 1
  fi

  echo "${marker_prefix}_OK submodule=${submodule} suite=${suite_name} checks=${checks}"
  rm -f "${log_file}"
  return 0
}

self_test_mode() {
  local fixture_dir out_dir
  fixture_dir="${REPO_ROOT}/tests/fixtures/streaming-regression"
  out_dir="${REPO_ROOT}/tmp/i333-self-test-$$"
  mkdir -p "$out_dir"

  ## --- Existing fixture-based verdict checks (preserved) -------------------
  "$0" --analyze \
    --journal-file "${fixture_dir}/silent-completion.jsonl" \
    --verdict-out "${out_dir}/silent.json" \
    --allow-silent >"${out_dir}/silent.log"
  jq -e '.outcome == "silent-completion" and .contract_valid == false' "${out_dir}/silent.json" >"${out_dir}/silent-jq.log"

  "$0" --analyze \
    --journal-file "${fixture_dir}/healthy-tool-continuation.jsonl" \
    --verdict-out "${out_dir}/healthy.json" >"${out_dir}/healthy.log"
  jq -e '.outcome == "tool-continuation" and .contract_valid == true' "${out_dir}/healthy.json" >"${out_dir}/healthy-jq.log"

  "$0" --analyze \
    --journal-file "${fixture_dir}/explicit-error.jsonl" \
    --verdict-out "${out_dir}/error.json" >"${out_dir}/error.log"
  jq -e '.outcome == "explicit-error" and .contract_valid == true' "${out_dir}/error.json" >"${out_dir}/error-jq.log"

  ## --- NXT-400: per-submodule streaming coverage gates ---------------------
  ## Suite mapping is intentionally pinned. Adding a new ui/streaming/*
  ## submodule should grow this list, never silently re-route an existing one.
  local submodule_specs=(
    "token-state|TOKEN-STREAM-TRANSITION-TABLE-SUITE|STREAMING_COVERAGE_TOKEN_STATE"
    "markdown|INCREMENTAL-MARKDOWN-SUITE|STREAMING_COVERAGE_MARKDOWN"
    "provider-runtime|STREAMING-PROVIDER-RUNTIME-SUITE|STREAMING_COVERAGE_PROVIDER_RUNTIME"
    "event-journal|STREAM-HOOKS-SUITE|STREAMING_COVERAGE_EVENT_JOURNAL"
  )

  local any_failed=0
  local spec submodule suite marker
  for spec in "${submodule_specs[@]}"; do
    IFS='|' read -r submodule suite marker <<<"${spec}"
    if ! run_submodule_coverage_gate "${submodule}" "${suite}" "${marker}"; then
      any_failed=1
    fi
  done

  if [[ "${any_failed}" -ne 0 ]]; then
    echo "I333_HEADLESS_HARNESS_SELF_TEST_FAIL reason=submodule-coverage-gate"
    return 1
  fi

  echo "I333_HEADLESS_HARNESS_SELF_TEST_OK"
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --run)
      MODE="run"
      shift
      ;;
    --analyze)
      MODE="analyze"
      shift
      ;;
    --self-test)
      MODE="self-test"
      shift
      ;;
    --binary)
      [[ $# -ge 2 ]] || die "Missing value for --binary"
      BINARY="$2"
      shift 2
      ;;
    --prompt)
      [[ $# -ge 2 ]] || die "Missing value for --prompt"
      PROMPT="$2"
      shift 2
      ;;
    --command)
      [[ $# -ge 2 ]] || die "Missing value for --command"
      COMMAND="$2"
      shift 2
      ;;
    --timeout-seconds)
      [[ $# -ge 2 ]] || die "Missing value for --timeout-seconds"
      TIMEOUT_SECONDS="$2"
      shift 2
      ;;
    --artifact-dir)
      [[ $# -ge 2 ]] || die "Missing value for --artifact-dir"
      ARTIFACT_DIR="$2"
      shift 2
      ;;
    --journal-file)
      [[ $# -ge 2 ]] || die "Missing value for --journal-file"
      JOURNAL_FILE="$2"
      shift 2
      ;;
    --journal-dir)
      [[ $# -ge 2 ]] || die "Missing value for --journal-dir"
      JOURNAL_DIR="$2"
      shift 2
      ;;
    --stdout-file)
      [[ $# -ge 2 ]] || die "Missing value for --stdout-file"
      STDOUT_FILE="$2"
      shift 2
      ;;
    --stderr-file)
      [[ $# -ge 2 ]] || die "Missing value for --stderr-file"
      STDERR_FILE="$2"
      shift 2
      ;;
    --verdict-out)
      [[ $# -ge 2 ]] || die "Missing value for --verdict-out"
      VERDICT_OUT="$2"
      shift 2
      ;;
    --allow-silent)
      ALLOW_SILENT=1
      shift
      ;;
    --help|-h)
      usage
      exit 0
      ;;
    *)
      die "Unknown argument: $1"
      ;;
  esac
done

case "$MODE" in
  run)
    run_mode
    ;;
  analyze)
    analyze_mode
    ;;
  self-test)
    self_test_mode
    ;;
  *)
    usage >&2
    exit 2
    ;;
esac
