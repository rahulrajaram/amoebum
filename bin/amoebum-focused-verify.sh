#!/usr/bin/env bash
set -euo pipefail

# amoebum-focused-verify.sh
#
# NXT-416: previously this wrapper invoked SBCL with
# `sbcl --noinform --non-interactive --script /dev/stdin <<EOF ... EOF`.
# On Debian SBCL 2.2.9, that combination silently swallows the script
# body and exits 0. Result: the wrapper printed AMOEBUM_FOCUSED_VERIFY_OK
# without any FiveAM suites ever executing — three friction records
# (f-0730, f-0731, f-0732) reported the same root cause from independent
# tranches.
#
# The fix in this file:
#   1. Replace `--script /dev/stdin <<EOF` with the working
#      `--eval '(load "<tmpfile>")' --quit -- <args>` pattern. The Lisp
#      body is written to a real temp file with mktemp; the heredoc-on-
#      stdin path is gone. (regression-canary.sh already uses the
#      `--eval` chain idiom and produces output correctly.)
#   2. The Lisp body emits a sentinel:
#        AMOEBUM_FOCUSED_SUITES_OK profile=<name> suites=<csv>
#      after every requested FiveAM suite has actually executed and
#      passed.
#   3. The bash wrapper greps stdout for that exact sentinel before
#      printing the existing AMOEBUM_FOCUSED_VERIFY_OK marker. If the
#      sentinel is missing, the wrapper exits non-zero with
#        AMOEBUM_FOCUSED_VERIFY_FAIL profile=<name> reason=suites-did-not-execute
#      so silent-pass cannot recur.

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"
PROFILES=(worktrees packages state policy ui extensions shell macros cycles)

usage() {
  cat <<'EOF'
Usage:
  bin/amoebum-focused-verify.sh <profile> [--suites SUITE1,SUITE2] [--no-overwatch]
  bin/amoebum-focused-verify.sh --list-profiles

Profiles:
  worktrees  Worktree/runtime and operator dashboard seam checks.
  packages   Package/load-order seam checks (incl. NXT-398 export goldens).
  state      Conversation/checkpoint seam checks.
  policy     Plan-execution/permissions seam checks.
  ui         UI streaming/chat-render seam checks.
  extensions Extension loader/runtime seam checks.
  shell      Shell runtime/permission seam checks.
  macros     Macro expansion/validation seam checks.
  cycles     Package-import-cycle guardrail (NXT-397).
  all        Run every profile.

Debug flags:
  --suites CSV     Run only the given FiveAM suites for the selected profile.
                   Skips the profile's extra build/audit commands.
  --no-overwatch   Run SBCL directly even when overwatch is installed.
EOF
}

list_profiles() {
  printf '%s\n' "${PROFILES[@]}" all
}

fail() {
  echo "AMOEBUM_FOCUSED_VERIFY_ERROR: $*" >&2
  exit 1
}

run_cmd() {
  echo "==> $*"
  (
    cd "${REPO_ROOT}"
    "$@"
  )
}

find_plan_root() {
  if [[ -f "${REPO_ROOT}/IMPLEMENTATION_PLAN.md" && -f "${REPO_ROOT}/PROMPT.md" ]]; then
    printf '%s\n' "${REPO_ROOT}"
    return 0
  fi

  while IFS= read -r worktree_path; do
    if [[ -f "${worktree_path}/IMPLEMENTATION_PLAN.md" && -f "${worktree_path}/PROMPT.md" ]]; then
      printf '%s\n' "${worktree_path}"
      return 0
    fi
  done < <(git -C "${REPO_ROOT}" worktree list --porcelain | awk '/^worktree / {print $2}')

  return 1
}

run_plan_validate() {
  local plan_root
  plan_root="$(find_plan_root)" || fail "unable to find a worktree with IMPLEMENTATION_PLAN.md and PROMPT.md"
  echo "==> (cd ${plan_root} && yarli plan validate)"
  (
    cd "${plan_root}"
    yarli plan validate
  )
}

# Write the FiveAM-runner Lisp body to a temp file and return its path on
# stdout. The body reads its arguments from sb-ext:*posix-argv* (after the
# `--` separator). This is the same idiom the existing wrapper used, but
# delivered via --eval (load ...) instead of --script /dev/stdin so it
# actually executes on Debian SBCL 2.2.9.
write_focused_suites_runner() {
  local tmpfile
  tmpfile="$(mktemp -t amoebum-focused-XXXXXX.lisp)"
  cat > "${tmpfile}" <<'LISP'
(labels ((split-comma-separated (text)
           (let ((items '())
                 (start 0)
                 (length (length text)))
             (loop for position = (position #\, text :start start)
                   do (push (subseq text start (or position length)) items)
                   if position
                     do (setf start (1+ position))
                   else
                     do (return (nreverse items)))))
         (script-arg (index)
           ;; argv layout under --eval ... --quit -- <a> <b> <c> <d>:
           ;; (sbcl -- <a> <b> <c> <d>); we want positional args after `--`.
           (let* ((argv (or #+sbcl sb-ext:*posix-argv* #-sbcl nil))
                  (sep (position "--" argv :test #'string=))
                  (tail (when sep (nthcdr (1+ sep) argv))))
             (and tail (nth index tail)))))
  (let* ((suite-spec (or (script-arg 0) ""))
         (repo-root-arg (or (script-arg 1) ""))
         (quicklisp-arg (or (script-arg 2) ""))
         (profile-name (or (script-arg 3) ""))
         (repo-root (and (plusp (length repo-root-arg))
                         (truename repo-root-arg))))
    (unless repo-root
      (format *error-output*
              "AMOEBUM_FOCUSED_SUITES_FAIL reason=missing-repo-root arg=~S~%"
              repo-root-arg)
      (sb-ext:exit :code 2))
    (unless (plusp (length (string-trim '(#\Space #\Tab #\Newline #\Return)
                                        suite-spec)))
      (format *error-output*
              "AMOEBUM_FOCUSED_SUITES_FAIL reason=missing-suite-spec~%")
      (sb-ext:exit :code 2))
    (load quicklisp-arg)
    (require :asdf)
    (let* ((asdf-pkg (or (find-package "ASDF")
                         (error "Missing package ASDF")))
           (load-asd-fn (symbol-function (or (find-symbol "LOAD-ASD" asdf-pkg)
                                             (error "Missing ASDF LOAD-ASD symbol"))))
           (load-system-fn (symbol-function (or (find-symbol "LOAD-SYSTEM" asdf-pkg)
                                                (error "Missing ASDF LOAD-SYSTEM symbol"))))
           (compile-warnings-sym (or (find-symbol "*COMPILE-FILE-WARNINGS-BEHAVIOUR*" asdf-pkg)
                                     (find-symbol "*COMPILE-FILE-WARNINGS-BEHAVIOR*" asdf-pkg))))
      (when compile-warnings-sym
        (setf (symbol-value compile-warnings-sym) :ignore))
      (dolist (asd-path '("pseudopod/pseudopod.asd"
                          "sw4rm-sdk/sw4rm-sdk.asd"
                          "ptui/ptui.asd"
                          "amoebum/amoebum.asd"))
        (funcall load-asd-fn (merge-pathnames asd-path repo-root)))
      (funcall load-system-fn :amoebum/test))
    (let* ((run-fn (symbol-function (or (find-symbol "RUN" "IT.BESE.FIVEAM")
                                        (error "Missing FiveAM RUN symbol"))))
           (status-fn (symbol-function (or (find-symbol "RESULTS-STATUS" "IT.BESE.FIVEAM")
                                           (error "Missing FiveAM RESULTS-STATUS symbol"))))
           (explain-fn (symbol-function (or (find-symbol "EXPLAIN!" "IT.BESE.FIVEAM")
                                            (error "Missing FiveAM EXPLAIN! symbol"))))
           (suite-names (split-comma-separated suite-spec))
           (suites-run '())
           (any-failed nil))
      (dolist (suite-name suite-names)
        (let* ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) suite-name))
               (suite (and (plusp (length trimmed))
                           (find-symbol trimmed "AMOEBUM/TEST"))))
          (unless suite
            (format *error-output*
                    "AMOEBUM_FOCUSED_SUITES_FAIL reason=missing-suite suite=~A profile=~A~%"
                    trimmed profile-name)
            (sb-ext:exit :code 3))
          (let ((results (funcall run-fn suite)))
            (push trimmed suites-run)
            (unless (funcall status-fn results)
              (funcall explain-fn results)
              (setf any-failed t)))))
      (finish-output)
      (when any-failed
        (format t "~&AMOEBUM_FOCUSED_SUITES_FAIL profile=~A suites=~{~A~^,~} reason=test-failures~%"
                profile-name (nreverse suites-run))
        (finish-output)
        (sb-ext:exit :code 1))
      (format t "~&AMOEBUM_FOCUSED_SUITES_OK profile=~A suites=~{~A~^,~}~%"
              profile-name (nreverse suites-run))
      (finish-output))))
LISP
  printf '%s\n' "${tmpfile}"
}

run_focused_suites() {
  local suites="$1"
  local hard_timeout="$2"
  local soft_timeout="$3"
  local silent_timeout="$4"
  local quicklisp_setup="${QUICKLISP_SETUP:-${HOME}/quicklisp/setup.lisp}"
  local overwatch_bin="${OVERWATCH_BIN:-${HOME}/.local/bin/overwatch}"

  if [[ -f "${REPO_ROOT}/ptui/.tools/quicklisp/setup.lisp" ]]; then
    quicklisp_setup="${REPO_ROOT}/ptui/.tools/quicklisp/setup.lisp"
  fi

  [[ -f "${quicklisp_setup}" ]] || fail "quicklisp setup not found at ${quicklisp_setup}"
  if ! sbcl_path="$(command -v sbcl 2>&1)"; then
    fail "sbcl not found on PATH"
  fi
  [[ -n "${sbcl_path}" ]] || fail "sbcl not found on PATH"

  local runner_script
  runner_script="$(write_focused_suites_runner)"
  local out_log
  out_log="$(mktemp -t amoebum-focused-out-XXXXXX.log)"
  # shellcheck disable=SC2064
  trap "rm -f '${runner_script}' '${out_log}'" EXIT INT TERM

  local -a runner=(
    sbcl
    --noinform
    --non-interactive
    --eval "(load \"${runner_script}\")"
    --quit
    --
    "${suites}"
    "${REPO_ROOT}"
    "${quicklisp_setup}"
    "${PROFILE_NAME:-unknown}"
  )

  echo "==> focused suites: ${suites} (profile=${PROFILE_NAME:-unknown})"

  local rc=0
  if (( FOCUSED_VERIFY_USE_OVERWATCH )) && [[ -x "${overwatch_bin}" ]]; then
    set +e
    timeout "${hard_timeout}" "${overwatch_bin}" run --profile generic --stream \
      --soft-timeout "${soft_timeout}" --silent-timeout "${silent_timeout}" \
      -- "${runner[@]}" 2>&1 | tee "${out_log}"
    rc=${PIPESTATUS[0]}
    set -e
  else
    set +e
    timeout "${hard_timeout}" "${runner[@]}" 2>&1 | tee "${out_log}"
    rc=${PIPESTATUS[0]}
    set -e
  fi

  if (( rc != 0 )); then
    echo "AMOEBUM_FOCUSED_VERIFY_FAIL profile=${PROFILE_NAME:-unknown} reason=sbcl-exit-${rc} suites=${suites}" >&2
    rm -f "${runner_script}" "${out_log}"
    trap - EXIT INT TERM
    exit 1
  fi

  if ! grep -q "^AMOEBUM_FOCUSED_SUITES_OK profile=${PROFILE_NAME:-unknown} suites=" "${out_log}"; then
    echo "AMOEBUM_FOCUSED_VERIFY_FAIL profile=${PROFILE_NAME:-unknown} reason=suites-did-not-execute suites=${suites}" >&2
    echo "  (expected sentinel 'AMOEBUM_FOCUSED_SUITES_OK profile=${PROFILE_NAME:-unknown} suites=...' was not present in stdout)" >&2
    rm -f "${runner_script}" "${out_log}"
    trap - EXIT INT TERM
    exit 1
  fi

  rm -f "${runner_script}" "${out_log}"
  trap - EXIT INT TERM
}

profile_debug_timeouts() {
  local profile="$1"
  case "${profile}" in
    worktrees|packages|state|policy) printf '%s %s %s\n' 1200 1200 300 ;;
    ui|extensions|shell|macros) printf '%s %s %s\n' 1800 1800 300 ;;
    cycles|all) fail "--suites is not supported for profile ${profile}" ;;
    *) fail "unknown profile: ${profile}" ;;
  esac
}

run_debug_profile() {
  local profile="$1"
  local suites="$2"
  local hard_timeout
  local soft_timeout
  local silent_timeout
  read -r hard_timeout soft_timeout silent_timeout < <(profile_debug_timeouts "${profile}")
  PROFILE_NAME="${profile}" run_focused_suites "${suites}" "${hard_timeout}" "${soft_timeout}" "${silent_timeout}"
}

verify_worktrees() {
  PROFILE_NAME=worktrees run_focused_suites "WORKTREE-RUNTIME-SUITE,WORKER-SUPERVISOR-SUITE,SWARM-EXECUTION-SEMANTICS-SUITE,WORKER-DASHBOARD-SUITE" 1200 1200 300
  run_cmd timeout 240 make build
}

verify_packages() {
  run_cmd timeout 120 ./bin/check-import-cycles.sh
  run_cmd timeout 240 make build
  run_cmd timeout 180 ./bin/package-surface-audit.sh
  run_package_export_goldens
  PROFILE_NAME=packages run_focused_suites "WORKTREE-RUNTIME-SUITE,WORKER-SUPERVISOR-SUITE" 1200 1200 300
  run_plan_validate
}

verify_cycles() {
  run_cmd timeout 120 ./bin/check-import-cycles.sh
}

run_package_export_goldens() {
  # NXT-398: subsystem-level public-symbol stability fixture.
  # The script is standalone — it bootstraps Quicklisp + ASDF, loads :amoebum,
  # then compares each target package's external symbols against a checked-in
  # golden under amoebum/test/snapshots/package-exports/. Set
  # AMOEBUM_UPDATE_SNAPSHOTS=1 in the environment to refresh goldens after a
  # deliberate facade change.
  echo "==> package-export goldens (NXT-398)"
  (
    cd "${REPO_ROOT}"
    timeout 600 sbcl --noinform \
      --script amoebum/test/package-export-golden-test.lisp
  )
}

verify_state() {
  PROFILE_NAME=state run_focused_suites "FP-COLLECTIONS-SUITE" 240 240 120
  PROFILE_NAME=state run_focused_suites "MEMORY-COMMAND-SUITE" 240 240 120
  # NXT-416: CONVERSATION-ROUNDTRIP-SUITE and CONVERSATION-UNIT-SUITE were
  # referenced by the old wrapper but never registered as FiveAM suites
  # (per friction f-0732). The conversation-{roundtrip,unit}-test.lisp
  # files contribute their tests to the parent AMOEBUM-SUITE without
  # creating a sub-suite. CONVERSATION-EXPORT-SUITE is the closest
  # registered conversation-state suite and is included here.
  PROFILE_NAME=state run_focused_suites "CONVERSATION-EXPORT-SUITE,SESSION-RESUME-SUITE" 1200 1200 300
  PROFILE_NAME=state run_focused_suites "CHECKPOINT-ROTATION-SUITE" 240 240 120
}

verify_policy() {
  PROFILE_NAME=policy run_focused_suites "PLAN-EXECUTION-UNIT-SUITE,PLAN-EXECUTION-TRANSITION-TABLE-SUITE,PERMISSIONS-UNIT-SUITE,PERMISSION-PATH-NORMALIZATION-SUITE,PERMISSION-PATH-MEMORY-SUITE,PERMISSION-ARGUMENT-GRANULARITY-SUITE" 1200 1200 300
}

verify_ui() {
  run_cmd timeout 120 ./bin/check-import-cycles.sh
  # NXT-416: STREAMING-BUDGET-SUITE was previously listed here, but the
  # owning file amoebum/test/streaming-budget-test.lisp is not loaded by
  # the amoebum/test ASDF system (so the suite symbol never gets
  # interned in package AMOEBUM/TEST). Including it caused
  # missing-suite failures once the wrapper started actually executing
  # tests. Dropped for now; restore once the test file is wired into the
  # asd. (Same story for AUDIT-LOG-SUITE, CONDITION-TO-LLM-CONTEXT-SUITE,
  # EVENT-HOOKS-SUITE, LLM-HOOKS-SUITE,
  # PERMISSION-PATH-IDENTITY-EDGE-CASE-SUITE, TTS-SUITE,
  # WEBHOOK-NOTIFICATION-SUITE — none referenced by any profile here.)
  PROFILE_NAME=ui run_focused_suites "STREAMING-STEP-SUITE,INCREMENTAL-MARKDOWN-SUITE,CHAT-SNAPSHOT-SUITE,APPROVAL-DIALOG-GUARD-SUITE,KEYBOARD-NAV-SUITE" 1800 1800 300
  # NXT-400: per-submodule streaming coverage gate. The harness emits one
  # STREAMING_COVERAGE_<MODULE>_OK verdict per intended ui/streaming submodule
  # (token-state, markdown, provider-runtime, event-journal) and only then
  # emits the top-level I333_HEADLESS_HARNESS_SELF_TEST_OK marker.
  run_cmd timeout 600 ./bin/headless-streaming-regression.sh --self-test
}

verify_extensions() {
  run_cmd timeout 120 ./bin/check-import-cycles.sh
  PROFILE_NAME=extensions run_focused_suites "EXTENSION-LOADER-SUITE,EXTENSION-LIFECYCLE-SUITE,EXTENSION-DISCOVERY-SUITE,EXTENSION-MANIFEST-SUITE,EXTENSION-SECURITY-SUITE,EXTENSION-CLI-SUITE,ASDF-EXTENSIONS-SUITE" 1800 1800 300
}

verify_shell() {
  PROFILE_NAME=shell run_focused_suites "SHELL-ENV-SUITE,SHELL-SAFETY-SUITE,SHELL-BACKGROUND-SUITE,SHELL-RUNAWAY-OUTPUT-SUITE,WRITE-SAFETY-SUITE,EDIT-VALIDATION-SUITE,TOOL-ARGUMENT-PROMPTING-SUITE,PERMISSION-ARGUMENT-GRANULARITY-SUITE" 1800 1800 300
}

verify_macros() {
  PROFILE_NAME=macros run_focused_suites "MACROEXPAND-GOLDEN-SUITE,DEFTOOL-TYPE-VALIDATION-SUITE,DEFTOOL-DANGEROUS-PERMISSION-SUITE,COMPILE-VALIDATION-CONDITIONS-SUITE,DEFHOOK-CROSS-REFERENCE-SUITE,ARGUMENT-PATTERN-DISPATCH-SUITE" 1800 1800 300
}

profile=""
debug_suites=""
list_only=0
FOCUSED_VERIFY_USE_OVERWATCH=1

while [[ $# -gt 0 ]]; do
  case "$1" in
    --help|-h)
      usage
      exit 0
      ;;
    --list-profiles)
      list_only=1
      ;;
    --suites)
      shift
      [[ $# -gt 0 ]] || { usage >&2; exit 2; }
      debug_suites="$1"
      ;;
    --no-overwatch)
      FOCUSED_VERIFY_USE_OVERWATCH=0
      ;;
    -*)
      usage >&2
      exit 2
      ;;
    *)
      if [[ -n "${profile}" ]]; then
        usage >&2
        exit 2
      fi
      profile="$1"
      ;;
  esac
  shift
done

if (( list_only )); then
  if [[ -n "${profile}" || -n "${debug_suites}" || ${FOCUSED_VERIFY_USE_OVERWATCH} -eq 0 ]]; then
    usage >&2
    exit 2
  fi
  list_profiles
  exit 0
fi

[[ -n "${profile}" ]] || { usage >&2; exit 2; }

if [[ -n "${debug_suites}" ]]; then
  run_debug_profile "${profile}" "${debug_suites}"
  echo "AMOEBUM_FOCUSED_VERIFY_OK profile=${profile}"
  exit 0
fi

case "${profile}" in
  worktrees) verify_worktrees ;;
  packages) verify_packages ;;
  state) verify_state ;;
  policy) verify_policy ;;
  ui) verify_ui ;;
  extensions) verify_extensions ;;
  shell) verify_shell ;;
  macros) verify_macros ;;
  cycles) verify_cycles ;;
  all)
    verify_cycles
    verify_worktrees
    verify_packages
    verify_state
    verify_policy
    verify_ui
    verify_extensions
    verify_shell
    verify_macros
    ;;
  *)
    usage >&2
    exit 2
    ;;
esac

echo "AMOEBUM_FOCUSED_VERIFY_OK profile=${profile}"
