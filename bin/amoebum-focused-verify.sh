#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"
PROFILES=(worktrees packages state policy ui extensions shell macros)

usage() {
  cat <<'EOF'
Usage:
  bin/amoebum-focused-verify.sh <profile>
  bin/amoebum-focused-verify.sh --list-profiles

Profiles:
  worktrees  Worktree/runtime and operator dashboard seam checks.
  packages   Package/load-order seam checks.
  state      Conversation/checkpoint seam checks.
  policy     Plan-execution/permissions seam checks.
  ui         UI streaming/chat-render seam checks.
  extensions Extension loader/runtime seam checks.
  shell      Shell runtime/permission seam checks.
  macros     Macro expansion/validation seam checks.
  all        Run every profile.
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

run_focused_suites() {
  local suites="$1"
  local hard_timeout="$2"
  local soft_timeout="$3"
  local silent_timeout="$4"
  local quicklisp_setup="${QUICKLISP_SETUP:-${HOME}/quicklisp/setup.lisp}"
  local overwatch_bin="/home/rahul/.local/bin/overwatch"
  local -a runner

  if [[ -f "${REPO_ROOT}/ptui/.tools/quicklisp/setup.lisp" ]]; then
    quicklisp_setup="${REPO_ROOT}/ptui/.tools/quicklisp/setup.lisp"
  fi

  [[ -f "${quicklisp_setup}" ]] || fail "quicklisp setup not found at ${quicklisp_setup}"
  command -v sbcl >/dev/null 2>&1 || fail "sbcl not found on PATH"

  runner=(
    sbcl
    --noinform
    --non-interactive
    --script
    /dev/stdin
    "${suites}"
    "${REPO_ROOT}"
    "${quicklisp_setup}"
  )

  echo "==> focused suites: ${suites}"
  if [[ -x "${overwatch_bin}" ]]; then
    timeout "${hard_timeout}" "${overwatch_bin}" run --profile generic --stream --soft-timeout "${soft_timeout}" --silent-timeout "${silent_timeout}" -- "${runner[@]}" <<'EOF'
(labels ((split-comma-separated (text)
           (let ((items '())
                 (start 0)
                 (length (length text)))
             (loop for position = (position #\, text :start start)
                   do (push (subseq text start (or position length)) items)
                   if position
                     do (setf start (1+ position))
                   else
                     do (return (nreverse items))))))
  (let* ((argv (or #+sbcl sb-ext:*posix-argv* #-sbcl nil))
         (suite-spec (or (and argv (second argv)) ""))
         (repo-root-arg (or (and argv (third argv)) ""))
         (quicklisp-arg (or (and argv (fourth argv)) ""))
         (repo-root (and (plusp (length repo-root-arg))
                         (truename repo-root-arg))))
    (unless repo-root
      (error "Unable to resolve repository root from ~S" repo-root-arg))
    (unless (plusp (length (string-trim '(#\Space #\Tab #\Newline #\Return)
                                        suite-spec)))
      (error "Pass a comma-separated suite list as the first script argument."))
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
           (suite-names (split-comma-separated suite-spec)))
      (dolist (suite-name suite-names)
        (let* ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) suite-name))
               (suite (and (plusp (length trimmed))
                           (find-symbol trimmed "AMOEBUM/TEST"))))
          (unless suite
            (error "Missing AMOEBUM/TEST suite named ~S" trimmed))
          (let ((results (funcall run-fn suite)))
            (unless (funcall status-fn results)
              (funcall explain-fn results)
              (sb-ext:exit :code 1))))))
    (format t "AMOEBUM_FOCUSED_SUITES_OK~%")))
EOF
  else
    timeout "${hard_timeout}" "${runner[@]}" <<'EOF'
(labels ((split-comma-separated (text)
           (let ((items '())
                 (start 0)
                 (length (length text)))
             (loop for position = (position #\, text :start start)
                   do (push (subseq text start (or position length)) items)
                   if position
                     do (setf start (1+ position))
                   else
                     do (return (nreverse items))))))
  (let* ((argv (or #+sbcl sb-ext:*posix-argv* #-sbcl nil))
         (suite-spec (or (and argv (second argv)) ""))
         (repo-root-arg (or (and argv (third argv)) ""))
         (quicklisp-arg (or (and argv (fourth argv)) ""))
         (repo-root (and (plusp (length repo-root-arg))
                         (truename repo-root-arg))))
    (unless repo-root
      (error "Unable to resolve repository root from ~S" repo-root-arg))
    (unless (plusp (length (string-trim '(#\Space #\Tab #\Newline #\Return)
                                        suite-spec)))
      (error "Pass a comma-separated suite list as the first script argument."))
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
           (suite-names (split-comma-separated suite-spec)))
      (dolist (suite-name suite-names)
        (let* ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) suite-name))
               (suite (and (plusp (length trimmed))
                           (find-symbol trimmed "AMOEBUM/TEST"))))
          (unless suite
            (error "Missing AMOEBUM/TEST suite named ~S" trimmed))
          (let ((results (funcall run-fn suite)))
            (unless (funcall status-fn results)
              (funcall explain-fn results)
              (sb-ext:exit :code 1))))))
    (format t "AMOEBUM_FOCUSED_SUITES_OK~%")))
EOF
  fi
}

verify_worktrees() {
  run_focused_suites "WORKTREE-RUNTIME-SUITE,WORKER-SUPERVISOR-SUITE,SWARM-EXECUTION-SEMANTICS-SUITE,WORKER-DASHBOARD-SUITE" 1200 1200 300
  run_cmd timeout 240 make build
}

verify_packages() {
  run_cmd timeout 240 make build
  run_cmd timeout 180 ./bin/package-surface-audit.sh
  run_focused_suites "WORKTREE-RUNTIME-SUITE,WORKER-SUPERVISOR-SUITE" 1200 1200 300
  run_plan_validate
}

verify_state() {
  run_focused_suites "FP-COLLECTIONS-SUITE" 240 240 120
  run_focused_suites "MEMORY-COMMAND-SUITE" 240 240 120
  run_focused_suites "CONVERSATION-ROUNDTRIP-SUITE,CONVERSATION-UNIT-SUITE,SESSION-RESUME-SUITE" 1200 1200 300
  run_focused_suites "CHECKPOINT-ROTATION-SUITE" 240 240 120
}

verify_policy() {
  run_focused_suites "PLAN-EXECUTION-UNIT-SUITE,PLAN-EXECUTION-TRANSITION-TABLE-SUITE,PERMISSIONS-UNIT-SUITE,PERMISSION-PATH-NORMALIZATION-SUITE,PERMISSION-PATH-MEMORY-SUITE,PERMISSION-ARGUMENT-GRANULARITY-SUITE" 1200 1200 300
}

verify_ui() {
  run_focused_suites "STREAMING-STEP-SUITE,STREAMING-BUDGET-SUITE,INCREMENTAL-MARKDOWN-SUITE,CHAT-SNAPSHOT-SUITE,APPROVAL-DIALOG-GUARD-SUITE,KEYBOARD-NAV-SUITE" 1800 1800 300
}

verify_extensions() {
  run_focused_suites "EXTENSION-LOADER-SUITE,EXTENSION-LIFECYCLE-SUITE,EXTENSION-DISCOVERY-SUITE,EXTENSION-MANIFEST-SUITE,EXTENSION-SECURITY-SUITE,EXTENSION-CLI-SUITE,ASDF-EXTENSIONS-SUITE" 1800 1800 300
}

verify_shell() {
  run_focused_suites "SHELL-ENV-SUITE,SHELL-SAFETY-SUITE,SHELL-BACKGROUND-SUITE,SHELL-RUNAWAY-OUTPUT-SUITE,WRITE-SAFETY-SUITE,EDIT-VALIDATION-SUITE,TOOL-ARGUMENT-PROMPTING-SUITE,PERMISSION-ARGUMENT-GRANULARITY-SUITE" 1800 1800 300
}

verify_macros() {
  run_focused_suites "MACROEXPAND-GOLDEN-SUITE,DEFTOOL-TYPE-VALIDATION-SUITE,DEFTOOL-DANGEROUS-PERMISSION-SUITE,COMPILE-VALIDATION-CONDITIONS-SUITE,DEFHOOK-CROSS-REFERENCE-SUITE,ARGUMENT-PATTERN-DISPATCH-SUITE" 1800 1800 300
}

profile="${1:-}"
case "${profile}" in
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
  "") usage >&2; exit 2 ;;
esac

if [[ $# -ne 1 ]]; then
  usage >&2
  exit 2
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
  all)
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
