;;;; amoebum/src/plan-execution.lisp
;;;;
;;;; Residual facade for the plan-execution subsystem.
;;;;
;;;; After NXT-363 + NXT-415 this file holds ONLY:
;;;;   - Shared defparameters (status keywords, command heads,
;;;;     continuity bound, *plan-execution-state* default + git runner)
;;;;   - The struct definitions every sibling module accesses
;;;;     (plan-execution-step, plan-execution-output-entry,
;;;;      plan-execution-state, plan-execution-context,
;;;;      plan-execution-transition)
;;;;   - The single accessor `current-plan-execution-state` used to
;;;;     resolve the default global state
;;;;
;;;; All real logic is in the sibling files loaded immediately after this
;;;; one in amoebum.asd, in the following order:
;;;;
;;;;   plan-execution-output.lisp        — entry normalization + output append
;;;;   plan-execution-context.lisp       — transition decision context, dry-run
;;;;   plan-execution-helpers.lisp       — status / step lookup / git runner
;;;;   plan-execution-effects.lisp       — apply-state/step/effects! reducer
;;;;   plan-execution-rollback.lisp      — git baseline + restore helpers
;;;;   plan-execution-state-machine.lisp — declarative transition table
;;;;   plan-execution-lifecycle.lisp     — public start/pause/resume/abort
;;;;   plan-execution-loop.lisp          — execute-approved-plan-steps loop
;;;;
;;;; PUBLIC CONTRACT (UNCHANGED): execute-approved-plan-steps and
;;;; execute-next-approved-plan-step keep their signatures, return
;;;; shapes, and restart-preservation properties from before NXT-415.
;;;; They are defined in src/plan-execution-loop.lisp.

(in-package :amoebum)

;;; --- Shared defparameters ----------------------------------------

(defparameter *known-plan-execution-statuses*
  '(:idle :ready :running :paused :completed :failed :aborted))

(defparameter *plan-execution-continuity-max-lines* 200)

(defparameter *plan-execution-command-heads*
  '("bash" "sh" "zsh" "fish" "timeout" "make" "cmake" "ninja" "pytest" "npm"
    "pnpm" "yarn" "node" "python" "python3" "pip" "uv" "go" "cargo" "git"
    "rg" "fd" "ls" "cat" "sed" "awk" "grep" "perl" "sbcl" "clisp" "qlot"
    "nix" "docker" "podman" "kubectl"))

;;; --- Struct definitions -----------------------------------------

(defstruct (plan-execution-step
            (:constructor make-plan-execution-step
                (&key index
                      description
                      (file-paths '())
                      (risk :medium)
                      (depends-on '())
                      (approved-p nil)
                      (status :pending)
                      started-at
                      finished-at)))
  index
  description
  (file-paths '() :type list)
  (risk :medium)
  (depends-on '() :type list)
  (approved-p nil :type boolean)
  (status :pending)
  started-at
  finished-at)

(defstruct (plan-execution-output-entry
            (:constructor make-plan-execution-output-entry
                (&key
                  line
                  step-index
                  (phase :execution)
                  (severity :info)
                  (style :plain)
                  (recovery-actions '())
                  timestamp)))
  line
  step-index
  (phase :execution)
  (severity :info)
  (style :plain)
  (recovery-actions '() :type list)
  timestamp)

(defstruct (plan-execution-state
            (:constructor %make-plan-execution-state
                (&key run-id
                      (status :idle)
                      created-at
                      started-at
                      finished-at
                      source-plan-exited-at
                      source-plan-exit-reason
                      (steps '())
                      (ordered-step-indexes '())
                      (approved-step-indexes '())
                      (pending-step-indexes '())
                      (completed-step-indexes '())
                      (continuity-output '())
                      current-step-index
                      failure-reason
                      abort-reason
                      rollback-baseline-stash
                      rollback-baseline-directory
                      rollback-attempted-p
                      rollback-succeeded-p
                      rollback-notes
                      awaiting-approval-step-index
                      (interactive-p nil))))
  run-id
  (status :idle)
  created-at
  started-at
  finished-at
  source-plan-exited-at
  source-plan-exit-reason
  (steps '() :type list)
  (ordered-step-indexes '() :type list)
  (approved-step-indexes '() :type list)
  (pending-step-indexes '() :type list)
  (completed-step-indexes '() :type list)
  (continuity-output '() :type list)
  current-step-index
  failure-reason
  abort-reason
  rollback-baseline-stash
  rollback-baseline-directory
  (rollback-attempted-p nil :type boolean)
  (rollback-succeeded-p nil :type boolean)
  rollback-notes
  awaiting-approval-step-index
  (interactive-p nil :type boolean))

(defstruct (plan-execution-context
            (:constructor make-plan-execution-context
                (&key executor
                      state
                      (rollback-on-failure-p t)
                      (signal-failure-p t)
                      (interactive-p nil)
                      rollback-directory
                      (execution-results '())
                      failure-condition)))
  executor
  state
  (rollback-on-failure-p t :type boolean)
  (signal-failure-p t :type boolean)
  (interactive-p nil :type boolean)
  rollback-directory
  (execution-results '() :type list)
  failure-condition)

(defstruct (plan-execution-transition
            (:constructor make-plan-execution-transition
                (&key
                  (state-updates '())
                  (step-updates '())
                  (effects '())
                  (done-p nil)
                  result
                  condition
                  decision-context
                  (structured-trace '()))))
  (state-updates '() :type list)
  (step-updates '() :type list)
  (effects '() :type list)
  (done-p nil :type boolean)
  result
  condition
  decision-context
  (structured-trace '() :type list))

;;; --- Module-global default state + git runner ------------------

(defparameter *plan-execution-state* (%make-plan-execution-state))
(defparameter *plan-execution-git-command-runner* nil)

;;; --- Default-state accessor used by every public entry point ---

(defun current-plan-execution-state ()
  (or *plan-execution-state*
      (setf *plan-execution-state* (%make-plan-execution-state))))
