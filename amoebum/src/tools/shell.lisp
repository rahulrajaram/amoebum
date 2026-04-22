(in-package :amoebum)

;;;; ---------------------------------------------------------------------------
;;;; Shell tool: top-level facade and `bash-exec` registration.
;;;;
;;;; This file is the thin entry point that combines the three submodules:
;;;;
;;;;   * `tools/shell/env.lisp`        — environment / cwd / normalization
;;;;                                     helpers (`%run-shell-command-with-runtime`
;;;;                                     wires these via `%prepare-shell-runtime`)
;;;;   * `tools/shell/runtime.lisp`    — permission/timeout/output-budget
;;;;                                     execution (`%run-shell-command`)
;;;;   * `tools/shell/background.lisp` — async task tracking
;;;;                                     (`%start-background-shell-task`,
;;;;                                     `%list-shell-tasks`,
;;;;                                     `%cleanup-shell-tasks`,
;;;;                                     `%fetch-shell-task`)
;;;;
;;;; What lives here:
;;;;   * `shell-execution-options` decoder for the legacy positional
;;;;     `%execute-shell-command` argument shape;
;;;;   * `%run-shell-command-with-runtime` dispatcher (foreground vs.
;;;;     background);
;;;;   * `%execute-shell-command` orchestrator;
;;;;   * the `bash-exec` `deftool` form (mode dispatch + arg normalization).
;;;;
;;;; Behavior — including timeout chain, signal propagation,
;;;; output-truncation budgets, branch-scope enforcement, and background-task
;;;; lifecycle — is preserved verbatim from the prior monolithic file.
;;;; ---------------------------------------------------------------------------

(defstruct (shell-execution-options
            (:constructor make-shell-execution-options
                (&key shell-executable background-p
                 enable-profile-init-p enable-project-env-p)))
  shell-executable
  background-p
  enable-profile-init-p
  enable-project-env-p)

(defun %decode-shell-execution-options (shell-or-background
                                        init-shell-profile-p
                                        init-project-env-p
                                        background)
  (let ((legacy-call-p (and (null init-shell-profile-p)
                            (null init-project-env-p)
                            (null background))))
    (make-shell-execution-options
     :shell-executable (unless legacy-call-p shell-or-background)
     :background-p (if legacy-call-p shell-or-background background)
     :enable-profile-init-p (and (not legacy-call-p) init-shell-profile-p)
     :enable-project-env-p (and (not legacy-call-p) init-project-env-p))))

(defun %run-shell-command-with-runtime (command directory timeout-seconds
                                        max-output-chars max-output-bytes max-output-lines
                                        resolved-shell profiles env-vars background-p)
  (if background-p
      (%start-background-shell-task command
                                    directory
                                    timeout-seconds
                                    max-output-chars
                                    max-output-bytes
                                    max-output-lines
                                    :shell-executable resolved-shell
                                    :profile-files profiles
                                    :env-vars env-vars)
      (%run-shell-command command
                          directory
                          timeout-seconds
                          max-output-chars
                          :max-output-bytes max-output-bytes
                          :max-output-lines max-output-lines
                          :shell-executable resolved-shell
                          :profile-files profiles
                          :env-vars env-vars)))

(defun %execute-shell-command (command cwd timeout-seconds
                               max-output-chars max-output-bytes max-output-lines
                               &optional shell-or-background
                                 init-shell-profile-p
                                 init-project-env-p
                                 background)
  (let ((options (%decode-shell-execution-options shell-or-background
                                                  init-shell-profile-p
                                                  init-project-env-p
                                                  background)))
    (multiple-value-bind (directory resolved-shell profiles env-vars)
        (%prepare-shell-runtime cwd
                                (shell-execution-options-enable-profile-init-p options)
                                (shell-execution-options-enable-project-env-p options)
                                (shell-execution-options-enable-project-env-p options)
                                (shell-execution-options-shell-executable options))
      (%persist-shell-directory directory)
      (%run-shell-command-with-runtime
       command
       directory
       timeout-seconds
       max-output-chars
       max-output-bytes
       max-output-lines
       resolved-shell
       profiles
       env-vars
       (shell-execution-options-background-p options)))))

(deftool bash-exec ((command (or null string)
                      :description "Shell command to execute in bash -lc"
                      :default nil)
                    (cwd (or null pathname)
                     :description "Optional working directory; persists across calls"
                     :default nil)
                    (timeout-seconds (or null integer)
                     :description "Command timeout in seconds (1-600)"
                     :default nil)
                    (max-output-chars (or null integer)
                     :description "Maximum captured characters for stdout/stderr"
                     :default nil)
                    (max-output-bytes (or null integer)
                     :description "Maximum combined stdout/stderr bytes before forced termination"
                     :default nil)
                    (max-output-lines (or null integer)
                     :description "Maximum combined stdout/stderr lines before forced termination"
                     :default nil)
                    (background boolean
                     :description "Run command asynchronously and return task ID"
                     :default nil)
                    (task-id (or null string)
                     :description "Background task ID to poll for completion"
                     :default nil)
                    (list-tasks boolean
                     :description "List background shell tasks"
                     :default nil)
                    (include-finished boolean
                     :description "Include completed tasks when LIST-TASKS is true"
                     :default t)
                    (cleanup-completed boolean
                     :description "Remove completed/failed/timed-out background tasks"
                     :default nil)
                    (include-running boolean
                     :description "Also remove running tasks when CLEANUP-COMPLETED is true"
                     :default nil))
  "Execute shell commands with stdout/stderr capture and background task retrieval."
  (:permission :full-auto)
  (:dangerous t)
  (:category :shell)
  (:timeout 600)
  (let ((mode-count (+ (if command 1 0)
                       (if task-id 1 0)
                       (if list-tasks 1 0)
                       (if cleanup-completed 1 0))))
    (when (> mode-count 1)
      (error "Choose exactly one mode: COMMAND, TASK-ID, LIST-TASKS, or CLEANUP-COMPLETED."))
    (cond
      (cleanup-completed
       (%cleanup-shell-tasks :include-running include-running))
      (list-tasks
       (%list-shell-tasks :include-finished include-finished))
      (task-id
       (%fetch-shell-task task-id))
      ((null command)
       (error "COMMAND is required unless TASK-ID, LIST-TASKS, or CLEANUP-COMPLETED is provided."))
      (t
       (let ((timeout (%normalize-timeout-seconds timeout-seconds))
             (max-output (%normalize-max-output-chars max-output-chars))
             (max-output-bytes* (%normalize-max-output-bytes max-output-bytes))
             (max-output-lines* (%normalize-max-output-lines max-output-lines)))
         (%execute-shell-command (%normalize-command command)
                                 cwd
                                 timeout
                                 max-output
                                 max-output-bytes*
                                 max-output-lines*
                                 nil
                                 t
                                 t
                                 background))))))
