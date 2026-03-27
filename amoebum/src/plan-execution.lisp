(in-package :amoebum)

(defparameter *known-plan-execution-statuses*
  '(:idle :ready :running :paused :completed :failed :aborted))

(defparameter *plan-execution-continuity-max-lines* 200)

(defparameter *plan-execution-command-heads*
  '("bash" "sh" "zsh" "fish" "timeout" "make" "cmake" "ninja" "pytest" "npm"
    "pnpm" "yarn" "node" "python" "python3" "pip" "uv" "go" "cargo" "git"
    "rg" "fd" "ls" "cat" "sed" "awk" "grep" "perl" "sbcl" "clisp" "qlot"
    "nix" "docker" "podman" "kubectl"))

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

(defparameter *plan-execution-state* (%make-plan-execution-state))
(defparameter *plan-execution-git-command-runner* nil)

(defun %normalize-plan-execution-status (value)
  (let* ((status-text (typecase value
                        (keyword (symbol-name value))
                        (symbol (symbol-name value))
                        (string value)
                        (t nil)))
         (status (if (and (stringp status-text)
                          (plusp (length (string-trim '(#\Space #\Tab #\Newline #\Return)
                                                      status-text))))
                     (intern (string-upcase (string-trim '(#\Space #\Tab #\Newline #\Return)
                                                         status-text))
                             :keyword)
                     :idle)))
    (if (member status *known-plan-execution-statuses* :test #'eq)
        status
        :idle)))

(defun %plan-execution-terminal-status-p (status)
  (member (%normalize-plan-execution-status status)
          '(:completed :failed :aborted)
          :test #'eq))

(defun %find-plan-execution-step (state step-index)
  (find step-index
        (plan-execution-state-steps state)
        :key #'plan-execution-step-index
        :test #'=))

(defun %mark-plan-execution-step-running (step)
  (check-type step plan-execution-step)
  (setf (plan-execution-step-status step) :running
        (plan-execution-step-started-at step) (get-universal-time)
        (plan-execution-step-finished-at step) nil)
  step)

(defun %mark-plan-execution-step-completed (step)
  (check-type step plan-execution-step)
  (setf (plan-execution-step-status step) :completed
        (plan-execution-step-finished-at step) (get-universal-time))
  step)

(defun %mark-plan-execution-step-pending (step)
  (check-type step plan-execution-step)
  (setf (plan-execution-step-status step) :pending
        (plan-execution-step-started-at step) nil
        (plan-execution-step-finished-at step) nil)
  step)

(defun %mark-plan-execution-step-blocked (step)
  (check-type step plan-execution-step)
  (let ((now (get-universal-time)))
    (setf (plan-execution-step-status step) :blocked
          (plan-execution-step-started-at step)
          (or (plan-execution-step-started-at step) now)
          (plan-execution-step-finished-at step) now))
  step)

(defun %complete-plan-execution-if-finished (state)
  (check-type state plan-execution-state)
  (when (null (plan-execution-state-pending-step-indexes state))
    (unless (eq :completed (plan-execution-state-status state))
      (setf (plan-execution-state-status state) :completed
            (plan-execution-state-current-step-index state) nil
            (plan-execution-state-finished-at state) (get-universal-time))
      (plan-execution-append-output
       "LIVE> All approved steps completed."
       :phase :execution
       :style :success
       :state state)))
  state)

(defun %plan-step-order (steps)
  (loop for step in (or steps '())
        for index = (plan-step-index step)
        when (integerp index)
          collect index))

(defun %approved-step-indexes-for-execution (plan-state ordered-indexes)
  (remove-if-not (lambda (index)
                   (member index ordered-indexes :test #'=))
                 (or (plan-mode-state-approved-step-indexes plan-state) '())))

(defun %plan-step->execution-step (step approved-p)
  (make-plan-execution-step
   :index (plan-step-index step)
   :description (plan-step-description step)
   :file-paths (copy-list (or (plan-step-file-paths step) '()))
   :risk (plan-step-risk step)
   :depends-on (copy-list (or (plan-step-depends-on step) '()))
   :approved-p (not (null approved-p))
   :status :pending))

(defun %safe-plan-execution-string (value &optional (fallback ""))
  (cond
    ((and (stringp value)
          (plusp (length value)))
     value)
    ((null value)
     fallback)
    (t
     (princ-to-string value))))

(defun %publish-plan-step-status-event (state step &key status)
  (check-type state plan-execution-state)
  (check-type step plan-execution-step)
  (let ((step-index (plan-execution-step-index step))
        (run-id (plan-execution-state-run-id state)))
    (when (and (integerp step-index)
               (stringp run-id)
               (plusp (length run-id)))
      (publish (current-event-bus)
               (make-plan-step-status-event
                :run-id run-id
                :step-index step-index
                :status (or status
                            (plan-execution-step-status step)
                            :pending)
                :description (plan-execution-step-description step)))))
  state)

(defun %publish-plan-step-status-snapshot (state)
  (check-type state plan-execution-state)
  (dolist (step (plan-execution-state-steps state))
    (%publish-plan-step-status-event state step))
  state)

(defun %normalize-output-phase (value)
  (let* ((text (%safe-plan-execution-string value "execution"))
         (phase (intern (string-upcase (string-trim '(#\Space #\Tab #\Newline #\Return)
                                                    text))
                        :keyword)))
    (if (member phase '(:execution :dry-run :system) :test #'eq)
        phase
        :execution)))

(defun %normalize-output-severity (value)
  (let* ((text (%safe-plan-execution-string value "info"))
         (severity (intern (string-upcase (string-trim '(#\Space #\Tab #\Newline #\Return)
                                                       text))
                           :keyword)))
    (if (member severity '(:debug :info :warning :error :critical) :test #'eq)
        severity
        :info)))

(defun %normalize-output-style (value)
  (let* ((text (%safe-plan-execution-string value "plain"))
         (style (intern (string-upcase (string-trim '(#\Space #\Tab #\Newline #\Return)
                                                    text))
                        :keyword)))
    (if (member style '(:plain :meta :preview :success :warning :error) :test #'eq)
        style
        :plain)))

(defun %normalize-recovery-actions (actions)
  (remove-duplicates
   (remove nil
           (loop for action in (or actions '())
                 for text = (string-trim '(#\Space #\Tab #\Newline #\Return)
                                         (%safe-plan-execution-string action ""))
                 when (plusp (length text))
                   collect text))
   :test #'string=))

(defun %default-recovery-actions (state step-index)
  (check-type state plan-execution-state)
  (let* ((step (and (integerp step-index)
                    (%find-plan-execution-step state step-index)))
         (file-paths (and step
                          (copy-list (or (plan-execution-step-file-paths step) '()))))
         (recovery-actions
           (list (format nil
                         "Retry step ~D after fixing the failing command, then rerun /execute."
                         step-index)
                 (format nil
                         "Review step ~D details with /plan review before retrying."
                         step-index)
                 "Choose next action: rerun /execute, inspect with /plan review, or revise with /plan modify.")))
    (when file-paths
      (let ((visible-paths (subseq file-paths 0 (min 3 (length file-paths)))))
        (setf recovery-actions
              (append recovery-actions
                      (list (format nil
                                    "Inspect referenced files: ~{~A~^, ~}."
                                    visible-paths))))))
    (%normalize-recovery-actions recovery-actions)))

(defun %trim-plan-execution-output (entries)
  (let ((overflow (- (length entries) *plan-execution-continuity-max-lines*)))
    (if (> overflow 0)
        (nthcdr overflow entries)
        entries)))

(defun plan-execution-append-output (line &key
                                            step-index
                                            (phase :execution)
                                            (severity :info)
                                            (style :plain)
                                            recovery-actions
                                            (state (current-plan-execution-state)))
  (check-type state plan-execution-state)
  (let* ((text (string-trim '(#\Space #\Tab #\Newline #\Return)
                            (%safe-plan-execution-string line "")))
         (normalized-step-index (and (integerp step-index) step-index))
         (normalized-severity (%normalize-output-severity severity))
         (normalized-recovery-actions (%normalize-recovery-actions recovery-actions))
         (resolved-recovery-actions
           (if (and (null normalized-recovery-actions)
                    normalized-step-index
                    (member normalized-severity '(:error :critical) :test #'eq))
               (%default-recovery-actions state normalized-step-index)
               normalized-recovery-actions)))
    (unless (plusp (length text))
      (return-from plan-execution-append-output state))
    (let* ((entry (make-plan-execution-output-entry
                   :line text
                   :step-index normalized-step-index
                   :phase (%normalize-output-phase phase)
                   :severity normalized-severity
                   :style (%normalize-output-style style)
                   :recovery-actions (copy-list resolved-recovery-actions)
                   :timestamp (get-universal-time)))
           (updated (append (plan-execution-state-continuity-output state)
                            (list entry))))
      (setf (plan-execution-state-continuity-output state)
            (%trim-plan-execution-output updated))
      state)))

(defun plan-execution-output-lines (&optional (state (current-plan-execution-state)))
  (check-type state plan-execution-state)
  (loop for entry in (plan-execution-state-continuity-output state)
        collect (plan-execution-output-entry-line entry)))

(defun %inline-code-spans (text)
  (let* ((source (%safe-plan-execution-string text ""))
         (length (length source))
         (index 0)
         (spans '()))
    (loop while (< index length) do
      (let ((start (position #\` source :start index)))
        (if (null start)
            (setf index length)
            (let ((end (position #\` source :start (1+ start))))
              (if (null end)
                  (setf index length)
                  (let ((snippet
                          (string-trim '(#\Space #\Tab #\Newline #\Return)
                                       (subseq source (1+ start) end))))
                    (when (plusp (length snippet))
                      (push snippet spans))
                    (setf index (1+ end))))))))
    (nreverse spans)))

(defun %leading-token (text)
  (let* ((trimmed
           (string-trim '(#\Space #\Tab #\Newline #\Return)
                        (%safe-plan-execution-string text "")))
         (length (length trimmed)))
    (if (zerop length)
        ""
        (let ((end (or (position-if (lambda (char)
                                      (member char '(#\Space #\Tab #\Newline #\Return)))
                                    trimmed)
                       length)))
          (string-downcase (subseq trimmed 0 end))))))

(defun %commandish-p (text)
  (let* ((trimmed
           (string-trim '(#\Space #\Tab #\Newline #\Return)
                        (%safe-plan-execution-string text "")))
         (length (length trimmed))
         (token (%leading-token trimmed)))
    (and (plusp length)
         (or (member token *plan-execution-command-heads* :test #'string=)
             (and (>= length 2)
                  (string= (subseq trimmed 0 2) "./"))
             (and (>= length 2)
                  (string= (subseq trimmed 0 2) "~/"))
             (char= (char trimmed 0) #\/)
             (search "&&" trimmed :test #'char=)
             (search "||" trimmed :test #'char=)
             (search "|" trimmed :test #'char=)
             (search ";" trimmed :test #'char=)
             (search ">" trimmed :test #'char=)
             (search "<" trimmed :test #'char=)))))

(defun %plan-step-command-previews (step)
  (check-type step plan-execution-step)
  (let* ((description (%safe-plan-execution-string
                       (plan-execution-step-description step)
                       ""))
         (inline-spans (%inline-code-spans description)))
    (remove-duplicates
     (loop for span in inline-spans
           for normalized = (string-trim '(#\Space #\Tab #\Newline #\Return)
                                         (%safe-plan-execution-string span ""))
           when (%commandish-p normalized)
             collect normalized)
     :test #'string=)))

(defun %plan-step-policy-path (step)
  (check-type step plan-execution-step)
  (first (copy-list (or (plan-execution-step-file-paths step) '()))))

(defun %plan-step-policy-command (step)
  (check-type step plan-execution-step)
  (first (%plan-step-command-previews step)))

(defun %plan-transition-context-data (state step event &key result condition)
  (check-type state plan-execution-state)
  (check-type step plan-execution-step)
  (list :run-id (plan-execution-state-run-id state)
        :state-status (plan-execution-state-status state)
        :step-description (plan-execution-step-description step)
        :step-status (plan-execution-step-status step)
        :step-file-paths (copy-list (or (plan-execution-step-file-paths step) '()))
        :approved-step-count (length (or (plan-execution-state-approved-step-indexes state) '()))
        :pending-step-count (length (or (plan-execution-state-pending-step-indexes state) '()))
        :completed-step-count (length (or (plan-execution-state-completed-step-indexes state) '()))
        :event event
        :result-summary (and result (%summarize-execution-result result))
        :condition-summary (when condition
                             (handler-case (princ-to-string condition)
                               (error () "step execution failed")))))

(defun %build-plan-transition-decision-context (state event step &key result condition)
  (check-type state plan-execution-state)
  (check-type step plan-execution-step)
  (build-policy-decision-context
   :kind :plan-transition
   :path (%plan-step-policy-path step)
   :command (%plan-step-policy-command step)
   :permission-mode :plan
   :plan-step-index (plan-execution-step-index step)
   :plan-event event
   :rules '()
   :context-data (%plan-transition-context-data state step event
                                                :result result
                                                :condition condition)
   :decision-id (format nil "~A:~A:~A"
                        (%safe-plan-execution-string
                         (plan-execution-state-run-id state)
                         "plan-exec")
                        (or (plan-execution-step-index step) "unknown")
                        event)))

(defun %summarize-execution-result (result)
  (let* ((text (%safe-plan-execution-string result ""))
         (trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) text)))
    (if (zerop (length trimmed))
        "completed without textual output"
        (let* ((line-end (or (position #\Newline trimmed) (length trimmed)))
               (line (subseq trimmed 0 line-end)))
          (if (> (length line) 120)
              (concatenate 'string (subseq line 0 117) "...")
              line)))))

(defun prime-plan-execution-continuity (&optional (state (current-plan-execution-state)))
  (check-type state plan-execution-state)
  (setf (plan-execution-state-continuity-output state) '())
  (plan-execution-append-output
   (format nil "Execution continuity initialized for run ~A."
           (%safe-plan-execution-string (plan-execution-state-run-id state) "unknown"))
   :phase :system
   :style :meta
   :state state)
  (let ((preview-count 0))
    (dolist (step (plan-execution-state-steps state))
      (let ((step-index (plan-execution-step-index step)))
        (when (and (integerp step-index)
                   (member step-index
                           (plan-execution-state-approved-step-indexes state)
                           :test #'=))
          (dolist (command (%plan-step-command-previews step))
            (incf preview-count)
            (plan-execution-append-output
             (format nil "DRY-RUN> [step ~D approved | non-executed] ~A"
                     step-index
                     command)
             :step-index step-index
             :phase :dry-run
             :style :preview
             :state state)))))
    (when (zerop preview-count)
      (plan-execution-append-output
       "DRY-RUN> No command snippets detected in approved steps."
       :phase :dry-run
       :style :meta
       :state state)))
  state)

(defun %next-plan-execution-run-id ()
  (format nil "plan-exec-~D-~D"
          (get-universal-time)
          (get-internal-real-time)))

(defun default-plan-execution-git-command-runner (directory args)
  (handler-case
      (multiple-value-bind (stdout stderr exit-code)
          (uiop:run-program (append (list "git") args)
                            :directory directory
                            :ignore-error-status t
                            :output :string
                            :error-output :string)
        (list :stdout (or stdout "")
              :stderr (or stderr "")
              :exit-code (or exit-code 0)))
    (error (condition)
      (list :stdout ""
            :stderr (princ-to-string condition)
            :exit-code 127))))

(defun %plan-execution-run-git (directory args)
  (let ((runner (or *plan-execution-git-command-runner*
                    #'default-plan-execution-git-command-runner)))
    (funcall runner directory args)))

(defun %plan-execution-git-ok-p (result)
  (and (listp result)
       (zerop (or (getf result :exit-code) 1))))

(defun %plan-execution-git-output (result)
  (string-trim '(#\Space #\Tab #\Newline #\Return)
               (format nil "~A~@[ ~A~]"
                       (or (getf result :stdout) "")
                       (let ((stderr (string-trim '(#\Space #\Tab #\Newline #\Return)
                                                  (or (getf result :stderr) ""))))
                         (and (plusp (length stderr))
                              stderr)))))

(defun %resolve-plan-execution-rollback-directory (rollback-directory)
  (let* ((raw (or rollback-directory (uiop:getcwd)))
         (resolved (ignore-errors (uiop:ensure-directory-pathname raw))))
    (or resolved raw)))

(defun %prepare-plan-execution-rollback-baseline (state rollback-directory)
  (check-type state plan-execution-state)
  (let* ((directory (%resolve-plan-execution-rollback-directory rollback-directory))
         (inside-result (%plan-execution-run-git directory
                                                 '("rev-parse" "--is-inside-work-tree"))))
    (setf (plan-execution-state-rollback-baseline-directory state) directory
          (plan-execution-state-rollback-baseline-stash state) nil
          (plan-execution-state-rollback-attempted-p state) nil
          (plan-execution-state-rollback-succeeded-p state) nil
          (plan-execution-state-rollback-notes state) nil)
    (unless (and (%plan-execution-git-ok-p inside-result)
                 (string-equal "true" (%plan-execution-git-output inside-result)))
      (setf (plan-execution-state-rollback-notes state)
            (format nil "Rollback baseline unavailable: ~A"
                    (%plan-execution-git-output inside-result)))
      (return-from %prepare-plan-execution-rollback-baseline nil))
    (let* ((before-stash (%plan-execution-run-git directory
                                                  '("rev-parse" "--verify" "-q" "refs/stash")))
           (before-hash (%plan-execution-git-output before-stash))
           (message (format nil "plan-exec-baseline-~A"
                            (%safe-plan-execution-string
                             (plan-execution-state-run-id state)
                             "unknown")))
           (stash-result (%plan-execution-run-git directory
                                                  (list "stash"
                                                        "push"
                                                        "--include-untracked"
                                                        "--message"
                                                        message)))
           (after-stash (%plan-execution-run-git directory
                                                 '("rev-parse" "--verify" "-q" "refs/stash")))
           (after-hash (%plan-execution-git-output after-stash))
           (stash-created-p (and (%plan-execution-git-ok-p stash-result)
                                 (%plan-execution-git-ok-p after-stash)
                                 (plusp (length after-hash))
                                 (not (string= after-hash before-hash)))))
      (unless (%plan-execution-git-ok-p stash-result)
        (setf (plan-execution-state-rollback-notes state)
              (format nil "Rollback baseline stash failed: ~A"
                      (%plan-execution-git-output stash-result)))
        (return-from %prepare-plan-execution-rollback-baseline nil))
      (when stash-created-p
        (let ((apply-result (%plan-execution-run-git directory
                                                     (list "stash" "apply" "--index" after-hash))))
          (unless (%plan-execution-git-ok-p apply-result)
            (setf (plan-execution-state-rollback-notes state)
                  (format nil "Rollback baseline apply failed: ~A"
                          (%plan-execution-git-output apply-result)))
            (return-from %prepare-plan-execution-rollback-baseline nil)))
        (setf (plan-execution-state-rollback-baseline-stash state) after-hash))
      t)))

(defun %drop-plan-execution-rollback-baseline (state)
  (check-type state plan-execution-state)
  (let ((stash (plan-execution-state-rollback-baseline-stash state))
        (directory (plan-execution-state-rollback-baseline-directory state)))
    (when (and (stringp stash)
               (plusp (length stash)))
      (%plan-execution-run-git directory (list "stash" "drop" stash))
      (setf (plan-execution-state-rollback-baseline-stash state) nil)))
  state)

(defun %rollback-plan-execution-via-git (state)
  (check-type state plan-execution-state)
  (let* ((directory (%resolve-plan-execution-rollback-directory
                     (plan-execution-state-rollback-baseline-directory state)))
         (stash (plan-execution-state-rollback-baseline-stash state))
         (reset-result (%plan-execution-run-git directory '("reset" "--hard" "HEAD")))
         (clean-result (%plan-execution-run-git directory '("clean" "-fd"))))
    (setf (plan-execution-state-rollback-attempted-p state) t)
    (unless (and (%plan-execution-git-ok-p reset-result)
                 (%plan-execution-git-ok-p clean-result))
      (setf (plan-execution-state-rollback-succeeded-p state) nil
            (plan-execution-state-rollback-notes state)
            (format nil "Rollback reset/clean failed: ~A | ~A"
                    (%plan-execution-git-output reset-result)
                    (%plan-execution-git-output clean-result)))
      (return-from %rollback-plan-execution-via-git nil))
    (when (and (stringp stash)
               (plusp (length stash)))
      (let ((apply-result (%plan-execution-run-git directory
                                                   (list "stash" "apply" "--index" stash))))
        (unless (%plan-execution-git-ok-p apply-result)
          (setf (plan-execution-state-rollback-succeeded-p state) nil
                (plan-execution-state-rollback-notes state)
                (format nil "Rollback stash apply failed: ~A"
                        (%plan-execution-git-output apply-result)))
          (return-from %rollback-plan-execution-via-git nil))))
    (%drop-plan-execution-rollback-baseline state)
    (setf (plan-execution-state-rollback-succeeded-p state) t
          (plan-execution-state-rollback-notes state) "Rollback restored git baseline.")
    t))

(defun current-plan-execution-state ()
  (or *plan-execution-state*
      (setf *plan-execution-state* (%make-plan-execution-state))))

(defun reset-plan-execution-state (&optional (state (current-plan-execution-state)))
  (check-type state plan-execution-state)
  (let ((transition (%dispatch-plan-execution-transition state :reset)))
    (%apply-plan-execution-transition! state transition))
  ;; Also clear fields not covered by the transition table state-updates
  (setf (plan-execution-state-run-id state) nil
        (plan-execution-state-created-at state) nil
        (plan-execution-state-source-plan-exited-at state) nil
        (plan-execution-state-source-plan-exit-reason state) nil
        (plan-execution-state-steps state) '()
        (plan-execution-state-ordered-step-indexes state) '()
        (plan-execution-state-approved-step-indexes state) '()
        (plan-execution-state-pending-step-indexes state) '()
        (plan-execution-state-completed-step-indexes state) '()
        (plan-execution-state-continuity-output state) '()
        (plan-execution-state-rollback-baseline-stash state) nil
        (plan-execution-state-rollback-baseline-directory state) nil
        (plan-execution-state-rollback-attempted-p state) nil
        (plan-execution-state-rollback-succeeded-p state) nil
        (plan-execution-state-rollback-notes state) nil)
  state)

(defun initialize-plan-execution (&key
                                    (plan-state (current-plan-mode-state))
                                    (state (current-plan-execution-state))
                                    run-id)
  (check-type plan-state plan-mode-state)
  (check-type state plan-execution-state)
  (let ((transition (%dispatch-plan-execution-transition state :initialize
                      :plan-state plan-state :run-id run-id)))
    (%apply-plan-execution-transition! state transition)
    state))

(defun plan-execution-ready-p (&optional (state (current-plan-execution-state)))
  (check-type state plan-execution-state)
  (eq :ready (plan-execution-state-status state)))

(defun %plan-execution-format-elapsed-seconds (elapsed-seconds)
  (let* ((total-seconds (max 0 (or elapsed-seconds 0)))
         (hours (truncate total-seconds 3600))
         (remaining (mod total-seconds 3600))
         (minutes (truncate remaining 60))
         (seconds (mod remaining 60)))
    (cond
      ((> hours 0)
       (format nil "~Dh ~Dm ~Ds" hours minutes seconds))
      ((> minutes 0)
       (format nil "~Dm ~Ds" minutes seconds))
      (t
       (format nil "~Ds" seconds)))))

(defun plan-execution-elapsed-seconds (&optional (state (current-plan-execution-state)))
  (check-type state plan-execution-state)
  (let* ((started-at (plan-execution-state-started-at state))
         (finished-at (plan-execution-state-finished-at state))
         (status (plan-execution-state-status state))
         (end-time (if (member status '(:completed :failed :aborted) :test #'eq)
                       finished-at
                       (get-universal-time))))
    (if (and (integerp started-at)
             (integerp end-time))
        (max 0 (- end-time started-at))
        0)))

(defun plan-execution-progress-line (&optional (state (current-plan-execution-state))
                                               &key
                                                 (prefix "Execution progress"))
  (check-type state plan-execution-state)
  (let* ((approved-indexes (or (plan-execution-state-approved-step-indexes state) '()))
         (total (length approved-indexes))
         (prefix-text (%safe-plan-execution-string prefix "Execution progress")))
    (unless (plusp total)
      (return-from plan-execution-progress-line
        (format nil "~A: step 0 of 0 (elapsed 0s)" prefix-text)))
    (let* ((current-index (plan-execution-state-current-step-index state))
           (current-position (and (integerp current-index)
                                  (position current-index approved-indexes :test #'=)))
           (completed (length (plan-execution-state-completed-step-indexes state)))
           (status (plan-execution-state-status state))
           (step-number
             (cond
               ((integerp current-position)
                (1+ current-position))
               ((eq status :completed)
                total)
               ((plusp completed)
                (min total (1+ completed)))
               (t
                1))))
      (format nil "~A: step ~D of ~D (elapsed ~A)"
              prefix-text
              step-number
              total
              (%plan-execution-format-elapsed-seconds
               (plan-execution-elapsed-seconds state))))))

(defun %append-plan-execution-progress-output (&optional (state (current-plan-execution-state)))
  (check-type state plan-execution-state)
  (plan-execution-append-output
   (format nil "LIVE> ~A"
           (plan-execution-progress-line state :prefix "Progress"))
   :phase :execution
   :style :meta
   :state state)
  state)

(defun start-plan-execution (&optional (state (current-plan-execution-state)))
  (check-type state plan-execution-state)
  (let ((transition (%dispatch-plan-execution-transition state :start)))
    (%apply-plan-execution-transition! state transition)
    state))

(defun pause-plan-execution (&optional (state (current-plan-execution-state)))
  (check-type state plan-execution-state)
  (let ((transition (%dispatch-plan-execution-transition state :pause)))
    (%apply-plan-execution-transition! state transition)
    state))

(defun resume-plan-execution (&optional (state (current-plan-execution-state)))
  (check-type state plan-execution-state)
  (let ((transition (%dispatch-plan-execution-transition state :resume)))
    (%apply-plan-execution-transition! state transition)
    state))

(defun abort-plan-execution (&key
                               (state (current-plan-execution-state))
                               reason)
  (check-type state plan-execution-state)
  (let ((transition (%dispatch-plan-execution-transition state :abort :reason reason)))
    (%apply-plan-execution-transition! state transition)
    state))

(defun plan-execution-next-step-index (&optional (state (current-plan-execution-state)))
  (check-type state plan-execution-state)
  (first (plan-execution-state-pending-step-indexes state)))

(defun %make-plan-execution-output-effect (line &key
                                                  step-index
                                                  (phase :execution)
                                                  (severity :info)
                                                  (style :plain)
                                                  recovery-actions)
  (list :kind :output
        :line line
        :step-index step-index
        :phase phase
        :severity severity
        :style style
        :recovery-actions recovery-actions))

(defun %make-plan-execution-status-effect (step-index status)
  (list :kind :status-event
        :step-index step-index
        :status status))

(defun %make-plan-execution-progress-effect ()
  (list :kind :progress-output))

(defun %apply-plan-execution-state-updates! (state updates)
  (check-type state plan-execution-state)
  (loop for (key value) on (or updates '()) by #'cddr
        do (case key
             (:status
              (setf (plan-execution-state-status state) value))
             (:started-at
              (setf (plan-execution-state-started-at state) value))
             (:finished-at
              (setf (plan-execution-state-finished-at state) value))
             (:current-step-index
              (setf (plan-execution-state-current-step-index state) value))
             (:pending-step-indexes
              (setf (plan-execution-state-pending-step-indexes state)
                    (copy-list (or value '()))))
             (:completed-step-indexes
              (setf (plan-execution-state-completed-step-indexes state)
                    (copy-list (or value '()))))
             (:failure-reason
              (setf (plan-execution-state-failure-reason state) value))
             (:abort-reason
              (setf (plan-execution-state-abort-reason state) value))
             (otherwise nil)))
  state)

(defun %apply-plan-execution-step-updates! (state step-updates)
  (check-type state plan-execution-state)
  (dolist (update (or step-updates '()) state)
    (let* ((step-index (getf update :index))
           (step (and (integerp step-index)
                      (%find-plan-execution-step state step-index))))
      (when step
        (setf (plan-execution-step-status step) (getf update :status)
              (plan-execution-step-started-at step) (getf update :started-at)
              (plan-execution-step-finished-at step) (getf update :finished-at))))))

(defun %apply-plan-execution-effect! (state effect)
  (check-type state plan-execution-state)
  (case (getf effect :kind)
    (:output
     (plan-execution-append-output
      (getf effect :line)
      :step-index (getf effect :step-index)
      :phase (or (getf effect :phase) :execution)
      :severity (or (getf effect :severity) :info)
      :style (or (getf effect :style) :plain)
      :recovery-actions (getf effect :recovery-actions)
      :state state))
    (:status-event
     (let* ((step-index (getf effect :step-index))
            (step (and (integerp step-index)
                       (%find-plan-execution-step state step-index))))
       (when step
         (%publish-plan-step-status-event state step :status (getf effect :status)))))
    (:progress-output
     (%append-plan-execution-progress-output state))
    (:initialize
     ;; Apply the bulk initialization from %pe-transition-initialize
     (let* ((run-id (getf effect :run-id))
            (ps (getf effect :plan-state))
            (steps (getf effect :steps))
            (ordered-indexes (getf effect :ordered-indexes))
            (approved-step-indexes (getf effect :approved-step-indexes)))
       (setf (plan-execution-state-run-id state) run-id
             (plan-execution-state-created-at state) (get-universal-time)
             (plan-execution-state-started-at state) nil
             (plan-execution-state-finished-at state) nil
             (plan-execution-state-source-plan-exited-at state) (plan-mode-state-exited-at ps)
             (plan-execution-state-source-plan-exit-reason state)
             (plan-mode-state-last-exit-reason ps)
             (plan-execution-state-steps state)
             (loop for step in steps
                   collect (%plan-step->execution-step
                            step
                            (member (plan-step-index step)
                                    approved-step-indexes
                                    :test #'=)))
             (plan-execution-state-ordered-step-indexes state) ordered-indexes
             (plan-execution-state-approved-step-indexes state) (copy-list approved-step-indexes)
             (plan-execution-state-pending-step-indexes state) (copy-list approved-step-indexes)
             (plan-execution-state-completed-step-indexes state) '()
             (plan-execution-state-continuity-output state) '()
             (plan-execution-state-current-step-index state) nil
             (plan-execution-state-failure-reason state) nil
             (plan-execution-state-abort-reason state) nil
             (plan-execution-state-rollback-baseline-stash state) nil
             (plan-execution-state-rollback-baseline-directory state) nil
             (plan-execution-state-rollback-attempted-p state) nil
             (plan-execution-state-rollback-succeeded-p state) nil
             (plan-execution-state-rollback-notes state) nil)
       (%publish-plan-step-status-snapshot state)
       (prime-plan-execution-continuity state)))
    (:delegate-start
     ;; Resume delegates to start — apply start transition
     (let ((transition (%dispatch-plan-execution-transition state :start)))
       (%apply-plan-execution-transition! state transition)))
    (otherwise nil))
  state)

(defun %apply-plan-execution-transition! (state transition)
  (check-type state plan-execution-state)
  (check-type transition plan-execution-transition)
  (%apply-plan-execution-state-updates! state
                                        (plan-execution-transition-state-updates transition))
  (%apply-plan-execution-step-updates! state
                                       (plan-execution-transition-step-updates transition))
  (dolist (effect (plan-execution-transition-effects transition) state)
    (%apply-plan-execution-effect! state effect)))

(defun %plan-execution-transition-step-running (state &key step now)
  (check-type state plan-execution-state)
  (check-type step plan-execution-step)
  (let* ((step-index (plan-execution-step-index step))
         (ts (get-universal-time))
         (decision-context
           (%build-plan-transition-decision-context state :step-running step))
         (context-plist (policy-decision-context-plist decision-context)))
    (make-plan-execution-transition
     :state-updates (list :current-step-index step-index)
     :step-updates (list (list :index step-index
                               :status :running
                               :started-at now
                               :finished-at nil))
     :effects (list (%make-plan-execution-status-effect step-index :running)
                    (%make-plan-execution-output-effect
                     (format nil "LIVE> [step ~D running] ~A"
                             step-index
                             (%safe-plan-execution-string
                              (plan-execution-step-description step)
                              "Executing approved step."))
                     :step-index step-index
                     :phase :execution
                     :style :meta)
                    (%make-plan-execution-progress-effect))
     :decision-context decision-context
     :structured-trace
     (list (make-policy-trace-entry
            :phase :input
            :source :plan-execution
            :data (list :step-index step-index
                        :event :step-running
                        :decision-context context-plist)
            :timestamp ts)
           (make-policy-trace-entry
            :phase :evaluate
            :source :step-transition
            :decision :running
            :data (list :step-index step-index
                        :decision-context context-plist)
            :timestamp ts)))))

(defun %plan-execution-transition-step-success (state &key step result now)
  (check-type state plan-execution-state)
  (check-type step plan-execution-step)
  (let* ((step-index (plan-execution-step-index step))
         (pending (copy-list (or (plan-execution-state-pending-step-indexes state) '())))
         (remaining (if (and pending (eql (first pending) step-index))
                        (rest pending)
                        (remove step-index pending :test #'= :count 1)))
         (completed (append (copy-list (or (plan-execution-state-completed-step-indexes state) '()))
                            (list step-index)))
         (done-p (null remaining))
         (state-updates (list :pending-step-indexes remaining
                              :completed-step-indexes completed
                              :current-step-index nil))
         (effects (list (%make-plan-execution-status-effect step-index :done)
                        (%make-plan-execution-output-effect
                         (format nil "LIVE> [step ~D done] ~A"
                                 step-index
                                 (%summarize-execution-result result))
                         :step-index step-index
                         :phase :execution
                         :style :success)))
         (ts (get-universal-time))
         (decision-context
           (%build-plan-transition-decision-context state :step-success step
                                                    :result result))
         (context-plist (policy-decision-context-plist decision-context)))
    (when done-p
      (setf state-updates (append state-updates
                                  (list :status :completed
                                        :finished-at now))
            effects (append effects
                            (list (%make-plan-execution-output-effect
                                   "LIVE> All approved steps completed."
                                   :phase :execution
                                   :style :success)))))
    (setf effects (append effects
                          (list (%make-plan-execution-progress-effect))))
    (make-plan-execution-transition
     :state-updates state-updates
     :step-updates (list (list :index step-index
                               :status :completed
                               :started-at (plan-execution-step-started-at step)
                               :finished-at now))
     :effects effects
     :done-p done-p
     :result result
     :decision-context decision-context
     :structured-trace
     (list (make-policy-trace-entry
            :phase :input
            :source :plan-execution
            :data (list :step-index step-index
                        :event :step-success
                        :decision-context context-plist)
            :timestamp ts)
           (make-policy-trace-entry
            :phase :evaluate
            :source :step-transition
            :decision :completed
            :data (list :step-index step-index
                        :done-p done-p
                        :decision-context context-plist)
            :timestamp ts)))))

(defun %plan-execution-transition-step-failure (state &key step condition now)
  (check-type state plan-execution-state)
  (check-type step plan-execution-step)
  (let* ((step-index (plan-execution-step-index step))
         (ts (get-universal-time))
         (decision-context
           (%build-plan-transition-decision-context state :step-failure step
                                                    :condition condition))
         (context-plist (policy-decision-context-plist decision-context)))
    (make-plan-execution-transition
     :state-updates (list :status :failed
                          :failure-reason condition
                          :current-step-index nil
                          :finished-at now)
     :step-updates (list (list :index step-index
                               :status :blocked
                               :started-at (or (plan-execution-step-started-at step) now)
                               :finished-at now))
     :effects (list (%make-plan-execution-status-effect step-index :blocked)
                    (%make-plan-execution-output-effect
                     (format nil
                             "LIVE> [step ~D failed] ~A. Choose next action: /execute (retry), /plan review, or /plan modify."
                             step-index
                             (%safe-plan-execution-string condition "step execution failed"))
                     :step-index step-index
                     :phase :execution
                     :severity :error
                     :style :error)
                    (%make-plan-execution-progress-effect))
     :done-p t
     :condition condition
     :decision-context decision-context
     :structured-trace
     (list (make-policy-trace-entry
            :phase :input
            :source :plan-execution
            :data (list :step-index step-index
                        :event :step-failure
                        :decision-context context-plist)
            :timestamp ts)
           (make-policy-trace-entry
            :phase :evaluate
            :source :step-transition
            :decision :failed
            :reason (when condition
                      (handler-case (princ-to-string condition)
                        (error () "step execution failed")))
            :data (list :step-index step-index
                        :decision-context context-plist)
            :timestamp ts)))))

;;; --- Complete transition table (FP-Refine Phase 1, Target 1) ---

(defun %pe-transition-reset (state &key &allow-other-keys)
  "Pure transition: reset all slots to idle defaults."
  (declare (ignore state))
  (make-plan-execution-transition
   :state-updates (list :status :idle
                        :started-at nil
                        :finished-at nil
                        :current-step-index nil
                        :failure-reason nil
                        :abort-reason nil)))

(defun %pe-transition-initialize (state &key plan-state run-id &allow-other-keys)
  "Pure transition: initialize from idle to ready with plan steps."
  (let* ((ps (or plan-state (current-plan-mode-state)))
         (steps (copy-list (or (plan-mode-state-steps ps) '())))
         (ordered-indexes (%plan-step-order steps))
         (approved-step-indexes (%approved-step-indexes-for-execution ps ordered-indexes)))
    (unless ordered-indexes
      (error "No plan steps are available for execution."))
    (unless approved-step-indexes
      (error "No approved plan steps are available for execution."))
    (make-plan-execution-transition
     :state-updates (list :status :ready)
     :effects (list (list :kind :initialize
                          :run-id (or run-id (%next-plan-execution-run-id))
                          :plan-state ps
                          :steps steps
                          :ordered-indexes ordered-indexes
                          :approved-step-indexes approved-step-indexes)))))

(defun %pe-transition-start (state &key &allow-other-keys)
  "Pure transition: move to running, set started-at if first start."
  (make-plan-execution-transition
   :state-updates (append (list :status :running
                                :finished-at nil)
                          (unless (plan-execution-state-started-at state)
                            (list :started-at (get-universal-time))))
   :effects (list (%make-plan-execution-output-effect
                   "LIVE> Execution run started."
                   :phase :execution
                   :style :meta)
                  (%make-plan-execution-progress-effect))))

(defun %pe-transition-pause (state &key &allow-other-keys)
  "Pure transition: move to paused."
  (declare (ignore state))
  (make-plan-execution-transition
   :state-updates (list :status :paused)
   :effects (list (%make-plan-execution-output-effect
                   "LIVE> Execution paused."
                   :phase :execution
                   :style :warning))))

(defun %pe-transition-resume (state &key &allow-other-keys)
  "Pure transition: resume delegates to start."
  (declare (ignore state))
  (make-plan-execution-transition
   :state-updates nil
   :effects (list (%make-plan-execution-output-effect
                   "LIVE> Execution resumed."
                   :phase :execution
                   :style :meta)
                  (list :kind :delegate-start))))

(defun %pe-transition-abort (state &key reason &allow-other-keys)
  "Pure transition: move to aborted."
  (declare (ignore state))
  (make-plan-execution-transition
   :state-updates (list :status :aborted
                        :abort-reason reason
                        :finished-at (get-universal-time))
   :effects (list (%make-plan-execution-output-effect
                   (format nil "LIVE> Execution aborted (~A)."
                           (%safe-plan-execution-string reason "unspecified"))
                   :phase :execution
                   :severity :warning
                   :style :warning))))

(defparameter +plan-execution-transition-table+
  '(;; Initialization
    ((:idle :initialize)       . %pe-transition-initialize)
    ;; Execution control
    ((:ready :start)           . %pe-transition-start)
    ((:paused :start)          . %pe-transition-start)
    ((:running :pause)         . %pe-transition-pause)
    ((:paused :resume)         . %pe-transition-resume)
    ;; Step lifecycle (existing handlers)
    ((:running :step-running)  . %plan-execution-transition-step-running)
    ((:running :step-success)  . %plan-execution-transition-step-success)
    ((:running :step-failure)  . %plan-execution-transition-step-failure)
    ;; Abort (from any non-terminal)
    ((:idle :abort)            . %pe-transition-abort)
    ((:ready :abort)           . %pe-transition-abort)
    ((:running :abort)         . %pe-transition-abort)
    ((:paused :abort)          . %pe-transition-abort)
    ;; Reset (from any)
    ((:idle :reset)            . %pe-transition-reset)
    ((:ready :reset)           . %pe-transition-reset)
    ((:running :reset)         . %pe-transition-reset)
    ((:paused :reset)          . %pe-transition-reset)
    ((:completed :reset)       . %pe-transition-reset)
    ((:failed :reset)          . %pe-transition-reset)
    ((:aborted :reset)         . %pe-transition-reset))
  "Declarative plan-execution state machine: ((from-status event) . handler-fn-name).")

(defun %dispatch-plan-execution-transition (state event &rest args)
  "Look up (status, event) in transition table, call handler, return transition."
  (check-type state plan-execution-state)
  (let* ((status (plan-execution-state-status state))
         (key (list status event))
         (entry (assoc key +plan-execution-transition-table+ :test #'equal)))
    (unless entry
      (error 'simple-error
             :format-control "Invalid plan-execution transition from ~S on event ~S."
             :format-arguments (list status event)))
    (apply (symbol-function (cdr entry)) state args)))

(defun %plan-execution-valid-transition-p (from-status event)
  "Return T if (FROM-STATUS EVENT) is a valid transition."
  (not (null (assoc (list from-status event) +plan-execution-transition-table+ :test #'equal))))

(defparameter *plan-execution-transition-handlers*
  (list (cons :step-running #'%plan-execution-transition-step-running)
        (cons :step-success #'%plan-execution-transition-step-success)
        (cons :step-failure #'%plan-execution-transition-step-failure)))

;;; NXT-137: Plan-execution effect type registry.

(defparameter *plan-execution-effect-type-registry*
  (let ((table (make-hash-table :test #'eq)))
    (setf (gethash :output table) '(:description "Append output text" :side-effect-p t)
          (gethash :status table) '(:description "Update step status indicator" :side-effect-p t)
          (gethash :progress table) '(:description "Refresh progress display" :side-effect-p t))
    table)
  "Registry mapping plan-execution effect types to metadata.")

(defun plan-execution-effect-type-registered-p (effect-type)
  "Return T if EFFECT-TYPE is registered."
  (not (null (gethash effect-type *plan-execution-effect-type-registry*))))

(defun plan-execution-effect-type-metadata (effect-type)
  "Return metadata plist for EFFECT-TYPE or NIL."
  (gethash effect-type *plan-execution-effect-type-registry*))

(defun register-plan-execution-effect-type (effect-type &key description (side-effect-p t))
  "Register a new plan-execution effect type."
  (setf (gethash effect-type *plan-execution-effect-type-registry*)
        (list :description description :side-effect-p side-effect-p))
  effect-type)

;;; NXT-138: Plan-execution transition event bus integration.

(defun publish-plan-execution-transition-event (transition &key event-bus event-type)
  "Publish TRANSITION to EVENT-BUS as a structured event if both are available."
  (when (and event-bus
             (plan-execution-transition-p transition)
             event-type)
    (let ((payload (list :state-updates (plan-execution-transition-state-updates transition)
                         :step-updates (plan-execution-transition-step-updates transition)
                         :done-p (plan-execution-transition-done-p transition)
                         :decision-context
                         (and (plan-execution-transition-decision-context transition)
                              (policy-decision-context-plist
                               (plan-execution-transition-decision-context transition)))
                         :structured-trace (plan-execution-transition-structured-trace transition))))
      (handler-case
          (publish event-bus event-type :payload payload)
        (error () nil))))
  transition)

(defun %evaluate-plan-execution-transition (state event &rest args &key &allow-other-keys)
  "Evaluate a plan-execution transition. Dispatches through the transition table
for events registered there, falls back to *plan-execution-transition-handlers*."
  (check-type state plan-execution-state)
  (let* ((status (plan-execution-state-status state))
         (table-entry (assoc (list status event) +plan-execution-transition-table+ :test #'equal)))
    (if table-entry
        (apply (symbol-function (cdr table-entry)) state args)
        ;; Fallback for backward compat with any external handler registrations
        (let ((handler (cdr (assoc event *plan-execution-transition-handlers* :test #'eq))))
          (unless handler
            (error "Unknown plan execution transition event ~S." event))
          (apply handler state args)))))

(defun execute-next-approved-plan-step (executor &key (state (current-plan-execution-state)))
  (check-type state plan-execution-state)
  (unless (functionp executor)
    (error "Plan execution step executor must be a function."))
  (let ((status (%normalize-plan-execution-status (plan-execution-state-status state))))
    (when (%plan-execution-terminal-status-p status)
      (error "Plan execution is already terminal (~S)." status))
    (when (eq :idle status)
      (error "Plan execution has not been initialized."))
    (when (eq :paused status)
      (error "Plan execution is paused. Resume it before executing steps."))
    (when (eq :ready status)
      (start-plan-execution state))
    (unless (eq :running (plan-execution-state-status state))
      (error "Plan execution cannot execute steps from status ~S."
             (plan-execution-state-status state)))
    (let ((next-step-index (plan-execution-next-step-index state)))
      (unless next-step-index
        (%complete-plan-execution-if-finished state)
        (return-from execute-next-approved-plan-step
          (values state nil nil t)))
      (let ((step (%find-plan-execution-step state next-step-index)))
        (unless step
          (error "Missing execution step for approved index ~D." next-step-index))
        (%apply-plan-execution-transition!
         state
         (%evaluate-plan-execution-transition state
                                             :step-running
                                             :step step
                                             :now (get-universal-time)))
        (handler-case
            (let ((result (funcall executor step)))
              (let ((transition
                      (%evaluate-plan-execution-transition state
                                                          :step-success
                                                          :step step
                                                          :result result
                                                          :now (get-universal-time))))
                (%apply-plan-execution-transition! state transition)
                (values state
                        step
                        result
                        (plan-execution-transition-done-p transition))))
          (error (condition)
            (%apply-plan-execution-transition!
             state
             (%evaluate-plan-execution-transition state
                                                 :step-failure
                                                 :step step
                                                 :condition condition
                                                 :now (get-universal-time)))
            (values state step condition t)))))))

(defvar *plan-step-approval-lock* (bt:make-lock "plan-step-approval-lock"))
(defvar *plan-step-approval-condvar* (bt:make-condition-variable :name "plan-step-approval-cv"))
(defvar *plan-step-awaiting-approval-p* nil
  "T when interactive plan execution is paused waiting for user approval of the next step.")
(defvar *plan-step-approved-p* nil
  "Set to T by the TUI when the user approves the next step.")

(defun approve-next-plan-step ()
  "Called by the TUI to approve the next step in interactive plan execution."
  (bt:with-lock-held (*plan-step-approval-lock*)
    (setf *plan-step-approved-p* t)
    (bt:condition-notify *plan-step-approval-condvar*)))

(defun plan-step-awaiting-approval-p ()
  "Returns T if interactive plan execution is paused for user approval."
  *plan-step-awaiting-approval-p*)

(defun %wait-for-plan-step-approval ()
  "Block until the user approves the next step."
  (bt:with-lock-held (*plan-step-approval-lock*)
    (setf *plan-step-awaiting-approval-p* t
          *plan-step-approved-p* nil)
    (loop until *plan-step-approved-p*
          do (bt:condition-wait *plan-step-approval-condvar*
                                *plan-step-approval-lock*))
    (setf *plan-step-awaiting-approval-p* nil
          *plan-step-approved-p* nil)))

(defun %plan-execution-context-state (context)
  (check-type context plan-execution-context)
  (or (plan-execution-context-state context)
      (error "Plan execution context is missing state.")))

(defun %plan-execution-context-executor (context)
  (check-type context plan-execution-context)
  (let ((executor (plan-execution-context-executor context)))
    (unless (functionp executor)
      (error "Plan execution executor must be a function."))
    executor))

(defun %prepare-plan-execution-context-rollback (context)
  (check-type context plan-execution-context)
  (let ((state (%plan-execution-context-state context)))
    (when (plan-execution-context-rollback-on-failure-p context)
      (if (%prepare-plan-execution-rollback-baseline
           state
           (plan-execution-context-rollback-directory context))
          (plan-execution-append-output
           "LIVE> Rollback baseline captured via git."
           :phase :system
           :style :meta
           :state state)
          (plan-execution-append-output
           (format nil "LIVE> Rollback baseline unavailable; proceeding without rollback (~A)."
                   (%safe-plan-execution-string
                    (plan-execution-state-rollback-notes state)
                    "no baseline"))
           :phase :system
           :severity :warning
           :style :warning
           :state state))))
  context)

(defun %plan-execution-context-await-step-approval (context first-step-p)
  (check-type context plan-execution-context)
  (unless (or (not (plan-execution-context-interactive-p context))
              first-step-p)
    (let* ((state (%plan-execution-context-state context))
           (next-idx (plan-execution-next-step-index state)))
      (when next-idx
        (plan-execution-append-output
         (format nil "LIVE> [step ~D] Waiting for approval... (press Enter in TUI)"
                 next-idx)
         :step-index next-idx
         :phase :execution
         :style :meta
         :state state)
        (%wait-for-plan-step-approval)))))

(defun %record-plan-execution-result (context step result)
  (check-type context plan-execution-context)
  (when step
    (push (cons (plan-execution-step-index step) result)
          (plan-execution-context-execution-results context)))
  context)

(defun %finish-plan-execution-success (context result done-p)
  (check-type context plan-execution-context)
  (let ((state (%plan-execution-context-state context)))
    (when done-p
      (when (typep result 'error)
        (error result))
      (%drop-plan-execution-rollback-baseline state)
      (return-from %finish-plan-execution-success
        (values state
                (nreverse (plan-execution-context-execution-results context))
                nil
                nil))))
  nil)

(defun %execute-approved-plan-steps-loop (context)
  (check-type context plan-execution-context)
  (let ((executor (%plan-execution-context-executor context))
        (state (%plan-execution-context-state context)))
    (loop
      with first-step-p = t
      do (%plan-execution-context-await-step-approval context first-step-p)
         (setf first-step-p nil)
         (multiple-value-bind (_ step result done-p)
             (execute-next-approved-plan-step executor :state state)
           (declare (ignore _))
           (%record-plan-execution-result context step result)
           (multiple-value-bind (completed-state results failure rollback)
               (%finish-plan-execution-success context result done-p)
             (when completed-state
               (return (values completed-state results failure rollback))))))))

(defun %handle-plan-execution-failure (context condition)
  (check-type context plan-execution-context)
  (let* ((state (%plan-execution-context-state context))
         (rollback-succeeded-p nil))
    (setf (plan-execution-context-failure-condition context) condition
          (plan-execution-state-status state) :failed
          (plan-execution-state-failure-reason state) (princ-to-string condition)
          (plan-execution-state-finished-at state) (get-universal-time))
    (plan-execution-append-output
     (format nil "LIVE> Execution failed: ~A"
             (%safe-plan-execution-string condition "unknown error"))
     :phase :execution
     :severity :error
     :style :error
     :step-index (plan-execution-state-current-step-index state)
     :state state)
    (when (and (plan-execution-context-rollback-on-failure-p context)
               (or (plan-execution-state-rollback-baseline-stash state)
                   (plan-execution-state-rollback-baseline-directory state)))
      (plan-execution-append-output
       "LIVE> Failure detected; attempting git rollback."
       :phase :system
       :severity :warning
       :style :warning
       :state state)
      (setf rollback-succeeded-p (%rollback-plan-execution-via-git state))
      (plan-execution-append-output
       (if rollback-succeeded-p
           "LIVE> Rollback completed; git baseline restored."
           (format nil "LIVE> Rollback failed: ~A"
                   (%safe-plan-execution-string
                    (plan-execution-state-rollback-notes state)
                    "unknown rollback failure")))
       :phase :system
       :severity (if rollback-succeeded-p :info :error)
       :style (if rollback-succeeded-p :success :error)
       :state state))
    (if (plan-execution-context-signal-failure-p context)
        (error condition)
        (values state
                (nreverse (plan-execution-context-execution-results context))
                (plan-execution-context-failure-condition context)
                rollback-succeeded-p))))

(defun execute-approved-plan-steps (executor &key
                                             (state (current-plan-execution-state))
                                             (rollback-on-failure-p t)
                                             (signal-failure-p t)
                                             (interactive-p nil)
                                             rollback-directory)
  (check-type state plan-execution-state)
  (let ((context (make-plan-execution-context
                  :executor executor
                  :state state
                  :rollback-on-failure-p rollback-on-failure-p
                  :signal-failure-p signal-failure-p
                  :interactive-p interactive-p
                  :rollback-directory rollback-directory)))
    (%plan-execution-context-executor context)
    (%prepare-plan-execution-context-rollback context)
    (handler-case
        (%execute-approved-plan-steps-loop context)
      (error (condition)
        (%handle-plan-execution-failure context condition)))))
