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
                  timestamp)))
  line
  step-index
  (phase :execution)
  (severity :info)
  (style :plain)
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
                      abort-reason)))
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
  abort-reason)

(defparameter *plan-execution-state* (%make-plan-execution-state))

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
                                            (state (current-plan-execution-state)))
  (check-type state plan-execution-state)
  (let* ((text (string-trim '(#\Space #\Tab #\Newline #\Return)
                            (%safe-plan-execution-string line ""))))
    (unless (plusp (length text))
      (return-from plan-execution-append-output state))
    (let* ((entry (make-plan-execution-output-entry
                   :line text
                   :step-index (and (integerp step-index) step-index)
                   :phase (%normalize-output-phase phase)
                   :severity (%normalize-output-severity severity)
                   :style (%normalize-output-style style)
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

(defun current-plan-execution-state ()
  (or *plan-execution-state*
      (setf *plan-execution-state* (%make-plan-execution-state))))

(defun reset-plan-execution-state (&optional (state (current-plan-execution-state)))
  (check-type state plan-execution-state)
  (setf (plan-execution-state-run-id state) nil
        (plan-execution-state-status state) :idle
        (plan-execution-state-created-at state) nil
        (plan-execution-state-started-at state) nil
        (plan-execution-state-finished-at state) nil
        (plan-execution-state-source-plan-exited-at state) nil
        (plan-execution-state-source-plan-exit-reason state) nil
        (plan-execution-state-steps state) '()
        (plan-execution-state-ordered-step-indexes state) '()
        (plan-execution-state-approved-step-indexes state) '()
        (plan-execution-state-pending-step-indexes state) '()
        (plan-execution-state-completed-step-indexes state) '()
        (plan-execution-state-continuity-output state) '()
        (plan-execution-state-current-step-index state) nil
        (plan-execution-state-failure-reason state) nil
        (plan-execution-state-abort-reason state) nil)
  state)

(defun initialize-plan-execution (&key
                                    (plan-state (current-plan-mode-state))
                                    (state (current-plan-execution-state))
                                    run-id)
  (check-type plan-state plan-mode-state)
  (check-type state plan-execution-state)
  (let* ((steps (copy-list (or (plan-mode-state-steps plan-state) '())))
         (ordered-indexes (%plan-step-order steps))
         (approved-step-indexes (%approved-step-indexes-for-execution plan-state
                                                                      ordered-indexes)))
    (unless ordered-indexes
      (error "No plan steps are available for execution."))
    (unless approved-step-indexes
      (error "No approved plan steps are available for execution."))
    (setf (plan-execution-state-run-id state) (or run-id (%next-plan-execution-run-id))
          (plan-execution-state-status state) :ready
          (plan-execution-state-created-at state) (get-universal-time)
          (plan-execution-state-started-at state) nil
          (plan-execution-state-finished-at state) nil
          (plan-execution-state-source-plan-exited-at state) (plan-mode-state-exited-at plan-state)
          (plan-execution-state-source-plan-exit-reason state)
          (plan-mode-state-last-exit-reason plan-state)
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
          (plan-execution-state-abort-reason state) nil)
    (prime-plan-execution-continuity state)
    state))

(defun plan-execution-ready-p (&optional (state (current-plan-execution-state)))
  (check-type state plan-execution-state)
  (eq :ready (plan-execution-state-status state)))

(defun start-plan-execution (&optional (state (current-plan-execution-state)))
  (check-type state plan-execution-state)
  (let ((status (%normalize-plan-execution-status (plan-execution-state-status state))))
    (unless (member status '(:ready :paused) :test #'eq)
      (error "Plan execution cannot start from status ~S." status))
    (when (null (plan-execution-state-started-at state))
      (setf (plan-execution-state-started-at state) (get-universal-time)))
    (setf (plan-execution-state-status state) :running
          (plan-execution-state-finished-at state) nil)
    (plan-execution-append-output
     "LIVE> Execution run started."
     :phase :execution
     :style :meta
     :state state)
    state))

(defun pause-plan-execution (&optional (state (current-plan-execution-state)))
  (check-type state plan-execution-state)
  (unless (eq :running (%normalize-plan-execution-status (plan-execution-state-status state)))
    (error "Plan execution can only be paused while running."))
  (setf (plan-execution-state-status state) :paused)
  (plan-execution-append-output
   "LIVE> Execution paused."
   :phase :execution
   :style :warning
   :state state)
  state)

(defun resume-plan-execution (&optional (state (current-plan-execution-state)))
  (check-type state plan-execution-state)
  (unless (eq :paused (%normalize-plan-execution-status (plan-execution-state-status state)))
    (error "Plan execution can only be resumed from paused state."))
  (plan-execution-append-output
   "LIVE> Execution resumed."
   :phase :execution
   :style :meta
   :state state)
  (start-plan-execution state))

(defun abort-plan-execution (&key
                               (state (current-plan-execution-state))
                               reason)
  (check-type state plan-execution-state)
  (when (%plan-execution-terminal-status-p (plan-execution-state-status state))
    (error "Plan execution is already terminal (~S)."
           (plan-execution-state-status state)))
  (setf (plan-execution-state-status state) :aborted
        (plan-execution-state-abort-reason state) reason
        (plan-execution-state-finished-at state) (get-universal-time))
  (plan-execution-append-output
   (format nil "LIVE> Execution aborted (~A)."
           (%safe-plan-execution-string reason "unspecified"))
   :phase :execution
   :severity :warning
   :style :warning
   :state state)
  state)

(defun plan-execution-next-step-index (&optional (state (current-plan-execution-state)))
  (check-type state plan-execution-state)
  (first (plan-execution-state-pending-step-indexes state)))

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
        (setf (plan-execution-state-current-step-index state) next-step-index)
        (%mark-plan-execution-step-running step)
        (plan-execution-append-output
         (format nil "LIVE> [step ~D running] ~A"
                 next-step-index
                 (%safe-plan-execution-string (plan-execution-step-description step)
                                              "Executing approved step."))
         :step-index next-step-index
         :phase :execution
         :style :meta
         :state state)
        (let ((result nil)
              (completed-p nil))
          (unwind-protect
               (progn
                 (setf result (funcall executor step)
                       completed-p t)
                 (%mark-plan-execution-step-completed step)
                 (setf (plan-execution-state-pending-step-indexes state)
                       (rest (plan-execution-state-pending-step-indexes state))
                       (plan-execution-state-completed-step-indexes state)
                       (append (plan-execution-state-completed-step-indexes state)
                               (list next-step-index))
                       (plan-execution-state-current-step-index state) nil)
                 (plan-execution-append-output
                  (format nil "LIVE> [step ~D done] ~A"
                          next-step-index
                          (%summarize-execution-result result))
                  :step-index next-step-index
                  :phase :execution
                  :style :success
                  :state state)
                 (%complete-plan-execution-if-finished state))
            (unless completed-p
              (%mark-plan-execution-step-pending step)
              (setf (plan-execution-state-current-step-index state) nil)
              (plan-execution-append-output
               (format nil "LIVE> [step ~D blocked] executor exited before completion."
                       next-step-index)
               :step-index next-step-index
               :phase :execution
               :severity :warning
               :style :warning
               :state state)))
          (values state
                  step
                  result
                  (null (plan-execution-state-pending-step-indexes state))))))))

(defun execute-approved-plan-steps (executor &key (state (current-plan-execution-state)))
  (check-type state plan-execution-state)
  (unless (functionp executor)
    (error "Plan execution executor must be a function."))
  (let ((execution-results '()))
    (loop
      (multiple-value-bind (_ step result done-p)
          (execute-next-approved-plan-step executor :state state)
        (declare (ignore _))
        (when step
          (push (cons (plan-execution-step-index step) result) execution-results))
        (when done-p
          (return (values state (nreverse execution-results))))))))
