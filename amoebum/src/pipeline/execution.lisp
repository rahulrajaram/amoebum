(in-package :amoebum)

(defun %check-permission-or-signal (tool-name arguments context)
  (let* ((path-arg (%coerce-path-string
                    (%extract-path-argument arguments)))
         (command-arg (%coerce-command-string
                       (%extract-command-argument arguments)))
         (effective-mode (%context-effective-permission-mode context))
         (metadata (find-tool-metadata tool-name))
         (dangerous-p (and metadata (tool-metadata-dangerous-p metadata)))
         (evaluation
           (evaluate-permission-decision
            :tool tool-name
            :path path-arg
            :command command-arg
            :dangerous-p dangerous-p
            :permission-mode effective-mode))
         (_materialized
           (%materialize-permission-evaluation! evaluation :record-history-p t))
         (effect (%permission-evaluation-effect evaluation tool-name arguments)))
    (declare (ignore _materialized))
    (cond
      ((eq (getf effect :kind) :allow) nil)
      ((eq (getf effect :kind) :prompt)
       ;; Publish the event so the TUI can show the approval dialog,
       ;; then block until the user resolves it.
       (%publish-permission-prompted context
                                     tool-name
                                     arguments
                                     (getf effect :kind)
                                     :reason (getf effect :reason)
                                     :reason-code (getf effect :reason-code))
       (let* ((pa (wait-for-pending-approval
                   tool-name arguments
                   :path (getf effect :path)
                   :command (getf effect :command)
                   :reason (getf effect :reason)
                   :decision-id (getf effect :decision-id)
                   :cancel-thunk (context-permission-cancel-thunk context)))
              (user-decision (pending-approval-decision pa))
              (decision-source (pending-approval-decision-source pa))
              (decision-reason-text
                (case decision-source
                  (:timeout "approval request timed out")
                  (:cancelled "approval was cancelled")
                  (:ui-error "approval dialog failed")
                  (:noninteractive "approval UI was inactive in non-interactive mode")
                  (otherwise "denied by user"))))
         ;; Track exact path approvals in session memory, and persist only
         ;; explicit "always allow" decisions.
         (when (and (eq user-decision :allow)
                    (stringp path-arg)
                    (plusp (length path-arg)))
           (remember-path-approval
            :tool tool-name
            :path path-arg
            :scope (if (pending-approval-remember-p pa) :always :session)
            :persist-p (pending-approval-remember-p pa)))
         ;; Retain deny memory and non-path "always allow" behavior in rules.
         (when (pending-approval-remember-p pa)
           (when (or (eq user-decision :deny)
                     (or (null path-arg)
                         (zerop (length path-arg))))
             (add-permission-rule :effect user-decision
                                  :tool tool-name
                                  :source :user-approval)))
         (unless (eq user-decision :allow)
           (error 'tool-permission-denied
                  :tool-name tool-name
                  :arguments arguments
                  :reason-code (getf effect :reason-code)
                  :message (format nil "Tool ~S denied: ~A."
                                   tool-name
                                   decision-reason-text)
                  :reason decision-reason-text))))
      (t
       ;; :deny or any other non-allow decision
       (when (eq (getf effect :kind) :deny)
         (%publish-permission-blocked context
                                      tool-name
                                      arguments
                                      (getf effect :kind)
                                      :reason (getf effect :reason)
                                      :actionable-reason (getf effect :actionable-reason)
                                      :reason-code (getf effect :reason-code)))
       (error 'tool-permission-denied
              :tool-name tool-name
              :arguments arguments
              :reason-code (getf effect :reason-code)
              :message (format nil "Permission decision ~A for tool ~S: ~A."
                               (getf effect :kind)
                               tool-name
                               (getf effect :actionable-reason))
              :reason (getf effect :actionable-reason))))))

(defun %coerce-tool-error (tool-name arguments condition timeout-seconds)
  (cond
    ((typep condition 'tool-error)
     condition)
    #+sbcl
    ((typep condition 'sb-ext:timeout)
     (make-condition 'tool-timeout
                     :tool-name tool-name
                     :arguments arguments
                     :timeout-seconds timeout-seconds
                     :message (format nil "Tool ~S timed out." tool-name)
                     :reason (format nil "timed out after ~A seconds" timeout-seconds)
                     :cause condition))
    (t
     (make-condition 'tool-execution-error
                     :tool-name tool-name
                     :arguments arguments
                     :message (princ-to-string condition)
                     :reason (princ-to-string condition)
                     :cause condition))))

(defmethod pseudopod:execute-tool :around ((call pseudopod:tool-call)
                                           (context tool-execution-context))
  (let* ((*pipeline-current-tool-name* (%tool-call-name-string call))
         (*pipeline-current-arguments* (%decode-tool-call-arguments call))
         (*pipeline-current-request-id* (%tool-call-request-id call))
         (*pipeline-start-time-ms* nil)
         (*pipeline-current-result* nil)
         (timeout-seconds (%metadata-timeout-seconds *pipeline-current-tool-name*)))
    (restart-case
        (handler-bind
            ((tool-missing-argument
               (lambda (condition)
                 (let ((recovery
                         (%missing-tool-argument-recovery
                          condition
                          *pipeline-current-arguments*
                          context)))
                   (when recovery
                     (case (first recovery)
                       (:retry
                        (invoke-restart 'retry-tool (second recovery)))
                       (:use-value
                        (invoke-restart 'use-value (second recovery)))))))))
          (handler-case
              (let* ((raw-result
                       #+sbcl
                       (if (and timeout-seconds (> timeout-seconds 0))
                           (sb-ext:with-timeout timeout-seconds
                             (call-next-method))
                           (call-next-method))
                       #-sbcl
                       (call-next-method))
                     (guarded-result (apply-sandbox-output-guard raw-result)))
                (setf *pipeline-current-result* guarded-result)
                (%post-tool-success context)
                guarded-result)
            #+sbcl
            (sb-ext:timeout (condition)
              (%signal-pipeline-tool-error context condition timeout-seconds))
            (error (condition)
              (%signal-pipeline-tool-error context condition timeout-seconds))))
      (retry-tool (&optional (new-arguments *pipeline-current-arguments*))
        :report "Retry tool execution."
        (pseudopod:execute-tool (%clone-tool-call-with-arguments call new-arguments)
                                context))
      (skip-tool ()
        :report "Skip this tool and continue."
        (format nil "Tool ~A skipped by pipeline restart." *pipeline-current-tool-name*))
      (use-value (value)
        :report "Provide a replacement value."
        value)
      (abort-tool ()
        :report "Abort tool execution and propagate failure."
        (error 'amoebum-error
               :message (format nil "Tool ~A aborted by pipeline restart."
                                *pipeline-current-tool-name*))))))

(defmethod pseudopod:execute-tool :before ((call pseudopod:tool-call)
                                           (context tool-execution-context))
  (let* ((tool-name *pipeline-current-tool-name*)
         (arguments (%call-arguments call))
         (effective-mode (%context-effective-permission-mode context)))
    (%ensure-tool-registered context tool-name arguments)
    (%validate-tool-arguments tool-name arguments)
    (%check-permission-or-signal tool-name arguments context)
    (sandbox-check-tool-call tool-name arguments
                             :permission-mode effective-mode)
    (%maybe-log-invocation context tool-name arguments)
    (multiple-value-bind (decision details)
        (%run-hook-dispatch context :pre-tool-use tool-name arguments)
      (declare (ignore details))
      (when (eq decision :deny)
        (error 'tool-permission-denied
               :tool-name tool-name
               :arguments arguments
               :message (format nil "Tool ~S blocked by pre-tool-use hook."
                                tool-name)
               :reason "blocked by pre-tool-use hook")))
    (setf *pipeline-start-time-ms* (monotonic-ms))
    (usdt-probe-tool-enter tool-name *pipeline-current-request-id*)
    (%publish-tool-invoked context tool-name arguments)))

(defmethod pseudopod:execute-tool :after ((call pseudopod:tool-call)
                                          (context tool-execution-context))
  ;; Post-success work (metrics, cache, hooks, events) moved to
  ;; %post-tool-success called from :around, because CLOS :after runs
  ;; inside call-next-method before :around can set *pipeline-current-result*.
  (declare (ignore call context))
  (when (and (plan-mode-active-p)
             (plan-mode-exploration-tool-p *pipeline-current-tool-name*))
    (record-plan-mode-exploration *pipeline-current-tool-name* :successful-p t))
  (values))
