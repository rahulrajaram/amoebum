(in-package :amoebum)

(defun %extract-path-argument (arguments)
  (or (%argument-value arguments "path")
      (%argument-value arguments "file")
      (%argument-value arguments "target")))

(defun %extract-command-argument (arguments)
  (or (%argument-value arguments "command")
      (%argument-value arguments "cmd")))

(defun %coerce-path-string (value)
  (typecase value
    (null nil)
    (pathname (namestring value))
    (string value)
    (symbol (symbol-name value))
    (t (princ-to-string value))))

(defun %coerce-command-string (value)
  (typecase value
    (null nil)
    (string value)
    (symbol (symbol-name value))
    (pathname (namestring value))
    (t (princ-to-string value))))

(defun %effective-event-bus (context)
  (or (context-event-bus context)
      (current-event-bus)))

(defun %context-effective-permission-mode (context)
  (%effective-permission-mode (context-permission-mode context)))

(defun %publish-permission-prompted (context tool-name arguments decision
                                     &key reason reason-code)
  (publish (%effective-event-bus context)
           (make-permission-prompted-event
            :tool-name tool-name
            :path (%coerce-path-string (%extract-path-argument arguments))
            :command (%coerce-command-string (%extract-command-argument arguments))
            :reason (or reason (format nil "permission decision ~A" decision))
            :reason-code reason-code
            :permission-mode (%context-effective-permission-mode context))))

(defun %publish-permission-blocked (context tool-name arguments decision
                                    &key reason actionable-reason reason-code)
  (let ((resolved-reason (or reason (format nil "permission decision ~A" decision))))
    (publish (%effective-event-bus context)
             (make-permission-blocked-event
              :tool-name tool-name
              :path (%coerce-path-string (%extract-path-argument arguments))
              :command (%coerce-command-string (%extract-command-argument arguments))
              :reason resolved-reason
              :actionable-reason (or actionable-reason resolved-reason)
              :reason-code reason-code
              :permission-mode (%context-effective-permission-mode context)))))

(defun %permission-evaluation-effect (evaluation tool-name arguments)
  (check-type evaluation permission-evaluation)
  (let* ((context (permission-evaluation-context evaluation))
         (decision-context (permission-evaluation-decision-context evaluation))
         (resolved-path (%coerce-path-string (%extract-path-argument arguments)))
         (resolved-command (%coerce-command-string (%extract-command-argument arguments)))
         (decision (permission-evaluation-decision evaluation))
         (reason (or (permission-evaluation-reason evaluation)
                     (format nil "permission decision ~A" decision))))
    (list :kind decision
          :decision-id (permission-check-context-decision-id context)
          :tool-name tool-name
          :path resolved-path
          :command resolved-command
          :reason reason
          :actionable-reason (or (permission-evaluation-actionable-reason evaluation)
                                 reason)
          :reason-code (permission-evaluation-reason-code evaluation)
          :decision-context (and decision-context
                                 (policy-decision-context-plist decision-context))
          :trace (permission-evaluation-trace evaluation))))

(defun %publish-tool-invoked (context tool-name arguments)
  (publish (%effective-event-bus context)
           (make-tool-invoked-event
            :tool-name tool-name
            :args arguments
            :permission-mode (%context-effective-permission-mode context)
            :request-id *pipeline-current-request-id*)))

(defun %publish-tool-error (context tool-error elapsed-ms)
  (publish (%effective-event-bus context)
           (make-tool-error-event
            :tool-name *pipeline-current-tool-name*
            :args *pipeline-current-arguments*
            :condition-reason-code (tool-error-reason-code tool-error)
            :condition (princ-to-string tool-error)
            :elapsed-ms elapsed-ms
            :request-id *pipeline-current-request-id*)))

(defun %signal-pipeline-tool-error (context condition timeout-seconds)
  (let* ((tool-error
           (%coerce-tool-error *pipeline-current-tool-name*
                               *pipeline-current-arguments*
                               condition
                               timeout-seconds))
         (elapsed-ms (%elapsed-milliseconds)))
    (usdt-probe-tool-exit *pipeline-current-tool-name*
                          *pipeline-current-request-id*
                          elapsed-ms
                          :status :error)
    (ignore-errors
      (note-tool-profiling-sample *pipeline-current-tool-name* elapsed-ms))
    (%record-tool-metrics context *pipeline-current-tool-name* elapsed-ms :error)
    (%publish-tool-error context tool-error elapsed-ms)
    (error tool-error)))

(defun %post-tool-success (context)
  "Record metrics, cache result, run post-hooks, and publish completed event.
Called from :around after *pipeline-current-result* is set, because CLOS
:after methods run before :around can assign the result variable."
  (let* ((tool-name *pipeline-current-tool-name*)
         (arguments *pipeline-current-arguments*)
         (elapsed-ms (%elapsed-milliseconds)))
    (usdt-probe-tool-exit tool-name
                          *pipeline-current-request-id*
                          elapsed-ms
                          :status :ok)
    (ignore-errors
      (note-tool-profiling-sample tool-name elapsed-ms))
    (%record-tool-metrics context tool-name elapsed-ms :ok)
    (%cache-tool-result context tool-name arguments *pipeline-current-result*)
    (%run-hook-dispatch context :post-tool-use tool-name *pipeline-current-result* elapsed-ms)
    (publish (%effective-event-bus context)
             (make-tool-completed-event
              :tool-name tool-name
              :args arguments
              :result *pipeline-current-result*
              :elapsed-ms elapsed-ms
              :request-id *pipeline-current-request-id*))))
