(in-package :amoebum)

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
