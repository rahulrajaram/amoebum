(in-package :amoebum.commands.plan)

(defstruct (plan-command-context
            (:constructor make-plan-command-context
                (plan-state raw-args active-p)))
  plan-state
  raw-args
  active-p)

(defparameter +plan-command-usage+
  "Usage: /plan [on|off|status|review|approve|reorder|reject|modify|request-modifications|request-changes] [args...] (approve accepts step selectors like `1`, `1,3`, `2-4`; reorder accepts `3 1` or `3->1`)")

(defun %plan-review-decision-label (decision)
  (case decision
    (:approved "approved")
    (:partially-approved "partially-approved")
    (:rejected "rejected")
    (:modification-requested "modification-requested")
    (:pending "pending")
    (otherwise
     (string-downcase (symbol-name (or decision :pending))))))

(defun %format-step-index-list (step-indexes)
  (if step-indexes
      (format nil "~{~D~^, ~}" step-indexes)
      "none"))

(defun %split-delimited (text delimiter)
  (let ((parts '())
        (start 0)
        (length (length text)))
    (loop for index from 0 to length do
      (when (or (= index length)
                (char= (char text index) delimiter))
        (push (subseq text start index) parts)
        (setf start (1+ index))))
    (nreverse parts)))

(defun %digit-string-p (text)
  (and (stringp text)
       (plusp (length text))
       (loop for char across text
             always (digit-char-p char))))

(defun %parse-single-step-index (text)
  (when (%digit-string-p text)
    (let ((value (parse-integer text)))
      (when (>= value 1)
        (list value)))))

(defun %parse-step-index-range (text)
  (when (find #\- text)
    (let* ((parts (%split-delimited text #\-))
           (from-text (and (= (length parts) 2)
                           (amoebum::%slash-trim (first parts))))
           (to-text (and (= (length parts) 2)
                         (amoebum::%slash-trim (second parts)))))
      (when (and from-text
                 to-text
                 (%digit-string-p from-text)
                 (%digit-string-p to-text))
        (let ((from (parse-integer from-text))
              (to (parse-integer to-text)))
          (when (and (>= from 1)
                     (>= to from))
            (loop for value from from to to
                  collect value)))))))

(defun %parse-step-index-fragment (fragment)
  (let ((trimmed (amoebum::%slash-trim fragment)))
    (and (plusp (length trimmed))
         (or (%parse-single-step-index trimmed)
             (%parse-step-index-range trimmed)))))

(defun %parse-step-index-token (token)
  (let ((fragments (%split-delimited token #\,))
        (result '()))
    (dolist (fragment fragments)
      (let ((parsed (%parse-step-index-fragment fragment)))
        (unless parsed
          (return-from %parse-step-index-token nil))
        (setf result (append result parsed))))
    result))

(defun %step-marker-token-p (token)
  (member token '("step" "steps") :test #'string-equal))

(defun %plan-approval-parse-error (saw-step-marker-p indexes invalid-tokens)
  (cond
    ((and saw-step-marker-p invalid-tokens)
     (format nil
             "Invalid step selector(s): ~{~A~^, ~}. Use step indexes like `1`, `1,3`, or `2-4`."
             (nreverse invalid-tokens)))
    ((and saw-step-marker-p (null indexes))
     "Expected at least one step index after `step` or `steps`.")
    (t
     nil)))

(defun %collect-plan-step-approval-request (tokens)
  (let ((indexes '())
        (invalid-tokens '())
        (saw-step-marker-p nil))
    (dolist (token tokens)
      (cond
        ((%step-marker-token-p token)
         (setf saw-step-marker-p t))
        (t
         (let ((parsed (%parse-step-index-token token)))
           (if parsed
               (setf indexes (append indexes parsed))
               (push token invalid-tokens))))))
    (values indexes invalid-tokens saw-step-marker-p)))

(defun %parse-plan-step-approval-args (raw-args)
  (let ((tokens (amoebum::%tokenize-command-arguments (or raw-args ""))))
    (if (null tokens)
        (values nil nil nil)
        (multiple-value-bind (indexes invalid-tokens saw-step-marker-p)
            (%collect-plan-step-approval-request tokens)
          (let ((parse-error (%plan-approval-parse-error
                              saw-step-marker-p indexes invalid-tokens)))
            (cond
              (parse-error
               (values nil nil parse-error))
              ((and indexes (null invalid-tokens))
               (values (sort (remove-duplicates indexes :test #'=) #'<)
                       t
                       nil))
              (t
               (values nil nil nil))))))))

(defun %parse-plan-step-reorder-token (token)
  (let* ((trimmed (amoebum::%slash-trim token))
         (arrow-position (search "->" trimmed :test #'char=)))
    (cond
      ((zerop (length trimmed))
       nil)
      ((%digit-string-p trimmed)
       (list (parse-integer trimmed)))
      ((and arrow-position
            (> arrow-position 0)
            (< (+ arrow-position 2) (length trimmed)))
       (let ((from-text (amoebum::%slash-trim (subseq trimmed 0 arrow-position)))
             (to-text (amoebum::%slash-trim (subseq trimmed (+ arrow-position 2)))))
         (when (and (%digit-string-p from-text)
                    (%digit-string-p to-text))
           (list (parse-integer from-text)
                 (parse-integer to-text)))))
      (t
       nil))))

(defun %plan-reorder-ignored-token-p (token)
  (member (string-downcase (amoebum::%slash-trim token))
          '("step" "steps" "to" "into" "position")
          :test #'string=))

(defun %collect-plan-reorder-indexes (tokens)
  (let ((indexes '())
        (invalid-tokens '()))
    (dolist (token tokens)
      (unless (%plan-reorder-ignored-token-p token)
        (let ((parsed (%parse-plan-step-reorder-token token)))
          (if parsed
              (setf indexes (append indexes parsed))
              (push token invalid-tokens)))))
    (values indexes invalid-tokens)))

(defun %plan-reorder-parse-error (indexes invalid-tokens)
  (cond
    (invalid-tokens
     (format nil
             "Invalid reorder token(s): ~{~A~^, ~}. Use `/plan reorder <from> <to>`."
             (nreverse invalid-tokens)))
    ((/= (length indexes) 2)
     "Expected exactly two step indexes for reorder (from and to).")
    ((or (< (first indexes) 1)
         (< (second indexes) 1))
     "Step indexes must be positive integers.")
    (t
     nil)))

(defun %parse-plan-step-reorder-args (raw-args)
  (let ((tokens (amoebum::%tokenize-command-arguments (or raw-args ""))))
    (if (null tokens)
        (values nil
                nil
                "Expected source and target step indexes (e.g. `/plan reorder 3 1`).")
        (multiple-value-bind (indexes invalid-tokens)
            (%collect-plan-reorder-indexes tokens)
          (let ((parse-error (%plan-reorder-parse-error indexes invalid-tokens)))
            (if parse-error
                (values nil nil parse-error)
                (values (first indexes)
                        (second indexes)
                        nil)))))))

(defun %plan-input-gating-reason-label (reason)
  (case reason
    (:plan-mode-active "plan mode active")
    (:review-pending "review pending")
    (:review-not-approved "review decision not approved")
    (:awaiting-explicit-execute "awaiting explicit execute transition")
    (otherwise "open")))

(defun %plan-input-gating-summary (snapshot)
  (let ((terminal-enabled-p
          (not (null (getf snapshot :terminal-stdin-enabled-p))))
        (execution-enabled-p
          (not (null (getf snapshot :execution-pathways-enabled-p)))))
    (format nil
            " Input gating: ~:[inactive~;active~] (~A). Terminal stdin: ~:[blocked~;enabled~]. Execution pathways: ~:[blocked~;enabled~]."
            (not (null (getf snapshot :active-p)))
            (%plan-input-gating-reason-label (getf snapshot :reason))
            terminal-enabled-p
            execution-enabled-p)))

(defun %write-plan-status-active-details (stream step-count approved-count input-gating-snapshot)
  (write-string "Plan mode is ON. PLAN MODE -- read-only [LOCK mutating tools blocked]." stream)
  (when (> step-count 0)
    (format stream " Approved steps: ~D/~D." approved-count step-count))
  (write-string (%plan-input-gating-summary input-gating-snapshot) stream))

(defun %write-plan-status-inactive-details (stream output-path approved-count step-count
                                            approved-step-indexes review-pending-p
                                            review-decision review-notes input-gating-snapshot)
  (write-string "Plan mode is OFF." stream)
  (when output-path
    (format stream " Last plan output: ~A." (namestring output-path)))
  (when (> step-count 0)
    (format stream " Approved steps: ~D/~D (~A)."
            approved-count
            step-count
            (%format-step-index-list approved-step-indexes)))
  (when review-pending-p
    (write-string " Plan review pending. Use /plan review to inspect the latest captured plan."
                  stream))
  (when (and (symbolp review-decision)
             (not (eq review-decision :pending)))
    (format stream " Last review decision: ~A."
            (%plan-review-decision-label review-decision)))
  (when (and (stringp review-notes)
             (plusp (length (amoebum::%slash-trim review-notes))))
    (format stream " Review notes: ~A." (amoebum::%slash-trim review-notes)))
  (write-string (%plan-input-gating-summary input-gating-snapshot) stream))

(defun %plan-status-output (context)
  (let* ((plan-state (plan-command-context-plan-state context))
         (active-p (plan-command-context-active-p context))
         (output-path (plan-mode-state-last-output-path plan-state))
         (review-pending-p (plan-mode-state-review-pending-p plan-state))
         (review-decision (plan-mode-state-review-decision plan-state))
         (review-notes (plan-mode-state-review-notes plan-state))
         (step-count (length (plan-mode-state-steps plan-state)))
         (approved-step-indexes (plan-mode-state-approved-step-indexes plan-state))
         (approved-count (length approved-step-indexes))
         (input-gating-snapshot (plan-input-gating-snapshot plan-state)))
    (with-output-to-string (out)
      (if active-p
          (%write-plan-status-active-details out step-count approved-count input-gating-snapshot)
          (%write-plan-status-inactive-details
           out
           output-path
           approved-count
           step-count
           approved-step-indexes
           review-pending-p
           review-decision
           review-notes
           input-gating-snapshot)))))

(defun %plan-exit-output (plan-markdown output-path write-to-file-p)
  (with-output-to-string (out)
    (write-string "Plan mode disabled." out)
    (if output-path
        (format out " Plan written to ~A." (namestring output-path))
        (when write-to-file-p
          (write-string " Plan file output unavailable." out)))
    (unless write-to-file-p
      (write-string " Plan file output skipped." out))
    (when (and (stringp plan-markdown)
               (plusp (length (amoebum::%slash-trim plan-markdown))))
      (format out "~%~%Plan captured in conversation:~%~%```markdown~%~A~%```"
              plan-markdown))))

(defun %write-plan-review-metadata (stream input-gating-snapshot review-decision
                                    review-notes approved-step-indexes step-count)
  (format stream
          "Input gating: ~:[inactive~;active~] (~A), terminal stdin ~:[blocked~;enabled~], execution pathways ~:[blocked~;enabled~].~%"
          (not (null (getf input-gating-snapshot :active-p)))
          (%plan-input-gating-reason-label (getf input-gating-snapshot :reason))
          (not (null (getf input-gating-snapshot :terminal-stdin-enabled-p)))
          (not (null (getf input-gating-snapshot :execution-pathways-enabled-p))))
  (when (symbolp review-decision)
    (format stream "Current decision: ~A.~%"
            (%plan-review-decision-label review-decision)))
  (when (and (stringp review-notes)
             (plusp (length (amoebum::%slash-trim review-notes))))
    (format stream "Review notes: ~A~%" (amoebum::%slash-trim review-notes)))
  (when (> step-count 0)
    (format stream "Approved steps: ~D/~D (~A).~%"
            (length approved-step-indexes)
            step-count
            (%format-step-index-list approved-step-indexes))))

(defun %plan-review-output (context)
  (let* ((plan-state (plan-command-context-plan-state context))
         (plan-markdown (plan-mode-state-last-plan-markdown plan-state))
         (review-decision (plan-mode-state-review-decision plan-state))
         (review-notes (plan-mode-state-review-notes plan-state))
         (approved-step-indexes (plan-mode-state-approved-step-indexes plan-state))
         (step-count (length (plan-mode-state-steps plan-state)))
         (input-gating-snapshot (plan-input-gating-snapshot plan-state)))
    (if (and (stringp plan-markdown)
             (plusp (length (amoebum::%slash-trim plan-markdown))))
        (with-output-to-string (out)
          (format out "Plan review:~%")
          (%write-plan-review-metadata out
                                       input-gating-snapshot
                                       review-decision
                                       review-notes
                                       approved-step-indexes
                                       step-count)
          (format out "~%```markdown~%~A~%```" plan-markdown))
        "No captured plan is available yet. Exit plan mode first to capture one.")))

(defun %plan-invalid-usage (detail)
  (make-slash-command-result
   :output (format nil "~A~%~A" detail +plan-command-usage+)
   :echo-input-p t))

(defun %plan-captured-available-p (context)
  (let ((plan-markdown
          (plan-mode-state-last-plan-markdown
           (plan-command-context-plan-state context))))
    (and (stringp plan-markdown)
         (plusp (length (amoebum::%slash-trim plan-markdown))))))

(defun %plan-captured-unavailable-result ()
  (make-slash-command-result
   :output "No captured plan is available yet. Exit plan mode first to capture one."))

(defun %plan-available-step-indexes (context)
  (plan-step-indexes (plan-command-context-plan-state context)))

(defun %plan-approved-step-indexes (context)
  (or (plan-mode-state-approved-step-indexes
       (plan-command-context-plan-state context))
      '()))

(defun %plan-ensure-captured-for-review (context reason)
  (when (plan-command-context-active-p context)
    (multiple-value-bind (_ output-path)
        (exit-plan-mode :state (plan-command-context-plan-state context)
                        :reason reason
                        :write-output-p t)
      (declare (ignore _ output-path))
      (setconfig :plan-mode nil)
      (setf (plan-command-context-active-p context) nil)))
  (%plan-captured-available-p context))

(defun %plan-recompute-review-decision (context)
  (let* ((available-step-indexes (%plan-available-step-indexes context))
         (step-count (length available-step-indexes))
         (approved-count (length (%plan-approved-step-indexes context))))
    (set-plan-review-decision
     (cond
       ((and (> step-count 0)
             (= approved-count step-count))
        :approved)
       ((plusp approved-count)
        :partially-approved)
       (t
        :pending))
     :state (plan-command-context-plan-state context))))

(defun %plan-parse-write-to-file-arg (context)
  (let ((raw-args (plan-command-context-raw-args context)))
    (if (amoebum::%slash-blank-p raw-args)
        (values t nil)
        (handler-case
            (values (amoebum::%parse-boolean-token raw-args) nil)
          (error ()
            (values t "Expected optional write-to-file argument to be true/false for this action."))))))

(defun %plan-decision-requires-captured-plan-p (context decision)
  (or (%plan-captured-available-p context)
      (and (eq decision :approved)
           (%plan-ensure-captured-for-review context :plan-command-approved-exit))))

(defun %plan-apply-decision-approvals (plan-state context decision)
  (when (eq decision :approved)
    (set-plan-step-approvals (%plan-available-step-indexes context)
                             :state plan-state))
  (when (member decision '(:rejected :modification-requested :pending) :test #'eq)
    (clear-plan-step-approvals plan-state)))

(defun %plan-decision-output (summary raw-args)
  (with-output-to-string (out)
    (write-string summary out)
    (unless (amoebum::%slash-blank-p raw-args)
      (format out " Notes recorded: ~A." raw-args))))

(defun %plan-update-decision (context decision summary)
  (let ((raw-args (plan-command-context-raw-args context))
        (plan-state (plan-command-context-plan-state context)))
    (if (%plan-decision-requires-captured-plan-p context decision)
        (progn
          (%plan-apply-decision-approvals plan-state context decision)
          (set-plan-review-decision decision :notes raw-args :state plan-state)
          (refresh-plan-review-markdown plan-state)
          (make-slash-command-result
           :output (%plan-decision-output summary raw-args)))
        (%plan-captured-unavailable-result))))

(defun %plan-show (context)
  (if (amoebum::%slash-blank-p (plan-command-context-raw-args context))
      (let ((plan-state (plan-command-context-plan-state context)))
        (setf (plan-mode-state-review-last-presented-at plan-state)
              (get-universal-time))
        (make-slash-command-result
         :output (%plan-review-output context)))
      (%plan-invalid-usage "The /plan review action does not accept extra arguments.")))

(defun %plan-status (context)
  (if (amoebum::%slash-blank-p (plan-command-context-raw-args context))
      (make-slash-command-result
       :output (%plan-status-output context))
      (%plan-invalid-usage "The /plan status action does not accept extra arguments.")))

(defun %plan-approve-step-subset (context requested-step-indexes)
  (let* ((plan-state (plan-command-context-plan-state context))
         (available-step-indexes (%plan-available-step-indexes context))
         (missing-step-indexes
           (remove-if (lambda (index)
                        (member index available-step-indexes :test #'=))
                      requested-step-indexes)))
    (cond
      ((null available-step-indexes)
       (make-slash-command-result
        :output "No captured plan steps are available to approve."))
      (missing-step-indexes
       (make-slash-command-result
        :output (format nil
                        "Unknown plan step index(es): ~A. Available steps: ~A."
                        (%format-step-index-list missing-step-indexes)
                        (%format-step-index-list available-step-indexes))))
      (t
       (approve-plan-steps requested-step-indexes :state plan-state)
       (let* ((approved-step-indexes (%plan-approved-step-indexes context))
              (approved-count (length approved-step-indexes))
              (step-count (length available-step-indexes))
              (all-approved-p (= approved-count step-count))
              (remaining-step-indexes
                (remove-if (lambda (index)
                             (member index approved-step-indexes :test #'=))
                           available-step-indexes)))
         (set-plan-review-decision (if all-approved-p :approved :partially-approved)
                                   :state plan-state)
         (refresh-plan-review-markdown plan-state)
         (make-slash-command-result
          :output (with-output-to-string (out)
                    (format out "Approved step(s): ~A."
                            (%format-step-index-list requested-step-indexes))
                    (format out " Current approval: ~D/~D."
                            approved-count
                            step-count)
                    (unless all-approved-p
                      (format out " Remaining steps: ~A."
                              (%format-step-index-list remaining-step-indexes))))))))))

(defun %plan-approve (context)
  (multiple-value-bind (requested-step-indexes step-request-p parse-error)
      (%parse-plan-step-approval-args (plan-command-context-raw-args context))
    (cond
      (parse-error
       (%plan-invalid-usage parse-error))
      ((not step-request-p)
       (%plan-update-decision context :approved "Plan approved."))
      ((not (%plan-ensure-captured-for-review context :plan-command-approved-exit))
       (%plan-captured-unavailable-result))
      (t
       (%plan-approve-step-subset context requested-step-indexes)))))

(defun %plan-reorder (context)
  (multiple-value-bind (from-index to-index parse-error)
      (%parse-plan-step-reorder-args (plan-command-context-raw-args context))
    (cond
      (parse-error
       (%plan-invalid-usage parse-error))
      ((not (%plan-captured-available-p context))
       (%plan-captured-unavailable-result))
      (t
       (let* ((plan-state (plan-command-context-plan-state context))
              (available-step-indexes (%plan-available-step-indexes context))
              (unknown-indexes
                (remove nil
                        (list (and (not (member from-index available-step-indexes :test #'=))
                                   from-index)
                              (and (not (member to-index available-step-indexes :test #'=))
                                   to-index)))))
         (cond
           ((null available-step-indexes)
            (make-slash-command-result
             :output "No captured plan steps are available to reorder."))
           (unknown-indexes
            (make-slash-command-result
             :output (format nil
                             "Unknown plan step index(es): ~A. Available steps: ~A."
                             (%format-step-index-list unknown-indexes)
                             (%format-step-index-list available-step-indexes))))
           ((= from-index to-index)
            (make-slash-command-result
             :output (format nil
                             "Step ~D is already at position ~D. No reorder needed."
                             from-index
                             to-index)))
           (t
            (reorder-plan-step from-index to-index :state plan-state)
            (%plan-recompute-review-decision context)
            (refresh-plan-review-markdown plan-state)
            (let ((approved-step-indexes (%plan-approved-step-indexes context)))
              (make-slash-command-result
               :output (format nil
                               "Reordered step ~D to position ~D. Approved steps: ~D/~D (~A)."
                               from-index
                               to-index
                               (length approved-step-indexes)
                               (length available-step-indexes)
                               (%format-step-index-list approved-step-indexes)))))))))))

(defun %plan-reject (context)
  (%plan-update-decision context :rejected "Plan rejected."))

(defun %plan-request-modifications (context)
  (%plan-update-decision
   context
   :modification-requested
   "Plan modifications requested. Re-enter /plan on to update the draft."))

(defun %plan-enable (context)
  (if (amoebum::%slash-blank-p (plan-command-context-raw-args context))
      (if (plan-command-context-active-p context)
          (make-slash-command-result
           :output "Plan mode already enabled.")
          (let ((plan-state (plan-command-context-plan-state context)))
            (enter-plan-mode :state plan-state :clear-steps-p t)
            (setconfig :plan-mode t)
            (setf (plan-command-context-active-p context) t)
            (make-slash-command-result
             :output "Plan mode enabled. PLAN MODE -- read-only [LOCK mutating tools blocked].")))
      (%plan-invalid-usage "The /plan on action does not accept extra arguments.")))

(defun %plan-export-active-plan (context write-to-file-p reason)
  (let ((plan-state (plan-command-context-plan-state context))
        (rendered-plan (amoebum::plan-markdown
                        :state (plan-command-context-plan-state context)
                        :reason reason)))
    (multiple-value-bind (_ output-path)
        (exit-plan-mode :state plan-state
                        :reason reason
                        :write-output-p write-to-file-p)
      (declare (ignore _))
      (setconfig :plan-mode nil)
      (setf (plan-command-context-active-p context) nil)
      (make-slash-command-result
       :output (%plan-exit-output rendered-plan output-path write-to-file-p)))))

(defun %plan-export (context)
  (multiple-value-bind (write-to-file-p parse-error)
      (%plan-parse-write-to-file-arg context)
    (cond
      (parse-error
       (%plan-invalid-usage parse-error))
      ((plan-command-context-active-p context)
       (%plan-export-active-plan context write-to-file-p :plan-command-exit))
      (t
       (make-slash-command-result
        :output "Plan mode already disabled.")))))

(defun %plan-toggle (context)
  (multiple-value-bind (write-to-file-p parse-error)
      (%plan-parse-write-to-file-arg context)
    (cond
      (parse-error
       (%plan-invalid-usage parse-error))
      ((plan-command-context-active-p context)
       (%plan-export-active-plan context write-to-file-p :plan-command-toggle))
      (t
       (toggle-plan-mode :state (plan-command-context-plan-state context)
                         :reason :plan-command-toggle)
       (setconfig :plan-mode t)
       (setf (plan-command-context-active-p context) t)
       (make-slash-command-result
        :output "Plan mode enabled. PLAN MODE -- read-only [LOCK mutating tools blocked].")))))

(defparameter +plan-command-dispatch-table+
  (list (cons :status #'%plan-status)
        (cons :review #'%plan-show)
        (cons :approve #'%plan-approve)
        (cons :reorder #'%plan-reorder)
        (cons :reject #'%plan-reject)
        (cons :modify #'%plan-request-modifications)
        (cons :request-modifications #'%plan-request-modifications)
        (cons :request-changes #'%plan-request-modifications)
        (cons :on #'%plan-enable)
        (cons :off #'%plan-export)
        (cons :toggle #'%plan-toggle)))

(defun %plan-dispatch-handler (action)
  (or (cdr (assoc action +plan-command-dispatch-table+))
      #'%plan-toggle))

(defun %plan-command-handler (_invocation arguments _context)
  (declare (ignore _invocation _context))
  (let* ((plan-state (current-plan-mode-state))
         (context (make-plan-command-context
                   plan-state
                   (amoebum::%slash-trim (or (gethash :ARGS arguments) ""))
                   (plan-mode-active-p plan-state)))
         (action (or (gethash :STATE arguments) :toggle)))
    (funcall (%plan-dispatch-handler action) context)))
