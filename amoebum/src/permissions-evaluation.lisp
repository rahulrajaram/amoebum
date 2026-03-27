(in-package :amoebum)

(defstruct (permission-check-context
            (:constructor make-permission-check-context
                (&key tool path command dangerous-p rules record-history-p tool-name mode
                      project-root normalized-path request-path canonical-command
                      argument-profile command-cache-key policy-command-text mcp-decision
                      plan-mode-blocked-p path-traversal-attempt-p
                      outside-project-root-p decision-id)))
  tool
  path
  command
  dangerous-p
  rules
  record-history-p
  tool-name
  mode
  project-root
  normalized-path
  request-path
  canonical-command
  argument-profile
  command-cache-key
  policy-command-text
  mcp-decision
  plan-mode-blocked-p
  path-traversal-attempt-p
  outside-project-root-p
  decision-id
  path-decision
  path-trace
  command-decision
  command-trace
  decision
  decision-source
  decision-reason
  actionable-reason
  decision-reason-code
  path-memory-checked-p
  path-memory-allowed-p)

(defstruct (permission-evaluation
            (:constructor make-permission-evaluation
                (&key decision
                      pre-escalation-decision
                      decision-source
                      reason-code
                      reason
                      actionable-reason
                      trace
                      (structured-trace '())
                      (dangerous-reasons '())
                      (dangerous-escalation-p nil)
                      decision-context
                      context)))
  decision
  pre-escalation-decision
  decision-source
  reason-code
  reason
  actionable-reason
  trace
  (structured-trace '() :type list)
  (dangerous-reasons '() :type list)
  (dangerous-escalation-p nil :type boolean)
  decision-context
  context)

(defun %build-permission-check-context (&key tool path command dangerous-p permission-mode
                                          approval-policy rules record-history-p)
  (let* ((tool-name (%tool-name tool))
         (mode (%effective-permission-mode permission-mode approval-policy))
         (project-root (%project-root-path))
         (normalized-path (or (%resolve-path-against-project-root path)
                              (%normalize-path path)))
         (request-path (or (%resolve-path-against-project-root path :resolve-symlinks-p nil)
                           (%normalize-request-path path)))
         (canonical-command (canonicalize-permission-command command))
         (policy-command-text (%policy-command-text canonical-command command))
         (explicit-plan-mode-p (eq mode :plan))
         (mcp-server-name (%mcp-tool-server-name tool-name)))
    (make-permission-check-context
     :tool tool
     :path path
     :command command
     :dangerous-p dangerous-p
     :rules rules
     :record-history-p record-history-p
     :tool-name tool-name
     :mode mode
     :project-root project-root
     :normalized-path normalized-path
     :request-path request-path
     :canonical-command canonical-command
     :argument-profile (%command-argument-profile-from-canonical canonical-command)
     :command-cache-key (%permission-command-cache-key tool canonical-command)
     :policy-command-text policy-command-text
     :mcp-decision (and mcp-server-name
                        (or (%mcp-server-config-decision mcp-server-name)
                            :prompt))
     :plan-mode-blocked-p (%plan-mode-blocked-p tool
                                                policy-command-text
                                                explicit-plan-mode-p)
     :path-traversal-attempt-p (%path-traversal-attempt-p path)
     :outside-project-root-p
     (or (%path-outside-project-root-p request-path project-root)
         (%path-outside-project-root-p normalized-path project-root))
     :decision-id (%next-permission-decision-id))))

(defun %populate-permission-check-evaluations (context)
  (multiple-value-bind (path-decision path-trace)
      (if (permission-check-context-normalized-path context)
          (evaluate-path-permission :tool (permission-check-context-tool context)
                                    :path (permission-check-context-normalized-path context)
                                    :rules (permission-check-context-rules context)
                                    :with-trace-p t)
          (values nil nil))
    (setf (permission-check-context-path-decision context) path-decision
          (permission-check-context-path-trace context) path-trace))
  (multiple-value-bind (command-decision command-trace)
      (if (permission-check-context-policy-command-text context)
          (evaluate-command-permission
           :tool (permission-check-context-tool context)
           :path (permission-check-context-normalized-path context)
           :command (permission-check-context-policy-command-text context)
           :canonical-command (permission-check-context-canonical-command context)
           :rules (permission-check-context-rules context)
           :with-trace-p t)
          (values nil nil))
    (setf (permission-check-context-command-decision context) command-decision
          (permission-check-context-command-trace context) command-trace))
  context)

(defun %permission-policy-context-data (context)
  (check-type context permission-check-context)
  (list :project-root (permission-check-context-project-root context)
        :request-path (permission-check-context-request-path context)
        :normalized-path (permission-check-context-normalized-path context)
        :policy-command-text (permission-check-context-policy-command-text context)
        :path-traversal-attempt-p (permission-check-context-path-traversal-attempt-p context)
        :outside-project-root-p (permission-check-context-outside-project-root-p context)
        :plan-mode-blocked-p (permission-check-context-plan-mode-blocked-p context)
        :mcp-decision (permission-check-context-mcp-decision context)
        :argument-profile (copy-tree (permission-check-context-argument-profile context))
        :command-cache-key (permission-check-context-command-cache-key context)
        :rule-count (length (or (permission-check-context-rules context) '()))))

(defun %build-permission-policy-decision-context (context)
  (check-type context permission-check-context)
  (build-policy-decision-context
   :kind :permission-check
   :tool-name (permission-check-context-tool-name context)
   :path (permission-check-context-normalized-path context)
   :command (permission-check-context-policy-command-text context)
   :permission-mode (permission-check-context-mode context)
   :dangerous-p (permission-check-context-dangerous-p context)
   :rules (permission-check-context-rules context)
   :context-data (%permission-policy-context-data context)
   :decision-id (permission-check-context-decision-id context)))

(defun %permission-check-path-memory-allowed-p (context)
  (unless (permission-check-context-path-memory-checked-p context)
    (setf (permission-check-context-path-memory-allowed-p context)
          (and (permission-check-context-path context)
               (%path-memory-allows-p (permission-check-context-tool context)
                                      (permission-check-context-path context)))
          (permission-check-context-path-memory-checked-p context) t))
  (permission-check-context-path-memory-allowed-p context))

;;; --- Permission Decision Dispatch Tables (FP-Refine Phase 2, Target 4) ---

(defun %permission-context-plan-mode-blocked-p (context)
  "Predicate: is the request blocked by plan mode?"
  (permission-check-context-plan-mode-blocked-p context))

(defun %permission-context-rule-denies-p (context)
  "Predicate: does any path or command rule deny the request?"
  (or (eq (permission-check-context-path-decision context) :deny)
      (eq (permission-check-context-command-decision context) :deny)))

(defun %permission-context-plan-readonly-allows-p (context)
  "Predicate: is the tool allowed by plan-mode readonly list?"
  (%plan-mode-readonly-allowed-p (permission-check-context-tool context)))

(defun %permission-context-command-allows-p (context)
  "Predicate: does the command rule explicitly allow?"
  (eq (permission-check-context-command-decision context) :allow))

(defun %permission-context-path-allows-p (context)
  "Predicate: does the path rule explicitly allow?"
  (eq (permission-check-context-path-decision context) :allow))

(defun %permission-context-has-mcp-decision-p (context)
  "Predicate: is there a non-nil MCP decision?"
  (not (null (permission-check-context-mcp-decision context))))

(defun %permission-context-mcp-decision-value (context)
  "Return the MCP decision value from context."
  (permission-check-context-mcp-decision context))

(defun %permission-context-mode-default-value (context)
  "Return the mode default decision for this context."
  (%mode-default-decision (permission-check-context-mode context)
                          (permission-check-context-tool context)
                          (permission-check-context-normalized-path context)
                          (permission-check-context-policy-command-text context)))

(defun %always-true (_context)
  "Predicate that always returns T. Used as catch-all in rule tables."
  (declare (ignore _context))
  t)

(defparameter +permission-base-decision-rules+
  '((:plan-mode-blocked  %permission-context-plan-mode-blocked-p     :deny)
    (:rule-deny          %permission-context-rule-denies-p           :deny)
    (:plan-mode-readonly %permission-context-plan-readonly-allows-p  :allow)
    (:path-memory-allows %permission-check-path-memory-allowed-p     :allow)
    (:command-allows     %permission-context-command-allows-p        :allow)
    (:path-allows        %permission-context-path-allows-p          :allow)
    (:mcp-decision       %permission-context-has-mcp-decision-p     %permission-context-mcp-decision-value)
    (:mode-default       %always-true                               %permission-context-mode-default-value))
  "Priority-ordered rule table for permission base decision.
Each entry: (label predicate-symbol decision-or-function-symbol).
When predicate returns non-nil, decision is either a keyword or a function called on context.")

(defun %evaluate-permission-decision-rules (context rules)
  "Evaluate RULES against CONTEXT, returning the first matching decision.
Each rule is (label predicate decision). When predicate(context) is true:
  - if decision is a keyword, return it directly
  - if decision is a symbol naming a function, call it on context."
  (loop for (label pred decision) in rules
        when (funcall pred context)
          return (if (keywordp decision)
                     decision
                     (funcall decision context))))

(defun %permission-check-base-decision (context)
  (%evaluate-permission-decision-rules context +permission-base-decision-rules+))

(defun %permission-check-apply-project-root-guard (context)
  (when (and (permission-check-context-outside-project-root-p context)
             (not (permission-check-context-plan-mode-blocked-p context))
             (not (eq (permission-check-context-path-decision context) :deny))
             (not (eq (permission-check-context-command-decision context) :deny))
             (not (eq (permission-check-context-path-decision context) :allow))
             (not (%permission-check-path-memory-allowed-p context)))
    (setf (permission-check-context-decision context) :deny
          (permission-check-context-decision-source context) :project-root-guard
          (permission-check-context-decision-reason-code context)
          :path-traversal-outside-project-root
          (permission-check-context-decision-reason context)
          (format nil "Resolved path ~A escapes project root ~A."
                  (or (permission-check-context-normalized-path context)
                      (permission-check-context-request-path context)
                      (%path-string (permission-check-context-path context)))
                  (permission-check-context-project-root context))))
  context)

(defun %permission-check-apply-plan-mode-block (context)
  (when (permission-check-context-plan-mode-blocked-p context)
    (setf (permission-check-context-decision context) :deny
          (permission-check-context-decision-source context) :plan-mode
          (permission-check-context-decision-reason-code context)
          :plan-mode-mutating-command-blocked
          (permission-check-context-decision-reason context)
          (%plan-mode-block-reason (permission-check-context-tool-name context)
                                   (permission-check-context-policy-command-text context))
          (permission-check-context-actionable-reason context)
          (%plan-mode-actionable-reason)))
  context)

(defun %permission-check-append-command-trace (context)
  (when *last-command-canonicalization-trace*
    (setf *last-command-canonicalization-trace*
          (append *last-command-canonicalization-trace*
                  (list :command-cache-key
                        (permission-check-context-command-cache-key context)))))
  context)

(defun %permission-check-trace (context final-decision dangerous-reasons dangerous-escalation-p)
  (list :decision-id (permission-check-context-decision-id context)
        :timestamp (get-universal-time)
        :tool (permission-check-context-tool-name context)
        :path (permission-check-context-normalized-path context)
        :command (permission-check-context-policy-command-text context)
        :command-argument-profile (permission-check-context-argument-profile context)
        :permission-mode (permission-check-context-mode context)
        :decision final-decision
        :pre-escalation-decision (permission-check-context-decision context)
        :decision-source (permission-check-context-decision-source context)
        :reason-code (permission-check-context-decision-reason-code context)
        :reason (permission-check-context-decision-reason context)
        :actionable-reason (permission-check-context-actionable-reason context)
        :dangerous-escalation-p dangerous-escalation-p
        :dangerous-reason-codes dangerous-reasons
        :path-decision (permission-check-context-path-decision context)
        :command-decision (permission-check-context-command-decision context)
        :mcp-decision (permission-check-context-mcp-decision context)
        :project-root (permission-check-context-project-root context)
        :request-path (permission-check-context-request-path context)
        :outside-project-root-p (permission-check-context-outside-project-root-p context)
        :path-traversal-attempt-p (permission-check-context-path-traversal-attempt-p context)
        :evaluation-trace (remove nil
                                  (list (permission-check-context-path-trace context)
                                        (permission-check-context-command-trace context)))))

(defun %permission-evaluation-structured-trace (context final-decision dangerous-reasons
                                                decision-context)
  "Build a list of policy-trace-entry structs from CONTEXT evaluation phases."
  (let ((now (get-universal-time))
        (entries '())
        (context-plist (and decision-context
                            (policy-decision-context-plist decision-context))))
    (push (make-policy-trace-entry
           :phase :input
           :source :permission-check
           :decision nil
           :reason-code nil
           :reason nil
           :data (list :tool-name (permission-check-context-tool-name context)
                       :path (permission-check-context-path context)
                       :command (permission-check-context-command context)
                       :mode (permission-check-context-mode context)
                       :dangerous-p (permission-check-context-dangerous-p context)
                       :decision-context context-plist)
           :timestamp now)
          entries)
    (push (make-policy-trace-entry
           :phase :evaluate
           :source (permission-check-context-decision-source context)
           :decision (permission-check-context-decision context)
           :reason-code (permission-check-context-decision-reason-code context)
           :reason (permission-check-context-decision-reason context)
           :data (list :path-decision (permission-check-context-path-decision context)
                       :command-decision (permission-check-context-command-decision context)
                       :decision-context context-plist)
           :timestamp now)
          entries)
    (when (or dangerous-reasons
              (not (eq final-decision (permission-check-context-decision context))))
      (push (make-policy-trace-entry
             :phase :escalate
             :source :dangerous-command-escalation
             :decision final-decision
             :reason-code (when dangerous-reasons :dangerous-escalation)
             :reason (when dangerous-reasons "Escalated to :prompt due to dangerous command")
             :data (list :dangerous-reasons dangerous-reasons
                         :decision-context context-plist)
             :timestamp now)
            entries))
    (push (make-policy-trace-entry
           :phase :materialize
           :source :final
           :decision final-decision
           :reason-code (permission-check-context-decision-reason-code context)
           :reason (permission-check-context-decision-reason context)
           :data (when context-plist
                   (list :decision-context context-plist))
           :timestamp now)
          entries)
    (nreverse entries)))

;;; NXT-136: Permission decision replay from structured trace.

(defun replay-permission-decision-from-trace (structured-trace)
  "Replay a permission decision from STRUCTURED-TRACE entries, returning
a summary plist of the evaluation phases without performing side effects."
  (let ((input-entry nil)
        (evaluate-entry nil)
        (escalate-entry nil)
        (materialize-entry nil))
    (dolist (entry structured-trace)
      (when (policy-trace-entry-p entry)
        (case (policy-trace-entry-phase entry)
          (:input (setf input-entry entry))
          (:evaluate (setf evaluate-entry entry))
          (:escalate (setf escalate-entry entry))
          (:materialize (setf materialize-entry entry)))))
    (list :input-data (and input-entry (policy-trace-entry-data input-entry))
          :evaluated-decision (and evaluate-entry (policy-trace-entry-decision evaluate-entry))
          :evaluated-source (and evaluate-entry (policy-trace-entry-source evaluate-entry))
          :evaluated-reason-code (and evaluate-entry (policy-trace-entry-reason-code evaluate-entry))
          :escalated-p (not (null escalate-entry))
          :escalated-decision (and escalate-entry (policy-trace-entry-decision escalate-entry))
          :final-decision (and materialize-entry (policy-trace-entry-decision materialize-entry)))))

;;; NXT-139: Policy decision auditor (trace to human-readable report).

(defun format-policy-trace-report (structured-trace &key (stream nil))
  "Format STRUCTURED-TRACE entries into a human-readable multi-line report.
When STREAM is nil, returns a string; otherwise writes to STREAM."
  (flet ((fmt (destination control &rest args)
           (apply #'format destination control args)))
    (let ((output (or stream (make-string-output-stream))))
      (fmt output "Policy Decision Report~%")
      (fmt output "======================~%")
      (dolist (entry structured-trace)
        (when (policy-trace-entry-p entry)
          (fmt output "~%Phase: ~A~%" (policy-trace-entry-phase entry))
          (when (policy-trace-entry-source entry)
            (fmt output "  Source: ~A~%" (policy-trace-entry-source entry)))
          (when (policy-trace-entry-decision entry)
            (fmt output "  Decision: ~A~%" (policy-trace-entry-decision entry)))
          (when (policy-trace-entry-reason-code entry)
            (fmt output "  Reason-Code: ~A~%" (policy-trace-entry-reason-code entry)))
          (when (policy-trace-entry-reason entry)
            (fmt output "  Reason: ~A~%" (policy-trace-entry-reason entry)))
          (when (policy-trace-entry-data entry)
            (fmt output "  Data: ~S~%" (policy-trace-entry-data entry)))))
      (unless stream
        (get-output-stream-string output)))))

;;; NXT-140: Permission decision diff between sessions.

(defun diff-permission-decisions (trace-a trace-b)
  "Compare two structured permission traces and return a diff plist.
TRACE-A and TRACE-B are lists of policy-trace-entry structs."
  (let ((replay-a (replay-permission-decision-from-trace trace-a))
        (replay-b (replay-permission-decision-from-trace trace-b)))
    (let ((changes '()))
      (flet ((check-field (key)
               (let ((val-a (getf replay-a key))
                     (val-b (getf replay-b key)))
                 (unless (equal val-a val-b)
                   (push (list :field key :before val-a :after val-b) changes)))))
        (check-field :evaluated-decision)
        (check-field :evaluated-source)
        (check-field :evaluated-reason-code)
        (check-field :escalated-p)
        (check-field :final-decision))
      (list :same-p (null changes)
            :changes (nreverse changes)
            :decision-a (getf replay-a :final-decision)
            :decision-b (getf replay-b :final-decision)))))

(defun %build-permission-evaluation (context)
  (let ((dangerous-reasons
          (or (and (permission-check-context-dangerous-p context)
                   '(:explicit-dangerous-flag))
              (and (permission-check-context-canonical-command context)
                   (%command-danger-reason-codes
                    (permission-check-context-canonical-command context))))))
    (setf (permission-check-context-decision context)
          (%permission-check-base-decision context))
    (%permission-check-apply-plan-mode-block context)
    (%permission-check-apply-project-root-guard context)
    (%permission-check-append-command-trace context)
    (let* ((dangerous-escalation-p
             (and (eq (permission-check-context-decision context) :allow)
                  (not (eq (permission-check-context-mode context) :yolo))
                  (not (null dangerous-reasons))))
           (final-decision
             (if dangerous-escalation-p
                 :prompt
                 (permission-check-context-decision context)))
           (decision-context
             (%build-permission-policy-decision-context context))
           (trace (%permission-check-trace context
                                           final-decision
                                           dangerous-reasons
                                           dangerous-escalation-p))
           (structured-trace
             (%permission-evaluation-structured-trace
              context final-decision dangerous-reasons decision-context)))
      (make-permission-evaluation
       :decision final-decision
       :pre-escalation-decision (permission-check-context-decision context)
       :decision-source (permission-check-context-decision-source context)
       :reason-code (permission-check-context-decision-reason-code context)
       :reason (permission-check-context-decision-reason context)
       :actionable-reason (permission-check-context-actionable-reason context)
       :trace trace
       :structured-trace structured-trace
       :dangerous-reasons dangerous-reasons
       :dangerous-escalation-p dangerous-escalation-p
       :decision-context decision-context
       :context context))))

(defun %materialize-permission-evaluation! (evaluation &key (record-history-p nil))
  (check-type evaluation permission-evaluation)
  (let* ((context (permission-evaluation-context evaluation))
         (trace (permission-evaluation-trace evaluation))
         (path-identity-snapshot
           (%record-permission-path-identity-check
            (permission-check-context-tool-name context)
            (permission-check-context-path context)
            (permission-evaluation-decision evaluation)
            (permission-check-context-decision-id context))))
    (when path-identity-snapshot
      (setf trace (append trace
                          (list :path-identity path-identity-snapshot))
            (permission-evaluation-trace evaluation) trace))
    (setf *last-permission-decision-trace* trace)
    (when record-history-p
      (%record-permission-decision trace))
    evaluation))

(defun evaluate-permission-decision (&key tool path command dangerous-p permission-mode
                                       approval-policy
                                       (rules *permission-rules*))
  (let ((context (%populate-permission-check-evaluations
                  (%build-permission-check-context
                   :tool tool
                   :path path
                   :command command
                   :dangerous-p dangerous-p
                   :permission-mode permission-mode
                   :approval-policy approval-policy
                   :rules rules
                   :record-history-p nil))))
    (%build-permission-evaluation context)))

(defun check-permission (&key tool path command dangerous-p permission-mode approval-policy
                           (rules *permission-rules*)
                           (record-history-p t))
  (let ((evaluation
          (evaluate-permission-decision
           :tool tool
           :path path
           :command command
           :dangerous-p dangerous-p
           :permission-mode permission-mode
           :approval-policy approval-policy
           :rules rules)))
    (%materialize-permission-evaluation! evaluation
                                         :record-history-p record-history-p)
    (permission-evaluation-decision evaluation)))

(defun explain-permission-decision (&key decision-id (rules *permission-rules*))
  (let* ((historical
           (cond
             ((null decision-id) (first *permission-decision-history*))
             ((string-equal decision-id "latest") (first *permission-decision-history*))
             (t (find decision-id
                      *permission-decision-history*
                      :key (lambda (entry) (getf entry :decision-id))
                      :test #'string=)))))
    (unless historical
      (return-from explain-permission-decision nil))
    (let* ((replay-decision
             (check-permission :tool (getf historical :tool)
                               :path (getf historical :path)
                               :command (getf historical :command)
                               :permission-mode (getf historical :permission-mode)
                               :rules rules
                               :record-history-p nil))
           (replay-trace *last-permission-decision-trace*))
      (list :decision-id (getf historical :decision-id)
            :historical historical
            :replay (and replay-trace
                         (append replay-trace
                                 (list :decision replay-decision)))))))
