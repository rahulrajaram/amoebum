(in-package :amoebum)

;;; NXT-127: Shared policy data types used by both permissions and plan-execution.

(defstruct (policy-trace-entry
            (:constructor make-policy-trace-entry
                (&key phase source decision reason-code reason data timestamp)))
  (phase nil)
  (source nil)
  (decision nil)
  (reason-code nil)
  (reason nil)
  (data nil :type list)
  (timestamp nil))

(defstruct (policy-decision-context
            (:constructor make-policy-decision-context
                (&key kind tool-name path command permission-mode
                      plan-step-index plan-event dangerous-p rules
                      context-data decision-id)))
  (kind nil)
  (tool-name nil)
  (path nil)
  (command nil)
  (permission-mode nil)
  (plan-step-index nil)
  (plan-event nil)
  (dangerous-p nil)
  (rules nil)
  (context-data nil :type list)
  (decision-id nil))

(defun build-policy-decision-context (&key kind tool-name path command permission-mode
                                           plan-step-index plan-event dangerous-p rules
                                           context-data decision-id)
  "Build a normalized policy-decision-context with copied list inputs."
  (make-policy-decision-context
   :kind kind
   :tool-name tool-name
   :path path
   :command command
   :permission-mode permission-mode
   :plan-step-index plan-step-index
   :plan-event plan-event
   :dangerous-p dangerous-p
   :rules (copy-tree (or rules '()))
   :context-data (copy-tree (or context-data '()))
   :decision-id decision-id))

(defun policy-decision-context-plist (context)
  "Project CONTEXT into a trace/event friendly plist."
  (check-type context policy-decision-context)
  (list :kind (policy-decision-context-kind context)
        :tool-name (policy-decision-context-tool-name context)
        :path (policy-decision-context-path context)
        :command (policy-decision-context-command context)
        :permission-mode (policy-decision-context-permission-mode context)
        :plan-step-index (policy-decision-context-plan-step-index context)
        :plan-event (policy-decision-context-plan-event context)
        :dangerous-p (policy-decision-context-dangerous-p context)
        :decision-id (policy-decision-context-decision-id context)
        :context-data (copy-tree (or (policy-decision-context-context-data context) '()))))

;;; NXT-134: Policy rule table struct.

(defstruct (policy-rule-table
            (:constructor make-policy-rule-table
                (&key (name "default") (source :session) (rules '()) metadata)))
  (name "default" :type string)
  (source :session)
  (rules '() :type list)
  (metadata nil :type list))

(defun policy-rule-table-rule-count (table)
  "Return the number of rules in TABLE."
  (check-type table policy-rule-table)
  (length (policy-rule-table-rules table)))

(defun policy-rule-table-find-rule (table rule-id)
  "Find a rule by ID in TABLE."
  (check-type table policy-rule-table)
  (find rule-id (policy-rule-table-rules table)
        :key (lambda (r) (and (listp r) (getf r :id)))
        :test #'equal))

;;; NXT-135: Policy rule registry with layered composition.

(defstruct (policy-rule-registry
            (:constructor %make-policy-rule-registry
                (&key session-layer (extension-layers '()) metadata)))
  session-layer
  (extension-layers '() :type list)
  (metadata nil :type list))

(defun make-policy-rule-registry (&key session-layer (extension-layers '()) metadata)
  "Create a layered policy rule registry.

SESSION-LAYER should be a POLICY-RULE-TABLE and represents session-local rules.
EXTENSION-LAYERS should be a list of POLICY-RULE-TABLE values provided by
extensions. The composed rule order is session first, then extension layers in
the provided order."
  (%make-policy-rule-registry
   :session-layer session-layer
   :extension-layers (remove nil (copy-list extension-layers))
   :metadata (copy-list metadata)))

(defun policy-rule-registry-layers (registry)
  "Return REGISTRY's active layers in evaluation order."
  (check-type registry policy-rule-registry)
  (remove nil
          (append (and (policy-rule-registry-session-layer registry)
                       (list (policy-rule-registry-session-layer registry)))
                  (copy-list (policy-rule-registry-extension-layers registry)))))

(defun policy-rule-registry-layer-count (registry)
  "Return the number of active layers in REGISTRY."
  (length (policy-rule-registry-layers registry)))

(defun policy-rule-registry-rule-count (registry)
  "Return the total composed rule count across REGISTRY layers."
  (loop for table in (policy-rule-registry-layers registry)
        sum (policy-rule-table-rule-count table)))

(defun policy-rule-registry-find-layer (registry layer)
  "Find a layer in REGISTRY by table name or source keyword."
  (check-type registry policy-rule-registry)
  (find layer (policy-rule-registry-layers registry)
        :test (lambda (needle table)
                (or (equal needle (policy-rule-table-name table))
                    (equal needle (policy-rule-table-source table))))))

(defun policy-rule-registry-composed-rules (registry)
  "Flatten REGISTRY layers into a single ordered rule list."
  (check-type registry policy-rule-registry)
  (loop for table in (policy-rule-registry-layers registry)
        append (copy-list (policy-rule-table-rules table))))
