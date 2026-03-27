(in-package :amoebum/test)

;;; ---------------------------------------------------------------------------
;;; Permission Decision Dispatch Table Tests (FP-Refine Phase 2, Target 4)
;;; ---------------------------------------------------------------------------

(def-suite permission-decision-dispatch-suite :in amoebum-suite
  :description "Tests for permission decision rule tables and mode default tables.")

(in-suite permission-decision-dispatch-suite)

;;; --- Helper ---

(defun %make-perm-context (&key (plan-mode-blocked-p nil)
                                (path-decision nil)
                                (command-decision nil)
                                (tool nil)
                                (path nil)
                                (command nil)
                                (mcp-decision nil)
                                (mode :supervised))
  "Build a minimal permission-check-context for testing."
  (amoebum::make-permission-check-context
   :tool tool
   :path path
   :command command
   :mode mode
   :plan-mode-blocked-p plan-mode-blocked-p
   :mcp-decision mcp-decision
   :normalized-path path
   :policy-command-text command))

(defun %set-perm-decisions (ctx &key path-decision command-decision)
  "Manually set path/command decisions on context (simulating evaluation)."
  (when path-decision
    (setf (amoebum::permission-check-context-path-decision ctx) path-decision))
  (when command-decision
    (setf (amoebum::permission-check-context-command-decision ctx) command-decision))
  ctx)

;;; --- Table Structure Tests ---

(test permission-base-decision-rules-has-8-entries
  (is (= 8 (length amoebum::+permission-base-decision-rules+))))

(test permission-mode-defaults-has-4-entries
  (is (= 4 (length amoebum::+permission-mode-defaults+))))

(test permission-base-decision-rules-labels-unique
  (let ((labels (mapcar #'first amoebum::+permission-base-decision-rules+)))
    (is (= (length labels) (length (remove-duplicates labels))))))

(test permission-base-decision-rules-predicates-are-functions
  (dolist (rule amoebum::+permission-base-decision-rules+)
    (is (fboundp (second rule)))))

(test permission-mode-defaults-values-are-keywords
  (dolist (entry amoebum::+permission-mode-defaults+)
    (is (keywordp (cdr entry)))))

;;; --- Individual Predicate Tests ---

(test predicate-plan-mode-blocked-true
  (let ((ctx (%make-perm-context :plan-mode-blocked-p t)))
    (is-true (amoebum::%permission-context-plan-mode-blocked-p ctx))))

(test predicate-plan-mode-blocked-false
  (let ((ctx (%make-perm-context :plan-mode-blocked-p nil)))
    (is-false (amoebum::%permission-context-plan-mode-blocked-p ctx))))

(test predicate-rule-denies-path-deny
  (let ((ctx (%set-perm-decisions (%make-perm-context)
                                  :path-decision :deny)))
    (is-true (amoebum::%permission-context-rule-denies-p ctx))))

(test predicate-rule-denies-command-deny
  (let ((ctx (%set-perm-decisions (%make-perm-context)
                                  :command-decision :deny)))
    (is-true (amoebum::%permission-context-rule-denies-p ctx))))

(test predicate-rule-denies-neither
  (let ((ctx (%set-perm-decisions (%make-perm-context)
                                  :path-decision :allow
                                  :command-decision :allow)))
    (is-false (amoebum::%permission-context-rule-denies-p ctx))))

(test predicate-command-allows-true
  (let ((ctx (%set-perm-decisions (%make-perm-context)
                                  :command-decision :allow)))
    (is-true (amoebum::%permission-context-command-allows-p ctx))))

(test predicate-command-allows-false
  (let ((ctx (%set-perm-decisions (%make-perm-context)
                                  :command-decision nil)))
    (is-false (amoebum::%permission-context-command-allows-p ctx))))

(test predicate-path-allows-true
  (let ((ctx (%set-perm-decisions (%make-perm-context)
                                  :path-decision :allow)))
    (is-true (amoebum::%permission-context-path-allows-p ctx))))

(test predicate-mcp-decision-present
  (let ((ctx (%make-perm-context :mcp-decision :allow)))
    (is-true (amoebum::%permission-context-has-mcp-decision-p ctx))))

(test predicate-mcp-decision-absent
  (let ((ctx (%make-perm-context :mcp-decision nil)))
    (is-false (amoebum::%permission-context-has-mcp-decision-p ctx))))

(test predicate-always-true
  (is-true (amoebum::%always-true nil))
  (is-true (amoebum::%always-true "anything")))

;;; --- Full Chain Evaluation Tests ---

(test chain-plan-mode-blocked-returns-deny
  (let ((ctx (%make-perm-context :plan-mode-blocked-p t :mode :full-auto)))
    (is (eq :deny (amoebum::%permission-check-base-decision ctx)))))

(test chain-rule-deny-overrides-mode
  (let ((ctx (%set-perm-decisions
              (%make-perm-context :mode :full-auto)
              :path-decision :deny)))
    (is (eq :deny (amoebum::%permission-check-base-decision ctx)))))

(test chain-command-allow-when-no-deny
  (let ((ctx (%set-perm-decisions (%make-perm-context) :command-decision :allow)))
    (is (eq :allow (amoebum::%permission-check-base-decision ctx)))))

(test chain-path-allow-when-no-deny-no-command-allow
  (let ((ctx (%set-perm-decisions (%make-perm-context)
                                  :path-decision :allow)))
    (is (eq :allow (amoebum::%permission-check-base-decision ctx)))))

(test chain-mcp-decision-when-set
  (let ((ctx (%make-perm-context :mcp-decision :prompt)))
    (is (eq :prompt (amoebum::%permission-check-base-decision ctx)))))

(test chain-falls-through-to-mode-default
  (let ((ctx (%make-perm-context :mode :full-auto)))
    (is (eq :allow (amoebum::%permission-check-base-decision ctx)))))

;;; --- Mode Default Table Tests ---

(test mode-default-plan-returns-prompt
  (is (eq :prompt (amoebum::%mode-default-decision :plan nil nil nil))))

(test mode-default-supervised-returns-prompt
  (is (eq :prompt (amoebum::%mode-default-decision :supervised nil nil nil))))

(test mode-default-full-auto-returns-allow
  (is (eq :allow (amoebum::%mode-default-decision :full-auto nil nil nil))))

(test mode-default-yolo-returns-allow
  (is (eq :allow (amoebum::%mode-default-decision :yolo nil nil nil))))

(test mode-default-unknown-returns-prompt
  (is (eq :prompt (amoebum::%mode-default-decision :nonexistent nil nil nil))))

;;; --- Auto-Edit Mode Tests ---

(test auto-edit-shell-tool-returns-prompt
  (is (eq :prompt (amoebum::%mode-default-decision :auto-edit "bash" nil "echo hi"))))

(test auto-edit-file-tool-returns-allow
  (is (eq :allow (amoebum::%mode-default-decision :auto-edit "read-file" "/tmp/f" nil))))

(test auto-edit-path-present-returns-allow
  (is (eq :allow (amoebum::%mode-default-decision :auto-edit "unknown" "/some/path" nil))))

(test auto-edit-no-path-no-file-tool-returns-prompt
  (is (eq :prompt (amoebum::%mode-default-decision :auto-edit "unknown-tool" nil nil))))

;;; --- Rule Evaluator Tests ---

(test evaluate-empty-rules-returns-nil
  (is (null (amoebum::%evaluate-permission-decision-rules nil '()))))

(test evaluate-rules-returns-first-match
  (let ((rules '((:a amoebum::%always-true :deny)
                 (:b amoebum::%always-true :allow))))
    (let ((ctx (%make-perm-context)))
      (is (eq :deny (amoebum::%evaluate-permission-decision-rules ctx rules))))))
