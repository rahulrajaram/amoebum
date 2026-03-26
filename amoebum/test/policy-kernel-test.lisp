(in-package :amoebum/test)

(def-suite policy-kernel-suite :in amoebum-suite
  :description "Permission and plan reducer kernels (I372).")

(in-suite policy-kernel-suite)

(defun %make-policy-kernel-execution-state ()
  (let* ((step (amoebum.plan:make-plan-execution-step
                :index 1
                :description "Run `make build`"
                :file-paths '("Makefile")))
         (state (amoebum::%make-plan-execution-state
                 :run-id "plan-exec-test"
                 :status :running
                 :steps (list step)
                 :ordered-step-indexes '(1)
                 :approved-step-indexes '(1)
                 :pending-step-indexes '(1)
                 :completed-step-indexes '())))
    (values state step)))

(test permission-evaluation-stays-pure-until-materialized
  (let ((amoebum::*permission-decision-history* '())
        (amoebum::*permission-decision-sequence* 0)
        (amoebum::*last-permission-decision-trace* nil))
    (let ((evaluation (amoebum::evaluate-permission-decision
                       :tool :write-file
                       :path "./notes.txt"
                       :permission-mode :plan)))
      (is (typep evaluation 'amoebum::permission-evaluation))
      (is (eq :deny (amoebum::permission-evaluation-decision evaluation)))
      (is (eq :plan-mode
              (amoebum::permission-evaluation-decision-source evaluation)))
      (is (eq :plan-mode-mutating-command-blocked
              (amoebum::permission-evaluation-reason-code evaluation)))
      (is (search "Plan mode blocked mutating tool"
                  (or (amoebum::permission-evaluation-reason evaluation) "")
                  :test #'char-equal))
      (is (null amoebum::*permission-decision-history*))
      (is (null amoebum::*last-permission-decision-trace*)))))

(test permission-effect-plan-keeps-prompt-metadata-explicit
  (let* ((arguments (make-hash-table :test #'equal))
         (_ (setf (gethash "path" arguments) "./notes.txt"))
         (evaluation (amoebum::evaluate-permission-decision
                      :tool :read-file
                      :path "./notes.txt"
                      :permission-mode :supervised))
         (effect (amoebum::%permission-evaluation-effect evaluation
                                                         "read-file"
                                                         arguments)))
    (declare (ignore _))
    (is (eq :prompt (getf effect :kind)))
    (is (string= "read-file" (or (getf effect :tool-name) "")))
    (is (string= "./notes.txt" (or (getf effect :path) "")))
    (is (stringp (getf effect :reason)))
    (is (eq :permission-check
            (getf (getf effect :decision-context) :kind)))
    (is (equal (amoebum::permission-evaluation-trace evaluation)
               (getf effect :trace)))))

(test permission-evaluation-builds-bridged-decision-context
  (let ((amoebum::*permission-decision-history* '())
        (amoebum::*permission-decision-sequence* 0)
        (amoebum::*last-permission-decision-trace* nil))
    (let* ((evaluation (amoebum::evaluate-permission-decision
                        :tool :write-file
                        :path "./notes.txt"
                        :permission-mode :plan))
           (decision-context (amoebum::permission-evaluation-decision-context evaluation))
           (context-plist (amoebum:policy-decision-context-plist decision-context))
           (input-entry (first (amoebum::permission-evaluation-structured-trace evaluation)))
           (trace-context (getf (amoebum:policy-trace-entry-data input-entry)
                                :decision-context)))
      (is (typep decision-context 'amoebum:policy-decision-context))
      (is (eq :permission-check (amoebum:policy-decision-context-kind decision-context)))
      (is (string= "write-file"
                   (or (amoebum:policy-decision-context-tool-name decision-context) "")))
      (is (eq :plan (amoebum:policy-decision-context-permission-mode decision-context)))
      (is (stringp (amoebum:policy-decision-context-path decision-context)))
      (is (search "notes.txt"
                  (or (amoebum:policy-decision-context-path decision-context) "")
                  :test #'char-equal))
      (is (eq :permission-check (getf context-plist :kind)))
      (is (equal context-plist trace-context)))))

;;; --- NXT-127: Policy trace entry struct ---

(test policy-trace-entry-struct-accessible
  (let ((entry (amoebum:make-policy-trace-entry
                :phase :input
                :source :permission-check
                :decision :allow
                :reason-code :explicit-allow
                :reason "rule match"
                :data '(:tool-name "read-file")
                :timestamp 1000)))
    (is (eq :input (amoebum:policy-trace-entry-phase entry)))
    (is (eq :permission-check (amoebum:policy-trace-entry-source entry)))
    (is (eq :allow (amoebum:policy-trace-entry-decision entry)))
    (is (eq :explicit-allow (amoebum:policy-trace-entry-reason-code entry)))
    (is (string= "rule match" (amoebum:policy-trace-entry-reason entry)))
    (is (equal '(:tool-name "read-file") (amoebum:policy-trace-entry-data entry)))
    (is (= 1000 (amoebum:policy-trace-entry-timestamp entry)))))

(test permission-evaluation-carries-structured-trace
  (let ((amoebum::*permission-decision-history* '())
        (amoebum::*permission-decision-sequence* 0)
        (amoebum::*last-permission-decision-trace* nil))
    (let ((evaluation (amoebum::evaluate-permission-decision
                       :tool :write-file
                       :path "./notes.txt"
                       :permission-mode :plan)))
      (is (typep evaluation 'amoebum::permission-evaluation))
      (let ((structured-trace (amoebum::permission-evaluation-structured-trace evaluation)))
        (is (listp structured-trace))
        (is (plusp (length structured-trace)))
        (is (every (lambda (e) (typep e 'amoebum::policy-trace-entry)) structured-trace))
        (let ((phases (mapcar #'amoebum:policy-trace-entry-phase structured-trace)))
          (is (member :input phases))
          (is (member :evaluate phases))
          (is (member :materialize phases)))))))

(test plan-execution-transition-carries-structured-trace
  (multiple-value-bind (state step)
      (%make-policy-kernel-execution-state)
    (setf (amoebum.plan:plan-execution-step-status step) :running
          (amoebum.plan:plan-execution-step-started-at step) 100)
    (let ((transition (amoebum::%evaluate-plan-execution-transition
                       state
                       :step-success
                       :step step
                       :result "make build ok"
                       :now 200)))
      (let ((structured-trace (amoebum::plan-execution-transition-structured-trace transition)))
        (is (listp structured-trace))
        (is (plusp (length structured-trace)))
        (is (every (lambda (e) (typep e 'amoebum::policy-trace-entry)) structured-trace))
        (let ((phases (mapcar #'amoebum:policy-trace-entry-phase structured-trace)))
          (is (member :input phases))
          (is (member :evaluate phases)))))))

(test plan-execution-transition-builds-bridged-decision-context
  (multiple-value-bind (state step)
      (%make-policy-kernel-execution-state)
    (setf (amoebum.plan:plan-execution-step-status step) :running
          (amoebum.plan:plan-execution-step-started-at step) 100)
    (let* ((transition (amoebum::%evaluate-plan-execution-transition
                        state
                        :step-success
                        :step step
                        :result "make build ok"
                        :now 200))
           (decision-context (amoebum::plan-execution-transition-decision-context transition))
           (context-plist (amoebum:policy-decision-context-plist decision-context))
           (input-entry (first (amoebum::plan-execution-transition-structured-trace transition)))
           (trace-context (getf (amoebum:policy-trace-entry-data input-entry)
                                :decision-context)))
      (is (typep decision-context 'amoebum:policy-decision-context))
      (is (eq :plan-transition (amoebum:policy-decision-context-kind decision-context)))
      (is (= 1 (amoebum:policy-decision-context-plan-step-index decision-context)))
      (is (eq :step-success (amoebum:policy-decision-context-plan-event decision-context)))
      (is (eq :plan (amoebum:policy-decision-context-permission-mode decision-context)))
      (is (string= "Makefile" (or (amoebum:policy-decision-context-path decision-context) "")))
      (is (string= "make build"
                   (or (amoebum:policy-decision-context-command decision-context) "")))
      (is (eq :plan-transition (getf context-plist :kind)))
      (is (equal context-plist trace-context)))))

(test plan-execution-transition-success-finishes-final-step
  (multiple-value-bind (state step)
      (%make-policy-kernel-execution-state)
    (setf (amoebum.plan:plan-execution-step-status step) :running
          (amoebum.plan:plan-execution-step-started-at step) 100)
    (let ((transition (amoebum::%evaluate-plan-execution-transition
                       state
                       :step-success
                       :step step
                       :result "make build ok"
                       :now 200)))
      (is-true (amoebum::plan-execution-transition-done-p transition))
      (is (eq :completed
              (getf (amoebum::plan-execution-transition-state-updates transition)
                    :status)))
      (amoebum::%apply-plan-execution-transition! state transition)
      (is (eq :completed (amoebum.plan:plan-execution-state-status state)))
      (is (null (amoebum.plan:plan-execution-state-pending-step-indexes state)))
      (is (equal '(1) (amoebum.plan:plan-execution-state-completed-step-indexes state)))
      (is (eq :completed (amoebum.plan:plan-execution-step-status step)))
      (let ((lines (amoebum.plan:plan-execution-output-lines state)))
        (is-true (find-if (lambda (line)
                            (search "[step 1 done]" line :test #'char-equal))
                          lines))
        (is-true (find-if (lambda (line)
                            (search "All approved steps completed" line
                                    :test #'char-equal))
                          lines))))))

(test plan-execution-transition-failure-blocks-step-and-emits-recovery-copy
  (multiple-value-bind (state step)
      (%make-policy-kernel-execution-state)
    (setf (amoebum.plan:plan-execution-step-status step) :running
          (amoebum.plan:plan-execution-step-started-at step) 100)
    (let* ((condition (make-condition 'simple-error
                                      :format-control "boom"
                                      :format-arguments '()))
           (transition (amoebum::%evaluate-plan-execution-transition
                        state
                        :step-failure
                        :step step
                        :condition condition
                        :now 200)))
      (is-true (amoebum::plan-execution-transition-done-p transition))
      (is (eq :failed
              (getf (amoebum::plan-execution-transition-state-updates transition)
                    :status)))
      (amoebum::%apply-plan-execution-transition! state transition)
      (is (eq :failed (amoebum.plan:plan-execution-state-status state)))
      (is (eq condition (amoebum.plan:plan-execution-state-failure-reason state)))
      (is (eq :blocked (amoebum.plan:plan-execution-step-status step)))
      (let ((lines (amoebum.plan:plan-execution-output-lines state)))
        (is-true (find-if (lambda (line)
                            (search "Choose next action" line :test #'char-equal))
                          lines))))))

;;; --- NXT-134: Policy rule table ---

(test policy-rule-table-struct-accessible
  (let ((table (amoebum:make-policy-rule-table
                :name "test-layer"
                :source :extension
                :rules (list '(:id "r1" :effect :allow :path "*.lisp")
                             '(:id "r2" :effect :deny :command "rm"))
                :metadata '(:version 1))))
    (is (string= "test-layer" (amoebum:policy-rule-table-name table)))
    (is (eq :extension (amoebum:policy-rule-table-source table)))
    (is (= 2 (amoebum:policy-rule-table-rule-count table)))
    (is (not (null (amoebum:policy-rule-table-find-rule table "r1"))))
    (is (null (amoebum:policy-rule-table-find-rule table "r99")))))

(test policy-rule-registry-composes-session-and-extension-layers
  (let* ((session-rule (amoebum:make-permission-rule
                        :id "session-allow"
                        :effect :allow
                        :tool "read-file"
                        :path "./notes.txt"
                        :source :session))
         (extension-rule-a (amoebum:make-permission-rule
                            :id "ext-a"
                            :effect :deny
                            :tool "read-file"
                            :path "./src/*.lisp"
                            :source :extension))
         (extension-rule-b (amoebum:make-permission-rule
                            :id "ext-b"
                            :effect :allow
                            :tool "grep-content"
                            :path "./src/*.lisp"
                            :source :extension))
         (session-layer (amoebum:make-policy-rule-table
                         :name "session"
                         :source :session
                         :rules (list session-rule)))
         (ext-a-layer (amoebum:make-policy-rule-table
                       :name "lint-ext"
                       :source :extension
                       :rules (list extension-rule-a)))
         (ext-b-layer (amoebum:make-policy-rule-table
                       :name "search-ext"
                       :source :extension
                       :rules (list extension-rule-b)))
         (registry (amoebum:make-policy-rule-registry
                    :session-layer session-layer
                    :extension-layers (list ext-a-layer ext-b-layer)
                    :metadata '(:version 1))))
    (is (= 3 (amoebum:policy-rule-registry-rule-count registry)))
    (is (= 3 (length (amoebum:policy-rule-registry-composed-rules registry))))
    (is (= 3 (amoebum:policy-rule-registry-layer-count registry)))
    (is (eq session-layer
            (first (amoebum:policy-rule-registry-layers registry))))
    (is (eq ext-a-layer
            (amoebum:policy-rule-registry-find-layer registry "lint-ext")))
    (is (eq ext-b-layer
            (amoebum:policy-rule-registry-find-layer registry "search-ext")))))

(test permission-evaluation-accepts-layered-policy-rule-registry
  (let* ((session-rule (amoebum:make-permission-rule
                        :id "session-deny"
                        :effect :deny
                        :tool "read-file"
                        :path "./shared.txt"
                        :source :session))
         (extension-rule (amoebum:make-permission-rule
                          :id "extension-allow"
                          :effect :allow
                          :tool "read-file"
                          :path "./shared.txt"
                          :source :extension))
         (registry (amoebum:make-policy-rule-registry
                    :session-layer
                    (amoebum:make-policy-rule-table
                     :name "session"
                     :source :session
                     :rules (list session-rule))
                    :extension-layers
                    (list (amoebum:make-policy-rule-table
                           :name "demo-ext"
                           :source :extension
                           :rules (list extension-rule))))))
    (multiple-value-bind (decision trace)
        (amoebum:evaluate-path-permission
         :tool "read-file"
         :path "./shared.txt"
         :rules registry
         :with-trace-p t)
      (is (eq :deny decision))
      (is (equal "session-deny" (getf trace :matched-rule-id)))
      (is (eq :session (getf trace :source))))))

;;; --- NXT-136: Permission decision replay ---

(test replay-permission-decision-from-trace-extracts-phases
  (let ((amoebum::*permission-decision-history* '())
        (amoebum::*permission-decision-sequence* 0)
        (amoebum::*last-permission-decision-trace* nil))
    (let* ((evaluation (amoebum::evaluate-permission-decision
                        :tool :write-file
                        :path "./notes.txt"
                        :permission-mode :plan))
           (replay (amoebum:replay-permission-decision-from-trace
                    (amoebum::permission-evaluation-structured-trace evaluation))))
      (is (listp replay))
      (is (not (null (getf replay :input-data))))
      (is (not (null (getf replay :evaluated-decision))))
      (is (not (null (getf replay :final-decision)))))))

;;; --- NXT-137: Plan-execution effect type registry ---

(test plan-execution-effect-type-registry-has-defaults
  (is-true (amoebum:plan-execution-effect-type-registered-p :output))
  (is-true (amoebum:plan-execution-effect-type-registered-p :status))
  (is-true (amoebum:plan-execution-effect-type-registered-p :progress))
  (is (null (amoebum:plan-execution-effect-type-registered-p :nonexistent))))

;;; --- NXT-139: Policy decision auditor ---

(test format-policy-trace-report-returns-string
  (let ((trace (list (amoebum:make-policy-trace-entry
                      :phase :input
                      :source :test
                      :data '(:tool-name "read-file"))
                     (amoebum:make-policy-trace-entry
                      :phase :evaluate
                      :source :rule-match
                      :decision :allow
                      :reason-code :explicit-allow))))
    (let ((report (amoebum:format-policy-trace-report trace)))
      (is (stringp report))
      (is (search "Policy Decision Report" report))
      (is (search "Phase: INPUT" report))
      (is (search "Decision: ALLOW" report)))))

;;; --- NXT-140: Permission decision diff ---

(test diff-permission-decisions-detects-changes
  (let ((trace-a (list (amoebum:make-policy-trace-entry
                        :phase :evaluate :decision :allow)
                       (amoebum:make-policy-trace-entry
                        :phase :materialize :decision :allow)))
        (trace-b (list (amoebum:make-policy-trace-entry
                        :phase :evaluate :decision :deny)
                       (amoebum:make-policy-trace-entry
                        :phase :materialize :decision :deny))))
    (let ((diff (amoebum:diff-permission-decisions trace-a trace-b)))
      (is (not (getf diff :same-p)))
      (is (eq :allow (getf diff :decision-a)))
      (is (eq :deny (getf diff :decision-b)))
      (is (plusp (length (getf diff :changes)))))))

(test diff-permission-decisions-same-when-identical
  (let ((trace (list (amoebum:make-policy-trace-entry
                      :phase :evaluate :decision :allow)
                     (amoebum:make-policy-trace-entry
                      :phase :materialize :decision :allow))))
    (let ((diff (amoebum:diff-permission-decisions trace trace)))
      (is-true (getf diff :same-p))
      (is (null (getf diff :changes))))))
