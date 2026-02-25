(in-package :amoebum/test)

(def-suite condition-to-llm-context-suite :in amoebum-suite
  :description "LLM context formatting for amoebum condition hierarchy (I209).")

(in-suite condition-to-llm-context-suite)

(defun %i209-assert-standard-context-fields (context expected-type)
  (is-true (search (format nil "Condition type: ~A" expected-type) context :test #'char-equal)
           "Expected condition type ~A in context: ~S"
           expected-type
           context)
  (is-true (search "Message:" context :test #'char-equal)
           "Expected message field in context: ~S"
           context)
  (is-true (search "Available restarts:" context :test #'char-equal)
           "Expected restarts field in context: ~S"
           context)
  (is-true (search "Suggested action:" context :test #'char-equal)
           "Expected suggested action field in context: ~S"
           context))

(test condition-to-llm-context-covers-all-amoebum-condition-types
  (let ((cases
          (list
           (list "amoebum-error"
                 (make-condition 'amoebum:amoebum-error :message "generic amoebum failure"))
           (list "tool-error"
                 (make-condition 'amoebum:tool-error
                                 :tool-name "i209-tool"
                                 :message "tool failed"))
           (list "tool-execution-error"
                 (make-condition 'amoebum:tool-execution-error
                                 :tool-name "i209-tool"
                                 :message "execution failed"))
           (list "tool-timeout"
                 (make-condition 'amoebum:tool-timeout
                                 :tool-name "i209-tool"
                                 :timeout-seconds 7))
           (list "tool-timeout-error"
                 (make-condition 'amoebum:tool-timeout-error
                                 :tool-name "i209-tool"
                                 :timeout-seconds 3))
           (list "tool-permission-denied"
                 (make-condition 'amoebum:tool-permission-denied
                                 :tool-name "i209-tool"
                                 :reason "requires approval"))
           (list "tool-not-found"
                 (make-condition 'amoebum:tool-not-found :tool-name "missing-tool"))
           (list "tool-not-found-error"
                 (make-condition 'amoebum:tool-not-found-error :tool-name "missing-tool"))
           (list "tool-argument-error"
                 (make-condition 'amoebum:tool-argument-error
                                 :tool-name "i209-tool"
                                 :argument-name "path"
                                 :reason "invalid"))
           (list "tool-missing-argument"
                 (make-condition 'amoebum:tool-missing-argument
                                 :tool-name "i209-tool"
                                 :argument-name "path"
                                 :reason "required"))
           (list "tool-type-mismatch"
                 (make-condition 'amoebum:tool-type-mismatch
                                 :tool-name "i209-tool"
                                 :argument-name "count"
                                 :reason "expected integer"))
           (list "hook-execution-error"
                 (make-condition 'amoebum:hook-execution-error
                                 :hook-id 'i209-hook
                                 :hook-point :pre-tool-use
                                 :message "hook failed"))
           (list "context-overflow-error"
                 (make-condition 'amoebum:context-overflow-error
                                 :used-tokens 2100
                                 :max-tokens 2048))
           (list "budget-exceeded-error"
                 (make-condition 'amoebum:budget-exceeded-error
                                 :kind :token
                                 :used 1200
                                 :budget 1000)))))
    (dolist (entry cases)
      (destructuring-bind (expected-type condition) entry
        (%i209-assert-standard-context-fields
         (amoebum:condition-to-llm-context condition)
         expected-type)))))

(test condition-to-llm-context-includes-available-tool-restarts
  (let* ((condition (make-condition 'amoebum:tool-error
                                    :tool-name "i209-tool"
                                    :message "failure"))
         (context
           (restart-case
               (restart-case
                   (restart-case
                       (restart-case
                           (amoebum:condition-to-llm-context condition)
                         (retry-tool ()
                           :report "Retry tool."
                           nil))
                     (skip-tool ()
                       :report "Skip tool."
                       nil))
                 (use-value (value)
                   :report "Use provided value."
                   value))
             (abort-tool ()
               :report "Abort tool."
               nil))))
    (is-true (search "[retry-tool]" context :test #'char-equal)
             "Expected retry-tool restart listed in context: ~S"
             context)
    (is-true (search "[skip-tool]" context :test #'char-equal)
             "Expected skip-tool restart listed in context: ~S"
             context)
    (is-true (search "[use-value]" context :test #'char-equal)
             "Expected use-value restart listed in context: ~S"
             context)
    (is-true (search "[abort-tool]" context :test #'char-equal)
             "Expected abort-tool restart listed in context: ~S"
             context)))
