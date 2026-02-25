(in-package :amoebum/test)

(def-suite llm-restart-selection-suite :in amoebum-suite
  :description "I215 LLM-driven restart selection for tool recovery.")

(in-suite llm-restart-selection-suite)

(defun %i215-make-args (&rest entries)
  (let ((table (make-hash-table :test #'equal)))
    (loop for (key value) on entries by #'cddr do
          (setf (gethash key table) value))
    table))

(defun %i215-register-tools (toolset)
  (pseudopod:register-tool-function
   toolset
   :name "i215-flaky"
   :description "Fails unless mode=ok."
   :parameters (let ((schema (make-hash-table :test #'equal)))
                 (setf (gethash "type" schema) "object")
                 schema)
   :fn (lambda (arguments _call)
         (declare (ignore _call))
         (if (string= (or (gethash "mode" arguments) "") "ok")
             "i215-ok"
             (error "forced failure")))))

(test i215-llm-restart-selection-and-ask-user-fallback
  (let ((original-toolset amoebum:*toolset*)
        (original-hooks amoebum:*hook-registry*)
        (original-event-bus amoebum:*event-bus*)
        (original-selector amoebum::*tool-error-llm-recovery-function*)
        (captured-context nil)
        (captured-restarts nil))
    (unwind-protect
        (let ((toolset (pseudopod:make-toolset)))
          (setf amoebum:*toolset* toolset
                amoebum:*hook-registry* (make-hash-table :test #'equal)
                amoebum:*event-bus* (amoebum:make-event-bus :capacity 16))
          (%i215-register-tools toolset)

          (setf amoebum::*tool-error-llm-recovery-function*
                (lambda (condition tool-name arguments restart-options)
                  (declare (ignore tool-name arguments))
                  (setf captured-context (amoebum:condition-to-llm-context condition)
                        captured-restarts restart-options)
                  "{\"restart\":\"retry-with-modified-args\",\"args\":{\"mode\":\"ok\"}}"))
          (let ((result
                  (amoebum:execute-tool-with-restarts
                   "i215-flaky"
                   (%i215-make-args "mode" "bad")
                   :toolset toolset
                   :permission-mode :full-auto)))
            (is (string= result "i215-ok"))
            (is-true (search "Condition type:" captured-context :test #'char-equal))
            (is-true (some (lambda (entry)
                             (string= (getf entry :name) "retry-with-modified-args"))
                           captured-restarts)))

          (setf amoebum::*tool-error-llm-recovery-function*
                (lambda (&rest _)
                  (declare (ignore _))
                  "not-json"))
          (let ((result
                  (amoebum:execute-tool-with-restarts
                   "i215-flaky"
                   (%i215-make-args "mode" "bad")
                   :toolset toolset
                   :permission-mode :full-auto)))
            (is-true (search "guidance" result :test #'char-equal))))
      (setf amoebum:*toolset* original-toolset
            amoebum:*hook-registry* original-hooks
            amoebum:*event-bus* original-event-bus
            amoebum::*tool-error-llm-recovery-function* original-selector))))
