(in-package :amoebum/test)

(def-suite restarts-round-trip-suite :in amoebum-suite
  :description "I210 restart round-trip tests for execute-tool-with-restarts.")

(in-suite restarts-round-trip-suite)

(defun %i210-make-args (&rest entries)
  (let ((table (make-hash-table :test #'equal)))
    (loop for (key value) on entries by #'cddr do
          (setf (gethash key table) value))
    table))

(defun %i210-register-tools (toolset)
  (pseudopod:register-tool-function
   toolset
   :name "i210-flaky"
   :description "Fails unless mode=ok."
   :parameters (let ((schema (make-hash-table :test #'equal)))
                 (setf (gethash "type" schema) "object")
                 schema)
   :fn (lambda (arguments _call)
         (declare (ignore _call))
         (if (string= (or (gethash "mode" arguments) "") "ok")
             "retry-ok"
             (error "forced failure"))))
  (pseudopod:register-tool-function
   toolset
   :name "i210-alt"
   :description "Alternative tool for use-alternative-tool restart."
   :parameters (let ((schema (make-hash-table :test #'equal)))
                 (setf (gethash "type" schema) "object")
                 schema)
   :fn (lambda (_arguments _call)
         (declare (ignore _arguments _call))
         "alt-ok")))

(test i210-restarts-round-trip
  (let ((original-toolset amoebum:*toolset*)
        (original-hooks amoebum:*hook-registry*)
        (original-event-bus amoebum:*event-bus*)
        (saw-required-restarts nil))
    (unwind-protect
        (let ((toolset (pseudopod:make-toolset)))
          (setf amoebum:*toolset* toolset
                amoebum:*hook-registry* (make-hash-table :test #'equal)
                amoebum:*event-bus* (amoebum:make-event-bus :capacity 16))
          (%i210-register-tools toolset)

          (let ((result
                  (handler-bind
                      ((amoebum:tool-error
                         (lambda (condition)
                           (let ((restart-names (mapcar #'restart-name
                                                        (compute-restarts condition))))
                             (setf saw-required-restarts
                                   (every (lambda (name)
                                            (member name restart-names))
                                          '(amoebum::retry-with-modified-args
                                            amoebum::use-alternative-tool
                                            amoebum::skip-tool-call
                                            amoebum::abort-step
                                            amoebum::ask-user))))
                           (invoke-restart 'amoebum::retry-with-modified-args
                                           (%i210-make-args "mode" "ok")))))
                    (amoebum:execute-tool-with-restarts
                     "i210-flaky"
                     (%i210-make-args "mode" "bad")
                     :toolset toolset
                     :permission-mode :full-auto))))
            (is (string= result "retry-ok"))
            (is-true saw-required-restarts))

          (let ((result
                  (handler-bind
                      ((amoebum:tool-error
                         (lambda (_condition)
                           (invoke-restart 'amoebum::use-alternative-tool
                                           "i210-alt"
                                           (%i210-make-args)))))
                    (amoebum:execute-tool-with-restarts
                     "i210-flaky"
                     (%i210-make-args "mode" "bad")
                     :toolset toolset
                     :permission-mode :full-auto))))
            (is (string= result "alt-ok")))

          (let ((result
                  (handler-bind
                      ((amoebum:tool-error
                         (lambda (_condition)
                           (invoke-restart 'amoebum::skip-tool-call))))
                    (amoebum:execute-tool-with-restarts
                     "i210-flaky"
                     (%i210-make-args "mode" "bad")
                     :toolset toolset
                     :permission-mode :full-auto))))
            (is-true (search "skipped" result :test #'char-equal)))

          (signals amoebum:amoebum-error
            (handler-bind
                ((amoebum:tool-error
                   (lambda (_condition)
                     (invoke-restart 'amoebum::abort-step))))
              (amoebum:execute-tool-with-restarts
               "i210-flaky"
               (%i210-make-args "mode" "bad")
               :toolset toolset
               :permission-mode :full-auto)))

          (let ((result
                  (handler-bind
                      ((amoebum:tool-error
                         (lambda (_condition)
                           (invoke-restart 'amoebum::ask-user))))
                    (amoebum:execute-tool-with-restarts
                     "i210-flaky"
                     (%i210-make-args "mode" "bad")
                     :toolset toolset
                     :permission-mode :full-auto))))
            (is-true (search "guidance" result :test #'char-equal))))
      (setf amoebum:*toolset* original-toolset
            amoebum:*hook-registry* original-hooks
            amoebum:*event-bus* original-event-bus))))

(test i216-parse-recovery-decision
  (let ((decision
          (amoebum:parse-recovery-decision
           "{\"restart\":\"retry-tool\",\"args\":[{\"mode\":\"ok\"}]}")))
    (is (eq (getf decision :restart) 'amoebum::retry-tool))
    (is (= (length (getf decision :args)) 1)))
  (let ((decision
          (amoebum:parse-recovery-decision
           "```json\n{\"restart\":\"skip_tool\"}\n```")))
    (is (eq (getf decision :restart) 'amoebum::skip-tool))
    (is (null (getf decision :args))))
  (is (null (amoebum:parse-recovery-decision "not-json"))))

(test i216-apply-user-recovery-decision-prompts-and-invokes
  (let ((out (make-string-output-stream))
        (invoked nil))
    (restart-case
        (let* ((in (make-string-input-stream "2\n"))
               (io (make-two-way-stream in out))
               (condition (make-condition 'amoebum:tool-error
                                          :tool-name "i216-tool"
                                          :message "forced i216 error")))
          (restart-case
              (progn
                (amoebum:apply-user-recovery-decision nil condition :query-io io)
                (setf invoked :none))
            (retry-tool (&optional _)
              (declare (ignore _))
              (setf invoked :retry-tool))
            (skip-tool ()
              (setf invoked :skip-tool))))
      (abort-tool ()
        (setf invoked :abort-tool)))
    (is (eq invoked :skip-tool))
    (let ((printed (get-output-stream-string out)))
      (is-true (search "Tool error:" printed :test #'char-equal))
      (is-true (search "Choose restart" printed :test #'char-equal)))))
