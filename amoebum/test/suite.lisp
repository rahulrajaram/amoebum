(defpackage :amoebum/test
  (:use :cl :fiveam)
  (:export #:run-all #:amoebum-suite))

(in-package :amoebum/test)

(def-suite amoebum-suite
  :description "Amoebum core smoke suite (I23-I42).")

(in-suite amoebum-suite)

(defparameter +i23-i41-smoke-scripts+
  '(("agents-smoke-test.lisp" "AMOEBUM_AGENTS_SMOKE_OK")
    ("chat-ui-smoke-test.lisp" "AMOEBUM_CHAT_UI_SMOKE_OK")
    ("commands-smoke-test.lisp" "AMOEBUM_COMMANDS_SMOKE_OK")
    ("conditions-smoke-test.lisp" "AMOEBUM_CONDITIONS_SMOKE_OK")
    ("config-smoke-test.lisp" "AMOEBUM_CONFIG_SMOKE_OK")
    ("defhook-smoke-test.lisp" "AMOEBUM_DEFHOOK_SMOKE_OK")
    ("defkeys-smoke-test.lisp" "AMOEBUM_DEFKEYS_SMOKE_OK")
    ("deftool-smoke-test.lisp" "AMOEBUM_DEFTOOL_SMOKE_OK")
    ("events-smoke-test.lisp" "AMOEBUM_EVENTS_SMOKE_OK")
    ("file-tools-smoke-test.lisp" "AMOEBUM_FILE_TOOLS_SMOKE_OK")
    ("memory-smoke-test.lisp" "AMOEBUM_MEMORY_SMOKE_OK")
    ("permissions-smoke-test.lisp" "AMOEBUM_PERMISSIONS_SMOKE_OK")
    ("pipeline-smoke-test.lisp" "AMOEBUM_PIPELINE_SMOKE_OK")
    ("plan-mode-smoke-test.lisp" "AMOEBUM_PLAN_MODE_SMOKE_OK")
    ("search-tools-smoke-test.lisp" "AMOEBUM_SEARCH_TOOLS_SMOKE_OK")
    ("shell-tool-smoke-test.lisp" "AMOEBUM_SHELL_TOOL_SMOKE_OK")
    ("status-bar-smoke-test.lisp" "AMOEBUM_STATUS_BAR_SMOKE_OK")
    ("streaming-smoke-test.lisp" "AMOEBUM_STREAMING_SMOKE_OK")))

(defparameter *i42-integration-tool-counter* 0)

(defun %amoebum-system-root ()
  (uiop:ensure-directory-pathname (asdf:system-source-directory "amoebum")))

(defun %run-smoke-script (filename)
  (let* ((script-path (merge-pathnames filename (%amoebum-system-root)))
         (command (list "sbcl" "--script" (namestring script-path))))
    (multiple-value-bind (stdout stderr exit-code)
        (uiop:run-program command
                          :ignore-error-status t
                          :output :string
                          :error-output :string)
      (values stdout stderr exit-code (namestring script-path)))))

(test i23-i41-subsystem-smokes-pass
  (is (>= (* 3 (length +i23-i41-smoke-scripts+)) 50)
      "Suite should provide at least 50 checks via subsystem smoke coverage.")
  (dolist (entry +i23-i41-smoke-scripts+)
    (destructuring-bind (filename sentinel) entry
      (multiple-value-bind (stdout stderr exit-code script-path)
          (%run-smoke-script filename)
        (let ((combined (concatenate 'string (or stdout "") (or stderr ""))))
          (is (integerp exit-code)
              "Expected script ~A to return integer exit code." script-path)
          (is (= exit-code 0)
              "Expected script ~A to exit 0, got exit code ~S output=~S"
              script-path
              exit-code
              combined)
          (is-true (search sentinel combined :test #'char-equal)
                   "Expected script ~A output to contain ~A, got ~S."
                   script-path
                   sentinel
                   combined))))))

(test core-packages-and-entrypoints-present
  (is-true (find-package :amoebum))
  (is-true (find-package :amoebum.internal))
  (is-true (fboundp 'amoebum:main))
  (is-true (fboundp 'amoebum:execute-tool))
  (is-true (fboundp 'amoebum:dispatch-slash-command)))

(test integration-tool-permission-event-hook-flow
  (let ((original-toolset amoebum:*toolset*)
        (original-metadata amoebum:*tool-metadata*)
        (original-hooks amoebum:*hook-registry*)
        (original-event-bus amoebum:*event-bus*)
        (original-rules amoebum:*permission-rules*))
    (unwind-protect
        (progn
          (setf amoebum:*toolset* (pseudopod:make-toolset)
                amoebum:*tool-metadata* (make-hash-table :test #'equal)
                amoebum:*hook-registry* (make-hash-table :test #'equal)
                amoebum:*event-bus* (amoebum:make-event-bus :capacity 64)
                amoebum:*permission-rules* nil
                *i42-integration-tool-counter* 0)
          (let ((invoked-events 0)
                (completed-events 0)
                (prompted-events 0)
                (pre-hook-count 0)
                (post-hook-count 0))
            (amoebum:subscribe amoebum:*event-bus*
                               amoebum:+event-type-tool-invoked+
                               (lambda (_event)
                                 (declare (ignore _event))
                                 (incf invoked-events)))
            (amoebum:subscribe amoebum:*event-bus*
                               amoebum:+event-type-tool-completed+
                               (lambda (_event)
                                 (declare (ignore _event))
                                 (incf completed-events)))
            (amoebum:subscribe amoebum:*event-bus*
                               amoebum:+event-type-permission-prompted+
                               (lambda (_event)
                                 (declare (ignore _event))
                                 (incf prompted-events)))
            (amoebum:register-hook :pre-tool-use
                                   'i42-suite-pre-hook
                                   (lambda (_tool-name _args)
                                     (declare (ignore _tool-name _args))
                                     (incf pre-hook-count)
                                     :allow))
            (amoebum:register-hook :post-tool-use
                                   'i42-suite-post-hook
                                   (lambda (_tool-name _result _elapsed-ms)
                                     (declare (ignore _tool-name _result _elapsed-ms))
                                     (incf post-hook-count)
                                     :ok))
            (eval
             '(amoebum:deftool i42-suite-tool
                  ((value integer :description "Integration probe value." :required t))
                "I42 integration probe tool."
                (:permission :auto)
                (:dangerous nil)
                (:category :smoke)
                (:timeout 5)
                (incf amoebum/test::*i42-integration-tool-counter*)
                (format nil "value=~D" value)))
            (let* ((allow-context
                     (amoebum:make-amoebum-context
                      :toolset amoebum:*toolset*
                      :permission-mode :full-auto
                      :event-bus amoebum:*event-bus*))
                   (allow-call
                     (pseudopod:make-tool-call
                      :id "i42-allow"
                      :name "i42-suite-tool"
                      :arguments "{\"value\":41}"))
                   (allow-result (amoebum:execute-tool allow-call allow-context)))
              (is (string= allow-result "value=41"))
              (is (= *i42-integration-tool-counter* 1))
              (is (= pre-hook-count 1))
              (is (= post-hook-count 1))
              (is (= invoked-events 1))
              (is (= completed-events 1))
              (is (= prompted-events 0)))
            (let* ((deny-context
                     (amoebum:make-amoebum-context
                      :toolset amoebum:*toolset*
                      :permission-mode :supervised
                      :event-bus amoebum:*event-bus*))
                   (deny-call
                     (pseudopod:make-tool-call
                      :id "i42-deny"
                      :name "i42-suite-tool"
                      :arguments "{\"value\":99}")))
              (signals amoebum:tool-permission-denied
                (amoebum:execute-tool deny-call deny-context))
              (is (= *i42-integration-tool-counter* 1))
              (is (= pre-hook-count 1))
              (is (= post-hook-count 1))
              (is (= invoked-events 1))
              (is (= completed-events 1))
              (is (= prompted-events 1)))))
      (setf amoebum:*toolset* original-toolset
            amoebum:*tool-metadata* original-metadata
            amoebum:*hook-registry* original-hooks
            amoebum:*event-bus* original-event-bus
            amoebum:*permission-rules* original-rules))))

(defun run-all ()
  "Run all amoebum tests and return T when successful."
  (let ((results (run 'amoebum-suite)))
    (explain! results)
    (results-status results)))
