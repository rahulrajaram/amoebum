(defpackage :amoebum/test
  (:use :cl :fiveam)
  (:export #:run-all #:amoebum-suite))

(in-package :amoebum/test)

(def-suite amoebum-suite
  :description "Amoebum core smoke suite (I23-I62).")

(in-suite amoebum-suite)

(defparameter +core-smoke-scripts+
  '(("agents-smoke-test.lisp" "AMOEBUM_AGENTS_SMOKE_OK")
    ("chat-ui-smoke-test.lisp" "AMOEBUM_CHAT_UI_SMOKE_OK")
    ("commands-smoke-test.lisp" "AMOEBUM_COMMANDS_SMOKE_OK")
    ("conditions-smoke-test.lisp" "AMOEBUM_CONDITIONS_SMOKE_OK")
    ("config-smoke-test.lisp" "AMOEBUM_CONFIG_SMOKE_OK")
    ("conversation-smoke-test.lisp" "AMOEBUM_CONVERSATION_SMOKE_OK")
    ("context-budget-smoke-test.lisp" "AMOEBUM_CONTEXT_BUDGET_SMOKE_OK")
    ("context-smoke-test.lisp" "AMOEBUM_CONTEXT_SMOKE_OK")
    ("defhook-smoke-test.lisp" "AMOEBUM_DEFHOOK_SMOKE_OK")
    ("defkeys-smoke-test.lisp" "AMOEBUM_DEFKEYS_SMOKE_OK")
    ("deftool-smoke-test.lisp" "AMOEBUM_DEFTOOL_SMOKE_OK")
    ("events-smoke-test.lisp" "AMOEBUM_EVENTS_SMOKE_OK")
    ("file-tools-smoke-test.lisp" "AMOEBUM_FILE_TOOLS_SMOKE_OK")
    ("git-smoke-test.lisp" "AMOEBUM_GIT_SMOKE_OK")
    ("haake-smoke-test.lisp" "AMOEBUM_HAAKE_SMOKE_OK")
    ("haake-migration-smoke-test.lisp" "AMOEBUM_HAAKE_MIGRATION_SMOKE_OK")
    ("lsp-smoke-test.lisp" "AMOEBUM_LSP_SMOKE_OK")
    ("mcp-smoke-test.lisp" "AMOEBUM_MCP_SMOKE_OK")
    ("memory-smoke-test.lisp" "AMOEBUM_MEMORY_SMOKE_OK")
    ("notifications-smoke-test.lisp" "AMOEBUM_NOTIFICATIONS_SMOKE_OK")
    ("permissions-smoke-test.lisp" "AMOEBUM_PERMISSIONS_SMOKE_OK")
    ("pipeline-smoke-test.lisp" "AMOEBUM_PIPELINE_SMOKE_OK")
    ("plan-mode-smoke-test.lisp" "AMOEBUM_PLAN_MODE_SMOKE_OK")
    ("search-tools-smoke-test.lisp" "AMOEBUM_SEARCH_TOOLS_SMOKE_OK")
    ("shell-tool-smoke-test.lisp" "AMOEBUM_SHELL_TOOL_SMOKE_OK")
    ("status-bar-smoke-test.lisp" "AMOEBUM_STATUS_BAR_SMOKE_OK")
    ("streaming-smoke-test.lisp" "AMOEBUM_STREAMING_SMOKE_OK")
    ("system-prompt-smoke-test.lisp" "AMOEBUM_SYSPROMPT_SMOKE_OK")
    ("web-fetch-smoke-test.lisp" "AMOEBUM_WEB_FETCH_SMOKE_OK")
    ("web-search-smoke-test.lisp" "AMOEBUM_WEB_SEARCH_SMOKE_OK")))

(defparameter +phase3-required-smoke-scripts+
  '("context-smoke-test.lisp"
    "context-budget-smoke-test.lisp"
    "git-smoke-test.lisp"
    "status-bar-smoke-test.lisp"
    "file-tools-smoke-test.lisp"
    "mcp-smoke-test.lisp"
    "haake-smoke-test.lisp"
    "haake-migration-smoke-test.lisp"
    "lsp-smoke-test.lisp"
    "web-search-smoke-test.lisp"
    "web-fetch-smoke-test.lisp"
    "chat-ui-smoke-test.lisp"
    "conversation-smoke-test.lisp"
    "system-prompt-smoke-test.lisp"
    "notifications-smoke-test.lisp"))

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

(test i23-i62-subsystem-smokes-pass
  (let ((filenames (mapcar #'first +core-smoke-scripts+)))
    (is (>= (+ (* 3 (length +core-smoke-scripts+))
               (length +phase3-required-smoke-scripts+))
            100)
        "Phase-3 smoke coverage should contribute at least 100 baseline checks.")
    (dolist (required +phase3-required-smoke-scripts+)
      (is-true (member required filenames :test #'string=)
               "Expected Phase 3 smoke script ~A in suite coverage list."
               required)))
  (dolist (entry +core-smoke-scripts+)
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

(test integration-context-compression-updates-status-bar
  (let* ((bus (amoebum:make-event-bus :capacity 128))
         (compressed-events '()))
    (let ((amoebum::*event-bus* bus)
          (amoebum::*context-window-limit* 220))
      (amoebum:subscribe bus
                         amoebum:+event-type-context-compressed+
                         (lambda (event)
                           (push event compressed-events)))
      (let ((chat-state
              (amoebum:ensure-chat-ui-state
               (amoebum:make-chat-ui-state
                :stream-runner nil
                :status-bar-state
                (amoebum:make-status-bar-state :event-bus bus
                                               :model-name "moonshot-v1-8k"
                                               :branch-name "feature/i62")))))
        (loop for idx from 1 to 28 do
          (amoebum:chat-ui-add-message
           chat-state
           (if (oddp idx) "user" "assistant")
           (format nil
                   "I62 context flow message ~D with enough repeated detail to trigger token accounting and compression behavior."
                   idx)))
        (let* ((status-state (amoebum:chat-ui-state-status-bar-state chat-state))
               (payload (and compressed-events
                             (amoebum:event-payload (first compressed-events))))
               (first-message (first (amoebum:chat-ui-state-messages chat-state))))
          (is-true compressed-events
                   "Expected at least one context:compressed event from conversation growth.")
          (is (typep payload 'amoebum:context-compressed-payload))
          (is (> (amoebum:context-compressed-payload-before-tokens payload)
                 (amoebum:context-compressed-payload-after-tokens payload))
              "Expected compression to reduce token count.")
          (is (> (amoebum:context-compressed-payload-saved-tokens payload) 0)
              "Expected positive token savings in context compression payload.")
          (is (eq (amoebum:context-compressed-payload-trigger payload) :auto)
              "Expected auto compression trigger from conversation growth.")
          (is (= (amoebum:chat-ui-state-context-used-tokens chat-state)
                 (amoebum:status-bar-state-context-used-tokens status-state))
              "Expected status bar context usage to match chat state usage.")
          (is (search "Tokens:"
                      (amoebum:status-bar-line status-state)
                      :test #'char-equal)
              "Expected status bar line to include context token budget segment.")
          (is-true (and (pseudopod:message-p first-message)
                        (string= (pseudopod:message-role first-message) "system"))
                   "Expected compressed history summary message at the front of conversation."))))))

(test integration-git-tool-permission-and-event-flow
  (let* ((bus (amoebum:make-event-bus :capacity 128))
         (invoked-events 0)
         (completed-events 0)
         (prompted-events 0)
         (latest-prompted-payload nil)
         (config (amoebum:current-config))
         (old-project-root (amoebum:config-project-root config))
         (tmp-root (uiop:ensure-directory-pathname
                    (merge-pathnames
                     (make-pathname :directory
                                    `(:relative
                                      ,(format nil "amoebum-i62-git-~D-~D"
                                               (get-universal-time)
                                               (random 1000000))))
                     (uiop:ensure-directory-pathname (uiop:temporary-directory))))))
    (labels ((run-git (&rest args)
               (multiple-value-bind (stdout stderr exit-code)
                   (uiop:run-program (append (list "git") args)
                                     :directory tmp-root
                                     :ignore-error-status t
                                     :output :string
                                     :error-output :string)
                 (unless (zerop exit-code)
                   (error "git ~{~A~^ ~} failed in ~A: ~A~%~A"
                          args
                          (namestring tmp-root)
                          stdout
                          stderr))
                 stdout)))
      (unwind-protect
          (progn
            (ensure-directories-exist (merge-pathnames #P".keep" tmp-root))
            (run-git "init")
            (run-git "config" "user.name" "Amoebum I62 Smoke")
            (run-git "config" "user.email" "amoebum-i62-smoke@example.com")
            (with-open-file (stream (merge-pathnames #P"README.md" tmp-root)
                                    :direction :output
                                    :if-exists :supersede
                                    :if-does-not-exist :create
                                    :external-format :utf-8)
              (write-string "i62 git integration smoke fixture\n" stream))
            (run-git "add" "--" "README.md")
            (run-git "commit" "-m" "chore: seed i62 integration repo")
            (run-git "branch" "-m" "main")
            (amoebum:setconfig :project-root tmp-root)

            (amoebum:subscribe bus
                               amoebum:+event-type-tool-invoked+
                               (lambda (_event)
                                 (declare (ignore _event))
                                 (incf invoked-events)))
            (amoebum:subscribe bus
                               amoebum:+event-type-tool-completed+
                               (lambda (_event)
                                 (declare (ignore _event))
                                 (incf completed-events)))
            (amoebum:subscribe bus
                               amoebum:+event-type-permission-prompted+
                               (lambda (event)
                                 (setf latest-prompted-payload
                                       (amoebum:event-payload event))
                                 (incf prompted-events)))

            (let* ((allow-context
                     (amoebum:make-amoebum-context
                      :permission-mode :full-auto
                      :event-bus bus
                      :initialize-notifications-p nil))
                   (allow-call
                     (pseudopod:make-tool-call
                      :id "i62-git-allow"
                      :name "git-status"
                      :arguments "{}"))
                   (allow-result (amoebum:execute-tool allow-call allow-context)))
              (is-true (stringp allow-result)
                       "Expected git-status tool result to be serialized JSON text.")
              (is (search "\"BRANCH\"" allow-result :test #'char-equal)
                  "Expected git-status tool result JSON to include BRANCH field.")
              (is (search "\"TRACKING\"" allow-result :test #'char-equal)
                  "Expected git-status tool result JSON to include TRACKING payload.")
              (is (= invoked-events 1))
              (is (= completed-events 1))
              (is (= prompted-events 0)))

            (let* ((deny-context
                     (amoebum:make-amoebum-context
                      :permission-mode :supervised
                      :event-bus bus
                      :initialize-notifications-p nil))
                   (deny-call
                     (pseudopod:make-tool-call
                      :id "i62-git-deny"
                      :name "git-status"
                      :arguments "{}")))
              (signals amoebum:tool-permission-denied
                (amoebum:execute-tool deny-call deny-context))
              (is (= invoked-events 1)
                  "Denied git tool call should not emit tool:invoked.")
              (is (= completed-events 1)
                  "Denied git tool call should not emit tool:completed.")
              (is (= prompted-events 1)
                  "Expected denied git tool call to emit permission:prompted event.")
              (is (typep latest-prompted-payload 'amoebum:permission-prompted-payload))
              (is (string= (amoebum::permission-prompted-payload-tool-name
                            latest-prompted-payload)
                           "git-status"))
              (is (search "permission decision prompt"
                          (or (amoebum::permission-prompted-payload-reason
                               latest-prompted-payload)
                              "")
                          :test #'char-equal)
                  "Expected permission prompt reason to describe prompt decision.")))
        (amoebum:setconfig :project-root old-project-root)))))

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
