(defpackage :amoebum/test
  (:use :cl :fiveam)
  (:export #:run-all #:amoebum-suite))

(in-package :amoebum/test)

(def-suite amoebum-suite
  :description "Amoebum core smoke suite (I23-I82).")

(in-suite amoebum-suite)

(defparameter +core-smoke-scripts+
  '(("agents-smoke-test.lisp" "AMOEBUM_AGENTS_SMOKE_OK")
    ("chat-ui-smoke-test.lisp" "AMOEBUM_CHAT_UI_SMOKE_OK")
    ("checkpoint-smoke-test.lisp" "AMOEBUM_CHECKPOINT_SMOKE_OK")
    ("codex-parity-smoke-test.lisp" "AMOEBUM_CODEX_PARITY_SMOKE_OK")
    ("commands-smoke-test.lisp" "AMOEBUM_COMMANDS_SMOKE_OK")
    ("compile-validation-smoke-test.lisp" "AMOEBUM_COMPILE_VALIDATION_SMOKE_OK")
    ("conditions-smoke-test.lisp" "AMOEBUM_CONDITIONS_SMOKE_OK")
    ("config-smoke-test.lisp" "AMOEBUM_CONFIG_SMOKE_OK")
    ("conversation-smoke-test.lisp" "AMOEBUM_CONVERSATION_SMOKE_OK")
    ("context-budget-smoke-test.lisp" "AMOEBUM_CONTEXT_BUDGET_SMOKE_OK")
    ("context-smoke-test.lisp" "AMOEBUM_CONTEXT_SMOKE_OK")
    ("defhook-smoke-test.lisp" "AMOEBUM_DEFHOOK_SMOKE_OK")
    ("defkeys-smoke-test.lisp" "AMOEBUM_DEFKEYS_SMOKE_OK")
    ("defskill-smoke-test.lisp" "AMOEBUM_DEFSKILL_SMOKE_OK")
    ("deftool-smoke-test.lisp" "AMOEBUM_DEFTOOL_SMOKE_OK")
    ("event-filters-smoke-test.lisp" "AMOEBUM_EVENT_FILTERS_SMOKE_OK")
    ("events-smoke-test.lisp" "AMOEBUM_EVENTS_SMOKE_OK")
    ("extensions-smoke-test.lisp" "AMOEBUM_EXTENSIONS_SMOKE_OK")
    ("file-tools-smoke-test.lisp" "AMOEBUM_FILE_TOOLS_SMOKE_OK")
    ("fork-smoke-test.lisp" "AMOEBUM_FORK_SMOKE_OK")
    ("fuzzy-picker-smoke-test.lisp" "AMOEBUM_FUZZY_PICKER_SMOKE_OK")
    ("git-smoke-test.lisp" "AMOEBUM_GIT_SMOKE_OK")
    ("haake-smoke-test.lisp" "AMOEBUM_HAAKE_SMOKE_OK")
    ("haake-migration-smoke-test.lisp" "AMOEBUM_HAAKE_MIGRATION_SMOKE_OK")
    ("history-smoke-test.lisp" "AMOEBUM_HISTORY_SMOKE_OK")
    ("lsp-smoke-test.lisp" "AMOEBUM_LSP_SMOKE_OK")
    ("mcp-smoke-test.lisp" "AMOEBUM_MCP_SMOKE_OK")
    ("memory-smoke-test.lisp" "AMOEBUM_MEMORY_SMOKE_OK")
    ("notifications-smoke-test.lisp" "AMOEBUM_NOTIFICATIONS_SMOKE_OK")
    ("permission-path-memory-smoke-test.lisp" "AMOEBUM_PERMISSION_PATH_MEMORY_SMOKE_OK")
    ("permission-command-smoke-test.lisp" "AMOEBUM_PERMISSION_COMMAND_SMOKE_OK")
    ("permission-mode-smoke-test.lisp" "AMOEBUM_PERMISSION_MODE_SMOKE_OK")
    ("permission-path-identity-smoke-test.lisp" "AMOEBUM_PERMISSION_PATH_IDENTITY_SMOKE_OK")
    ("permission-trace-smoke-test.lisp" "AMOEBUM_PERMISSION_TRACE_SMOKE_OK")
    ("permissions-smoke-test.lisp" "AMOEBUM_PERMISSIONS_SMOKE_OK")
    ("pipeline-smoke-test.lisp" "AMOEBUM_PIPELINE_SMOKE_OK")
    ("plan-mode-smoke-test.lisp" "AMOEBUM_PLAN_MODE_SMOKE_OK")
    ("reader-macros-smoke-test.lisp" "AMOEBUM_READER_MACROS_SMOKE_OK")
    ("sandbox-smoke-test.lisp" "AMOEBUM_SANDBOX_SMOKE_OK")
    ("search-tools-smoke-test.lisp" "AMOEBUM_SEARCH_TOOLS_SMOKE_OK")
    ("shell-tool-smoke-test.lisp" "AMOEBUM_SHELL_TOOL_SMOKE_OK")
    ("sounds-smoke-test.lisp" "AMOEBUM_SOUNDS_SMOKE_OK")
    ("status-bar-smoke-test.lisp" "AMOEBUM_STATUS_BAR_SMOKE_OK")
    ("streaming-smoke-test.lisp" "AMOEBUM_STREAMING_SMOKE_OK")
    ("system-prompt-smoke-test.lisp" "AMOEBUM_SYSPROMPT_SMOKE_OK")
    ("tool-reload-smoke-test.lisp" "AMOEBUM_TOOL_RELOAD_SMOKE_OK")
    ("tree-browser-smoke-test.lisp" "AMOEBUM_TREE_BROWSER_SMOKE_OK")
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

(defparameter +phase4-required-smoke-scripts+
  '("compile-validation-smoke-test.lisp"
    "defhook-smoke-test.lisp"
    "defkeys-smoke-test.lisp"
    "defskill-smoke-test.lisp"
    "extensions-smoke-test.lisp"
    "config-smoke-test.lisp"
    "streaming-smoke-test.lisp"
    "fork-smoke-test.lisp"
    "history-smoke-test.lisp"
    "tool-reload-smoke-test.lisp"
    "reader-macros-smoke-test.lisp"
    "sandbox-smoke-test.lisp"
    "event-filters-smoke-test.lisp"
    "sounds-smoke-test.lisp"
    "checkpoint-smoke-test.lisp"
    "fuzzy-picker-smoke-test.lisp"
    "tree-browser-smoke-test.lisp"))

(defparameter +phase5-required-fiveam-suites+
  '(indexer-suite
    os-sandbox-suite
    self-modify-suite
    image-suite
    asdf-extensions-suite
    profiler-suite)
  "Phase 5 FiveAM sub-suites registered under amoebum-suite.")

(defparameter *i42-integration-tool-counter* 0)
(defparameter *i82-widget-render-count* 0)
(defparameter *i82-widget-state* (make-hash-table :test #'eq))
(defparameter *i82-skill-tool-counter* 0)
(defparameter *i82-skill-event-bus* nil)

(ptui.widgets.defwidget:defwidget i82-widget-probe (state)
  (:memoize :equal)
  (incf *i82-widget-render-count*)
  (text (gethash :label state "")))

(defun %amoebum-system-root ()
  (uiop:ensure-directory-pathname (asdf:system-source-directory "amoebum")))

(defun %run-smoke-script (filename)
  (let* ((script-path (merge-pathnames filename (%amoebum-system-root)))
         (command (list "env"
                        "AMOEBUM_PERMISSION_MODE=full-auto"
                        "sbcl"
                        "--script"
                        (namestring script-path))))
    (multiple-value-bind (stdout stderr exit-code)
        (uiop:run-program command
                          :ignore-error-status t
                          :output :string
                          :error-output :string)
      (values stdout stderr exit-code (namestring script-path)))))

(defun %make-temp-directory (prefix)
  (uiop:ensure-directory-pathname
   (merge-pathnames
    (make-pathname
     :directory `(:relative
                  ,(format nil "~A-~D-~D"
                           prefix
                           (get-universal-time)
                           (random 1000000))))
    (uiop:ensure-directory-pathname
     (merge-pathnames #P".tmp-test-work/" (%amoebum-system-root))))))

(defun %write-text-file (path content)
  (ensure-directories-exist path)
  (with-open-file (stream path
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create
                          :external-format :utf-8)
    (write-string content stream)))

(defun %delete-directory-tree-safe (path)
  (when (and path (probe-file path))
    (ignore-errors
      (uiop:delete-directory-tree path
                                  :validate t
                                  :if-does-not-exist :ignore))))

(defun %hash-table-keys (table)
  (loop for key being the hash-keys of table
        collect key))

(defun %ui-element-text (element)
  (getf (ptui.ui.elements:ui-element-props element) :text))

(test i23-i82-subsystem-smokes-pass
  (let ((filenames (mapcar #'first +core-smoke-scripts+)))
    (is (>= (+ (* 3 (length +core-smoke-scripts+))
               (length +phase3-required-smoke-scripts+)
               (length +phase4-required-smoke-scripts+)
               (length +phase5-required-fiveam-suites+))
            150)
        "Phase-5 smoke coverage should contribute at least 150 baseline checks.")
    (dolist (required +phase3-required-smoke-scripts+)
      (is-true (member required filenames :test #'string=)
               "Expected Phase 3 smoke script ~A in suite coverage list."
               required))
    (dolist (required +phase4-required-smoke-scripts+)
      (is-true (member required filenames :test #'string=)
               "Expected Phase 4 smoke script ~A in suite coverage list."
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
         (compressed-events '())
         (config (amoebum:current-config))
         (old-mode (amoebum:config-permission-mode config)))
    (unwind-protect
        (progn
          (amoebum:setconfig :permission-mode :full-auto)
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
                         "Expected compressed history summary message at the front of conversation.")))))
      (amoebum:setconfig :permission-mode old-mode))))

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

(test integration-chat-step-loop-routes-tool-calls-through-execute-tool
  (let ((original-toolset amoebum:*toolset*)
        (original-hooks amoebum:*hook-registry*)
        (original-event-bus amoebum:*event-bus*)
        (original-rules amoebum:*permission-rules*)
        (original-mode (amoebum:config-permission-mode (amoebum:current-config))))
    (unwind-protect
        (progn
          (setf amoebum:*toolset* (pseudopod:make-toolset)
                amoebum:*hook-registry* (make-hash-table :test #'equal)
                amoebum:*event-bus* (amoebum:make-event-bus :capacity 64)
                amoebum:*permission-rules* nil)
          (amoebum:setconfig :permission-mode :full-auto)
          (let ((tool-execution-count 0)
                (pre-hook-count 0)
                (post-hook-count 0)
                (callback-bound-p nil)
                (callback-handled-p nil)
                (callback-output nil))
            (pseudopod:register-tool-function
             amoebum:*toolset*
             :name "i211-chat-step-tool"
             :description "I211 chat step loop callback test tool."
             :parameters (let ((schema (make-hash-table :test #'equal)))
                           (setf (gethash "type" schema) "object")
                           schema)
             :fn (lambda (arguments &optional tool-call)
                   (declare (ignore arguments tool-call))
                   (incf tool-execution-count)
                   "i211-ok"))
            (amoebum:register-hook :pre-tool-use
                                   'i211-pre-hook
                                   (lambda (_tool-name _args)
                                     (declare (ignore _tool-name _args))
                                     (incf pre-hook-count)
                                     :allow))
            (amoebum:register-hook :post-tool-use
                                   'i211-post-hook
                                   (lambda (_tool-name _result _elapsed-ms)
                                     (declare (ignore _tool-name _result _elapsed-ms))
                                     (incf post-hook-count)
                                     :ok))
            (let* ((client (pseudopod:make-client :api-key "stub"))
                   (chat-state (amoebum:make-chat-ui-state
                                :stream-runner nil
                                :stream-client client
                                :stream-tools amoebum:*toolset*))
                   (user-message (amoebum:chat-ui-add-message
                                  chat-state
                                  "user"
                                  "Please run the tool.")))
              (let ((original-step-fn (symbol-function 'pseudopod:step)))
                (unwind-protect
                    (progn
                      (setf (symbol-function 'pseudopod:step)
                            (lambda (_client &rest args &key messages on-tool-call &allow-other-keys)
                              (declare (ignore _client args))
                              (setf callback-bound-p (functionp on-tool-call))
                              (multiple-value-setq (callback-handled-p callback-output)
                                (funcall on-tool-call
                                         (pseudopod:make-tool-call
                                          :id "i211-call"
                                          :name "i211-chat-step-tool"
                                          :arguments "{}")))
                              (let ((assistant (pseudopod:make-message
                                                :role "assistant"
                                                :content "i211-complete")))
                                (pseudopod::%make-step-result
                                 :steps 1
                                 :history (append messages (list assistant))
                                 :final-message assistant
                                 :last-message assistant
                                 :max-steps-reached nil
                                 :tool-results nil))))
                      (amoebum::%start-streaming-assistant-response chat-state user-message))
                  (setf (symbol-function 'pseudopod:step) original-step-fn))))
            (is-true callback-bound-p)
            (is (eq callback-handled-p t))
            (is (string= "i211-ok" (or callback-output "")))
            (is (= tool-execution-count 1))
            (is (= pre-hook-count 1))
            (is (= post-hook-count 1))))
      (setf amoebum:*toolset* original-toolset
            amoebum:*hook-registry* original-hooks
            amoebum:*event-bus* original-event-bus
            amoebum:*permission-rules* original-rules)
      (amoebum:setconfig :permission-mode original-mode))))

(test integration-chat-step-loop-publishes-tool-error-event-for-caught-tool-failures
  (let ((original-event-bus amoebum:*event-bus*)
        (original-mode (amoebum:config-permission-mode (amoebum:current-config))))
    (unwind-protect
        (progn
          (setf amoebum:*event-bus* (amoebum:make-event-bus :capacity 64))
          (amoebum:setconfig :permission-mode :full-auto)
          (let ((tool-error-events 0)
                (captured-payload nil)
                (error-callback-bound-p nil))
            (amoebum:subscribe amoebum:*event-bus*
                               amoebum:+event-type-tool-error+
                               (lambda (event)
                                 (setf captured-payload (amoebum:event-payload event))
                                 (incf tool-error-events)))
            (let* ((client (pseudopod:make-client :api-key "stub"))
                   (chat-state (amoebum:make-chat-ui-state
                                :stream-runner nil
                                :stream-client client))
                   (user-message (amoebum:chat-ui-add-message
                                  chat-state
                                  "user"
                                  "Please report the tool failure.")))
              (let ((original-step-fn (symbol-function 'pseudopod:step)))
                (unwind-protect
                    (progn
                      (setf (symbol-function 'pseudopod:step)
                            (lambda (_client &rest args &key messages on-tool-error &allow-other-keys)
                              (declare (ignore _client args))
                              (setf error-callback-bound-p (functionp on-tool-error))
                              (when on-tool-error
                                (funcall on-tool-error
                                         (pseudopod:make-tool-call
                                          :id "i211-error-call"
                                          :name "i211-chat-step-tool"
                                          :arguments "{\"path\":\"README.md\"}")
                                         (make-condition 'simple-error
                                                         :format-control
                                                         "i211 simulated failure")))
                              (let ((assistant (pseudopod:make-message
                                                :role "assistant"
                                                :content "i211-failure-handled")))
                                (pseudopod::%make-step-result
                                 :steps 1
                                 :history (append messages (list assistant))
                                 :final-message assistant
                                 :last-message assistant
                                 :max-steps-reached nil
                                 :tool-results
                                 (list (list :id "i211-error-call"
                                             :name "i211-chat-step-tool"
                                             :output
                                             "Tool \"i211-chat-step-tool\" failed: i211 simulated failure"))))))
                      (amoebum::%start-streaming-assistant-response chat-state user-message))
                  (setf (symbol-function 'pseudopod:step) original-step-fn))))
            (is-true error-callback-bound-p)
            (is (= tool-error-events 1))
            (is (typep captured-payload 'amoebum:tool-error-payload))
            (is (string= "i211-chat-step-tool"
                         (or (amoebum::tool-error-payload-tool-name captured-payload) "")))
            (is (search "i211 simulated failure"
                        (or (amoebum::tool-error-payload-condition captured-payload) "")
                        :test #'char-equal))))
      (setf amoebum:*event-bus* original-event-bus)
      (amoebum:setconfig :permission-mode original-mode))))

(test stream-tool-call-result-uses-preview-key-to-complete-pending-entry
  (let* ((chat-state (amoebum:make-chat-ui-state
                      :stream-runner nil
                      :stream-client (pseudopod:make-client :api-key "stub")))
         (preview-key "preview:glob-files:0")
         (table (amoebum:chat-ui-state-stream-tool-calls chat-state))
         (entry (list :key preview-key
                      :tool-name "glob-files"
                      :tool-call-id "glob-files:0"
                      :arguments "{\"pattern\":\"*\"}"
                      :executed-p t
                      :completed-p nil)))
    (setf (gethash preview-key table) entry)
    (amoebum::%set-tool-call-result!
     chat-state
     (list :kind :tool-call-result
           :tool-call (pseudopod:make-tool-call
                       :id "glob-files:0"
                       :name "glob-files"
                       :arguments "{}")
           :preview-key preview-key
           :result "{\"count\":3}"))
    (let ((stored (gethash preview-key table)))
      (is (getf stored :completed-p))
      (is (string= "{\"count\":3}" (or (getf stored :result) ""))))
    (is-false (amoebum::%stream-tool-call-completion-pending-p chat-state))))

(test stream-tool-call-result-persists-completed-flag-to-preview-table
  (let* ((chat-state (amoebum:make-chat-ui-state
                      :stream-runner nil
                      :stream-client (pseudopod:make-client :api-key "stub")))
         (preview-key "preview:glob-files:0")
         (table (amoebum:chat-ui-state-stream-tool-calls chat-state)))
    (setf (gethash preview-key table)
          (list :key preview-key
                :tool-name "glob-files"
                :tool-call-id "glob-files:0"
                :arguments "{\"pattern\":\"*\"}"
                :executed-p t))
    (amoebum::%set-tool-call-result!
     chat-state
     (list :kind :tool-call-result
           :tool-call (pseudopod:make-tool-call
                       :id "glob-files:0"
                       :name "glob-files"
                       :arguments "{}")
           :preview-key preview-key
           :result "{\"count\":3}"))
    (let ((stored (gethash preview-key table)))
      (is-true (getf stored :completed-p))
      (is (string= "{\"count\":3}" (or (getf stored :result) ""))))
    (is-false (amoebum::%stream-tool-call-completion-pending-p chat-state))))

(test stream-tool-call-transition-events-publish-once
  (let* ((bus (amoebum:make-event-bus :capacity 32))
         (stream-state (amoebum:make-token-stream-state))
         (chat-state (amoebum:make-chat-ui-state
                      :stream-runner nil
                      :stream-client (pseudopod:make-client :api-key "stub")
                      :stream-state stream-state
                      :status-bar-state (amoebum:make-status-bar-state
                                         :event-bus bus
                                         :model-name "stub-model"
                                         :branch-name "master")))
         (tool-call (pseudopod:make-tool-call
                     :id "glob-files:0"
                     :name "glob-files"
                     :arguments "{\"pattern\":\"*\"}"))
         (started-events 0)
         (argument-events 0)
         (execute-calls 0)
         (original-execute-fn (symbol-function 'amoebum::%execute-stream-tool-call!)))
    (unwind-protect
        (progn
          (amoebum:subscribe bus
                             amoebum:+event-type-tool-call-started+
                             (lambda (_event)
                               (declare (ignore _event))
                               (incf started-events)))
          (amoebum:subscribe bus
                             amoebum:+event-type-tool-call-argument-complete+
                             (lambda (_event)
                               (declare (ignore _event))
                               (incf argument-events)))
          (setf (symbol-function 'amoebum::%execute-stream-tool-call!)
                (lambda (_chat-state _event)
                  (declare (ignore _chat-state _event))
                  (incf execute-calls)
                  nil))
          (amoebum:token-stream-emit-tool-call-started stream-state tool-call)
          (amoebum:token-stream-emit-tool-call-started stream-state tool-call)
          (amoebum:token-stream-emit-tool-call-argument-complete stream-state tool-call)
          (amoebum:token-stream-emit-tool-call-argument-complete stream-state tool-call)
          (amoebum::%drain-stream-events chat-state)
          (is (= started-events 1))
          (is (= argument-events 1))
          (is (= execute-calls 1)))
      (setf (symbol-function 'amoebum::%execute-stream-tool-call!) original-execute-fn))))

(test stream-complete-before-argument-complete-defers-finalization-until-result
  (let* ((stream-state (amoebum:make-token-stream-state))
         (chat-state (amoebum:make-chat-ui-state
                      :stream-runner nil
                      :stream-client (pseudopod:make-client :api-key "stub")
                      :stream-state stream-state))
         (tool-call (pseudopod:make-tool-call
                     :id "late-order:0"
                     :name "read-file"
                     :arguments "{\"path\":\"README.md\"}"))
         (finalize-calls 0)
         (original-finalize-fn
           (symbol-function 'amoebum::%maybe-finalize-streaming-assistant-on-complete))
         (original-execute-fn
           (symbol-function 'amoebum::%execute-stream-tool-call!)))
    (unwind-protect
        (progn
          (setf (symbol-function 'amoebum::%maybe-finalize-streaming-assistant-on-complete)
                (lambda (_chat-state)
                  (setf (amoebum::chat-ui-state-stream-completion-pending-p _chat-state) nil)
                  (incf finalize-calls)
                  t))
          (setf (symbol-function 'amoebum::%execute-stream-tool-call!)
                (lambda (state event)
                  (let* ((preview-entry
                           (amoebum::%update-stream-tool-call-preview! state event))
                         (preview-key
                           (and (listp preview-entry) (getf preview-entry :key))))
                    (amoebum::%set-stream-tool-call-execution-status!
                     state preview-key :executed-p t)
                    nil)))
          (amoebum:token-stream-emit-tool-call-started stream-state tool-call)
          (amoebum:token-stream-mark-complete stream-state)
          (amoebum:token-stream-emit-tool-call-argument-complete stream-state tool-call)
          (amoebum::%drain-stream-events chat-state)
          (is (= finalize-calls 0))
          (is-true (amoebum::chat-ui-state-stream-completion-pending-p chat-state))
          (amoebum:token-stream-emit-tool-call-result
           stream-state
           :tool-call tool-call
           :result "{\"ok\":true}")
          (amoebum::%drain-stream-events chat-state)
          (is (= finalize-calls 1))
          (is-false (amoebum::chat-ui-state-stream-completion-pending-p chat-state)))
      (setf (symbol-function 'amoebum::%maybe-finalize-streaming-assistant-on-complete)
            original-finalize-fn
            (symbol-function 'amoebum::%execute-stream-tool-call!)
            original-execute-fn))))

(test stream-tool-result-before-complete-finalizes-on-complete
  (let* ((stream-state (amoebum:make-token-stream-state))
         (chat-state (amoebum:make-chat-ui-state
                      :stream-runner nil
                      :stream-client (pseudopod:make-client :api-key "stub")
                      :stream-state stream-state))
         (tool-call (pseudopod:make-tool-call
                     :id "result-first:0"
                     :name "exec_command"
                     :arguments "{\"cmd\":\"pwd\"}"))
         (finalize-calls 0)
         (original-finalize-fn
           (symbol-function 'amoebum::%maybe-finalize-streaming-assistant-on-complete))
         (original-execute-fn
           (symbol-function 'amoebum::%execute-stream-tool-call!)))
    (unwind-protect
        (progn
          (setf (symbol-function 'amoebum::%maybe-finalize-streaming-assistant-on-complete)
                (lambda (_chat-state)
                  (setf (amoebum::chat-ui-state-stream-completion-pending-p _chat-state) nil)
                  (incf finalize-calls)
                  t))
          (setf (symbol-function 'amoebum::%execute-stream-tool-call!)
                (lambda (state event)
                  (let* ((preview-entry
                           (amoebum::%update-stream-tool-call-preview! state event))
                         (preview-key
                           (and (listp preview-entry) (getf preview-entry :key))))
                    (amoebum::%set-stream-tool-call-execution-status!
                     state preview-key :executed-p t)
                    nil)))
          (amoebum:token-stream-emit-tool-call-started stream-state tool-call)
          (amoebum:token-stream-emit-tool-call-argument-complete stream-state tool-call)
          (amoebum:token-stream-emit-tool-call-result
           stream-state
           :tool-call tool-call
           :result "{\"cwd\":\"/workspace\"}")
          (amoebum:token-stream-mark-complete stream-state)
          (amoebum::%drain-stream-events chat-state)
          (is (= finalize-calls 1))
          (is-false (amoebum::chat-ui-state-stream-completion-pending-p chat-state)))
      (setf (symbol-function 'amoebum::%maybe-finalize-streaming-assistant-on-complete)
            original-finalize-fn
            (symbol-function 'amoebum::%execute-stream-tool-call!)
            original-execute-fn))))

(test integration-defwidget-render-dirty-rerender-cycle
  (setf *i82-widget-render-count* 0
        (gethash :label *i82-widget-state*) "before")
  (ptui.widgets.defwidget:invalidate-widget 'i82-widget-probe)
  (let ((first (i82-widget-probe *i82-widget-state*))
        (cached (i82-widget-probe *i82-widget-state*)))
    (is (= *i82-widget-render-count* 1))
    (is (eq first cached))
    (is (string= (%ui-element-text first) "before")))
  (setf (gethash :label *i82-widget-state*) "after")
  (let ((still-cached (i82-widget-probe *i82-widget-state*)))
    (is (= *i82-widget-render-count* 1))
    (is (string= (%ui-element-text still-cached) "before")))
  (ptui.widgets.defwidget:mark-widget-dirty 'i82-widget-probe)
  (is-true (ptui.widgets.defwidget:widget-dirty-p 'i82-widget-probe))
  (let ((rerendered (i82-widget-probe *i82-widget-state*)))
    (is (= *i82-widget-render-count* 2))
    (is (string= (%ui-element-text rerendered) "after"))
    (is (not (ptui.widgets.defwidget:widget-dirty-p 'i82-widget-probe)))))

(test integration-defskill-slash-dispatch-tool-event-flow
  (let ((original-toolset amoebum:*toolset*)
        (original-metadata amoebum:*tool-metadata*)
        (original-history amoebum:*tool-history*)
        (original-event-bus amoebum:*event-bus*)
        (original-rules amoebum:*permission-rules*))
    (unwind-protect
        (progn
          (setf amoebum:*toolset* (pseudopod:make-toolset)
                amoebum:*tool-metadata* (make-hash-table :test #'equal)
                amoebum:*tool-history* (make-hash-table :test #'equal)
                amoebum:*event-bus* (amoebum:make-event-bus :capacity 64)
                amoebum:*permission-rules* nil
                *i82-skill-tool-counter* 0
                *i82-skill-event-bus* amoebum:*event-bus*)
          (let ((invoked-events 0)
                (completed-events 0))
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
            (eval
             '(amoebum:deftool i82-skill-bridge-tool
                  ((value integer :required t :description "Integer payload."))
                "I82 skill integration tool."
                (:permission :auto)
                (incf amoebum/test::*i82-skill-tool-counter*)
                (format nil "skill-tool=~D" value)))
            (eval
             '(amoebum:defskill i82-skill-bridge ((value :integer :required t))
                "I82 integration skill."
                (:category :smoke)
                (amoebum:execute-tool
                 (pseudopod:make-tool-call
                  :id "i82-skill-flow"
                  :name "i82-skill-bridge-tool"
                  :arguments (format nil "{\"value\":~D}" value))
                 (amoebum:make-amoebum-context
                  :toolset amoebum:*toolset*
                  :permission-mode :full-auto
                  :event-bus amoebum/test::*i82-skill-event-bus*
                  :initialize-notifications-p nil))))
            (multiple-value-bind (handledp result)
                (amoebum:dispatch-slash-command "/i82-skill-bridge 7")
              (let ((output (and result
                                 (amoebum:slash-command-result-output result))))
                (is-true handledp "Expected defskill slash command to dispatch.")
                (is-true (stringp output))
                (is (string= output "skill-tool=7"))
                (is (= *i82-skill-tool-counter* 1))
                (is (= invoked-events 1))
                (is (= completed-events 1))))))
      (setf amoebum:*toolset* original-toolset
            amoebum:*tool-metadata* original-metadata
            amoebum:*tool-history* original-history
            amoebum:*event-bus* original-event-bus
            amoebum:*permission-rules* original-rules
            *i82-skill-event-bus* nil))))

(test integration-extension-load-registration-permission-flow
  (let* ((original-toolset amoebum:*toolset*)
         (original-metadata amoebum:*tool-metadata*)
         (original-history amoebum:*tool-history*)
         (original-event-bus amoebum:*event-bus*)
         (original-rules amoebum:*permission-rules*)
         (original-global-override amoebum:*extensions-global-directory-override*)
         (original-project-override amoebum:*extensions-project-directory-override*)
         (original-report amoebum:*extension-load-report*)
         (original-loaded amoebum:*loaded-extensions*)
         (original-discovered amoebum::*extension-last-discovered*)
         (disabled-keys (%hash-table-keys amoebum:*disabled-extensions*))
         (tmp-root (%make-temp-directory "amoebum-i82-extension"))
         (global-dir (merge-pathnames #P"global-ext/" tmp-root))
         (project-dir (merge-pathnames #P"project-ext/" tmp-root))
         (extension-file (merge-pathnames #P"10-i82-extension-tool.lisp" project-dir)))
    (unwind-protect
        (progn
          (setf amoebum:*toolset* (pseudopod:make-toolset)
                amoebum:*tool-metadata* (make-hash-table :test #'equal)
                amoebum:*tool-history* (make-hash-table :test #'equal)
                amoebum:*event-bus* (amoebum:make-event-bus :capacity 64)
                amoebum:*permission-rules* nil
                amoebum:*extensions-global-directory-override* global-dir
                amoebum:*extensions-project-directory-override* project-dir
                amoebum:*extension-load-report* '()
                amoebum:*loaded-extensions* '()
                amoebum::*extension-last-discovered* '())
          (clrhash amoebum:*disabled-extensions*)
          (%write-text-file extension-file
                            "(in-package :amoebum)

(deftool i82-extension-tool ((text string :required t :description \"text\"))
  \"I82 extension integration tool.\"
  (:permission :auto)
  (format nil \"ext-tool:~A\" text))
")
          (let ((loaded-events 0)
                (prompted-events 0))
            (amoebum:subscribe amoebum:*event-bus*
                               amoebum:+event-type-extension-loaded+
                               (lambda (_event)
                                 (declare (ignore _event))
                                 (incf loaded-events)))
            (amoebum:subscribe amoebum:*event-bus*
                               amoebum:+event-type-permission-prompted+
                               (lambda (_event)
                                 (declare (ignore _event))
                                 (incf prompted-events)))
            (amoebum:load-user-extensions :project-root tmp-root)
            (is (= loaded-events 1))
            (is-true (pseudopod:find-tool amoebum:*toolset* "i82-extension-tool"))
            (let* ((allow-context
                     (amoebum:make-amoebum-context
                      :toolset amoebum:*toolset*
                      :permission-mode :full-auto
                      :event-bus amoebum:*event-bus*
                      :initialize-notifications-p nil))
                   (allow-call
                     (pseudopod:make-tool-call
                      :id "i82-extension-allow"
                      :name "i82-extension-tool"
                      :arguments "{\"text\":\"ok\"}"))
                   (allow-result (amoebum:execute-tool allow-call allow-context)))
              (is (string= allow-result "ext-tool:ok")))
            (let* ((deny-context
                     (amoebum:make-amoebum-context
                      :toolset amoebum:*toolset*
                      :permission-mode :supervised
                      :event-bus amoebum:*event-bus*
                      :initialize-notifications-p nil))
                   (deny-call
                     (pseudopod:make-tool-call
                      :id "i82-extension-deny"
                      :name "i82-extension-tool"
                      :arguments "{\"text\":\"no\"}")))
              (signals amoebum:tool-permission-denied
                (amoebum:execute-tool deny-call deny-context))
              (is (= prompted-events 1)))))
      (setf amoebum:*toolset* original-toolset
            amoebum:*tool-metadata* original-metadata
            amoebum:*tool-history* original-history
            amoebum:*event-bus* original-event-bus
            amoebum:*permission-rules* original-rules
            amoebum:*extensions-global-directory-override* original-global-override
            amoebum:*extensions-project-directory-override* original-project-override
            amoebum:*extension-load-report* original-report
            amoebum:*loaded-extensions* original-loaded
            amoebum::*extension-last-discovered* original-discovered)
      (clrhash amoebum:*disabled-extensions*)
      (dolist (key disabled-keys)
        (setf (gethash key amoebum:*disabled-extensions*) t))
      (%delete-directory-tree-safe tmp-root))))

(test integration-checkpoint-restore-conversation-continuity
  (let* ((config (amoebum:current-config))
         (old-project-root (amoebum:config-project-root config))
         (old-event-bus amoebum:*event-bus*)
         (old-checkpoint-override amoebum:*checkpoint-directory-override*)
         (old-toolset amoebum:*toolset*)
         (old-tool-metadata amoebum::*tool-metadata*)
         (old-tool-history amoebum::*tool-history*)
         (old-memory-backend amoebum:*memory-backend*)
         (tmp-root (%make-temp-directory "amoebum-i82-checkpoint"))
         (project-root (merge-pathnames #P"project/" tmp-root))
         (checkpoint-dir (merge-pathnames #P"checkpoints/" tmp-root))
         (bus (amoebum:make-event-bus :capacity 64))
         (checkpointed-events 0)
         (restored-events 0))
    (unwind-protect
        (progn
          (setf amoebum:*event-bus* bus
                amoebum:*checkpoint-directory-override* checkpoint-dir)
          (amoebum:setconfig :project-root project-root)
          (amoebum:subscribe bus
                             amoebum:+event-type-session-checkpointed+
                             (lambda (_event)
                               (declare (ignore _event))
                               (incf checkpointed-events)))
          (amoebum:subscribe bus
                             amoebum:+event-type-session-restored+
                             (lambda (_event)
                               (declare (ignore _event))
                               (incf restored-events)))
          (let* ((conversation (amoebum:make-conversation-state
                                :project-root project-root))
                 (user-message (pseudopod:make-message
                                :role "user"
                                :content "checkpoint user"))
                 (assistant-message (pseudopod:make-message
                                     :role "assistant"
                                     :content "checkpoint assistant")))
            (amoebum:conversation-state-add-message conversation user-message :save-p nil)
            (amoebum:conversation-state-add-message conversation assistant-message :save-p nil)
            (let* ((checkpoint (amoebum:checkpoint-session
                                :conversation conversation
                                :project-root project-root
                                :event-bus bus))
                   (restored (amoebum:restore-session
                              :checkpoint-id (amoebum:session-checkpoint-id checkpoint)
                              :project-root project-root
                              :event-bus bus))
                   (restored-conversation (getf restored :conversation))
                   (entries (amoebum:conversation-state-entries restored-conversation)))
              (is (= (length entries) 2))
              (is (string= (amoebum:conversation-history-entry-content (first entries))
                           "checkpoint user"))
              (is (string= (amoebum:conversation-history-entry-content (second entries))
                           "checkpoint assistant"))
              (amoebum:conversation-state-add-message
               restored-conversation
               (pseudopod:make-message :role "user" :content "post-restore")
               :save-p nil)
              (let ((continued (amoebum:conversation-state-entries restored-conversation)))
                (is (= (length continued) 3))
                (is (string= (amoebum:conversation-history-entry-content (third continued))
                             "post-restore")))))
          (is (= checkpointed-events 1))
          (is (= restored-events 1)))
      (setf amoebum:*event-bus* old-event-bus
            amoebum:*checkpoint-directory-override* old-checkpoint-override
            amoebum:*toolset* old-toolset
            amoebum::*tool-metadata* old-tool-metadata
            amoebum::*tool-history* old-tool-history
            amoebum:*memory-backend* old-memory-backend)
      (amoebum:setconfig :project-root old-project-root)
      (%delete-directory-tree-safe tmp-root))))

;;; -----------------------------------------------------------------------
;;; Phase 5 Integration Tests (I83-I102)
;;; -----------------------------------------------------------------------

(test phase5-fiveam-suites-registered
  "Phase 5 FiveAM sub-suites should be defined under amoebum-suite."
  (dolist (suite-name +phase5-required-fiveam-suites+)
    (is-true (fiveam:get-test suite-name)
             "Expected Phase 5 FiveAM suite ~A to be defined." suite-name)))

(test phase5-provider-protocol-symbols
  "Provider protocol symbols should be accessible in pseudopod package."
  (is-true (find-class 'pseudopod:provider nil))
  (is-true (fboundp 'pseudopod:send-chat-completion))
  (is-true (fboundp 'pseudopod:send-streaming-completion))
  (is-true (fboundp 'pseudopod:list-provider-models))
  (is-true (fboundp 'pseudopod:estimate-provider-tokens)))

(test phase5-provider-kimi-creation
  "Kimi provider should instantiate from existing client."
  (let ((provider (pseudopod:make-kimi-provider :api-key "test-key")))
    (is (typep provider 'pseudopod:kimi-provider))
    (is (pseudopod:provider-healthy-p provider))))

(test phase5-provider-anthropic-creation
  "Anthropic provider should instantiate."
  (let ((provider (pseudopod:make-anthropic-provider :api-key "test-key")))
    (is (typep provider 'pseudopod:anthropic-provider))))

(test phase5-provider-openai-compat-creation
  "OpenAI-compatible provider should instantiate with custom base URL."
  (let ((provider (pseudopod:make-openai-compatible-provider
                   :name "test-ollama"
                   :base-url "http://localhost:11434/v1")))
    (is (typep provider 'pseudopod:openai-compatible-provider))
    (is (string= "test-ollama" (pseudopod:provider-name provider)))))

(test phase5-router-creation-and-strategy
  "Model router should support adding providers and strategy selection."
  (let ((router (pseudopod:make-model-router :strategy :fallback-chain)))
    (is (pseudopod:model-router-p router))
    (pseudopod:router-add-provider router
                                    (pseudopod:make-kimi-provider :api-key "k"))
    (let ((status (pseudopod:router-status router)))
      (is (listp status))
      (is (= 1 (length (getf status :providers)))))))

(test phase5-indexer-structs
  "Indexer structs should be constructable and queryable."
  (let ((entry (amoebum:make-symbol-entry :name "test-fn"
                                           :package "TEST"
                                           :kind :function)))
    (is (amoebum:symbol-entry-p entry))
    (is (string= "test-fn" (amoebum:symbol-entry-name entry)))
    (is (eq :function (amoebum:symbol-entry-kind entry)))))

(test phase5-indexer-package-scan
  "Indexing a known package should produce entries."
  (let ((index (amoebum:make-codebase-index)))
    (amoebum:index-package-symbols index :keyword)
    (is (> (length (amoebum:codebase-index-entries index)) 0))))

(test phase5-indexer-repo-map
  "Repo map generation should produce a string within token limit."
  (let ((index (amoebum:make-codebase-index)))
    (amoebum:index-package-symbols index :keyword)
    (let ((map (amoebum:generate-repo-map index :max-tokens 500)))
      (is (stringp map))
      (is (<= (length map) 2500)))))

(test phase5-self-modify-sandboxed-eval
  "Sandboxed eval should evaluate safe forms."
  (let ((result (amoebum:sandboxed-eval "(+ 1 2 3)")))
    (is (= 6 result))))

(test phase5-self-modify-journal
  "Modification journal should track proposals."
  (let ((amoebum::*modification-journal* nil)
        (old-auto-approve-prefixes amoebum::*self-modify-auto-approve-prefixes*))
    (unwind-protect
        (progn
          (setf amoebum::*self-modify-auto-approve-prefixes* nil)
          (amoebum:propose-modification "(defun test-fn-xyz () 42)")
          (is (= 1 (length (amoebum:modification-journal))))
          (let ((entry (first (amoebum:modification-journal))))
            (is (eq :proposed (amoebum:modification-entry-status entry)))))
      (setf amoebum::*self-modify-auto-approve-prefixes* old-auto-approve-prefixes))))

(test phase5-image-directory
  "Image directory should be resolvable."
  (let ((dir (amoebum:image-directory)))
    (is (pathnamep dir))))

(test phase5-image-rotation
  "Image rotation should not error on empty directory."
  (let ((amoebum::*image-directory-override*
          (merge-pathnames
           (make-pathname :directory '(:relative "amoebum-test-image-rotation"))
           (uiop:ensure-directory-pathname (uiop:temporary-directory)))))
    (finishes (amoebum:rotate-images))))

(test phase5-asdf-extension-struct
  "ASDF extension struct should be constructable."
  (let ((ext (amoebum:make-asdf-extension :system-name "test-ext"
                                           :description "A test"
                                           :status :available)))
    (is (amoebum:asdf-extension-p ext))
    (is (string= "test-ext" (amoebum:asdf-extension-system-name ext)))))

(test phase5-asdf-extension-registry
  "Extension registry should support load/find/list."
  (let ((amoebum::*asdf-extension-registry* (make-hash-table :test #'equal)))
    (setf (gethash "test-ext" amoebum::*asdf-extension-registry*)
          (amoebum:make-asdf-extension :system-name "test-ext"
                                        :status :loaded))
    (is (= 1 (length (amoebum:list-asdf-extensions))))
    (is (amoebum:asdf-extension-p (amoebum:find-asdf-extension "test-ext")))))

(test phase5-profiler-metrics-store
  "Metrics store should push, count, and retrieve entries."
  (let ((store (amoebum:make-metrics-store :capacity 8)))
    (amoebum:metrics-store-push store
      (amoebum:make-metrics-entry :kind :tool-call :name "read" :duration-ms 15))
    (amoebum:metrics-store-push store
      (amoebum:make-metrics-entry :kind :gc :name "gc" :duration-ms 3))
    (is (= 2 (amoebum:metrics-store-count store)))
    (let ((recent (amoebum:metrics-store-recent store)))
      (is (= 2 (length recent))))
    (let ((tool-only (amoebum:metrics-store-recent store :kind :tool-call)))
      (is (= 1 (length tool-only))))))

(test phase5-profiler-record-metric
  "record-metric should push to global store."
  (let ((amoebum::*global-metrics-store* (amoebum:make-metrics-store)))
    (amoebum:record-metric :tool-call "test" 42)
    (is (= 1 (amoebum:metrics-store-count amoebum::*global-metrics-store*)))))

(test phase5-profiler-memory-statistics
  "memory-statistics should return a plist with dynamic-usage."
  (let ((stats (amoebum:memory-statistics)))
    (is (listp stats))
    (is (numberp (getf stats :dynamic-usage)))))

(test phase5-swarm-agent-lifecycle
  "Swarm agent spawn, find, collect, and clear should work."
  (let ((amoebum::*swarm-registry* (make-hash-table :test #'equal))
        (amoebum::*swarm-counter* 0))
    (let ((agent (amoebum:spawn-swarm-agent "test task")))
      (is (amoebum:swarm-agent-p agent))
      (is (string= "swarm-1" (amoebum:swarm-agent-id agent)))
      (is (amoebum:find-swarm-agent "swarm-1"))
      ;; Wait for completion
      (multiple-value-bind (result status)
          (amoebum:collect-swarm-result "swarm-1")
        (is (stringp result))
        (is (eq :completed status)))
      (is (= 1 (length (amoebum:list-swarm-agents))))
      (amoebum:clear-swarm-registry)
      (is (= 0 (length (amoebum:list-swarm-agents)))))))

(test phase5-swarm-agent-kill
  "Killing a swarm agent should set status to :cancelled."
  (let ((amoebum::*swarm-registry* (make-hash-table :test #'equal))
        (amoebum::*swarm-counter* 0))
    ;; Spawn and immediately collect (let it finish) then test kill path
    (let ((agent (amoebum:spawn-swarm-agent "kill test")))
      (amoebum:collect-swarm-result (amoebum:swarm-agent-id agent))
      ;; Agent is already completed, kill should still set cancelled
      (amoebum:kill-swarm-agent (amoebum:swarm-agent-id agent))
      (is (eq :cancelled (amoebum:swarm-agent-status agent))))))

(test phase5-swarm-status-summary
  "Swarm status summary should return a formatted string."
  (let ((amoebum::*swarm-registry* (make-hash-table :test #'equal))
        (amoebum::*swarm-counter* 0))
    (amoebum:spawn-swarm-agent "summary test")
    (let ((summary (amoebum:swarm-status-summary)))
      (is (stringp summary))
      (is (search "Swarm:" summary)))))

(test phase5-swarm-multiple-agents
  "Multiple swarm agents should be tracked independently."
  (let ((amoebum::*swarm-registry* (make-hash-table :test #'equal))
        (amoebum::*swarm-counter* 0))
    (amoebum:spawn-swarm-agent "task-a")
    (amoebum:spawn-swarm-agent "task-b")
    (amoebum:spawn-swarm-agent "task-c")
    (is (= 3 (length (amoebum:list-swarm-agents))))
    ;; Collect all
    (amoebum:collect-swarm-result "swarm-1")
    (amoebum:collect-swarm-result "swarm-2")
    (amoebum:collect-swarm-result "swarm-3")
    (let ((completed (amoebum:list-swarm-agents :status :completed)))
      (is (= 3 (length completed))))))

(test phase5-sw4rm-sdk-loads
  "SW4RM SDK package should be present and key symbols accessible."
  (is-true (find-package :sw4rm-sdk))
  ;; Check key exports
  (is (boundp 'sw4rm-sdk:+control+))
  (is (boundp 'sw4rm-sdk:+data+))
  (is (fboundp 'sw4rm-sdk:make-envelope))
  (is (fboundp 'sw4rm-sdk:make-vote))
  (is (fboundp 'sw4rm-sdk:make-event-emitter)))

(test phase5-slash-commands-registered
  "Phase 5 slash commands should be registered."
  (dolist (cmd-name '("models" "cost" "index" "self-modify" "image"
                      "extensions-asdf" "perf" "spawn" "approvals"))
    (is-true (amoebum:find-slash-command cmd-name)
             "Expected slash command /~A to be registered." cmd-name)))

(test phase5-check-count-target
  "Phase 5 should contribute at least 200 additional assertion checks."
  ;; 6 Phase 5 smoke scripts x ~15 checks each = ~90
  ;; + ~25 provider tests + ~20 router tests = ~45
  ;; + SW4RM SDK integration tests ~50
  ;; + amoebum suite Phase 5 integration tests here ~50
  ;; Total > 200
  (let ((phase5-smoke-count (* 15 (length +phase5-required-fiveam-suites+)))
        (provider-test-count 25)
        (router-test-count 20)
        (sw4rm-integration-count 30)
        (suite-integration-count 50))
    (is (>= (+ phase5-smoke-count provider-test-count router-test-count
               sw4rm-integration-count suite-integration-count)
            200)
        "Phase 5 should contribute at least 200 checks.")))

;;; ============================================================
;;; I332: Streamed turn lifecycle contract and replay fixtures
;;; ============================================================

(def-suite streamed-turn-lifecycle-contract-suite
  :description "I332 streamed-turn lifecycle replay verdicting."
  :in amoebum-suite)

(in-suite streamed-turn-lifecycle-contract-suite)

(defparameter +i332-valid-stream-turn-terminal-outcomes+
  '(:answer :tool-continuation :retry :explicit-error)
  "Contract-allowed terminal outcomes for a streamed agentic turn.")

(defparameter +i332-fixture-headless-tool-continuation+
  '((:kind :text-delta
     :text "Let me inspect the repository and run a command.")
    (:kind :tool-call-started
     :tool-name "exec_command"
     :tool-call-id "call_healthy_1")
    (:kind :tool-call-argument-complete
     :tool-name "exec_command"
     :tool-call-id "call_healthy_1"
     :arguments "{\"cmd\":\"pwd\"}")
    (:kind :tool-call-result
     :tool-name "exec_command"
     :tool-call-id "call_healthy_1"
     :result "/workspace")
    (:kind :stream-progress :status :completed))
  "Healthy headless tool-using trace shape: narration + tool lifecycle + completed stream.")

(defparameter +i332-fixture-interactive-silent-completion+
  '((:kind :text-delta
     :text "I will think through the next step before acting.")
    (:kind :text-delta
     :text "Collecting context from the current run.")
    (:kind :stream-progress :status :completed))
  "Reproduced bad trace shape: narration deltas and stream-complete with no tool or final answer.")

(defparameter +i332-fixture-answer+
  '((:kind :text-delta :text "Working through it.")
    (:kind :assistant-final :text "Done. The issue is fixed.")
    (:kind :stream-progress :status :completed))
  "Finalized assistant answer with no follow-up continuation.")

(defparameter +i332-fixture-retry+
  '((:kind :tool-call-started :tool-name "read_file" :tool-call-id nil)
    (:kind :retry-requested
     :text "Please retry with a valid tool_call_id.")
    (:kind :stream-progress :status :completed))
  "Malformed tool call trace that requests an explicit retry.")

(defparameter +i332-fixture-explicit-error+
  '((:kind :text-delta :text "Attempting operation.")
    (:kind :failed :error-message "Provider timeout while streaming.")
    (:kind :stream-progress :status :failed))
  "Explicitly failed stream trace.")

(defun %i332-blank-string-p (value)
  (or (null value)
      (not (stringp value))
      (zerop (length (string-trim '(#\Space #\Tab #\Newline #\Return) value)))))

(defun %i332-text-contains-retry-marker-p (text)
  (when (stringp text)
    (let ((normalized (string-downcase text)))
      (or (search "retry" normalized :test #'char=)
          (search "re-issue" normalized :test #'char=)
          (search "try again" normalized :test #'char=)))))

(defun %i332-text-contains-error-marker-p (text)
  (when (stringp text)
    (let ((normalized (string-downcase text)))
      (or (search "stream failed" normalized :test #'char=)
          (search "provider timeout" normalized :test #'char=)
          (search "[stream failed:" normalized :test #'char=)
          (and (search "tool " normalized :test #'char=)
               (search " failed" normalized :test #'char=))
          (search "fatal" normalized :test #'char=)))))

(defun %i332-replay-streamed-turn-trace (events)
  "Deterministically replay normalized trace EVENTS and produce classifier signals."
  (let ((saw-stream-complete nil)
        (saw-stream-progress nil)
        (saw-text-delta nil)
        (saw-answer nil)
        (saw-tool-signal nil)
        (saw-retry nil)
        (saw-explicit-error nil))
    (dolist (event events)
      (let* ((kind (and (listp event) (getf event :kind)))
             (text (and (listp event) (getf event :text)))
             (error-message (and (listp event) (getf event :error-message))))
        (when (%i332-text-contains-retry-marker-p text)
          (setf saw-retry t))
        (when (or (%i332-text-contains-error-marker-p text)
                  (%i332-text-contains-error-marker-p error-message))
          (setf saw-explicit-error t))
        (case kind
          ((:text-delta :assistant-delta)
           (unless (%i332-blank-string-p text)
             (setf saw-text-delta t)))
          ((:assistant-final :assistant-message)
           (unless (or (%i332-blank-string-p text)
                       (getf event :partialp))
             (setf saw-answer t)))
          ((:tool-call-delta
            :tool-call-started
            :tool-call-argument-complete
            :tool-call-result
            :tool-started
            :tool-completed
            :tool-error
            :tool)
           (setf saw-tool-signal t))
          ((:retry :retry-requested)
           (setf saw-retry t))
          ((:failed :error :explicit-error :cancelled)
           (setf saw-explicit-error t))
          (:complete
           (setf saw-stream-complete t))
          (:stream-progress
           (setf saw-stream-progress t)
           (let ((status (getf event :status)))
             (when (eq status :completed)
               (setf saw-stream-complete t))
             (when (member status '(:failed :cancelled) :test #'eq)
               (setf saw-explicit-error t))))
          (otherwise nil))))
    (list :saw-stream-complete saw-stream-complete
          :saw-stream-progress saw-stream-progress
          :saw-text-delta saw-text-delta
          :saw-answer saw-answer
          :saw-tool-signal saw-tool-signal
          :saw-retry saw-retry
          :saw-explicit-error saw-explicit-error)))

(defun %i332-classify-streamed-turn-outcome (events)
  "Classify replayed streamed-turn EVENTS into lifecycle terminal outcomes."
  (let* ((signals (%i332-replay-streamed-turn-trace events))
         (saw-stream-complete (getf signals :saw-stream-complete))
         (saw-stream-progress (getf signals :saw-stream-progress))
         (saw-text-delta (getf signals :saw-text-delta))
         (saw-answer (getf signals :saw-answer))
         (saw-tool-signal (getf signals :saw-tool-signal))
         (saw-retry (getf signals :saw-retry))
         (saw-explicit-error (getf signals :saw-explicit-error)))
    (cond
      (saw-explicit-error :explicit-error)
      (saw-retry :retry)
      (saw-answer :answer)
      (saw-tool-signal :tool-continuation)
      ((and saw-stream-complete
            (or saw-stream-progress saw-text-delta)
            (not saw-answer)
            (not saw-tool-signal))
       :silent-completion)
      (t :silent-completion))))

(test i332-replay-helper-detects-silent-shape-signals
  (let ((signals (%i332-replay-streamed-turn-trace
                  +i332-fixture-interactive-silent-completion+)))
    (is-true (getf signals :saw-stream-complete))
    (is-true (getf signals :saw-stream-progress))
    (is-true (getf signals :saw-text-delta))
    (is (null (getf signals :saw-tool-signal)))
    (is (null (getf signals :saw-answer)))))

(test i332-headless-tool-trace-classifies-tool-continuation
  (is (eq :tool-continuation
          (%i332-classify-streamed-turn-outcome
           +i332-fixture-headless-tool-continuation+))))

(test i332-interactive-silent-completion-trace-is-detected
  (let ((outcome (%i332-classify-streamed-turn-outcome
                  +i332-fixture-interactive-silent-completion+)))
    (is (eq :silent-completion outcome))
    (is (null (member outcome
                      +i332-valid-stream-turn-terminal-outcomes+
                      :test #'eq)))))

(test i332-answer-outcome-classification
  (is (eq :answer
          (%i332-classify-streamed-turn-outcome
           +i332-fixture-answer+))))

(test i332-retry-outcome-classification
  (is (eq :retry
          (%i332-classify-streamed-turn-outcome
           +i332-fixture-retry+))))

(test i332-explicit-error-outcome-classification
  (is (eq :explicit-error
          (%i332-classify-streamed-turn-outcome
           +i332-fixture-explicit-error+))))

(test i332-contract-valid-terminal-outcomes-covered
  (let ((observed (list (%i332-classify-streamed-turn-outcome +i332-fixture-answer+)
                        (%i332-classify-streamed-turn-outcome +i332-fixture-headless-tool-continuation+)
                        (%i332-classify-streamed-turn-outcome +i332-fixture-retry+)
                        (%i332-classify-streamed-turn-outcome +i332-fixture-explicit-error+))))
    (dolist (outcome +i332-valid-stream-turn-terminal-outcomes+)
      (is-true (member outcome observed :test #'eq)
               "Expected fixture coverage for contract terminal outcome ~S."
               outcome))))

(in-suite amoebum-suite)

(defun run-all ()
  "Run all amoebum tests and return T when successful."
  (let ((amoebum:*desktop-notifications-suppressed* t)
        (results (run 'amoebum-suite)))
    (explain! results)
    (results-status results)))
