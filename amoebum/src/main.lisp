(in-package :amoebum)

(defun %cli-json-function (name)
  (let* ((json-package (find-package :jonathan))
         (symbol (and json-package
                      (find-symbol name json-package))))
    (unless (and symbol (fboundp symbol))
      (error "Jonathan function ~A is unavailable." name))
    (symbol-function symbol)))

(defun %json-encode (value)
  (funcall (%cli-json-function "TO-JSON") value))

(defun %json-object (&rest pairs)
  (let ((table (make-hash-table :test #'equal)))
    (loop for (key value) on pairs by #'cddr do
          (setf (gethash key table) value))
    table))

(defun %trim-cli-arg (value)
  (string-trim '(#\Space #\Tab #\Newline #\Return)
               (or (and value (princ-to-string value)) "")))

(defun %non-empty-cli-arg-p (value)
  (> (length (%trim-cli-arg value)) 0))

(defun %resume-latest-token-p (value)
  (let ((trimmed (%trim-cli-arg value)))
    (or (string-equal trimmed "latest")
        (string-equal trimmed "1")
        (string-equal trimmed "true"))))

(defun %consume-cli-value (args index flag)
  (let* ((next-index (1+ index))
         (value (and (< next-index (length args))
                     (nth next-index args))))
    (unless (%non-empty-cli-arg-p value)
      (error "Missing value for CLI option ~A." flag))
    (values (%trim-cli-arg value) next-index)))

(defun %parse-cli-options (argv)
  (let ((json-mode-p nil)
        (demo-mode-p nil)
        (help-mode-p nil)
        (version-mode-p nil)
        (command nil)
        (prompt nil)
        (resume nil)
        (session-id nil)
        (image-paths '()))
    (loop for index from 0 below (length argv) do
      (let ((argument (or (nth index argv) "")))
        (cond
          ((or (string= argument "--help")
               (string= argument "-h"))
           (setf help-mode-p t))
          ((string= argument "--version")
           (setf version-mode-p t))
          ((or (string= argument "--json")
               (string= argument "--non-interactive"))
           (setf json-mode-p t))
          ((string= argument "--demo")
           (setf demo-mode-p t))
          ((or (string= argument "--command")
               (string= argument "-c"))
           (multiple-value-bind (value consumed-index)
               (%consume-cli-value argv index "--command")
             (setf command value
                   index consumed-index)))
          ((uiop:string-prefix-p "--command=" argument)
           (setf command (%trim-cli-arg (subseq argument (length "--command=")))))
          ((string= argument "--prompt")
           (multiple-value-bind (value consumed-index)
               (%consume-cli-value argv index "--prompt")
             (setf prompt value
                   index consumed-index)))
          ((uiop:string-prefix-p "--prompt=" argument)
           (setf prompt (%trim-cli-arg (subseq argument (length "--prompt=")))))
          ((string= argument "--resume")
           (let ((next (and (< (1+ index) (length argv))
                            (nth (1+ index) argv))))
             (if (and next
                      (%non-empty-cli-arg-p next)
                      (not (uiop:string-prefix-p "-" next)))
                 (progn
                   (setf resume (%trim-cli-arg next))
                   (incf index))
                 (setf resume "latest"))))
          ((uiop:string-prefix-p "--resume=" argument)
           (setf resume (%trim-cli-arg (subseq argument (length "--resume=")))))
          ((string= argument "--session-id")
           (multiple-value-bind (value consumed-index)
               (%consume-cli-value argv index "--session-id")
             (setf session-id value
                   index consumed-index)))
          ((uiop:string-prefix-p "--session-id=" argument)
           (setf session-id (%trim-cli-arg (subseq argument (length "--session-id=")))))
          ((string= argument "--image")
           (multiple-value-bind (value consumed-index)
               (%consume-cli-value argv index "--image")
             (push value image-paths)
             (setf index consumed-index)))
          ((uiop:string-prefix-p "--image=" argument)
           (push (%trim-cli-arg (subseq argument (length "--image="))) image-paths)))))
    (list :json-mode-p json-mode-p
          :demo-mode-p demo-mode-p
          :help-mode-p help-mode-p
          :version-mode-p version-mode-p
          :command command
          :prompt prompt
          :resume resume
          :session-id session-id
          :image-paths (nreverse image-paths))))

(defun %amoebum-version ()
  (or (ignore-errors
        (let ((system (asdf:find-system :amoebum nil)))
          (and system (asdf:component-version system))))
      "0.1.0"))

(defun %print-cli-help ()
  (dolist (line `("Usage:"
                  "  amoebum"
                  "  amoebum --demo"
                  "  amoebum --json --prompt <text> [--image <path> ...]"
                  "  amoebum --json --command </slash-command>"
                  "  amoebum [--resume [latest|<session-id>]]"
                  "  amoebum [--session-id <session-id>]"
                  "  amoebum --help"
                  "  amoebum --version"
                  ""
                  "Modes:"
                  "  default      Launch the interactive TUI."
                  "  --demo       Launch the interactive demo without provider credentials."
                  "  --json       Run the machine-readable JSON CLI contract."
                  ""
                  "Notes:"
                  "  Repo wrapper: ./bin/amoebum bootstraps local Yarli state first."
                  "  Installed wrapper: amoebum runs repo-independently from the packaged image."))
    (format t "~A~%" line))
  (finish-output)
  t)

(defun %print-cli-version ()
  (format t "amoebum ~A~%" (%amoebum-version))
  (finish-output)
  t)

(defparameter +default-gc-nursery-megabytes+ 64
  "Default SBCL nursery size for normal interactive sessions.")

(defparameter +demo-gc-nursery-megabytes+ 64
  "Default SBCL nursery size for demo-mode sessions unless overridden by env.")

(defun %parse-gc-nursery-megabytes (value)
  (let* ((trimmed (%trim-cli-arg value))
         (parsed (and (plusp (length trimmed))
                      (ignore-errors (parse-integer trimmed :junk-allowed nil)))))
    (and (integerp parsed)
         (> parsed 0)
         parsed)))

(defun %gc-nursery-megabytes (&key demo-mode-p)
  (or (%parse-gc-nursery-megabytes (uiop:getenv "AMOEBUM_GC_NURSERY_MB"))
      (if demo-mode-p
          +demo-gc-nursery-megabytes+
          +default-gc-nursery-megabytes+)))

(defun %resolve-cli-conversation (&key session-id resume)
  (let ((trimmed-session-id (%trim-cli-arg session-id))
        (trimmed-resume (%trim-cli-arg resume)))
    (when (and (> (length trimmed-session-id) 0)
               (> (length trimmed-resume) 0))
      (error "Use either --session-id or --resume, not both."))
    (cond
      ((> (length trimmed-session-id) 0)
       (or (conversation-load-session trimmed-session-id)
           (make-conversation-state :session-id trimmed-session-id)))
      ((> (length trimmed-resume) 0)
       (if (%resume-latest-token-p trimmed-resume)
           (or (conversation-load-latest)
               (error "Cannot resume latest session: no saved sessions found."))
           (or (conversation-load-session trimmed-resume)
               (error "Cannot resume session ~S: no saved conversation found."
                      trimmed-resume))))
      (t
       (or (conversation-load-latest)
           (make-conversation-state))))))

(defun %validate-image-path (path)
  (%validate-image-input-path (%trim-cli-arg path)))

(defun %image-content-part (path)
  (%make-image-content-part path))

(defun %build-user-message-content (prompt image-paths)
  (let ((parts '()))
    (when (%non-empty-cli-arg-p prompt)
      (push (pseudopod:make-text-part (%trim-cli-arg prompt)) parts))
    (dolist (image-path image-paths)
      (push (%image-content-part image-path) parts))
    (nreverse parts)))

(defun %cli-handle-command (command conversation)
  (if (or (null command)
          (not (%non-empty-cli-arg-p command)))
      (values nil "No command provided." nil)
      (let ((trimmed (%trim-cli-arg command)))
        (if (slash-command-input-p trimmed)
            (multiple-value-bind (handled result)
                (dispatch-slash-command trimmed
                                        :config (current-config)
                                        :chat-state (make-chat-ui-state :conversation conversation))
              (values handled
                      (if (and result (typep result 'slash-command-result))
                          (or (slash-command-result-output result) "")
                          (or (and result (princ-to-string result))
                              ""))
                      (and (typep result 'slash-command-result)
                           (slash-command-result-payload result))))
            (values nil "Only slash commands are supported in --json command mode."
                    nil)))))

(defun %cli-handle-prompt (prompt image-paths conversation)
  (let ((content (%build-user-message-content prompt image-paths)))
    (if (null content)
        (values nil "Prompt and image attachments are empty.")
        (progn
          (conversation-state-add-message
           conversation
           (pseudopod:make-message :role "user" :content content)
           :save-p t)
          (values t "Prompt accepted into conversation session state.")))))

(defun %cli-last-assistant-message (chat-state)
  (let ((messages (chat-ui-state-messages chat-state)))
    (loop for message in (reverse messages)
          when (and (pseudopod:message-p message)
                    (string= (string-downcase (or (pseudopod:message-role message)
                                                 "assistant"))
                             "assistant"))
            do (let ((text (%message-content->text message)))
                 (when (and (stringp text) (plusp (length text)))
                   (return text)))
          finally (return ""))))

(defun %cli-run-headless-assistant (conversation prompt image-paths)
  (let* ((*desktop-notifications-suppressed* t)
         (content (%build-user-message-content prompt image-paths))
         (chat-state (make-chat-ui-state
                      :conversation conversation
                      :stream-client (pseudopod:make-client))))
    (if (null content)
        (values nil "Prompt and image attachments are empty.")
        (progn
          (chat-ui-add-message chat-state "user" content)
          (let ((amoebum::*missing-tool-argument-recovery-mode*
                  :structured-error))
            (%start-step-loop-assistant-response chat-state))
          (values t (%cli-last-assistant-message chat-state))))))

(defun %event-journal-enabled-p ()
  (let ((value (uiop:getenv "AMOEBUM_EVENT_JOURNAL")))
    (and value (plusp (length (string-trim '(#\Space #\Tab #\Newline #\Return) value)))
         (%parse-boolean value))))

(defun %event-journal-directory ()
  (let ((value (uiop:getenv "AMOEBUM_EVENT_JOURNAL_DIR")))
    (let ((trimmed (and value
                        (string-trim '(#\Space #\Tab #\Newline #\Return) value))))
      (and (and trimmed (plusp (length trimmed)))
           (uiop:native-namestring
            (uiop:ensure-directory-pathname trimmed))))))

(defun %run-with-optional-event-journal (thunk)
  (let ((journal nil))
    (when (%event-journal-enabled-p)
      (handler-case
          (setf journal (start-event-journal
                         :journal (make-event-journal-instance
                                   :directory (%event-journal-directory))))
        (error (condition)
          (log-runtime-condition condition
                                 :kind "event-journal-init-failed"
                                 :source :main
                                 :message "Event journal initialization failed."
                                 :details (list :event-journal-directory
                                                (%event-journal-directory))
                                 :path (runtime-log-path)
                                 :include-backtrace-p nil)
          (format *error-output* "[amoebum] event journal init failed: ~A~%"
                  condition))))
    (unwind-protect
         (funcall thunk)
      (when journal
        (ignore-errors
          (let ((paths (journal-segment-paths journal)))
            (log-runtime-event :level :info
                               :kind "event-journal-stopped"
                               :source :main
                               :message "Event journal stopped."
                               :details (list :paths paths))
            (format *error-output*
                    "[amoebum] event journal stopped: ~A~%"
                    (or paths "<none>"))))
        (ignore-errors (stop-event-journal journal))))))

(defparameter +cli-json-schema-version+ "amoebum.cli.json.v1")
(defparameter +cli-json-schema-doc+ "docs/json-cli-contract.md")

(defun %cli-plist-like-p (value)
  (and (listp value)
       (evenp (length value))
       (loop for key in value by #'cddr
             always (or (keywordp key)
                        (stringp key)
                        (symbolp key)))))

(defun %cli-json-encodable (value)
  (cond
    ((or (null value)
         (stringp value)
         (numberp value)
         (eq value t))
     value)
    ((keywordp value)
     (string-downcase (symbol-name value)))
    ((hash-table-p value)
     (let ((table (make-hash-table :test #'equal)))
       (maphash (lambda (key item)
                  (setf (gethash (string-downcase
                                  (if (or (symbolp key) (keywordp key))
                                      (symbol-name key)
                                      (princ-to-string key)))
                                 table)
                        (%cli-json-encodable item)))
                value)
       table))
    ((vectorp value)
     (coerce (loop for item across value
                   collect (%cli-json-encodable item))
             'vector))
    ((%cli-plist-like-p value)
     (let ((table (make-hash-table :test #'equal)))
       (loop for (key item) on value by #'cddr do
         (setf (gethash (string-downcase
                         (if (or (symbolp key) (keywordp key))
                             (symbol-name key)
                             (princ-to-string key)))
                        table)
               (%cli-json-encodable item)))
       table))
    ((listp value)
     (coerce (mapcar #'%cli-json-encodable value) 'vector))
    (t
     (princ-to-string value))))

(defun %cli-json-result-kind (action ok)
  (let ((normalized-action (or action "none")))
    (cond
      ((string= normalized-action "error") "error")
      ((string= normalized-action "command") (if ok "tool" "error"))
      ((string= normalized-action "prompt") (if ok "prompt" "error"))
      ((not ok) "error")
      (t "prompt"))))

(defun %cli-json-events (&key action ok command output error command-payload)
  (let* ((normalized-action (or action "none"))
         (events (list
                  (%json-object
                   "kind" "progress"
                   "phase" "started"
                   "action" normalized-action))))
    (when (string= normalized-action "command")
      (push (%json-object
             "kind" "tool"
             "phase" (if ok "completed" "failed")
             "name" "slash-command"
             "command" command
             "output" output
             "payload" (%cli-json-encodable command-payload)
             "error" error)
            events))
    (push (%json-object
           "kind" "progress"
           "phase" (if ok "completed" "failed")
           "action" normalized-action
           "message" (if ok
                          "Headless run completed."
                          "Headless run failed."))
          events)
    (nreverse events)))

(defun %emit-cli-json-result (&key ok mode action output error command prompt
                                session-id image-paths command-payload)
  (let* ((normalized-ok (not (null ok)))
         (normalized-mode (or mode "interactive"))
         (normalized-action (or action "none"))
         (normalized-command-payload (%cli-json-encodable command-payload))
         (images (coerce (or image-paths '()) 'vector))
         (result-kind (%cli-json-result-kind normalized-action normalized-ok))
         (result-status (if normalized-ok "completed" "failed"))
         (events (%cli-json-events :action normalized-action
                                   :ok normalized-ok
                                   :command command
                                   :output output
                                   :error error
                                   :command-payload normalized-command-payload))
         (payload
          (%json-object
           "schema_version" +cli-json-schema-version+
           "schema_doc" +cli-json-schema-doc+
           "ok" normalized-ok
           "mode" normalized-mode
           "action" normalized-action
           "command" command
           "prompt" prompt
           "session_id" session-id
           "images" images
           "output" output
           "error" error
           "command_payload" normalized-command-payload
           "request" (%json-object
                      "mode" normalized-mode
                      "action" normalized-action
                      "command" command
                      "prompt" prompt
                      "images" images
                      "session_id" session-id
                      "command_payload" normalized-command-payload)
           "result" (%json-object
                     "kind" result-kind
                     "status" result-status
                     "output" output
                     "error" error
                     "tool" (and (string= result-kind "tool")
                                 (%json-object
                                  "name" "slash-command"
                                  "command" command
                                  "payload" normalized-command-payload))
                     "progress" (%json-object
                                 "status" result-status))
           "events" (coerce events 'vector))))
    (format t "~A~%" (%json-encode payload))
    (finish-output)))

(defun run-cli-json (&rest argv)
  (let* ((options (%parse-cli-options argv))
         (command (getf options :command))
         (prompt (getf options :prompt))
         (resume (getf options :resume))
         (session-id (getf options :session-id))
         (image-paths (getf options :image-paths))
         (conversation (%resolve-cli-conversation
                        :session-id session-id
                        :resume resume)))
    (handler-case
      (progn
        (when (and (%non-empty-cli-arg-p command)
                   (%non-empty-cli-arg-p prompt))
          (error "Use either --command or --prompt, not both."))
        (multiple-value-bind (ok output command-payload)
            (if (%non-empty-cli-arg-p command)
                (%cli-handle-command command conversation)
                (if (plusp (length image-paths))
                    ;; Some configured providers are text-only; keep JSON prompt/image
                    ;; mode deterministic by persisting the user turn without forcing
                    ;; a model roundtrip.
                    (%cli-handle-prompt prompt image-paths conversation)
                    (%cli-run-headless-assistant conversation prompt image-paths)))
          (let ((active-session (conversation-state-session-id conversation)))
            (conversation-save conversation)
            (%emit-cli-json-result
             :ok ok
             :mode "json"
             :action (if (%non-empty-cli-arg-p command) "command" "prompt")
             :output output
             :command command
             :prompt prompt
             :session-id active-session
             :image-paths image-paths
             :command-payload command-payload))
            t))
      (error (condition)
        (log-runtime-condition condition
                               :kind "cli-json-error"
                               :source :main
                               :message "JSON mode failed."
                               :details (list :argv argv)
                               :path (runtime-log-path)
                               :include-backtrace-p nil)
        (%emit-cli-json-result
         :ok nil
         :mode "json"
         :action "error"
         :error (princ-to-string condition)
         :command command
         :prompt prompt
         :session-id (and conversation (conversation-state-session-id conversation))
         :image-paths image-paths)
        nil))))

(defun %configure-gc-tuning (&key demo-mode-p)
  "Configure SBCL GC for lower latency and better interactive performance.
Streaming responses generate substantial short-lived allocation (styled-line
lists, grapheme segments, cell clones) — a larger nursery lets these objects
die without triggering GC on every frame."
  #+sbcl
  (let ((nursery-megabytes (%gc-nursery-megabytes :demo-mode-p demo-mode-p)))
    ;; The env override keeps perf investigations reproducible without
    ;; hard-coding different defaults for demo and interactive runs.
    (setf (sb-ext:bytes-consed-between-gcs) (* nursery-megabytes 1024 1024))
    ;; Trigger a GC to establish baseline with new settings
    (sb-ext:gc :full t))
  #-sbcl
  nil)

(defun %resolve-interactive-chat-state (&key demo-mode-p session-id resume)
  (unless demo-mode-p
    (make-chat-ui-state
     :conversation (%resolve-cli-conversation
                    :session-id session-id
                    :resume resume))))

(defun %run-interactive-chat-ui (&key demo-mode-p session-id resume
                                   (backend :auto) (fps 60))
  (run-chat-ui :backend backend
               :fps fps
               :demo demo-mode-p
               :initial-state (%resolve-interactive-chat-state
                               :demo-mode-p demo-mode-p
                               :session-id session-id
                               :resume resume)))

(defun main (&rest argv)
  (let ((effective-argv (or argv
                            #+sbcl (rest sb-ext:*posix-argv*)
                            #-sbcl nil))
        (options nil))
    (setf options (%parse-cli-options effective-argv))
    (when (getf options :help-mode-p)
      (return-from main (%print-cli-help)))
    (when (getf options :version-mode-p)
      (return-from main (%print-cli-version)))
    (%configure-gc-tuning :demo-mode-p (getf options :demo-mode-p))
    (reload-config :cli-arguments effective-argv)
    ;; Load YAML theme if not already loaded by config system
    ;; The bundled Tokyo Night theme is the default, but can be overridden via:
    ;;   - :theme-yaml t in config files (auto-detect from standard locations)
    ;;   - :theme-yaml "/path/to/theme.yaml" in config files (specific path)
    ;;   - --theme-yaml /path/to/theme.yaml CLI option
    ;;   - --theme /path/to/theme.yaml CLI option (shorthand)
    ;; User themes in ~/.config/amoebum/theme.yaml take precedence over bundled.
    (unless *yaml-theme-loaded-p*
      (let ((theme-yaml-config (config-value :theme-yaml)))
        (multiple-value-bind (yaml-success yaml-status)
            (cond
              ;; If config explicitly sets :theme-yaml nil, skip YAML theme loading
              ((null theme-yaml-config)
               (values t :disabled))
              ;; If config sets :theme-yaml t, auto-detect (bundled Tokyo Night is fallback)
              ((eq theme-yaml-config t)
               (load-yaml-theme :if-not-loaded t))
              ;; If config sets a specific path, use it
              ((stringp theme-yaml-config)
               (if (probe-file (pathname theme-yaml-config))
                   (load-yaml-theme :cli-path theme-yaml-config :if-not-loaded t)
                   (progn
                     (log-runtime-event :level :warn
                                        :kind "yaml-theme-config-path-not-found"
                                        :source :main
                                        :message "Theme YAML path from config not found, using bundled theme."
                                        :details (list :path theme-yaml-config))
                     (load-yaml-theme :if-not-loaded t))))
              ;; Default: load bundled theme
              (t
               (load-yaml-theme :if-not-loaded t)))
          (unless yaml-success
            (log-runtime-event :level :warn
                               :kind "yaml-theme-failed"
                               :source :main
                               :message "YAML theme loading failed, using built-in Lisp theme."
                               :details (list :status yaml-status))))))
    (enable-tts-post-receive-hook)
    (let* ((mode (cond
                   ((getf options :help-mode-p) :help)
                   ((getf options :version-mode-p) :version)
                   ((getf options :json-mode-p) :json)
                   ((getf options :demo-mode-p) :demo)
                   (t :interactive)))
           (completedp nil))
      (log-runtime-event :level :info
                         :kind "runtime-startup"
                         :source :main
                         :message "Amoebum runtime starting."
                         :details (list :mode mode
                                        :argv effective-argv
                                        :runtime-log (runtime-log-path)
                                        :crash-log (crash-log-path)))
      (unwind-protect
           (handler-case
               (let ((result
                       (%run-with-optional-event-journal
                        (lambda ()
                          (cond
                            ((getf options :json-mode-p)
                             (apply #'run-cli-json effective-argv))
                            ((getf options :demo-mode-p)
                             (%run-interactive-chat-ui :demo-mode-p t))
                            (t
                             (%run-interactive-chat-ui
                              :session-id (getf options :session-id)
                              :resume (getf options :resume))))))))
                 (setf completedp t)
                 result)
             (error (condition)
               (log-runtime-condition condition
                                      :kind "runtime-unhandled-error"
                                      :source :main
                                      :message "Amoebum runtime exited with an unhandled condition."
                                      :details (list :mode mode
                                                     :argv effective-argv)
                                      :path (crash-log-path))
               (error condition)))
        (when completedp
          (log-runtime-event :level :info
                             :kind "runtime-shutdown"
                             :source :main
                             :message "Amoebum runtime stopped cleanly."
                             :details (list :mode mode)))))))
