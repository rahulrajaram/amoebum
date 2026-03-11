#.(progn (require :asdf) nil)

(let* ((smoke-file (or *load-truename* *compile-file-truename*))
       (amoebum-dir (and smoke-file (make-pathname :name nil :type nil :defaults smoke-file)))
       (repo-root (and amoebum-dir (truename (merge-pathnames #P"../" amoebum-dir)))))
  (unless repo-root
    (error "Unable to resolve repository root from ~S" smoke-file))

  (load (merge-pathnames #P"ptui/.tools/quicklisp/setup.lisp" repo-root))
  (require :asdf)

  (let* ((asdf-pkg (or (find-package "ASDF")
                       (error "Missing package ASDF")))
         (load-asd-sym (or (find-symbol "LOAD-ASD" asdf-pkg)
                           (error "Missing symbol LOAD-ASD in ASDF package")))
         (load-system-sym (or (find-symbol "LOAD-SYSTEM" asdf-pkg)
                              (error "Missing symbol LOAD-SYSTEM in ASDF package")))
         (load-asd-fn (symbol-function load-asd-sym))
         (load-system-fn (symbol-function load-system-sym)))
    (funcall load-asd-fn (merge-pathnames #P"pseudopod/pseudopod.asd" repo-root))
    (funcall load-asd-fn (merge-pathnames #P"ptui/ptui.asd" repo-root))
    (funcall load-asd-fn (merge-pathnames #P"sw4rm-sdk/sw4rm-sdk.asd" repo-root))
    (funcall load-asd-fn (merge-pathnames #P"amoebum/amoebum.asd" repo-root))
    (funcall load-system-fn "amoebum"))

  (let* ((amoebum-pkg (or (find-package "AMOEBUM")
                          (error "Missing package AMOEBUM after load.")))
         (pseudopod-pkg (or (find-package "PSEUDOPOD")
                            (error "Missing package PSEUDOPOD after load.")))
         (uiop-pkg (or (find-package "UIOP")
                       (find-package "ASDF/UTILITY")
                       (error "Missing UIOP package after requiring ASDF.")))
         (symbol-in
           (lambda (name package)
             (or (find-symbol name package)
                 (error "Missing symbol ~A in package ~A." name (package-name package)))))
         (fn-in
           (lambda (name package)
             (symbol-function (funcall symbol-in name package))))
         (ensure-directory-pathname-fn (funcall fn-in "ENSURE-DIRECTORY-PATHNAME" uiop-pkg))
         (reload-config-fn (funcall fn-in "RELOAD-CONFIG" amoebum-pkg))
         (load-config-fn (funcall fn-in "LOAD-CONFIG" amoebum-pkg))
         (current-config-fn (funcall fn-in "CURRENT-CONFIG" amoebum-pkg))
         (config-value-fn (funcall fn-in "CONFIG-VALUE" amoebum-pkg))
         (setconfig-fn (funcall fn-in "SETCONFIG" amoebum-pkg))
         (dispatch-slash-command-fn (funcall fn-in "DISPATCH-SLASH-COMMAND" amoebum-pkg))
         (slash-command-result-output-fn (funcall fn-in "SLASH-COMMAND-RESULT-OUTPUT" amoebum-pkg))
         (assemble-system-prompt-fn (funcall fn-in "ASSEMBLE-SYSTEM-PROMPT" amoebum-pkg))
         (make-mcp-server-fn (funcall fn-in "MAKE-MCP-SERVER" amoebum-pkg))
         (mcp-server-transport-fn (funcall fn-in "MCP-SERVER-TRANSPORT" amoebum-pkg))
         (mcp-server-endpoint-url-fn (funcall fn-in "MCP-SERVER-ENDPOINT-URL" amoebum-pkg))
         (mcp-server-start-fn (funcall fn-in "MCP-SERVER-START" amoebum-pkg))
         (mcp-server-health-check-fn (funcall fn-in "MCP-SERVER-HEALTH-CHECK" amoebum-pkg))
         (mcp-server-stop-fn (funcall fn-in "MCP-SERVER-STOP" amoebum-pkg))
         (jsonrpc-deserialize-message-fn (funcall fn-in "JSONRPC-DESERIALIZE-MESSAGE" amoebum-pkg))
         (jsonrpc-serialize-message-fn (funcall fn-in "JSONRPC-SERIALIZE-MESSAGE" amoebum-pkg))
         (run-cli-json-fn (funcall fn-in "RUN-CLI-JSON" amoebum-pkg))
         (make-conversation-state-fn (funcall fn-in "MAKE-CONVERSATION-STATE" amoebum-pkg))
         (conversation-state-add-message-fn (funcall fn-in "CONVERSATION-STATE-ADD-MESSAGE" amoebum-pkg))
         (conversation-save-fn (funcall fn-in "CONVERSATION-SAVE" amoebum-pkg))
         (conversation-load-session-fn (funcall fn-in "CONVERSATION-LOAD-SESSION" amoebum-pkg))
         (conversation-state-messages-fn (funcall fn-in "CONVERSATION-STATE-MESSAGES" amoebum-pkg))
         (conversation-state-session-id-fn (funcall fn-in "CONVERSATION-STATE-SESSION-ID" amoebum-pkg))
         (find-skill-fn (funcall fn-in "FIND-SKILL" amoebum-pkg))
         (skill-metadata-usage-fn (funcall fn-in "SKILL-METADATA-USAGE" amoebum-pkg))
         (mcp-http-request-fn-sym
           (funcall symbol-in "*MCP-STREAMABLE-HTTP-REQUEST-FUNCTION*" amoebum-pkg))
         (make-message-fn (funcall fn-in "MAKE-MESSAGE" pseudopod-pkg))
         (message-content-fn (funcall fn-in "MESSAGE-CONTENT" pseudopod-pkg))
         (content-part-type-fn (funcall fn-in "CONTENT-PART-TYPE" pseudopod-pkg))
         (content-part-text-fn (funcall fn-in "CONTENT-PART-TEXT" pseudopod-pkg)))
    (labels
        ((assert-true (condition format-string &rest format-args)
           (unless condition
             (error (apply #'format nil format-string format-args))))
         (trim-whitespace (text)
           (string-trim '(#\Space #\Tab #\Newline #\Return) (or text "")))
         (write-text-file (path content)
           (ensure-directories-exist path)
           (with-open-file (stream path
                                   :direction :output
                                   :if-exists :supersede
                                   :if-does-not-exist :create
                                   :external-format :utf-8)
             (write-string content stream)))
         (parse-json (payload)
           (let* ((jonathan-package (or (find-package :jonathan)
                                        (error "Missing package JONATHAN.")))
                  (parse-symbol (or (find-symbol "PARSE" jonathan-package)
                                    (error "Missing JONATHAN:PARSE."))))
             (funcall (symbol-function parse-symbol) payload :as :hash-table)))
         (invoke-json-cli (&rest args)
           (let* ((output (with-output-to-string (stream)
                            (let ((*standard-output* stream))
                              (apply run-cli-json-fn args))))
                  (json-text (trim-whitespace output)))
             (assert-true (> (length json-text) 0)
                          "Expected JSON CLI invocation to emit output for args ~S."
                          args)
             (parse-json json-text)))
         (contains-substring-p (needle haystack)
           (and (stringp haystack)
                (search needle haystack :test #'char-equal))))
      (let* ((tmp-root
               (funcall ensure-directory-pathname-fn
                        (merge-pathnames
                         (make-pathname :directory `(:relative
                                                     ".tmp-codex-parity-smokes"
                                                     ,(format nil "amoebum-i125-~A-~A"
                                                              (get-universal-time)
                                                              (random 1000000))))
                         repo-root)))
             (project-root (merge-pathnames #P"project/" tmp-root))
             (working-dir (merge-pathnames #P"src/" project-root))
             (global-agents (merge-pathnames #P"global/AGENTS.md" tmp-root))
             (project-agents (merge-pathnames #P"AGENTS.md" project-root))
             (directory-agents (merge-pathnames #P"src/AGENTS.md" project-root))
             (image-path (merge-pathnames #P"fixtures/sample.png" project-root)))
        (ensure-directories-exist working-dir)
        (write-text-file global-agents "GLOBAL_AGENTS_SENTINEL")
        (write-text-file project-agents "PROJECT_AGENTS_SENTINEL")
        (write-text-file directory-agents "DIRECTORY_AGENTS_SENTINEL")
        (ensure-directories-exist image-path)
        (with-open-file (stream image-path
                                :direction :output
                                :if-exists :supersede
                                :if-does-not-exist :create
                                :element-type '(unsigned-byte 8))
          (dolist (octet '(137 80 78 71 13 10 26 10 0 0 0 0))
            (write-byte octet stream)))

        (funcall reload-config-fn :project-root project-root)

        ;; Approval policy presets
        (dolist (policy '(:untrusted :on-failure :on-request :never))
          (let ((cfg (funcall load-config-fn
                              :project-root project-root
                              :cli-values (list :approval-policy policy))))
            (assert-true (eq (funcall config-value-fn :approval-policy cfg) policy)
                         "Expected approval policy preset ~S to round-trip through config."
                         policy)))

        ;; Sandbox mode presets
        (dolist (mode '(:read-only :workspace-write :danger-full-access))
          (let ((cfg (funcall load-config-fn
                              :project-root project-root
                              :cli-values (list :sandbox-mode mode))))
            (assert-true (eq (funcall config-value-fn :sandbox-mode cfg) mode)
                         "Expected sandbox mode preset ~S to round-trip through config."
                         mode)))

        ;; --json non-interactive command output
        (let ((payload (invoke-json-cli "--json" "--command" "/config")))
          (assert-true (eq (gethash "ok" payload) t)
                       "Expected --json /config command to report ok=true.")
          (assert-true (string= (gethash "mode" payload) "json")
                       "Expected --json payload mode=json.")
          (assert-true (contains-substring-p "Configuration values:"
                                             (or (gethash "output" payload) ""))
                       "Expected --json /config output to include configuration report."))

        ;; Session resume semantics
        (let ((conversation
                (funcall make-conversation-state-fn
                         :session-id "i125-resume-session"
                         :project-root project-root)))
          (funcall conversation-state-add-message-fn
                   conversation
                   (funcall make-message-fn :role "user" :content "resume me")
                   :save-p t)
          (funcall conversation-save-fn conversation)
          (let ((payload
                  (invoke-json-cli "--json"
                                   "--session-id" "i125-resume-session"
                                   "--command" "/config")))
            (assert-true (string= (gethash "session_id" payload) "i125-resume-session")
                         "Expected --session-id to resume that conversation id.")))

        ;; AGENTS.md layering support
        (let ((assembled
                (funcall assemble-system-prompt-fn
                         :project-root project-root
                         :cwd working-dir
                         :global-layer-path global-agents
                         :project-layer-path project-agents
                         :directory-layer-paths (list directory-agents))))
          (assert-true (contains-substring-p "GLOBAL_AGENTS_SENTINEL" assembled)
                       "Expected global AGENTS layer content in assembled prompt.")
          (assert-true (contains-substring-p "PROJECT_AGENTS_SENTINEL" assembled)
                       "Expected project AGENTS layer content in assembled prompt.")
          (assert-true (contains-substring-p "DIRECTORY_AGENTS_SENTINEL" assembled)
                       "Expected directory AGENTS layer content in assembled prompt."))

        ;; MCP stdio + streamable-http transport surface
        (let* ((old-http-request-fn (symbol-value mcp-http-request-fn-sym))
               (stdio-server (funcall make-mcp-server-fn :name "stdio-smoke"
                                      :command "cat"))
               (http-server (funcall make-mcp-server-fn :name "http-smoke"
                                     :transport :streamable-http
                                     :endpoint-url "http://127.0.0.1:9999/mcp")))
          (unwind-protect
              (progn
                (setf (symbol-value mcp-http-request-fn-sym)
                      (lambda (_endpoint-url payload &key headers timeout-seconds)
                        (declare (ignore _endpoint-url headers timeout-seconds))
                        (let* ((request (funcall jsonrpc-deserialize-message-fn payload))
                               (id (gethash "id" request))
                               (method (or (gethash "method" request) "")))
                          (cond
                            ((string= method "initialize")
                             (values (funcall jsonrpc-serialize-message-fn
                                              (let ((result (make-hash-table :test #'equal))
                                                    (capabilities (make-hash-table :test #'equal)))
                                                (setf (gethash "protocolVersion" result)
                                                      (symbol-value (funcall symbol-in "*MCP-PROTOCOL-VERSION*" amoebum-pkg))
                                                (gethash "capabilities" result) capabilities)
                                                (let ((message (make-hash-table :test #'equal)))
                                                  (setf (gethash "jsonrpc" message) "2.0"
                                                        (gethash "id" message) id
                                                        (gethash "result" message) result)
                                                  message)))
                                     200))
                            ((string= method "ping")
                             (values (funcall jsonrpc-serialize-message-fn
                                              (let ((message (make-hash-table :test #'equal))
                                                    (pong (make-hash-table :test #'equal)))
                                                (setf (gethash "jsonrpc" message) "2.0"
                                                      (gethash "id" message) id
                                                      (gethash "pong" pong) t
                                                      (gethash "result" message) pong)
                                                message))
                                     200))
                            ((or (string= method "initialized")
                                 (string= method "shutdown")
                                 (string= method "exit"))
                             (values "" 202))
                            (t
                             (values (funcall jsonrpc-serialize-message-fn
                                              (let ((message (make-hash-table :test #'equal))
                                                    (result (make-hash-table :test #'equal)))
                                                (setf (gethash "jsonrpc" message) "2.0"
                                                      (gethash "id" message) id
                                                      (gethash "result" message) result)
                                                message))
                                     200))))))
                (assert-true (eq (funcall mcp-server-transport-fn stdio-server) :stdio)
                             "Expected default MCP transport to be :stdio.")
                (assert-true (eq (funcall mcp-server-transport-fn http-server) :streamable-http)
                             "Expected HTTP MCP transport to normalize to :streamable-http.")
                (assert-true (string= (funcall mcp-server-endpoint-url-fn http-server)
                                      "http://127.0.0.1:9999/mcp")
                             "Expected MCP HTTP endpoint URL to be retained.")
                (funcall mcp-server-start-fn http-server)
                (assert-true (funcall mcp-server-health-check-fn http-server)
                             "Expected started streamable-http MCP server health check to pass.")
                (assert-true (funcall mcp-server-stop-fn http-server)
                             "Expected streamable-http MCP server stop to succeed."))
            (setf (symbol-value mcp-http-request-fn-sym) old-http-request-fn)))

        ;; SW4RM networked delegation mode surface
        (let ((old-mode (funcall config-value-fn :swarm-delegation-mode (funcall current-config-fn))))
          (unwind-protect
              (progn
                (funcall setconfig-fn :swarm-delegation-mode :networked)
                (multiple-value-bind (handled result)
                    (funcall dispatch-slash-command-fn "/spawn i125 networked delegation smoke")
                  (assert-true handled "Expected /spawn to be handled.")
                  (assert-true (contains-substring-p
                                "networked"
                                (funcall slash-command-result-output-fn result))
                               "Expected /spawn output to mention networked delegation mode.")))
            (funcall setconfig-fn :swarm-delegation-mode old-mode)))

        ;; Image input in non-interactive chat turns
        (let* ((payload (invoke-json-cli "--json"
                                         "--prompt" "triage this image"
                                         "--image" (namestring image-path)))
               (session-id (gethash "session_id" payload))
               (images (gethash "images" payload))
               (normalized-images
                 (cond
                   ((vectorp images) (loop for item across images collect item))
                   ((listp images) images)
                   (t nil)))
               (conversation (funcall conversation-load-session-fn session-id))
               (messages (funcall conversation-state-messages-fn conversation))
               (latest (car (last messages)))
               (parts (and latest (funcall message-content-fn latest)))
               (has-image-placeholder
                 (loop for part in (or parts '())
                       thereis (contains-substring-p "[image"
                                                     (or (funcall content-part-text-fn part) "")))))
          (assert-true (eq (gethash "ok" payload) t)
                       "Expected --json prompt/image invocation to succeed.")
          (assert-true (stringp session-id)
                       "Expected --json prompt/image invocation to return session_id.")
          (assert-true (and (= (length normalized-images) 1)
                            (string= (first normalized-images) (namestring image-path)))
                       "Expected --json payload images array to include the supplied image path.")
          (assert-true has-image-placeholder
                       "Expected persisted user message content to include an image marker, got ~S."
                       parts)
          (assert-true (string= (funcall conversation-state-session-id-fn conversation)
                                session-id)
                       "Expected reloaded session id to match JSON payload."))

        ;; Built-in /review parity surface
        (let ((review-skill (funcall find-skill-fn "review")))
          (assert-true review-skill "Expected built-in review skill to be registered.")
          (assert-true (contains-substring-p "/review"
                                             (funcall skill-metadata-usage-fn review-skill))
                       "Expected review skill usage to expose /review command.")))))

  (format t "AMOEBUM_CODEX_PARITY_SMOKE_OK~%"))
