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
    (funcall load-asd-fn (merge-pathnames #P"sw4rm-sdk/sw4rm-sdk.asd" repo-root))
    (funcall load-asd-fn (merge-pathnames #P"ptui/ptui.asd" repo-root))
    (funcall load-asd-fn (merge-pathnames #P"amoebum/amoebum.asd" repo-root))
    (funcall load-system-fn "amoebum"))

  (let* ((amoebum-pkg (or (find-package "AMOEBUM")
                          (error "Missing package AMOEBUM after load.")))
         (pseudopod-pkg (or (find-package "PSEUDOPOD")
                            (error "Missing package PSEUDOPOD after load.")))
         (symbol-in
           (lambda (name package)
             (or (find-symbol name package)
                 (error "Missing symbol ~A in package ~A." name (package-name package)))))
         (fn-in
           (lambda (name package)
             (symbol-function (funcall symbol-in name package))))
         (make-request-fn (funcall fn-in "MAKE-JSONRPC-REQUEST-MESSAGE" amoebum-pkg))
         (make-response-fn (funcall fn-in "MAKE-JSONRPC-RESPONSE-MESSAGE" amoebum-pkg))
         (serialize-fn (funcall fn-in "JSONRPC-SERIALIZE-MESSAGE" amoebum-pkg))
         (deserialize-fn (funcall fn-in "JSONRPC-DESERIALIZE-MESSAGE" amoebum-pkg))
         (frame-fn (funcall fn-in "JSONRPC-FRAME-MESSAGE" amoebum-pkg))
         (read-fn (funcall fn-in "JSONRPC-READ-MESSAGE" amoebum-pkg))
         (make-client-fn (funcall fn-in "MAKE-MCP-JSONRPC-CLIENT" amoebum-pkg))
         (send-request-fn (funcall fn-in "MCP-JSONRPC-SEND-REQUEST" amoebum-pkg))
         (handle-incoming-fn (funcall fn-in "MCP-JSONRPC-HANDLE-INCOMING-MESSAGE" amoebum-pkg))
         (timeout-condition-sym (funcall symbol-in "MCP-TIMEOUT" amoebum-pkg))
         (make-server-fn (funcall fn-in "MAKE-MCP-SERVER" amoebum-pkg))
         (server-start-fn (funcall fn-in "MCP-SERVER-START" amoebum-pkg))
         (server-stop-fn (funcall fn-in "MCP-SERVER-STOP" amoebum-pkg))
         (server-health-fn (funcall fn-in "MCP-SERVER-HEALTH-CHECK" amoebum-pkg))
         (server-process-fn (funcall fn-in "MCP-SERVER-PROCESS" amoebum-pkg))
         (server-restart-count-fn (funcall fn-in "MCP-SERVER-RESTART-COUNT" amoebum-pkg))
         (discover-mcp-tools-fn (funcall fn-in "DISCOVER-MCP-SERVER-TOOLS" amoebum-pkg))
         (make-event-bus-fn (funcall fn-in "MAKE-EVENT-BUS" amoebum-pkg))
         (subscribe-fn (funcall fn-in "SUBSCRIBE" amoebum-pkg))
         (make-context-fn (funcall fn-in "MAKE-AMOEBUM-CONTEXT" amoebum-pkg))
         (execute-tool-fn (funcall fn-in "EXECUTE-TOOL" amoebum-pkg))
         (setconfig-fn (funcall fn-in "SETCONFIG" amoebum-pkg))
         (config-value-fn (funcall fn-in "CONFIG-VALUE" amoebum-pkg))
         (current-config-fn (funcall fn-in "CURRENT-CONFIG" amoebum-pkg))
         (make-toolset-fn (funcall fn-in "MAKE-TOOLSET" pseudopod-pkg))
         (make-tool-call-fn (funcall fn-in "MAKE-TOOL-CALL" pseudopod-pkg))
         (tool-permission-denied-sym (funcall symbol-in "TOOL-PERMISSION-DENIED" amoebum-pkg))
         (event-type-mcp-discovered
          (symbol-value (funcall symbol-in "+EVENT-TYPE-MCP-TOOL-DISCOVERED+" amoebum-pkg)))
         (event-type-mcp-invoked
          (symbol-value (funcall symbol-in "+EVENT-TYPE-MCP-TOOL-INVOKED+" amoebum-pkg))))
    (labels ((assert-true (condition format-string &rest format-args)
               (unless condition
                 (error (apply #'format nil format-string format-args))))
             (make-obj (&rest pairs)
               (let ((obj (make-hash-table :test #'equal)))
                 (loop for (key value) on pairs by #'cddr do
                       (setf (gethash key obj) value))
                 obj))
             (parse-json-text (payload)
               (let* ((jonathan-package (or (find-package :jonathan)
                                            (error "Missing package JONATHAN.")))
                      (parse-symbol (or (find-symbol "PARSE" jonathan-package)
                                        (error "Missing JONATHAN:PARSE."))))
                 (funcall (symbol-function parse-symbol) payload :as :hash-table)))
             (trim-whitespace (text)
               (string-trim '(#\Space #\Tab #\Newline #\Return)
                            (or text "")))
             (wait-until (predicate timeout-seconds &optional (poll-seconds 0.05d0))
               (let ((deadline (+ (get-internal-real-time)
                                  (ceiling (* timeout-seconds
                                              internal-time-units-per-second)))))
                 (loop
                   do
                      (when (funcall predicate)
                        (return t))
                      (when (>= (get-internal-real-time) deadline)
                        (return nil))
                      (sleep poll-seconds))))
             (command-available-p (command)
               (handler-case
                   (progn
                     (uiop:run-program (list command "--version")
                                       :ignore-error-status t
                                       :output :string
                                       :error-output :string)
                     t)
                 (error ()
                   nil)))
             (resolve-python-command ()
               (or (and (command-available-p "python3") "python3")
                   (and (command-available-p "python") "python")
                   (error "MCP lifecycle smoke requires python3 or python.")))
             (write-mock-server-script (path)
               (with-open-file (stream path
                                       :direction :output
                                       :if-does-not-exist :create
                                       :if-exists :supersede)
                 (format stream "~{~A~%~}"
                         '("import json"
                           "import sys"
                           ""
                           "def read_message():"
                           "    headers = {}"
                           "    while True:"
                           "        line = sys.stdin.buffer.readline()"
                           "        if not line:"
                           "            return None"
                           "        if line in (b\"\\r\\n\", b\"\\n\"):"
                           "            break"
                           "        if b\":\" not in line:"
                           "            continue"
                           "        key, value = line.split(b\":\", 1)"
                           "        headers[key.strip().lower()] = value.strip()"
                           "    length = int(headers.get(b\"content-length\", b\"0\"))"
                           "    payload = sys.stdin.buffer.read(length)"
                           "    if len(payload) != length:"
                           "        return None"
                           "    return json.loads(payload.decode(\"utf-8\"))"
                           ""
                           "def send_message(message):"
                           "    payload = json.dumps(message, separators=(\",\", \":\")).encode(\"utf-8\")"
                           "    header = f\"Content-Length: {len(payload)}\\r\\n\\r\\n\".encode(\"ascii\")"
                           "    sys.stdout.buffer.write(header)"
                           "    sys.stdout.buffer.write(payload)"
                           "    sys.stdout.buffer.flush()"
                           ""
                           "while True:"
                           "    message = read_message()"
                           "    if message is None:"
                           "        break"
                           "    method = message.get(\"method\")"
                           "    request_id = message.get(\"id\")"
                           ""
                           "    if method == \"initialize\":"
                           "        if request_id is not None:"
                           "            send_message({\"jsonrpc\": \"2.0\", \"id\": request_id, \"result\": {\"protocolVersion\": \"2024-11-05\", \"capabilities\": {}}})"
                           "        continue"
                           ""
                           "    if method == \"ping\":"
                           "        if request_id is not None:"
                           "            send_message({\"jsonrpc\": \"2.0\", \"id\": request_id, \"result\": {\"pong\": True}})"
                           "        continue"
                           ""
                           "    if method == \"tools/list\":"
                           "        if request_id is not None:"
                           "            send_message({\"jsonrpc\": \"2.0\", \"id\": request_id, \"result\": {\"tools\": ["
                           "                {\"name\": \"echo\", \"description\": \"Echo text\", \"inputSchema\": {\"type\": \"object\", \"properties\": {\"text\": {\"type\": \"string\"}}, \"required\": [\"text\"]}},"
                           "                {\"name\": \"sum\", \"description\": \"Add two integers\", \"inputSchema\": {\"type\": \"object\", \"properties\": {\"a\": {\"type\": \"integer\"}, \"b\": {\"type\": \"integer\"}}, \"required\": [\"a\", \"b\"]}}"
                           "            ]}})"
                           "        continue"
                           ""
                           "    if method == \"tools/call\":"
                           "        params = message.get(\"params\") or {}"
                           "        tool_name = params.get(\"name\")"
                           "        arguments = params.get(\"arguments\") or {}"
                           "        if tool_name == \"echo\":"
                           "            text = arguments.get(\"text\", \"\")"
                           "            result = {\"content\": [{\"type\": \"text\", \"text\": f\"echo:{text}\"}], \"structuredContent\": {\"echo\": text}, \"isError\": False}"
                           "            if request_id is not None:"
                           "                send_message({\"jsonrpc\": \"2.0\", \"id\": request_id, \"result\": result})"
                           "            continue"
                           "        if tool_name == \"sum\":"
                           "            a = int(arguments.get(\"a\", 0))"
                           "            b = int(arguments.get(\"b\", 0))"
                           "            result = {\"content\": [{\"type\": \"text\", \"text\": str(a + b)}], \"structuredContent\": {\"sum\": a + b}, \"isError\": False}"
                           "            if request_id is not None:"
                           "                send_message({\"jsonrpc\": \"2.0\", \"id\": request_id, \"result\": result})"
                           "            continue"
                           "        if request_id is not None:"
                           "            send_message({\"jsonrpc\": \"2.0\", \"id\": request_id, \"error\": {\"code\": -32001, \"message\": \"unknown tool\"}})"
                           "        continue"
                           ""
                           "    if method in (\"shutdown\", \"exit\"):"
                           "        if request_id is not None:"
                           "            send_message({\"jsonrpc\": \"2.0\", \"id\": request_id, \"result\": {\"ok\": True}})"
                           "        break"
                           ""
                           "    if request_id is not None:"
                           "        send_message({\"jsonrpc\": \"2.0\", \"id\": request_id, \"error\": {\"code\": -32601, \"message\": \"method not found\"}})"
                           ""))))
             (server-process-pid (process)
               #+sbcl
               (and process (ignore-errors (sb-ext:process-pid process)))
               #-sbcl
               nil)
             (process-present-p (pid)
               (multiple-value-bind (stdout _stderr exit-code)
                   (uiop:run-program
                    (list "ps" "-o" "stat=" "-p" (write-to-string pid))
                    :ignore-error-status t
                    :output :string
                    :error-output :string)
                 (declare (ignore _stderr))
                 (and (= exit-code 0)
                      (> (length (trim-whitespace stdout)) 0))))
             (kill-process (process)
               #+sbcl
               (ignore-errors (sb-ext:process-kill process 9))
               #-sbcl
               nil))
      (let* ((params (make-obj "cursor" "abc123"))
             (request (funcall make-request-fn "tools/list" :params params :id 42))
             (encoded (funcall serialize-fn request))
             (decoded (funcall deserialize-fn encoded)))
        (assert-true (string= (gethash "jsonrpc" decoded) "2.0")
                     "Expected JSON-RPC version 2.0 in decoded request.")
        (assert-true (string= (gethash "method" decoded) "tools/list")
                     "Expected decoded request method tools/list.")
        (assert-true (eql (gethash "id" decoded) 42)
                     "Expected decoded request id 42, got ~S."
                     (gethash "id" decoded))
        (assert-true
         (string= (gethash "cursor" (gethash "params" decoded)) "abc123")
         "Expected decoded params cursor value."))

      (let* ((response (funcall make-response-fn
                                "req-roundtrip"
                                :result (make-obj "ok" t)))
             (frame (funcall frame-fn response))
             (decoded (funcall read-fn (make-string-input-stream frame))))
        (assert-true (string= (gethash "id" decoded) "req-roundtrip")
                     "Expected framed decode to preserve response id.")
        (assert-true (eq (gethash "ok" (gethash "result" decoded)) t)
                     "Expected framed decode to preserve result payload."))

      (let* ((client (funcall make-client-fn
                              :output-stream (make-string-output-stream)
                              :default-timeout-seconds 2.0d0))
             (result-a nil)
             (result-b nil))
        #+sb-thread
        (let* ((thread-a
                 (sb-thread:make-thread
                  (lambda ()
                    (setf result-a
                          (funcall send-request-fn
                                   client
                                   "demo/a"
                                   :request-id "req-a"
                                   :timeout-seconds 2.0d0)))
                  :name "amoebum-mcp-smoke-request-a"))
               (thread-b
                 (sb-thread:make-thread
                  (lambda ()
                    (setf result-b
                          (funcall send-request-fn
                                   client
                                   "demo/b"
                                   :request-id "req-b"
                                   :timeout-seconds 2.0d0)))
                  :name "amoebum-mcp-smoke-request-b")))
          (sleep 0.05)
          (funcall handle-incoming-fn
                   client
                   (funcall make-response-fn "req-b" :result (make-obj "value" "B")))
          (funcall handle-incoming-fn
                   client
                   (funcall make-response-fn "req-a" :result (make-obj "value" "A")))
          (sb-thread:join-thread thread-a)
          (sb-thread:join-thread thread-b))
        #-sb-thread
        (error "This smoke test requires SBCL thread support.")
        (assert-true (string= (gethash "value" (gethash "result" result-a)) "A")
                     "Expected req-a to correlate to response payload A.")
        (assert-true (string= (gethash "value" (gethash "result" result-b)) "B")
                     "Expected req-b to correlate to response payload B."))

      (let* ((client (funcall make-client-fn
                              :output-stream (make-string-output-stream)))
             (saw-timeout nil))
        (handler-case
            (funcall send-request-fn
                     client
                     "demo/timeout"
                     :request-id "req-timeout"
                     :timeout-seconds 0.05d0)
          (condition (condition)
            (when (typep condition timeout-condition-sym)
              (setf saw-timeout t))))
        (assert-true saw-timeout
                     "Expected MCP timeout condition for unresolved request."))

      (let* ((temp-root (merge-pathnames
                         (format nil "amoebum-mcp-smoke-~D-~D/"
                                 (get-universal-time)
                                 (random 1000000))
                         (uiop:temporary-directory)))
             (script-path (merge-pathnames "mock-mcp-server.py" temp-root))
             (python-command (resolve-python-command))
             (server nil)
             (restarted-pid nil))
        (ensure-directories-exist temp-root)
        (write-mock-server-script script-path)
        (unwind-protect
            (progn
              (setf server
                    (funcall make-server-fn
                             :name "smoke-mcp-server"
                             :command python-command
                             :args (list (namestring script-path))
                             :initialize-timeout-seconds 1.0d0
                             :ping-timeout-seconds 0.1d0
                             :health-check-interval-seconds 0.1d0
                             :restart-backoff-base-seconds 0.05d0
                             :restart-backoff-max-seconds 0.2d0
                             :shutdown-grace-seconds 0.2d0))
              (funcall server-start-fn server)
              (assert-true (funcall server-health-fn server)
                           "Expected MCP server initialize+ping health check to succeed.")
              (let* ((toolset (funcall make-toolset-fn))
                     (event-bus (funcall make-event-bus-fn :capacity 64))
                     (discovered-events 0)
                     (invoked-events 0)
                     (old-mcp-permissions
                      (funcall config-value-fn
                               :mcp-server-permissions
                               (funcall current-config-fn))))
                (funcall subscribe-fn
                         event-bus
                         event-type-mcp-discovered
                         (lambda (event)
                           (declare (ignore event))
                           (incf discovered-events)))
                ;; invoke-mcp-tool-bridge publishes mcp:tool-invoked on
                ;; (current-event-bus), not the context event-bus.
                (let ((global-bus (funcall (funcall fn-in "CURRENT-EVENT-BUS" amoebum-pkg))))
                  (funcall subscribe-fn
                           global-bus
                           event-type-mcp-invoked
                           (lambda (event)
                             (declare (ignore event))
                             (incf invoked-events))))
                (unwind-protect
                    (progn
                      (funcall setconfig-fn :mcp-server-permissions nil)
                      (let ((discovered
                              (funcall discover-mcp-tools-fn
                                       server
                                       :toolset toolset
                                       :event-bus event-bus)))
                        (assert-true (= (length discovered) 2)
                                     "Expected tools/list discovery to register two tools, got ~S."
                                     discovered)
                        (assert-true (member "mcp/smoke-mcp-server/echo" discovered
                                             :test #'string=)
                                     "Expected namespaced echo tool in discovery results.")
                        (assert-true (member "mcp/smoke-mcp-server/sum" discovered
                                             :test #'string=)
                                     "Expected namespaced sum tool in discovery results."))
                      (assert-true (= discovered-events 2)
                                   "Expected mcp:tool-discovered events for each tool.")

                      (let* ((blocked-context
                               (funcall make-context-fn
                                        :toolset toolset
                                        :permission-mode :full-auto
                                        :event-bus event-bus))
                             (blocked-call
                               (funcall make-tool-call-fn
                                        :id "mcp-blocked"
                                        :name "mcp/smoke-mcp-server/echo"
                                        :arguments "{\"text\":\"blocked\"}"))
                             (saw-denied nil))
                        (handler-case
                            (funcall execute-tool-fn blocked-call blocked-context)
                          (condition (condition)
                            (when (typep condition tool-permission-denied-sym)
                              (setf saw-denied t))))
                        (assert-true saw-denied
                                     "Expected default MCP permission decision to require prompt."))

                      (funcall setconfig-fn
                               :mcp-server-permissions
                               (list (cons "smoke-mcp-server" :allow)))
                      (let* ((allow-context
                               (funcall make-context-fn
                                        :toolset toolset
                                        :permission-mode :full-auto
                                        :event-bus event-bus))
                             (allow-call
                               (funcall make-tool-call-fn
                                        :id "mcp-allowed"
                                        :name "mcp/smoke-mcp-server/echo"
                                        :arguments "{\"text\":\"hello-mcp\"}"))
                             (output (funcall execute-tool-fn allow-call allow-context))
                             (wrapped (parse-json-text output))
                             (result (gethash "result" wrapped))
                             (structured (and (hash-table-p result)
                                              (gethash "structuredContent" result))))
                        (assert-true (string= (gethash "type" wrapped) "tool-result")
                                     "Expected MCP invocation wrapper type=tool-result.")
                        (assert-true (string= (gethash "name" wrapped)
                                              "mcp/smoke-mcp-server/echo")
                                     "Expected wrapped MCP tool result to preserve namespaced name.")
                        (assert-true (string= (gethash "echo" structured) "hello-mcp")
                                     "Expected tools/call result structuredContent echo value."))
                      (assert-true (= invoked-events 1)
                                   "Expected exactly one mcp:tool-invoked event after allow path."))
                  (funcall setconfig-fn :mcp-server-permissions old-mcp-permissions)))
              (let ((initial-pid
                      (server-process-pid (funcall server-process-fn server))))
                (assert-true (integerp initial-pid)
                             "Expected initial MCP server process pid, got ~S."
                             initial-pid)
                (kill-process (funcall server-process-fn server))
                (assert-true
                 (wait-until (lambda ()
                               (>= (funcall server-restart-count-fn server) 1))
                             4.0d0)
                 "Expected MCP health monitor to auto-restart crashed server.")
                (setf restarted-pid
                      (server-process-pid (funcall server-process-fn server)))
                (assert-true (integerp restarted-pid)
                             "Expected restarted MCP server process pid, got ~S."
                             restarted-pid)
                (assert-true (/= initial-pid restarted-pid)
                             "Expected MCP server restart to replace pid (~S -> ~S)."
                             initial-pid
                             restarted-pid))
              (assert-true (funcall server-health-fn server)
                           "Expected restarted MCP server health check to succeed.")
              (assert-true (funcall server-stop-fn server)
                           "Expected MCP server stop to complete graceful shutdown.")
              (assert-true (not (process-present-p restarted-pid))
                           "Expected MCP server process ~S to be fully reaped after shutdown."
                           restarted-pid))
          (ignore-errors
            (when server
              (funcall server-stop-fn server)))
          (ignore-errors
            (when (probe-file temp-root)
              (uiop:delete-directory-tree temp-root
                                          :validate t
                                          :if-does-not-exist :ignore)))))))

  (format t "AMOEBUM_MCP_SMOKE_OK~%"))
