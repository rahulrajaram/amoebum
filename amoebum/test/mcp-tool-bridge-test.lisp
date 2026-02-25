(in-package :amoebum/test)

(def-suite mcp-tool-bridge-suite :in amoebum-suite
  :description "MCP tool bridge registry/deftool integration tests (I235).")

(in-suite mcp-tool-bridge-suite)

(defun %i235-ht (&rest pairs)
  (let ((table (make-hash-table :test #'equal)))
    (loop for (key value) on pairs by #'cddr do
          (setf (gethash key table) value))
    table))

(defun %i235-test-server (name)
  (let ((server (amoebum:make-mcp-server :name name :command "/bin/true" :args '())))
    (setf (amoebum:mcp-server-running-p server) t
          (amoebum:mcp-server-jsonrpc-client server)
          (amoebum:make-mcp-jsonrpc-client
           :input-stream (make-string-input-stream "")
           :output-stream (make-string-output-stream)))
    server))

(defun %i235-tools-list-response (tool-name &key (description "MCP test tool") (required nil))
  (%i235-ht
   "jsonrpc" "2.0"
   "id" "tools-list"
   "result"
   (%i235-ht
    "tools"
    (list
     (%i235-ht
      "name" tool-name
      "description" description
      "inputSchema"
      (%i235-ht
       "type" "object"
       "properties"
       (%i235-ht
        "text" (%i235-ht "type" "string" "description" "Echo text value"))
       "required" required))))))

(test mcp-tool-bridge-registers-wrappers-and-mappings
  (let ((old-toolset amoebum:*toolset*)
        (old-metadata amoebum:*tool-metadata*)
        (old-send-request (symbol-function 'amoebum:mcp-jsonrpc-send-request)))
    (unwind-protect
        (progn
          (setf amoebum:*toolset* (pseudopod:make-toolset)
                amoebum:*tool-metadata* (make-hash-table :test #'equal))
          (amoebum:clear-mcp-tool-registries)
          (setf (symbol-function 'amoebum:mcp-jsonrpc-send-request)
                (lambda (_client method &key params request-id timeout-seconds)
                  (declare (ignore _client request-id timeout-seconds))
                  (cond
                    ((string= method "tools/list")
                     (%i235-tools-list-response "echo" :description "Echo text" :required (list "text")))
                    ((string= method "tools/call")
                     (let* ((arguments (and (hash-table-p params)
                                            (gethash "arguments" params)))
                            (text (and (hash-table-p arguments)
                                       (gethash "text" arguments))))
                       (%i235-ht
                        "jsonrpc" "2.0"
                        "id" "tools-call"
                        "result" (%i235-ht
                                  "structuredContent" (%i235-ht "echo" text)
                                  "isError" nil))))
                    (t
                     (%i235-ht "jsonrpc" "2.0"
                               "id" "unknown"
                               "error" (%i235-ht "code" -32601
                                                 "message" "method not found"))))))
          (let* ((server (%i235-test-server "i235-bridge-server"))
                 (discovered (amoebum:register-mcp-tool-server server :discover-tools-p t))
                 (tool-name "mcp/i235-bridge-server/echo")
                 (tool-definition (pseudopod:find-tool amoebum:*toolset* tool-name))
                 (metadata (gethash tool-name amoebum:*tool-metadata*))
                 (call-result (funcall (pseudopod:tool-definition-fn tool-definition)
                                       (%i235-ht "text" "hello")
                                       nil))
                 (result-payload (gethash "result" call-result))
                 (structured (and (hash-table-p result-payload)
                                  (gethash "structuredContent" result-payload)))
                 (forward-map (amoebum:mcp-tool-name-for-amoebum-tool tool-name))
                 (reverse-map (amoebum:amoebum-tool-name-for-mcp-tool
                               "i235-bridge-server"
                               "echo")))
            (is-true (member tool-name discovered :test #'string=))
            (is-true tool-definition)
            (is-true metadata)
            (is (string= (amoebum:tool-metadata-mcp-server metadata) "i235-bridge-server"))
            (is (eq (amoebum:tool-metadata-category metadata) :mcp))
            (is (string= (gethash "type" call-result) "tool-result"))
            (is (string= (gethash "echo" structured) "hello"))
            (is (equal (getf forward-map :mcp-tool-name) "echo"))
            (is (string= reverse-map tool-name))))
      (setf (symbol-function 'amoebum:mcp-jsonrpc-send-request) old-send-request
            amoebum:*toolset* old-toolset
            amoebum:*tool-metadata* old-metadata)
      (amoebum:clear-mcp-tool-registries))))

(test mcp-tool-bridge-maps-jsonrpc-error-to-tool-condition
  (let ((old-toolset amoebum:*toolset*)
        (old-send-request (symbol-function 'amoebum:mcp-jsonrpc-send-request)))
    (unwind-protect
        (progn
          (setf amoebum:*toolset* (pseudopod:make-toolset))
          (amoebum:clear-mcp-tool-registries)
          (setf (symbol-function 'amoebum:mcp-jsonrpc-send-request)
                (lambda (_client method &key params request-id timeout-seconds)
                  (declare (ignore _client params request-id timeout-seconds))
                  (cond
                    ((string= method "tools/list")
                     (%i235-tools-list-response "badargs"))
                    ((string= method "tools/call")
                     (%i235-ht
                      "jsonrpc" "2.0"
                      "id" "tools-call"
                      "error" (%i235-ht "code" -32602 "message" "invalid params")))
                    (t
                     (%i235-ht "jsonrpc" "2.0" "id" "x" "result" (%i235-ht))))))
          (let* ((server (%i235-test-server "i235-error-server"))
                 (discovered (amoebum:register-mcp-tool-server server :discover-tools-p t))
                 (tool-definition (pseudopod:find-tool amoebum:*toolset* (first discovered)))
                 (saw-argument-error nil))
            (handler-case
                (funcall (pseudopod:tool-definition-fn tool-definition)
                         (%i235-ht "text" "hello")
                         nil)
              (amoebum:tool-argument-error ()
                (setf saw-argument-error t)))
            (is-true saw-argument-error
                     "Expected MCP JSON-RPC code -32602 to map to TOOL-ARGUMENT-ERROR.")))
      (setf (symbol-function 'amoebum:mcp-jsonrpc-send-request) old-send-request
            amoebum:*toolset* old-toolset)
      (amoebum:clear-mcp-tool-registries))))

(test mcp-tool-bridge-smoke-sentinel
  (format t "MCP_TOOL_BRIDGE_SMOKE_OK~%")
  (is-true t))
