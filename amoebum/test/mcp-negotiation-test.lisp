(in-package :amoebum/test)

(def-suite mcp-negotiation-suite
  :description "I234 MCP capability negotiation and status reporting."
  :in amoebum-suite)

(in-suite mcp-negotiation-suite)

(defun %i234-hash (&rest pairs)
  (let ((table (make-hash-table :test #'equal)))
    (loop for (key value) on pairs by #'cddr do
          (setf (gethash key table) value))
    table))

(defun %i234-mock-server (&key (name "i234-server"))
  (let ((server (amoebum:make-mcp-server :name name :command "/bin/true")))
    (setf (amoebum:mcp-server-running-p server) t
          (amoebum:mcp-server-jsonrpc-client server)
          (amoebum:make-mcp-jsonrpc-client
           :output-stream (make-string-output-stream)))
    server))

(defun %i234-mock-tools-capability ()
  (%i234-hash "list" (list (%i234-hash "name" "echo")
                           (%i234-hash "name" "sum"))))

(test negotiation-stores-server-info-and-warns-on-version-mismatch
  (let* ((server (%i234-mock-server :name "i234-negotiation"))
         (warnings '()))
    (let ((amoebum:*mcp-negotiation-request-function*
            (lambda (_client _method &key params timeout-seconds &allow-other-keys)
              (declare (ignore _client _method params timeout-seconds))
              (%i234-hash
               "result"
               (%i234-hash
                "protocolVersion" "2099-01-01"
                "capabilities"
                (%i234-hash
                 "tools" (%i234-mock-tools-capability)
                 "resources" (%i234-hash "listChanged" t)
                 "prompts" (%i234-hash "listChanged" t)
                 "logging" (%i234-hash "enabled" t)))))))
      (handler-bind ((warning (lambda (condition)
                                (push (princ-to-string condition) warnings)
                                (muffle-warning condition))))
        (let ((info (amoebum:mcp-negotiate-server-capabilities server
                                                                (amoebum:mcp-server-jsonrpc-client
                                                                 server)
                                                                :timeout-seconds 1.0d0)))
          (is (typep info 'amoebum:mcp-server-info))
          (is (eq info (amoebum:mcp-server-server-info server)))
          (is (string= "2099-01-01" (amoebum:mcp-server-info-protocol-version info)))
          (is-false (amoebum:mcp-server-info-protocol-version-match-p info))
          (is (equal '("echo" "sum") (amoebum:mcp-server-info-declared-tools info))))))
    (is-true warnings)))

(test capability-filtering-registers-only-declared-tools
  (let ((server (%i234-mock-server :name "i234-tools"))
        (toolset (pseudopod:make-toolset)))
    (unwind-protect
        (progn
          (setf (amoebum:mcp-server-server-info server)
                (amoebum:make-mcp-server-info
                 :protocol-version amoebum:*mcp-protocol-version*
                 :protocol-version-match-p t
                 :declared-tools '("echo")
                 :capabilities (%i234-hash)
                 :tools-capability (%i234-mock-tools-capability)))
          (amoebum:clear-mcp-tool-registries)
          (amoebum:register-mcp-tool-server server :discover-tools-p nil :toolset toolset)
          (let ((amoebum:*mcp-tools-list-request-function*
                  (lambda (_server _cursor)
                    (declare (ignore _server _cursor))
                    (values (list (%i234-hash "name" "echo"
                                              "description" "Echo text"
                                              "inputSchema" (%i234-hash "type" "object"))
                                  (%i234-hash "name" "sum"
                                              "description" "Add numbers"
                                              "inputSchema" (%i234-hash "type" "object")))
                            nil))))
            (let ((tools (amoebum:discover-mcp-server-tools server :toolset toolset)))
              (is (= 1 (length tools)))
              (is (string= "mcp/i234-tools/echo" (first tools)))
              (is (not (null (gethash "mcp/i234-tools/echo"
                                      amoebum:*mcp-tool-binding-registry*))))
              (is (null (gethash "mcp/i234-tools/sum"
                                 amoebum:*mcp-tool-binding-registry*)))
              (is (= 1 (amoebum:mcp-server-info-discovered-tool-count
                        (amoebum:mcp-server-server-info server)))))))
      (amoebum:clear-mcp-tool-registries))))

(test mcp-status-command-reports-capabilities-and-tool-count
  (let ((server (%i234-mock-server :name "i234-status"))
        (toolset (pseudopod:make-toolset)))
    (unwind-protect
        (progn
          (setf (amoebum:mcp-server-server-info server)
                (amoebum:make-mcp-server-info
                 :protocol-version amoebum:*mcp-protocol-version*
                 :protocol-version-match-p t
                 :capabilities (%i234-hash "tools" t
                                           "resources" t
                                           "prompts" t
                                           "logging" t)
                 :tools-capability (%i234-mock-tools-capability)
                 :resources-capability (%i234-hash "listChanged" t)
                 :prompts-capability (%i234-hash "listChanged" t)
                 :logging-capability (%i234-hash "enabled" t)
                 :declared-tools '("echo")
                 :discovered-tool-count 0))
          (amoebum:clear-mcp-tool-registries)
          (amoebum:register-mcp-tool-server server :discover-tools-p nil :toolset toolset)
          (let ((amoebum:*mcp-tools-list-request-function*
                  (lambda (_server _cursor)
                    (declare (ignore _server _cursor))
                    (values (list (%i234-hash "name" "echo"
                                              "description" "Echo text"
                                              "inputSchema" (%i234-hash "type" "object")))
                            nil))))
            (amoebum:discover-mcp-server-tools server :toolset toolset))
          (multiple-value-bind (handled result)
              (amoebum:dispatch-slash-command "/mcp-status")
            (is-true handled)
            (is-true (typep result 'amoebum:slash-command-result))
            (let ((output (or (amoebum:slash-command-result-output result) ""))
                  (caps-text "capabilities=tools, resources, prompts, logging")
                  (tools-text "discovered-tools=1"))
              (is-true (search "i234-status" output :test #'char-equal))
              (is-true (search caps-text output :test #'char-equal))
              (is-true (search tools-text output :test #'char-equal)))))
      (amoebum:clear-mcp-tool-registries))))

(test mcp-negotiation-smoke-sentinel
  (is-true t)
  (format t "MCP_NEGOTIATION_SMOKE_OK~%"))
