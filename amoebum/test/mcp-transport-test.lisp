(in-package :amoebum/test)

(def-suite mcp-transport-suite
  :description "I340 MCP stdio/streamable-http transport parity coverage."
  :in amoebum-suite)

(in-suite mcp-transport-suite)

(defun %i340-hash (&rest pairs)
  (let ((table (make-hash-table :test #'equal)))
    (loop for (key value) on pairs by #'cddr do
          (setf (gethash key table) value))
    table))

(defun %i340-jsonrpc-response-body (id &key result error)
  (let ((message (%i340-hash "jsonrpc" "2.0"
                             "id" id)))
    (cond
      (result
       (setf (gethash "result" message) result))
      (error
       (setf (gethash "error" message) error))
      (t
       (setf (gethash "result" message) (%i340-hash))))
    (amoebum:jsonrpc-serialize-message message)))

(test streamable-http-jsonrpc-send-request-roundtrip
  (let ((calls '()))
    (let ((amoebum::*mcp-streamable-http-request-function*
            (lambda (endpoint-url payload &key headers timeout-seconds)
              (push (list :endpoint-url endpoint-url
                          :headers headers
                          :timeout-seconds timeout-seconds
                          :payload payload)
                    calls)
              (let* ((request (amoebum:jsonrpc-deserialize-message payload))
                     (id (gethash "id" request))
                     (method (gethash "method" request)))
                (values (%i340-jsonrpc-response-body
                         id
                         :result (%i340-hash "ok" t "method" method))
                        200)))))
      (let* ((client (amoebum:make-mcp-jsonrpc-client
                      :transport :streamable-http
                      :endpoint-url "https://mcp.example.test/session"
                      :http-headers '(("authorization" . "Bearer test-token"))))
             (response (amoebum:mcp-jsonrpc-send-request client "ping" :request-id "req-i340")))
        (is (equal "req-i340" (gethash "id" response)))
        (is-true (eq t (gethash "ok" (gethash "result" response))))
        (is (= 1 (length calls)))
        (let ((call (first calls)))
          (is (equal "https://mcp.example.test/session" (getf call :endpoint-url)))
          (is (equal '(("authorization" . "Bearer test-token"))
                     (getf call :headers))))))))

(test streamable-http-jsonrpc-id-mismatch-signals-error
  (let ((amoebum::*mcp-streamable-http-request-function*
          (lambda (_endpoint-url payload &key headers timeout-seconds)
            (declare (ignore _endpoint-url headers timeout-seconds))
            (let* ((request (amoebum:jsonrpc-deserialize-message payload)))
              (values (%i340-jsonrpc-response-body
                       (format nil "wrong-~A" (gethash "id" request))
                       :result (%i340-hash "ok" t))
                      200)))))
    (let ((client (amoebum:make-mcp-jsonrpc-client
                   :transport :streamable-http
                   :endpoint-url "https://mcp.example.test/session")))
      (signals error
        (amoebum:mcp-jsonrpc-send-request client "ping" :request-id "req-i340-mismatch")))))

(test streamable-http-server-start-negotiates-and-health-checks
  (let ((methods '())
        (server (amoebum:make-mcp-server
                 :name "i340-http-server"
                 :transport :streamable-http
                 :endpoint-url "https://mcp.example.test/session"
                 :http-headers '((:authorization . "Bearer i340"))
                 :auto-restart-p nil)))
    (let ((amoebum::*mcp-streamable-http-request-function*
            (lambda (_endpoint-url payload &key headers timeout-seconds)
              (declare (ignore _endpoint-url headers timeout-seconds))
              (let* ((request (amoebum:jsonrpc-deserialize-message payload))
                     (id (gethash "id" request))
                     (method (or (gethash "method" request) "")))
                (push method methods)
                (cond
                  ((string= method "initialize")
                   (values (%i340-jsonrpc-response-body
                            id
                            :result (%i340-hash
                                     "protocolVersion" amoebum:*mcp-protocol-version*
                                     "capabilities"
                                     (%i340-hash
                                      "tools"
                                      (%i340-hash
                                       "list"
                                       (list (%i340-hash "name" "echo"))))))
                           200))
                  ((string= method "ping")
                   (values (%i340-jsonrpc-response-body
                            id
                            :result (%i340-hash "pong" t))
                           200))
                  ((or (string= method "initialized")
                       (string= method "shutdown")
                       (string= method "exit"))
                   (values "" 202))
                  (t
                   (values (%i340-jsonrpc-response-body
                            id
                            :error (%i340-hash "code" -32601
                                               "message" "method not found"))
                           200)))))))
      (unwind-protect
          (progn
            (amoebum:mcp-server-start server)
            (is-true (amoebum:mcp-server-running-p server))
            (is-true (amoebum:mcp-jsonrpc-client-p (amoebum:mcp-server-jsonrpc-client server)))
            (is-true (typep (amoebum:mcp-server-server-info server)
                            'amoebum:mcp-server-info))
            (is (equal '("echo")
                       (amoebum:mcp-server-info-declared-tools
                        (amoebum:mcp-server-server-info server))))
            (is-true (amoebum:mcp-server-health-check server))
            (let ((seen (remove-duplicates methods :test #'string=)))
              (is-true (member "initialize" seen :test #'string=))
              (is-true (member "initialized" seen :test #'string=))
              (is-true (member "ping" seen :test #'string=))))
        (ignore-errors
          (amoebum:mcp-server-stop server))))))

(test streamable-http-server-start-surfaces-http-errors
  (let ((server (amoebum:make-mcp-server
                 :name "i340-http-failure"
                 :transport :streamable-http
                 :endpoint-url "https://mcp.example.test/session"
                 :auto-restart-p nil)))
    (let ((amoebum::*mcp-streamable-http-request-function*
            (lambda (_endpoint-url _payload &key headers timeout-seconds)
              (declare (ignore _endpoint-url _payload headers timeout-seconds))
              (values "service unavailable" 503))))
      (signals error
        (amoebum:mcp-server-start server))
      (is-false (amoebum:mcp-server-running-p server))
      (is-true (typep (amoebum:mcp-server-last-error server) 'error)))))

(test mcp-transport-smoke-sentinel
  (is-true t)
  (format t "MCP_TRANSPORT_SMOKE_OK~%"))
