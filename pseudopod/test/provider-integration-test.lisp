(in-package :pseudopod/test)

;;; ---------------------------------------------------------------------------
;;; End-to-end provider integration tests (I142)
;;; ---------------------------------------------------------------------------

(def-suite provider-integration-suite :in pseudopod-suite
  :description "Provider integration tests with lightweight mock SSE server.")

(in-suite provider-integration-suite)

(defstruct mock-provider-server
  thread
  socket
  stop-flag
  port)

(defun %make-hash (&rest kvs)
  (let ((table (make-hash-table :test #'equal)))
    (loop for (k v) on kvs by #'cddr
          do (setf (gethash k table) v))
    table))

(defun %append-sse-event (stream event payload)
  (format stream "event: ~A~%" event)
  (if payload
      (format stream "data: ~A~%~%" (jonathan:to-json payload))
      (format stream "data: [DONE]~%~%")))

(defparameter *mock-anthropic-streaming-response*
  (with-output-to-string (stream)
    (%append-sse-event
     stream "message_start"
     (%make-hash
      "type" "message_start"
      "message" (%make-hash
                 "role" "assistant"
                 "usage" (%make-hash "input_tokens" 12
                                     "output_tokens" 0))))
    (%append-sse-event
     stream "content_block_start"
     (%make-hash
      "type" "content_block_start"
      "index" 0
      "content_block" (%make-hash
                       "type" "text")))
    (%append-sse-event
     stream "content_block_delta"
     (%make-hash
      "type" "content_block_delta"
      "index" 0
      "delta" (%make-hash
               "type" "text_delta"
               "text" "Anthropic stream chunk.")))
    (%append-sse-event
     stream "content_block_stop"
     (%make-hash "type" "content_block_stop" "index" 0))
    (%append-sse-event
     stream "content_block_start"
     (%make-hash
      "type" "content_block_start"
      "index" 1
      "content_block" (%make-hash
                       "type" "tool_use"
                       "id" "anthropic-tool-call"
                       "name" "lookup")))
    (%append-sse-event
     stream "content_block_delta"
     (%make-hash
      "type" "content_block_delta"
      "index" 1
      "delta" (%make-hash
               "type" "input_json_delta"
               "partial_json" "{\"query\":\"Anthropic ")))
    (%append-sse-event
     stream "content_block_delta"
     (%make-hash
      "type" "content_block_delta"
      "index" 1
      "delta" (%make-hash
               "type" "input_json_delta"
               "partial_json" "integration\"}")))
    (%append-sse-event
     stream "content_block_stop"
     (%make-hash "type" "content_block_stop" "index" 1))
    (%append-sse-event
     stream "message_delta"
     (%make-hash
      "type" "message_delta"
      "usage" (%make-hash "input_tokens" 12
                          "output_tokens" 19)))))

(defparameter *mock-openai-streaming-response*
  (make-stream-sse-body
   (make-stream-sse-payload
    :role "assistant"
    :content "OpenAI stream "
    :tool-calls (list
                 (make-stream-tool-call-delta
                  :index 0
                  :id "openai-tool-call"
                  :type "function"
                  :name "lookup"
                  :arguments "{\"query\":\"OpenAI ")))
   (make-stream-sse-payload
    :content "integration"
    :tool-calls (list
                 (make-stream-tool-call-delta
                  :index 0
                  :arguments "response\"}")))
   (make-stream-sse-payload :content nil)))

(defun %read-mock-line (stream)
  (let ((line (read-line stream nil nil)))
    (and line (string-right-trim '(#\Return) line))))

(defun %read-mock-headers (stream)
  (let ((headers (make-hash-table :test #'equal)))
    (loop for line = (%read-mock-line stream)
          while (and line (plusp (length line))
                     (not (string= "" line)))
          do (let ((colon (position #\: line)))
               (when colon
                 (let ((name (string-downcase
                               (string-trim '(#\Space #\Tab)
                                           (subseq line 0 colon))))
                       (value (string-trim '(#\Space #\Tab)
                                           (subseq line (1+ colon)))))
                   (setf (gethash name headers) value)))))
    headers))

(defun %read-mock-body (stream headers)
  (let* ((content-length (ignore-errors
                           (parse-integer
                            (gethash "content-length" headers))))
         (length (if (and (integerp content-length) (plusp content-length))
                     content-length
                     0)))
    (if (zerop length)
        ""
        (let ((buffer (make-string length)))
          (let ((read-bytes (read-sequence buffer stream)))
            (if (and (integerp read-bytes) (plusp read-bytes))
                (subseq buffer 0 read-bytes)
                ""))))))

(defun %read-mock-request (stream)
  (let ((request-line (%read-mock-line stream)))
    (when (or (null request-line) (string= request-line ""))
      (return-from %read-mock-request (values nil nil nil)))
    (let* ((parts (uiop:split-string request-line :separator " " :remove-empty-subseqs t))
           (method (and (plusp (length parts)) (first parts)))
           (path (and (> (length parts) 1) (second parts)))
           (headers (%read-mock-headers stream))
           (body (%read-mock-body stream headers)))
      (values method path body))))

(defun %server-send-response (stream status reason body headers)
  (let ((payload (or body "")))
    (format stream "HTTP/1.1 ~A ~A~%" status reason)
    (format stream "Connection: close~%")
    (dolist (header headers)
      (destructuring-bind (name value) header
        (format stream "~A: ~A~%" name value)))
    (format stream "Content-Length: ~D~%" (length payload))
    (format stream "~%")
    (write-string payload stream)
    (finish-output stream)))

(defun %server-select-response (path body)
  (let* ((streaming-p (and (stringp body)
                           (let ((parsed-body (ignore-errors
                                                (jonathan:parse body :as :hash-table))))
                             (and (hash-table-p parsed-body)
                                  (gethash "stream" parsed-body)))))
         (is-fail (and path (search "/fail" path))))
    (cond
      (is-fail
       (values 429 "Too Many Requests"
               (list (list "Content-Type" "application/json"))
               "{\"error\":{\"message\":\"rate limited\"}}"))
      ((and path (search "/v1/messages" path))
       (values 200 "OK"
               (list (list "Content-Type" "text/event-stream; charset=utf-8"))
               (if streaming-p
                   *mock-anthropic-streaming-response*
                   "{\"role\":\"assistant\",\"content\":\"Anthropic fallback\"}")))
      ((and path (search "/chat/completions" path))
       (values 200 "OK"
               (list (list "Content-Type" "text/event-stream; charset=utf-8"))
               *mock-openai-streaming-response*))
      (t
       (values 404 "Not Found"
               (list (list "Content-Type" "application/json"))
               "{\"error\":\"not found\"}")))))

(defun %serve-mock-provider-connection (stream)
  (multiple-value-bind (method path body) (%read-mock-request stream)
    (declare (ignore method))
    (when path
      (multiple-value-bind (status reason headers payload)
          (%server-select-response path body)
        (%server-send-response stream status reason payload headers)))))

(defun start-mock-provider-server ()
  (let* ((stop-flag (cons nil nil))
         (listener (usocket:socket-listen "127.0.0.1" 0 :reuse-address t))
         (port (usocket:get-local-port listener)))
    (let ((server (make-mock-provider-server
                   :socket listener
                   :stop-flag stop-flag
                   :port port)))
      (setf (mock-provider-server-thread server)
            (sb-thread:make-thread
             (lambda ()
               (loop
                 (handler-case
                     (when (car stop-flag)
                       (return))
                   (error () nil))
                 (handler-case
                     (let ((conn (usocket:socket-accept listener)))
                       (handler-case
                           (let ((conn-stream (usocket:socket-stream conn)))
                             (%serve-mock-provider-connection conn-stream)
                             (ignore-errors (close conn-stream)))
                         (error (condition)
                           (warn "mock provider server connection error: ~A" condition)))
                       (ignore-errors (usocket:socket-close conn)))
                   (error (condition)
                     (unless (car stop-flag)
                       (warn "mock provider server accept error: ~A" condition))))))
             :name "mock-provider-server"))
      server)))

(defun stop-mock-provider-server (server)
  (let ((stop-flag (mock-provider-server-stop-flag server))
        (thread (mock-provider-server-thread server))
        (socket (mock-provider-server-socket server)))
    (when stop-flag
      (setf (car stop-flag) t))
    (ignore-errors (usocket:socket-close socket))
    (when thread
      (ignore-errors (sb-thread:join-thread thread)))))

(defmacro with-mock-provider-server ((server) &body body)
  `(let ((,server (start-mock-provider-server)))
     (unwind-protect
          (progn ,@body)
       (stop-mock-provider-server ,server))))

(defun %content-to-text (content)
  (cond
    ((stringp content) content)
    ((or (listp content) (vectorp content))
     (with-output-to-string (stream)
       (dolist (item (if (vectorp content) (coerce content 'list) content))
         (let ((text (and (hash-table-p item) (gethash "text" item))))
           (when (stringp text)
             (write-string text stream))))))
    (t "")))

(defun %tool-calls-list (tool-calls)
  (cond
    ((null tool-calls) nil)
    ((listp tool-calls) tool-calls)
    ((vectorp tool-calls) (coerce tool-calls 'list))
    (t nil)))

(defun %tool-call-name (tool-call)
  (cond
    ((not tool-call) "")
    ((pseudopod:tool-call-p tool-call)
     (pseudopod:tool-call-name tool-call))
    ((hash-table-p tool-call)
     (let ((tool-name (gethash "name" tool-call))
           (function-body (and (hash-table-p (gethash "function" tool-call))
                              (gethash "function" tool-call)))
           (function-name (and function-body (gethash "name" function-body))))
       (cond
         ((stringp function-name) function-name)
         ((stringp tool-name) tool-name)
         (t ""))))
    (t "")))

(defun %tool-call-arguments (tool-call)
  (cond
    ((not tool-call) "")
    ((pseudopod:tool-call-p tool-call)
     (or (pseudopod:tool-call-arguments tool-call) ""))
    ((hash-table-p tool-call)
     (let ((arguments (gethash "arguments" tool-call))
           (function-body (and (hash-table-p (gethash "function" tool-call))
                              (gethash "function" tool-call)))
           (function-arguments (and function-body (gethash "arguments" function-body))))
       (cond
         ((stringp function-arguments) function-arguments)
         ((stringp arguments) arguments)
         (t ""))))
    (t "")))

(defun %mock-server-url (server &optional path)
  (if path
      (format nil "http://127.0.0.1:~A~A" (mock-provider-server-port server) path)
      (format nil "http://127.0.0.1:~A" (mock-provider-server-port server))))

(test provider-integration-anthropic-streaming-mock
  (with-mock-provider-server (server)
    (let* ((provider (pseudopod:make-anthropic-provider
                      :api-key "sk-ant-test"
                      :base-url (%mock-server-url server)))
           (chunks nil))
      (let ((result (pseudopod:send-streaming-completion
                     provider
                     (list (pseudopod:make-message :role "user"
                                                   :content "Tell me about mocks"))
                     (lambda (chunk)
                       (push chunk chunks))
                     :model "claude-sonnet-4-5-20250929")))
        (is (= 1 (length chunks)))
        (is (string= "Anthropic stream chunk." (first chunks)))
        (is (string= "assistant" (or (gethash "role" result) "")))
        (is (string= "Anthropic stream chunk."
                     (%content-to-text (gethash "content" result))))
        (is (= 12 (gethash "input_tokens" (gethash "usage" result))))
        (let ((tool-calls (%tool-calls-list (gethash "tool_calls" result))))
          (is (= 1 (length tool-calls)))
          (is (string= "lookup" (%tool-call-name (first tool-calls))))
          (is (string= "{\"query\":\"Anthropic integration\"}"
                       (%tool-call-arguments (first tool-calls)))))))))

(test provider-integration-openai-streaming-mock
  (with-mock-provider-server (server)
    (let* ((provider (pseudopod:make-openai-compatible-provider
                      :api-key "sk-openai-test"
                      :base-url (%mock-server-url server)
                      :name "openai"))
           (chunks nil))
      (let ((result (pseudopod:send-streaming-completion
                     provider
                     (list (pseudopod:make-message :role "user"
                                                   :content "Test openai stream"))
                     (lambda (chunk)
                       (push chunk chunks))
                     :model "gpt-4o")))
        (is (= 2 (length chunks)))
        (is (equal '("OpenAI stream " "integration") (reverse chunks)))
        (is (string= "assistant" (or (gethash "role" result) "")))
        (is (string= "OpenAI stream integration"
                     (%content-to-text (gethash "content" result))))
        (let ((tool-calls (%tool-calls-list (gethash "tool_calls" result)))
          (is (= 1 (length tool-calls)))
          (is (string= "lookup" (%tool-call-name (first tool-calls))))
          (is (string= "{\"query\":\"OpenAI response\"}"
                       (%tool-call-arguments (first tool-calls)))))))))

(test provider-integration-rate-limit-degrades-provider
  (with-mock-provider-server (server)
    (let* ((provider (pseudopod:make-openai-compatible-provider
                      :api-key "sk-openai-test"
                      :base-url (%mock-server-url server "/fail")
                      :name "openai"))
           (router (pseudopod:make-model-router :strategy :fallback-chain))
           (status-code nil))
      (pseudopod:router-add-provider router provider)
      (handler-case
          (pseudopod:router-streaming-completion
           router
           (list (pseudopod:make-message :role "user" :content "Trigger failure"))
           (lambda (chunk) (declare (ignore chunk))))
        (pseudopod:pseudopod-api-error (condition)
          (setf status-code (pseudopod:pseudopod-api-error-status-code condition))))
      (is (= 429 status-code))
      (is (eql nil (pseudopod:provider-healthy-p provider))))))

(test provider-integration-router-fallback-chain
  (with-mock-provider-server (server)
    (let* ((primary (pseudopod:make-openai-compatible-provider
                      :api-key "sk-openai-primary"
                      :base-url (%mock-server-url server "/fail")
                      :name "openai-primary"))
           (backup (pseudopod:make-openai-compatible-provider
                    :api-key "sk-openai-backup"
                    :base-url (%mock-server-url server)
                    :name "openai-backup"))
           (router (pseudopod:make-model-router :strategy :fallback-chain))
           (chunks nil))
      (pseudopod:router-add-provider router backup)
      (pseudopod:router-add-provider router primary)
      (let ((result (pseudopod:router-streaming-completion
                     router
                     (list (pseudopod:make-message :role "user" :content "Use fallback"))
                     (lambda (chunk) (push chunk chunks))
                     :model "gpt-4o")))
        (is (= 2 (length chunks)))
        (is (string= "assistant" (or (gethash "role" result) "")))
        (is (string= "OpenAI stream integration"
                     (%content-to-text (gethash "content" result))))
        (is (= 1 (length (%tool-calls-list (gethash "tool_calls" result)))))
        (is (eql nil (pseudopod:provider-healthy-p primary)))
        (is (eql t (pseudopod:provider-healthy-p backup))))))))
