(in-package :amoebum)

(defparameter *mcp-jsonrpc-default-timeout-seconds* 30.0d0)
(defparameter *mcp-jsonrpc-poll-interval-seconds* 0.01d0)
(defparameter *mcp-jsonrpc-known-transports* '(:stdio :streamable-http))

(defparameter *mcp-streamable-http-request-function* nil
  "Optional transport override for MCP streamable HTTP requests.
Signature: (endpoint-url payload &key headers timeout-seconds) ->
           (values response-body status-code).")

(define-condition mcp-timeout (amoebum-error)
  ((request-id :initarg :request-id
               :initform nil
               :reader mcp-timeout-request-id)
   (timeout-seconds :initarg :timeout-seconds
                    :initform *mcp-jsonrpc-default-timeout-seconds*
                    :reader mcp-timeout-timeout-seconds))
  (:report (lambda (condition stream)
             (let ((request-id (mcp-timeout-request-id condition))
                   (timeout-seconds (mcp-timeout-timeout-seconds condition)))
               (if request-id
                   (format stream "MCP request ~S timed out after ~,2F seconds."
                           request-id
                           timeout-seconds)
                   (format stream "MCP request timed out after ~,2F seconds."
                           timeout-seconds))))))

(defstruct (mcp-pending-response
            (:constructor make-mcp-pending-response ()))
  (ready-p nil :type boolean)
  response)

(defstruct (mcp-jsonrpc-client
            (:constructor %make-mcp-jsonrpc-client
                (&key
                   (transport :stdio)
                   endpoint-url
                   (http-headers nil)
                   input-stream
                   output-stream
                   (default-timeout-seconds *mcp-jsonrpc-default-timeout-seconds*))))
  (transport :stdio)
  endpoint-url
  (http-headers nil)
  input-stream
  output-stream
  (default-timeout-seconds *mcp-jsonrpc-default-timeout-seconds* :type real)
  (next-request-id 0 :type integer)
  (pending-responses (make-hash-table :test #'equal))
  (pending-lock (bordeaux-threads:make-lock "amoebum-mcp-jsonrpc-pending-lock"))
  (write-lock (bordeaux-threads:make-lock "amoebum-mcp-jsonrpc-write-lock"))
  (notifications nil)
  (notification-lock (bordeaux-threads:make-lock "amoebum-mcp-jsonrpc-notification-lock"))
  (reader-running-p nil :type boolean)
  reader-thread
  reader-error)

(defmacro %with-mcp-lock ((lock) &body body)
  `(bordeaux-threads:with-lock-held (,lock)
     ,@body))

(defun %normalize-jsonrpc-method (method)
  (let ((normalized (and method
                         (string-trim '(#\Space #\Tab #\Newline #\Return)
                                      (princ-to-string method)))))
    (unless (and normalized (> (length normalized) 0))
      (error "JSON-RPC METHOD must be a non-empty string or symbol."))
    normalized))

(defun %normalize-mcp-timeout-seconds (timeout-seconds)
  (let ((normalized (or timeout-seconds *mcp-jsonrpc-default-timeout-seconds*)))
    (unless (and (realp normalized) (> normalized 0))
      (error "TIMEOUT-SECONDS must be a positive real number, got ~S."
             timeout-seconds))
    (float normalized 1.0d0)))

(defun %require-stream-or-nil (value label)
  (unless (or (null value) (streamp value))
    (error "~A must be a stream or NIL, got ~S." label value))
  value)

(defun %normalize-mcp-jsonrpc-transport (transport)
  (let ((normalized
          (cond
            ((keywordp transport) transport)
            ((symbolp transport)
             (intern (string-upcase (symbol-name transport)) :keyword))
            ((stringp transport)
             (intern (string-upcase
                      (string-trim '(#\Space #\Tab #\Newline #\Return)
                                   (substitute #\- #\_ transport)))
                     :keyword))
            (t :stdio))))
    (unless (member normalized *mcp-jsonrpc-known-transports* :test #'eq)
      (error "Unsupported MCP JSON-RPC transport ~S (expected one of ~S)."
             transport
             *mcp-jsonrpc-known-transports*))
    normalized))

(defun %normalize-mcp-jsonrpc-endpoint-url (endpoint-url)
  (unless (stringp endpoint-url)
    (error "MCP JSON-RPC ENDPOINT-URL must be a string, got ~S." endpoint-url))
  (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) endpoint-url)))
    (unless (> (length trimmed) 0)
      (error "MCP JSON-RPC ENDPOINT-URL must not be empty."))
    trimmed))

(defun %normalize-mcp-jsonrpc-http-headers (headers)
  (unless (or (null headers) (listp headers))
    (error "MCP JSON-RPC HTTP-HEADERS must be a list of string pairs, got ~S."
           headers))
  (loop for entry in (or headers '())
        collect
        (cond
          ((and (consp entry)
                (stringp (car entry))
                (stringp (cdr entry)))
           entry)
          ((and (consp entry)
                (keywordp (car entry))
                (stringp (cdr entry)))
           (cons (string-downcase (symbol-name (car entry)))
                 (cdr entry)))
          (t
           (error "Invalid MCP JSON-RPC HTTP header entry ~S." entry)))))

(defun %jonathan-function (name)
  (let* ((jonathan-package (find-package :jonathan))
         (symbol (and jonathan-package
                      (find-symbol name jonathan-package))))
    (unless (and symbol (fboundp symbol))
      (error "Jonathan function ~A is unavailable." name))
    (symbol-function symbol)))

(defun %json-serialize (value)
  (funcall (%jonathan-function "TO-JSON") value))

(defun %json-deserialize (payload)
  (funcall (%jonathan-function "PARSE")
           payload
           :as :hash-table
           :junk-allowed t))

(defun %blank-string-p (value)
  (or (null value)
      (zerop (length (string-trim '(#\Space #\Tab #\Newline #\Return)
                                  (princ-to-string value))))))

(defun %jsonrpc-2.0-p (message)
  (and (hash-table-p message)
       (string= (or (gethash "jsonrpc" message) "")
                "2.0")))

(defun %utf-8-octet-length (text)
  #+sbcl
  (length (sb-ext:string-to-octets text :external-format :utf-8))
  #-sbcl
  (length text))

(defun %header-value (headers name)
  (gethash (string-downcase name) headers))

(defun %parse-header-line (line)
  (let ((separator (position #\: line)))
    (unless separator
      (error "Malformed MCP JSON-RPC header line: ~S." line))
    (values
     (string-trim '(#\Space #\Tab)
                  (subseq line 0 separator))
     (string-trim '(#\Space #\Tab #\Return)
                  (subseq line (1+ separator))))))

(defun %read-header-block (stream)
  (let ((headers (make-hash-table :test #'equal))
        (saw-header nil))
    (loop
      for raw-line = (read-line stream nil nil)
      do
         (cond
           ((null raw-line)
            (if saw-header
                (error "Unexpected EOF while reading MCP JSON-RPC headers.")
                (return nil)))
           (t
            (let ((line (string-trim '(#\Space #\Tab #\Return) raw-line)))
              (if (zerop (length line))
                  (when saw-header
                    (return headers))
                  (multiple-value-bind (header-name header-value)
                      (%parse-header-line line)
                    (setf saw-header t
                          (gethash (string-downcase header-name) headers)
                          header-value)))))))))

(defun %response-message-p (message)
  (and (hash-table-p message)
       (nth-value 1 (gethash "id" message))
       (or (nth-value 1 (gethash "result" message))
           (nth-value 1 (gethash "error" message)))))

(defun %next-request-id (client)
  (%with-mcp-lock ((mcp-jsonrpc-client-pending-lock client))
    (incf (mcp-jsonrpc-client-next-request-id client))))

(defun %register-pending-response (client request-id)
  (%with-mcp-lock ((mcp-jsonrpc-client-pending-lock client))
    (setf (gethash request-id (mcp-jsonrpc-client-pending-responses client))
          (make-mcp-pending-response))))

(defun %take-ready-response (client request-id)
  (%with-mcp-lock ((mcp-jsonrpc-client-pending-lock client))
    (let ((pending (gethash request-id (mcp-jsonrpc-client-pending-responses client))))
      (when (and pending (mcp-pending-response-ready-p pending))
        (remhash request-id (mcp-jsonrpc-client-pending-responses client))
        (mcp-pending-response-response pending)))))

(defun %clear-pending-response (client request-id)
  (%with-mcp-lock ((mcp-jsonrpc-client-pending-lock client))
    (remhash request-id (mcp-jsonrpc-client-pending-responses client))))

(defun %resolve-pending-response (client request-id response)
  (%with-mcp-lock ((mcp-jsonrpc-client-pending-lock client))
    (let ((pending (gethash request-id (mcp-jsonrpc-client-pending-responses client))))
      (when pending
        (setf (mcp-pending-response-response pending) response
              (mcp-pending-response-ready-p pending) t))
      (not (null pending)))))

(defun %enqueue-notification (client message)
  (%with-mcp-lock ((mcp-jsonrpc-client-notification-lock client))
    (push message (mcp-jsonrpc-client-notifications client)))
  :notification)

(defun %await-response (client request-id timeout-seconds)
  (let* ((timeout-seconds (%normalize-mcp-timeout-seconds timeout-seconds))
         (deadline (+ (get-internal-real-time)
                      (ceiling (* timeout-seconds internal-time-units-per-second)))))
    (loop
      for response = (%take-ready-response client request-id)
      when response do (return response)
      do
         (unless (mcp-jsonrpc-client-reader-running-p client)
           (ignore-errors (mcp-jsonrpc-poll client :block-p nil)))
         (when (>= (get-internal-real-time) deadline)
           (%clear-pending-response client request-id)
           (error 'mcp-timeout
                  :request-id request-id
                  :timeout-seconds timeout-seconds))
         (sleep *mcp-jsonrpc-poll-interval-seconds*))))

(defun %mcp-streamable-http-default-request (endpoint-url payload
                                             &key headers timeout-seconds)
  (let* ((timeout (%normalize-mcp-timeout-seconds timeout-seconds))
         (args (append (list "curl"
                             "--silent"
                             "--show-error"
                             "--location"
                             "--write-out"
                             "\n%{http_code}"
                             "--max-time"
                             (format nil "~,2F" timeout)
                             "--request"
                             "POST"
                             "--header"
                             "Content-Type: application/json")
                       (loop for (header . value) in headers
                             append (list "--header"
                                          (format nil "~A: ~A" header value)))
                       (list "--data" payload endpoint-url))))
    (multiple-value-bind (stdout stderr exit-code)
        (uiop:run-program args
                          :ignore-error-status t
                          :output :string
                          :error-output :string)
      (unless (and (integerp exit-code) (zerop exit-code))
        (error "MCP streamable-http request failed (curl exit ~S): ~A"
               exit-code
               (string-trim '(#\Space #\Tab #\Newline #\Return)
                            (or stderr ""))))
      (let* ((lines (uiop:split-string (or stdout "") :separator '(#\Newline)))
             (status-line (or (car (last lines)) "0"))
             (status-code (or (parse-integer status-line :junk-allowed t) 0))
             (body (format nil "~{~A~^~%~}" (butlast lines))))
        (values body status-code)))))

(defun %mcp-streamable-http-request (endpoint-url payload
                                     &key headers timeout-seconds)
  (funcall (or *mcp-streamable-http-request-function*
               #'%mcp-streamable-http-default-request)
           endpoint-url
           payload
           :headers headers
           :timeout-seconds timeout-seconds))

(defun %mcp-streamable-http-send-message (client message timeout-seconds)
  (let* ((payload (jsonrpc-serialize-message message))
         (endpoint-url (or (mcp-jsonrpc-client-endpoint-url client)
                           (error "MCP streamable-http client has no ENDPOINT-URL.")))
         (headers (mcp-jsonrpc-client-http-headers client)))
    (multiple-value-bind (body status-code)
        (%mcp-streamable-http-request endpoint-url
                                      payload
                                      :headers headers
                                      :timeout-seconds timeout-seconds)
      (unless (and (integerp status-code) (<= 200 status-code) (< status-code 300))
        (error "MCP streamable-http request to ~A failed with HTTP status ~S."
               endpoint-url
               status-code))
      (unless (%blank-string-p body)
        (let ((decoded (%json-deserialize body)))
          (unless (%jsonrpc-2.0-p decoded)
            (error "Decoded streamable-http payload is not JSON-RPC 2.0: ~S."
                   decoded))
          decoded)))))

(defun make-mcp-jsonrpc-client (&key
                                  (transport :stdio)
                                  endpoint-url
                                  (http-headers nil)
                                  input-stream
                                  output-stream
                                  (default-timeout-seconds
                                    *mcp-jsonrpc-default-timeout-seconds*)
                                  (start-reader-p nil))
  (let* ((normalized-transport (%normalize-mcp-jsonrpc-transport transport))
         (normalized-endpoint-url
           (when (eq normalized-transport :streamable-http)
             (%normalize-mcp-jsonrpc-endpoint-url endpoint-url)))
         (normalized-headers
           (when (eq normalized-transport :streamable-http)
             (%normalize-mcp-jsonrpc-http-headers http-headers)))
         (client (%make-mcp-jsonrpc-client
                  :transport normalized-transport
                  :endpoint-url normalized-endpoint-url
                  :http-headers normalized-headers
                  :input-stream (%require-stream-or-nil input-stream "INPUT-STREAM")
                  :output-stream (%require-stream-or-nil output-stream "OUTPUT-STREAM")
                  :default-timeout-seconds
                  (%normalize-mcp-timeout-seconds default-timeout-seconds))))
    (when start-reader-p
      (mcp-jsonrpc-start-reader client))
    client))

(defun make-jsonrpc-request-message (method &key (params nil params-supplied-p) id)
  (when (null id)
    (error "JSON-RPC request requires a non-NIL ID."))
  (let ((message (make-hash-table :test #'equal)))
    (setf (gethash "jsonrpc" message) "2.0"
          (gethash "method" message) (%normalize-jsonrpc-method method)
          (gethash "id" message) id)
    (when params-supplied-p
      (setf (gethash "params" message) params))
    message))

(defun make-jsonrpc-notification-message (method &key (params nil params-supplied-p))
  (let ((message (make-hash-table :test #'equal)))
    (setf (gethash "jsonrpc" message) "2.0"
          (gethash "method" message) (%normalize-jsonrpc-method method))
    (when params-supplied-p
      (setf (gethash "params" message) params))
    message))

(defun make-jsonrpc-response-message (id &key (result nil result-supplied-p)
                                           (error nil error-supplied-p))
  (when (eq id nil)
    (error "JSON-RPC response requires an ID."))
  (when (and result-supplied-p error-supplied-p)
    (error "JSON-RPC response cannot contain both RESULT and ERROR."))
  (unless (or result-supplied-p error-supplied-p)
    (error "JSON-RPC response requires either RESULT or ERROR."))
  (let ((message (make-hash-table :test #'equal)))
    (setf (gethash "jsonrpc" message) "2.0"
          (gethash "id" message) id)
    (if result-supplied-p
        (setf (gethash "result" message) result)
        (setf (gethash "error" message) error))
    message))

(defun jsonrpc-serialize-message (message)
  (unless (hash-table-p message)
    (error "Expected JSON-RPC message hash-table, got ~S." message))
  (unless (%jsonrpc-2.0-p message)
    (error "JSON-RPC message missing \"jsonrpc\":\"2.0\" field."))
  (%json-serialize message))

(defun jsonrpc-deserialize-message (payload)
  (unless (stringp payload)
    (error "JSON-RPC payload must be a string, got ~S." payload))
  (let ((message (%json-deserialize payload)))
    (unless (%jsonrpc-2.0-p message)
      (error "Decoded payload is not a JSON-RPC 2.0 message."))
    message))

(defun jsonrpc-frame-message (message)
  (let* ((payload (if (stringp message)
                      message
                      (jsonrpc-serialize-message message)))
         (content-length (%utf-8-octet-length payload)))
    (format nil "Content-Length: ~D~C~C~C~C~A"
            content-length
            #\Return
            #\Linefeed
            #\Return
            #\Linefeed
            payload)))

(defun jsonrpc-write-message (stream message)
  (unless (streamp stream)
    (error "STREAM must be a stream, got ~S." stream))
  (write-string (jsonrpc-frame-message message) stream)
  (finish-output stream)
  message)

(defun jsonrpc-read-message (stream &key (eof-error-p nil))
  (unless (streamp stream)
    (error "STREAM must be a stream, got ~S." stream))
  (let ((headers (%read-header-block stream)))
    (when (null headers)
      (if eof-error-p
          (error 'end-of-file :stream stream)
          (return-from jsonrpc-read-message nil)))
    (let* ((length-text (%header-value headers "content-length")))
      (unless length-text
        (error "Missing Content-Length header in MCP JSON-RPC message."))
      (let* ((content-length (parse-integer length-text :junk-allowed t)))
        (unless (and content-length (>= content-length 0))
          (error "Invalid Content-Length value ~S." length-text))
        (let ((payload (make-string content-length)))
          (let ((count (read-sequence payload stream)))
            (unless (= count content-length)
              (error "Unexpected EOF while reading MCP JSON-RPC payload (expected ~D chars, got ~D)."
                     content-length
                     count))
            (jsonrpc-deserialize-message payload)))))))

(defun mcp-jsonrpc-send-message (client message &key timeout-seconds)
  (unless (mcp-jsonrpc-client-p client)
    (error "CLIENT must be an MCP-JSONRPC-CLIENT, got ~S." client))
  (case (mcp-jsonrpc-client-transport client)
    (:streamable-http
     (%mcp-streamable-http-send-message client message timeout-seconds))
    (otherwise
     (let ((output (mcp-jsonrpc-client-output-stream client)))
       (unless output
         (error "MCP JSON-RPC client has no output stream."))
       (%with-mcp-lock ((mcp-jsonrpc-client-write-lock client))
         (jsonrpc-write-message output message))))))

(defun mcp-jsonrpc-send-notification (client method &key (params nil params-supplied-p))
  (mcp-jsonrpc-send-message
   client
   (if params-supplied-p
       (make-jsonrpc-notification-message method :params params)
       (make-jsonrpc-notification-message method))))

(defun mcp-jsonrpc-send-request (client method &key
                                         (params nil params-supplied-p)
                                         (request-id nil request-id-supplied-p)
                                         timeout-seconds)
  (unless (mcp-jsonrpc-client-p client)
    (error "CLIENT must be an MCP-JSONRPC-CLIENT, got ~S." client))
  (let* ((id (if request-id-supplied-p
                 request-id
                 (%next-request-id client)))
         (timeout (or timeout-seconds
                      (mcp-jsonrpc-client-default-timeout-seconds client))))
    (if (eq (mcp-jsonrpc-client-transport client) :streamable-http)
        (let* ((request (if params-supplied-p
                            (make-jsonrpc-request-message method :params params :id id)
                            (make-jsonrpc-request-message method :id id)))
               (response (mcp-jsonrpc-send-message client request :timeout-seconds timeout)))
          (unless (hash-table-p response)
            (error "MCP streamable-http request ~S returned no JSON-RPC response."
                   id))
          (unless (equal (gethash "id" response) id)
            (error "MCP streamable-http response id mismatch for request ~S (got ~S)."
                   id
                   (gethash "id" response)))
          response)
        (progn
          (%register-pending-response client id)
          (unwind-protect
              (progn
                (if params-supplied-p
                    (mcp-jsonrpc-send-message
                     client
                     (make-jsonrpc-request-message method :params params :id id))
                    (mcp-jsonrpc-send-message
                     client
                     (make-jsonrpc-request-message method :id id)))
                (%await-response client id timeout))
            (%clear-pending-response client id))))))

(defun mcp-jsonrpc-handle-incoming-message (client message)
  (unless (mcp-jsonrpc-client-p client)
    (error "CLIENT must be an MCP-JSONRPC-CLIENT, got ~S." client))
  (unless (hash-table-p message)
    (error "MESSAGE must be a hash-table, got ~S." message))
  (if (%response-message-p message)
      (if (%resolve-pending-response client
                                     (gethash "id" message)
                                     message)
          :response
          :unmatched-response)
      (%enqueue-notification client message)))

(defun mcp-jsonrpc-poll (client &key (block-p nil))
  (unless (mcp-jsonrpc-client-p client)
    (error "CLIENT must be an MCP-JSONRPC-CLIENT, got ~S." client))
  (when (eq (mcp-jsonrpc-client-transport client) :streamable-http)
    (return-from mcp-jsonrpc-poll nil))
  (let ((input (mcp-jsonrpc-client-input-stream client)))
    (when (null input)
      (return-from mcp-jsonrpc-poll nil))
    (when (or block-p
              (listen input))
      (let ((message (jsonrpc-read-message input :eof-error-p nil)))
        (when message
          (mcp-jsonrpc-handle-incoming-message client message))
        message))))

(defun mcp-jsonrpc-drain-notifications (client)
  (unless (mcp-jsonrpc-client-p client)
    (error "CLIENT must be an MCP-JSONRPC-CLIENT, got ~S." client))
  (%with-mcp-lock ((mcp-jsonrpc-client-notification-lock client))
    (prog1 (nreverse (copy-list (mcp-jsonrpc-client-notifications client)))
      (setf (mcp-jsonrpc-client-notifications client) nil))))

(defun %mcp-jsonrpc-reader-loop (client)
  (unwind-protect
      (loop while (mcp-jsonrpc-client-reader-running-p client) do
        (handler-case
            (let ((message (jsonrpc-read-message
                            (mcp-jsonrpc-client-input-stream client)
                            :eof-error-p nil)))
              (if message
                  (mcp-jsonrpc-handle-incoming-message client message)
                  (setf (mcp-jsonrpc-client-reader-running-p client) nil)))
          (error (condition)
            (setf (mcp-jsonrpc-client-reader-error client) condition
                  (mcp-jsonrpc-client-reader-running-p client) nil))))
    (setf (mcp-jsonrpc-client-reader-thread client) nil
          (mcp-jsonrpc-client-reader-running-p client) nil)))

(defun mcp-jsonrpc-start-reader (client)
  (unless (mcp-jsonrpc-client-p client)
    (error "CLIENT must be an MCP-JSONRPC-CLIENT, got ~S." client))
  (when (eq (mcp-jsonrpc-client-transport client) :streamable-http)
    (return-from mcp-jsonrpc-start-reader client))
  (unless (mcp-jsonrpc-client-input-stream client)
    (error "Cannot start MCP JSON-RPC reader without an input stream."))
  (unless (mcp-jsonrpc-client-reader-running-p client)
    (setf (mcp-jsonrpc-client-reader-running-p client) t
          (mcp-jsonrpc-client-reader-error client) nil)
    #+sb-thread
    (setf (mcp-jsonrpc-client-reader-thread client)
          (sb-thread:make-thread
           (lambda ()
             (%mcp-jsonrpc-reader-loop client))
           :name "amoebum-mcp-jsonrpc-reader"))
    #-sb-thread
    (setf (mcp-jsonrpc-client-reader-thread client) :unsupported-threading))
  client)

(defun mcp-jsonrpc-stop-reader (client)
  (unless (mcp-jsonrpc-client-p client)
    (error "CLIENT must be an MCP-JSONRPC-CLIENT, got ~S." client))
  (setf (mcp-jsonrpc-client-reader-running-p client) nil)
  client)
