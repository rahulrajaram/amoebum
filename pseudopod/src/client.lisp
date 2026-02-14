(in-package :pseudopod)

(defparameter *default-base-url* "https://api.moonshot.ai/v1")
(defparameter *default-model* "kimi-k2.5")
(defparameter *default-api-key-file*
  (merge-pathnames #P".moonshotai" (user-homedir-pathname)))

(defparameter *default-max-response-bytes* (* 64 1024 1024))

(defstruct (client (:constructor %make-client))
  (api-key "" :type string)
  (base-url *default-base-url* :type string)
  (model *default-model* :type string)
  (temperature 1.0d0 :type real)
  (max-tokens 32768 :type integer)
  (top-p 0.95d0 :type real)
  (timeout-seconds 180 :type integer)
  (max-response-bytes *default-max-response-bytes* :type integer))

(defun %mask-api-key (key)
  "Mask an API key for safe display in error messages. Shows first 3 and last 4 chars."
  (if (and (stringp key) (> (length key) 8))
      (format nil "~A...~A"
              (subseq key 0 3)
              (subseq key (- (length key) 4)))
      "***"))

(defmethod print-object ((obj client) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~A api-key=~A"
            (client-model obj)
            (%mask-api-key (client-api-key obj)))))

(defun %normalize-api-key (value)
  (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return)
                              (or value ""))))
    (if (plusp (length trimmed))
        trimmed
        nil)))

(defun read-api-key (&key (env-var "MOONSHOT_API_KEY")
                       (path *default-api-key-file*))
  "Read API key from ENV-VAR first, then PATH (defaults to ~/.moonshotai)."
  (let ((from-env (uiop:getenv env-var)))
    (cond
      ((%normalize-api-key from-env)
       (%normalize-api-key from-env))
      ((uiop:file-exists-p path)
       (or (%normalize-api-key (uiop:read-file-string path))
           (error "Moonshot API key file exists but is empty: ~A" path)))
      (t
       (error "Moonshot API key not found. Set ~A or create ~A"
              env-var
              path)))))

(defun make-client (&key api-key
                      (api-key-file *default-api-key-file*)
                      (base-url *default-base-url*)
                      (model *default-model*)
                      (temperature 1.0d0)
                      (max-tokens 32768)
                      (top-p 0.95d0)
                      (timeout-seconds 180)
                      (max-response-bytes *default-max-response-bytes*))
  "Create a Moonshot client configuration."
  (let ((normalized-api-key (%normalize-api-key api-key)))
    (when (and api-key (null normalized-api-key))
      (error "Provided Moonshot API key is empty."))
  (%make-client
   :api-key (or normalized-api-key
                (read-api-key :path api-key-file))
   :base-url base-url
   :model model
   :temperature temperature
   :max-tokens max-tokens
   :top-p top-p
   :timeout-seconds timeout-seconds
   :max-response-bytes max-response-bytes)))

(defun %make-raw-message (role content)
  (let ((obj (make-hash-table :test #'equal)))
    (setf (gethash "role" obj) role)
    (setf (gethash "content" obj) content)
    obj))

(defun %coerce-request-message (message)
  (cond
    ((message-p message) (message-to-hash message))
    ((hash-table-p message) message)
    (t
     (error "Expected message struct or hash-table, got ~S" message))))

(defun %normalize-request-messages (system-prompt user-prompt messages)
  (if messages
      (let ((message-list (cond
                            ((listp messages) messages)
                            ((vectorp messages)
                             (loop for item across messages collect item))
                            (t
                             (error "Expected :messages to be a list or vector, got ~S"
                                    messages)))))
        (mapcar #'%coerce-request-message message-list))
      (list (%make-raw-message "system" system-prompt)
            (%make-raw-message "user" user-prompt))))

(defun %coerce-request-tool (tool)
  (cond
    ((tool-definition-p tool) (tool-definition-to-hash tool))
    ((hash-table-p tool) tool)
    (t
     (error "Expected tool-definition or hash-table, got ~S" tool))))

(defun %normalize-request-tools (tools)
  (when tools
    (let ((tool-list (cond
                       ((listp tools) tools)
                       ((vectorp tools)
                        (loop for item across tools collect item))
                       (t
                        (error "Expected :tools to be a list or vector, got ~S"
                               tools)))))
      (mapcar #'%coerce-request-tool tool-list))))

(defun %build-payload (client system-prompt user-prompt streamp &key messages tools)
  (let ((payload (make-hash-table :test #'equal)))
    (setf (gethash "model" payload) (client-model client))
    (setf (gethash "messages" payload)
          (%normalize-request-messages system-prompt user-prompt messages))
    (let ((normalized-tools (%normalize-request-tools tools)))
      (when normalized-tools
        (setf (gethash "tools" payload) normalized-tools)))
    (setf (gethash "temperature" payload) (coerce (client-temperature client) 'double-float))
    (setf (gethash "max_tokens" payload) (client-max-tokens client))
    (setf (gethash "top_p" payload) (coerce (client-top-p client) 'double-float))
    (setf (gethash "stream" payload) (if streamp t :false))
    (jonathan:to-json payload)))

(defun %auth-headers (client)
  `((:authorization . ,(format nil "Bearer ~A" (client-api-key client)))))

(defun %json-headers (client)
  (append (%auth-headers client)
          '((:content-type . "application/json"))))

(defun %endpoint (client path)
  (let ((base (string-right-trim "/" (client-base-url client))))
    (format nil "~A~A" base path)))

(defun %chat-endpoint (client)
  (%endpoint client "/chat/completions"))

(defun %coerce-response-body (body)
  (cond
    ((stringp body) body)
    ((streamp body)
     (prog1
         (handler-case (uiop:slurp-stream-string body)
           (error () nil))
       (handler-case (close body)
         (error () nil))))
    ((null body) nil)
    (t (handler-case (princ-to-string body)
         (error () nil)))))

(defun %timeout-condition-p (condition)
  (or (typep condition 'usocket:timeout-error)
      (typep condition 'usocket:deadline-timeout-error)
      (typep condition 'dexador.error:http-request-request-timeout)
      (typep condition 'dexador.error:http-request-gateway-timeout)
      #+sbcl (typep condition 'sb-sys:io-timeout)))

(defun %signal-http-status-error (status body &key cause streamp)
  (let* ((body-text (or (%coerce-response-body body) "<no-body>"))
         (kind (if streamp "streaming request" "request")))
    (cond
      ((and (integerp status)
            (member status '(401 403) :test #'=))
       (error 'pseudopod-auth-error
              :message (format nil "Moonshot ~A unauthorized (status=~A): ~A"
                               kind status body-text)
              :status-code status
              :body body-text
              :cause cause))
      ((and (integerp status)
            (member status '(408 504) :test #'=))
       (error 'pseudopod-timeout-error
              :message (format nil "Moonshot ~A timed out (status=~A): ~A"
                               kind status body-text)
              :cause cause))
      (t
       (error 'pseudopod-api-error
              :message (format nil "Moonshot ~A failed (status=~A): ~A"
                               kind status body-text)
              :status-code status
              :body body-text
              :cause cause)))))

(defun %signal-dexador-http-error (condition &key streamp)
  (let ((status (ignore-errors (dexador.error:response-status condition)))
        (body (ignore-errors (dexador.error:response-body condition))))
    (cond
      ((typep condition 'dexador.error:http-request-unauthorized)
       (error 'pseudopod-auth-error
              :message (format nil "Moonshot ~A unauthorized (status=401)."
                               (if streamp "streaming request" "request"))
              :status-code 401
              :body (%coerce-response-body body)
              :cause condition))
      ((or (typep condition 'dexador.error:http-request-request-timeout)
           (typep condition 'dexador.error:http-request-gateway-timeout))
       (error 'pseudopod-timeout-error
              :message (format nil "Moonshot ~A timed out."
                               (if streamp "streaming request" "request"))
              :cause condition))
      (status
       (%signal-http-status-error status body :cause condition :streamp streamp))
      (t
       (error 'pseudopod-api-error
              :message (format nil "Moonshot ~A failed: ~A"
                               (if streamp "streaming request" "request")
                               condition)
              :status-code nil
              :body (%coerce-response-body body)
              :cause condition)))))

(defun %timeout-message (&key streamp method)
  (if streamp
      "Moonshot streaming request timed out."
      (format nil "Moonshot ~A request timed out."
              (or method "API"))))

(defun %perform-request (thunk &key streamp method)
  (handler-case
      (funcall thunk)
    (dexador.error:http-request-failed (condition)
      (%signal-dexador-http-error condition :streamp streamp))
    (error (condition)
      (if (%timeout-condition-p condition)
          (error 'pseudopod-timeout-error
                 :message (%timeout-message :streamp streamp :method method)
                 :cause condition)
          (error condition)))))

(defun %request-post (client payload &key streamp endpoint method)
  (let ((args (list (or endpoint (%chat-endpoint client))
                    :content payload
                    :headers (%json-headers client)
                    :connect-timeout (client-timeout-seconds client)
                    :read-timeout (client-timeout-seconds client)
                    :keep-alive nil)))
    (when streamp
      (setf args (append args (list :want-stream t))))
    (%perform-request (lambda () (apply #'dex:post args))
                      :streamp streamp
                      :method (or method "POST"))))

(defun %request-get (client endpoint &key streamp)
  (let ((args (list endpoint
                    :headers (%auth-headers client)
                    :connect-timeout (client-timeout-seconds client)
                    :read-timeout (client-timeout-seconds client)
                    :keep-alive nil)))
    (when streamp
      (setf args (append args (list :want-stream t))))
    (%perform-request (lambda () (apply #'dex:get args))
                      :streamp streamp
                      :method "GET")))

(defun %request-delete (client endpoint)
  (%perform-request
   (lambda ()
     (dex:delete endpoint
                 :headers (%auth-headers client)
                 :connect-timeout (client-timeout-seconds client)
                 :read-timeout (client-timeout-seconds client)
                 :keep-alive nil))
   :streamp nil
   :method "DELETE"))

(defun %request-post-multipart (client endpoint form-data)
  (%perform-request
   (lambda ()
     (dex:post endpoint
               :content form-data
               :headers (%auth-headers client)
               :connect-timeout (client-timeout-seconds client)
               :read-timeout (client-timeout-seconds client)
               :keep-alive nil))
   :streamp nil
   :method "POST"))

(defun %parse-json-response (body)
  (let ((payload (%coerce-response-body body)))
    (handler-case
        (jonathan:parse payload :as :hash-table)
      (error (condition)
        (error 'pseudopod-parse-error
               :message (format nil "Moonshot response JSON parse failed: ~A"
                                condition)
               :payload payload
               :cause condition)))))

(defun chat-completion (client user-prompt
                        &key
                          (system-prompt "You are a helpful assistant.")
                          messages
                          tools)
  "Run a non-streaming Moonshot chat completion and return parsed JSON object."
  (multiple-value-bind (body status)
      (%request-post client
                     (%build-payload client system-prompt user-prompt nil
                                     :messages messages
                                     :tools tools)
                     :streamp nil)
    (unless (<= 200 status 299)
      (%signal-http-status-error status body :streamp nil))
    (%parse-json-response body)))

(defun chat-completion* (client user-prompt
                         &key
                           (system-prompt "You are a helpful assistant.")
                           messages
                           tools)
  "Run a non-streaming Moonshot chat completion and return typed assistant message."
  (let* ((response (chat-completion client
                                    user-prompt
                                    :system-prompt system-prompt
                                    :messages messages
                                    :tools tools))
         (choices (and (hash-table-p response) (gethash "choices" response)))
         (choice (%first-item choices))
         (raw-message (and (hash-table-p choice) (gethash "message" choice))))
    (if (hash-table-p raw-message)
        (hash-to-message raw-message)
        (error 'pseudopod-parse-error
               :message "Moonshot response missing assistant message."
               :payload response))))

(defun %parse-model-list (response)
  (let* ((data (and (hash-table-p response) (gethash "data" response)))
         (items (%sequence->list data)))
    (mapcar (lambda (item)
              (if (hash-table-p item)
                  (hash-to-model-info item)
                  (error 'pseudopod-parse-error
                         :message "Moonshot model list response contains non-object item."
                         :payload item)))
            items)))

(defun list-models (client)
  "List available models from /models and return typed model-info values."
  (multiple-value-bind (body status)
      (%request-get client (%endpoint client "/models") :streamp nil)
    (unless (<= 200 status 299)
      (%signal-http-status-error status body :streamp nil))
    (%parse-model-list (%parse-json-response body))))

(defun %token-count-integer (value)
  (cond
    ((integerp value) value)
    ((and (stringp value)
          (plusp (length value))
          (every #'digit-char-p value))
     (parse-integer value))
    (t nil)))

(defun %token-count-from-hash (hash)
  (or (%token-count-integer (gethash "total_tokens" hash))
      (%token-count-integer (gethash "input_tokens" hash))
      (%token-count-integer (gethash "token_count" hash))
      (%token-count-integer (gethash "tokens" hash))
      (let ((usage (gethash "usage" hash)))
        (and (hash-table-p usage)
             (%token-count-from-hash usage)))
      (let ((data (gethash "data" hash)))
        (cond
          ((hash-table-p data)
           (%token-count-from-hash data))
          ((or (listp data) (vectorp data))
           (loop for item in (%sequence->list data)
                 thereis (and (hash-table-p item)
                              (%token-count-from-hash item))))
          (t nil)))))

(defun %normalize-estimate-messages (messages text)
  (if messages
      (let ((message-list (cond
                            ((listp messages) messages)
                            ((vectorp messages)
                             (loop for item across messages collect item))
                            (t
                             (error "Expected :messages to be a list or vector, got ~S"
                                    messages)))))
        (mapcar #'%coerce-request-message message-list))
      (let ((normalized-text (and (stringp text)
                                  (string-trim '(#\Space #\Tab #\Newline #\Return)
                                               text))))
        (unless (and normalized-text (plusp (length normalized-text)))
          (error "estimate-tokens requires non-empty :text or :messages."))
        (list (%make-raw-message "user" normalized-text)))))

(defun estimate-tokens (client &key text messages model)
  "Estimate input token usage via /tokenizers/estimate-token-count.
Returns two values: token-count integer and parsed response hash-table."
  (let ((payload (make-hash-table :test #'equal)))
    (setf (gethash "model" payload) (or model (client-model client)))
    (setf (gethash "messages" payload)
          (%normalize-estimate-messages messages text))
    (multiple-value-bind (body status)
        (%request-post client
                       (jonathan:to-json payload)
                       :streamp nil
                       :endpoint (%endpoint client "/tokenizers/estimate-token-count")
                       :method "POST /tokenizers/estimate-token-count")
      (unless (<= 200 status 299)
        (%signal-http-status-error status body :streamp nil))
      (let* ((response (%parse-json-response body))
             (count (and (hash-table-p response)
                         (%token-count-from-hash response))))
        (unless (integerp count)
          (error 'pseudopod-parse-error
                 :message "Moonshot token estimate response missing token count."
                 :payload response))
        (values count response)))))

(defun %ensure-upload-file-path (file-path)
  (let ((pathname (etypecase file-path
                    (pathname file-path)
                    (string (pathname file-path)))))
    (unless (uiop:file-exists-p pathname)
      (error "Upload file does not exist: ~A" pathname))
    pathname))

(defun %parse-file-list (response)
  (let* ((data (and (hash-table-p response) (gethash "data" response)))
         (items (%sequence->list data)))
    (mapcar (lambda (item)
              (if (hash-table-p item)
                  (hash-to-file-object item)
                  (error 'pseudopod-parse-error
                         :message "Moonshot file list response contains non-object item."
                         :payload item)))
            items)))

(defun upload-file (client file-path &key (purpose "file-extract"))
  "Upload FILE-PATH via /files multipart API and return a typed file-object."
  (let* ((resolved-path (%ensure-upload-file-path file-path))
         (form-data `(("purpose" . ,purpose)
                      ("file" . ,resolved-path))))
    (multiple-value-bind (body status)
        (%request-post-multipart client
                                 (%endpoint client "/files")
                                 form-data)
      (unless (<= 200 status 299)
        (%signal-http-status-error status body :streamp nil))
      (hash-to-file-object (%parse-json-response body)))))

(defun get-file (client file-id)
  "Fetch file metadata from /files/{file-id}."
  (multiple-value-bind (body status)
      (%request-get client
                    (%endpoint client (format nil "/files/~A" file-id))
                    :streamp nil)
    (unless (<= 200 status 299)
      (%signal-http-status-error status body :streamp nil))
    (hash-to-file-object (%parse-json-response body))))

(defun list-files (client)
  "List files from /files and return a list of typed file-object values."
  (multiple-value-bind (body status)
      (%request-get client (%endpoint client "/files") :streamp nil)
    (unless (<= 200 status 299)
      (%signal-http-status-error status body :streamp nil))
    (%parse-file-list (%parse-json-response body))))

(defun delete-file (client file-id)
  "Delete /files/{file-id}. Returns parsed deletion response hash-table."
  (multiple-value-bind (body status)
      (%request-delete client (%endpoint client (format nil "/files/~A" file-id)))
    (unless (<= 200 status 299)
      (%signal-http-status-error status body :streamp nil))
    (%parse-json-response body)))

(defun file-content (client file-id)
  "Fetch raw file content from /files/{file-id}/content as a string."
  (multiple-value-bind (body status)
      (%request-get client
                    (%endpoint client (format nil "/files/~A/content" file-id))
                    :streamp nil)
    (unless (<= 200 status 299)
      (%signal-http-status-error status body :streamp nil))
    (or (%coerce-response-body body) "")))

(defun %non-empty-string-p (value)
  (and (stringp value)
       (> (length value) 0)))

(defun %merge-stream-string (current chunk)
  (cond
    ((not (%non-empty-string-p chunk)) current)
    ((%non-empty-string-p current)
     (concatenate 'string current chunk))
    (t chunk)))

(defun %parse-stream-tool-call-index (value)
  (cond
    ((integerp value) value)
    ((and (stringp value)
          (plusp (length value))
          (every #'digit-char-p value))
     (parse-integer value))
    (t nil)))

(defun %ensure-stream-tool-call-partial (partials index)
  (or (gethash index partials)
      (let ((entry (make-hash-table :test #'equal))
            (function-body (make-hash-table :test #'equal)))
        (setf (gethash "type" entry) "function")
        (setf (gethash "function" entry) function-body)
        (setf (gethash index partials) entry)
        entry)))

(defun %merge-stream-tool-call-delta (partials raw-tool-call)
  (when (hash-table-p raw-tool-call)
    (let* ((index (%parse-stream-tool-call-index (gethash "index" raw-tool-call)))
           (entry (and index (%ensure-stream-tool-call-partial partials index))))
      (when entry
        (let ((id (gethash "id" raw-tool-call))
              (type (gethash "type" raw-tool-call))
              (name (gethash "name" raw-tool-call))
              (arguments (gethash "arguments" raw-tool-call))
              (function-delta (and (hash-table-p (gethash "function" raw-tool-call))
                                   (gethash "function" raw-tool-call)))
              (function-body (or (and (hash-table-p (gethash "function" entry))
                                      (gethash "function" entry))
                                 (let ((fresh (make-hash-table :test #'equal)))
                                   (setf (gethash "function" entry) fresh)
                                   fresh))))
          (when (%non-empty-string-p id)
            (setf (gethash "id" entry)
                  (%merge-stream-string (gethash "id" entry) id)))
          (when (%non-empty-string-p type)
            (setf (gethash "type" entry) type))
          (when (%non-empty-string-p name)
            (setf (gethash "name" function-body)
                  (%merge-stream-string (gethash "name" function-body) name)))
          (when (%non-empty-string-p arguments)
            (setf (gethash "arguments" function-body)
                  (%merge-stream-string (gethash "arguments" function-body) arguments)))
          (when function-delta
            (let ((delta-name (gethash "name" function-delta))
                  (delta-arguments (gethash "arguments" function-delta)))
              (when (%non-empty-string-p delta-name)
                (setf (gethash "name" function-body)
                      (%merge-stream-string (gethash "name" function-body)
                                            delta-name)))
              (when (%non-empty-string-p delta-arguments)
                (setf (gethash "arguments" function-body)
                      (%merge-stream-string (gethash "arguments" function-body)
                                            delta-arguments))))))))))

(defun %sorted-stream-tool-call-indexes (partials)
  (let (indexes)
    (maphash (lambda (index entry)
               (when (and (integerp index) (hash-table-p entry))
                 (push index indexes)))
             partials)
    (sort indexes #'<)))

(defun %finalize-stream-tool-call-partials (partials)
  (loop for index in (%sorted-stream-tool-call-indexes partials)
        for raw-tool-call = (gethash index partials)
        when (hash-table-p raw-tool-call)
          collect (hash-to-tool-call raw-tool-call)))

(defun %consume-sse-line (line on-reasoning on-content on-role
                               tool-call-partials content-stream
                               parse-error-count)
  (let ((payload nil))
    (cond
      ((uiop:string-prefix-p "data: " line)
       (setf payload (subseq line 6)))
      ((uiop:string-prefix-p "data:" line)
       (setf payload (string-left-trim " " (subseq line 5)))))
    (when (and payload
               (plusp (length payload))
               (not (string= payload "[DONE]")))
      (handler-case
          (let* ((json (jonathan:parse payload :as :hash-table :junk-allowed t))
                 (choices (gethash "choices" json))
                 (choice (%first-item choices))
                 (delta (and (hash-table-p choice) (gethash "delta" choice)))
                 (role (and (hash-table-p delta) (gethash "role" delta)))
                 (reasoning (and (hash-table-p delta) (gethash "reasoning_content" delta)))
                 (content (and (hash-table-p delta) (gethash "content" delta)))
                 (tool-calls (and (hash-table-p delta) (gethash "tool_calls" delta))))
            (when (and on-role (%non-empty-string-p role))
              (funcall on-role role))
            (when (and on-reasoning (%non-empty-string-p reasoning))
              (funcall on-reasoning reasoning))
            (when (%non-empty-string-p content)
              (write-string content content-stream)
              (when on-content
                (funcall on-content content)))
            (dolist (tool-call (%sequence->list tool-calls))
              (%merge-stream-tool-call-delta tool-call-partials tool-call)))
        (error (condition)
          (declare (ignore condition))
          (when parse-error-count
            (incf (car parse-error-count))))))))

(defun %stream-chat-completion-collect (client user-prompt
                                        &key
                                          (system-prompt "You are a helpful assistant.")
                                          messages
                                          tools
                                          on-reasoning
                                          on-content)
  (let ((role "assistant")
        (content-stream (make-string-output-stream))
        (tool-call-partials (make-hash-table :test #'eql))
        (parse-error-count (list 0))
        (max-bytes (client-max-response-bytes client))
        (bytes-read 0))
    (multiple-value-bind (body-stream status)
        (%request-post client
                       (%build-payload client system-prompt user-prompt t
                                       :messages messages
                                       :tools tools)
                       :streamp t)
      (unless (<= 200 status 299)
        (let ((error-body (%coerce-response-body body-stream)))
          (handler-case (close body-stream)
            (error () nil))
          (%signal-http-status-error status error-body :streamp t)))
      (unwind-protect
          (handler-case
              (loop for line = (read-line body-stream nil nil)
                    while line do
                      (incf bytes-read (length line))
                      (when (and (plusp max-bytes) (> bytes-read max-bytes))
                        (error 'pseudopod-api-error
                               :message (format nil
                                                "Streaming response exceeded ~:D byte limit."
                                                max-bytes)
                               :status-code nil
                               :body nil))
                      (%consume-sse-line line
                                         on-reasoning
                                         on-content
                                         (lambda (next-role) (setf role next-role))
                                         tool-call-partials
                                         content-stream
                                         parse-error-count))
            (error (condition)
              (if (%timeout-condition-p condition)
                  (error 'pseudopod-timeout-error
                         :message "Moonshot streaming request timed out."
                         :cause condition)
                  (error condition))))
        (handler-case (close body-stream)
          (error () nil))))
    (values role
            (get-output-stream-string content-stream)
            (%finalize-stream-tool-call-partials tool-call-partials)
            (car parse-error-count))))

(defun %emit-stream-tool-calls (tool-calls on-tool-call)
  (when on-tool-call
    (dolist (tool-call tool-calls)
      (funcall on-tool-call tool-call))))

(defun stream-chat-completion (client user-prompt
                               &key
                                 (system-prompt "You are a helpful assistant.")
                                 messages
                                 tools
                                 on-reasoning
                                 on-content
                                 on-tool-call)
  "Run a streaming Moonshot completion.
ON-REASONING and ON-CONTENT receive streaming text chunks.
ON-TOOL-CALL receives reconstructed tool-call structs when present."
  (multiple-value-bind (role content tool-calls)
      (%stream-chat-completion-collect client
                                       user-prompt
                                       :system-prompt system-prompt
                                       :messages messages
                                       :tools tools
                                       :on-reasoning on-reasoning
                                       :on-content on-content)
    (declare (ignore role))
    (%emit-stream-tool-calls tool-calls on-tool-call)
    (values content tool-calls)))

(defun stream-chat-completion* (client user-prompt
                                &key
                                  (system-prompt "You are a helpful assistant.")
                                  messages
                                  tools
                                  on-reasoning
                                  on-content
                                  on-tool-call)
  "Run a streaming Moonshot completion and return a typed message struct."
  (multiple-value-bind (role content tool-calls)
      (%stream-chat-completion-collect client
                                       user-prompt
                                       :system-prompt system-prompt
                                       :messages messages
                                       :tools tools
                                       :on-reasoning on-reasoning
                                       :on-content on-content)
    (%emit-stream-tool-calls tool-calls on-tool-call)
    (make-message :role (if (%non-empty-string-p role) role "assistant")
                  :content (or content "")
                  :tool-calls tool-calls)))

(defun print-streamed-completion (client user-prompt
                                  &key
                                    (system-prompt "You are a helpful assistant.")
                                    (print-reasoning t))
  "Print a streaming completion directly to *STANDARD-OUTPUT*."
  (stream-chat-completion
   client
   user-prompt
   :system-prompt system-prompt
   :on-reasoning (when print-reasoning
                   (lambda (text) (write-string text)))
   :on-content (lambda (text) (write-string text)))
  (terpri))

(defun main ()
  "Simple interactive entrypoint for quick manual testing."
  (let ((client (make-client)))
    (format t "Prompt: ")
    (finish-output)
    (let ((prompt (read-line *standard-input* nil "")))
      (when (zerop (length prompt))
        (error "Prompt cannot be empty."))
      (print-streamed-completion client prompt))))
