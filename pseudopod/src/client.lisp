(in-package :pseudopod)

(defparameter *default-base-url* "https://api.moonshot.ai/v1")
(defparameter *default-model* "kimi-k2.5")
(defparameter *default-api-key-file*
  (merge-pathnames #P".moonshotai" (user-homedir-pathname)))

(defstruct (client (:constructor %make-client))
  (api-key "" :type string)
  (base-url *default-base-url* :type string)
  (model *default-model* :type string)
  (temperature 1.0d0 :type real)
  (max-tokens 32768 :type integer)
  (top-p 0.95d0 :type real)
  (timeout-seconds 180 :type integer))

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
                      (timeout-seconds 180))
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
   :timeout-seconds timeout-seconds)))

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

(defun %headers (client)
  `((:authorization . ,(format nil "Bearer ~A" (client-api-key client)))
    (:content-type . "application/json")))

(defun %endpoint (client)
  (format nil "~A/chat/completions" (client-base-url client)))

(defun %coerce-response-body (body)
  (cond
    ((stringp body) body)
    ((streamp body)
     (prog1
         (ignore-errors (uiop:slurp-stream-string body))
       (ignore-errors (close body))))
    ((null body) nil)
    (t (ignore-errors (princ-to-string body)))))

(defun %timeout-condition-p (condition)
  (let* ((class-name (class-name (class-of condition)))
         (class-name-string (string-upcase (if (symbolp class-name)
                                               (symbol-name class-name)
                                               (princ-to-string class-name))))
         (message-string (string-upcase (princ-to-string condition))))
    (or (search "TIMEOUT" class-name-string)
        (search "TIMEOUT" message-string)
        (search "TIMED OUT" message-string))))

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

(defun %request-post (client payload &key streamp)
  (let ((args (list (%endpoint client)
                    :content payload
                    :headers (%headers client)
                    :connect-timeout (client-timeout-seconds client)
                    :read-timeout (client-timeout-seconds client)
                    :keep-alive nil)))
    (when streamp
      (setf args (append args (list :want-stream t))))
    (handler-case
        (apply #'dex:post args)
      (dexador.error:http-request-failed (condition)
        (%signal-dexador-http-error condition :streamp streamp))
      (error (condition)
        (if (%timeout-condition-p condition)
            (error 'pseudopod-timeout-error
                   :message (format nil "Moonshot ~A timed out."
                                    (if streamp "streaming request" "request"))
                   :cause condition)
            (error condition))))))

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

(defun %consume-sse-line (line on-reasoning on-content)
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
                 (reasoning (and (hash-table-p delta) (gethash "reasoning_content" delta)))
                 (content (and (hash-table-p delta) (gethash "content" delta))))
            (when (and on-reasoning (stringp reasoning) (> (length reasoning) 0))
              (funcall on-reasoning reasoning))
            (when (and on-content (stringp content) (> (length content) 0))
              (funcall on-content content)))
        (error ()
          ;; Ignore malformed or non-JSON SSE lines while streaming.
          nil)))))

(defun stream-chat-completion (client user-prompt
                               &key
                                 (system-prompt "You are a helpful assistant.")
                                 messages
                                 on-reasoning
                                 on-content)
  "Run a streaming Moonshot completion.
ON-REASONING and ON-CONTENT are callbacks that receive text chunks."
  (multiple-value-bind (body-stream status)
      (%request-post client
                     (%build-payload client system-prompt user-prompt t
                                     :messages messages)
                     :streamp t)
    (unless (<= 200 status 299)
      (let ((error-body (%coerce-response-body body-stream)))
        (ignore-errors (close body-stream))
        (%signal-http-status-error status error-body :streamp t)))
    (unwind-protect
        (handler-case
            (loop for line = (read-line body-stream nil nil)
                  while line do
                    (%consume-sse-line line on-reasoning on-content))
          (error (condition)
            (if (%timeout-condition-p condition)
                (error 'pseudopod-timeout-error
                       :message "Moonshot streaming request timed out."
                       :cause condition)
                (error condition))))
      (ignore-errors (close body-stream)))))

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
