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

(defun %trimmed-non-empty-string (value)
  (when (stringp value)
    (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) value)))
      (when (plusp (length trimmed))
        trimmed))))

(defun %openai-content-part-text-p (part)
  (and (hash-table-p part)
       (string= (string-downcase (or (gethash "type" part) ""))
                "text")
       (stringp (gethash "text" part))))

(defun %openai-make-text-content-part (text)
  (let ((part (make-hash-table :test #'equal)))
    (setf (gethash "type" part) "text"
          (gethash "text" part) (or text ""))
    part))

(defun %openai-make-image-url-content-part (url)
  (let ((part (make-hash-table :test #'equal))
        (image-url (make-hash-table :test #'equal)))
    (setf (gethash "type" part) "image_url"
          (gethash "url" image-url) url
          (gethash "image_url" part) image-url)
    part))

(defun %openai-image-data-uri (part)
  (when (hash-table-p part)
    (let* ((media-type (%trimmed-non-empty-string
                        (or (gethash "media_type" part)
                            (gethash "mime_type" part)
                            (gethash "mime-type" part))))
           (data (%trimmed-non-empty-string (gethash "data" part))))
      (when (and media-type data)
        (format nil "data:~A;base64,~A" media-type data)))))

(defun %openai-image-url-value (part)
  (when (hash-table-p part)
    (or (%trimmed-non-empty-string (gethash "url" part))
        (let ((image-url (gethash "image_url" part)))
          (and (hash-table-p image-url)
               (%trimmed-non-empty-string (gethash "url" image-url)))))))

;;; --- Content-Part Coercion Dispatch Tables (FP-Refine Phase 2) ---

(defun %openai-coerce-text-part (hash)
  (%openai-make-text-content-part
   (or (%trimmed-non-empty-string (gethash "text" hash))
       (%trimmed-non-empty-string (gethash "content" hash))
       "")))

(defun %openai-coerce-image-part (hash)
  (let ((uri (or (%openai-image-data-uri hash)
                 (%openai-image-url-value hash))))
    (if uri
        (%openai-make-image-url-content-part uri)
        (%openai-make-text-content-part
         (or (%trimmed-non-empty-string (gethash "text" hash))
             "[image]")))))

(defun %openai-coerce-image-url-part (hash)
  (let ((uri (%openai-image-url-value hash)))
    (if uri
        (%openai-make-image-url-content-part uri)
        (%openai-make-text-content-part
         (or (%trimmed-non-empty-string (gethash "text" hash))
             "[image]")))))

(defparameter +openai-content-part-coercers+
  '(("text"        . %openai-coerce-text-part)
    ("image"       . %openai-coerce-image-part)
    ("input_image" . %openai-coerce-image-part)
    ("image_url"   . %openai-coerce-image-url-part))
  "Dispatch table mapping content-part type strings to pure coercion handlers.
Each handler: (hash-table) -> coerced hash-table.")

(defun %dispatch-content-part-coercion (type table hash)
  "Look up TYPE in dispatch TABLE and call the matched handler on HASH.
Returns the coerced result, or NIL if no handler matches."
  (let ((entry (assoc type table :test #'string=)))
    (when entry
      (funcall (cdr entry) hash))))

(defun %openai-coerce-content-part (part)
  (let* ((hash (cond
                 ((content-part-p part)
                  (content-part-to-hash part))
                 ((hash-table-p part)
                  (%copy-hash-table part))
                 ((stringp part)
                  (%openai-make-text-content-part part))
                 (t nil)))
         (type (and (hash-table-p hash)
                    (string-downcase (or (gethash "type" hash) "")))))
    (cond
      ((null hash) nil)
      (t (or (%dispatch-content-part-coercion type +openai-content-part-coercers+ hash)
             hash)))))

(defun %openai-normalize-message-content (content)
  (cond
    ((or (null content) (stringp content))
     content)
    ((or (listp content) (vectorp content))
     (let ((parts (remove nil (mapcar #'%openai-coerce-content-part
                                      (%sequence->list content)))))
       (cond
         ((null parts) "")
         ((and (= (length parts) 1)
               (%openai-content-part-text-p (first parts)))
          (gethash "text" (first parts)))
         (t
          (coerce parts 'vector)))))
    ((or (hash-table-p content)
         (content-part-p content))
     (let ((part (%openai-coerce-content-part content)))
       (cond
         ((null part) "")
         ((%openai-content-part-text-p part) (gethash "text" part))
         (t (vector part)))))
    (t
     content)))

(defun %openai-normalize-message-for-payload (message)
  (let ((raw (cond
               ((message-p message) (message-to-hash message))
               ((hash-table-p message) message)
               ((stringp message) (%make-raw-message "user" message))
               (t nil))))
    (unless (hash-table-p raw)
      (error "Expected message struct, hash-table, or string, got ~S" message))
    (let ((normalized (%copy-hash-table raw)))
      (setf (gethash "content" normalized)
            (%openai-normalize-message-content (gethash "content" raw)))
      normalized)))

(defun %coerce-request-message (message)
  (%openai-normalize-message-for-payload message))

(defun %request-message-tool-calls-empty-p (tool-calls)
  (or (null tool-calls)
      (and (listp tool-calls) (null tool-calls))
      (and (vectorp tool-calls) (zerop (length tool-calls)))))

(defun %request-message-content-empty-p (content)
  (cond
    ((null content) t)
    ((stringp content)
     (null (%trimmed-non-empty-string content)))
    ((content-part-p content)
     (%request-message-content-empty-p
      (or (content-part-text content)
          (content-part-think content))))
    ((hash-table-p content)
     (let ((type (string-downcase (or (gethash "type" content) ""))))
       (cond
         ((or (string= type "")
              (string= type "text")
              (string= type "think"))
          (null (%trimmed-non-empty-string
                 (or (gethash "text" content)
                     (gethash "content" content)
                     (gethash "think" content)))))
         (t nil))))
    ((or (listp content) (vectorp content))
     (let ((items (%sequence->list content)))
       (or (null items)
           (every #'%request-message-content-empty-p items))))
    (t nil)))

(defun %empty-assistant-request-message-p (message)
  (when (hash-table-p message)
    (let ((role (gethash "role" message))
          (tool-calls (gethash "tool_calls" message))
          (content (gethash "content" message)))
      (and (stringp role)
           (string= role "assistant")
           (%request-message-tool-calls-empty-p tool-calls)
           (%request-message-content-empty-p content)))))

(defun %msg-role (msg)
  "Extract role string from a message struct or hash-table."
  (cond
    ((message-p msg) (message-role msg))
    ((hash-table-p msg) (gethash "role" msg ""))
    (t "")))

(defun %msg-tool-call-ids (msg)
  "Extract list of tool_call IDs declared in an assistant message."
  (cond
    ((message-p msg)
     (let ((tcs (message-tool-calls msg)))
       (when tcs (mapcar #'tool-call-id tcs))))
    ((hash-table-p msg)
     (let ((tcs (gethash "tool_calls" msg)))
       (when tcs
         (mapcar (lambda (tc)
                   (if (hash-table-p tc)
                       (gethash "id" tc)
                       (and (tool-call-p tc) (tool-call-id tc))))
                 (%sequence->list tcs)))))
    (t nil)))

(defun %msg-tool-call-id (msg)
  "Extract tool_call_id from a tool response message."
  (cond
    ((message-p msg) (message-tool-call-id msg))
    ((hash-table-p msg) (gethash "tool_call_id" msg))
    (t nil)))

(defun %strip-tool-calls-from-msg (msg)
  "Return a copy of MSG with tool_calls removed."
  (cond
    ((message-p msg)
     (make-message :role (message-role msg)
                   :name (message-name msg)
                   :content (or (message-content msg)
                                (list (make-content-part :type "text" :text "")))
                   :tool-call-id (message-tool-call-id msg)
                   :reasoning-content (message-reasoning-content msg)))
    ((hash-table-p msg)
     (let ((copy (%copy-hash-table msg)))
       (remhash "tool_calls" copy)
       (unless (gethash "content" copy)
         (setf (gethash "content" copy) ""))
       copy))
    (t msg)))

(defun %sanitize-tool-calls (messages)
  "Reorder messages so tool responses immediately follow their assistant message.
Strip tool_calls whose responses are missing entirely, and drop orphaned tool
response messages. This satisfies OpenAI-compatible APIs (Moonshot, etc.) which
require strict assistant→tool response adjacency."
  ;; Strategy: walk messages. When we see an assistant with tool_calls, collect
  ;; its declared IDs. Then pull matching tool responses from anywhere in the
  ;; remaining messages and insert them right after the assistant message. Any
  ;; tool responses that don't match a prior assistant tool_call are dropped.
  ;; Any assistant tool_calls with no response anywhere are stripped.
  (let* ((msgs (copy-list messages))
         ;; Collect all tool_call_ids that have responses anywhere
         (response-ids (make-hash-table :test #'equal))
         ;; Map tool_call_id → tool response message
         (response-map (make-hash-table :test #'equal)))
    ;; First pass: index all tool response messages
    (dolist (msg msgs)
      (when (string= "tool" (%msg-role msg))
        (let ((tcid (%msg-tool-call-id msg)))
          (when (and tcid (stringp tcid) (plusp (length tcid)))
            (setf (gethash tcid response-ids) t)
            (setf (gethash tcid response-map) msg)))))
    ;; Second pass: rebuild message list with correct ordering
    (let ((result nil)
          (used-response-ids (make-hash-table :test #'equal)))
      (dolist (msg msgs)
        (let ((role (%msg-role msg)))
          (cond
            ;; Tool response messages: skip here, they get inserted after their assistant
            ((string= "tool" role)
             nil)
            ;; Assistant with tool_calls: keep if responses exist, reorder responses after
            ((and (string= "assistant" role) (%msg-tool-call-ids msg))
             (let ((tc-ids (%msg-tool-call-ids msg))
                   (all-present t))
               (dolist (id tc-ids)
                 (unless (gethash id response-ids)
                   (setf all-present nil)))
               (if all-present
                   (progn
                     (push msg result)
                     ;; Insert tool responses immediately after
                     (dolist (id tc-ids)
                       (let ((resp (gethash id response-map)))
                         (when resp
                           (push resp result)
                           (setf (gethash id used-response-ids) t)))))
                   ;; Some responses missing: strip tool_calls entirely
                   (push (%strip-tool-calls-from-msg msg) result))))
            ;; Everything else: keep as-is
            (t (push msg result)))))
      (nreverse result))))

(defun %normalize-request-messages (system-prompt user-prompt messages)
  (if messages
      (let ((message-list (cond
                            ((listp messages) messages)
                            ((vectorp messages)
                             (loop for item across messages collect item))
                            (t
                             (error "Expected :messages to be a list or vector, got ~S"
                                    messages)))))
        (remove-if #'%empty-assistant-request-message-p
                   (mapcar #'%coerce-request-message
                           (%sanitize-tool-calls message-list))))
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
        (setf (gethash "tools" payload) normalized-tools)
        (setf (gethash "tool_choice" payload) "auto")))
    (setf (gethash "temperature" payload) (coerce (client-temperature client) 'double-float))
    (setf (gethash "max_tokens" payload) (client-max-tokens client))
    (setf (gethash "top_p" payload) (coerce (client-top-p client) 'double-float))
    (setf (gethash "stream" payload) (if streamp t :false))
    (let ((json (jonathan:to-json payload)))
      ;; Debug: dump payload to file when it contains tool_calls
      (when (search "tool_calls" json)
        (ignore-errors
          (let ((path (merge-pathnames ".amoebum/runtime/last-payload.json"
                                       (user-homedir-pathname))))
            (ensure-directories-exist path)
            (with-open-file (f path :direction :output :if-exists :supersede
                                    :if-does-not-exist :create)
              (write-string json f)
              (finish-output f)))))
      json)))

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

;;; --- HTTP Error Dispatch Tables (FP-Refine Phase 2, Target 2) ---

(defparameter +http-status-error-classes+
  '(((401 403) . :auth)
    ((408 504) . :timeout))
  "Maps HTTP status code groups to error classification keywords.")

(defparameter +dexador-error-type-classes+
  '((dexador.error:http-request-unauthorized     . :auth)
    (dexador.error:http-request-request-timeout   . :timeout)
    (dexador.error:http-request-gateway-timeout   . :timeout))
  "Maps dexador exception types to error classification keywords.")

(defparameter +http-error-class-conditions+
  '((:auth    . pseudopod-auth-error)
    (:timeout . pseudopod-timeout-error)
    (:api     . pseudopod-api-error))
  "Maps error classification keywords to pseudopod condition types.")

(defun %classify-http-status (status)
  "Classify an HTTP status code as :auth, :timeout, or :api.
Pure function — no side effects."
  (if (integerp status)
      (or (cdr (assoc-if (lambda (codes) (member status codes :test #'=))
                          +http-status-error-classes+))
          :api)
      :api))

(defun %classify-dexador-error (condition)
  "Classify a dexador error condition as :auth, :timeout, or NIL.
Pure function — no side effects."
  (cdr (assoc-if (lambda (type) (typep condition type))
                  +dexador-error-type-classes+)))

(defun %http-error-message (class kind status body-text)
  "Build the error message string for a given error CLASS.
Pure function — no side effects."
  (case class
    (:auth    (format nil "Moonshot ~A unauthorized (status=~A): ~A" kind status body-text))
    (:timeout (format nil "Moonshot ~A timed out (status=~A): ~A" kind status body-text))
    (t        (format nil "Moonshot ~A failed (status=~A): ~A" kind status body-text))))

(defun %http-error-initargs (class status body-text message cause)
  "Build the initarg plist for signaling an HTTP error condition.
Pure function — no side effects."
  (case class
    (:auth    (list :message message :status-code status :body body-text :cause cause))
    (:timeout (list :message message :cause cause))
    (t        (list :message message :status-code status :body body-text :cause cause))))

(defun %signal-http-status-error (status body &key cause streamp)
  (let* ((body-text (or (%coerce-response-body body) "<no-body>"))
         (kind (if streamp "streaming request" "request"))
         (class (%classify-http-status status))
         (condition-type (cdr (assoc class +http-error-class-conditions+)))
         (message (%http-error-message class kind status body-text))
         (initargs (%http-error-initargs class status body-text message cause)))
    (apply #'error condition-type initargs)))

(defun %signal-dexador-http-error (condition &key streamp)
  (let ((status (ignore-errors (dexador.error:response-status condition)))
        (body (ignore-errors (dexador.error:response-body condition))))
    (let ((dex-class (%classify-dexador-error condition)))
      (cond
        (dex-class
         (let* ((kind (if streamp "streaming request" "request"))
                (body-text (or (%coerce-response-body body) "<no-body>"))
                (condition-type (cdr (assoc dex-class +http-error-class-conditions+)))
                (message (%http-error-message dex-class kind
                                              (or status (case dex-class (:auth 401) (t nil)))
                                              body-text))
                (initargs (%http-error-initargs dex-class
                                                (or status (case dex-class (:auth 401) (t nil)))
                                                body-text message condition)))
           (apply #'error condition-type initargs)))
        (status
         (%signal-http-status-error status body :cause condition :streamp streamp))
        (t
         (error 'pseudopod-api-error
                :message (format nil "Moonshot ~A failed: ~A"
                                 (if streamp "streaming request" "request")
                                 condition)
                :status-code nil
                :body (%coerce-response-body body)
                :cause condition))))))

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

(defvar *active-stream-registry* (make-hash-table :test #'equal)
  "Maps stream-id -> stream state plist.")

(defvar *stream-id-random-state* (make-random-state t)
  "Random state used for stream-id generation.")

(defvar *stream-registry-lock*
  #+sb-thread (sb-thread:make-mutex :name "pseudopod-stream-registry-lock")
  #-sb-thread nil
  "Mutex guarding *ACTIVE-STREAM-REGISTRY* when threads are available.")

(defmacro %with-stream-registry-lock (&body body)
  #+sb-thread
  `(sb-thread:with-mutex (*stream-registry-lock*)
     ,@body)
  #-sb-thread
  `(progn
     ,@body))

(defun %normalize-stream-id (stream-id)
  (when stream-id
    (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return)
                                (princ-to-string stream-id))))
      (and (plusp (length trimmed))
           trimmed))))

(defun %generate-stream-id ()
  (format nil "stream-~D-~D"
          (get-universal-time)
          (random most-positive-fixnum *stream-id-random-state*)))

(defun %register-active-stream (&optional requested-stream-id)
  (let ((stream-id (or (%normalize-stream-id requested-stream-id)
                       (%generate-stream-id))))
    (%with-stream-registry-lock
      (loop while (gethash stream-id *active-stream-registry*) do
        (setf stream-id (%generate-stream-id)))
      (setf (gethash stream-id *active-stream-registry*)
            (list :cancelled-p nil
                  :started-at (get-universal-time))))
    stream-id))

(defun %unregister-active-stream (stream-id)
  (%with-stream-registry-lock
    (remhash stream-id *active-stream-registry*))
  stream-id)

(defun %stream-cancelled-p (stream-id)
  (%with-stream-registry-lock
    (let ((state (and stream-id
                      (gethash stream-id *active-stream-registry*))))
      (and state
           (getf state :cancelled-p)))))

(defun cancel-stream (stream-id)
  "Request cancellation of an active stream. Returns true when stream is found."
  (let ((normalized (%normalize-stream-id stream-id)))
    (and normalized
         (%with-stream-registry-lock
           (let ((state (gethash normalized *active-stream-registry*)))
             (when state
               (setf (getf state :cancelled-p) t)
               t))))))

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
            (function-body (make-hash-table :test #'equal))
            (extras (make-hash-table :test #'equal)))
        (setf (gethash "type" entry) "function")
        (setf (gethash "function" entry) function-body)
        (setf (gethash "stream_index" extras) index)
        (setf (gethash "extras" entry) extras)
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
                                            delta-arguments)))))
          (values entry index))))))

(defun %stream-tool-call-name (entry)
  (let ((function-body (and (hash-table-p entry)
                            (gethash "function" entry))))
    (and (hash-table-p function-body)
         (%non-empty-string-p (gethash "name" function-body))
         (gethash "name" function-body))))

(defun %stream-tool-call-arguments (entry)
  (let ((function-body (and (hash-table-p entry)
                            (gethash "function" entry))))
    (and (hash-table-p function-body)
         (%non-empty-string-p (gethash "arguments" function-body))
         (gethash "arguments" function-body))))

(defun %stream-tool-call-arguments-complete-p (arguments)
  (let ((trimmed (and (stringp arguments)
                      (string-trim '(#\Space #\Tab #\Newline #\Return) arguments))))
    (when (%non-empty-string-p trimmed)
      (handler-case
          (hash-table-p (jonathan:parse trimmed :as :hash-table))
        (error ()
          nil)))))

(defun %make-stream-text-delta-chunk (text)
  (list :type :text-delta
        :text (or text "")))

(defun %make-stream-tool-call-delta-chunk (index entry)
  (let* ((name (%stream-tool-call-name entry))
         (arguments (%stream-tool-call-arguments entry))
         (tool-call (hash-to-tool-call entry)))
    (list :type :tool-call-delta
          :index index
          :name name
          :arguments arguments
          :arguments-complete-p (%stream-tool-call-arguments-complete-p arguments)
          :tool-call tool-call)))

(defun %copy-hash-table-shallow (table)
  (let ((copy (make-hash-table :test #'equal)))
    (when (hash-table-p table)
      (maphash (lambda (key value)
                 (setf (gethash key copy) value))
               table))
    copy))

(defun %make-stream-usage-delta-chunk (usage)
  (let* ((usage-copy (%copy-hash-table-shallow usage))
         (prompt-tokens (or (%token-count-integer (gethash "prompt_tokens" usage-copy))
                            (%token-count-integer (gethash "input_tokens" usage-copy))))
         (completion-tokens (or (%token-count-integer (gethash "completion_tokens" usage-copy))
                                (%token-count-integer (gethash "output_tokens" usage-copy))))
         (total-tokens (or (%token-count-integer (gethash "total_tokens" usage-copy))
                           (and prompt-tokens completion-tokens
                                (+ prompt-tokens completion-tokens)))))
    (list :type :usage-delta
          :usage usage-copy
          :prompt-tokens prompt-tokens
          :completion-tokens completion-tokens
          :total-tokens total-tokens)))

(defun %ensure-stream-tool-call-state (tool-call-states index)
  (or (gethash index tool-call-states)
      (setf (gethash index tool-call-states)
            (list :started-emitted-p nil
                  :arguments-complete-emitted-p nil))))

(defun %emit-stream-chunk (on-chunk chunk)
  (when on-chunk
    (funcall on-chunk chunk)))

(defun %emit-stream-tool-call-delta (index
                                     entry
                                     tool-call-states
                                     on-chunk
                                     on-tool-call-delta
                                     on-tool-call-started
                                     on-tool-call-argument-complete)
  (let* ((chunk (%make-stream-tool-call-delta-chunk index entry))
         (tool-call (getf chunk :tool-call))
         (name (getf chunk :name))
         (arguments-complete-p (not (null (getf chunk :arguments-complete-p))))
         (state (%ensure-stream-tool-call-state tool-call-states index)))
    (%emit-stream-chunk on-chunk chunk)
    (when on-tool-call-delta
      (funcall on-tool-call-delta chunk))
    (when (and (%non-empty-string-p name)
               (not (getf state :started-emitted-p)))
      (setf (getf state :started-emitted-p) t)
      (when on-tool-call-started
        (funcall on-tool-call-started tool-call)))
    (when (and arguments-complete-p
               (not (getf state :arguments-complete-emitted-p)))
      (setf (getf state :arguments-complete-emitted-p) t)
      (when on-tool-call-argument-complete
        (funcall on-tool-call-argument-complete tool-call)))))

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

(defstruct (sse-parse-state (:constructor %make-sse-parse-state))
  on-reasoning
  on-content
  on-role
  on-chunk
  on-usage-delta
  on-tool-call-delta
  on-tool-call-started
  on-tool-call-argument-complete
  snapshot
  tool-call-partials
  tool-call-states
  content-stream
  usage-delta-state
  parse-error-count)

(defstruct (stream-collection-context (:constructor %make-stream-collection-context))
  client
  user-prompt
  system-prompt
  messages
  tools
  on-reasoning
  on-content
  on-tool-call
  on-chunk
  on-usage-delta
  on-tool-call-delta
  on-tool-call-started
  on-tool-call-argument-complete
  (snapshot (make-stream-turn-snapshot) :type stream-turn-snapshot)
  stream-id
  (role "assistant")
  (content-stream (make-string-output-stream))
  (reasoning-stream (make-string-output-stream))
  (tool-call-partials (make-hash-table :test #'eql))
  (tool-call-states (make-hash-table :test #'eql))
  (usage-delta-state (list nil))
  (parse-error-count (list 0))
  (stream-status :completed)
  active-stream-id
  (max-bytes 0)
  (bytes-read 0))

(defun %make-stream-collection-context-from-args (client user-prompt args)
  (%make-stream-collection-context
   :client client
   :user-prompt user-prompt
   :system-prompt (or (getf args :system-prompt) "You are a helpful assistant.")
   :messages (getf args :messages)
   :tools (getf args :tools)
   :on-reasoning (getf args :on-reasoning)
   :on-content (getf args :on-content)
   :on-tool-call (getf args :on-tool-call)
   :on-chunk (getf args :on-chunk)
   :on-usage-delta (getf args :on-usage-delta)
   :on-tool-call-delta (getf args :on-tool-call-delta)
   :on-tool-call-started (getf args :on-tool-call-started)
   :on-tool-call-argument-complete (getf args :on-tool-call-argument-complete)
   :snapshot (make-stream-turn-snapshot)
   :stream-id (getf args :stream-id)
   :max-bytes (client-max-response-bytes client)))

(defun %make-sse-parse-state-from-context (context)
  (%make-sse-parse-state
   :on-reasoning (lambda (chunk)
                   (when (%non-empty-string-p chunk)
                     (write-string chunk (stream-collection-context-reasoning-stream context)))
                   (let ((cb (stream-collection-context-on-reasoning context)))
                     (when cb (funcall cb chunk))))
   :on-content (stream-collection-context-on-content context)
   :on-role (lambda (next-role)
              (setf (stream-collection-context-role context) next-role))
   :on-chunk (stream-collection-context-on-chunk context)
   :on-usage-delta (stream-collection-context-on-usage-delta context)
   :on-tool-call-delta (stream-collection-context-on-tool-call-delta context)
   :on-tool-call-started (stream-collection-context-on-tool-call-started context)
   :on-tool-call-argument-complete
   (stream-collection-context-on-tool-call-argument-complete context)
   :snapshot (stream-collection-context-snapshot context)
   :tool-call-partials (stream-collection-context-tool-call-partials context)
   :tool-call-states (stream-collection-context-tool-call-states context)
   :content-stream (stream-collection-context-content-stream context)
   :usage-delta-state (stream-collection-context-usage-delta-state context)
   :parse-error-count (stream-collection-context-parse-error-count context)))

(defun %sse-line-payload (line)
  (cond
    ((uiop:string-prefix-p "data: " line)
     (subseq line 6))
    ((uiop:string-prefix-p "data:" line)
     (string-left-trim " " (subseq line 5)))
    (t nil)))

(defun %processable-sse-payload-p (payload)
  (and payload
       (plusp (length payload))
       (not (string= payload "[DONE]"))))

(defun %parse-sse-json-payload (payload)
  (jonathan:parse payload :as :hash-table :junk-allowed t))

(defun %sse-first-delta (json)
  (let* ((choices (and (hash-table-p json) (gethash "choices" json)))
         (choice (%first-item choices)))
    (and (hash-table-p choice) (gethash "delta" choice))))

(defun %dispatch-sse-role (state value)
  (let ((callback (sse-parse-state-on-role state)))
    (when (and callback (%non-empty-string-p value))
      (funcall callback value)))
  (let ((snapshot (sse-parse-state-snapshot state)))
    (when snapshot
      (stream-turn-apply-event! snapshot
                                (list :type :role
                                      :role value)))))

(defun %dispatch-sse-reasoning (state value)
  (let ((callback (sse-parse-state-on-reasoning state)))
    (when (and callback (%non-empty-string-p value))
      (funcall callback value)))
  (let ((snapshot (sse-parse-state-snapshot state)))
    (when snapshot
      (stream-turn-apply-event! snapshot
                                (list :type :reasoning-delta
                                      :text value)))))

(defun %dispatch-sse-content (state value)
  (when (%non-empty-string-p value)
    (write-string value (sse-parse-state-content-stream state))
    (let ((on-content (sse-parse-state-on-content state))
          (on-chunk (sse-parse-state-on-chunk state)))
      (when on-content
        (funcall on-content value))
      (%emit-stream-chunk on-chunk (%make-stream-text-delta-chunk value))))
  (let ((snapshot (sse-parse-state-snapshot state)))
    (when snapshot
      (stream-turn-apply-event! snapshot
                                (list :type :text-delta
                                      :text value)))))

(defun %dispatch-sse-usage (state value)
  (when (hash-table-p value)
    (let* ((usage-chunk (%make-stream-usage-delta-chunk value))
           (on-chunk (sse-parse-state-on-chunk state))
           (on-usage-delta (sse-parse-state-on-usage-delta state))
           (usage-delta-state (sse-parse-state-usage-delta-state state)))
      (%emit-stream-chunk on-chunk usage-chunk)
      (when on-usage-delta
        (funcall on-usage-delta usage-chunk))
      (when usage-delta-state
        (setf (car usage-delta-state) (getf usage-chunk :usage)))
      (let ((snapshot (sse-parse-state-snapshot state)))
        (when snapshot
          (stream-turn-apply-event! snapshot usage-chunk))))))

(defun %dispatch-sse-tool-calls (state value)
  (dolist (tool-call (%sequence->list value))
    (multiple-value-bind (entry index)
        (%merge-stream-tool-call-delta (sse-parse-state-tool-call-partials state)
                                       tool-call)
      (when (and (hash-table-p entry)
                 (integerp index))
        (let ((snapshot (sse-parse-state-snapshot state)))
          (when snapshot
            (stream-turn-apply-event! snapshot
                                      (%make-stream-tool-call-delta-chunk index entry))))
        (%emit-stream-tool-call-delta
         index
         entry
         (sse-parse-state-tool-call-states state)
         (sse-parse-state-on-chunk state)
         (sse-parse-state-on-tool-call-delta state)
         (sse-parse-state-on-tool-call-started state)
         (sse-parse-state-on-tool-call-argument-complete state))))))

(defparameter *sse-delta-dispatchers*
  '(("role" . %dispatch-sse-role)
    ("reasoning_content" . %dispatch-sse-reasoning)
    ("content" . %dispatch-sse-content)))

(defparameter *sse-json-dispatchers*
  '(("usage" . %dispatch-sse-usage)))

(defun %dispatch-sse-field-group (state source dispatchers)
  (when (hash-table-p source)
    (dolist (dispatcher dispatchers)
      (let ((value (gethash (car dispatcher) source)))
        (when value
          (funcall (symbol-function (cdr dispatcher)) state value))))))

(defun %dispatch-sse-json (state json)
  (let ((delta (%sse-first-delta json)))
    (%dispatch-sse-field-group state delta *sse-delta-dispatchers*)
    (%dispatch-sse-field-group state json *sse-json-dispatchers*)
    (%dispatch-sse-tool-calls state (and (hash-table-p delta)
                                         (gethash "tool_calls" delta)))))

(defun %increment-sse-parse-error-count (state)
  (let ((parse-error-count (sse-parse-state-parse-error-count state)))
    (when parse-error-count
      (incf (car parse-error-count))))
  (let ((snapshot (sse-parse-state-snapshot state)))
    (when snapshot
      (stream-turn-apply-event! snapshot '(:type :parse-error)))))

(defun %consume-sse-line (line state)
  (let ((payload (%sse-line-payload line)))
    (when (%processable-sse-payload-p payload)
      (handler-case
          (%dispatch-sse-json state (%parse-sse-json-payload payload))
        (error (condition)
          (declare (ignore condition))
          (%increment-sse-parse-error-count state))))))

(defun %stream-request-payload (context)
  (%build-payload (stream-collection-context-client context)
                  (stream-collection-context-system-prompt context)
                  (stream-collection-context-user-prompt context)
                  t
                  :messages (stream-collection-context-messages context)
                  :tools (stream-collection-context-tools context)))

(defun %stream-request (context)
  (%request-post (stream-collection-context-client context)
                 (%stream-request-payload context)
                 :streamp t))

(defun %ensure-stream-success-status (body-stream status)
  (unless (<= 200 status 299)
    (let ((error-body (%coerce-response-body body-stream)))
      (handler-case (close body-stream)
        (error () nil))
      (%signal-http-status-error status error-body :streamp t))))

(defun %stream-collection-cancelled-p (context)
  (%stream-cancelled-p (stream-collection-context-active-stream-id context)))

(defun %mark-stream-cancelled (context)
  (setf (stream-collection-context-stream-status context) :cancelled))

(defun %update-stream-byte-count (context line)
  (incf (stream-collection-context-bytes-read context) (length line))
  (let ((max-bytes (stream-collection-context-max-bytes context)))
    (when (and (plusp max-bytes)
               (> (stream-collection-context-bytes-read context) max-bytes))
      (error 'pseudopod-api-error
             :message (format nil
                              "Streaming response exceeded ~:D byte limit."
                              max-bytes)
             :status-code nil
             :body nil))))

(defun %process-stream-line (context parser-state line)
  (when (%stream-collection-cancelled-p context)
    (%mark-stream-cancelled context)
    (return-from %process-stream-line :stop))
  (%update-stream-byte-count context line)
  (%consume-sse-line line parser-state)
  (when (%stream-collection-cancelled-p context)
    (%mark-stream-cancelled context)
    :stop))

(defun %collect-stream-body (context body-stream)
  (let ((parser-state (%make-sse-parse-state-from-context context)))
    (handler-case
        (loop for line = (read-line body-stream nil nil)
              while line
              until (eq :stop (%process-stream-line context parser-state line)))
      (error (condition)
        (if (%timeout-condition-p condition)
            (error 'pseudopod-timeout-error
                   :message "Moonshot streaming request timed out."
                   :cause condition)
            (error condition))))))

(defun %stream-collection-values (context)
  (let* ((snapshot (stream-collection-context-snapshot context))
         (reasoning (stream-turn-snapshot-reasoning-content snapshot)))
    (values (or (stream-turn-snapshot-stream-id snapshot)
                (stream-collection-context-active-stream-id context))
            (stream-turn-snapshot-role snapshot)
            (stream-turn-snapshot-content snapshot)
            (stream-turn-snapshot-tool-calls snapshot)
            (stream-turn-snapshot-parse-error-count snapshot)
            (or (stream-turn-snapshot-status snapshot)
                (stream-collection-context-stream-status context))
            (or (stream-turn-snapshot-usage snapshot)
                (car (stream-collection-context-usage-delta-state context)))
            (unless (%stream-turn-blank-string-p reasoning)
              reasoning))))

(defun %stream-chat-completion-collect (context)
  (setf (stream-collection-context-active-stream-id context)
        (%register-active-stream (stream-collection-context-stream-id context)))
  (unwind-protect
      (multiple-value-bind (body-stream status)
          (%stream-request context)
        (%ensure-stream-success-status body-stream status)
        (unwind-protect
            (%collect-stream-body context body-stream)
          (handler-case (close body-stream)
            (error () nil))))
  (%unregister-active-stream
   (stream-collection-context-active-stream-id context)))
  (maybe-finalize-stream-turn-answer!
   (stream-collection-context-snapshot context))
  (stream-turn-apply-event!
   (stream-collection-context-snapshot context)
   (list :type :done
         :stream-id (stream-collection-context-active-stream-id context)
         :status (stream-collection-context-stream-status context)
         :usage (car (stream-collection-context-usage-delta-state context))
         :parse-error-count (car (stream-collection-context-parse-error-count context))))
  (%stream-collection-values context))

(defun %emit-stream-tool-calls (tool-calls on-tool-call)
  (when on-tool-call
    (dolist (tool-call tool-calls)
      (funcall on-tool-call tool-call))))

(defun %blank-stream-content-p (content)
  (or (null content)
      (not (stringp content))
      (zerop (length (string-trim '(#\Space #\Tab #\Newline #\Return)
                                  content)))))

(defun %stream-terminal-outcome-missing-p (stream-status content tool-calls)
  (and (eq stream-status :completed)
       (%blank-stream-content-p content)
       (null (%sequence->list tool-calls))))

(defun %keyword-present-p (plist key)
  (loop for plist-key in plist by #'cddr
        thereis (eq plist-key key)))

(defun %split-legacy-stream-callback (args)
  (if (and args (functionp (first args)))
      (values (first args) (rest args))
      (values nil args)))

(defun %stream-chat-completion-dispatch (context)
  "Run a streaming Moonshot completion.
ON-REASONING and ON-CONTENT receive streaming text chunks.
ON-CHUNK receives chunk plists with :TYPE = :TEXT-DELTA, :TOOL-CALL-DELTA,
or :USAGE-DELTA.
ON-USAGE-DELTA receives token-usage chunk plists.
ON-TOOL-CALL-DELTA receives incremental tool-call chunk plists.
ON-TOOL-CALL-STARTED fires when a streamed tool call has a resolved name.
ON-TOOL-CALL-ARGUMENT-COMPLETE fires when a streamed tool argument JSON object parses.
ON-TOOL-CALL receives reconstructed tool-call structs when present."
  (multiple-value-bind (active-stream-id role content tool-calls parse-error-count
                        stream-status usage)
      (%stream-chat-completion-collect context)
    (declare (ignore role))
    (when (%stream-terminal-outcome-missing-p stream-status content tool-calls)
      (error 'pseudopod-parse-error
             :message "Streaming response completed with no assistant content and no tool calls."
             :payload (list :stream-status stream-status
                            :parse-error-count parse-error-count
                            :usage usage)))
    (%emit-stream-tool-calls tool-calls
                             (stream-collection-context-on-tool-call context))
    (values content
            tool-calls
            active-stream-id
            stream-status
            usage
            parse-error-count)))

(defun stream-chat-completion (client user-prompt &rest args)
  "Run a streaming Moonshot completion.

Legacy compatibility: supports an optional positional callback as the third
argument, treated as :ON-CONTENT."
  (multiple-value-bind (legacy-on-content keyword-args)
      (%split-legacy-stream-callback args)
    (let ((effective-args keyword-args))
      (when (and legacy-on-content
                 (not (%keyword-present-p keyword-args :on-content)))
        (setf effective-args (append effective-args
                                     (list :on-content legacy-on-content))))
      (%stream-chat-completion-dispatch
       (%make-stream-collection-context-from-args client user-prompt effective-args)))))

(defun %stream-chat-completion*-dispatch (context)
  "Run a streaming Moonshot completion and return a typed message struct."
  (multiple-value-bind (active-stream-id role content tool-calls parse-error-count
                        stream-status usage reasoning-content)
      (%stream-chat-completion-collect context)
    (when (%stream-terminal-outcome-missing-p stream-status content tool-calls)
      (error 'pseudopod-parse-error
             :message "Streaming response completed with no assistant content and no tool calls."
             :payload (list :stream-status stream-status
                            :parse-error-count parse-error-count
                            :usage usage)))
    (%emit-stream-tool-calls tool-calls
                             (stream-collection-context-on-tool-call context))
    (values (make-message :role (if (%non-empty-string-p role) role "assistant")
                          :content (or content "")
                          :tool-calls tool-calls
                          :reasoning-content reasoning-content)
            active-stream-id
            stream-status
            usage
            parse-error-count)))

(defun stream-chat-completion* (client user-prompt &rest args)
  "Run a streaming Moonshot completion and return a typed message struct.

Legacy compatibility: supports an optional positional callback as the third
argument, treated as :ON-CONTENT."
  (multiple-value-bind (legacy-on-content keyword-args)
      (%split-legacy-stream-callback args)
    (let ((effective-args keyword-args))
      (when (and legacy-on-content
                 (not (%keyword-present-p keyword-args :on-content)))
        (setf effective-args (append effective-args
                                     (list :on-content legacy-on-content))))
      (%stream-chat-completion*-dispatch
       (%make-stream-collection-context-from-args client user-prompt effective-args)))))

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
