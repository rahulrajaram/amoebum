(in-package :pseudopod)

;;; ---------------------------------------------------------------------------
;;; OpenAI-Compatible Provider (I94)
;;;
;;; Generic provider for OpenAI API-compatible services:
;;; OpenAI, Together AI, Groq, Ollama, vLLM, etc.
;;; ---------------------------------------------------------------------------

(defclass openai-compatible-provider (provider)
  ((organization :initarg :organization :accessor openai-compat-organization
                 :type (or null string) :initform nil))
  (:default-initargs
   :name "openai"
   :base-url "https://api.openai.com/v1"
   :default-model "gpt-4o")
  (:documentation "Provider for OpenAI-compatible chat completions API."))

(defun make-openai-compatible-provider (&key api-key
                                             (name "openai")
                                             (base-url "https://api.openai.com/v1")
                                             (model "gpt-4o")
                                             (timeout-seconds 180)
                                             organization)
  "Create an OpenAI-compatible provider."
  (let ((resolved-key (or api-key
                          (handler-case
                              (let ((env-key (uiop:getenv "OPENAI_API_KEY")))
                                (if (and env-key (plusp (length env-key)))
                                    env-key
                                    ""))
                            (error () "")))))
    (make-instance 'openai-compatible-provider
                   :name name
                   :api-key resolved-key
                   :base-url (string-right-trim "/" base-url)
                   :default-model model
                   :timeout-seconds timeout-seconds
                   :organization organization)))

(defun %openai-compat-headers (provider)
  "Build HTTP headers for OpenAI-compatible API."
  (let ((headers `((:authorization . ,(format nil "Bearer ~A" (provider-api-key provider)))
                   (:content-type . "application/json"))))
    (when (and (slot-boundp provider 'organization)
               (openai-compat-organization provider))
      (push `(:openai-organization . ,(openai-compat-organization provider))
            headers))
    headers))

(defun %openai-compat-endpoint (provider path)
  "Build full URL for OpenAI-compatible API."
  (format nil "~A~A" (provider-base-url provider) path))

(defun %openai-compat-normalize-stream-result (role content tool-calls usage)
  (let ((response (make-hash-table :test #'equal)))
    (setf (gethash "role" response) (or role "assistant"))
    (setf (gethash "content" response) (or content ""))
    (setf (gethash "tool_calls" response) (coerce (or tool-calls '()) 'vector))
    (when usage
      (setf (gethash "usage" response) usage))
    response))

(defun %openai-compat-trim-sse-data (line)
  (cond
    ((uiop:string-prefix-p "data: " line) (subseq line 6))
    ((uiop:string-prefix-p "data:" line)
     (string-left-trim '(#\Space #\Tab) (subseq line 5)))
    (t nil)))

(defun %openai-compat-collect-stream (body-stream callback)
  (let ((role "assistant")
        (content-stream (make-string-output-stream))
        (usage nil)
        (tool-call-partials (make-hash-table :test #'eql)))
    (labels ((consume-payload (payload)
               (let* ((json (jonathan:parse payload :as :hash-table :junk-allowed t))
                      (choices (and (hash-table-p json) (gethash "choices" json)))
                      (choice (%first-item choices))
                      (delta (and (hash-table-p choice) (gethash "delta" choice)))
                      (delta-role (and (hash-table-p delta) (gethash "role" delta)))
                      (delta-content (and (hash-table-p delta) (gethash "content" delta)))
                      (delta-tool-calls (and (hash-table-p delta) (gethash "tool_calls" delta)))
                      (event-usage (and (hash-table-p json) (gethash "usage" json))))
                 (when (%non-empty-string-p delta-role)
                   (setf role delta-role))
                 (when (%non-empty-string-p delta-content)
                   (write-string delta-content content-stream)
                   (when callback
                     (funcall callback delta-content)))
                 (dolist (tool-call (%sequence->list delta-tool-calls))
                   (when (hash-table-p tool-call)
                     (%merge-stream-tool-call-delta tool-call-partials tool-call)))
                 (when (hash-table-p event-usage)
                   (setf usage event-usage)))))
      (loop for line = (read-line body-stream nil nil)
            while line do
              (let ((payload (%openai-compat-trim-sse-data line)))
                (when payload
                  (let ((trimmed (string-right-trim '(#\Space #\Tab #\Return) payload)))
                    (cond
                      ((string= trimmed "[DONE]")
                       (loop-finish))
                      ((plusp (length trimmed))
                       (handler-case
                           (consume-payload trimmed)
                         (error () nil))))))))
      (values role
              (get-output-stream-string content-stream)
              (%finalize-stream-tool-call-partials tool-call-partials)
              usage))))

(defun %openai-compat-coerce-message (m)
  "Coerce a message to hash-table format for OpenAI API."
  (cond
    ((hash-table-p m) m)
    ((message-p m) (message-to-hash m))
    ((stringp m)
     (let ((h (make-hash-table :test #'equal)))
       (setf (gethash "role" h) "user"
             (gethash "content" h) m)
       h))
    (t m)))

(defun %openai-compat-build-payload (provider messages &key model temperature max-tokens
                                                            top-p tools tool-choice
                                                            system-prompt stream-p
                                                            extra-params)
  "Build JSON payload for OpenAI-compatible chat completions."
  (let ((payload (make-hash-table :test #'equal)))
    (setf (gethash "model" payload) (or model (provider-default-model provider)))
    (let ((all-messages (mapcar #'%openai-compat-coerce-message messages)))
      (when system-prompt
        (push (%make-raw-message "system" system-prompt) all-messages))
      (setf (gethash "messages" payload) all-messages))
    (when temperature
      (setf (gethash "temperature" payload) (coerce temperature 'double-float)))
    (when max-tokens
      (setf (gethash "max_tokens" payload) max-tokens))
    (when top-p
      (setf (gethash "top_p" payload) (coerce top-p 'double-float)))
    (when tools
      (setf (gethash "tools" payload)
            (mapcar (lambda (td)
                      (if (tool-definition-p td)
                          (tool-definition-to-hash td)
                          td))
                    tools)))
    (when tool-choice
      (setf (gethash "tool_choice" payload) tool-choice))
    (when stream-p
      (setf (gethash "stream" payload) t))
    ;; Merge any extra parameters
    (when (hash-table-p extra-params)
      (maphash (lambda (k v) (setf (gethash k payload) v)) extra-params))
    (jonathan:to-json payload)))

(defmethod send-chat-completion ((provider openai-compatible-provider) messages
                                 &key model temperature max-tokens top-p
                                      tools tool-choice system-prompt extra-params)
  (%provider-timed-call provider
    (lambda ()
      (let* ((payload (%openai-compat-build-payload provider messages
                                                     :model model
                                                     :temperature temperature
                                                     :max-tokens max-tokens
                                                     :top-p top-p
                                                     :tools tools
                                                     :tool-choice tool-choice
                                                     :system-prompt system-prompt
                                                     :extra-params extra-params))
             (url (%openai-compat-endpoint provider "/chat/completions"))
             (headers (%openai-compat-headers provider)))
        (multiple-value-bind (body status)
            (dex:request url
                         :method :post
                         :headers headers
                         :content payload
                         :read-timeout (provider-timeout-seconds provider)
                         :connect-timeout 30)
          (unless (<= 200 status 299)
            (error 'pseudopod-api-error
                   :message (format nil "OpenAI-compatible API error (status ~A)" status)
                   :status-code status
                   :body (if (stringp body) body (princ-to-string body))))
          (let ((text (cond ((stringp body) body)
                            ((streamp body)
                             (handler-case (uiop:slurp-stream-string body)
                               (error () "")))
                            (t (princ-to-string body)))))
            (jonathan:parse text :as :hash-table)))))))

(defmethod send-streaming-completion ((provider openai-compatible-provider) messages callback
                                      &key model temperature max-tokens top-p
                                           tools tool-choice system-prompt extra-params)
  "Send an OpenAI-compatible streaming completion request using SSE parsing."
  (%provider-timed-call provider
    (lambda ()
      (let* ((payload (%openai-compat-build-payload provider messages
                                                     :model model
                                                     :temperature temperature
                                                     :max-tokens max-tokens
                                                     :top-p top-p
                                                     :tools tools
                                                     :tool-choice tool-choice
                                                     :system-prompt system-prompt
                                                     :stream-p t
                                                     :extra-params extra-params))
             (url (%openai-compat-endpoint provider "/chat/completions"))
             (headers (%openai-compat-headers provider))
             (result nil))
        (multiple-value-bind (body-stream status)
            (dex:request url
                         :method :post
                         :headers headers
                         :content payload
                         :read-timeout (provider-timeout-seconds provider)
                         :connect-timeout 30
                         :want-stream t)
          (unless (<= 200 status 299)
            (let ((error-body (%coerce-response-body body-stream)))
              (handler-case (close body-stream)
                (error () nil))
              (%signal-http-status-error status error-body :streamp t)))
          (unwind-protect
              (multiple-value-bind (parsed-role parsed-content tool-call-partials parsed-usage)
                  (%openai-compat-collect-stream body-stream callback)
                (setf result
                      (%openai-compat-normalize-stream-result parsed-role
                                                            parsed-content
                                                            tool-call-partials
                                                            parsed-usage)))
            (handler-case (close body-stream)
              (error () nil)))
          result)))))

(defmethod list-provider-models ((provider openai-compatible-provider))
  "List models from /models endpoint."
  (handler-case
      (let* ((url (%openai-compat-endpoint provider "/models"))
             (headers (%openai-compat-headers provider)))
        (multiple-value-bind (body status)
            (dex:request url
                         :method :get
                         :headers headers
                         :read-timeout (provider-timeout-seconds provider)
                         :connect-timeout 30)
          (when (<= 200 status 299)
            (let* ((text (cond ((stringp body) body)
                               ((streamp body)
                                (handler-case (uiop:slurp-stream-string body)
                                  (error () nil)))
                               (t nil)))
                   (parsed (when text (jonathan:parse text :as :hash-table)))
                  (data (and (hash-table-p parsed) (gethash "data" parsed))))
              (when (listp data)
	                     (mapcar (lambda (item)
	                               (if (hash-table-p item)
	                                   (hash-to-model-info item)
	                                   item))
	                        data))))))
    (error () nil)))
