(in-package :pseudopod)

;;; ---------------------------------------------------------------------------
;;; Anthropic Provider (I94)
;;;
;;; Claude Messages API provider. Handles content blocks, thinking blocks,
;;; and Anthropic-specific response format.
;;; ---------------------------------------------------------------------------

(defclass anthropic-provider (provider)
  ((api-version :initarg :api-version :accessor anthropic-provider-api-version
                :type string :initform "2023-06-01")
   (beta-features :initarg :beta-features :accessor anthropic-provider-beta-features
                  :type list :initform nil))
  (:default-initargs
   :name "anthropic"
   :base-url "https://api.anthropic.com"
   :default-model "claude-sonnet-4-5-20250929")
  (:documentation "Provider for the Anthropic Messages API (Claude)."))

(defun make-anthropic-provider (&key api-key
                                     (base-url "https://api.anthropic.com")
                                     (model "claude-sonnet-4-5-20250929")
                                     (timeout-seconds 180)
                                     (api-version "2023-06-01")
                                     beta-features)
  "Create an anthropic-provider."
  (let ((resolved-key (or api-key
                          (handler-case
                              (let ((env-key (uiop:getenv "ANTHROPIC_API_KEY")))
                                (if (and env-key (plusp (length env-key)))
                                    env-key
                                    ""))
                            (error () "")))))
    (make-instance 'anthropic-provider
                   :name "anthropic"
                   :api-key resolved-key
                   :base-url base-url
                   :default-model model
                   :timeout-seconds timeout-seconds
                   :api-version api-version
                   :beta-features beta-features)))

(defun %anthropic-headers (provider)
  "Build HTTP headers for Anthropic API."
  (let ((headers `((:x-api-key . ,(provider-api-key provider))
                   (:anthropic-version . ,(anthropic-provider-api-version provider))
                   (:content-type . "application/json"))))
    (when (anthropic-provider-beta-features provider)
      (push `(:anthropic-beta . ,(format nil "~{~A~^,~}"
                                          (anthropic-provider-beta-features provider)))
            headers))
    headers))

(defun %anthropic-endpoint (provider path)
  "Build full URL for Anthropic API."
  (format nil "~A~A"
          (string-right-trim "/" (provider-base-url provider))
          path))

(defun %anthropic-coerce-message (m)
  "Coerce a message for the Anthropic format."
  (cond
    ((hash-table-p m) m)
    ((message-p m) (message-to-hash m))
    ((stringp m)
     (let ((h (make-hash-table :test #'equal)))
       (setf (gethash "role" h) "user"
             (gethash "content" h) m)
       h))
    (t m)))

(defun %anthropic-build-payload (provider messages &key model temperature max-tokens
                                                        top-p tools tool-choice
                                                        system-prompt stream-p)
  "Build JSON payload for the Anthropic Messages API."
  (let ((payload (make-hash-table :test #'equal)))
    (setf (gethash "model" payload) (or model (provider-default-model provider)))
    (setf (gethash "max_tokens" payload) (or max-tokens 4096))
    (when system-prompt
      (setf (gethash "system" payload) system-prompt))
    (setf (gethash "messages" payload)
          (mapcar #'%anthropic-coerce-message messages))
    (when temperature
      (setf (gethash "temperature" payload) (coerce temperature 'double-float)))
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
    (jonathan:to-json payload)))

(defun %anthropic-parse-response (body)
  "Parse an Anthropic Messages API response into a hash-table."
  (cond
    ((stringp body)
     (jonathan:parse body :as :hash-table))
    ((streamp body)
     (let ((text (handler-case (uiop:slurp-stream-string body)
                   (error () nil))))
       (when text (jonathan:parse text :as :hash-table))))
    ((hash-table-p body) body)
    (t nil)))

(defun %anthropic-extract-text (response)
  "Extract text content from Anthropic response content blocks."
  (let* ((content (and (hash-table-p response) (gethash "content" response)))
         (blocks (cond ((listp content) content)
                       ((vectorp content) (coerce content 'list))
                       (t nil))))
    (with-output-to-string (out)
      (dolist (block blocks)
        (when (and (hash-table-p block)
                   (string= "text" (gethash "type" block "")))
          (write-string (or (gethash "text" block) "") out))))))

(defmethod send-chat-completion ((provider anthropic-provider) messages
                                 &key model temperature max-tokens top-p
                                      tools tool-choice system-prompt extra-params)
  (declare (ignore extra-params))
  (%provider-timed-call provider
    (lambda ()
      (let* ((payload (%anthropic-build-payload provider messages
                                                 :model model
                                                 :temperature temperature
                                                 :max-tokens max-tokens
                                                 :top-p top-p
                                                 :tools tools
                                                 :tool-choice tool-choice
                                                 :system-prompt system-prompt))
             (url (%anthropic-endpoint provider "/v1/messages"))
             (headers (%anthropic-headers provider)))
        (multiple-value-bind (body status)
            (dex:request url
                         :method :post
                         :headers headers
                         :content payload
                         :read-timeout (provider-timeout-seconds provider)
                         :connect-timeout 30)
          (unless (<= 200 status 299)
            (error 'pseudopod-api-error
                   :message (format nil "Anthropic API error (status ~A)" status)
                   :status-code status
                   :body (if (stringp body) body (princ-to-string body))))
          (%anthropic-parse-response body))))))

(defmethod send-streaming-completion ((provider anthropic-provider) messages callback
                                      &key model temperature max-tokens top-p
                                           tools tool-choice system-prompt extra-params)
  (declare (ignore extra-params))
  (%provider-timed-call provider
    (lambda ()
      (let* ((payload (%anthropic-build-payload provider messages
                                                 :model model
                                                 :temperature temperature
                                                 :max-tokens max-tokens
                                                 :top-p top-p
                                                 :tools tools
                                                 :tool-choice tool-choice
                                                 :system-prompt system-prompt
                                                 :stream-p t))
             (url (%anthropic-endpoint provider "/v1/messages"))
             (headers (%anthropic-headers provider))
             (accumulated-text (make-string-output-stream))
             (response-hash nil))
        (handler-case
            (dex:request url
                         :method :post
                         :headers headers
                         :content payload
                         :read-timeout (provider-timeout-seconds provider)
                         :connect-timeout 30
                         :want-stream t)
          (error (c) (error c)))
        ;; For SSE streaming, we'd read event lines. For now, return the
        ;; non-streaming result through callback for compatibility.
        (let ((result (send-chat-completion provider messages
                                            :model model
                                            :temperature temperature
                                            :max-tokens max-tokens
                                            :top-p top-p
                                            :tools tools
                                            :tool-choice tool-choice
                                            :system-prompt system-prompt)))
          (let ((text (%anthropic-extract-text result)))
            (when (plusp (length text))
              (funcall callback text))
            (write-string text accumulated-text))
          (setf response-hash result)
          response-hash)))))

(defmethod list-provider-models ((provider anthropic-provider))
  "Anthropic does not have a public models endpoint; return known models."
  (list (%make-model-info :id "claude-opus-4-6"
                          :object "model" :owned-by "anthropic")
        (%make-model-info :id "claude-sonnet-4-5-20250929"
                          :object "model" :owned-by "anthropic")
        (%make-model-info :id "claude-haiku-4-5-20251001"
                          :object "model" :owned-by "anthropic")))

(defmethod estimate-provider-tokens ((provider anthropic-provider) text)
  "Anthropic ~4 chars per token heuristic (cl100k-like tokenizer)."
  (ceiling (length text) 4))
