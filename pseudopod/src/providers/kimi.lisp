(in-package :pseudopod)

;;; ---------------------------------------------------------------------------
;;; Kimi Provider (I94)
;;;
;;; Wraps the existing pseudopod:client for Moonshot/Kimi API.
;;; Zero code duplication -- delegates to chat-completion / stream-chat-completion.
;;; ---------------------------------------------------------------------------

(defclass kimi-provider (provider)
  ((client :initarg :client :accessor kimi-provider-client :type (or null client)
           :initform nil))
  (:default-initargs
   :name "kimi"
   :base-url *default-base-url*
   :default-model *default-model*)
  (:documentation "Provider wrapping the existing Moonshot/Kimi pseudopod client."))

(defun make-kimi-provider (&key api-key (base-url *default-base-url*)
                                (model *default-model*)
                                (temperature 1.0d0)
                                (max-tokens 32768)
                                (top-p 0.95d0)
                                (timeout-seconds 180)
                                client)
  "Create a kimi-provider. If CLIENT is given, wraps it; otherwise creates one."
  (let* ((resolved-key (or api-key
                           (and client (client-api-key client))
                           (handler-case (read-api-key) (error () ""))))
         (resolved-client (or client
                              (handler-case
                                  (make-client :api-key resolved-key
                                               :base-url base-url
                                               :model model
                                               :temperature temperature
                                               :max-tokens max-tokens
                                               :top-p top-p
                                               :timeout-seconds timeout-seconds)
                                (error ()
                                  nil)))))
    (make-instance 'kimi-provider
                   :name "kimi"
                   :api-key resolved-key
                   :base-url base-url
                   :default-model model
                   :timeout-seconds timeout-seconds
                   :client resolved-client)))

(defun %ensure-kimi-client (provider)
  "Ensure the kimi-provider has a valid client."
  (or (kimi-provider-client provider)
      (let ((c (make-client :api-key (provider-api-key provider)
                            :base-url (provider-base-url provider)
                            :model (provider-default-model provider)
                            :timeout-seconds (provider-timeout-seconds provider))))
        (setf (kimi-provider-client provider) c)
        c)))

(defun %kimi-build-messages (messages system-prompt)
  "Build raw message list from MESSAGES, prepending SYSTEM-PROMPT if given."
  (let ((raw (mapcar (lambda (m)
                       (cond ((message-p m) (message-to-hash m))
                             ((hash-table-p m) m)
                             ((stringp m) (%make-raw-message "user" m))
                             (t m)))
                     messages)))
    (if system-prompt
        (cons (%make-raw-message "system" system-prompt) raw)
        raw)))

(defmethod send-chat-completion ((provider kimi-provider) messages
                                 &key model temperature max-tokens top-p
                                      tools tool-choice system-prompt extra-params)
  (declare (ignore tool-choice extra-params))
  (let* ((client (%ensure-kimi-client provider))
         (resolved-model (or model (provider-default-model provider)))
         (raw-messages (%kimi-build-messages messages system-prompt))
         ;; Temporarily override client model for this request
         (saved-model (client-model client)))
    (unwind-protect
         (%provider-timed-call provider
           (lambda ()
             (setf (client-model client) resolved-model)
             (when temperature (setf (client-temperature client) temperature))
             (when max-tokens (setf (client-max-tokens client) max-tokens))
             (when top-p (setf (client-top-p client) top-p))
             (chat-completion client nil :messages raw-messages :tools tools)))
      (setf (client-model client) saved-model))))

(defmethod send-streaming-completion ((provider kimi-provider) messages callback
                                      &key model temperature max-tokens top-p
                                           tools tool-choice system-prompt extra-params)
  (declare (ignore tool-choice extra-params))
  (let* ((client (%ensure-kimi-client provider))
         (resolved-model (or model (provider-default-model provider)))
         (raw-messages (%kimi-build-messages messages system-prompt))
         (saved-model (client-model client)))
    (unwind-protect
         (%provider-timed-call provider
           (lambda ()
             (setf (client-model client) resolved-model)
             (when temperature (setf (client-temperature client) temperature))
             (when max-tokens (setf (client-max-tokens client) max-tokens))
             (when top-p (setf (client-top-p client) top-p))
             (stream-chat-completion client nil callback
                                     :messages raw-messages
                                     :tools tools)))
      (setf (client-model client) saved-model))))

(defmethod list-provider-models ((provider kimi-provider))
  (let ((client (%ensure-kimi-client provider)))
    (list-models client)))

(defmethod estimate-provider-tokens ((provider kimi-provider) text)
  (let ((client (%ensure-kimi-client provider)))
    (estimate-tokens client :text text)))
