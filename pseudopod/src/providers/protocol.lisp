(in-package :pseudopod)

;;; ---------------------------------------------------------------------------
;;; Provider Protocol (I94)
;;;
;;; Abstract CLOS base class + generic functions for multi-model providers.
;;; Concrete subclasses: kimi-provider, anthropic-provider, openai-compatible-provider.
;;; ---------------------------------------------------------------------------

(defclass provider ()
  ((name :initarg :name :reader provider-name :type string)
   (api-key :initarg :api-key :accessor provider-api-key :type string)
   (base-url :initarg :base-url :accessor provider-base-url :type string)
   (default-model :initarg :default-model :accessor provider-default-model :type string)
   (timeout-seconds :initarg :timeout-seconds :accessor provider-timeout-seconds
                    :type integer :initform 180)
   (max-response-bytes :initarg :max-response-bytes :accessor provider-max-response-bytes
                       :type integer :initform (* 64 1024 1024))
   (healthy-p :initform t :accessor provider-healthy-p :type boolean)
   (last-error :initform nil :accessor provider-last-error)
   (request-count :initform 0 :accessor provider-request-count :type integer)
   (error-count :initform 0 :accessor provider-error-count :type integer)
   (last-latency-ms :initform 0 :accessor provider-last-latency-ms :type integer))
  (:documentation "Abstract base class for LLM API providers."))

(defmethod print-object ((obj provider) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~A model=~A healthy=~A"
            (provider-name obj)
            (provider-default-model obj)
            (provider-healthy-p obj))))

;;; --- Generic Functions ---

(defgeneric send-chat-completion (provider messages &key model temperature max-tokens
                                                        top-p tools tool-choice
                                                        system-prompt extra-params)
  (:documentation "Send a chat completion request. Returns a hash-table response."))

(defgeneric send-streaming-completion (provider messages callback &key model temperature
                                                                       max-tokens top-p
                                                                       tools tool-choice
                                                                       system-prompt extra-params)
  (:documentation "Send a streaming chat completion. CALLBACK is called with each chunk."))

(defgeneric list-provider-models (provider)
  (:documentation "Return a list of model-info structs available on this provider."))

(defgeneric estimate-provider-tokens (provider text)
  (:documentation "Estimate token count for TEXT using provider-specific heuristics."))

(defgeneric provider-health-check (provider)
  (:documentation "Perform a health check on the provider. Returns T if healthy."))

;;; --- Default Methods ---

(defmethod estimate-provider-tokens ((provider provider) text)
  "Default: ~4 chars per token heuristic."
  (ceiling (length text) 4))

(defmethod provider-health-check ((provider provider))
  "Default: try to list models."
  (handler-case
      (progn
        (list-provider-models provider)
        (setf (provider-healthy-p provider) t)
        t)
    (error (c)
      (setf (provider-healthy-p provider) nil
            (provider-last-error provider) c)
      nil)))

;;; --- Provider Metrics ---

(defun provider-record-request (provider latency-ms &optional errorp)
  "Record a request to PROVIDER metrics."
  (incf (provider-request-count provider))
  (setf (provider-last-latency-ms provider) latency-ms)
  (when errorp
    (incf (provider-error-count provider)))
  provider)

(defun provider-error-rate (provider)
  "Return error rate as a float 0.0-1.0."
  (let ((total (provider-request-count provider)))
    (if (zerop total)
        0.0
        (/ (float (provider-error-count provider)) (float total)))))

;;; --- with-provider macro ---

(defvar *current-provider* nil
  "Dynamically bound provider for with-provider macro.")

(defmacro with-provider ((provider) &body body)
  "Execute BODY with *current-provider* bound to PROVIDER."
  `(let ((*current-provider* ,provider))
     ,@body))

(defun current-provider ()
  "Return the current dynamically-bound provider, or NIL."
  *current-provider*)

;;; --- Provider-aware wrappers ---

(defun %provider-timed-call (provider fn)
  "Call FN, tracking latency and errors in PROVIDER metrics."
  (let ((start-ms (get-internal-real-time)))
    (handler-case
        (let ((result (funcall fn)))
          (let ((elapsed-ms (round (* 1000.0
                                      (/ (- (get-internal-real-time) start-ms)
                                         internal-time-units-per-second)))))
            (provider-record-request provider elapsed-ms nil))
          result)
      (error (c)
        (let ((elapsed-ms (round (* 1000.0
                                    (/ (- (get-internal-real-time) start-ms)
                                       internal-time-units-per-second)))))
          (provider-record-request provider elapsed-ms t)
          (setf (provider-last-error provider) c)
          (error c))))))
