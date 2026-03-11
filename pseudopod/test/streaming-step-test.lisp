(in-package :pseudopod/test)

;;; ---------------------------------------------------------------------------
;;; Streaming conversation step tests (I246)
;;; ---------------------------------------------------------------------------

(def-suite streaming-step-suite :in pseudopod-suite
  :description "conversation-step-streaming chunk and cancellation behavior.")

(in-suite streaming-step-suite)

(defun %streaming-step-hash (&rest kvs)
  (let ((hash (make-hash-table :test #'equal)))
    (loop for (key value) on kvs by #'cddr
          do (setf (gethash key hash) value))
    hash))

(defparameter *streaming-step-sse-fixture*
  (make-stream-sse-body
   (make-stream-sse-payload
    :role "assistant"
    :content "Hello "
    :tool-calls (list
                 (make-stream-tool-call-delta
                  :index 0
                  :id "stream-call-1"
                  :type "function"
                  :name "lookup"
                  :arguments "{\"query\":\"stream")))
   (make-stream-sse-payload
    :content "world"
    :tool-calls (list
                 (make-stream-tool-call-delta
                  :index 0
                  :arguments "ing\"}")))
   (%streaming-step-hash
    "choices" (vector)
    "usage" (%streaming-step-hash
             "prompt_tokens" 7
             "completion_tokens" 5
             "total_tokens" 12))))

(defparameter *streaming-step-cancel-fixture*
  (make-stream-sse-body
   (make-stream-sse-payload :role "assistant" :content "first ")
   (make-stream-sse-payload :content "second ")
   (make-stream-sse-payload :content "third ")))

(test conversation-step-streaming-emits-text-tool-usage-and-done
  (with-stub-dex
      (:post (lambda (url &rest args &key content want-stream &allow-other-keys)
               (declare (ignore args url))
               (unless want-stream
                 (error "Expected streaming request."))
               (let ((request (jonathan:parse content :as :hash-table)))
                 (unless (gethash "stream" request)
                   (error "Expected stream=true payload.")))
               (values (make-string-input-stream *streaming-step-sse-fixture*) 200)))
    (let* ((client (pseudopod:make-client :api-key "stub"))
           (conversation (pseudopod:make-conversation :client client))
           (stream-id "stream-i246-main")
           (chunks nil)
           (tool-deltas nil)
           (usage-deltas nil)
           (done-chunk nil))
      (multiple-value-bind (assistant-message actual-stream-id stream-status usage parse-error-count)
          (pseudopod:conversation-step-streaming
           conversation
           :user-prompt "Stream once."
           :stream-id stream-id
           :on-chunk (lambda (chunk)
                       (push chunk chunks))
           :on-tool-call-delta (lambda (chunk)
                                 (push chunk tool-deltas))
           :on-usage-delta (lambda (chunk)
                             (push chunk usage-deltas))
           :on-done (lambda (chunk)
                      (setf done-chunk chunk)))
        (let* ((ordered-chunks (reverse chunks))
               (chunk-types (mapcar (lambda (chunk)
                                      (getf chunk :type))
                                    ordered-chunks))
               (tool-call (first (pseudopod:message-tool-calls assistant-message)))
               (final-tool-delta (first tool-deltas))
               (usage-delta (first usage-deltas)))
          (is-true (pseudopod:message-p assistant-message))
          (is (string= "assistant" (pseudopod:message-role assistant-message)))
          (is (string= "Hello world"
                       (or (message-first-text assistant-message) "")))
          (is (string= stream-id actual-stream-id))
          (is (eq :completed stream-status))
          (is (= 0 parse-error-count))
          (is (= 2 (length (pseudopod:conversation-history conversation))))
          (is (= 1 (length (pseudopod:message-tool-calls assistant-message))))
          (is-true (pseudopod:tool-call-p tool-call))
          (is (string= "lookup" (pseudopod:tool-call-name tool-call)))
          (is (string= "{\"query\":\"streaming\"}"
                       (or (pseudopod:tool-call-arguments tool-call) "")))
          (is (member :text-delta chunk-types))
          (is (member :tool-call-delta chunk-types))
          (is (member :usage-delta chunk-types))
          (is (member :done chunk-types))
          (is-true (hash-table-p usage))
          (is (= 12 (gethash "total_tokens" usage)))
          (is-true (hash-table-p (getf usage-delta :usage)))
          (is-true (not (null (getf final-tool-delta :arguments-complete-p))))
          (is (string= "{\"query\":\"streaming\"}"
                       (or (getf final-tool-delta :arguments) "")))
          (is (eq :done (getf done-chunk :type)))
          (is (eq :completed (getf done-chunk :status))))))))

(test conversation-step-streaming-can-be-cancelled
  (with-stub-dex
      (:post (lambda (url &rest args &key content want-stream &allow-other-keys)
               (declare (ignore args url content))
               (unless want-stream
                 (error "Expected streaming request."))
               (values (make-string-input-stream *streaming-step-cancel-fixture*) 200)))
    (let* ((client (pseudopod:make-client :api-key "stub"))
           (conversation (pseudopod:make-conversation :client client))
           (stream-id "stream-i246-cancel")
           (seen-text nil)
           (cancelled-p nil)
           (done-chunk nil))
      (multiple-value-bind (assistant-message actual-stream-id stream-status usage parse-error-count)
          (pseudopod:conversation-step-streaming
           conversation
           :user-prompt "Cancel this stream."
           :stream-id stream-id
           :on-chunk (lambda (chunk)
                       (when (eq :text-delta (getf chunk :type))
                         (push (getf chunk :text) seen-text)
                         (unless cancelled-p
                           (setf cancelled-p
                                 (pseudopod:cancel-stream stream-id)))))
           :on-done (lambda (chunk)
                      (setf done-chunk chunk)))
        (declare (ignore usage))
        (is-true cancelled-p)
        (is (string= stream-id actual-stream-id))
        (is (eq :cancelled stream-status))
        (is (= 0 parse-error-count))
        (is (equal '("first ") (reverse seen-text)))
        (is (string= "first " (or (message-first-text assistant-message) "")))
        (is (eq :done (getf done-chunk :type)))
        (is (eq :cancelled (getf done-chunk :status)))))))

(test streaming-step-smoke
  (format t "STREAMING_STEP_SMOKE_OK~%")
  (is-true t))
