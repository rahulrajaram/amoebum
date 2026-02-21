(in-package :pseudopod/test)

;;; ---------------------------------------------------------------------------
;;; OpenAI-compatible streaming parser tests (I137)
;;; ---------------------------------------------------------------------------

(def-suite openai-streaming-suite :in pseudopod-suite
  :description "OpenAI-compatible provider SSE streaming parser tests.")

(in-suite openai-streaming-suite)

(defun %openai-test-hash (&rest kvs)
  (let ((hash (make-hash-table :test #'equal)))
    (loop for (k v) on kvs by #'cddr
          do (setf (gethash k hash) v))
    hash))

(defparameter *openai-streaming-sse-fixture*
  (with-output-to-string (stream)
    (format stream "data: ~A~%"
            (jonathan:to-json
             (make-stream-sse-payload
              :role "assistant"
              :content "OpenAI "
              :tool-calls (list
                           (make-stream-tool-call-delta
                            :index 0
                            :id "openai-tool-call"
                            :type "function"
                            :name "lookup"
                            :arguments "{\"query\":\"Open")))))
    (format stream "data: ~A~%"
            (jonathan:to-json
             (make-stream-sse-payload
              :content "stream"
              :tool-calls (list
                           (make-stream-tool-call-delta
                            :index 0
                            :arguments "AI\"}")))))
    (format stream "data: ~A~%"
            (jonathan:to-json
             (make-stream-sse-payload :content "!")))
    (format stream "data: ~A~%"
            (jonathan:to-json
             (%openai-test-hash
              "choices" (vector)
              "usage" (%openai-test-hash
                       "prompt_tokens" 11
                       "completion_tokens" 7
                       "total_tokens" 18))))
    (format stream "data: [DONE]~%")
    ;; Must be ignored after [DONE].
    (format stream "data: ~A~%"
            (jonathan:to-json
             (make-stream-sse-payload :content "ignored")))))

(defun %openai-tool-calls-list (tool-calls)
  (cond
    ((null tool-calls) nil)
    ((listp tool-calls) tool-calls)
    ((vectorp tool-calls) (coerce tool-calls 'list))
    (t nil)))

(test openai-streaming-sse-parses-text-tool-call-deltas-and-done
  (with-stub-dex
      (:request (lambda (url &rest args &key method content want-stream &allow-other-keys)
                  (declare (ignore args url))
                  (unless (eql method :post)
                    (error "Expected POST request."))
                  (unless want-stream
                    (error "Expected streaming request."))
                  (let ((request (jonathan:parse content :as :hash-table)))
                    (unless (gethash "stream" request)
                      (error "Expected stream=true in OpenAI payload.")))
                  (values (make-string-input-stream *openai-streaming-sse-fixture*) 200)))
    (let* ((provider (pseudopod:make-openai-compatible-provider
                      :api-key "sk-openai-test"
                      :base-url "http://localhost"))
           (chunks nil)
           (result (pseudopod:send-streaming-completion
                    provider
                    (list (pseudopod:make-message :role "user"
                                                  :content "stream test"))
                    (lambda (chunk)
                      (push chunk chunks))
                    :model "gpt-4o")))
      (setf chunks (reverse chunks))
      (is (equal '("OpenAI " "stream" "!") chunks))
      (is (string= "assistant" (or (gethash "role" result) "")))
      (is (string= "OpenAI stream!" (or (gethash "content" result) "")))
      (let ((usage (gethash "usage" result)))
        (is (= 11 (gethash "prompt_tokens" usage)))
        (is (= 7 (gethash "completion_tokens" usage)))
        (is (= 18 (gethash "total_tokens" usage))))
      (let* ((tool-calls (%openai-tool-calls-list (gethash "tool_calls" result)))
             (tool-call (and tool-calls (first tool-calls))))
        (is (= 1 (length tool-calls)))
        (is-true (pseudopod:tool-call-p tool-call))
        (is (string= "openai-tool-call"
                     (or (pseudopod:tool-call-id tool-call) "")))
        (is (string= "lookup" (or (pseudopod:tool-call-name tool-call) "")))
        (is (string= "{\"query\":\"OpenAI\"}"
                     (or (pseudopod:tool-call-arguments tool-call) "")))))))
