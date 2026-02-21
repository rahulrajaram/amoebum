(in-package :pseudopod/test)

;;; ---------------------------------------------------------------------------
;;; Anthropic streaming parser tests (I136)
;;; ---------------------------------------------------------------------------

(def-suite anthropic-streaming-suite :in pseudopod-suite
  :description "Anthropic provider SSE streaming parser tests.")

(in-suite anthropic-streaming-suite)

(defun %anthropic-make-table (&rest kvs)
  (let ((hash (make-hash-table :test #'equal)))
    (loop for (k v) on kvs by #'cddr
          do (setf (gethash k hash) v))
    hash))

(defun %anthropic-append-event (stream event payload)
  (format stream "event: ~A~%" event)
  (format stream "data: ~A~%~%" (jonathan:to-json payload)))

(defparameter *anthropic-streaming-sse-fixture*
  (with-output-to-string (stream)
    (%anthropic-append-event
     stream "message_start"
     (%anthropic-make-table
      "type" "message_start"
      "message" (%anthropic-make-table
                 "role" "assistant"
                 "usage" (%anthropic-make-table
                          "input_tokens" 12
                          "output_tokens" 0))))
    (%anthropic-append-event
     stream "content_block_start"
     (%anthropic-make-table
      "type" "content_block_start"
      "index" 0
      "content_block" (%anthropic-make-table "type" "text")))
    (%anthropic-append-event
     stream "content_block_delta"
     (%anthropic-make-table
      "type" "content_block_delta"
      "index" 0
      "delta" (%anthropic-make-table
               "type" "text_delta"
               "text" "Anthropic ")))
    (%anthropic-append-event
     stream "content_block_delta"
     (%anthropic-make-table
      "type" "content_block_delta"
      "index" 0
      "delta" (%anthropic-make-table
               "type" "text_delta"
               "text" "stream.")))
    (%anthropic-append-event
     stream "content_block_stop"
     (%anthropic-make-table
      "type" "content_block_stop"
      "index" 0))
    (%anthropic-append-event
     stream "content_block_start"
     (%anthropic-make-table
      "type" "content_block_start"
      "index" 1
      "content_block" (%anthropic-make-table
                       "type" "thinking")))
    (%anthropic-append-event
     stream "content_block_delta"
     (%anthropic-make-table
      "type" "content_block_delta"
      "index" 1
      "delta" (%anthropic-make-table
               "type" "thinking_delta"
               "text" "thinking")))
    (%anthropic-append-event
     stream "content_block_delta"
     (%anthropic-make-table
      "type" "content_block_delta"
      "index" 1
      "delta" (%anthropic-make-table
               "type" "thinking_delta"
               "text" "_path")))
    (%anthropic-append-event
     stream "content_block_stop"
     (%anthropic-make-table
      "type" "content_block_stop"
      "index" 1))
    (%anthropic-append-event
     stream "content_block_start"
     (%anthropic-make-table
      "type" "content_block_start"
      "index" 2
      "content_block" (%anthropic-make-table
                       "type" "tool_use"
                       "id" "anthropic-tool-call"
                       "name" "lookup")))
    (%anthropic-append-event
     stream "content_block_delta"
     (%anthropic-make-table
      "type" "content_block_delta"
      "index" 2
      "delta" (%anthropic-make-table
               "type" "input_json_delta"
               "partial_json" "{\"query\":\"Anthropic status")
      ))
    (%anthropic-append-event
     stream "content_block_delta"
     (%anthropic-make-table
      "type" "content_block_delta"
      "index" 2
      "delta" (%anthropic-make-table
               "type" "input_json_delta"
               "partial_json" "\"}")))
    (%anthropic-append-event
     stream "content_block_stop"
     (%anthropic-make-table
      "type" "content_block_stop"
      "index" 2))
    (%anthropic-append-event
     stream "message_delta"
     (%anthropic-make-table
      "type" "message_delta"
      "usage" (%anthropic-make-table
               "input_tokens" 12
               "output_tokens" 17)))
    (%anthropic-append-event
     stream "message_stop"
     (%anthropic-make-table
      "type" "message_stop"
      "message" (%anthropic-make-table
                 "usage" (%anthropic-make-table
                           "input_tokens" 12
                           "output_tokens" 19))))))

(defun %tool-calls-list (tool-calls)
  (cond
    ((null tool-calls) nil)
    ((listp tool-calls) tool-calls)
    ((vectorp tool-calls) (coerce tool-calls 'list))
    (t nil)))

(test anthropic-streaming-sse-parses-incremental-text-tool-calls-and-thinking
  (with-stub-dex
      (:request (lambda (url &rest args &key method content want-stream &allow-other-keys)
                  (declare (ignore args url content))
                  (unless (eql method :post)
                    (error "Expected POST request."))
               (unless want-stream (error "Expected streaming request."))
               (values (make-string-input-stream *anthropic-streaming-sse-fixture*) 200)))
    (let* ((provider (pseudopod:make-anthropic-provider :api-key "sk-ant-test"
                                                        :base-url "http://localhost"))
           (chunks nil)
           (result (pseudopod:send-streaming-completion
                    provider
                    (list (pseudopod:make-message :role "user"
                                                  :content "Streaming with tool"))
                    (lambda (chunk)
                      (push chunk chunks))
                    :model "claude-sonnet-4-5-20250929")))
      (setf chunks (reverse chunks))
      (is (= 2 (length chunks)))
      (is (equal '("Anthropic " "stream.") chunks))
      (is (string= "assistant" (or (gethash "role" result) "")))
      (is (string= "Anthropic stream." (gethash "content" result "")))
      (is (= 19 (gethash "output_tokens" (gethash "usage" result))))
      (let* ((tool-calls (%tool-calls-list (gethash "tool_calls" result "")))
             (tool-call (and tool-calls (first tool-calls)))
             (tool-function (and (hash-table-p tool-call)
                                (gethash "function" tool-call)))
             (tool-id (and (hash-table-p tool-call) (gethash "id" tool-call)))
             (tool-name (or (and (hash-table-p tool-function)
                                 (gethash "name" tool-function))
                            (and (hash-table-p tool-call)
                                 (gethash "name" tool-call))))
             (tool-args (or (and (hash-table-p tool-function)
                                 (gethash "arguments" tool-function))
                            (and (hash-table-p tool-call)
                                 (gethash "arguments" tool-call)))))
        (is (= 1 (length tool-calls)))
        (is (string= "anthropic-tool-call" tool-id))
        (is (string= "lookup" tool-name))
        (is (string= "{\"query\":\"Anthropic status\"}" tool-args)))
      (let ((content (or (gethash "content" result) "")))
        (is-false (search "thinking" content))
        (is-false (search "thinking_path" content))
        (is (stringp (gethash "content" result))))
      (is (string= "thinking_path" (or (gethash "thinking" result) ""))))))
