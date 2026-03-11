(in-package :pseudopod/test)

;;; ---------------------------------------------------------------------------
;;; Provider protocol tranche tests (I243)
;;; ---------------------------------------------------------------------------

(def-suite provider-protocol-suite :in pseudopod-suite
  :description "I243 provider protocol + Kimi/Anthropic coverage.")

(in-suite provider-protocol-suite)

(defun %sequence->list* (value)
  (cond
    ((null value) nil)
    ((listp value) value)
    ((vectorp value) (coerce value 'list))
    (t (list value))))

(defun %i341-image-content-part (&key
                                   (media-type "image/png")
                                   (data "ZmFrZS1pbWFnZS1ieXRlcw==")
                                   (label "[image fixture.png]"))
  (let ((part (make-hash-table :test #'equal)))
    (setf (gethash "type" part) "image"
          (gethash "media_type" part) media-type
          (gethash "data" part) data
          (gethash "text" part) label
          (gethash "path" part) "fixtures/fixture.png")
    part))

(defun %method-present-p (generic-symbol class-symbol &optional (arity 1))
  (let* ((generic (symbol-function generic-symbol))
         (specializers (loop for index from 0 below arity
                             collect (if (zerop index)
                                         (find-class class-symbol)
                                         (find-class t)))))
    (not (null (ignore-errors
                 (find-method generic nil specializers nil))))))

(test provider-protocol-generics-implemented-for-kimi-and-anthropic
  (dolist (provider-class '(pseudopod:kimi-provider pseudopod:anthropic-provider))
    (is-true (%method-present-p 'pseudopod:send-chat-completion provider-class 2))
    (is-true (%method-present-p 'pseudopod:send-streaming-completion provider-class 3))
    (is-true (%method-present-p 'pseudopod:list-provider-models provider-class 1))
    (is-true (%method-present-p 'pseudopod:estimate-provider-tokens provider-class 2))))

(test anthropic-provider-coerces-tool-use-and-tool-result-messages
  (let ((captured-payload nil))
    (with-stub-dex
        (:request (lambda (url &rest args &key method content &allow-other-keys)
                    (declare (ignore args url))
                    (unless (eql method :post)
                      (error "Expected POST request."))
                    (setf captured-payload (jonathan:parse content :as :hash-table))
                    (let ((response (make-hash-table :test #'equal))
                          (content-block (make-hash-table :test #'equal))
                          (usage (make-hash-table :test #'equal)))
                      (setf (gethash "type" content-block) "text")
                      (setf (gethash "text" content-block) "ok")
                      (setf (gethash "content" response) (vector content-block))
                      (setf (gethash "input_tokens" usage) 1)
                      (setf (gethash "output_tokens" usage) 1)
                      (setf (gethash "usage" response) usage)
                      (values (jonathan:to-json response) 200))))
      (let* ((provider (pseudopod:make-anthropic-provider :api-key "sk-ant-test"
                                                          :base-url "http://localhost"))
             (assistant-tool-call (pseudopod:make-tool-call
                                   :id "call_1"
                                   :name "lookup"
                                   :arguments "{\"query\":\"status\"}"))
             (messages (list
                        (pseudopod:make-message :role "system"
                                                :content "System policy")
                        (pseudopod:make-message :role "assistant"
                                                :content ""
                                                :tool-calls (list assistant-tool-call))
                        (pseudopod:make-message :role "tool"
                                                :tool-call-id "call_1"
                                                :content "tool-output"))))
        (pseudopod:send-chat-completion provider messages)
        (let* ((request-messages (%sequence->list* (gethash "messages" captured-payload)))
               (assistant-message (first request-messages))
               (tool-result-message (second request-messages))
               (assistant-content (%sequence->list* (and (hash-table-p assistant-message)
                                                         (gethash "content" assistant-message))))
               (tool-use-block (find "tool_use" assistant-content
                                     :key (lambda (b)
                                            (and (hash-table-p b) (gethash "type" b)))
                                     :test #'string=))
               (tool-result-content (%sequence->list* (and (hash-table-p tool-result-message)
                                                           (gethash "content" tool-result-message))))
               (tool-result-block (first tool-result-content)))
          (is (string= "System policy" (or (gethash "system" captured-payload) "")))
          (is (= 2 (length request-messages)))
          (is (string= "assistant" (or (gethash "role" assistant-message) "")))
          (is (hash-table-p tool-use-block))
          (is (string= "call_1" (or (gethash "id" tool-use-block) "")))
          (is (string= "lookup" (or (gethash "name" tool-use-block) "")))
          (let ((input (and (hash-table-p tool-use-block) (gethash "input" tool-use-block))))
            (is (hash-table-p input))
            (is (string= "status" (or (gethash "query" input) ""))))
          (is (string= "user" (or (gethash "role" tool-result-message) "")))
          (is (hash-table-p tool-result-block))
          (is (string= "tool_result" (or (gethash "type" tool-result-block) "")))
          (is (string= "call_1" (or (gethash "tool_use_id" tool-result-block) "")))
          (is (string= "tool-output" (or (gethash "content" tool-result-block) ""))))))))

(test openai-provider-coerces-generic-image-content-part
  (let ((captured-payload nil))
    (with-stub-dex
        (:request (lambda (url &rest args &key method content &allow-other-keys)
                    (declare (ignore args url))
                    (unless (eql method :post)
                      (error "Expected POST request."))
                    (setf captured-payload (jonathan:parse content :as :hash-table))
                    (let ((response (make-hash-table :test #'equal))
                          (choice (make-hash-table :test #'equal))
                          (message (make-hash-table :test #'equal)))
                      (setf (gethash "role" message) "assistant"
                            (gethash "content" message) "ok"
                            (gethash "message" choice) message
                            (gethash "choices" response) (vector choice))
                      (values (jonathan:to-json response) 200))))
      (let* ((provider (pseudopod:make-openai-compatible-provider
                        :api-key "sk-openai-test"
                        :base-url "http://localhost/v1"
                        :model "gpt-4o"
                        :name "openai"))
             (messages (list (pseudopod:make-message
                              :role "user"
                              :content (list (pseudopod:make-text-part "triage")
                                             (%i341-image-content-part))))))
        (pseudopod:send-chat-completion provider messages)
        (let* ((request-messages (%sequence->list* (gethash "messages" captured-payload)))
               (user-message (first request-messages))
               (content-blocks (%sequence->list* (gethash "content" user-message)))
               (image-block (find "image_url"
                                  content-blocks
                                  :test #'string=
                                  :key (lambda (block)
                                         (and (hash-table-p block)
                                              (gethash "type" block)))))
               (image-url (and (hash-table-p image-block)
                               (gethash "image_url" image-block))))
          (is (= 1 (length request-messages)))
          (is (>= (length content-blocks) 2))
          (is (hash-table-p image-block))
          (is (hash-table-p image-url))
          (is-true (uiop:string-prefix-p
                    "data:image/png;base64,"
                    (or (gethash "url" image-url) ""))))))))

(test anthropic-provider-coerces-generic-image-content-part
  (let ((captured-payload nil))
    (with-stub-dex
        (:request (lambda (url &rest args &key method content &allow-other-keys)
                    (declare (ignore args url))
                    (unless (eql method :post)
                      (error "Expected POST request."))
                    (setf captured-payload (jonathan:parse content :as :hash-table))
                    (let ((response (make-hash-table :test #'equal))
                          (content-block (make-hash-table :test #'equal))
                          (usage (make-hash-table :test #'equal)))
                      (setf (gethash "type" content-block) "text")
                      (setf (gethash "text" content-block) "ok")
                      (setf (gethash "content" response) (vector content-block))
                      (setf (gethash "input_tokens" usage) 1)
                      (setf (gethash "output_tokens" usage) 1)
                      (setf (gethash "usage" response) usage)
                      (values (jonathan:to-json response) 200))))
      (let* ((provider (pseudopod:make-anthropic-provider
                        :api-key "sk-ant-test"
                        :base-url "http://localhost"))
             (messages (list (pseudopod:make-message
                              :role "user"
                              :content (list "triage"
                                             (%i341-image-content-part))))))
        (pseudopod:send-chat-completion provider messages)
        (let* ((request-messages (%sequence->list* (gethash "messages" captured-payload)))
               (user-message (first request-messages))
               (content-blocks (%sequence->list* (gethash "content" user-message)))
               (image-block (find "image"
                                  content-blocks
                                  :test #'string=
                                  :key (lambda (block)
                                         (and (hash-table-p block)
                                              (gethash "type" block)))))
               (source (and (hash-table-p image-block)
                            (gethash "source" image-block))))
          (is (= 1 (length request-messages)))
          (is (hash-table-p image-block))
          (is (hash-table-p source))
          (is (string= "base64" (or (gethash "type" source) "")))
          (is (string= "image/png" (or (gethash "media_type" source) "")))
          (is (string= "ZmFrZS1pbWFnZS1ieXRlcw=="
                       (or (gethash "data" source) ""))))))))

(test kimi-provider-streaming-callback-fires
  (let ((original-stream-fn (symbol-function 'pseudopod:stream-chat-completion)))
    (unwind-protect
         (progn
           (setf (symbol-function 'pseudopod:stream-chat-completion)
                 (lambda (client user-prompt callback &rest args &key &allow-other-keys)
                   (declare (ignore client user-prompt args))
                   (funcall callback "Kimi ")
                   (funcall callback "stream")
                   (let ((response (make-hash-table :test #'equal)))
                     (setf (gethash "role" response) "assistant")
                     (setf (gethash "content" response) "Kimi stream")
                     response)))
           (let* ((provider (pseudopod:make-kimi-provider
                             :client (pseudopod:make-client :api-key "sk-kimi")))
                  (chunks nil)
                  (result (pseudopod:send-streaming-completion
                           provider
                           (list (pseudopod:make-message :role "user" :content "hi"))
                           (lambda (chunk) (push chunk chunks)))))
             (is (equal '("Kimi " "stream") (reverse chunks)))
             (is (string= "assistant" (or (gethash "role" result) "")))
             (is (string= "Kimi stream" (or (gethash "content" result) "")))))
      (setf (symbol-function 'pseudopod:stream-chat-completion) original-stream-fn))))

(defparameter *provider-protocol-anthropic-sse*
  (with-output-to-string (stream)
    (format stream "event: message_start~%")
    (format stream "data: {\"type\":\"message_start\",\"message\":{\"role\":\"assistant\"}}~%~%")
    (format stream "event: content_block_start~%")
    (format stream "data: {\"type\":\"content_block_start\",\"index\":0,\"content_block\":{\"type\":\"text\"}}~%~%")
    (format stream "event: content_block_delta~%")
    (format stream "data: {\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"text_delta\",\"text\":\"Anthropic \"}}~%~%")
    (format stream "event: content_block_delta~%")
    (format stream "data: {\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"text_delta\",\"text\":\"stream\"}}~%~%")
    (format stream "event: content_block_stop~%")
    (format stream "data: {\"type\":\"content_block_stop\",\"index\":0}~%~%")
    (format stream "event: message_stop~%")
    (format stream "data: {\"type\":\"message_stop\",\"message\":{\"usage\":{\"input_tokens\":2,\"output_tokens\":3}}}~%~%")))

(test anthropic-provider-streaming-callback-fires
  (with-stub-dex
      (:request (lambda (url &rest args &key method want-stream &allow-other-keys)
                  (declare (ignore args url))
                  (unless (eql method :post)
                    (error "Expected POST request."))
                  (unless want-stream
                    (error "Expected streaming request."))
                  (values (make-string-input-stream *provider-protocol-anthropic-sse*) 200)))
    (let* ((provider (pseudopod:make-anthropic-provider :api-key "sk-ant-test"
                                                        :base-url "http://localhost"))
           (chunks nil)
           (result (pseudopod:send-streaming-completion
                    provider
                    (list (pseudopod:make-message :role "user" :content "stream"))
                    (lambda (chunk) (push chunk chunks)))))
      (is (equal '("Anthropic " "stream") (reverse chunks)))
      (is (string= "assistant" (or (gethash "role" result) "")))
      (is (string= "Anthropic stream" (or (gethash "content" result) ""))))))

(test provider-protocol-smoke-sentinel
  (format t "PROVIDER_PROTOCOL_SMOKE_OK~%")
  (is-true t))
