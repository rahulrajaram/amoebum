(defpackage :pseudopod/test
  (:use :cl :fiveam)
  (:export #:run-all #:pseudopod-suite))

(in-package :pseudopod/test)

(def-suite pseudopod-suite
  :description "Pseudopod SDK test suite")

(in-suite pseudopod-suite)

;;; ---- Helpers ----

(defun first-item (sequence)
  (cond
    ((null sequence) nil)
    ((listp sequence) (first sequence))
    ((vectorp sequence)
     (when (> (length sequence) 0)
       (aref sequence 0)))
    (t nil)))

(defun sequence->list (sequence)
  (cond
    ((null sequence) nil)
    ((listp sequence) sequence)
    ((vectorp sequence)
     (loop for item across sequence collect item))
    (t nil)))

(defun last-item (sequence)
  (let ((items (sequence->list sequence)))
    (and items (car (last items)))))

(defun message-first-text (message)
  (let* ((parts (pseudopod:message-content message))
         (first-part (and (listp parts) (first parts))))
    (and (pseudopod:content-part-p first-part)
         (pseudopod:content-part-text first-part))))

(defun raw-message-text (raw-message)
  (let ((content (and (hash-table-p raw-message)
                      (gethash "content" raw-message))))
    (cond
      ((stringp content) content)
      ((and (vectorp content)
            (> (length content) 0)
            (hash-table-p (aref content 0))
            (stringp (gethash "text" (aref content 0))))
       (gethash "text" (aref content 0)))
      ((and (listp content)
            (hash-table-p (first content))
            (stringp (gethash "text" (first content))))
       (gethash "text" (first content)))
      (t ""))))

(defun make-tool-schema ()
  (let ((schema (make-hash-table :test #'equal))
        (properties (make-hash-table :test #'equal)))
    (setf (gethash "type" schema) "object")
    (setf (gethash "properties" schema) properties)
    schema))

(defun make-stub-file-hash (&key (id "file-123")
                                 (filename "upload.txt")
                                 (bytes 12)
                                 (purpose "file-extract"))
  (let ((hash (make-hash-table :test #'equal)))
    (setf (gethash "id" hash) id)
    (setf (gethash "object" hash) "file")
    (setf (gethash "filename" hash) filename)
    (setf (gethash "bytes" hash) bytes)
    (setf (gethash "created_at" hash) 1700000000)
    (setf (gethash "purpose" hash) purpose)
    (setf (gethash "status" hash) "processed")
    hash))

(defun make-stub-model-hash (&key (id "kimi-k2.5")
                                  (owned-by "moonshot"))
  (let ((hash (make-hash-table :test #'equal)))
    (setf (gethash "id" hash) id)
    (setf (gethash "object" hash) "model")
    (setf (gethash "created" hash) 1700000000)
    (setf (gethash "owned_by" hash) owned-by)
    hash))

(defun alist-string-value (alist key)
  (cdr (assoc key alist :test #'string=)))

(defun make-stream-tool-call-delta (&key index id type name arguments)
  (let ((tool-call (make-hash-table :test #'equal))
        (function-body (make-hash-table :test #'equal)))
    (setf (gethash "index" tool-call) index)
    (when id
      (setf (gethash "id" tool-call) id))
    (when type
      (setf (gethash "type" tool-call) type))
    (when name
      (setf (gethash "name" function-body) name))
    (when arguments
      (setf (gethash "arguments" function-body) arguments))
    (when (plusp (hash-table-count function-body))
      (setf (gethash "function" tool-call) function-body))
    tool-call))

(defun make-stream-sse-payload (&key role reasoning content tool-calls)
  (let ((delta (make-hash-table :test #'equal))
        (choice (make-hash-table :test #'equal))
        (payload (make-hash-table :test #'equal)))
    (when role
      (setf (gethash "role" delta) role))
    (when reasoning
      (setf (gethash "reasoning_content" delta) reasoning))
    (when content
      (setf (gethash "content" delta) content))
    (when tool-calls
      (setf (gethash "tool_calls" delta) (coerce tool-calls 'vector)))
    (setf (gethash "delta" choice) delta)
    (setf (gethash "choices" payload) (vector choice))
    payload))

(defun make-stream-sse-body (&rest payloads)
  (with-output-to-string (stream)
    (dolist (payload payloads)
      (format stream "data: ~A~%" (jonathan:to-json payload)))
    (format stream "data: [DONE]~%")))

;;; ---- Stub macro for safe function rebinding ----

(defmacro with-stub-dex ((&key post get delete) &body body)
  "Safely rebind dexador functions for testing, restoring originals on exit."
  (let ((orig-post (gensym "ORIG-POST"))
        (orig-get (gensym "ORIG-GET"))
        (orig-delete (gensym "ORIG-DELETE")))
    `(let ((,orig-post (symbol-function 'dex:post))
           (,orig-get (symbol-function 'dex:get))
           (,orig-delete (symbol-function 'dex:delete)))
       (unwind-protect
           (progn
             ,@(when post
                 `((setf (symbol-function 'dex:post) ,post)))
             ,@(when get
                 `((setf (symbol-function 'dex:get) ,get)))
             ,@(when delete
                 `((setf (symbol-function 'dex:delete) ,delete)))
             ,@body)
         (setf (symbol-function 'dex:post) ,orig-post)
         (setf (symbol-function 'dex:get) ,orig-get)
         (setf (symbol-function 'dex:delete) ,orig-delete)))))

;;; ---- I14 Tests: Auth and Error Handling ----

(test bad-api-key-signals-auth-error
  (with-stub-dex
      (:post (lambda (url &rest args &key want-stream &allow-other-keys)
               (declare (ignore url args))
               (if want-stream
                   (values (make-string-input-stream "{\"error\":\"unauthorized\"}") 401)
                   (values "{\"error\":\"unauthorized\"}" 401))))
    (let ((bad-client (pseudopod:make-client :api-key "deliberately-bad-key")))
      (signals pseudopod:pseudopod-auth-error
        (pseudopod:chat-completion bad-client "Say hi.")))))

;;; ---- I15 Tests: Message Model ----

(test message-round-trip
  (dolist (role '("system" "user" "assistant" "tool"))
    (let* ((text (format nil "~A message" role))
           (source (pseudopod:make-message :role role :content text))
           (hash (pseudopod:message-to-hash source))
           (result (pseudopod:hash-to-message hash)))
      (is (string= role (pseudopod:message-role result)))
      (is (string= text (or (message-first-text result) ""))))))

(test chat-completion-star-returns-message
  (with-stub-dex
      (:post (lambda (url &rest args &key content want-stream &allow-other-keys)
               (declare (ignore url args want-stream))
               (let* ((request (jonathan:parse content :as :hash-table))
                      (messages (and (hash-table-p request)
                                     (gethash "messages" request)))
                      (choices (vector
                                (let ((choice (make-hash-table :test #'equal))
                                      (message (make-hash-table :test #'equal)))
                                  (setf (gethash "role" message) "assistant")
                                  (setf (gethash "content" message) "I15 typed reply")
                                  (setf (gethash "message" choice) message)
                                  choice)))
                      (response (make-hash-table :test #'equal)))
                 (unless (and (or (listp messages) (vectorp messages))
                              (> (length messages) 0))
                   (error "Expected serialized messages in payload, got ~S" request))
                 (setf (gethash "choices" response) choices)
                 (values (jonathan:to-json response) 200))))
    (let* ((client (pseudopod:make-client :api-key "stub"))
           (typed-messages (list
                            (pseudopod:make-message
                             :role "system"
                             :content "You are a test helper.")
                            (pseudopod:make-message
                             :role "user"
                             :content "Reply with typed output.")))
           (message (pseudopod:chat-completion*
                     client
                     ""
                     :messages typed-messages)))
      (is (pseudopod:message-p message))
      (is (string= "assistant" (pseudopod:message-role message)))
      (is (string= "I15 typed reply" (or (message-first-text message) ""))))))

;;; ---- I16 Tests: Tool Loop ----

(test step-loop-invokes-tool-and-returns-final-message
  (with-stub-dex
      (:post (lambda (url &rest args &key content want-stream &allow-other-keys)
               (declare (ignore url args want-stream))
               (let* ((request (jonathan:parse content :as :hash-table))
                      (messages (gethash "messages" request))
                      (tools (gethash "tools" request))
                      (messages-list (sequence->list messages))
                      (tool-message
                        (find-if (lambda (msg)
                                   (string= "tool"
                                            (or (and (hash-table-p msg)
                                                     (gethash "role" msg))
                                                "")))
                                 messages-list))
                      (choice (make-hash-table :test #'equal))
                      (assistant (make-hash-table :test #'equal))
                      (response (make-hash-table :test #'equal)))
                 (declare (ignore tools))
                 (setf (gethash "role" assistant) "assistant")
                 (if tool-message
                     (setf (gethash "content" assistant)
                           (format nil "Tool says ~A"
                                   (or (gethash "content" tool-message) "")))
                     (let ((tool-call (make-hash-table :test #'equal))
                           (function-body (make-hash-table :test #'equal)))
                       (setf (gethash "content" assistant) "")
                       (setf (gethash "id" tool-call) "call-1")
                       (setf (gethash "type" tool-call) "function")
                       (setf (gethash "name" function-body) "get-current-time")
                       (setf (gethash "arguments" function-body) "{}")
                       (setf (gethash "function" tool-call) function-body)
                       (setf (gethash "tool_calls" assistant) (vector tool-call))))
                 (setf (gethash "message" choice) assistant)
                 (setf (gethash "choices" response) (vector choice))
                 (setf (gethash "id" response) "resp-step-loop")
                 (values (jonathan:to-json response) 200))))
    (let* ((client (pseudopod:make-client :api-key "stub"))
           (toolset (pseudopod:make-toolset))
           (captured-call-count 0))
      (pseudopod:register-tool-function
       toolset
       :name "get-current-time"
       :description "Return a deterministic timestamp."
       :parameters (make-tool-schema)
       :fn (lambda (arguments tool-call)
             (declare (ignore arguments tool-call))
             (incf captured-call-count)
             "2026-02-13T00:00:00Z"))
      (let* ((result (pseudopod:step
                      client
                      :user-prompt "What time is it?"
                      :toolset toolset
                      :max-steps 4))
             (final-message (pseudopod:step-result-final-message result))
             (final-text (and (pseudopod:message-p final-message)
                              (message-first-text final-message)))
             (tool-results (pseudopod:step-result-tool-results result))
             (first-result (first tool-results)))
        (is-true (pseudopod:message-p final-message))
        (is (string= "assistant" (pseudopod:message-role final-message)))
        (is-true (and (stringp final-text) (search "2026-02-13T00:00:00Z" final-text)))
        (is (= 1 captured-call-count))
        (is-true (and first-result
                      (string= "2026-02-13T00:00:00Z"
                               (or (getf first-result :output) ""))))
        (is-false (pseudopod:step-result-max-steps-reached result))))))

(test step-loop-respects-max-steps
  (with-stub-dex
      (:post (lambda (url &rest args &key content want-stream &allow-other-keys)
               (declare (ignore url args content want-stream))
               (let ((choice (make-hash-table :test #'equal))
                     (assistant (make-hash-table :test #'equal))
                     (response (make-hash-table :test #'equal))
                     (tool-call (make-hash-table :test #'equal))
                     (function-body (make-hash-table :test #'equal)))
                 (setf (gethash "role" assistant) "assistant")
                 (setf (gethash "content" assistant) "")
                 (setf (gethash "id" tool-call) "call-repeat")
                 (setf (gethash "type" tool-call) "function")
                 (setf (gethash "name" function-body) "get-current-time")
                 (setf (gethash "arguments" function-body) "{}")
                 (setf (gethash "function" tool-call) function-body)
                 (setf (gethash "tool_calls" assistant) (vector tool-call))
                 (setf (gethash "message" choice) assistant)
                 (setf (gethash "choices" response) (vector choice))
                 (setf (gethash "id" response) "resp-max-steps")
                 (values (jonathan:to-json response) 200))))
    (let* ((client (pseudopod:make-client :api-key "stub"))
           (toolset (pseudopod:make-toolset)))
      (pseudopod:register-tool-function
       toolset
       :name "get-current-time"
       :description "Return a deterministic timestamp."
       :parameters (make-tool-schema)
       :fn (lambda (arguments tool-call)
             (declare (ignore arguments tool-call))
             "2026-02-13T00:00:00Z"))
      (let ((result (pseudopod:step
                     client
                     :user-prompt "Loop forever."
                     :toolset toolset
                     :max-steps 2)))
        (is-true (pseudopod:step-result-max-steps-reached result))
        (is (= 2 (pseudopod:step-result-steps result)))
        (is-false (pseudopod:step-result-final-message result))))))

;;; ---- I17 Tests: Streaming Tool Call Support ----

(test streaming-tool-call-support
  (with-stub-dex
      (:post (lambda (url &rest args &key content want-stream &allow-other-keys)
               (declare (ignore url args))
               (let* ((request (jonathan:parse content :as :hash-table))
                      (tools (gethash "tools" request)))
                 (declare (ignore tools))
                 (unless want-stream
                   (error "Expected streaming request for SSE test."))
                 (values
                  (make-string-input-stream
                   (make-stream-sse-body
                    (make-stream-sse-payload
                     :role "assistant"
                     :reasoning "think-step "
                     :content "Hello "
                     :tool-calls (list
                                  (make-stream-tool-call-delta
                                   :index 0
                                   :id "call-1"
                                   :type "function"
                                   :name "get-weather"
                                   :arguments "{\"city\":\"")
                                  (make-stream-tool-call-delta
                                   :index 1
                                   :id "call-2"
                                   :type "function"
                                   :name "get-time"
                                   :arguments "{\"tz\":\"")))
                    (make-stream-sse-payload
                     :content "world"
                     :tool-calls (list
                                  (make-stream-tool-call-delta
                                   :index 0
                                   :arguments "SF\"}")
                                  (make-stream-tool-call-delta
                                   :index 1
                                   :arguments "UTC\"}")))))
                  200))))
    (let* ((client (pseudopod:make-client :api-key "stub"))
           (tools (list
                   (pseudopod:make-tool-definition
                    :name "get-weather"
                    :description "stream test tool"
                    :parameters (make-tool-schema)
                    :fn (lambda (arguments tool-call)
                          (declare (ignore arguments tool-call))
                          "{}"))))
           (content-chunks nil)
           (tool-callbacks nil))
      (multiple-value-bind (content tool-calls)
          (pseudopod:stream-chat-completion
           client
           "Stream a tool call."
           :messages (list (pseudopod:make-message
                            :role "user"
                            :content "Stream tools."))
           :tools tools
           :on-content (lambda (chunk) (push chunk content-chunks))
           :on-tool-call (lambda (tool-call) (push tool-call tool-callbacks)))
        (is (string= "Hello world" (or content "")))
        (is (= 2 (length tool-calls)))
        (is-true (every #'pseudopod:tool-call-p tool-calls))
        (is (= 2 (length tool-callbacks)))
        (is (equal '("world" "Hello ") content-chunks))
        (is (string= "call-1" (or (pseudopod:tool-call-id (first tool-calls)) "")))
        (is (string= "get-weather" (pseudopod:tool-call-name (first tool-calls))))
        (is (string= "{\"city\":\"SF\"}"
                     (or (pseudopod:tool-call-arguments (first tool-calls)) "")))
        (is (string= "call-2" (or (pseudopod:tool-call-id (second tool-calls)) "")))
        (is (string= "get-time" (pseudopod:tool-call-name (second tool-calls))))
        (is (string= "{\"tz\":\"UTC\"}"
                     (or (pseudopod:tool-call-arguments (second tool-calls)) ""))))
      ;; Also test stream-chat-completion*
      (let ((message (pseudopod:stream-chat-completion*
                      client
                      "Stream typed tool call."
                      :messages (list (pseudopod:make-message
                                       :role "user"
                                       :content "Stream typed tools."))
                      :tools tools)))
        (is-true (pseudopod:message-p message))
        (is (string= "assistant" (or (pseudopod:message-role message) "")))
        (is (string= "Hello world" (or (message-first-text message) "")))
        (is (= 2 (length (pseudopod:message-tool-calls message))))))))

;;; ---- I18 Tests: Models and Token Estimation ----

(test models-and-token-estimation
  (with-stub-dex
      (:get (lambda (url &rest args)
              (declare (ignore args))
              (unless (search "/models" url)
                (error "Expected models endpoint, got ~S" url))
              (let ((response (make-hash-table :test #'equal)))
                (setf (gethash "object" response) "list")
                (setf (gethash "data" response)
                      (vector
                       (make-stub-model-hash :id "kimi-k2.5")
                       (make-stub-model-hash :id "kimi-k2.5-preview"
                                             :owned-by "moonshot-preview")))
                (values (jonathan:to-json response) 200)))
       :post (lambda (url &rest args &key content &allow-other-keys)
               (declare (ignore args))
               (unless (search "/tokenizers/estimate-token-count" url)
                 (error "Expected token estimate endpoint, got ~S" url))
               (let* ((request (jonathan:parse content :as :hash-table))
                      (response (make-hash-table :test #'equal))
                      (data (make-hash-table :test #'equal)))
                 (declare (ignore request))
                 (setf (gethash "total_tokens" data) 17)
                 (setf (gethash "data" response) data)
                 (values (jonathan:to-json response) 200))))
    (let* ((client (pseudopod:make-client :api-key "stub"))
           (models (pseudopod:list-models client)))
      (is (= 2 (length models)))
      (is-true (pseudopod:model-info-p (first models)))
      (is (string= "kimi-k2.5" (pseudopod:model-info-id (first models))))
      (multiple-value-bind (token-count response)
          (pseudopod:estimate-tokens client
                                     :messages
                                     (list (pseudopod:make-message
                                            :role "user"
                                            :content "Count these tokens.")))
        (is (= 17 token-count))
        (is-true (hash-table-p response))))))

;;; ---- I19 Tests: Files API ----

(test files-api
  (let ((tmp-file nil))
    (unwind-protect
        (progn
          (setf tmp-file
                (merge-pathnames
                 (format nil "pseudopod-test-~D.txt" (random 1000000))
                 (uiop:temporary-directory)))
          (with-open-file (stream tmp-file
                                  :direction :output
                                  :if-exists :supersede
                                  :if-does-not-exist :create)
            (write-string "file payload" stream))
          (with-stub-dex
              (:post (lambda (url &rest args &key content &allow-other-keys)
                       (declare (ignore args))
                       (unless (search "/files" url)
                         (error "Expected upload endpoint, got ~S" url))
                       (let ((purpose (alist-string-value content "purpose"))
                             (file-part (alist-string-value content "file")))
                         (declare (ignore purpose))
                         (values
                          (jonathan:to-json
                           (make-stub-file-hash
                            :id "file-upload-123"
                            :filename (file-namestring file-part)
                            :bytes 12
                            :purpose "batch"))
                          200)))
               :get (lambda (url &rest args)
                      (declare (ignore args))
                      (cond
                        ((search "/files/file-123/content" url)
                         (values "FILE_CONTENT_OK" 200))
                        ((search "/files/file-123" url)
                         (values (jonathan:to-json
                                  (make-stub-file-hash
                                   :id "file-123"
                                   :filename "existing.txt"
                                   :bytes 99))
                                 200))
                        ((search "/files" url)
                         (let ((response (make-hash-table :test #'equal)))
                           (setf (gethash "object" response) "list")
                           (setf (gethash "data" response)
                                 (vector
                                  (make-stub-file-hash
                                   :id "file-123"
                                   :filename "existing.txt"
                                   :bytes 99)))
                           (values (jonathan:to-json response) 200)))
                        (t
                         (error "Unexpected GET URL ~S" url))))
               :delete (lambda (url &rest args)
                         (declare (ignore args))
                         (unless (search "/files/file-123" url)
                           (error "Unexpected DELETE URL ~S" url))
                         (let ((response (make-hash-table :test #'equal)))
                           (setf (gethash "id" response) "file-123")
                           (setf (gethash "object" response) "file")
                           (setf (gethash "deleted" response) t)
                           (values (jonathan:to-json response) 200))))
            (let* ((client (pseudopod:make-client :api-key "stub"))
                   (upload (pseudopod:upload-file client tmp-file :purpose "batch"))
                   (fetched (pseudopod:get-file client "file-123"))
                   (listed (pseudopod:list-files client))
                   (deleted (pseudopod:delete-file client "file-123"))
                   (content (pseudopod:file-content client "file-123")))
              (is-true (pseudopod:file-object-p upload))
              (is (string= "file-upload-123" (pseudopod:file-object-id upload)))
              (is-true (pseudopod:file-object-p fetched))
              (is (string= "file-123" (pseudopod:file-object-id fetched)))
              (is (= 1 (length listed)))
              (is-true (pseudopod:file-object-p (first listed)))
              (is-true (and (hash-table-p deleted) (eq t (gethash "deleted" deleted))))
              (is (string= "FILE_CONTENT_OK" content)))))
      (when (and tmp-file (uiop:file-exists-p tmp-file))
        (handler-case (cl:delete-file tmp-file)
          (error () nil))))))

;;; ---- I20 Tests: Conversation Context ----

(test conversation-helpers
  (with-stub-dex
      (:post (lambda (url &rest args &key content want-stream &allow-other-keys)
               (declare (ignore url args want-stream))
               (let* ((request (jonathan:parse content :as :hash-table))
                      (messages (sequence->list (gethash "messages" request)))
                      (tools (sequence->list (gethash "tools" request)))
                      (last-message (last-item messages))
                      (tool-message (find-if (lambda (msg)
                                               (string= "tool"
                                                        (or (and (hash-table-p msg)
                                                                 (gethash "role" msg))
                                                            "")))
                                             messages))
                      (assistant (make-hash-table :test #'equal))
                      (choice (make-hash-table :test #'equal))
                      (response (make-hash-table :test #'equal)))
                 (setf (gethash "role" assistant) "assistant")
                 (cond
                   (tool-message
                    (setf (gethash "content" assistant)
                          (format nil "Tool says ~A"
                                  (raw-message-text tool-message))))
                   ((and tools
                         (string= "user"
                                  (or (and (hash-table-p last-message)
                                           (gethash "role" last-message))
                                      ""))
                         (search "What time"
                                 (raw-message-text last-message)))
                    (let ((tool-call (make-hash-table :test #'equal))
                          (function-body (make-hash-table :test #'equal)))
                      (setf (gethash "content" assistant) "")
                      (setf (gethash "id" tool-call) "conv-call-1")
                      (setf (gethash "type" tool-call) "function")
                      (setf (gethash "name" function-body) "get-current-time")
                      (setf (gethash "arguments" function-body) "{}")
                      (setf (gethash "function" tool-call) function-body)
                      (setf (gethash "tool_calls" assistant) (vector tool-call))))
                   (t
                    (setf (gethash "content" assistant)
                          (format nil "Echo: ~A"
                                  (raw-message-text last-message)))))
                 (setf (gethash "message" choice) assistant)
                 (setf (gethash "choices" response) (vector choice))
                 (values (jonathan:to-json response) 200))))
    (let* ((client (pseudopod:make-client :api-key "stub"))
           (toolset (pseudopod:make-toolset))
           (conversation (pseudopod:make-conversation
                          :client client
                          :system-prompt "You are a conversation test helper."
                          :toolset toolset)))
      (pseudopod:register-tool-function
       toolset
       :name "get-current-time"
       :description "Return a deterministic timestamp."
       :parameters (make-tool-schema)
       :fn (lambda (arguments tool-call)
             (declare (ignore arguments tool-call))
             "2026-02-14T00:00:00Z"))
      (pseudopod:conversation-add-user conversation "Hello there.")
      (let* ((first-reply (pseudopod:conversation-complete conversation))
             (history (pseudopod:conversation-history conversation)))
        (is (string= "Echo: Hello there." (or (message-first-text first-reply) "")))
        (is (= 2 (length history))))
      (let* ((step-result (pseudopod:conversation-step
                           conversation
                           :user-prompt "What time is it?"
                           :max-steps 4))
             (final-message (pseudopod:step-result-final-message step-result))
             (final-text (and final-message (message-first-text final-message))))
        (is-true (pseudopod:message-p final-message))
        (is-true (search "2026-02-14T00:00:00Z" (or final-text "")))
        (is (= 1 (length (pseudopod:step-result-tool-results step-result))))
        (let ((last-history (last-item (pseudopod:conversation-history conversation))))
          (is (string= "assistant" (pseudopod:message-role last-history)))))
      (pseudopod:conversation-add-assistant conversation "Manual assistant note.")
      (pseudopod:conversation-add-tool-result
       conversation
       "manual-tool"
       "manual-output"
       :tool-call-id "manual-1")
      (let ((last-history (last-item (pseudopod:conversation-history conversation))))
        (is (string= "tool" (pseudopod:message-role last-history)))
        (is (string= "manual-output" (or (message-first-text last-history) ""))))
      (pseudopod:conversation-clear conversation)
      (is-true (null (pseudopod:conversation-history conversation))))))

;;; ---- Negative Tests ----

(test malformed-json-response-signals-parse-error
  (with-stub-dex
      (:post (lambda (url &rest args &key &allow-other-keys)
               (declare (ignore url args))
               (values "THIS IS NOT JSON{{{" 200)))
    (let ((client (pseudopod:make-client :api-key "stub")))
      (signals pseudopod:pseudopod-parse-error
        (pseudopod:chat-completion client "hello")))))

(test partial-stream-with-malformed-sse-lines
  (with-stub-dex
      (:post (lambda (url &rest args &key want-stream &allow-other-keys)
               (declare (ignore url args))
               (unless want-stream
                 (error "Expected streaming request."))
               (values
                (make-string-input-stream
                 (format nil "data: NOT_JSON~%data: ~A~%data: [DONE]~%"
                         (jonathan:to-json
                          (make-stream-sse-payload :content "ok"))))
                200)))
    (let ((client (pseudopod:make-client :api-key "stub")))
      (multiple-value-bind (content tool-calls)
          (pseudopod:stream-chat-completion
           client "test"
           :messages (list (pseudopod:make-message
                            :role "user" :content "test")))
        (declare (ignore tool-calls))
        (is (string= "ok" (or content "")))))))

(test invalid-json-tool-arguments-signals-parse-error
  (let ((toolset (pseudopod:make-toolset)))
    (pseudopod:register-tool-function
     toolset
     :name "test-tool"
     :description "A test tool"
     :parameters (make-tool-schema)
     :fn (lambda (arguments tool-call)
           (declare (ignore arguments tool-call))
           "ok"))
    (let ((bad-call (pseudopod:make-tool-call
                     :id "call-bad"
                     :name "test-tool"
                     :arguments "NOT VALID JSON{{")))
      (signals pseudopod:pseudopod-parse-error
        (pseudopod:invoke-tool-call toolset bad-call)))))

(test tool-arity-one-accepted
  "Test that single-argument tool functions work correctly."
  (let ((toolset (pseudopod:make-toolset)))
    (pseudopod:register-tool-function
     toolset
     :name "one-arg-tool"
     :description "Takes only arguments"
     :parameters (make-tool-schema)
     :fn (lambda (arguments)
           (declare (ignore arguments))
           "one-arg-result"))
    (let ((call (pseudopod:make-tool-call
                 :id "call-one"
                 :name "one-arg-tool"
                 :arguments "{}")))
      (is (string= "one-arg-result"
                    (pseudopod:invoke-tool-call toolset call))))))

(test api-key-masked-in-print
  "Test that API keys are masked when printing client structs."
  (let* ((client (pseudopod:make-client :api-key "sk-1234567890abcdefghij"))
         (printed (princ-to-string client)))
    (is-false (search "1234567890" printed))
    (is-true (search "sk-" printed))
    (is-true (search "ghij" printed))))

;;; ---- Runner ----

(defun run-all ()
  "Run all pseudopod tests and return T on success."
  (let ((results (run 'pseudopod-suite)))
    (explain! results)
    (results-status results)))
