(load "/home/rahul/Documents/amoebum/ptui/.tools/quicklisp/setup.lisp")
(require :asdf)

(let* ((asdf-pkg (or (find-package "ASDF")
                     (error "Missing package ASDF")))
       (load-asd-sym (or (find-symbol "LOAD-ASD" asdf-pkg)
                         (error "Missing symbol LOAD-ASD in ASDF package")))
       (load-system-sym (or (find-symbol "LOAD-SYSTEM" asdf-pkg)
                            (error "Missing symbol LOAD-SYSTEM in ASDF package")))
       (load-asd-fn (symbol-function load-asd-sym))
       (load-system-fn (symbol-function load-system-sym)))
  (funcall load-asd-fn #P"/home/rahul/Documents/amoebum/pseudopod/pseudopod.asd")
  (funcall load-system-fn "pseudopod"))

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

(defun assert-bad-api-key-signals-auth-error ()
  (let ((original-post (symbol-function 'dex:post)))
    (unwind-protect
        (progn
          (setf (symbol-function 'dex:post)
                (lambda (url &rest args &key want-stream &allow-other-keys)
                  (declare (ignore url args))
                  (if want-stream
                      (values (make-string-input-stream "{\"error\":\"unauthorized\"}") 401)
                      (values "{\"error\":\"unauthorized\"}" 401))))
          (let ((bad-client (pseudopod:make-client :api-key "deliberately-bad-key")))
            (handler-case
                (progn
                  (pseudopod:chat-completion bad-client "Say hi.")
                  (error "Expected pseudopod-auth-error for bad API key."))
              (pseudopod:pseudopod-auth-error ()
                t))))
      (setf (symbol-function 'dex:post) original-post))))

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

(defun assert-message-round-trip ()
  (dolist (role '("system" "user" "assistant" "tool"))
    (let* ((text (format nil "~A message" role))
           (source (pseudopod:make-message :role role :content text))
           (hash (pseudopod:message-to-hash source))
           (result (pseudopod:hash-to-message hash)))
      (unless (string= role (pseudopod:message-role result))
        (error "Role round-trip mismatch role=~S result=~S"
               role
               (pseudopod:message-role result)))
      (unless (string= text (or (message-first-text result) ""))
        (error "Content round-trip mismatch role=~S text=~S result=~S"
               role
               text
               (message-first-text result))))))

(defun assert-chat-completion-star-returns-message ()
  (let ((original-post (symbol-function 'dex:post)))
    (unwind-protect
        (progn
          (setf (symbol-function 'dex:post)
                (lambda (url &rest args &key content want-stream &allow-other-keys)
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
            (unless (pseudopod:message-p message)
              (error "Expected chat-completion* to return a message struct, got ~S"
                     message))
            (unless (string= "assistant" (pseudopod:message-role message))
              (error "Expected assistant role, got ~S"
                     (pseudopod:message-role message)))
            (unless (string= "I15 typed reply" (or (message-first-text message) ""))
              (error "Expected typed content, got ~S"
                     (message-first-text message)))))
      (setf (symbol-function 'dex:post) original-post))))

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

(defun assert-models-and-token-estimation ()
  (let ((original-post (symbol-function 'dex:post))
        (original-get (symbol-function 'dex:get)))
    (unwind-protect
        (progn
          (setf (symbol-function 'dex:get)
                (lambda (url &rest args)
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
                    (values (jonathan:to-json response) 200))))
          (setf (symbol-function 'dex:post)
                (lambda (url &rest args &key content &allow-other-keys)
                  (declare (ignore args))
                  (unless (search "/tokenizers/estimate-token-count" url)
                    (error "Expected token estimate endpoint, got ~S" url))
                  (let* ((request (jonathan:parse content :as :hash-table))
                         (model (and (hash-table-p request)
                                     (gethash "model" request)))
                         (messages (and (hash-table-p request)
                                        (gethash "messages" request)))
                         (response (make-hash-table :test #'equal))
                         (data (make-hash-table :test #'equal)))
                    (unless (string= "kimi-k2.5" (or model ""))
                      (error "Expected model kimi-k2.5, got ~S" model))
                    (unless (and (or (listp messages) (vectorp messages))
                                 (> (length messages) 0))
                      (error "Expected non-empty messages payload, got ~S" request))
                    (setf (gethash "total_tokens" data) 17)
                    (setf (gethash "data" response) data)
                    (values (jonathan:to-json response) 200))))
          (let* ((client (pseudopod:make-client :api-key "stub"))
                 (models (pseudopod:list-models client)))
            (unless (and (= 2 (length models))
                         (pseudopod:model-info-p (first models))
                         (string= "kimi-k2.5"
                                  (pseudopod:model-info-id (first models))))
              (error "List-models result mismatch: ~S" models))
            (multiple-value-bind (token-count response)
                (pseudopod:estimate-tokens client
                                           :messages
                                           (list (pseudopod:make-message
                                                  :role "user"
                                                  :content "Count these tokens.")))
              (unless (= 17 token-count)
                (error "Estimate-tokens count mismatch: ~S" token-count))
              (unless (hash-table-p response)
                (error "Estimate-tokens expected response hash-table, got ~S"
                       response)))))
      (setf (symbol-function 'dex:post) original-post)
      (setf (symbol-function 'dex:get) original-get))))

(defun assert-files-api ()
  (let ((original-post (symbol-function 'dex:post))
        (original-get (symbol-function 'dex:get))
        (original-delete (symbol-function 'dex:delete))
        (tmp-file nil))
    (unwind-protect
        (progn
          (setf tmp-file
                (merge-pathnames
                 (format nil "pseudopod-i19-~D.txt" (random 1000000))
                 (uiop:temporary-directory)))
          (with-open-file (stream tmp-file
                                  :direction :output
                                  :if-exists :supersede
                                  :if-does-not-exist :create)
            (write-string "file payload" stream))
          (setf (symbol-function 'dex:post)
                (lambda (url &rest args &key content &allow-other-keys)
                  (declare (ignore args))
                  (unless (search "/files" url)
                    (error "Expected upload endpoint, got ~S" url))
                  (let ((purpose (alist-string-value content "purpose"))
                        (file-part (alist-string-value content "file")))
                    (unless (string= "batch" (or purpose ""))
                      (error "Expected multipart purpose=batch, got ~S" purpose))
                    (unless (pathnamep file-part)
                      (error "Expected multipart file pathname, got ~S" file-part))
                    (values
                     (jonathan:to-json
                      (make-stub-file-hash
                       :id "file-upload-123"
                       :filename (file-namestring file-part)
                       :bytes 12
                       :purpose purpose))
                     200))))
          (setf (symbol-function 'dex:get)
                (lambda (url &rest args)
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
                     (error "Unexpected GET URL ~S" url)))))
          (setf (symbol-function 'dex:delete)
                (lambda (url &rest args)
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
            (unless (and (pseudopod:file-object-p upload)
                         (string= "file-upload-123" (pseudopod:file-object-id upload))
                         (string= (file-namestring tmp-file)
                                  (pseudopod:file-object-filename upload)))
              (error "Upload result mismatch: ~S" upload))
            (unless (and (pseudopod:file-object-p fetched)
                         (string= "file-123" (pseudopod:file-object-id fetched)))
              (error "Get-file result mismatch: ~S" fetched))
            (unless (and (= 1 (length listed))
                         (pseudopod:file-object-p (first listed))
                         (string= "file-123"
                                  (pseudopod:file-object-id (first listed))))
              (error "List-files result mismatch: ~S" listed))
            (unless (and (hash-table-p deleted)
                         (eq t (gethash "deleted" deleted)))
              (error "Delete-file result mismatch: ~S" deleted))
            (unless (string= "FILE_CONTENT_OK" content)
              (error "File-content mismatch: ~S" content)))))
      (ignore-errors
        (when (and tmp-file (uiop:file-exists-p tmp-file))
          (delete-file tmp-file)))
      (setf (symbol-function 'dex:post) original-post)
      (setf (symbol-function 'dex:get) original-get)
      (setf (symbol-function 'dex:delete) original-delete)))

(defun assert-step-loop-invokes-tool-and-returns-final-message ()
  (let ((original-post (symbol-function 'dex:post)))
    (unwind-protect
        (progn
          (setf (symbol-function 'dex:post)
                (lambda (url &rest args &key content want-stream &allow-other-keys)
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
                    (unless (and (or (listp tools) (vectorp tools))
                                 (> (length tools) 0))
                      (error "Expected tool definitions in payload, got ~S" request))
                    (unless (string= "get-current-time"
                                     (or (gethash "name"
                                                   (gethash "function"
                                                            (first-item tools)))
                                         ""))
                      (error "Expected get-current-time tool in payload, got ~S" tools))
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
              (unless (pseudopod:message-p final-message)
                (error "Expected final message, got ~S (max-steps=~S steps=~S tool-results=~S)"
                       final-message
                       (pseudopod:step-result-max-steps-reached result)
                       (pseudopod:step-result-steps result)
                       tool-results))
              (unless (string= "assistant" (pseudopod:message-role final-message))
                (error "Expected assistant final role, got ~S"
                       (pseudopod:message-role final-message)))
              (unless (and (stringp final-text)
                           (search "2026-02-13T00:00:00Z" final-text))
                (error "Expected final response to include tool output, got ~S"
                       final-text))
              (unless (= 1 captured-call-count)
                (error "Expected exactly one tool invocation, got ~S"
                       captured-call-count))
              (unless (and first-result
                           (string= "2026-02-13T00:00:00Z"
                                    (or (getf first-result :output) "")))
                (error "Expected recorded tool result output, got ~S"
                       tool-results))
              (when (pseudopod:step-result-max-steps-reached result)
                (error "Expected normal completion, got max-steps termination.")))))
      (setf (symbol-function 'dex:post) original-post))))

(defun assert-step-loop-respects-max-steps ()
  (let ((original-post (symbol-function 'dex:post)))
    (unwind-protect
        (progn
          (setf (symbol-function 'dex:post)
                (lambda (url &rest args &key content want-stream &allow-other-keys)
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
              (unless (pseudopod:step-result-max-steps-reached result)
                (error "Expected max-steps termination, got ~S" result))
              (unless (= 2 (pseudopod:step-result-steps result))
                (error "Expected exactly 2 model steps, got ~S"
                       (pseudopod:step-result-steps result)))
              (when (pseudopod:step-result-final-message result)
                (error "Expected nil final message on max-steps termination.")))))
      (setf (symbol-function 'dex:post) original-post))))

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

(defun assert-streaming-tool-call-support ()
  (let ((original-post (symbol-function 'dex:post)))
    (unwind-protect
        (progn
          (setf (symbol-function 'dex:post)
                (lambda (url &rest args &key content want-stream &allow-other-keys)
                  (declare (ignore url args))
                  (let* ((request (jonathan:parse content :as :hash-table))
                         (tools (gethash "tools" request)))
                    (unless want-stream
                      (error "Expected streaming request for SSE test."))
                    (unless (and (or (listp tools) (vectorp tools))
                                 (> (length tools) 0))
                      (error "Expected serialized tools in stream payload, got ~S" request))
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
              (unless (string= "Hello world" (or content ""))
                (error "Expected merged stream content, got ~S" content))
              (unless (and (= 2 (length tool-calls))
                           (pseudopod:tool-call-p (first tool-calls))
                           (pseudopod:tool-call-p (second tool-calls)))
                (error "Expected two reconstructed tool-call structs, got ~S" tool-calls))
              (unless (and (= 2 (length tool-callbacks))
                           (every #'pseudopod:tool-call-p tool-callbacks))
                (error "Expected on-tool-call callback invocations, got ~S"
                       tool-callbacks))
              (unless (equal '("world" "Hello ") content-chunks)
                (error "Expected streamed content chunk callbacks, got ~S"
                       content-chunks))
              (unless (and (string= "call-1"
                                    (or (pseudopod:tool-call-id (first tool-calls)) ""))
                           (string= "get-weather"
                                    (pseudopod:tool-call-name (first tool-calls)))
                           (string= "{\"city\":\"SF\"}"
                                    (or (pseudopod:tool-call-arguments
                                         (first tool-calls))
                                        "")))
                (error "First reconstructed tool-call mismatch: ~S"
                       (first tool-calls)))
              (unless (and (string= "call-2"
                                    (or (pseudopod:tool-call-id (second tool-calls)) ""))
                           (string= "get-time"
                                    (pseudopod:tool-call-name (second tool-calls)))
                           (string= "{\"tz\":\"UTC\"}"
                                    (or (pseudopod:tool-call-arguments
                                         (second tool-calls))
                                        "")))
                (error "Second reconstructed tool-call mismatch: ~S"
                       (second tool-calls))))
            (let* ((message (pseudopod:stream-chat-completion*
                             client
                             "Stream typed tool call."
                             :messages (list (pseudopod:make-message
                                              :role "user"
                                              :content "Stream typed tools."))
                             :tools tools)))
              (unless (and (pseudopod:message-p message)
                           (string= "assistant"
                                    (or (pseudopod:message-role message) "")))
                (error "Expected stream-chat-completion* typed assistant message, got ~S"
                       message))
              (unless (string= "Hello world" (or (message-first-text message) ""))
                (error "Expected merged typed stream content, got ~S"
                       (message-first-text message)))
              (unless (= 2 (length (pseudopod:message-tool-calls message)))
                (error "Expected typed streamed tool calls on message, got ~S"
                       (pseudopod:message-tool-calls message))))))
      (setf (symbol-function 'dex:post) original-post))))

(defun assert-conversation-helpers ()
  (let ((original-post (symbol-function 'dex:post)))
    (unwind-protect
        (progn
          (setf (symbol-function 'dex:post)
                (lambda (url &rest args &key content want-stream &allow-other-keys)
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
              (unless (string= "Echo: Hello there."
                               (or (message-first-text first-reply) ""))
                (error "Expected conversation-complete echo reply, got ~S"
                       (message-first-text first-reply)))
              (unless (= 2 (length history))
                (error "Expected user+assistant history after complete, got ~S"
                       history)))
            (let* ((step-result (pseudopod:conversation-step
                                 conversation
                                 :user-prompt "What time is it?"
                                 :max-steps 4))
                   (final-message (pseudopod:step-result-final-message step-result))
                   (final-text (and final-message (message-first-text final-message))))
              (unless (and (pseudopod:message-p final-message)
                           (search "2026-02-14T00:00:00Z" (or final-text "")))
                (error "Expected conversation-step final message with tool result, got ~S"
                       final-message))
              (unless (= 1 (length (pseudopod:step-result-tool-results step-result)))
                (error "Expected one tool result in step result, got ~S"
                       (pseudopod:step-result-tool-results step-result)))
              (let ((last-history (last-item (pseudopod:conversation-history conversation))))
                (unless (and (pseudopod:message-p last-history)
                             (string= "assistant" (pseudopod:message-role last-history)))
                  (error "Expected conversation history to end with assistant message, got ~S"
                         last-history))))
            (pseudopod:conversation-add-assistant conversation "Manual assistant note.")
            (pseudopod:conversation-add-tool-result
             conversation
             "manual-tool"
             "manual-output"
             :tool-call-id "manual-1")
            (let ((last-history (last-item (pseudopod:conversation-history conversation))))
              (unless (and (pseudopod:message-p last-history)
                           (string= "tool" (pseudopod:message-role last-history))
                           (string= "manual-output"
                                    (or (message-first-text last-history) "")))
                (error "Expected manual tool message append, got ~S" last-history)))
            (pseudopod:conversation-clear conversation)
            (unless (null (pseudopod:conversation-history conversation))
              (error "Expected conversation-clear to reset history."))))
      (setf (symbol-function 'dex:post) original-post))))

(handler-case
    (progn
      (assert-message-round-trip)
      (format t "PSEUDOPOD_I15_ROUNDTRIP_OK~%")
      (assert-chat-completion-star-returns-message)
      (format t "PSEUDOPOD_I15_CHAT_COMPLETION_STAR_OK~%")
      (assert-step-loop-invokes-tool-and-returns-final-message)
      (format t "PSEUDOPOD_I16_TOOL_LOOP_OK~%")
      (assert-step-loop-respects-max-steps)
      (format t "PSEUDOPOD_I16_MAX_STEPS_OK~%")
      (assert-streaming-tool-call-support)
      (format t "PSEUDOPOD_I17_STREAM_TOOL_OK~%")
      (assert-models-and-token-estimation)
      (format t "PSEUDOPOD_I18_LIST_MODELS_OK~%")
      (format t "PSEUDOPOD_I18_ESTIMATE_TOKENS_OK~%")
      (assert-files-api)
      (format t "PSEUDOPOD_I19_FILE_UPLOAD_OK~%")
      (format t "PSEUDOPOD_I19_FILE_CRUD_OK~%")
      (assert-conversation-helpers)
      (format t "PSEUDOPOD_I20_CONVERSATION_OK~%")
      (assert-bad-api-key-signals-auth-error)
      (format t "PSEUDOPOD_I14_AUTH_ERROR_OK~%")
      (format t "PSEUDOPOD_I21_HARDENING_OK~%")
      (asdf:load-system "pseudopod/test")
      (unless (uiop:symbol-call :pseudopod/test :run-all)
        (error "FiveAM test suite failed."))
      (format t "PSEUDOPOD_I22_FIVEAM_OK~%")
      (let* ((client (pseudopod:make-client))
             (response (pseudopod:chat-completion
                        client
                        "Reply with EXACTLY: MOONSHOT_OK"
                        :system-prompt "You are a connectivity test assistant."))
             (choices (and (hash-table-p response) (gethash "choices" response)))
             (choice (first-item choices))
             (message (and (hash-table-p choice) (gethash "message" choice)))
             (content (and (hash-table-p message) (gethash "content" message)))
             (extracted-content (pseudopod:extract-content response)))
        (unless (stringp content)
          (error "Missing assistant content in response: ~S" response))
        (unless (and (stringp extracted-content)
                     (string= extracted-content content))
          (error "extract-content mismatch. extracted=~S content=~S"
                 extracted-content content))
        (format t "PSEUDOPOD_I14_EXTRACT_CONTENT_OK~%")
        (format t "PSEUDOPOD_SMOKE_OK~%")
        (format t "assistant=~A~%" content)))
  (error (e)
    (format *error-output* "PSEUDOPOD_SMOKE_ERROR: ~A~%" e)
    (sb-ext:exit :code 1)))
