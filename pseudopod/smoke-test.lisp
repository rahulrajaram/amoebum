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
      (assert-bad-api-key-signals-auth-error)
      (format t "PSEUDOPOD_I14_AUTH_ERROR_OK~%")
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
