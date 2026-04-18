(in-package :amoebum)

(defparameter +chat-stream-default-system-prompt+
  +system-prompt-base-layer+)

;; Bound by chat.lisp while a stream is active to observe each incoming chunk.
(defvar *stream-chunk-hook-callback* nil)

(defstruct (stream-pseudopod-chat-context
            (:constructor %make-stream-pseudopod-chat-context
                (&key stream-state prompt messages system-prompt resolved-client tools
                      model base-url stream-request-id stream-mode stream-start-ms)))
  stream-state
  prompt
  messages
  system-prompt
  resolved-client
  tools
  model
  base-url
  stream-request-id
  (stream-mode :stream)
  (stream-chunk-count 0 :type fixnum)
  (stream-char-count 0 :type fixnum)
  (stream-start-ms 0 :type integer)
  (stream-status :ok))

(defun %make-stream-pseudopod-chat-runtime (stream-state prompt messages system-prompt client tools)
  (let ((resolved-client (or client (pseudopod:make-client))))
    (%make-stream-pseudopod-chat-context
     :stream-state stream-state
     :prompt prompt
     :messages messages
     :system-prompt system-prompt
     :resolved-client resolved-client
     :tools tools
     :model (pseudopod:client-model resolved-client)
     :base-url (pseudopod:client-base-url resolved-client)
     :stream-request-id (format nil "stream-~D" (%usdt-now-ms))
     :stream-mode :stream
     :stream-start-ms (%usdt-now-ms))))

(defun %stream-pseudopod-chat-track-chunk! (context chunk chunk-kind)
  (when (functionp *stream-chunk-hook-callback*)
    (funcall *stream-chunk-hook-callback* chunk))
  (when (and (stringp chunk) (plusp (length chunk)))
    (incf (stream-pseudopod-chat-context-stream-chunk-count context))
    (incf (stream-pseudopod-chat-context-stream-char-count context) (length chunk))
    (usdt-probe-llm-stream-chunk
     (stream-pseudopod-chat-context-model context)
     (stream-pseudopod-chat-context-base-url context)
     (stream-pseudopod-chat-context-stream-mode context)
     (stream-pseudopod-chat-context-stream-request-id context)
     (stream-pseudopod-chat-context-stream-chunk-count context)
     chunk
     :chunk-kind chunk-kind
     :total-chunks (stream-pseudopod-chat-context-stream-chunk-count context)
     :total-chars (stream-pseudopod-chat-context-stream-char-count context)))
  (when (or (eq chunk-kind :content) (eq chunk-kind :fallback-content))
    (token-stream-emit-chunk (stream-pseudopod-chat-context-stream-state context) chunk)))

(defun %stream-pseudopod-chat-handle-tool-call (context tool-call)
  (token-stream-emit-tool-call-started
   (stream-pseudopod-chat-context-stream-state context)
   tool-call)
  (token-stream-emit-tool-call-argument-complete
   (stream-pseudopod-chat-context-stream-state context)
   tool-call))

(defun %stream-pseudopod-chat-emit-fallback-tool-calls (context message)
  (let ((tool-calls (and (pseudopod:message-p message)
                         (pseudopod:message-tool-calls message))))
    (dolist (tool-call tool-calls)
      (%stream-pseudopod-chat-handle-tool-call context tool-call))))

(defun %stream-pseudopod-chat-emit-fallback-error (context condition)
  (%stream-pseudopod-chat-track-chunk!
   context
   (format nil "\n[streaming error: ~A. Falling back to non-stream mode]\n"
           condition)
   :fallback-error))

(defun %stream-pseudopod-chat-run-primary-stream (context)
  (pseudopod:stream-chat-completion*
   (stream-pseudopod-chat-context-resolved-client context)
   (stream-pseudopod-chat-context-prompt context)
   :system-prompt (stream-pseudopod-chat-context-system-prompt context)
   :messages (stream-pseudopod-chat-context-messages context)
   :tools (stream-pseudopod-chat-context-tools context)
   :on-content (lambda (chunk)
                 (%stream-pseudopod-chat-track-chunk! context chunk :content))
   :on-reasoning (lambda (chunk)
                   (%stream-pseudopod-chat-track-chunk! context chunk :reasoning))
   :on-tool-call-delta
   (lambda (chunk)
     (token-stream-emit-tool-call-delta
      (stream-pseudopod-chat-context-stream-state context)
      chunk))
   :on-tool-call-started
   (lambda (tool-call)
     (token-stream-emit-tool-call-started
      (stream-pseudopod-chat-context-stream-state context)
      tool-call))
   :on-tool-call
   (lambda (tool-call)
     (%stream-pseudopod-chat-handle-tool-call context tool-call))
   :on-tool-call-argument-complete
   (lambda (tool-call)
     (token-stream-emit-tool-call-argument-complete
      (stream-pseudopod-chat-context-stream-state context)
      tool-call))))

(defun %stream-pseudopod-chat-prepare-fallback! (context)
  (setf (stream-pseudopod-chat-context-stream-request-id context)
        (format nil "fallback-~D" (%usdt-now-ms))
        (stream-pseudopod-chat-context-stream-mode context) :fallback
        (stream-pseudopod-chat-context-stream-chunk-count context) 0
        (stream-pseudopod-chat-context-stream-char-count context) 0)
  context)

(defun %stream-pseudopod-chat-fallback-content (message)
  (cond
    ((not (pseudopod:message-p message)) "")
    ((stringp (pseudopod:message-content message))
     (pseudopod:message-content message))
    (t
     (%message-content->text message))))

(defun %stream-pseudopod-chat-run-fallback (context)
  (let* ((message (pseudopod:chat-completion*
                   (stream-pseudopod-chat-context-resolved-client context)
                   (stream-pseudopod-chat-context-prompt context)
                   :system-prompt (stream-pseudopod-chat-context-system-prompt context)
                   :messages (stream-pseudopod-chat-context-messages context)
                   :tools (stream-pseudopod-chat-context-tools context)))
         (content (%stream-pseudopod-chat-fallback-content message)))
    (unless (%token-stream-blank-string-p content)
      (%stream-pseudopod-chat-track-chunk! context content :fallback-content))
    (%stream-pseudopod-chat-emit-fallback-tool-calls context message)))

(defun %stream-pseudopod-chat-handle-primary-error (context stream-state condition)
  (setf (stream-pseudopod-chat-context-stream-status context) :error)
  (token-stream-check-cancel stream-state)
  (%stream-pseudopod-chat-prepare-fallback! context)
  (%stream-pseudopod-chat-emit-fallback-error context condition)
  (%stream-pseudopod-chat-with-probe
   context
   :fallback
   (stream-pseudopod-chat-context-stream-request-id context)
   (lambda ()
     (%stream-pseudopod-chat-run-fallback context))))

(defun %stream-pseudopod-chat-run-provider-runtime (context stream-state)
  (%stream-pseudopod-chat-with-probe
   context
   :stream
   (stream-pseudopod-chat-context-stream-request-id context)
   (lambda ()
     (handler-case
         (%stream-pseudopod-chat-run-primary-stream context)
       (token-stream-cancelled ()
         (setf (stream-pseudopod-chat-context-stream-status context) :cancelled)
         (error 'token-stream-cancelled))
       (error (condition)
         (%stream-pseudopod-chat-handle-primary-error context stream-state condition))))))

(defun %stream-pseudopod-chat-with-probe (context mode request-id thunk)
  (let ((status :ok)
        (start-ms (%usdt-now-ms)))
    (usdt-probe-llm-request-start
     (stream-pseudopod-chat-context-model context)
     (stream-pseudopod-chat-context-base-url context)
     mode
     request-id)
    (unwind-protect
         (handler-case
             (funcall thunk)
           (error (condition)
             (setf status :error)
             (error condition)))
      (usdt-probe-llm-request-end
       (stream-pseudopod-chat-context-model context)
       (stream-pseudopod-chat-context-base-url context)
       mode
       request-id
       (max 0 (- (%usdt-now-ms) start-ms))
       :status status))))

(defun stream-pseudopod-chat (stream-state prompt messages
                              &key
                                (system-prompt +chat-stream-default-system-prompt+)
                                client
                                tools)
  (let ((context (%make-stream-pseudopod-chat-runtime
                  stream-state prompt messages system-prompt client tools)))
    (%stream-pseudopod-chat-run-provider-runtime context stream-state)))
