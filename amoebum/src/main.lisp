(in-package :amoebum)

(defun %cli-json-function (name)
  (let* ((json-package (find-package :jonathan))
         (symbol (and json-package
                      (find-symbol name json-package))))
    (unless (and symbol (fboundp symbol))
      (error "Jonathan function ~A is unavailable." name))
    (symbol-function symbol)))

(defun %json-encode (value)
  (funcall (%cli-json-function "TO-JSON") value))

(defun %json-object (&rest pairs)
  (let ((table (make-hash-table :test #'equal)))
    (loop for (key value) on pairs by #'cddr do
          (setf (gethash key table) value))
    table))

(defun %trim-cli-arg (value)
  (string-trim '(#\Space #\Tab #\Newline #\Return)
               (or (and value (princ-to-string value)) "")))

(defun %non-empty-cli-arg-p (value)
  (> (length (%trim-cli-arg value)) 0))

(defun %consume-cli-value (args index flag)
  (let* ((next-index (1+ index))
         (value (and (< next-index (length args))
                     (nth next-index args))))
    (unless (%non-empty-cli-arg-p value)
      (error "Missing value for CLI option ~A." flag))
    (values (%trim-cli-arg value) next-index)))

(defun %parse-cli-options (argv)
  (let ((json-mode-p nil)
        (command nil)
        (prompt nil)
        (resume nil)
        (session-id nil)
        (image-paths '()))
    (loop for index from 0 below (length argv) do
      (let ((argument (or (nth index argv) "")))
        (cond
          ((or (string= argument "--json")
               (string= argument "--non-interactive"))
           (setf json-mode-p t))
          ((or (string= argument "--command")
               (string= argument "-c"))
           (multiple-value-bind (value consumed-index)
               (%consume-cli-value argv index "--command")
             (setf command value
                   index consumed-index)))
          ((uiop:string-prefix-p "--command=" argument)
           (setf command (%trim-cli-arg (subseq argument (length "--command=")))))
          ((string= argument "--prompt")
           (multiple-value-bind (value consumed-index)
               (%consume-cli-value argv index "--prompt")
             (setf prompt value
                   index consumed-index)))
          ((uiop:string-prefix-p "--prompt=" argument)
           (setf prompt (%trim-cli-arg (subseq argument (length "--prompt=")))))
          ((string= argument "--resume")
           (let ((next (and (< (1+ index) (length argv))
                            (nth (1+ index) argv))))
             (if (and next
                      (%non-empty-cli-arg-p next)
                      (not (uiop:string-prefix-p "-" next)))
                 (progn
                   (setf resume (%trim-cli-arg next))
                   (incf index))
                 (setf resume "latest"))))
          ((uiop:string-prefix-p "--resume=" argument)
           (setf resume (%trim-cli-arg (subseq argument (length "--resume=")))))
          ((string= argument "--session-id")
           (multiple-value-bind (value consumed-index)
               (%consume-cli-value argv index "--session-id")
             (setf session-id value
                   index consumed-index)))
          ((uiop:string-prefix-p "--session-id=" argument)
           (setf session-id (%trim-cli-arg (subseq argument (length "--session-id=")))))
          ((string= argument "--image")
           (multiple-value-bind (value consumed-index)
               (%consume-cli-value argv index "--image")
             (push value image-paths)
             (setf index consumed-index)))
          ((uiop:string-prefix-p "--image=" argument)
           (push (%trim-cli-arg (subseq argument (length "--image="))) image-paths)))))
    (list :json-mode-p json-mode-p
          :command command
          :prompt prompt
          :resume resume
          :session-id session-id
          :image-paths (nreverse image-paths))))

(defun %resolve-cli-conversation (&key session-id resume)
  (let ((trimmed-session-id (%trim-cli-arg session-id))
        (trimmed-resume (%trim-cli-arg resume)))
    (or (when (> (length trimmed-session-id) 0)
          (conversation-load-session trimmed-session-id))
        (when (> (length trimmed-resume) 0)
          (if (or (string= trimmed-resume "latest")
                  (string= trimmed-resume "1")
                  (string= trimmed-resume "true"))
              (conversation-load-latest)
              (conversation-load-session trimmed-resume)))
        (conversation-load-latest)
        (make-conversation-state))))

(defun %validate-image-path (path)
  (let* ((trimmed (%trim-cli-arg path))
         (resolved (and (> (length trimmed) 0)
                        (ignore-errors (probe-file trimmed)))))
    (unless resolved
      (error "Image path ~S does not exist." path))
    resolved))

(defun %read-binary-file-octets (path)
  (with-open-file (stream path :direction :input :element-type '(unsigned-byte 8))
    (let ((octets (make-array (file-length stream) :element-type '(unsigned-byte 8))))
      (read-sequence octets stream)
      octets)))

(defun %image-content-part (path)
  (let* ((resolved (%validate-image-path path))
         (mime-type (%image-mime-type resolved)))
    (unless mime-type
      (error "Unsupported image type for ~A." (namestring resolved)))
    (let* ((octets (%read-binary-file-octets resolved))
           (base64 (%base64-encode-octets octets))
           (part (make-hash-table :test #'equal)))
      (setf (gethash "type" part) "image"
            (gethash "media_type" part) mime-type
            (gethash "data" part) base64
            (gethash "path" part) (namestring resolved)
            (gethash "text" part) (format nil "[image ~A]" (file-namestring resolved)))
      part)))

(defun %build-user-message-content (prompt image-paths)
  (let ((parts '()))
    (when (%non-empty-cli-arg-p prompt)
      (push (pseudopod:make-text-part (%trim-cli-arg prompt)) parts))
    (dolist (image-path image-paths)
      (push (%image-content-part image-path) parts))
    (nreverse parts)))

(defun %cli-handle-command (command conversation)
  (if (or (null command)
          (not (%non-empty-cli-arg-p command)))
      (values nil "No command provided.")
      (let ((trimmed (%trim-cli-arg command)))
        (if (slash-command-input-p trimmed)
            (multiple-value-bind (handled result)
                (dispatch-slash-command trimmed
                                        :config (current-config)
                                        :chat-state (make-chat-ui-state :conversation conversation))
              (values handled
                      (if (and result (typep result 'slash-command-result))
                          (or (slash-command-result-output result) "")
                          (or (and result (princ-to-string result))
                              ""))))
            (values nil "Only slash commands are supported in --json command mode.")))))

(defun %cli-handle-prompt (prompt image-paths conversation)
  (let ((content (%build-user-message-content prompt image-paths)))
    (if (null content)
        (values nil "Prompt and image attachments are empty.")
        (progn
          (conversation-state-add-message
           conversation
           (pseudopod:make-message :role "user" :content content)
           :save-p t)
          (values t "Prompt accepted into conversation session state.")))))

(defun %emit-cli-json-result (&key ok mode action output error command prompt
                                session-id image-paths)
  (let ((payload
          (%json-object
           "ok" (not (null ok))
           "mode" (or mode "interactive")
           "action" (or action "none")
           "command" command
           "prompt" prompt
           "session_id" session-id
           "images" (coerce (or image-paths '()) 'vector)
           "output" output
           "error" error)))
    (format t "~A~%" (%json-encode payload))
    (finish-output)))

(defun run-cli-json (&rest argv)
  (let* ((options (%parse-cli-options argv))
         (command (getf options :command))
         (prompt (getf options :prompt))
         (resume (getf options :resume))
         (session-id (getf options :session-id))
         (image-paths (getf options :image-paths))
         (conversation (%resolve-cli-conversation
                        :session-id session-id
                        :resume resume)))
    (when (and (%non-empty-cli-arg-p command)
               (%non-empty-cli-arg-p prompt))
      (error "Use either --command or --prompt, not both."))
    (handler-case
        (multiple-value-bind (ok output)
            (if (%non-empty-cli-arg-p command)
                (%cli-handle-command command conversation)
                (%cli-handle-prompt prompt image-paths conversation))
          (let ((active-session (conversation-state-session-id conversation)))
            (conversation-save conversation)
            (%emit-cli-json-result
             :ok ok
             :mode "json"
             :action (if (%non-empty-cli-arg-p command) "command" "prompt")
             :output output
             :command command
             :prompt prompt
             :session-id active-session
             :image-paths image-paths))
          t)
      (error (condition)
        (%emit-cli-json-result
         :ok nil
         :mode "json"
         :action "error"
         :error (princ-to-string condition)
         :command command
         :prompt prompt
         :session-id (and conversation (conversation-state-session-id conversation))
         :image-paths image-paths)
        nil))))

(defun main (&rest argv)
  (activate-amoebum-readtable)
  (let ((effective-argv (or argv
                            #+sbcl (rest sb-ext:*posix-argv*)
                            #-sbcl nil)))
    (reload-config :cli-arguments effective-argv)
    (enable-tts-post-receive-hook)
    (let ((options (%parse-cli-options effective-argv)))
      (if (getf options :json-mode-p)
          (apply #'run-cli-json effective-argv)
          (run-chat-ui :backend :auto :fps 20)))))
