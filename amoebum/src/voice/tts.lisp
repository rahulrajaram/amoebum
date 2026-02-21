(in-package :amoebum)

(defparameter *tts-default-voice* "af_heart")
(defparameter *tts-default-command* "kokoro-tts")
(defparameter *tts-default-voices*
  '("af_heart" "af_sarah" "am_adam" "bm_george"))

(defparameter *tts-run-command-function* #'pseudopod:run-command
  "Function used to execute TTS subprocess commands.")

(defvar *tts-backend* nil
  "Active TTS backend instance.")

(defclass tts-backend () ())

(defgeneric speak-text (backend text &key)
  (:documentation "Queue TEXT for speech output on BACKEND."))

(defgeneric stop-speaking (backend)
  (:documentation "Stop speech output and clear any queued segments on BACKEND."))

(defgeneric speaking-p (backend)
  (:documentation "Return non-NIL when BACKEND is currently speaking or has queued segments."))

(defgeneric set-voice (backend voice)
  (:documentation "Set active VOICE on BACKEND."))

(defgeneric list-voices (backend)
  (:documentation "Return available voices for BACKEND."))

(defclass kokoro-tts-backend (tts-backend)
  ((command :initarg :command
            :initform *tts-default-command*
            :accessor kokoro-tts-command)
   (mode :initarg :mode
         :initform :cli
         :accessor kokoro-tts-mode)
   (voice :initarg :voice
          :initform *tts-default-voice*
          :accessor kokoro-tts-voice)
   (queue :initform '()
          :accessor kokoro-tts-queue)
   (lock :initform (bordeaux-threads:make-lock "amoebum-kokoro-tts-lock")
         :reader kokoro-tts-lock)
   (worker-thread :initform nil
                  :accessor kokoro-tts-worker-thread)
   (speaking-now-p :initform nil
                   :accessor kokoro-tts-speaking-now-p)
   (stop-requested-p :initform nil
                     :accessor kokoro-tts-stop-requested-p)
   (run-command-function :initarg :run-command-function
                         :initform nil
                         :accessor kokoro-tts-run-command-function)))

(defun make-kokoro-tts-backend (&key
                                  (command *tts-default-command*)
                                  (voice *tts-default-voice*)
                                  (mode :cli)
                                  run-command-function)
  (make-instance 'kokoro-tts-backend
                 :command (or command *tts-default-command*)
                 :voice (or voice *tts-default-voice*)
                 :mode (if (member mode '(:cli :python-module) :test #'eq)
                           mode
                           :cli)
                 :run-command-function run-command-function))

(defun %tts-trim (value)
  (if (stringp value)
      (string-trim '(#\Space #\Tab #\Newline #\Return) value)
      ""))

(defun %tts-non-empty-string-p (value)
  (plusp (length (%tts-trim value))))

(defun %tts-shell-quote (text)
  (format nil "'~A'"
          (with-output-to-string (out)
            (loop for char across (or text "") do
              (if (char= char #\')
                  (write-string "'\"'\"'" out)
                  (write-char char out))))))

(defun %tts-build-kokoro-command (backend text)
  (let ((command (%tts-trim (kokoro-tts-command backend)))
        (voice (%tts-trim (kokoro-tts-voice backend)))
        (payload (%tts-trim text)))
    (cond
      ((eq (kokoro-tts-mode backend) :python-module)
       (format nil "~A -m kokoro --voice ~A --text ~A"
               (if (%tts-non-empty-string-p command) command "python3")
               (%tts-shell-quote (if (%tts-non-empty-string-p voice)
                                     voice
                                     *tts-default-voice*))
               (%tts-shell-quote payload)))
      (t
       (format nil "~A --voice ~A --text ~A"
               (if (%tts-non-empty-string-p command) command *tts-default-command*)
               (%tts-shell-quote (if (%tts-non-empty-string-p voice)
                                     voice
                                     *tts-default-voice*))
               (%tts-shell-quote payload))))))

(defun %tts-normalize-command-result (result)
  (cond
    ((pseudopod:command-result-p result)
     (list :exit-code (or (pseudopod:command-result-exit-code result) 1)
           :stdout (or (pseudopod:command-result-stdout result) "")
           :stderr (or (pseudopod:command-result-stderr result) "")))
    ((listp result)
     (list :exit-code (or (getf result :exit-code) 1)
           :stdout (or (getf result :stdout) "")
           :stderr (or (getf result :stderr) "")))
    (t
     (list :exit-code 1
           :stdout ""
           :stderr (princ-to-string result)))))

(defun %tts-run-command (backend command-string)
  (%tts-normalize-command-result
   (funcall (or (kokoro-tts-run-command-function backend)
                *tts-run-command-function*
                #'pseudopod:run-command)
            command-string)))

(defun %tts-split-segment (text &key (max-length 320))
  (let ((trimmed (%tts-trim text)))
    (if (or (zerop (length trimmed))
            (<= (length trimmed) max-length))
        (if (zerop (length trimmed)) '() (list trimmed))
        (loop with segments = '()
              with start = 0
              with len = (length trimmed)
              while (< start len) do
                (let* ((remaining (- len start))
                       (chunk-len (min max-length remaining))
                       (end (+ start chunk-len)))
                  (when (< end len)
                    (let ((space-pos (position #\Space trimmed
                                               :start start
                                               :end end
                                               :from-end t)))
                      (when (and space-pos (> space-pos start))
                        (setf end space-pos))))
                  (push (%tts-trim (subseq trimmed start end)) segments)
                  (setf start end)
                  (loop while (and (< start len)
                                   (char= (char trimmed start) #\Space))
                        do (incf start)))
              finally (return (remove-if-not #'%tts-non-empty-string-p
                                             (nreverse segments)))))))

(defun %split-text-into-tts-segments (text)
  (let ((segments '()))
    (dolist (line (cl-ppcre:split "\\n+" (or text "")))
      (dolist (segment (%tts-split-segment line))
        (push segment segments)))
    (nreverse segments)))

(defun %kokoro-run-worker (backend)
  (loop
    (let ((segment nil))
      (bordeaux-threads:with-lock-held ((kokoro-tts-lock backend))
        (when (kokoro-tts-stop-requested-p backend)
          (setf (kokoro-tts-queue backend) '()
                (kokoro-tts-stop-requested-p backend) nil
                (kokoro-tts-speaking-now-p backend) nil
                (kokoro-tts-worker-thread backend) nil)
          (return))
        (if (null (kokoro-tts-queue backend))
            (setf (kokoro-tts-speaking-now-p backend) nil
                  (kokoro-tts-worker-thread backend) nil)
            (progn
              (setf segment (pop (kokoro-tts-queue backend))
                    (kokoro-tts-speaking-now-p backend) t))))
      (unless segment
        (return))
      (let* ((command (%tts-build-kokoro-command backend segment))
             (result (%tts-run-command backend command))
             (exit-code (or (getf result :exit-code) 1)))
        (unless (zerop exit-code)
          (ptui.util.log:log-warn
           "Kokoro TTS command failed exit=~S stderr=~S command=~S"
           exit-code
           (getf result :stderr)
           command))))))

(defun %start-kokoro-worker-if-needed (backend)
  (bordeaux-threads:with-lock-held ((kokoro-tts-lock backend))
    (when (and (kokoro-tts-queue backend)
               (or (null (kokoro-tts-worker-thread backend))
                   (not (bordeaux-threads:thread-alive-p
                         (kokoro-tts-worker-thread backend)))))
      (setf (kokoro-tts-worker-thread backend)
            (bordeaux-threads:make-thread
             (lambda ()
               (%kokoro-run-worker backend))
             :name "amoebum-kokoro-tts-worker")))))

(defmethod speak-text ((backend kokoro-tts-backend) text &key)
  (let ((segments (%split-text-into-tts-segments text)))
    (when segments
      (bordeaux-threads:with-lock-held ((kokoro-tts-lock backend))
        (setf (kokoro-tts-stop-requested-p backend) nil
              (kokoro-tts-queue backend)
              (nconc (kokoro-tts-queue backend) (copy-list segments))))
      (%start-kokoro-worker-if-needed backend))
    (length segments)))

(defmethod stop-speaking ((backend kokoro-tts-backend))
  (bordeaux-threads:with-lock-held ((kokoro-tts-lock backend))
    (setf (kokoro-tts-stop-requested-p backend) t
          (kokoro-tts-queue backend) '()))
  t)

(defmethod speaking-p ((backend kokoro-tts-backend))
  (bordeaux-threads:with-lock-held ((kokoro-tts-lock backend))
    (or (kokoro-tts-speaking-now-p backend)
        (not (null (kokoro-tts-queue backend))))))

(defmethod set-voice ((backend kokoro-tts-backend) voice)
  (let ((trimmed (%tts-trim voice)))
    (unless (plusp (length trimmed))
      (error "VOICE must be a non-empty string."))
    (setf (kokoro-tts-voice backend) trimmed)))

(defmethod list-voices ((backend kokoro-tts-backend))
  (let* ((base (%tts-trim (kokoro-tts-command backend)))
         (command (if (eq (kokoro-tts-mode backend) :python-module)
                      (format nil "~A -m kokoro --list-voices"
                              (if (%tts-non-empty-string-p base) base "python3"))
                      (format nil "~A --list-voices"
                              (if (%tts-non-empty-string-p base)
                                  base
                                  *tts-default-command*))))
         (result (%tts-run-command backend command))
         (exit-code (or (getf result :exit-code) 1)))
    (if (zerop exit-code)
        (let ((parsed
                (remove-if-not #'%tts-non-empty-string-p
                               (mapcar #'%tts-trim
                                       (cl-ppcre:split "\\r?\\n"
                                                       (or (getf result :stdout) ""))))))
          (if parsed parsed (copy-list *tts-default-voices*)))
        (copy-list *tts-default-voices*))))

(defun auto-speak-enabled-p (&optional (cfg (ignore-errors (current-config))))
  (and (config-p cfg)
       (eq t (config-value :tts-auto-speak cfg))))

(defun %message-text-for-tts (message)
  (cond
    ((typep message 'pseudopod:message)
     (with-output-to-string (out)
       (dolist (part (pseudopod:message-content message))
         (cond
           ((and (pseudopod:content-part-p part)
                 (stringp (pseudopod:content-part-text part)))
            (write-string (pseudopod:content-part-text part) out))
           ((stringp part)
            (write-string part out))
           (t nil)))))
    ((stringp message)
     message)
    (t
     (princ-to-string (or message "")))))

(defun %assistant-response-text (response)
  (let ((text (%tts-trim (%message-text-for-tts response))))
    (if (plusp (length text))
        text
        nil)))

(defun ensure-tts-backend (&optional (cfg (ignore-errors (current-config))))
  (or *tts-backend*
      (setf *tts-backend*
            (make-kokoro-tts-backend
             :command (and (config-p cfg) (config-value :tts-command cfg))
             :voice (or (and (config-p cfg) (config-value :tts-voice cfg))
                        *tts-default-voice*)
             :mode (if (and (config-p cfg)
                            (eq t (config-value :tts-python-module cfg)))
                       :python-module
                       :cli)))))

(defun speak-with-default-backend (text &key backend)
  (let ((resolved-text (%assistant-response-text text)))
    (if (null resolved-text)
        (values nil "No speakable text.")
        (let* ((target (or backend (ensure-tts-backend)))
               (queued (speak-text target resolved-text)))
          (values (> queued 0) resolved-text)))))

(defun %chat-state-messages-for-tts (chat-state)
  (or (and chat-state
           (fboundp 'chat-ui-state-messages)
           (ignore-errors (chat-ui-state-messages chat-state)))
      (let ((conversation
              (and chat-state
                   (fboundp 'chat-ui-state-conversation)
                   (ignore-errors (chat-ui-state-conversation chat-state)))))
        (and conversation
             (fboundp 'conversation-state-messages)
             (ignore-errors (conversation-state-messages conversation))))))

(defun %find-last-assistant-text (messages)
  (loop for message in (reverse (or messages '()))
        when (and (typep message 'pseudopod:message)
                  (string-equal (or (pseudopod:message-role message) "")
                                "assistant"))
          do (let ((text (%assistant-response-text message)))
               (when text
                 (return text)))
        finally (return nil)))

(defun speak-last-assistant-response (&key chat-state backend)
  (let ((text (%find-last-assistant-text (%chat-state-messages-for-tts chat-state))))
    (if text
        (speak-with-default-backend text :backend backend)
        (values nil nil))))

(defun tts-post-receive-auto-speak-hook (response)
  (when (auto-speak-enabled-p)
    (ignore-errors
      (speak-with-default-backend response)))
  :ok)

(defun enable-tts-post-receive-hook ()
  (when (fboundp 'register-hook)
    (register-hook :post-receive
                   'tts-auto-speak
                   #'tts-post-receive-auto-speak-hook
                   :priority 30))
  t)
