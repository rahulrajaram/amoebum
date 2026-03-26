(in-package :amoebum)

(defun %format-checkpoint-timestamp (timestamp)
  (if (and (integerp timestamp) (plusp timestamp))
      (multiple-value-bind (second minute hour day month year)
          (decode-universal-time timestamp)
        (format nil "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D"
                year month day hour minute second))
      "unknown"))

(defun %apply-chat-conversation! (chat-state next-conversation)
  (when (and (typep chat-state 'chat-ui-state)
             (typep next-conversation 'conversation-state))
    (setf (chat-ui-state-conversation chat-state) next-conversation
          (chat-ui-state-messages chat-state)
          (conversation-state-messages next-conversation)
          (chat-ui-state-message-scrollback-lines chat-state) 0
          (chat-ui-state-max-message-scrollback-lines chat-state) 0)
    (%sync-chat-context-usage! chat-state :allow-auto-compress-p nil))
  next-conversation)

(defun %checkpoint-usage ()
  "/checkpoint [save|list|restore <id>]")

(defun %render-checkpoint-list (&optional (checkpoints (list-session-checkpoints :limit 25)))
  (if (null checkpoints)
      "No checkpoints available."
      (with-output-to-string (out)
        (format out "Checkpoints (~D):~%" (length checkpoints))
        (loop for checkpoint in checkpoints
              for index from 1 do
                (format out "~2D. ~A ~A~@[ [~A]~]~%"
                        index
                        (session-checkpoint-id checkpoint)
                        (%format-checkpoint-timestamp
                         (session-checkpoint-created-at checkpoint))
                        (and (session-checkpoint-auto-p checkpoint)
                             (string-downcase
                              (symbol-name (session-checkpoint-trigger checkpoint)))))))))

(defun %checkpoint-context-conversation (context)
  (let* ((chat-state (slash-command-context-chat-state context))
         (conversation (and (typep chat-state 'chat-ui-state)
                            (chat-ui-state-conversation chat-state))))
    (and (typep conversation 'conversation-state)
         conversation)))

(defun %checkpoint-invalid-usage (&optional details)
  (make-slash-command-result
   :echo-input-p t
   :output (format nil "~@[~A~%~]Usage: ~A"
                   details
                   (%checkpoint-usage))))

(defun %checkpoint-handle-save (tokens conversation cfg)
  (if (> (length tokens) 1)
      (%checkpoint-invalid-usage (format nil "Unexpected argument ~S." (second tokens)))
      (handler-case
          (let ((checkpoint (checkpoint-session :conversation conversation
                                                :config cfg
                                                :trigger :manual
                                                :auto-p nil)))
            (make-slash-command-result
             :echo-input-p t
             :output (format nil "Saved checkpoint ~A."
                             (session-checkpoint-id checkpoint))))
        (error (condition)
          (make-slash-command-result
           :echo-input-p t
           :output (format nil "Checkpoint save failed: ~A" condition))))))

(defun %checkpoint-handle-list (tokens)
  (if (> (length tokens) 1)
      (%checkpoint-invalid-usage (format nil "Unexpected argument ~S." (second tokens)))
      (make-slash-command-result
       :echo-input-p t
       :output (%render-checkpoint-list))))

(defun %checkpoint-handle-restore (tokens chat-state cfg)
  (let ((target (%extensions-join (rest tokens))))
    (if (%slash-blank-p target)
        (%checkpoint-invalid-usage "Specify a checkpoint id, index, or path to restore.")
        (handler-case
            (let* ((restored (restore-session :checkpoint-id target :config cfg))
                   (checkpoint (getf restored :checkpoint))
                   (restored-conversation (getf restored :conversation)))
              (%apply-chat-conversation! chat-state restored-conversation)
              (make-slash-command-result
               :echo-input-p t
               :output (format nil
                               "Restored checkpoint ~A (~D message~:P)."
                               (session-checkpoint-id checkpoint)
                               (length (conversation-state-entries restored-conversation)))))
          (error (condition)
            (make-slash-command-result
             :echo-input-p t
             :output (format nil "Checkpoint restore failed: ~A" condition)))))))

(defun %checkpoint-normalize-action (tokens)
  (let ((token (if tokens
                   (string-downcase (first tokens))
                   "save")))
    (or (loop for (action . aliases) in '(("save" "save" "now")
                                          ("list" "list" "ls")
                                          ("restore" "restore"))
              thereis (and (member token aliases :test #'string=)
                           action))
        token)))

(defun %checkpoint-dispatch-handler (action)
  (cdr (assoc action
              `(("save" . ,(lambda (tokens context)
                             (%checkpoint-handle-save tokens
                                                      (%checkpoint-context-conversation context)
                                                      (or (slash-command-context-config context)
                                                          (%current-config-safe)))))
                ("list" . ,(lambda (tokens _context)
                             (declare (ignore _context))
                             (%checkpoint-handle-list tokens)))
                ("restore" . ,(lambda (tokens context)
                                (%checkpoint-handle-restore
                                 tokens
                                 (slash-command-context-chat-state context)
                                 (or (slash-command-context-config context)
                                     (%current-config-safe))))))
              :test #'string=)))

(defun %checkpoint-handler (_invocation arguments context)
  (declare (ignore _invocation))
  (let* ((raw (or (gethash :ARGS arguments) ""))
         (tokens (%tokenize-command-arguments raw))
         (action-token (%checkpoint-normalize-action tokens))
         (handler (%checkpoint-dispatch-handler action-token)))
    (if handler
        (funcall handler tokens context)
        (%checkpoint-invalid-usage (format nil "Unknown /checkpoint action ~S." action-token)))))

(defun %checkpoint-id-completions (fragment)
  (let ((prefix (%slash-trim fragment)))
    (loop for checkpoint in (list-session-checkpoints :limit 25)
          for id = (session-checkpoint-id checkpoint)
          when (%starts-with-ci-p prefix id)
            collect id)))

(defun %checkpoint-arg-completer (_command _invocation index fragment prefix-tokens)
  (declare (ignore _command _invocation))
  (let ((head (and prefix-tokens (string-downcase (first prefix-tokens))))
        (prefix (%slash-trim fragment)))
    (cond
      ((= index 0)
       (loop for option in '("save" "list" "restore")
             when (%starts-with-ci-p prefix option)
               collect option))
      ((and (string= head "restore") (= index 1))
       (%checkpoint-id-completions fragment))
      (t
       nil))))

(defun %session-usage ()
  "/session [current|list|resume <id|latest>|new [id]]")

(defun %session-context-project-root (context)
  (let ((cfg (or (slash-command-context-config context)
                 (%current-config-safe))))
    (and (config-p cfg)
         (config-project-root cfg))))

(defun %session-context-conversation (context)
  (let* ((chat-state (slash-command-context-chat-state context))
         (conversation (and (typep chat-state 'chat-ui-state)
                            (chat-ui-state-conversation chat-state))))
    (and (typep conversation 'conversation-state)
         conversation)))

(defun %session-resume-latest-token-p (value)
  (let ((trimmed (%slash-trim value)))
    (or (string-equal trimmed "latest")
        (string-equal trimmed "1")
        (string-equal trimmed "true"))))

(defun %session-resolve-resume (target &key project-root)
  (let ((trimmed (%slash-trim (or target ""))))
    (if (%session-resume-latest-token-p trimmed)
        (conversation-load-latest :project-root project-root)
        (conversation-load-session trimmed :project-root project-root))))

(defun %render-session-list (&optional sessions)
  (let ((records (or sessions
                     (conversation-list-sessions :limit 25))))
    (if (null records)
        "No saved conversations available."
        (with-output-to-string (out)
          (format out "Saved conversations (~D):~%" (length records))
          (loop for record in records
                for index from 1 do
                  (format out "~2D. ~A ~A state=~(~A~) messages=~D~%"
                          index
                          (or (getf record :session-id) "unknown")
                          (%format-checkpoint-timestamp (getf record :updated-at))
                          (or (getf record :state) :idle)
                          (or (getf record :message-count) 0)))))))

(defun %session-current-output (conversation)
  (if (not (typep conversation 'conversation-state))
      "No active conversation session."
      (format nil "Current session ~A (state=~(~A~), fork=~A, messages=~D)."
              (conversation-state-session-id conversation)
              (conversation-state-state conversation)
              (conversation-active-fork-name conversation)
              (length (conversation-state-entries conversation)))))

(defun %session-invalid-usage (&optional details)
  (make-slash-command-result
   :echo-input-p t
   :output (format nil "~@[~A~%~]Usage: ~A"
                   details
                   (%session-usage))))

(defun %session-handle-current (tokens conversation)
  (if (> (length tokens) 1)
      (%session-invalid-usage (format nil "Unexpected argument ~S." (second tokens)))
      (make-slash-command-result
       :echo-input-p t
       :output (%session-current-output conversation))))

(defun %session-handle-list (tokens project-root)
  (if (> (length tokens) 1)
      (%session-invalid-usage (format nil "Unexpected argument ~S." (second tokens)))
      (make-slash-command-result
       :echo-input-p t
       :output (%render-session-list
                (conversation-list-sessions
                 :project-root project-root
                 :limit 25)))))

(defun %session-handle-resume (tokens project-root chat-state)
  (let ((target (%extensions-join (rest tokens))))
    (if (%slash-blank-p target)
        (%session-invalid-usage "Specify a session id or 'latest'.")
        (let ((restored (%session-resolve-resume target :project-root project-root)))
          (if (null restored)
              (make-slash-command-result
               :echo-input-p t
               :output (format nil "Session ~S not found." target))
              (let* ((active (%apply-chat-conversation! chat-state restored))
                     (session-id (conversation-state-session-id active)))
                (make-slash-command-result
                 :echo-input-p t
                 :output (format nil
                                 "Resumed session ~A (state=~(~A~), fork=~A, messages=~D)."
                                 session-id
                                 (conversation-state-state active)
                                 (conversation-active-fork-name active)
                                 (length (conversation-state-entries active))))))))))

(defun %session-handle-new (tokens project-root chat-state)
  (if (> (length tokens) 2)
      (%session-invalid-usage (format nil "Unexpected argument ~S." (third tokens)))
      (let* ((requested-id (%slash-trim (or (second tokens) "")))
             (fresh (if (%slash-blank-p requested-id)
                        (make-conversation-state :project-root project-root)
                        (make-conversation-state :project-root project-root
                                                 :session-id requested-id)))
             (active (%apply-chat-conversation! chat-state fresh)))
        (conversation-save active)
        (make-slash-command-result
         :echo-input-p t
         :output (format nil "Started session ~A."
                         (conversation-state-session-id active))))))

(defun %session-normalize-action (tokens)
  (let ((token (if tokens
                   (string-downcase (first tokens))
                   "current")))
    (or (loop for (action . aliases) in '(("current" "current" "show")
                                          ("list" "list" "ls")
                                          ("resume" "resume")
                                          ("new" "new"))
              thereis (and (member token aliases :test #'string=)
                           action))
        token)))

(defun %session-dispatch-handler (action)
  (cdr (assoc action
              `(("current" . ,(lambda (tokens context)
                                (%session-handle-current
                                 tokens
                                 (%session-context-conversation context))))
                ("list" . ,(lambda (tokens context)
                             (%session-handle-list
                              tokens
                              (%session-context-project-root context))))
                ("resume" . ,(lambda (tokens context)
                               (%session-handle-resume
                                tokens
                                (%session-context-project-root context)
                                (slash-command-context-chat-state context))))
                ("new" . ,(lambda (tokens context)
                            (%session-handle-new
                             tokens
                             (%session-context-project-root context)
                             (slash-command-context-chat-state context)))))
              :test #'string=)))

(defun %session-handler (_invocation arguments context)
  (declare (ignore _invocation))
  (let* ((raw (or (gethash :ARGS arguments) ""))
         (tokens (%tokenize-command-arguments raw))
         (action-token (%session-normalize-action tokens))
         (handler (%session-dispatch-handler action-token)))
    (if handler
        (funcall handler tokens context)
        (%session-invalid-usage (format nil "Unknown /session action ~S." action-token)))))

(defun %session-id-completions (fragment)
  (let ((prefix (%slash-trim fragment)))
    (loop for record in (conversation-list-sessions :limit 25)
          for id = (getf record :session-id)
          when (and (stringp id)
                    (%starts-with-ci-p prefix id))
            collect id)))

(defun %session-arg-completer (_command _invocation index fragment prefix-tokens)
  (declare (ignore _command _invocation))
  (let ((head (and prefix-tokens (string-downcase (first prefix-tokens))))
        (prefix (%slash-trim fragment)))
    (cond
      ((= index 0)
       (loop for option in '("current" "list" "resume" "new")
             when (%starts-with-ci-p prefix option)
               collect option))
      ((and (string= head "resume") (= index 1))
       (let ((ids (%session-id-completions fragment)))
         (if (%starts-with-ci-p prefix "latest")
             (cons "latest" ids)
             ids)))
      (t
       nil))))

(defun register-session-slash-commands ()
  (register-slash-command
   (make-slash-command
    :name "checkpoint"
    :description "Save a session checkpoint, list checkpoints, or restore one."
    :usage (%checkpoint-usage)
    :parameters
    (list (make-slash-command-parameter
           :name "args"
           :type :string
           :required-p nil
           :greedy-p t
           :description "Optional action: save, list, restore <id>."))
    :handler #'%checkpoint-handler
    :completer #'%checkpoint-arg-completer))
  (register-slash-command
   (make-slash-command
    :name "session"
    :description "Inspect, list, resume, or start persisted conversation sessions."
    :usage (%session-usage)
    :parameters
    (list (make-slash-command-parameter
           :name "args"
           :type :string
           :required-p nil
           :greedy-p t
           :description "Optional action: current, list, resume <id|latest>, new [id]."))
    :handler #'%session-handler
    :completer #'%session-arg-completer))
  t)
