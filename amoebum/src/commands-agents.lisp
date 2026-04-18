(in-package :amoebum)

(defun %format-tool-history-timestamp (timestamp)
  (if (and (integerp timestamp) (plusp timestamp))
      (multiple-value-bind (second minute hour day month year)
          (decode-universal-time timestamp)
        (format nil "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D"
                year month day hour minute second))
      "unknown"))

(defun %tool-history-source-text (value)
  (cond
    ((pathnamep value) (namestring value))
    ((null value) nil)
    (t (princ-to-string value))))

(defun %hash-table-keys-local (table)
  (loop for key being the hash-keys of table
        collect key))

(defun %known-tool-names ()
  (let ((seen (make-hash-table :test #'equal))
        (names '()))
    (labels ((remember (value)
               (let ((text (%slash-trim (princ-to-string value))))
                 (when (plusp (length text))
                   (let ((key (string-downcase text)))
                     (unless (gethash key seen)
                       (setf (gethash key seen) t)
                       (push key names)))))))
      (when (and (boundp '*tool-metadata*)
                 (hash-table-p *tool-metadata*))
        (dolist (name (%hash-table-keys-local *tool-metadata*))
          (remember name)))
      (when (and (boundp '*tool-history*)
                 (hash-table-p *tool-history*))
        (dolist (name (%hash-table-keys-local *tool-history*))
          (remember name))))
    (sort names #'string<)))

(defun %tool-name-completions (fragment)
  (let ((prefix (%slash-trim fragment)))
    (loop for name in (%known-tool-names)
          when (%starts-with-ci-p prefix name)
            collect name)))

(defun %tool-history-handler (_invocation arguments _context)
  (declare (ignore _invocation _context))
  (let* ((tool-name (gethash :NAME arguments))
         (versions (and tool-name (tool-history tool-name))))
    (if (%slash-blank-p tool-name)
        (make-slash-command-result :echo-input-p t :output "Usage: /tool-history <tool-name>")
        (make-slash-command-result
         :echo-input-p t
         :output (if (null versions)
                     (format nil "No history available for tool ~A." tool-name)
                     (with-output-to-string (out)
                       (format out "Tool history for ~A (~D version~:P):~%"
                               (string-downcase tool-name)
                               (length versions))
                       (dolist (entry versions)
                         (let* ((version (getf entry :version))
                                (timestamp (getf entry :timestamp))
                                (source-file (%tool-history-source-text
                                              (getf entry :source-file)))
                                (source-line (getf entry :source-line)))
                           (format out "~D. ~A"
                                   version
                                   (%format-tool-history-timestamp timestamp))
                           (when source-file
                             (format out " source=~A" source-file))
                           (when source-line
                             (format out " line=~A" source-line))
                           (format out "~%")))))))))

(defun %tool-rollback-handler (_invocation arguments _context)
  (declare (ignore _invocation _context))
  (let* ((tool-name (gethash :NAME arguments))
         (version (or (gethash :VERSION arguments) 1)))
    (if (%slash-blank-p tool-name)
        (make-slash-command-result :echo-input-p t :output "Usage: /tool-rollback <tool-name> [version]")
        (handler-case
            (progn
              (rollback-tool tool-name :version version)
              (let ((remaining (length (tool-history tool-name))))
                (make-slash-command-result
                 :echo-input-p t
                 :output (format nil "Rolled back ~A to version ~D. History now has ~D entry~:P."
                                 (string-downcase tool-name)
                                 version
                                 remaining))))
          (error (condition)
            (make-slash-command-result
             :echo-input-p t
             :output (format nil "Tool rollback failed: ~A" condition)))))))

(defun %fork-usage ()
  "/fork <name> [message-index] | /fork <name> --at <message-index>")

(defun %fork-branch-point-text (value)
  (if (integerp value)
      (format nil "~D" value)
      "-"))

(defun %fork-parse-arguments (raw-arguments)
  (let* ((tokens (%tokenize-command-arguments (or raw-arguments "")))
         (name nil)
         (message-index nil)
         (errors '()))
    (cond
      ((null tokens)
       (push "Missing required fork name." errors))
      ((= (length tokens) 1)
       (setf name (first tokens)))
      ((and (= (length tokens) 3)
            (string-equal (second tokens) "--at"))
       (setf name (first tokens))
       (handler-case
           (setf message-index (parse-integer (third tokens)))
         (error ()
           (push (format nil "Fork message-index must be an integer, got ~S."
                         (third tokens))
                 errors))))
      ((= (length tokens) 2)
       (setf name (first tokens))
       (handler-case
           (setf message-index (parse-integer (second tokens)))
         (error ()
           (push (format nil "Fork message-index must be an integer, got ~S."
                         (second tokens))
                 errors))))
      (t
       (push (format nil "Unexpected arguments ~{~S~^ ~}." tokens) errors)))
    (values name message-index (nreverse errors))))

(defun %fork-chat-event-bus (chat-state)
  (or (and (typep chat-state 'chat-ui-state)
           (typep (chat-ui-state-status-bar-state chat-state) 'status-bar-state)
           (status-bar-state-event-bus (chat-ui-state-status-bar-state chat-state)))
      (current-event-bus)))

(defun %fork-handler (_invocation arguments context)
  (declare (ignore _invocation))
  (let* ((raw-arguments (or (gethash :ARGS arguments) ""))
         (chat-state (slash-command-context-chat-state context))
         (conversation (and (typep chat-state 'chat-ui-state)
                            (chat-ui-state-conversation chat-state))))
    (unless (typep conversation 'conversation-state)
      (return-from %fork-handler
        (make-slash-command-result
         :echo-input-p t
         :output "Conversation history is unavailable for this session.")))
    (multiple-value-bind (name message-index errors)
        (%fork-parse-arguments raw-arguments)
      (if errors
          (make-slash-command-result
           :echo-input-p t
           :output (format nil "~{~A~%~}Usage: ~A" errors (%fork-usage)))
          (handler-case
              (let* ((forked (fork-conversation conversation
                                                name
                                                :message-index message-index
                                                :save-p t
                                                :event-bus (%fork-chat-event-bus chat-state)))
                     (entry-count (length (conversation-state-entries forked)))
                     (branch-point (conversation-state-fork-branch-point forked)))
                (make-slash-command-result
                 :echo-input-p t
                 :output (format nil "Created fork ~A at message ~A (~D message~:P)."
                                 (conversation-active-fork-name forked)
                                 (%fork-branch-point-text branch-point)
                                 entry-count)))
            (error (condition)
              (make-slash-command-result
               :echo-input-p t
               :output (format nil "Failed to create fork: ~A" condition))))))))

(defun %forks-handler (_invocation _arguments context)
  (declare (ignore _invocation _arguments))
  (let* ((chat-state (slash-command-context-chat-state context))
         (conversation (and (typep chat-state 'chat-ui-state)
                            (chat-ui-state-conversation chat-state))))
    (unless (typep conversation 'conversation-state)
      (return-from %forks-handler
        (make-slash-command-result
         :echo-input-p t
         :output "Conversation history is unavailable for this session.")))
    (let* ((active (conversation-active-fork-name conversation))
           (forks (sort (copy-list (conversation-list-forks conversation))
                        #'string<
                        :key (lambda (record)
                               (string-downcase (or (getf record :name) ""))))))
      (make-slash-command-result
       :echo-input-p t
       :output (if (null forks)
                   "No conversation forks available."
                   (with-output-to-string (out)
                     (format out "Conversation forks (~D):~%" (length forks))
                     (dolist (record forks)
                       (let* ((name (or (getf record :name) "unknown"))
                              (branch-point (getf record :branch-point))
                              (message-count (or (getf record :message-count) 0))
                              (active-marker (if (string-equal name active) "*" " ")))
                         (format out "~A ~A (branch-point: ~A, messages: ~D)~%"
                                 active-marker
                                 name
                                 (%fork-branch-point-text branch-point)
                                 message-count)))))))))

(defun %switch-fork-handler (_invocation arguments context)
  (declare (ignore _invocation))
  (let* ((target (gethash :NAME arguments))
         (chat-state (slash-command-context-chat-state context))
         (conversation (and (typep chat-state 'chat-ui-state)
                            (chat-ui-state-conversation chat-state))))
    (unless (typep conversation 'conversation-state)
      (return-from %switch-fork-handler
        (make-slash-command-result
         :echo-input-p t
         :output "Conversation history is unavailable for this session.")))
    (when (%slash-blank-p target)
      (return-from %switch-fork-handler
        (make-slash-command-result :echo-input-p t :output "Usage: /switch-fork <name>")))
    (handler-case
        (let* ((switched (conversation-switch-fork conversation target :save-p t))
               (messages (conversation-state-messages switched)))
          (when (typep chat-state 'chat-ui-state)
            (setf (chat-ui-state-conversation chat-state) switched
                  (chat-ui-state-messages chat-state) messages
                  (chat-ui-state-message-scrollback-lines chat-state) 0
                  (chat-ui-state-max-message-scrollback-lines chat-state) 0))
          (make-slash-command-result
           :echo-input-p t
           :output (format nil "Switched to fork ~A (~D message~:P)."
                           (conversation-active-fork-name switched)
                           (length messages))))
      (error (condition)
        (make-slash-command-result
         :echo-input-p t
         :output (format nil "Failed to switch fork: ~A" condition))))))

(defun %switch-fork-arg-completer (_command _invocation index fragment _prefix)
  (declare (ignore _command _invocation _prefix))
  (if (= index 0)
      (let ((prefix (%slash-trim fragment)))
        (loop for option in '("main")
              when (%starts-with-ci-p prefix option)
                collect option))
      '()))

(defun %tool-history-arg-completer (_command _invocation index fragment _prefix-tokens)
  (declare (ignore _command _invocation _prefix-tokens))
  (if (= index 0) (%tool-name-completions fragment) '()))

(defun %tool-rollback-arg-completer (_command _invocation index fragment _prefix-tokens)
  (declare (ignore _command _invocation _prefix-tokens))
  (cond
    ((= index 0)
     (%tool-name-completions fragment))
    ((= index 1)
     (let ((prefix (%slash-trim fragment)))
       (loop for option in '("1" "2" "3" "4" "5")
             when (%starts-with-ci-p prefix option)
               collect option)))
    (t
     '())))

(declaim (ftype function register-agent-runtime-slash-commands
                         register-swarm-runtime-slash-commands))

(defun register-agent-slash-commands ()
  (register-slash-command
   (make-slash-command
    :name "tool-history"
    :description "Show hot-reload history versions for a tool."
    :usage "/tool-history <tool-name>"
    :parameters
    (list (make-slash-command-parameter :name "name" :type :string :required-p t :description "Tool name to inspect."))
    :handler #'%tool-history-handler
    :completer #'%tool-history-arg-completer))
  (register-slash-command
   (make-slash-command
    :name "tool-rollback"
    :description "Restore a previous hot-reload version of a tool."
    :usage "/tool-rollback <tool-name> [version]"
    :parameters
    (list (make-slash-command-parameter :name "name" :type :string :required-p t :description "Tool name to roll back.")
          (make-slash-command-parameter :name "version" :type :integer :required-p nil :default 1 :description "History version index."))
    :handler #'%tool-rollback-handler
    :completer #'%tool-rollback-arg-completer))
  (register-slash-command
   (make-slash-command
    :name "fork"
    :description "Create a named conversation fork at the current or specified message index."
    :usage (%fork-usage)
    :parameters
    (list (make-slash-command-parameter :name "args" :type :string :required-p nil :greedy-p t :description "Fork name and optional message index."))
    :handler #'%fork-handler))
  (register-slash-command
   (make-slash-command :name "forks" :description "List conversation forks with branch point and message count." :usage "/forks" :handler #'%forks-handler))
  (register-slash-command
   (make-slash-command
    :name "switch-fork"
    :description "Switch active conversation fork."
    :usage "/switch-fork <name>"
    :parameters
    (list (make-slash-command-parameter :name "name" :type :string :required-p t :description "Fork name to activate."))
    :handler #'%switch-fork-handler
    :completer #'%switch-fork-arg-completer))
  (register-agent-runtime-slash-commands)
  (register-swarm-runtime-slash-commands)
  (register-agent-handoff-slash-commands)
  (register-worktree-handoff-slash-command)
  t)
