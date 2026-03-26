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

(defun %agent-status-text (status)
  (if (symbolp status)
      (string-downcase (symbol-name status))
      (princ-to-string status)))

(defun %runtime-agent-backend-label (agent)
  (let ((backend (runtime-agent-backend agent)))
    (if backend
        (%spawn-delegation-backend-label backend)
        "unknown")))

(defun %list-runtime-agents (&key (include-completed-p t))
  (append (list-agents :include-completed-p include-completed-p)
          (if include-completed-p
              (list-swarm-agents)
              (loop for agent in (list-swarm-agents)
                    unless (runtime-agent-terminal-p agent :backend :swarm)
                      collect agent))))

(defun %agent-task-summary (agent)
  (let ((task (%slash-trim (runtime-agent-task agent))))
    (if (plusp (length task))
        task
        "(no task description)")))

(defun %agents-handler (_invocation _arguments _context)
  (declare (ignore _invocation _arguments _context))
  (let ((running (%list-runtime-agents :include-completed-p nil)))
    (if (null running)
        (make-slash-command-result :output "No running agents.")
        (make-slash-command-result
         :output (with-output-to-string (out)
                   (format out "Running agents (~D):~%" (length running))
                   (dolist (agent running)
                     (format out "- ~A | ~A | ~A~@[ | ~A~]~%"
                             (runtime-agent-id agent)
                             (%agent-status-text (runtime-agent-status agent))
                             (%runtime-agent-backend-label agent)
                             (%agent-task-summary agent))))))))

(defun %agent-output-body (agent output)
  (let* ((trimmed-output (%slash-trim output))
         (result (runtime-agent-result agent))
         (result-text (and result
                           (not (null result))
                           (%slash-trim (princ-to-string result))))
         (error-message (runtime-agent-error-message agent))
         (error-text (and (stringp error-message)
                          (%slash-trim error-message))))
    (cond
      ((plusp (length trimmed-output))
       (with-output-to-string (out)
         (write-string trimmed-output out)
         (when (and result-text
                    (plusp (length result-text))
                    (not (string-equal trimmed-output result-text)))
           (format out "~%Result: ~A" result-text))
         (when (and error-text
                    (plusp (length error-text)))
           (format out "~%Error: ~A" error-message))))
      ((and result-text
            (plusp (length result-text)))
       result-text)
      ((and error-text
            (plusp (length error-text)))
       (format nil "Error: ~A" error-message))
      (t "No output captured yet."))))

(defun %agent-handler (_invocation arguments _context)
  (declare (ignore _invocation _context))
  (let* ((agent-id (gethash :ID arguments))
         (action (or (gethash :ACTION arguments) :output))
         (agent (and agent-id (find-runtime-agent agent-id)))
         (backend (and agent (runtime-agent-backend agent))))
    (unless agent
      (return-from %agent-handler
        (make-slash-command-result :output (format nil "Unknown agent id ~S." agent-id))))
    (case action
      (:cancel
       (let ((cancelled-p
               (case backend
                 (:swarm (progn
                           (kill-swarm-agent agent-id)
                           t))
                 (otherwise (cancel-agent agent-id)))))
         (if cancelled-p
             (make-slash-command-result
              :output (format nil "Cancel requested for ~A agent ~A."
                              (%runtime-agent-backend-label agent)
                              agent-id))
             (make-slash-command-result
              :output (format nil "Failed to cancel agent ~A." agent-id)))))
      (:output
       (let* ((output (runtime-agent-output agent :backend backend))
              (updated (or (find-runtime-agent agent-id :backend backend) agent)))
         (make-slash-command-result
          :output (format nil "Agent ~A (~A, ~A backend) output:~%~A"
                          agent-id
                          (%agent-status-text (runtime-agent-status updated :backend backend))
                          (%runtime-agent-backend-label updated)
                          (%agent-output-body updated output)))))
      (otherwise
       (make-slash-command-result :output (format nil "Unsupported /agent action ~S." action))))))

(defun %agent-activity-usage ()
  "/agent-activity [agent-id] [--type inference|tool-call|waiting|idle] [--limit N]")

(defun %agent-activity-type-token (token)
  (let ((candidate (and token
                        (intern (string-upcase (%slash-trim token)) :keyword))))
    (when (member candidate +agent-activity-types+ :test #'eq)
      candidate)))

(defun %parse-agent-activity-args (raw-args)
  (let ((tokens (%tokenize-command-arguments (or raw-args "")))
        (agent-id nil)
        (activity-type nil)
        (limit 20)
        (errors '()))
    (loop while tokens do
      (let ((token (pop tokens)))
        (cond
          ((string= token "--type")
           (if tokens
               (let ((parsed (%agent-activity-type-token (pop tokens))))
                 (if parsed
                     (setf activity-type parsed)
                     (push "Invalid --type value. Expected inference|tool-call|waiting|idle." errors)))
               (push "Missing value for --type." errors)))
          ((string= token "--limit")
           (if tokens
               (let ((limit-token (pop tokens)))
                 (handler-case
                     (let ((parsed (parse-integer limit-token)))
                       (if (> parsed 0)
                           (setf limit parsed)
                           (push "--limit must be a positive integer." errors)))
                   (error ()
                     (push "--limit must be a positive integer." errors))))
               (push "Missing value for --limit." errors)))
          ((and (plusp (length token)) (char= (char token 0) #\-))
           (push (format nil "Unknown option ~S." token) errors))
          ((null agent-id)
           (setf agent-id token))
          (t
           (push (format nil "Unexpected argument ~S." token) errors)))))
    (values agent-id activity-type limit (nreverse errors))))

(defun %render-agent-activity-output (entries &key agent-id activity-type)
  (let ((agent-label (if (%slash-blank-p (or agent-id "")) "all" agent-id))
        (type-label (if activity-type
                        (string-downcase (symbol-name activity-type))
                        "all")))
    (if (null entries)
        (format nil "Agent activity [agent=~A type=~A]: no matching entries."
                agent-label type-label)
        (with-output-to-string (out)
          (format out "Agent activity [agent=~A type=~A] (~D entries):~%"
                  agent-label type-label (length entries))
          (dolist (entry entries)
            (format out "- #~D | ~A | ~A~@[ | ~A~]~%"
                    (agent-activity-entry-sequence entry)
                    (agent-activity-entry-agent-id entry)
                    (string-downcase
                     (symbol-name (agent-activity-entry-activity-type entry)))
                    (agent-activity-entry-description entry)))))))

(defun %agent-activity-handler (_invocation arguments _context)
  (declare (ignore _invocation _context))
  (multiple-value-bind (agent-id activity-type limit errors)
      (%parse-agent-activity-args (or (gethash :ARGS arguments) ""))
    (if errors
        (make-slash-command-result
         :output (format nil "~{~A~%~}Usage: ~A" errors (%agent-activity-usage)))
        (let ((entries (list-agent-activity
                        :agent-id agent-id
                        :activity-type activity-type
                        :limit limit)))
          (make-slash-command-result
           :output (%render-agent-activity-output entries
                                                  :agent-id agent-id
                                                  :activity-type activity-type))))))

(defun %agent-id-completions (fragment)
  (let ((prefix (%slash-trim fragment)))
    (loop for agent in (list-agents)
          for id = (agent-record-id agent)
          when (%starts-with-ci-p prefix id)
            collect id)))

(defun %agent-arg-completer (_command _invocation index fragment prefix-tokens)
  (declare (ignore _command _invocation))
  (cond
    ((= index 0)
     (%agent-id-completions fragment))
    ((= index 1)
     (if (and prefix-tokens (plusp (length (%slash-trim (first prefix-tokens)))))
         (let ((prefix (%slash-trim fragment)))
           (loop for option in '("cancel" "output")
                 when (%starts-with-ci-p prefix option)
                   collect option))
         '()))
    (t
     '())))

(defun %agent-activity-id-completions (fragment)
  (let ((prefix (%slash-trim fragment))
        (seen (make-hash-table :test #'equal))
        (ids '()))
    (dolist (entry (list-agent-activity :limit 200))
      (let ((id (agent-activity-entry-agent-id entry)))
        (when (and (stringp id)
                   (plusp (length id))
                   (%starts-with-ci-p prefix id)
                   (not (gethash id seen)))
          (setf (gethash id seen) t)
          (push id ids))))
    (sort ids #'string<)))

(defun %agent-activity-type-completions (fragment)
  (let ((prefix (%slash-trim fragment)))
    (loop for option in '("inference" "tool-call" "waiting" "idle")
          when (%starts-with-ci-p prefix option)
            collect option)))

(defun %agent-activity-arg-completer (_command _invocation index fragment prefix-tokens)
  (declare (ignore _command _invocation))
  (let* ((previous (and (> index 0) (nth (1- index) prefix-tokens)))
         (prefix (%slash-trim fragment)))
    (cond
      ((and previous (string= previous "--type"))
       (%agent-activity-type-completions fragment))
      ((and previous (string= previous "--limit"))
       (loop for option in '("10" "20" "50" "100")
             when (%starts-with-ci-p prefix option)
               collect option))
      ((and (plusp (length prefix)) (char= (char prefix 0) #\-))
       (loop for option in '("--type" "--limit")
             when (%starts-with-ci-p prefix option)
               collect option))
      ((= index 0)
       (append (%agent-activity-id-completions fragment)
               (loop for option in '("--type" "--limit")
                     when (%starts-with-ci-p prefix option)
                       collect option)))
      (t
       (loop for option in '("--type" "--limit")
             when (%starts-with-ci-p prefix option)
               collect option)))))

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

(defun %spawn-handler (_invocation arguments _context)
  (declare (ignore _invocation))
  (let ((task-text (or (gethash :TASK arguments) "")))
    (if (zerop (length (%slash-trim task-text)))
        (make-slash-command-result :output "Usage: /spawn <task-description>")
        (handler-case
            (multiple-value-bind (record backend)
                (%spawn-task-via-configured-backend
                 task-text
                 :config (and _context
                              (slash-command-context-config _context)))
              (let ((agent-id (%spawned-delegation-record-id record backend)))
              (make-slash-command-result
               :echo-input-p t
               :output (format nil "Spawned agent ~A via ~A backend for task: ~A"
                               agent-id
                               (%spawn-delegation-backend-label backend)
                               task-text))))
          (error (condition)
            (make-slash-command-result
             :echo-input-p t
             :output (format nil "Failed to spawn agent: ~A" condition)))))))

(defun register-agent-slash-commands ()
  (register-slash-command
   (make-slash-command :name "agents" :description "List currently running background agents." :usage "/agents" :handler #'%agents-handler))
  (register-slash-command
   (make-slash-command
    :name "agent"
    :description "Inspect or control a background agent."
    :usage "/agent <id> <cancel|output>"
    :parameters
    (list (make-slash-command-parameter :name "id" :type :string :required-p t :description "Agent identifier.")
          (make-slash-command-parameter :name "action" :type :keyword :required-p t :choices '(:cancel :output) :description "Agent action."))
    :handler #'%agent-handler
    :completer #'%agent-arg-completer))
  (register-slash-command
   (make-slash-command
    :name "agent-activity"
    :description "Show recent real-time agent activity with optional agent/type filters."
    :usage (%agent-activity-usage)
    :parameters
    (list (make-slash-command-parameter :name "args" :type :string :required-p nil :greedy-p t :description "Optional: [agent-id] [--type ...] [--limit N]."))
    :handler #'%agent-activity-handler
    :completer #'%agent-activity-arg-completer))
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
  (register-slash-command
   (make-slash-command
    :name "spawn"
    :description "Spawn a sw4rm sub-agent for a task."
    :usage "/spawn <task-description>"
    :parameters
    (list (make-slash-command-parameter :name "task" :type :string :required-p t :greedy-p t :description "Task description for the spawned agent."))
    :handler #'%spawn-handler))
  t)
