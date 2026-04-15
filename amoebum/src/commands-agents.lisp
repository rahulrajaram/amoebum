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

(defun %runtime-agent-worktree-summary (agent)
  (let ((metadata (runtime-agent-worktree agent)))
    (when metadata
      (let* ((worktree-id (worktree-metadata-id metadata))
             (branch (worktree-metadata-branch metadata))
             (inspection (and (worktree-metadata-path metadata)
                              (ignore-errors
                                (inspect-local-worktree :worktree metadata))))
             (status-fragment
               (and inspection
                    (getf inspection :abandoned-p)
                    (format nil "~A/~A"
                            (string-downcase
                             (symbol-name
                              (or (getf inspection :lifecycle-state)
                                  :abandoned)))
                            (string-downcase
                             (symbol-name
                              (or (getf inspection :cleanup-classification)
                                  :unknown)))))))
        (format nil "wt ~A~@[ (~A)~]~@[ ~A~]"
                (or worktree-id "?")
                branch
                status-fragment)))))

(defun %agents-handler (_invocation _arguments _context)
  (declare (ignore _invocation _arguments _context))
  (let ((running (%list-runtime-agents :include-completed-p nil)))
    (if (null running)
        (make-slash-command-result :output "No running agents.")
        (make-slash-command-result
         :output (with-output-to-string (out)
                   (format out "Running agents (~D):~%" (length running))
                   (dolist (agent running)
                     (format out "- ~A | ~A | ~A~@[ | ~A~]~@[ | ~A~]~%"
                             (runtime-agent-id agent)
                             (%agent-status-text (runtime-agent-status agent))
                             (%runtime-agent-backend-label agent)
                             (%agent-task-summary agent)
                             (%runtime-agent-worktree-summary agent))))))))

;;; ---------------------------------------------------------------------------
;;; NXT-014: /agent-tree — parent-child execution tree
;;;
;;; Local agents carry a parent-message-id that identifies the conversation
;;; message which triggered their spawn.  We group agents by that key and
;;; display the tree as:
;;;
;;;   [root agents — no parent-message-id]
;;;     agent-task-0001 | running | local | <task>
;;;   [message msg-42]
;;;     agent-task-0002 | completed | local | <task>
;;;     agent-task-0003 | running   | local | <task>
;;;   [sw4rm agents]
;;;     swarm-1 | running | sw4rm | <task>
;;;
;;; ---------------------------------------------------------------------------

(defun %agent-tree-format-node (out agent indent-prefix)
  "Write one agent row to OUT with INDENT-PREFIX."
  (format out "~A~A | ~A | ~A | ~A~%"
          indent-prefix
          (runtime-agent-id agent)
          (%agent-status-text (runtime-agent-status agent))
          (%runtime-agent-backend-label agent)
          (%agent-task-summary agent)))

(defun %agent-tree-handler (_invocation _arguments _context)
  "NXT-014: Show the parent-child execution tree for all known agents.
Local agents are grouped by their parent-message-id; SW4RM agents are shown
separately.  The tree reflects prompt->agent and agent->sub-agent handoffs."
  (declare (ignore _invocation _arguments _context))
  (let* ((all-local (list-agents :include-completed-p t))
         (all-swarm (list-swarm-agents))
         ;; Partition local agents: those with no parent first, then group
         ;; the rest by parent-message-id.
         (roots '())
         (by-parent (make-hash-table :test #'equal)))
    ;; Build groupings
    (dolist (agent all-local)
      (let ((pmid (agent-record-parent-message-id agent)))
        (if (or (null pmid) (%agent-blank-string-p (princ-to-string pmid)))
            (push agent roots)
            (push agent (gethash pmid by-parent '())))))
    (setf roots (nreverse roots))
    (if (and (null roots)
             (zerop (hash-table-count by-parent))
             (null all-swarm))
        (make-slash-command-result :output "No agents recorded.")
        (make-slash-command-result
         :output (with-output-to-string (out)
                   ;; Root-level agents (no parent message)
                   (when roots
                     (format out "[root agents]~%")
                     (dolist (agent roots)
                       (%agent-tree-format-node out agent "  ")))
                   ;; Agents grouped by parent-message-id
                   (let ((parent-ids '()))
                     (maphash (lambda (k _v)
                                (declare (ignore _v))
                                (push k parent-ids))
                              by-parent)
                     (setf parent-ids (sort parent-ids #'string<))
                     (dolist (pmid parent-ids)
                       (let ((children (nreverse (gethash pmid by-parent '()))))
                         (format out "[message ~A]~%" pmid)
                         (dolist (child children)
                           (%agent-tree-format-node out child "  ")))))
                   ;; SW4RM agents
                   (when all-swarm
                     (format out "[sw4rm agents]~%")
                     (dolist (agent all-swarm)
                       (%agent-tree-format-node out agent "  "))))))))

;;; ---------------------------------------------------------------------------
;;; NXT-015: /swarm-status — surface SW4RM state in the chat overlay
;;;
;;; Shows the active swarm delegation mode, number of active/total SW4RM
;;; agents, the handoff sequence counter (proxy for total handoffs issued),
;;; and the number of open user negotiation rooms.
;;; ---------------------------------------------------------------------------

(defun %swarm-status-handler (_invocation _arguments _context)
  "NXT-015: Show SW4RM state: delegation mode, agent counts, handoff count, room count."
  (declare (ignore _invocation _arguments _context))
  (let* ((mode (%configured-swarm-delegation-mode))
         (all-swarm (list-swarm-agents))
         (active-swarm (remove-if #'runtime-agent-terminal-p all-swarm))
         ;; *user-handoff-sequence* is the monotonic counter for issued handoffs
         (handoff-count (if (boundp '*user-handoff-sequence*)
                            *user-handoff-sequence*
                            0))
         ;; *user-negotiation-room-participants* maps room-id -> participants
         (room-count (if (and (boundp '*user-negotiation-room-participants*)
                              (hash-table-p *user-negotiation-room-participants*))
                         (hash-table-count *user-negotiation-room-participants*)
                         0))
         (local-active (active-agent-count)))
    (make-slash-command-result
     :output (with-output-to-string (out)
               (format out "SW4RM status:~%")
               (format out "  delegation-mode : ~A~%"
                       (string-downcase (symbol-name mode)))
               (format out "  local agents    : ~D active~%" local-active)
               (format out "  swarm agents    : ~D active / ~D total~%"
                       (length active-swarm) (length all-swarm))
               (format out "  handoffs issued : ~D~%" handoff-count)
               (format out "  review rooms    : ~D open~%" room-count)))))

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

;;; ---- NXT-003: /swarm-peers ----

(defun %format-peer-entry (peer)
  (let ((session-id (or (getf peer :session-id) "?"))
        (user-id (or (getf peer :user-id) "?"))
        (agent-id (or (getf peer :agent-id) "?"))
        (capabilities (getf peer :capabilities)))
    (format nil "  ~A (user: ~A, agent: ~A)~@[ caps: ~{~A~^, ~}~]"
            session-id user-id agent-id capabilities)))

(defun %swarm-peers-handler (_invocation _arguments _context)
  (declare (ignore _invocation _arguments _context))
  (let ((peers (list-user-session-peers)))
    (make-slash-command-result
     :echo-input-p t
     :output (if (null peers)
                 "No registered swarm peers."
                 (with-output-to-string (out)
                   (format out "Swarm peers (~D):~%" (length peers))
                   (dolist (peer peers)
                     (format out "~A~%" (%format-peer-entry peer))))))))

;;; ---- NXT-004: /handoffs ----

(defun %format-handoff-entry (handoff)
  (let ((handoff-id (or (getf handoff :handoff-id)
                        (getf handoff :request-id) "?"))
        (status (or (getf handoff :status) :pending))
        (from (or (getf handoff :from-session-id)
                  (getf handoff :from-agent) "?"))
        (to (or (getf handoff :to-session-id)
                (getf handoff :to-agent) "?"))
        (reason (or (getf handoff :reason) "")))
    (format nil "  ~A | ~A | from: ~A → to: ~A~@[ | ~A~]"
            handoff-id status from to
            (when (plusp (length reason)) reason))))

(defun %handoffs-handler (_invocation _arguments context)
  (declare (ignore _invocation _arguments))
  (let* ((chat-state (slash-command-context-chat-state context))
         (conversation (and (typep chat-state 'chat-ui-state)
                            (chat-ui-state-conversation chat-state)))
         (session-id (and (typep conversation 'conversation-state)
                          (conversation-state-session-id conversation))))
    (unless session-id
      (return-from %handoffs-handler
        (make-slash-command-result
         :echo-input-p t
         :output "No active session — cannot query handoffs.")))
    (handler-case
        (let ((pending (get-user-pending-handoffs session-id)))
          (make-slash-command-result
           :echo-input-p t
           :output (if (null pending)
                       "No pending handoffs for this session."
                       (with-output-to-string (out)
                         (format out "Pending handoffs (~D):~%" (length pending))
                         (dolist (entry pending)
                           (format out "~A~%" (%format-handoff-entry entry)))))))
      (error (condition)
        (make-slash-command-result
         :echo-input-p t
         :output (format nil "Failed to query handoffs: ~A" condition))))))

;;; ---- NXT-005: /handoff-accept, /handoff-reject, /handoff-complete ----

(defun %handoff-accept-handler (_invocation arguments _context)
  (declare (ignore _invocation _context))
  (let ((handoff-id (gethash :ID arguments)))
    (when (%slash-blank-p handoff-id)
      (return-from %handoff-accept-handler
        (make-slash-command-result :echo-input-p t :output "Usage: /handoff-accept <handoff-id>")))
    (handler-case
        (let ((result (accept-user-handoff handoff-id)))
          (make-slash-command-result
           :echo-input-p t
           :output (format nil "Accepted handoff ~A. Status: ~A"
                           handoff-id (or (getf result :status) :accepted))))
      (error (condition)
        (make-slash-command-result
         :echo-input-p t
         :output (format nil "Failed to accept handoff ~A: ~A" handoff-id condition))))))

(defun %handoff-reject-handler (_invocation arguments _context)
  (declare (ignore _invocation _context))
  (let ((args (or (gethash :ARGS arguments) "")))
    (let* ((tokens (%tokenize-command-arguments args))
           (handoff-id (first tokens))
           (reason (or (format nil "~{~A~^ ~}" (rest tokens)) "rejected")))
      (when (%slash-blank-p handoff-id)
        (return-from %handoff-reject-handler
          (make-slash-command-result :echo-input-p t :output "Usage: /handoff-reject <handoff-id> [reason...]")))
      (handler-case
          (let ((result (reject-user-handoff handoff-id reason)))
            (make-slash-command-result
             :echo-input-p t
             :output (format nil "Rejected handoff ~A. Status: ~A"
                             handoff-id (or (getf result :status) :rejected))))
        (error (condition)
          (make-slash-command-result
           :echo-input-p t
           :output (format nil "Failed to reject handoff ~A: ~A" handoff-id condition)))))))

(defun %handoff-complete-handler (_invocation arguments _context)
  (declare (ignore _invocation _context))
  (let ((handoff-id (gethash :ID arguments)))
    (when (%slash-blank-p handoff-id)
      (return-from %handoff-complete-handler
        (make-slash-command-result :echo-input-p t :output "Usage: /handoff-complete <handoff-id>")))
    (handler-case
        (let ((result (complete-user-handoff handoff-id)))
          (make-slash-command-result
           :echo-input-p t
           :output (format nil "Completed handoff ~A. Status: ~A"
                           handoff-id (or (getf result :status) :completed))))
      (error (condition)
        (make-slash-command-result
         :echo-input-p t
         :output (format nil "Failed to complete handoff ~A: ~A" handoff-id condition))))))

;;; ---- NXT-344: /worktree-handoff ----

(defun %worktree-handoff-usage ()
  "Usage: /worktree-handoff <list|inspect|accept|defer|help> [args...]
  list
  inspect <handoff-id>
  accept <handoff-id> [note...]
  defer <handoff-id> [note...]")

(defun %worktree-handoff-status-text (status)
  (string-downcase (symbol-name (or status :pending))))

(defun %worktree-handoff-summary-line (snapshot)
  (let* ((handoff-id (or (getf snapshot :handoff-id) "?"))
         (status (%worktree-handoff-status-text (getf snapshot :status)))
         (worktree (getf snapshot :worktree))
         (worktree-id (or (getf worktree :id) "?"))
         (branch (getf worktree :branch))
         (target-ref (or (getf snapshot :target-ref) "?"))
         (room-id (getf snapshot :negotiation-room-id))
         (conflicts (getf (getf snapshot :preflight) :conflicts)))
    (format nil
            "  ~A | ~A | wt ~A~@[ (~A)~] -> ~A~@[ | conflicts ~D~]~@[ | room ~A~]"
            handoff-id
            status
            worktree-id
            branch
            target-ref
            (and conflicts (length conflicts))
            room-id)))

(defun %worktree-handoff-inspect-output (snapshot)
  (let* ((worktree (getf snapshot :worktree))
         (preflight (getf snapshot :preflight))
         (conflicts (getf preflight :conflicts))
         (negotiation-status (getf snapshot :negotiation-status)))
    (with-output-to-string (out)
      (format out "Worktree handoff ~A~%" (or (getf snapshot :handoff-id) "?"))
      (format out "status: ~A~%"
              (%worktree-handoff-status-text (getf snapshot :status)))
      (format out "worktree: ~A~@[ (~A)~]~%"
              (or (getf worktree :id) "?")
              (getf worktree :branch))
      (format out "target-ref: ~A~%" (or (getf snapshot :target-ref) "?"))
      (format out "agent: ~A~@[ via ~A~]~%"
              (or (getf snapshot :agent-id) "?")
              (getf snapshot :backend))
      (format out "room: ~A~@[ | artifact: ~A~]~%"
              (or (getf snapshot :negotiation-room-id) "?")
              (getf snapshot :artifact-id))
      (when negotiation-status
        (format out "negotiation-status: ~S~%" negotiation-status))
      (when conflicts
        (format out "conflicts: ~{~A~^, ~}~%" conflicts))
      (when (getf snapshot :note)
        (format out "note: ~A~%" (getf snapshot :note)))
      (when (getf snapshot :task)
        (format out "task: ~A~%" (getf snapshot :task))))))

(defun %worktree-handoff-note (tokens)
  (let ((note (%slash-trim (format nil "~{~A~^ ~}" tokens))))
    (unless (zerop (length note))
      note)))

(defun %worktree-handoff-handler (_invocation arguments _context)
  (declare (ignore _invocation _context))
  (let* ((args (or (gethash :ARGS arguments) ""))
         (tokens (%tokenize-command-arguments args))
         (subcommand (or (first tokens) "list")))
    (cond
      ((or (string-equal subcommand "help")
           (string-equal subcommand "--help"))
       (make-slash-command-result
        :echo-input-p t
        :output (%worktree-handoff-usage)))
      ((string-equal subcommand "list")
       (let ((handoffs (list-worktree-conflict-handoffs)))
         (make-slash-command-result
          :echo-input-p t
          :output (if (null handoffs)
                      "No worktree conflict handoffs."
                      (with-output-to-string (out)
                        (format out "Worktree conflict handoffs (~D):~%"
                                (length handoffs))
                        (dolist (snapshot handoffs)
                          (format out "~A~%"
                                  (%worktree-handoff-summary-line snapshot))))))))
      ((string-equal subcommand "inspect")
       (let ((handoff-id (second tokens)))
         (if (%slash-blank-p handoff-id)
             (make-slash-command-result
              :echo-input-p t
              :output "Usage: /worktree-handoff inspect <handoff-id>")
             (let ((snapshot (find-worktree-conflict-handoff
                              handoff-id
                              :include-room-status-p t)))
               (make-slash-command-result
                :echo-input-p t
                :output (if snapshot
                            (%worktree-handoff-inspect-output snapshot)
                            (format nil "Unknown worktree handoff ~A." handoff-id)))))))
      ((string-equal subcommand "accept")
       (let ((handoff-id (second tokens)))
         (if (%slash-blank-p handoff-id)
             (make-slash-command-result
              :echo-input-p t
              :output "Usage: /worktree-handoff accept <handoff-id> [note...]")
             (handler-case
                 (let ((snapshot (accept-worktree-conflict-handoff
                                  handoff-id
                                  :note (%worktree-handoff-note (cddr tokens)))))
                   (make-slash-command-result
                    :echo-input-p t
                    :output (format nil
                                    "Accepted worktree handoff ~A. Status: ~A"
                                    handoff-id
                                    (%worktree-handoff-status-text
                                     (getf snapshot :status)))))
               (error (condition)
                 (make-slash-command-result
                  :echo-input-p t
                  :output (format nil
                                  "Failed to accept worktree handoff ~A: ~A"
                                  handoff-id
                                  condition)))))))
      ((string-equal subcommand "defer")
       (let ((handoff-id (second tokens)))
         (if (%slash-blank-p handoff-id)
             (make-slash-command-result
              :echo-input-p t
              :output "Usage: /worktree-handoff defer <handoff-id> [note...]")
             (handler-case
                 (let ((snapshot (defer-worktree-conflict-handoff
                                  handoff-id
                                  :note (%worktree-handoff-note (cddr tokens)))))
                   (make-slash-command-result
                    :echo-input-p t
                    :output (format nil
                                    "Deferred worktree handoff ~A. Status: ~A"
                                    handoff-id
                                    (%worktree-handoff-status-text
                                     (getf snapshot :status)))))
               (error (condition)
                 (make-slash-command-result
                  :echo-input-p t
                  :output (format nil
                                  "Failed to defer worktree handoff ~A: ~A"
                                  handoff-id
                                  condition)))))))
      (t
       (make-slash-command-result
        :echo-input-p t
        :output (format nil
                        "Unknown worktree-handoff subcommand ~S. Try /worktree-handoff help."
                        subcommand))))))

;;; ---- NXT-006: /review-room ----

(defun %review-room-handler (_invocation arguments _context)
  (declare (ignore _invocation _context))
  (let* ((args (or (gethash :ARGS arguments) ""))
         (tokens (%tokenize-command-arguments args))
         (subcommand (first tokens)))
    (cond
      ((or (null subcommand) (string-equal subcommand "help"))
       (make-slash-command-result
        :echo-input-p t
        :output "Usage: /review-room <create|submit|critique|status|wait> [args...]
  create <room-id> <session-id> [session-id...]
  submit <room-id> <artifact-id> <artifact-text>
  critique <room-id> <artifact-id> <pass|fail> [details...]
  status <room-id>
  wait <artifact-id> [--timeout N]"))
      ((string-equal subcommand "create")
       (%review-room-create-handler (rest tokens)))
      ((string-equal subcommand "submit")
       (%review-room-submit-handler (rest tokens)))
      ((string-equal subcommand "critique")
       (%review-room-critique-handler (rest tokens)))
      ((string-equal subcommand "status")
       (%review-room-status-handler (rest tokens)))
      ((string-equal subcommand "wait")
       (%review-room-wait-handler (rest tokens)))
      (t
       (make-slash-command-result
        :echo-input-p t
        :output (format nil "Unknown review-room subcommand ~S. Try /review-room help." subcommand))))))

(defun %review-room-create-handler (tokens)
  (let ((room-id (first tokens))
        (participants (rest tokens)))
    (unless (and room-id participants)
      (return-from %review-room-create-handler
        (make-slash-command-result :echo-input-p t :output "Usage: /review-room create <room-id> <session-id> [session-id...]")))
    (handler-case
        (let ((result (create-user-negotiation-room room-id participants)))
          (make-slash-command-result
           :echo-input-p t
           :output (format nil "Created review room ~A with ~D participant~:P."
                           room-id (length participants))))
      (error (condition)
        (make-slash-command-result
         :echo-input-p t
         :output (format nil "Failed to create review room: ~A" condition))))))

(defun %review-room-submit-handler (tokens)
  (let ((room-id (first tokens))
        (artifact-id (second tokens))
        (artifact-text (format nil "~{~A~^ ~}" (cddr tokens))))
    (unless (and room-id artifact-id (plusp (length artifact-text)))
      (return-from %review-room-submit-handler
        (make-slash-command-result :echo-input-p t :output "Usage: /review-room submit <room-id> <artifact-id> <artifact-text>")))
    (handler-case
        (let ((result (submit-user-negotiation-artifact
                       room-id nil artifact-id artifact-text)))
          (make-slash-command-result
           :echo-input-p t
           :output (format nil "Submitted artifact ~A to room ~A." artifact-id room-id)))
      (error (condition)
        (make-slash-command-result
         :echo-input-p t
         :output (format nil "Failed to submit artifact: ~A" condition))))))

(defun %review-room-critique-handler (tokens)
  (let* ((room-id (first tokens))
         (artifact-id (second tokens))
         (verdict-text (third tokens))
         (passed (and verdict-text (string-equal verdict-text "pass")))
         (details (format nil "~{~A~^ ~}" (cdddr tokens))))
    (unless (and room-id artifact-id verdict-text)
      (return-from %review-room-critique-handler
        (make-slash-command-result :echo-input-p t :output "Usage: /review-room critique <room-id> <artifact-id> <pass|fail> [details...]")))
    (handler-case
        (let ((result (add-user-negotiation-critique
                       room-id artifact-id nil passed
                       :details (when (plusp (length details)) details))))
          (make-slash-command-result
           :echo-input-p t
           :output (format nil "Critique ~A for artifact ~A in room ~A."
                           (if passed "PASS" "FAIL") artifact-id room-id)))
      (error (condition)
        (make-slash-command-result
         :echo-input-p t
         :output (format nil "Failed to add critique: ~A" condition))))))

(defun %review-room-status-handler (tokens)
  (let ((room-id (first tokens)))
    (unless room-id
      (return-from %review-room-status-handler
        (make-slash-command-result :echo-input-p t :output "Usage: /review-room status <room-id>")))
    (handler-case
        (let ((status (get-user-negotiation-room-status room-id)))
          (make-slash-command-result
           :echo-input-p t
           :output (if status
                       (with-output-to-string (out)
                         (format out "Room ~A:~%" room-id)
                         (format out "  Participants: ~{~A~^, ~}~%"
                                 (or (getf status :participant-session-ids) '("none")))
                         (format out "  Active critics: ~{~A~^, ~}~%"
                                 (or (getf status :active-critic-session-ids) '("none")))
                         (format out "  Status: ~A" (or (getf status :status) "unknown")))
                       (format nil "Room ~A not found." room-id))))
      (error (condition)
        (make-slash-command-result
         :echo-input-p t
         :output (format nil "Failed to get room status: ~A" condition))))))

(defun %review-room-wait-handler (tokens)
  (let* ((artifact-id (first tokens))
         (timeout-s 30.0))
    (loop for rest on (rest tokens) by #'cddr
          when (string-equal (first rest) "--timeout")
            do (handler-case
                   (setf timeout-s (coerce (parse-integer (second rest)) 'double-float))
                 (error () nil)))
    (unless artifact-id
      (return-from %review-room-wait-handler
        (make-slash-command-result :echo-input-p t :output "Usage: /review-room wait <artifact-id> [--timeout N]")))
    (handler-case
        (let ((decision (wait-for-user-negotiation-decision artifact-id :timeout-s timeout-s)))
          (make-slash-command-result
           :echo-input-p t
           :output (if decision
                       (format nil "Decision for ~A: ~A" artifact-id (getf decision :decision))
                       (format nil "Timed out waiting for decision on ~A." artifact-id))))
      (error (condition)
        (make-slash-command-result
         :echo-input-p t
         :output (format nil "Wait failed: ~A" condition))))))

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
  ;; NXT-003: /swarm-peers
  (register-slash-command
   (make-slash-command :name "swarm-peers" :description "List registered SW4RM session peers and capabilities." :usage "/swarm-peers" :handler #'%swarm-peers-handler))
  ;; NXT-004: /handoffs
  (register-slash-command
   (make-slash-command :name "handoffs" :description "List pending handoffs for the current session." :usage "/handoffs" :handler #'%handoffs-handler))
  ;; NXT-005: /handoff-accept, /handoff-reject, /handoff-complete
  (register-slash-command
   (make-slash-command
    :name "handoff-accept"
    :description "Accept a pending handoff request."
    :usage "/handoff-accept <handoff-id>"
    :parameters
    (list (make-slash-command-parameter :name "id" :type :string :required-p t :description "Handoff identifier."))
    :handler #'%handoff-accept-handler))
  (register-slash-command
   (make-slash-command
    :name "handoff-reject"
    :description "Reject a pending handoff request."
    :usage "/handoff-reject <handoff-id> [reason...]"
    :parameters
    (list (make-slash-command-parameter :name "args" :type :string :required-p t :greedy-p t :description "Handoff ID and optional rejection reason."))
    :handler #'%handoff-reject-handler))
  (register-slash-command
   (make-slash-command
    :name "handoff-complete"
    :description "Mark a handoff as complete."
    :usage "/handoff-complete <handoff-id>"
    :parameters
    (list (make-slash-command-parameter :name "id" :type :string :required-p t :description "Handoff identifier."))
    :handler #'%handoff-complete-handler))
  (register-slash-command
   (make-slash-command
    :name "worktree-handoff"
    :description "Inspect or update manual worktree merge conflict handoffs."
    :usage "/worktree-handoff <list|inspect|accept|defer|help> [args...]"
    :parameters
    (list (make-slash-command-parameter :name "args" :type :string :required-p nil :greedy-p t :description "Subcommand and arguments."))
    :handler #'%worktree-handoff-handler))
  ;; NXT-006: /review-room
  (register-slash-command
   (make-slash-command
    :name "review-room"
    :description "Multi-user code review rooms: create, submit, critique, status, wait."
    :usage "/review-room <create|submit|critique|status|wait> [args...]"
    :parameters
    (list (make-slash-command-parameter :name "args" :type :string :required-p nil :greedy-p t :description "Subcommand and arguments."))
    :handler #'%review-room-handler))
  ;; NXT-014: /agent-tree
  (register-slash-command
   (make-slash-command
    :name "agent-tree"
    :description "Show parent-child execution tree linking prompts, agents, SW4RM handoffs, and workers."
    :usage "/agent-tree"
    :handler #'%agent-tree-handler))
  ;; NXT-015: /swarm-status
  (register-slash-command
   (make-slash-command
    :name "swarm-status"
    :description "Surface SW4RM state: delegation mode, agent counts, handoff count, and open review rooms."
    :usage "/swarm-status"
    :handler #'%swarm-status-handler))
  t)
