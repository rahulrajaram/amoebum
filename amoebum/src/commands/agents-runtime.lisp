(in-package :amoebum)

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

(defun %agent-tree-format-node (out agent indent-prefix)
  (format out "~A~A | ~A | ~A | ~A~%"
          indent-prefix
          (runtime-agent-id agent)
          (%agent-status-text (runtime-agent-status agent))
          (%runtime-agent-backend-label agent)
          (%agent-task-summary agent)))

(defun %agent-tree-handler (_invocation _arguments _context)
  (declare (ignore _invocation _arguments _context))
  (let* ((all-local (list-agents :include-completed-p t))
         (all-swarm (list-swarm-agents))
         (roots '())
         (by-parent (make-hash-table :test #'equal)))
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
                   (when roots
                     (format out "[root agents]~%")
                     (dolist (agent roots)
                       (%agent-tree-format-node out agent "  ")))
                   (let ((parent-ids '()))
                     (maphash (lambda (key _value)
                                (declare (ignore _value))
                                (push key parent-ids))
                              by-parent)
                     (setf parent-ids (sort parent-ids #'string<))
                     (dolist (parent-id parent-ids)
                       (let ((children (nreverse (gethash parent-id by-parent '()))))
                         (format out "[message ~A]~%" parent-id)
                         (dolist (child children)
                           (%agent-tree-format-node out child "  ")))))
                   (when all-swarm
                     (format out "[sw4rm agents]~%")
                     (dolist (agent all-swarm)
                       (%agent-tree-format-node out agent "  "))))))))

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
       (make-slash-command-result
        :output (format nil "Unsupported /agent action ~S." action))))))

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

(defun register-agent-runtime-slash-commands ()
  (register-slash-command
   (make-slash-command
    :name "agents"
    :description "List currently running background agents."
    :usage "/agents"
    :handler #'%agents-handler))
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
    :name "spawn"
    :description "Spawn a sw4rm sub-agent for a task."
    :usage "/spawn <task-description>"
    :parameters
    (list (make-slash-command-parameter :name "task" :type :string :required-p t :greedy-p t :description "Task description for the spawned agent."))
    :handler #'%spawn-handler))
  (register-slash-command
   (make-slash-command
    :name "agent-tree"
    :description "Show parent-child execution tree linking prompts, agents, SW4RM handoffs, and workers."
    :usage "/agent-tree"
    :handler #'%agent-tree-handler))
  t)
