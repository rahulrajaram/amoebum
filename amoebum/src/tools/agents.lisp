(in-package :amoebum)

;;; ============================================================
;;; Multi-Agent Orchestration Tools
;;;
;;; 8 deftool definitions that expose the worker/agent
;;; infrastructure to the LLM for multi-agent workflows.
;;; ============================================================

;;; --- Helper: serialize plist to JSON-like string ---

(defun %agent-tool-serialize (plist)
  "Serialize a plist or list of plists to a JSON-like string for LLM consumption."
  (cond
    ((null plist) "{}")
    ((and (consp plist) (keywordp (first plist)))
     ;; Single plist
     (with-output-to-string (out)
       (write-char #\{ out)
       (loop for (key value) on plist by #'cddr
             for first-p = t then nil do
               (unless first-p (write-string ", " out))
               (format out "~S: ~A"
                       (string-downcase (symbol-name key))
                       (%agent-tool-format-value value)))
       (write-char #\} out)))
    ((and (consp plist) (consp (first plist)))
     ;; List of plists
     (with-output-to-string (out)
       (write-char #\[ out)
       (loop for item in plist
             for first-p = t then nil do
               (unless first-p (write-string ", " out))
               (write-string (%agent-tool-serialize item) out))
       (write-char #\] out)))
    (t (princ-to-string plist))))

(defun %agent-tool-format-value (value)
  "Format a single value for JSON-like serialization."
  (cond
    ((null value) "null")
    ((eq value t) "true")
    ((eq value nil) "false")
    ((keywordp value) (format nil "~S" (string-downcase (symbol-name value))))
    ((stringp value) (format nil "~S" value))
    ((numberp value) (princ-to-string value))
    ((and (consp value) (keywordp (first value)))
     (%agent-tool-serialize value))
    ((listp value)
     (format nil "[~{~A~^, ~}]"
             (mapcar #'%agent-tool-format-value value)))
    (t (format nil "~S" (princ-to-string value)))))

;;; --- Tool 1: spawn-agent-worker ---

(deftool spawn-agent-worker
    ((task string :required t :description "Task description for the agent to perform.")
     (persona string :description "Persona name from available agent personas manifest.")
     (custom-prompt string :description "Ad-hoc system prompt override (ignored if persona is given).")
     (label string :description "Human-readable label for the worker.")
     (timeout-seconds integer :default 600 :description "Worker timeout in seconds."))
  "Spawn a single agent worker with optional persona. Returns worker ID and status."
  (:permission :supervised)
  (:category :agents)
  (:timeout 10)
  (let* ((personas (discover-persona-files))
         (persona-def (when (and persona (plusp (length persona)))
                        (find-persona-by-name persona personas)))
         (effective-prompt (cond
                            (persona-def (persona-definition-system-prompt persona-def))
                            ((and custom-prompt (plusp (length custom-prompt)))
                             custom-prompt)
                            (t nil)))
         (effective-label (or (and label (plusp (length label)) label)
                              (and persona-def
                                   (format nil "~A: ~A"
                                           (persona-definition-name persona-def)
                                           (subseq task 0 (min 50 (length task)))))
                              (subseq task 0 (min 60 (length task)))))
         (worker (spawn-worker :agent task
                               :label effective-label
                               :timeout-seconds (or timeout-seconds 600))))
    ;; Store persona metadata on the worker for observability
    (when persona-def
      (%with-worker-lock
        (setf (worker-record-result worker)
              (list :persona (persona-definition-name persona-def)
                    :system-prompt-override (and effective-prompt t)))))
    (%agent-tool-serialize
     (list :worker-id (worker-record-id worker)
           :status (worker-record-status worker)
           :label effective-label
           :persona (and persona-def (persona-definition-name persona-def))))))

;;; --- Tool 2: fan-out-workers ---

(deftool fan-out-workers
    ((worker-specs list :required t
                   :description "List of worker specs. Each spec is a JSON object with keys: task (required), persona, label, type (shell or agent, default agent).")
     (timeout-seconds integer :default 600
                      :description "Group-level timeout in seconds."))
  "Spawn multiple workers concurrently. Returns group ID and worker IDs."
  (:permission :supervised)
  (:category :agents)
  (:timeout 10)
  (let* ((personas (discover-persona-files))
         (specs (mapcar (lambda (spec)
                          (let* ((spec-task (or (getf spec :task)
                                               (cdr (assoc "task" spec :test #'string=))
                                               ""))
                                 (spec-persona (or (getf spec :persona)
                                                   (cdr (assoc "persona" spec :test #'string=))))
                                 (spec-label (or (getf spec :label)
                                                 (cdr (assoc "label" spec :test #'string=))))
                                 (spec-type (or (getf spec :type)
                                                (cdr (assoc "type" spec :test #'string=))
                                                :agent))
                                 (type-kw (cond
                                            ((keywordp spec-type) spec-type)
                                            ((string-equal spec-type "shell") :shell)
                                            (t :agent))))
                            (list :type type-kw
                                  :command (if (stringp spec-task)
                                               spec-task
                                               (princ-to-string spec-task))
                                  :label (or spec-label
                                             (and spec-persona
                                                  (let ((pd (find-persona-by-name
                                                             spec-persona personas)))
                                                    (when pd
                                                      (persona-definition-name pd))))
                                             (subseq (princ-to-string spec-task)
                                                     0 (min 60 (length
                                                                 (princ-to-string spec-task))))))))
                        (if (listp worker-specs) worker-specs '()))))
    (multiple-value-bind (group-id worker-ids)
        (fan-out-workers specs :timeout-seconds (or timeout-seconds 600))
      (%agent-tool-serialize
       (list :group-id group-id
             :worker-ids worker-ids
             :count (length worker-ids))))))

;;; --- Tool 3: join-workers ---

(deftool join-workers
    ((group-id string :required t :description "Worker group ID from fan-out-workers."))
  "Block until all workers in a group complete. Returns results for each worker."
  (:permission :auto)
  (:category :agents)
  (:timeout 660)
  (let ((results (join-worker-group group-id)))
    (if results
        (%agent-tool-serialize
         (mapcar (lambda (triple)
                   (list :worker-id (first triple)
                         :status (second triple)
                         :output (or (worker-output (first triple)) "")))
                 results))
        (%agent-tool-serialize
         (list :error "Group not found"
               :group-id group-id)))))

;;; --- Tool 4: race-workers ---

(deftool race-workers
    ((group-id string :required t :description "Worker group ID from fan-out-workers."))
  "Block until the first worker in a group completes. Cancels remaining workers."
  (:permission :auto)
  (:category :agents)
  (:timeout 660)
  (multiple-value-bind (winner-id winner-status winner-result)
      (race-worker-group group-id)
    (if winner-id
        (%agent-tool-serialize
         (list :winner-id winner-id
               :status winner-status
               :output (or (worker-output winner-id) "")))
        (%agent-tool-serialize
         (list :error "No winner (timeout or group not found)"
               :group-id group-id)))))

;;; --- Tool 5: worker-status ---

(deftool worker-status-tool
    ((worker-id string :required t :description "Worker ID to check status for."))
  "Check the current status of a worker."
  (:permission :auto)
  (:category :agents)
  (:timeout 5)
  (let ((status (worker-status worker-id)))
    (if status
        (let ((worker (%find-worker worker-id)))
          (%agent-tool-serialize
           (list :worker-id worker-id
                 :status status
                 :label (and worker (worker-record-label worker))
                 :error-message (and worker (worker-record-error-message worker)))))
        (%agent-tool-serialize
         (list :worker-id worker-id
               :status :not-found)))))

;;; --- Tool 6: worker-result ---

(deftool worker-result-tool
    ((worker-id string :required t :description "Worker ID to get the result for."))
  "Get the result of a completed worker."
  (:permission :auto)
  (:category :agents)
  (:timeout 5)
  (let ((result (worker-result worker-id))
        (output (worker-output worker-id))
        (status (worker-status worker-id)))
    (%agent-tool-serialize
     (list :worker-id worker-id
           :status (or status :not-found)
           :result result
           :output (or output "")))))

;;; --- Tool 7: worker-cancel ---

(deftool worker-cancel-tool
    ((worker-id string :required t :description "Worker ID to cancel."))
  "Cancel a running worker."
  (:permission :supervised)
  (:category :agents)
  (:timeout 5)
  (let ((cancelled (worker-cancel worker-id)))
    (%agent-tool-serialize
     (list :worker-id worker-id
           :cancelled cancelled
           :status (or (worker-status worker-id) :not-found)))))

;;; --- Tool 8: list-workers ---

(deftool list-workers-tool
    ((include-finished boolean :default t :description "Whether to include completed/failed workers."))
  "List active and recent workers."
  (:permission :auto)
  (:category :agents)
  (:timeout 5)
  (let ((workers (worker-list :include-finished (if (eq include-finished nil) nil t))))
    (%agent-tool-serialize
     (list :count (length workers)
           :workers (mapcar (lambda (w)
                              (list :id (worker-record-id w)
                                    :type (worker-record-type w)
                                    :label (worker-record-label w)
                                    :status (worker-record-status w)
                                    :error-message (worker-record-error-message w)))
                            workers)))))
