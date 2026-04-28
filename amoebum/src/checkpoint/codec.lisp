(in-package :amoebum)

(defun %checkpoint-encode-value (value)
  (cond
    ((stringp value) value)
    ((hash-table-p value)
     (list :__hash-table__
           (loop for key being the hash-keys of value
                 collect (list (%checkpoint-encode-value key)
                               (%checkpoint-encode-value (gethash key value))))))
    ((pathnamep value)
     (list :__pathname__ (namestring value)))
    ((vectorp value)
     (list :__vector__
           (loop for item across value
                 collect (%checkpoint-encode-value item))))
    ((consp value)
     (mapcar #'%checkpoint-encode-value value))
    (t value)))

(defun %checkpoint-decode-value (value)
  (cond
    ((and (consp value)
          (eq (first value) :__hash-table__)
          (listp (second value)))
     (let ((table (make-hash-table :test #'equal)))
       (dolist (pair (second value))
         (when (and (listp pair) (= (length pair) 2))
           (setf (gethash (%checkpoint-decode-value (first pair)) table)
                 (%checkpoint-decode-value (second pair)))))
       table))
    ((and (consp value)
          (eq (first value) :__pathname__)
          (stringp (second value)))
     (pathname (second value)))
    ((and (consp value)
          (eq (first value) :__vector__))
     (coerce (mapcar #'%checkpoint-decode-value (second value)) 'vector))
    ((consp value)
     (mapcar #'%checkpoint-decode-value value))
    (t value)))

(defun %function-symbol->snapshot (fn)
  (when (functionp fn)
    (multiple-value-bind (_ closurep name)
        (function-lambda-expression fn)
      (declare (ignore _ closurep))
      (when (and (symbolp name) (symbol-package name))
        (list :package (package-name (symbol-package name))
              :name (symbol-name name))))))

(defun %snapshot->function-symbol (snapshot)
  (when (and (listp snapshot)
             (stringp (getf snapshot :package))
             (stringp (getf snapshot :name)))
    (let ((package (find-package (getf snapshot :package))))
      (when package
        (find-symbol (getf snapshot :name) package)))))

(defun %fallback-restored-tool-fn (tool-name)
  (lambda (&rest _)
    (declare (ignore _))
    (format nil "Tool ~A is unavailable after checkpoint restore." tool-name)))

(defun %tool-definition->snapshot (definition)
  (check-type definition pseudopod:tool-definition)
  (list :name (pseudopod:tool-definition-name definition)
        :description (pseudopod:tool-definition-description definition)
        :parameters (%checkpoint-encode-value
                     (pseudopod:tool-definition-parameters definition))
        :fn-symbol (%function-symbol->snapshot
                    (pseudopod:tool-definition-fn definition))))

(defun %snapshot->tool-definition (snapshot)
  (let* ((tool-name (or (getf snapshot :name) "unknown-tool"))
         (fn-symbol (%snapshot->function-symbol (getf snapshot :fn-symbol)))
         (fn (if (and fn-symbol (fboundp fn-symbol))
                 (symbol-function fn-symbol)
                 (%fallback-restored-tool-fn tool-name))))
    (pseudopod:make-tool-definition
     :name tool-name
     :description (or (getf snapshot :description) "")
     :parameters (%checkpoint-decode-value (getf snapshot :parameters))
     :fn fn)))

(defun %tool-metadata->snapshot (metadata)
  (when (tool-metadata-p metadata)
    (list :name (tool-metadata-name metadata)
          :permission (tool-metadata-permission metadata)
          :dangerous-p (tool-metadata-dangerous-p metadata)
          :category (tool-metadata-category metadata)
          :timeout-seconds (tool-metadata-timeout-seconds metadata)
          :source-file (%checkpoint-encode-value
                        (tool-metadata-source-file metadata))
          :source-line (tool-metadata-source-line metadata)
          :parameter-specs (%checkpoint-encode-value
                            (tool-metadata-parameter-specs metadata))
          :defined-at (tool-metadata-defined-at metadata)
          :mcp-server (tool-metadata-mcp-server metadata))))

(defun %snapshot->tool-metadata (snapshot)
  (make-tool-metadata
   :name (getf snapshot :name)
   :permission (getf snapshot :permission)
   :dangerous-p (not (null (getf snapshot :dangerous-p)))
   :category (getf snapshot :category)
   :timeout-seconds (getf snapshot :timeout-seconds)
   :source-file (%checkpoint-decode-value (getf snapshot :source-file))
   :source-line (getf snapshot :source-line)
   :parameter-specs (%checkpoint-decode-value (getf snapshot :parameter-specs))
   :defined-at (getf snapshot :defined-at)
   :mcp-server (getf snapshot :mcp-server)))

(defun %tool-history-entry->snapshot (entry)
  (when (tool-history-entry-p entry)
    (list :tool-definition (%tool-definition->snapshot
                            (tool-history-entry-tool-definition entry))
          :tool-metadata (%tool-metadata->snapshot
                          (tool-history-entry-tool-metadata entry))
          :timestamp (tool-history-entry-timestamp entry)
          :source-file (%checkpoint-encode-value
                        (tool-history-entry-source-file entry))
          :source-line (tool-history-entry-source-line entry))))

(defun %snapshot->tool-history-entry (snapshot)
  (make-tool-history-entry
   :tool-definition (and (getf snapshot :tool-definition)
                         (%snapshot->tool-definition
                          (getf snapshot :tool-definition)))
   :tool-metadata (and (getf snapshot :tool-metadata)
                       (%snapshot->tool-metadata
                        (getf snapshot :tool-metadata)))
   :timestamp (or (getf snapshot :timestamp) (get-universal-time))
   :source-file (%checkpoint-decode-value (getf snapshot :source-file))
   :source-line (getf snapshot :source-line)))

(defun %tools->snapshot ()
  (let ((definitions
          (sort (mapcar #'%tool-definition->snapshot
                        (pseudopod:toolset-tools *toolset*))
                #'string<
                :key (lambda (entry)
                       (string-downcase (or (getf entry :name) "")))))
        (metadata '())
        (history '()))
    (maphash (lambda (tool-name metadata-value)
               (push (list :name tool-name
                           :metadata (%tool-metadata->snapshot metadata-value))
                     metadata))
             *tool-metadata*)
    (maphash (lambda (tool-name entries)
               (push (list :name tool-name
                           :entries (mapcar #'%tool-history-entry->snapshot entries))
                     history))
             *tool-history*)
    (list :definitions definitions
          :metadata (sort metadata #'string<
                          :key (lambda (entry)
                                 (string-downcase
                                  (or (getf entry :name) ""))))
          :history (sort history #'string<
                         :key (lambda (entry)
                                (string-downcase
                                 (or (getf entry :name) "")))))))

(defun %restore-tools-from-snapshot (snapshot)
  (let ((definitions (or (getf snapshot :definitions) '()))
        (metadata-snapshots (or (getf snapshot :metadata) '()))
        (history-snapshots (or (getf snapshot :history) '()))
        (toolset (pseudopod:make-toolset))
        (metadata (make-hash-table :test #'equal))
        (history (make-hash-table :test #'equal)))
    (dolist (entry definitions)
      (when (and (listp entry) (getf entry :name))
        (pseudopod:register-tool toolset (%snapshot->tool-definition entry))))
    (dolist (entry metadata-snapshots)
      (let ((name (and (listp entry) (getf entry :name)))
            (value (and (listp entry) (getf entry :metadata))))
        (when (and name value)
          (setf (gethash name metadata) (%snapshot->tool-metadata value)))))
    (dolist (entry history-snapshots)
      (let ((name (and (listp entry) (getf entry :name)))
            (entries (and (listp entry) (getf entry :entries))))
        (when name
          (setf (gethash name history)
                (mapcar #'%snapshot->tool-history-entry (or entries '()))))))
    (setf *toolset* toolset
          *tool-metadata* metadata
          *tool-history* history)
    toolset))

(defun %memory-entry->snapshot (entry)
  (check-type entry memory-entry)
  (list :key (memory-entry-key entry)
        :value (memory-entry-value entry)
        :scope (%checkpoint-encode-value (memory-entry-scope entry))
        :source (memory-entry-source entry)
        :created-at (memory-entry-created-at entry)))

(defun %snapshot->memory-entry (snapshot)
  (make-memory-entry
   :key (getf snapshot :key)
   :value (getf snapshot :value)
   :scope (%checkpoint-decode-value (getf snapshot :scope))
   :source (getf snapshot :source)
   :created-at (or (getf snapshot :created-at) (get-universal-time))))

(defun %memory-scope-snapshot (backend scope)
  (handler-case
      (mapcar #'%memory-entry->snapshot
              (memory-list backend :scope scope))
    (error () '())))

(defun %memory->snapshot (backend)
  (list :backend-kind (memory-backend-kind backend)
        :global (%memory-scope-snapshot backend :global)
        :project (%memory-scope-snapshot backend :project)
        :session (%memory-scope-snapshot backend :session)
        :effective (%memory-scope-snapshot backend :effective)))

(defun %restore-memory-from-snapshot (snapshot backend)
  (let ((session-entries (mapcar #'%snapshot->memory-entry
                                 (or (getf snapshot :session) '()))))
    (when (file-memory-backend-p backend)
      (let ((global-entries (mapcar #'%snapshot->memory-entry
                                    (or (getf snapshot :global) '())))
            (project-entries (mapcar #'%snapshot->memory-entry
                                     (or (getf snapshot :project) '()))))
        (%write-memory-file (file-memory-backend-global-path backend) global-entries)
        (%write-memory-file (file-memory-backend-project-path backend) project-entries)))
    (setf *session-memory-entries* session-entries)
    backend))

(defun %agent-record->snapshot (agent)
  (check-type agent agent-record)
  (list :id (agent-record-id agent)
        :type (agent-record-type agent)
        :task (agent-record-task agent)
        :parent-message-id (agent-record-parent-message-id agent)
        :status (agent-record-status agent)
        :created-ms (agent-record-created-ms agent)
        :started-ms (agent-record-started-ms agent)
        :finished-ms (agent-record-finished-ms agent)
        :cancel-requested-p (not (null (agent-record-cancel-requested-p agent)))
        :result (agent-record-result agent)
        :stdout (agent-record-stdout agent)
        :stderr (agent-record-stderr agent)
        :error-message (agent-record-error-message agent)))

(defun %snapshot->agent-record (snapshot)
  (%make-agent-record
   :id (or (getf snapshot :id)
           (format nil "task-~4,'0D"
                   (1+ (max 0 (or *next-agent-sequence* 0)))))
   :type (%normalize-agent-type (getf snapshot :type))
   :task (or (getf snapshot :task) "")
   :parent-message-id (getf snapshot :parent-message-id)
   :status (if (keywordp (getf snapshot :status))
               (getf snapshot :status)
               :queued)
   :created-ms (if (integerp (getf snapshot :created-ms))
                   (getf snapshot :created-ms)
                   (%agent-now-ms))
   :started-ms (if (integerp (getf snapshot :started-ms))
                   (getf snapshot :started-ms)
                   0)
   :finished-ms (if (integerp (getf snapshot :finished-ms))
                    (getf snapshot :finished-ms)
                    0)
   :cancel-requested-p (not (null (getf snapshot :cancel-requested-p)))
   :result (getf snapshot :result)
   :stdout (getf snapshot :stdout)
   :stderr (getf snapshot :stderr)
   :error-message (getf snapshot :error-message)
   :thread nil))

(defun %agent-sequence-from-id (agent-id)
  (when (stringp agent-id)
    (let ((separator (position #\- agent-id :from-end t)))
      (when (and separator (< separator (1- (length agent-id))))
        (ignore-errors
          (parse-integer agent-id :start (1+ separator)))))))

(defun %agents->snapshot ()
  (let ((records '())
        (max-sequence 0))
    (%with-agent-registry-lock ()
      (maphash (lambda (_id agent)
                 (declare (ignore _id))
                 (when (typep agent 'agent-record)
                   (let* ((entry (%agent-record->snapshot agent))
                          (sequence (%agent-sequence-from-id
                                     (getf entry :id))))
                     (when (and sequence (> sequence max-sequence))
                       (setf max-sequence sequence))
                     (push entry records))))
               *agent-registry*)
      (when (and (integerp *next-agent-sequence*)
                 (> *next-agent-sequence* max-sequence))
        (setf max-sequence *next-agent-sequence*)))
    (list :next-agent-sequence max-sequence
          :records (sort records
                         #'string<
                         :key (lambda (entry)
                                (string-downcase
                                 (or (getf entry :id) "")))))))

(defun %restore-agents-from-snapshot (snapshot)
  (let ((records (if (and (listp snapshot)
                          (listp (getf snapshot :records)))
                     (getf snapshot :records)
                     '()))
        (snapshot-sequence (and (listp snapshot)
                                (getf snapshot :next-agent-sequence)))
        (max-sequence 0))
    (ptui.runtime.queue:queue-pop-all *agent-completion-queue*)
    (%with-agent-registry-lock ()
      (clrhash *agent-registry*)
      (dolist (entry records)
        (when (listp entry)
          (let ((agent (%snapshot->agent-record entry)))
            (setf (gethash (agent-record-id agent) *agent-registry*) agent)
            (let ((sequence (%agent-sequence-from-id (agent-record-id agent))))
              (when (and sequence (> sequence max-sequence))
                (setf max-sequence sequence))))))
      (setf *next-agent-sequence*
            (max max-sequence
                 (if (and (integerp snapshot-sequence)
                          (>= snapshot-sequence 0))
                     snapshot-sequence
                     0))))
    (list-agents :include-completed-p t)))

(defun %mcp-server->snapshot (server)
  "Capture serializable configuration from a live MCP-SERVER struct."
  (check-type server mcp-server)
  (list :name (mcp-server-name server)
        :transport (mcp-server-transport server)
        :command (mcp-server-command server)
        :args (copy-list (mcp-server-args server))
        :cwd (and (mcp-server-cwd server)
                  (namestring (mcp-server-cwd server)))
        :endpoint-url (mcp-server-endpoint-url server)
        :http-headers (copy-tree (mcp-server-http-headers server))
        :auto-restart-p (mcp-server-auto-restart-p server)))

(defun %mcp-servers->snapshot ()
  "Iterate *mcp-tool-server-registry* and snapshot each server's config."
  (let ((entries '()))
    (maphash (lambda (name server)
               (declare (ignore name))
               (when (mcp-server-p server)
                 (handler-case
                     (push (%mcp-server->snapshot server) entries)
                   (error () nil))))
             *mcp-tool-server-registry*)
    (sort entries #'string< :key (lambda (e) (or (getf e :name) "")))))

(defun %restore-mcp-servers-from-snapshot (snapshot)
  "Re-register and start MCP servers from snapshot config.
Each entry that fails to start is logged but does not abort the restore."
  (when (listp snapshot)
    (dolist (entry snapshot)
      (when (listp entry)
        (let ((name (getf entry :name)))
          (when (and name (not (find-mcp-tool-server name)))
            (handler-case
                (let* ((transport (or (getf entry :transport) :stdio))
                       (server (make-mcp-server
                                :name name
                                :transport transport
                                :command (getf entry :command)
                                :args (or (getf entry :args) '())
                                :cwd (getf entry :cwd)
                                :endpoint-url (getf entry :endpoint-url)
                                :http-headers (getf entry :http-headers)
                                :auto-restart-p (getf entry :auto-restart-p))))
                  (mcp-server-start server)
                  (register-mcp-tool-server server
                                            :name name
                                            :discover-tools-p t))
              (error (condition)
                (warn "Failed to restore MCP server ~A: ~A" name condition)))))))))

(defun %path-approvals->snapshot ()
  (%serialize-path-approval-memory))

(defun %restore-path-approvals-from-snapshot (snapshot)
  (when (and (listp snapshot) (getf snapshot :entries))
    (let ((raw-entries (getf snapshot :entries))
          (loaded 0))
      (when (listp raw-entries)
        (dolist (entry raw-entries)
          (let ((normalized (%normalize-persisted-path-approval-entry entry)))
            (when normalized
              (%merge-persistent-path-approval-entry normalized)
              (incf loaded)))))
      (%trim-path-approval-memory)
      loaded)))

(defun %conversation->snapshot (conversation)
  (when (typep conversation 'conversation-state)
    (amoebum.fp:update
     '()
     (:session-id (conversation-state-session-id conversation))
     (:state (conversation-state-state conversation))
     (:created-at (conversation-state-created-at conversation))
     (:updated-at (conversation-state-updated-at conversation))
     (:active-fork (conversation-state-active-fork conversation))
     (:fork-branch-point (conversation-state-fork-branch-point conversation))
     (:forks (copy-tree (conversation-state-forks conversation)))
     (:entries (mapcar #'%conversation-entry->sexp
                       (conversation-state-entries conversation))))))

(defun %conversation-from-snapshot (snapshot project-root)
  (let* ((root (uiop:ensure-directory-pathname project-root))
         (session-id (or (getf snapshot :session-id)
                         (%checkpoint-id-from-time)))
         (entries (mapcar #'%conversation-entry-coerce
                          (or (getf snapshot :entries) '())))
         (active-fork (or (getf snapshot :active-fork)
                          +conversation-default-fork-name+))
         (forks (%conversation-normalize-forks
                 (or (getf snapshot :forks) '())
                 active-fork
                 entries))
         (conversation
           (%make-conversation-state
            :session-id session-id
            :state (%conversation-normalize-state (getf snapshot :state))
            :entries (%conversation-copy-entries entries)
            :created-at (or (getf snapshot :created-at) (get-universal-time))
            :updated-at (or (getf snapshot :updated-at) (get-universal-time))
            :active-fork active-fork
            :fork-branch-point (getf snapshot :fork-branch-point)
            :forks forks
            :project-root root
            :session-path (conversation-session-path session-id
                                                     :project-root root))))
    (conversation-save conversation :save-manifest-p t :save-fork-file-p t)
    conversation))

(defun %config->snapshot (config)
  (let ((values '()))
    (maphash (lambda (key value)
               (let ((source (gethash key (config-sources config))))
                 (push (list :key key
                             :value (%checkpoint-encode-value value)
                             :source source)
                       values)))
             (config-values config))
    (list :model (config-model config)
          :permission-mode (config-permission-mode config)
          :memory-backend (config-memory-backend config)
          :values (sort values #'string<
                        :key (lambda (entry)
                               (%checkpoint-sort-key (getf entry :key)))))))

(defun %restore-config-from-snapshot (snapshot project-root)
  (let ((cfg (reload-config :project-root project-root)))
    (dolist (entry (or (getf snapshot :values) '()))
      (let ((key (getf entry :key))
            (source (getf entry :source))
            (value (%checkpoint-decode-value (getf entry :value))))
        (when (and key
                   (not (eq key :project-root))
                   (not (eq source :built-in)))
          (ignore-errors
            (setconfig key value)))))
    cfg))

(defun %extension-record->snapshot (entry)
  (when (extension-load-record-p entry)
    (list :path (extension-load-record-path entry)
          :scope (extension-load-record-scope entry)
          :status (extension-load-record-status entry)
          :message (extension-load-record-message entry)
          :timestamp (extension-load-record-timestamp entry))))

(defun %extensions->snapshot ()
  (let ((disabled '()))
    (maphash (lambda (key value)
               (when value
                 (push key disabled)))
             *disabled-extensions*)
    (list :load-report (mapcar #'%extension-record->snapshot
                               (list-extension-report))
          :loaded (mapcar #'%extension-record->snapshot
                          (list-loaded-extensions))
          :last-discovered (copy-list *extension-last-discovered*)
          :disabled-keys (sort disabled #'string<))))

(defun %restore-extensions-from-snapshot (snapshot &key project-root)
  (clrhash *disabled-extensions*)
  (dolist (disabled-key (or (getf snapshot :disabled-keys) '()))
    (let ((trimmed (%checkpoint-trim disabled-key)))
      (when (plusp (length trimmed))
        (setf (gethash (string-downcase trimmed) *disabled-extensions*) t))))
  (setf *extension-last-discovered*
        (copy-list (or (getf snapshot :last-discovered) '())))
  (reload-user-extensions :project-root project-root))

(defun %checkpoint-payload (&key
                              checkpoint-id
                              created-at
                              project-root
                              trigger
                              auto-p
                              config
                              conversation
                              extensions
                              tools
                              memory
                              agents
                              mcp-servers
                              path-approvals)
  (list :checkpoint-version 1
        :checkpoint-id checkpoint-id
        :created-at created-at
        :trigger (if (keywordp trigger) trigger :manual)
        :auto-p (not (null auto-p))
        :project-root (namestring (uiop:ensure-directory-pathname project-root))
        :config config
        :conversation conversation
        :extensions extensions
        :tools tools
        :memory memory
        :agents agents
        :mcp-servers mcp-servers
        :path-approvals path-approvals))

(defun %write-checkpoint-payload (path payload)
  (ensure-directories-exist path)
  (with-open-file (stream path
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create
                          :external-format :utf-8)
    (let ((*print-pretty* t)
          (*print-readably* t))
      (prin1 payload stream)
      (terpri stream)))
  path)

(defun %read-checkpoint-payload (path)
  (let ((resolved (and path (probe-file path))))
    (unless resolved
      (return-from %read-checkpoint-payload nil))
    (handler-case
        (with-open-file (stream resolved :direction :input :external-format :utf-8)
          (read stream nil nil))
      (error () nil))))

(defun %session-snapshot-payload (&key
                                    snapshot-id
                                    created-at
                                    project-root
                                    conversation
                                    memory
                                    agents)
  (list :snapshot-version 1
        :snapshot-id snapshot-id
        :created-at created-at
        :project-root (namestring (uiop:ensure-directory-pathname project-root))
        :conversation conversation
        :memory memory
        :agents agents))
