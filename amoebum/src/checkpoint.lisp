(in-package :amoebum)

(defparameter *checkpoint-directory-override* nil)
(defparameter *session-snapshot-directory-override* nil)
(defparameter *checkpoint-last-activity-at* (get-universal-time))
(defparameter *checkpoint-last-auto-checkpoint-at* nil)
(defparameter *checkpoint-default-max-count* 10)
;; Declared in src/macros/deftool.lisp; referenced here for load-order safety.
(defvar *toolset*)
(defvar *tool-metadata*)
(defvar *tool-history*)

(defstruct (session-checkpoint
            (:constructor make-session-checkpoint
                (&key id path created-at (auto-p nil) (trigger :manual))))
  id
  path
  (created-at 0 :type integer)
  (auto-p nil :type boolean)
  (trigger :manual :type keyword))

(defun %checkpoint-trim (value)
  (if (stringp value)
      (string-trim '(#\Space #\Tab #\Newline #\Return) value)
      ""))

(defun %checkpoint-project-root (&key project-root config)
  (uiop:ensure-directory-pathname
   (or project-root
       (and (config-p config) (config-project-root config))
       (and (ignore-errors (current-config))
            (config-project-root (ignore-errors (current-config))))
       (ignore-errors (uiop:getcwd))
       *default-pathname-defaults*)))

(defun checkpoint-directory (&key project-root config)
  (or (and *checkpoint-directory-override*
           (uiop:ensure-directory-pathname *checkpoint-directory-override*))
      (merge-pathnames #P".amoebum/checkpoints/"
                       (%checkpoint-project-root :project-root project-root
                                                 :config config))))

(defun session-snapshot-directory ()
  (or (and *session-snapshot-directory-override*
           (uiop:ensure-directory-pathname *session-snapshot-directory-override*))
      (merge-pathnames #P".amoebum/session-snapshots/"
                       (user-homedir-pathname))))

(defun %checkpoint-id-from-time (&optional (timestamp (get-universal-time)))
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time timestamp 0)
    (format nil "~4,'0D~2,'0D~2,'0DT~2,'0D~2,'0D~2,'0DZ"
            year month day hour minute second)))

(defun %checkpoint-project-name (&key project-root config)
  (let* ((root (%checkpoint-project-root :project-root project-root :config config))
         (segments (ignore-errors (pathname-directory root)))
         (leaf (and (listp segments) (car (last segments)))))
    (cond
      ((stringp leaf) leaf)
      ((symbolp leaf) (symbol-name leaf))
      (t "project"))))

(defun %checkpoint-slugify (value &optional (fallback "project"))
  (let* ((raw (string-downcase (%checkpoint-trim (princ-to-string value))))
         (buffer (make-string-output-stream))
         (last-was-dash nil))
    (loop for ch across raw do
      (cond
        ((or (alphanumericp ch) (char= ch #\_) (char= ch #\-))
         (write-char ch buffer)
         (setf last-was-dash nil))
        (last-was-dash
         nil)
        (t
         (write-char #\- buffer)
         (setf last-was-dash t))))
    (let* ((candidate (%checkpoint-trim (get-output-stream-string buffer)))
           (trimmed (string-trim "-" candidate)))
      (if (plusp (length trimmed))
          trimmed
          fallback))))

(defun %checkpoint-path (checkpoint-id &key project-root config)
  (let* ((project-name (%checkpoint-project-name :project-root project-root
                                                 :config config))
         (project-slug (%checkpoint-slugify project-name))
         (filename (format nil "~A-~A.core" project-slug checkpoint-id)))
    (merge-pathnames (pathname filename)
                     (checkpoint-directory :project-root project-root
                                           :config config))))

(defun %next-checkpoint-path (&key project-root config timestamp)
  (let ((base-id (%checkpoint-id-from-time timestamp)))
    (loop for suffix from 0
          for id = (if (zerop suffix)
                       base-id
                       (format nil "~A-~D" base-id suffix))
          for path = (%checkpoint-path id :project-root project-root
                                          :config config)
          unless (probe-file path)
            do (return path))))

(defun %session-snapshot-path (snapshot-id)
  (merge-pathnames (pathname (format nil "~A.sexp" snapshot-id))
                   (session-snapshot-directory)))

(defun %next-session-snapshot-path (&key timestamp)
  (let ((base-id (%checkpoint-id-from-time timestamp)))
    (loop for suffix from 0
          for id = (if (zerop suffix)
                       base-id
                       (format nil "~A-~D" base-id suffix))
          for path = (%session-snapshot-path id)
          unless (probe-file path)
            do (return path))))

(defun %checkpoint-sort-key (value)
  (string-downcase
   (cond
     ((keywordp value) (symbol-name value))
     ((symbolp value) (symbol-name value))
     (t (princ-to-string value)))))

(defun %checkpoint-encode-value (value)
  (cond
    ((stringp value)
     value)
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
    (t
     value)))

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
    (t
     value)))

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
    (error ()
      '())))

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

;;; --- MCP server config snapshot (1B) ---

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
          ;; Skip if a server with this name is already running
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

;;; --- Path approval snapshot (1C) ---

(defun %path-approvals->snapshot ()
  "Snapshot path approval memory using existing serialization."
  (%serialize-path-approval-memory))

(defun %restore-path-approvals-from-snapshot (snapshot)
  "Merge path approvals from a checkpoint snapshot into *path-approval-memory*."
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
    (list :session-id (conversation-state-session-id conversation)
          :state (conversation-state-state conversation)
          :created-at (conversation-state-created-at conversation)
          :updated-at (conversation-state-updated-at conversation)
          :active-fork (conversation-state-active-fork conversation)
          :fork-branch-point (conversation-state-fork-branch-point conversation)
          :forks (copy-tree (conversation-state-forks conversation))
          :entries (mapcar #'%conversation-entry->sexp
                           (conversation-state-entries conversation)))))

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
      (error ()
        nil))))

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

(defun %checkpoint-files (&key project-root config)
  (let* ((dir (checkpoint-directory :project-root project-root :config config))
         (core-pattern (merge-pathnames #P"*.core" dir))
         (sexp-pattern (merge-pathnames #P"*.sexp" dir)))
    (sort (append (copy-list (directory core-pattern))
                  (copy-list (directory sexp-pattern)))
          (lambda (left right)
            (let ((left-date (or (ignore-errors (file-write-date left)) 0))
                  (right-date (or (ignore-errors (file-write-date right)) 0)))
              (if (= left-date right-date)
                  (string> (namestring left) (namestring right))
                  (> left-date right-date)))))))

(defun checkpoint-max-count (&optional (config (current-config)))
  (let ((value (and (config-p config)
                    (config-value :auto-checkpoint-max-count config))))
    (if (and (integerp value) (> value 0))
        value
        *checkpoint-default-max-count*)))

(defun rotate-session-checkpoints (&key project-root config max-count)
  (let* ((resolved-config (or config (current-config)))
         (limit (or max-count (checkpoint-max-count resolved-config))))
    (when (or (null limit) (<= limit 0))
      (return-from rotate-session-checkpoints 0))
    (let* ((files (%checkpoint-files :project-root project-root
                                     :config resolved-config))
           (excess (nthcdr limit files))
           (deleted 0))
      (dolist (path excess)
        (handler-case
            (when (probe-file path)
              (delete-file path)
              (incf deleted))
          (error () nil)))
      deleted)))

(defun %checkpoint-created-at (payload path)
  (let ((created-at (and (listp payload) (getf payload :created-at))))
    (if (integerp created-at)
        created-at
        (or (ignore-errors (file-write-date path))
            (get-universal-time)))))

(defun %session-snapshot-files ()
  (let* ((dir (session-snapshot-directory))
         (pattern (merge-pathnames #P"*.sexp" dir)))
    (sort (copy-list (directory pattern))
          #'>
          :key (lambda (path)
                 (or (ignore-errors (file-write-date path)) 0)))))

(defun list-session-snapshots (&key limit)
  (let ((records '()))
    (dolist (path (%session-snapshot-files))
      (let* ((payload (%read-checkpoint-payload path))
             (snapshot-id (or (and (listp payload) (getf payload :snapshot-id))
                              (pathname-name path)))
             (created-at (%checkpoint-created-at payload path)))
        (push (make-session-checkpoint
               :id snapshot-id
               :path path
               :created-at created-at
               :auto-p nil
               :trigger :snapshot)
              records)))
    (let ((sorted (nreverse records)))
      (if (and (integerp limit) (>= limit 0))
          (subseq sorted 0 (min limit (length sorted)))
          sorted))))

(defun %resolve-session-snapshot-path (&key snapshot-id snapshot-path)
  (when snapshot-path
    (let ((resolved (probe-file snapshot-path)))
      (when resolved
        (return-from %resolve-session-snapshot-path resolved))))
  (let ((records (list-session-snapshots)))
    (when (null records)
      (return-from %resolve-session-snapshot-path nil))
    (cond
      ((or (null snapshot-id)
           (and (stringp snapshot-id)
                (zerop (length (%checkpoint-trim snapshot-id)))))
       (session-checkpoint-path (first records)))
      ((integerp snapshot-id)
       (let ((index (1- snapshot-id)))
         (and (>= index 0)
              (< index (length records))
              (session-checkpoint-path (nth index records)))))
      ((and (stringp snapshot-id)
            (ignore-errors (parse-integer snapshot-id)))
       (%resolve-session-snapshot-path
        :snapshot-id (parse-integer snapshot-id)))
      ((and (stringp snapshot-id)
            (probe-file snapshot-id))
       (probe-file snapshot-id))
      (t
       (let* ((needle (string-downcase (%checkpoint-trim (princ-to-string snapshot-id))))
              (match
                (find-if (lambda (entry)
                           (let* ((id (string-downcase
                                       (or (session-checkpoint-id entry) "")))
                                  (name (string-downcase
                                         (or (pathname-name
                                              (session-checkpoint-path entry))
                                             ""))))
                             (or (string= needle id)
                                 (string= needle name)
                                 (search needle id :test #'char=))))
                         records)))
         (and match (session-checkpoint-path match)))))))

(defun list-session-checkpoints (&key project-root config limit)
  (let ((records '()))
    (dolist (path (%checkpoint-files :project-root project-root :config config))
      (let* ((payload (%read-checkpoint-payload path))
             (checkpoint-id (or (and (listp payload) (getf payload :checkpoint-id))
                                (pathname-name path)))
             (created-at (%checkpoint-created-at payload path))
             (auto-p (and (listp payload) (not (null (getf payload :auto-p)))))
             (trigger (if (and (listp payload) (keywordp (getf payload :trigger)))
                          (getf payload :trigger)
                          :manual)))
        (push (make-session-checkpoint
               :id checkpoint-id
               :path path
               :created-at created-at
               :auto-p auto-p
               :trigger trigger)
              records)))
    (let ((sorted (nreverse records)))
      (if (and (integerp limit) (>= limit 0))
          (subseq sorted 0 (min limit (length sorted)))
          sorted))))

(defun %resolve-checkpoint-path (&key checkpoint-id checkpoint-path project-root config)
  (when checkpoint-path
    (let ((resolved (probe-file checkpoint-path)))
      (when resolved
        (return-from %resolve-checkpoint-path resolved))))
  (let ((records (list-session-checkpoints :project-root project-root :config config)))
    (when (null records)
      (return-from %resolve-checkpoint-path nil))
    (cond
      ((or (null checkpoint-id)
           (and (stringp checkpoint-id)
                (zerop (length (%checkpoint-trim checkpoint-id)))))
       (session-checkpoint-path (first records)))
      ((integerp checkpoint-id)
       (let ((index (1- checkpoint-id)))
         (and (>= index 0)
              (< index (length records))
              (session-checkpoint-path (nth index records)))))
      ((and (stringp checkpoint-id)
            (ignore-errors (parse-integer checkpoint-id)))
       (%resolve-checkpoint-path
        :checkpoint-id (parse-integer checkpoint-id)
        :project-root project-root
        :config config))
      ((and (stringp checkpoint-id)
            (probe-file checkpoint-id))
       (probe-file checkpoint-id))
      (t
       (let* ((needle (string-downcase (%checkpoint-trim (princ-to-string checkpoint-id))))
              (match
                (find-if (lambda (entry)
                           (let* ((id (string-downcase
                                       (or (session-checkpoint-id entry) "")))
                                  (name (string-downcase
                                         (or (pathname-name
                                              (session-checkpoint-path entry))
                                             ""))))
                             (or (string= needle id)
                                 (string= needle name)
                                 (search needle id :test #'char=))))
                         records)))
         (and match (session-checkpoint-path match)))))))

(defun %ensure-conversation-for-checkpoint (conversation project-root)
  (or (and (typep conversation 'conversation-state) conversation)
      (conversation-load-latest :project-root project-root)
      (make-conversation-state :project-root project-root)))

(defun %publish-session-checkpointed (event-bus checkpoint payload)
  (let* ((conversation (or (getf payload :conversation) '()))
         (extensions (or (getf payload :extensions) '()))
         (tools (or (getf payload :tools) '()))
         (memory (or (getf payload :memory) '())))
    (publish (or event-bus (current-event-bus))
             (make-session-checkpointed-event
              :checkpoint-id (session-checkpoint-id checkpoint)
              :path (namestring (session-checkpoint-path checkpoint))
              :trigger (session-checkpoint-trigger checkpoint)
              :auto-p (session-checkpoint-auto-p checkpoint)
              :message-count (length (or (getf conversation :entries) '()))
              :extension-count (length (or (getf extensions :loaded) '()))
              :tool-count (length (or (getf tools :definitions) '()))
              :memory-count (+ (length (or (getf memory :global) '()))
                               (length (or (getf memory :project) '()))
                               (length (or (getf memory :session) '())))))))

(defun %publish-session-restored (event-bus checkpoint payload)
  (let* ((conversation (or (getf payload :conversation) '()))
         (extensions (or (getf payload :extensions) '()))
         (tools (or (getf payload :tools) '()))
         (memory (or (getf payload :memory) '())))
    (publish (or event-bus (current-event-bus))
             (make-session-restored-event
              :checkpoint-id (session-checkpoint-id checkpoint)
              :path (namestring (session-checkpoint-path checkpoint))
              :trigger (session-checkpoint-trigger checkpoint)
              :message-count (length (or (getf conversation :entries) '()))
              :extension-count (length (or (getf extensions :loaded) '()))
              :tool-count (length (or (getf tools :definitions) '()))
              :memory-count (+ (length (or (getf memory :global) '()))
                               (length (or (getf memory :project) '()))
                               (length (or (getf memory :session) '())))))))

(defun save-session-snapshot (&key
                                conversation
                                memory-backend
                                project-root
                                snapshot-id
                                (timestamp (get-universal-time)))
  (unless (session-persistence-enabled-p)
    (return-from save-session-snapshot nil))
  (let* ((resolved-project-root (%checkpoint-project-root :project-root project-root))
         (resolved-conversation
           (%ensure-conversation-for-checkpoint conversation resolved-project-root))
         (resolved-memory-backend (or memory-backend (current-memory-backend)))
         (manual-id (let ((trimmed (%checkpoint-trim (or snapshot-id ""))))
                      (and (plusp (length trimmed)) trimmed)))
         (resolved-path (if manual-id
                            (%session-snapshot-path manual-id)
                            (%next-session-snapshot-path :timestamp timestamp)))
         (resolved-id (or manual-id (pathname-name resolved-path)))
         (payload (%session-snapshot-payload
                   :snapshot-id resolved-id
                   :created-at timestamp
                   :project-root resolved-project-root
                   :conversation (%conversation->snapshot resolved-conversation)
                   :memory (%memory->snapshot resolved-memory-backend)
                   :agents (%agents->snapshot))))
    (conversation-save resolved-conversation :save-manifest-p t :save-fork-file-p t)
    (%write-checkpoint-payload resolved-path payload)
    (make-session-checkpoint
     :id resolved-id
     :path resolved-path
     :created-at timestamp
     :auto-p nil
     :trigger :snapshot)))

(defun load-session-snapshot (&key
                                snapshot-id
                                snapshot-path
                                project-root
                                memory-backend)
  (let* ((resolved-project-root (%checkpoint-project-root :project-root project-root))
         (resolved-path (%resolve-session-snapshot-path
                         :snapshot-id snapshot-id
                         :snapshot-path snapshot-path)))
    (unless resolved-path
      (error "Session snapshot ~S not found." (or snapshot-id snapshot-path)))
    (let* ((payload (%read-checkpoint-payload resolved-path))
           (snapshot-id* (or (and (listp payload) (getf payload :snapshot-id))
                             (pathname-name resolved-path)))
           (created-at (%checkpoint-created-at payload resolved-path))
           (snapshot (make-session-checkpoint
                      :id snapshot-id*
                      :path resolved-path
                      :created-at created-at
                      :auto-p nil
                      :trigger :snapshot))
           (project-root*
             (uiop:ensure-directory-pathname
              (or project-root
                  (and (listp payload)
                       (stringp (getf payload :project-root))
                       (pathname (getf payload :project-root)))
                  resolved-project-root)))
           (conversation-snapshot (and (listp payload) (getf payload :conversation)))
           (memory-snapshot (and (listp payload) (getf payload :memory)))
           (agents-snapshot (and (listp payload) (getf payload :agents)))
           (restored-conversation (%conversation-from-snapshot conversation-snapshot
                                                              project-root*)))
      (when (null memory-backend)
        (setf *memory-backend* nil))
      (let ((restored-memory-backend (or memory-backend (current-memory-backend))))
        (%restore-memory-from-snapshot memory-snapshot restored-memory-backend)
        (%restore-agents-from-snapshot agents-snapshot)
        (checkpoint-mark-activity)
        (list :snapshot snapshot
              :conversation restored-conversation
              :memory-backend restored-memory-backend
              :agents (list-agents :include-completed-p t))))))

(defun checkpoint-session (&key
                             conversation
                             config
                             memory-backend
                             project-root
                             event-bus
                             (trigger :manual)
                             (auto-p nil)
                             (timestamp (get-universal-time)))
  (unless (session-persistence-enabled-p)
    (return-from checkpoint-session nil))
  (let* ((resolved-config (or config (current-config)))
         (resolved-project-root (%checkpoint-project-root
                                 :project-root project-root
                                 :config resolved-config))
         (resolved-conversation
           (%ensure-conversation-for-checkpoint conversation resolved-project-root))
         (resolved-memory-backend (or memory-backend (current-memory-backend)))
         (checkpoint-path (%next-checkpoint-path :project-root resolved-project-root
                                                 :config resolved-config
                                                 :timestamp timestamp))
         (checkpoint-id (pathname-name checkpoint-path))
         (payload (%checkpoint-payload
                   :checkpoint-id checkpoint-id
                   :created-at timestamp
                   :project-root resolved-project-root
                   :trigger trigger
                   :auto-p auto-p
                   :config (%config->snapshot resolved-config)
                   :conversation (%conversation->snapshot resolved-conversation)
                   :extensions (%extensions->snapshot)
                   :tools (%tools->snapshot)
                   :memory (%memory->snapshot resolved-memory-backend)
                   :agents (%agents->snapshot)
                   :mcp-servers (%mcp-servers->snapshot)
                   :path-approvals (%path-approvals->snapshot)))
         (checkpoint (make-session-checkpoint
                      :id checkpoint-id
                      :path checkpoint-path
                      :created-at timestamp
                      :auto-p auto-p
                      :trigger (if (keywordp trigger) trigger :manual))))
    (conversation-save resolved-conversation :save-manifest-p t :save-fork-file-p t)
    (%write-checkpoint-payload checkpoint-path payload)
    (rotate-session-checkpoints :project-root resolved-project-root
                                :config resolved-config
                                :max-count (checkpoint-max-count resolved-config))
    (when auto-p
      (setf *checkpoint-last-auto-checkpoint-at* timestamp))
    (%publish-session-checkpointed event-bus checkpoint payload)
    checkpoint))

(defstruct (restore-session-context
            (:constructor %make-restore-session-context
                (&key checkpoint-id checkpoint-path project-root config
                      memory-backend event-bus resolved-config
                      resolved-project-root resolved-path)))
  checkpoint-id
  checkpoint-path
  project-root
  config
  memory-backend
  event-bus
  resolved-config
  resolved-project-root
  resolved-path
  payload
  checkpoint
  effective-project-root
  restored-config
  restored-conversation
  restored-memory-backend)

(defun %restore-session-context (checkpoint-id checkpoint-path project-root
                                 config memory-backend event-bus)
  (let* ((resolved-config (or config (current-config)))
         (resolved-project-root (%checkpoint-project-root
                                 :project-root project-root
                                 :config resolved-config))
         (resolved-path (%resolve-checkpoint-path
                         :checkpoint-id checkpoint-id
                         :checkpoint-path checkpoint-path
                         :project-root resolved-project-root
                         :config resolved-config)))
    (%make-restore-session-context
     :checkpoint-id checkpoint-id
     :checkpoint-path checkpoint-path
     :project-root project-root
     :config config
     :memory-backend memory-backend
     :event-bus event-bus
     :resolved-config resolved-config
     :resolved-project-root resolved-project-root
     :resolved-path resolved-path)))

(defun %restore-session-resolved-path (context)
  (or (restore-session-context-resolved-path context)
      (error "Checkpoint ~S not found."
             (or (restore-session-context-checkpoint-id context)
                 (restore-session-context-checkpoint-path context)))))

(defun %restore-session-load-payload (context)
  (let* ((resolved-path (%restore-session-resolved-path context))
         (payload (%read-checkpoint-payload resolved-path)))
    (setf (restore-session-context-payload context) payload)
    payload))

(defun %restore-session-checkpoint-record (payload resolved-path)
  (let* ((checkpoint-id (or (and (listp payload) (getf payload :checkpoint-id))
                            (pathname-name resolved-path)))
         (created-at (%checkpoint-created-at payload resolved-path))
         (trigger (if (and (listp payload) (keywordp (getf payload :trigger)))
                      (getf payload :trigger)
                      :manual)))
    (make-session-checkpoint
     :id checkpoint-id
     :path resolved-path
     :created-at created-at
     :auto-p (and (listp payload) (not (null (getf payload :auto-p))))
     :trigger trigger)))

(defun %restore-session-effective-project-root (context)
  (let ((payload (restore-session-context-payload context)))
    (uiop:ensure-directory-pathname
     (or (restore-session-context-project-root context)
         (and (listp payload)
              (getf payload :project-root)
              (pathname (getf payload :project-root)))
         (restore-session-context-resolved-project-root context)))))

(defun %restore-session-config-snapshot (context)
  (let ((payload (restore-session-context-payload context)))
    (and (listp payload) (getf payload :config))))

(defun %restore-session-conversation-snapshot (context)
  (let ((payload (restore-session-context-payload context)))
    (and (listp payload) (getf payload :conversation))))

(defun %restore-session-extensions-snapshot (context)
  (let ((payload (restore-session-context-payload context)))
    (and (listp payload) (getf payload :extensions))))

(defun %restore-session-tools-snapshot (context)
  (let ((payload (restore-session-context-payload context)))
    (and (listp payload) (getf payload :tools))))

(defun %restore-session-memory-snapshot (context)
  (let ((payload (restore-session-context-payload context)))
    (and (listp payload) (getf payload :memory))))

(defun %restore-session-agents-snapshot (context)
  (let ((payload (restore-session-context-payload context)))
    (and (listp payload) (getf payload :agents))))

(defun %restore-session-mcp-servers-snapshot (context)
  (let ((payload (restore-session-context-payload context)))
    (and (listp payload) (getf payload :mcp-servers))))

(defun %restore-session-path-approvals-snapshot (context)
  (let ((payload (restore-session-context-payload context)))
    (and (listp payload) (getf payload :path-approvals))))

(defun %restore-session-core-state (context)
  (let* ((payload (%restore-session-load-payload context))
         (project-root (%restore-session-effective-project-root context)))
    (setf (restore-session-context-checkpoint context)
          (%restore-session-checkpoint-record payload
                                             (%restore-session-resolved-path context))
          (restore-session-context-effective-project-root context) project-root
          (restore-session-context-restored-config context)
          (%restore-config-from-snapshot (%restore-session-config-snapshot context)
                                         project-root)
          (restore-session-context-restored-conversation context)
          (%conversation-from-snapshot (%restore-session-conversation-snapshot context)
                                       project-root))
    context))

(defun %restore-session-extensions-and-tools (context)
  (let ((project-root (restore-session-context-effective-project-root context)))
    (%restore-extensions-from-snapshot (%restore-session-extensions-snapshot context)
                                       :project-root project-root)
    (%restore-tools-from-snapshot (%restore-session-tools-snapshot context))
    context))

(defun %restore-session-memory-backend (context)
  (setf *memory-backend* nil)
  (let ((memory-backend (or (restore-session-context-memory-backend context)
                            (current-memory-backend))))
    (%restore-memory-from-snapshot (%restore-session-memory-snapshot context)
                                   memory-backend)
    (setf (restore-session-context-restored-memory-backend context) memory-backend)
    context))

(defun %restore-session-finish (context)
  (%publish-session-restored (restore-session-context-event-bus context)
                             (restore-session-context-checkpoint context)
                             (restore-session-context-payload context))
  (checkpoint-mark-activity)
  (setf *checkpoint-last-auto-checkpoint-at* nil)
  (list :checkpoint (restore-session-context-checkpoint context)
        :config (restore-session-context-restored-config context)
        :conversation (restore-session-context-restored-conversation context)
        :memory-backend (restore-session-context-restored-memory-backend context)))

(defun restore-session (&key
                          checkpoint-id
                          checkpoint-path
                          project-root
                          config
                          memory-backend
                          event-bus)
  (let ((context (%restore-session-context checkpoint-id
                                           checkpoint-path
                                           project-root
                                           config
                                           memory-backend
                                           event-bus)))
    (%restore-session-core-state context)
    (%restore-session-extensions-and-tools context)
    (%restore-session-memory-backend context)
    ;; Restore agents, MCP server configs, and path approvals (1A/1B/1C).
    ;; Each guard handles backward compat with older checkpoints missing these keys.
    (let ((agents-snapshot (%restore-session-agents-snapshot context)))
      (when agents-snapshot
        (%restore-agents-from-snapshot agents-snapshot)))
    (let ((mcp-snapshot (%restore-session-mcp-servers-snapshot context)))
      (when mcp-snapshot
        (%restore-mcp-servers-from-snapshot mcp-snapshot)))
    (let ((approvals-snapshot (%restore-session-path-approvals-snapshot context)))
      (when approvals-snapshot
        (%restore-path-approvals-from-snapshot approvals-snapshot)))
    ;; Resume event journal if not already running (1D).
    (ignore-errors
      (when (and (fboundp 'start-event-journal)
                 (or (null *event-journal*)
                     (not (event-journal-running-p *event-journal*))))
        (start-event-journal
         :event-bus (or (restore-session-context-event-bus context)
                        (current-event-bus)))))
    (%restore-session-finish context)))

(defun checkpoint-auto-idle-seconds (&optional (config (current-config)))
  (let ((value (and (config-p config)
                    (config-value :auto-checkpoint-idle-seconds config))))
    (if (and (integerp value) (>= value 0))
        value
        1800)))

(defun checkpoint-mark-activity (&optional (timestamp (get-universal-time)))
  (setf *checkpoint-last-activity-at* timestamp)
  timestamp)

;;; ---------------------------------------------------------------------------
;;; Image Save/Restore (I99)
;;;
;;; Wraps sb-ext:save-lisp-and-die with pre-save cleanup and post-restore reinit.
;;; Supports image rotation (keep N most recent).
;;; ---------------------------------------------------------------------------

(defvar *image-directory-override* nil)
(defvar *image-max-count* 5
  "Maximum number of saved images to keep.")
(defvar *image-pre-save-hooks* '()
  "List of functions called before saving an image.")
(defvar *image-post-restore-hooks* '()
  "List of functions called after restoring an image.")
(defvar *image-fd-cleanup-hooks* '()
  "Functions called to close file descriptors before image save.")
(defvar *image-network-drain-hooks* '()
  "Functions called to drain network resources before image save.")
(defvar *image-agent-checkpoint-hooks* '()
  "Functions called to serialize agent state before image save.")
(defvar *image-terminal-snapshot-hooks* '()
  "Functions called to enrich terminal state snapshots before image save.")
(defvar *image-terminal-reopen-hooks* '()
  "Functions called to re-open terminal resources after restore.")
(defvar *image-mcp-reconnect-hooks* '()
  "Functions called to re-establish MCP connections after restore.")
(defvar *image-api-reauth-hooks* '()
  "Functions called to re-authenticate API clients after restore.")
(defvar *image-tracked-streams* '()
  "List of streams tracked for pre-save file-descriptor cleanup.")
(defvar *image-last-terminal-state* nil
  "Most recent terminal snapshot captured during pre-save cleanup.")
(defvar *image-last-pre-save-report* nil
  "Most recent pre-save cleanup report.")
(defvar *image-last-post-restore-report* nil
  "Most recent post-restore init report.")

(defun image-directory (&key project-root config)
  "Return the directory for saved images."
  (or (and *image-directory-override*
           (uiop:ensure-directory-pathname *image-directory-override*))
      (merge-pathnames #P".amoebum/images/"
                       (%checkpoint-project-root :project-root project-root
                                                  :config config))))

(defun %image-path (name &key project-root config)
  "Build image file path."
  (merge-pathnames (pathname (format nil "~A.core" name))
                   (image-directory :project-root project-root :config config)))

(defun %image-files (&key project-root config)
  "List existing image files, sorted newest first."
  (let* ((dir (image-directory :project-root project-root :config config))
         (pattern (merge-pathnames #P"*.core" dir)))
    (sort (copy-list (directory pattern))
          #'>
          :key (lambda (path)
                 (or (ignore-errors (file-write-date path)) 0)))))

(defun rotate-images (&key project-root config (max-count *image-max-count*))
  "Delete old images, keeping at most MAX-COUNT."
  (let* ((images (%image-files :project-root project-root :config config))
         (excess (nthcdr max-count images))
         (deleted 0))
    (dolist (old excess)
      (handler-case
          (progn (delete-file old) (incf deleted))
        (error () nil)))
    deleted))

(defun list-saved-images (&key project-root config)
  "Return alist of (name . path) for saved images."
  (mapcar (lambda (path)
            (cons (pathname-name path) path))
          (%image-files :project-root project-root :config config)))

(defun register-image-tracked-stream (stream)
  "Track STREAM so image pre-save cleanup can close it."
  (when (streamp stream)
    (pushnew stream *image-tracked-streams* :test #'eq))
  stream)

(defun %close-image-tracked-streams ()
  "Close tracked streams and drop closed entries from the registry."
  (let ((closed-count 0)
        (remaining '()))
    (dolist (stream *image-tracked-streams*)
      (cond
        ((not (streamp stream))
         nil)
        ((not (open-stream-p stream))
         nil)
        (t
         (handler-case
             (progn
               (close stream)
               (incf closed-count))
           (error ()
             (push stream remaining))))))
    (setf *image-tracked-streams* (nreverse remaining))
    closed-count))

(defun register-image-fd-cleanup-hook (fn)
  (pushnew fn *image-fd-cleanup-hooks* :test #'eq))

(defun register-image-network-drain-hook (fn)
  (pushnew fn *image-network-drain-hooks* :test #'eq))

(defun register-image-agent-checkpoint-hook (fn)
  (pushnew fn *image-agent-checkpoint-hooks* :test #'eq))

(defun register-image-terminal-snapshot-hook (fn)
  (pushnew fn *image-terminal-snapshot-hooks* :test #'eq))

(defun register-image-terminal-reopen-hook (fn)
  (pushnew fn *image-terminal-reopen-hooks* :test #'eq))

(defun register-image-mcp-reconnect-hook (fn)
  (pushnew fn *image-mcp-reconnect-hooks* :test #'eq))

(defun register-image-api-reauth-hook (fn)
  (pushnew fn *image-api-reauth-hooks* :test #'eq))

(defun %invoke-image-hook (hook label &key argument argument-supplied-p)
  (handler-case
      (if argument-supplied-p
          (handler-case
              (funcall hook argument)
            (program-error ()
              ;; Backward-compatibility: existing hooks are zero-arg.
              (funcall hook)))
          (funcall hook))
    (error (condition)
      (format *error-output* "~A hook error: ~A~%" label condition)
      :image-hook-error)))

(defun %run-image-hooks (hooks label &key argument argument-supplied-p collect-results-p)
  "Run HOOKS, returning successful hook count and optional results list."
  (let ((success-count 0)
        (results '()))
    (dolist (hook hooks)
      (let ((result (%invoke-image-hook hook
                                        label
                                        :argument argument
                                        :argument-supplied-p argument-supplied-p)))
        (unless (eq result :image-hook-error)
          (incf success-count)
          (when collect-results-p
            (push result results)))))
    (values success-count (nreverse results))))

(defun %hook-result->count (value)
  (if (and (integerp value) (>= value 0))
      value
      1))

(defun %run-image-counting-hooks (hooks label &key argument argument-supplied-p)
  (multiple-value-bind (success-count results)
      (%run-image-hooks hooks
                        label
                        :argument argument
                        :argument-supplied-p argument-supplied-p
                        :collect-results-p t)
    (if (null results)
        success-count
        (reduce #'+ results :initial-value 0 :key #'%hook-result->count))))

(defun %default-terminal-state-snapshot ()
  (list :captured-at (get-universal-time)
        :term (uiop:getenv "TERM")
        :columns (uiop:getenv "COLUMNS")
        :lines (uiop:getenv "LINES")
        :cwd (ignore-errors (uiop:getcwd))))

(defun %capture-terminal-state ()
  (let* ((base (%default-terminal-state-snapshot))
         (hook-results
           (nth-value 1
                      (%run-image-hooks *image-terminal-snapshot-hooks*
                                        "terminal-snapshot"
                                        :argument base
                                        :argument-supplied-p t
                                        :collect-results-p t))))
    (if hook-results
        (append base (list :hook-states hook-results))
        base)))

(defun %emit-system-restored-event (event-bus report)
  (publish (or event-bus (current-event-bus))
           (make-event :type "system:restored"
                       :source :amoebum
                       :severity :info
                       :payload report)))

(defun %image-pre-save-cleanup ()
  "Perform cleanup before saving an image."
  (let* ((fd-cleanup-count
           (%run-image-counting-hooks *image-fd-cleanup-hooks*
                                      "pre-save/fd-cleanup"))
         (network-drain-count
           (%run-image-counting-hooks *image-network-drain-hooks*
                                      "pre-save/network-drain"))
         (agent-checkpoint-count
           (%run-image-counting-hooks *image-agent-checkpoint-hooks*
                                      "pre-save/agent-checkpoint"))
         (terminal-state (%capture-terminal-state))
         (extension-hook-count
           (%run-image-counting-hooks *image-pre-save-hooks* "pre-save/extension"))
         (report (list :captured-at (get-universal-time)
                       :fd-cleanup-count fd-cleanup-count
                       :network-drain-count network-drain-count
                       :agent-checkpoint-count agent-checkpoint-count
                       :terminal-state terminal-state
                       :extension-hook-count extension-hook-count)))
    (setf *image-last-terminal-state* terminal-state
          *image-last-pre-save-report* report)
    report))

(defun %image-post-restore-init (&key event-bus terminal-state)
  "Reinitialize after restoring an image."
  ;; Tune SBCL GC: enlarge nursery so short-lived render-loop objects
  ;; die in Gen 0 instead of being promoted to Gen 1.
  #+sbcl
  (setf (sb-ext:bytes-consed-between-gcs) (* 64 1024 1024))  ; 64 MB nursery
  (let* ((resolved-terminal-state (or terminal-state *image-last-terminal-state*))
         (terminal-reopen-count
           (%run-image-counting-hooks *image-terminal-reopen-hooks*
                                      "post-restore/terminal-reopen"
                                      :argument resolved-terminal-state
                                      :argument-supplied-p t))
         (mcp-reconnect-count
           (%run-image-counting-hooks *image-mcp-reconnect-hooks*
                                      "post-restore/mcp-reconnect"))
         (api-reauth-count
           (%run-image-counting-hooks *image-api-reauth-hooks*
                                      "post-restore/api-reauth"))
         (report (list :restored-at (get-universal-time)
                       :terminal-state resolved-terminal-state
                       :terminal-reopen-count terminal-reopen-count
                       :mcp-reconnect-count mcp-reconnect-count
                       :api-reauth-count api-reauth-count)))
    (%emit-system-restored-event event-bus report)
    (let ((extension-hook-count
            (%run-image-counting-hooks *image-post-restore-hooks*
                                       "post-restore/extension"
                                       :argument report
                                       :argument-supplied-p t)))
      (setf report (append report (list :extension-hook-count extension-hook-count)))
      (setf *image-last-post-restore-report* report)
      report)))

(defstruct (save-amoebum-image-request
            (:constructor make-save-amoebum-image-request
                (&key path project-root config name toplevel-fn
                      (rotate-p t) resolved-name resolved-path)))
  path
  project-root
  config
  name
  toplevel-fn
  (rotate-p t)
  resolved-name
  resolved-path)

(defun %resolve-save-amoebum-image-request (request)
  (let ((resolved-name (or (save-amoebum-image-request-name request)
                           (%checkpoint-id-from-time))))
    (setf (save-amoebum-image-request-resolved-name request) resolved-name
          (save-amoebum-image-request-resolved-path request)
          (or (save-amoebum-image-request-path request)
              (%image-path resolved-name
                           :project-root (save-amoebum-image-request-project-root request)
                           :config (save-amoebum-image-request-config request))))
    request))

(defun %prepare-save-amoebum-image (request)
  (ensure-directories-exist (save-amoebum-image-request-resolved-path request))
  (%image-pre-save-cleanup)
  request)

(defun %checkpoint-before-image-save (request)
  (handler-case
      (checkpoint-session :project-root (save-amoebum-image-request-project-root request)
                          :config (save-amoebum-image-request-config request)
                          :trigger :image-save
                          :auto-p nil)
    (error () nil))
  request)

(defun %rotate-images-before-save (request)
  (when (save-amoebum-image-request-rotate-p request)
    (rotate-images :project-root (save-amoebum-image-request-project-root request)
                   :config (save-amoebum-image-request-config request)))
  request)

(defun %default-restored-image-toplevel ()
  (lambda ()
    (%image-post-restore-init)
    #+sbcl
    (handler-case
        (handler-bind
            ((serious-condition
               (lambda (c)
                 ;; Skip interactive-interrupt — let the outer handler-case
                 ;; catch it and exit cleanly with code 0.
                 (unless (typep c 'sb-sys:interactive-interrupt)
                   ;; Capture backtrace BEFORE stack unwinds
                   (ignore-errors
                     (let ((bt (with-output-to-string (s)
                                 (sb-debug:print-backtrace :stream s :count 40))))
                       (log-runtime-condition
                        c
                        :kind "restore-error"
                        :source :checkpoint
                        :message "Uncaught condition in restored Amoebum image."
                        :details (list :condition-type (type-of c)
                                       :argv (rest sb-ext:*posix-argv*))
                        :include-backtrace-p nil)
                       ;; Write full backtrace separately
                       (with-open-file (f (merge-pathnames ".amoebum/runtime/full-backtrace.log"
                                                           (user-homedir-pathname))
                                          :direction :output :if-exists :supersede
                                          :if-does-not-exist :create)
                         (format f "Condition: ~A~%Type: ~A~%~%~A~%" c (type-of c) bt)
                         (finish-output f))))
                   (ignore-errors
                     (format *error-output* "Restore error (~A): ~A~%" (type-of c) c)
                     (format *error-output* "Crash log: ~A~%"
                             (namestring (crash-log-path)))
                     (write-line "Full backtrace: ~/.amoebum/runtime/full-backtrace.log"
                                 *error-output*))
                   (sb-ext:exit :code 1 :abort t)))))
          (progn
            (main)
            (sb-ext:exit :code 0 :abort t)))
      (sb-sys:interactive-interrupt ()
        (sb-ext:exit :code 0 :abort t)))
    #-sbcl
    (progn
      (main)
      (uiop:quit 0))))

(defun %save-amoebum-image-request (request)
  #+sbcl
  (progn
    ;; Compact the heap before save so --dynamic-space-size can shrink it at runtime.
    (sb-ext:gc :full t)
    (sb-ext:save-lisp-and-die
     (save-amoebum-image-request-resolved-path request)
     :toplevel (or (save-amoebum-image-request-toplevel-fn request)
                   (%default-restored-image-toplevel))
     :executable t
     :purify t))
  #-sbcl
  (error "Image save/restore requires SBCL (sb-ext:save-lisp-and-die).")
  (save-amoebum-image-request-resolved-path request))

(defun save-amoebum-image (&key path project-root config
                                (name nil)
                                (toplevel-fn nil)
                                (rotate-p t))
  "Save the current Lisp image to PATH.
Pre-save cleanup runs before save; post-restore init runs on resume.
If ROTATE-P, old images are pruned."
  (let ((request (%resolve-save-amoebum-image-request
                  (make-save-amoebum-image-request
                   :path path
                   :project-root project-root
                   :config config
                   :name name
                   :toplevel-fn toplevel-fn
                   :rotate-p rotate-p))))
    (%prepare-save-amoebum-image request)
    (%checkpoint-before-image-save request)
    (%rotate-images-before-save request)
    (%save-amoebum-image-request request)))

(defun register-image-pre-save-hook (fn)
  "Register a function to call before saving an image."
  (pushnew fn *image-pre-save-hooks* :test #'eq))

(defun register-image-post-restore-hook (fn)
  "Register a function to call after restoring an image."
  (pushnew fn *image-post-restore-hooks* :test #'eq))

(eval-when (:load-toplevel :execute)
  (register-image-fd-cleanup-hook #'%close-image-tracked-streams))

(defun maybe-auto-checkpoint (&key
                                conversation
                                config
                                memory-backend
                                project-root
                                event-bus
                                (busy-p nil)
                                (timestamp (get-universal-time)))
  (let* ((resolved-config (or config (current-config)))
         (interval (checkpoint-auto-idle-seconds resolved-config)))
    (when (or busy-p (<= interval 0))
      (return-from maybe-auto-checkpoint nil))
    (when (and (integerp *checkpoint-last-activity-at*)
               (>= (- timestamp *checkpoint-last-activity-at*) interval)
               (or (null *checkpoint-last-auto-checkpoint-at*)
                   (>= (- timestamp *checkpoint-last-auto-checkpoint-at*) interval)))
      (checkpoint-session :conversation conversation
                          :config resolved-config
                          :memory-backend memory-backend
                          :project-root project-root
                          :event-bus event-bus
                          :trigger :idle
                          :auto-p t
                          :timestamp timestamp))))
