(in-package :amoebum)

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
        (push (%checkpoint-session-record checkpoint-id
                                          path
                                          created-at
                                          :auto-p auto-p
                                          :trigger trigger)
              records)))
    (%checkpoint-limit-records (nreverse records) limit)))

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
    ;; Each guard preserves backward compatibility with older checkpoints.
    (let ((agents-snapshot (%restore-session-agents-snapshot context)))
      (when agents-snapshot
        (%restore-agents-from-snapshot agents-snapshot)))
    (let ((mcp-snapshot (%restore-session-mcp-servers-snapshot context)))
      (when mcp-snapshot
        (%restore-mcp-servers-from-snapshot mcp-snapshot)))
    (let ((approvals-snapshot (%restore-session-path-approvals-snapshot context)))
      (when approvals-snapshot
        (%restore-path-approvals-from-snapshot approvals-snapshot)))
    (ignore-errors
      (when (and (fboundp 'start-event-journal)
                 (or (null *event-journal*)
                     (not (event-journal-running-p *event-journal*))))
        (start-event-journal
         :event-bus (or (restore-session-context-event-bus context)
                        (current-event-bus)))))
    (%restore-session-finish context)))
