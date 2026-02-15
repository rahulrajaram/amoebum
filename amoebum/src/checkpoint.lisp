(in-package :amoebum)

(defparameter *checkpoint-directory-override* nil)
(defparameter *checkpoint-last-activity-at* (get-universal-time))
(defparameter *checkpoint-last-auto-checkpoint-at* nil)
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

(defun %checkpoint-id-from-time (&optional (timestamp (get-universal-time)))
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time timestamp 0)
    (format nil "~4,'0D~2,'0D~2,'0DT~2,'0D~2,'0D~2,'0DZ"
            year month day hour minute second)))

(defun %checkpoint-path (checkpoint-id &key project-root config)
  (merge-pathnames (pathname (format nil "~A.sexp" checkpoint-id))
                   (checkpoint-directory :project-root project-root
                                         :config config)))

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
     (coerce (mapcar #'%checkpoint-decode-value (rest value)) 'vector))
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
          :defined-at (tool-metadata-defined-at metadata))))

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
   :defined-at (getf snapshot :defined-at)))

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
                              memory)
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
        :memory memory))

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

(defun %checkpoint-files (&key project-root config)
  (let* ((dir (checkpoint-directory :project-root project-root :config config))
         (pattern (merge-pathnames #P"*.sexp" dir)))
    (sort (copy-list (directory pattern))
          #'>
          :key (lambda (path)
                 (or (ignore-errors (file-write-date path)) 0)))))

(defun %checkpoint-created-at (payload path)
  (let ((created-at (and (listp payload) (getf payload :created-at))))
    (if (integerp created-at)
        created-at
        (or (ignore-errors (file-write-date path))
            (get-universal-time)))))

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

(defun checkpoint-session (&key
                             conversation
                             config
                             memory-backend
                             project-root
                             event-bus
                             (trigger :manual)
                             (auto-p nil)
                             (timestamp (get-universal-time)))
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
                   :memory (%memory->snapshot resolved-memory-backend)))
         (checkpoint (make-session-checkpoint
                      :id checkpoint-id
                      :path checkpoint-path
                      :created-at timestamp
                      :auto-p auto-p
                      :trigger (if (keywordp trigger) trigger :manual))))
    (conversation-save resolved-conversation :save-manifest-p t :save-fork-file-p t)
    (%write-checkpoint-payload checkpoint-path payload)
    (when auto-p
      (setf *checkpoint-last-auto-checkpoint-at* timestamp))
    (%publish-session-checkpointed event-bus checkpoint payload)
    checkpoint))

(defun restore-session (&key
                          checkpoint-id
                          checkpoint-path
                          project-root
                          config
                          memory-backend
                          event-bus)
  (let* ((resolved-config (or config (current-config)))
         (resolved-project-root (%checkpoint-project-root
                                 :project-root project-root
                                 :config resolved-config))
         (resolved-path (%resolve-checkpoint-path
                         :checkpoint-id checkpoint-id
                         :checkpoint-path checkpoint-path
                         :project-root resolved-project-root
                         :config resolved-config)))
    (unless resolved-path
      (error "Checkpoint ~S not found." (or checkpoint-id checkpoint-path)))
    (let* ((payload (%read-checkpoint-payload resolved-path))
           (checkpoint-id* (or (and (listp payload) (getf payload :checkpoint-id))
                               (pathname-name resolved-path)))
           (created-at (%checkpoint-created-at payload resolved-path))
           (trigger (if (and (listp payload) (keywordp (getf payload :trigger)))
                        (getf payload :trigger)
                        :manual))
           (checkpoint (make-session-checkpoint
                        :id checkpoint-id*
                        :path resolved-path
                        :created-at created-at
                        :auto-p (and (listp payload) (not (null (getf payload :auto-p))))
                        :trigger trigger))
           (project-root*
             (uiop:ensure-directory-pathname
              (or project-root
                  (and (listp payload)
                       (getf payload :project-root)
                       (pathname (getf payload :project-root)))
                  resolved-project-root)))
           (config-snapshot (and (listp payload) (getf payload :config)))
           (conversation-snapshot (and (listp payload) (getf payload :conversation)))
           (extensions-snapshot (and (listp payload) (getf payload :extensions)))
           (tools-snapshot (and (listp payload) (getf payload :tools)))
           (memory-snapshot (and (listp payload) (getf payload :memory)))
           (restored-config (%restore-config-from-snapshot config-snapshot project-root*))
           (restored-conversation (%conversation-from-snapshot conversation-snapshot
                                                              project-root*)))
      (%restore-extensions-from-snapshot extensions-snapshot :project-root project-root*)
      (%restore-tools-from-snapshot tools-snapshot)
      (setf *memory-backend* nil)
      (let ((restored-memory-backend (or memory-backend (current-memory-backend))))
        (%restore-memory-from-snapshot memory-snapshot restored-memory-backend)
        (%publish-session-restored event-bus checkpoint payload)
        (checkpoint-mark-activity)
        (setf *checkpoint-last-auto-checkpoint-at* nil)
        (list :checkpoint checkpoint
              :config restored-config
              :conversation restored-conversation
              :memory-backend restored-memory-backend)))))

(defun checkpoint-auto-idle-seconds (&optional (config (current-config)))
  (let ((value (and (config-p config)
                    (config-value :auto-checkpoint-idle-seconds config))))
    (if (and (integerp value) (>= value 0))
        value
        300)))

(defun checkpoint-mark-activity (&optional (timestamp (get-universal-time)))
  (setf *checkpoint-last-activity-at* timestamp)
  timestamp)

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
