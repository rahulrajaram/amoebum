(in-package :amoebum)

(defclass memory-backend () ())

(defclass file-memory-backend (memory-backend)
  ((global-path
    :initarg :global-path
    :reader file-memory-backend-global-path)
   (project-path
    :initarg :project-path
    :reader file-memory-backend-project-path)
   (project-root
    :initarg :project-root
    :reader file-memory-backend-project-root)))

(defstruct (memory-entry
            (:constructor make-memory-entry
                (&key key value scope source
                 (created-at (get-universal-time)))))
  key
  value
  scope
  source
  created-at)

(defstruct (memory-candidate
            (:constructor make-memory-candidate
                (&key kind text key confidence)))
  kind
  text
  key
  confidence)

(defparameter *memory-backend* nil)
(defparameter *session-memory-entries* '())
(defparameter *memory-editor-runner* nil)

(defgeneric memory-backend-kind (backend))
(defgeneric memory-store (backend key value &key scope source))
(defgeneric memory-query (backend query &key scope limit))
(defgeneric memory-list (backend &key scope))
(defgeneric memory-delete (backend key &key scope))
(defgeneric memory-forget (backend &key scope))

(defun %trim-text (text)
  (if (stringp text)
      (string-trim '(#\Space #\Tab #\Newline #\Return) text)
      ""))

(defun %string-prefix-p-ci (prefix text)
  (let ((prefix-len (length prefix))
        (text-len (length text)))
    (and (<= prefix-len text-len)
         (string-equal prefix text :end2 prefix-len))))

(defun %string-suffix-p-ci (suffix text)
  (let ((suffix-len (length suffix))
        (text-len (length text)))
    (and (<= suffix-len text-len)
         (string-equal suffix text :start2 (- text-len suffix-len)))))

(defun %collapse-whitespace (text)
  (let ((trimmed (%trim-text text)))
    (with-output-to-string (out)
      (loop with in-space = nil
            for char across trimmed do
              (if (member char '(#\Space #\Tab #\Newline #\Return) :test #'char=)
                  (unless in-space
                    (write-char #\Space out)
                    (setf in-space t))
                  (progn
                    (write-char char out)
                    (setf in-space nil)))))))

(defun %normalize-memory-key (text)
  (let* ((collapsed (%collapse-whitespace text))
         (downcased (string-downcase collapsed)))
    (if (zerop (length downcased))
        (format nil "entry-~D" (get-universal-time))
        (let ((normalized
                (with-output-to-string (out)
                  (loop with pending-separator = nil
                        for char across downcased do
                          (cond
                            ((or (alphanumericp char) (char= char #\_))
                             (when pending-separator
                               (write-char #\- out)
                               (setf pending-separator nil))
                             (write-char char out))
                            (t
                             (setf pending-separator t)))))))
          (if (zerop (length normalized))
              (format nil "entry-~D" (get-universal-time))
              normalized)))))

(defun %default-global-memory-path ()
  (merge-pathnames #P".amoebum/memory/MEMORY.md" (user-homedir-pathname)))

(defun %default-project-memory-path (&optional project-root)
  (let* ((cfg (or (ignore-errors (current-config)) nil))
         (root (or project-root
                   (and cfg (config-project-root cfg))
                   *default-pathname-defaults*)))
    (merge-pathnames #P".amoebum/MEMORY.md" (uiop:ensure-directory-pathname root))))

(defun make-file-memory-backend (&key global-path project-path project-root)
  (make-instance 'file-memory-backend
                 :global-path (or global-path (%default-global-memory-path))
                 :project-path (or project-path (%default-project-memory-path project-root))
                 :project-root (uiop:ensure-directory-pathname
                                (or project-root
                                    (make-pathname :name nil
                                                   :type nil
                                                   :defaults (or project-path
                                                                 (%default-project-memory-path)))))))

(defun reset-memory-backend (&optional backend)
  (setf *memory-backend* backend))

(defmethod memory-backend-kind ((backend memory-backend))
  (declare (ignore backend))
  :unknown)

(defmethod memory-backend-kind ((backend file-memory-backend))
  (declare (ignore backend))
  :file)

(defun %call-if-fbound (symbol &rest args)
  (when (fboundp symbol)
    (apply (symbol-function symbol) args)))

(defun %configured-memory-backend (cfg)
  (let ((configured (or (and cfg (config-memory-backend cfg))
                        (and cfg (config-value :memory-backend cfg))
                        :auto)))
    (if (keywordp configured)
        configured
        (intern (string-upcase (princ-to-string configured)) :keyword))))

(defun %autodetect-haake-enabled-p (cfg)
  (let ((value (and cfg (config-value :haake-autodetect cfg))))
    (if (null value)
        t
        (not (null value)))))

(defun %make-file-backend-from-config (cfg)
  (make-file-memory-backend
   :project-root (and cfg (config-project-root cfg))))

(defun %make-haake-backend-from-config (cfg)
  (%call-if-fbound 'make-haake-cli-memory-backend
                   :command (or (and cfg (config-value :haake-command cfg))
                                "haake")
                   :project-id (and cfg (config-value :haake-project-id cfg))
                   :agent (or (and cfg (config-value :haake-agent cfg))
                              "amoebum")
                   :project-root (and cfg (config-project-root cfg))))

(defun %haake-cli-available-from-config-p (cfg)
  (if (fboundp 'haake-cli-available-p)
      (funcall (symbol-function 'haake-cli-available-p)
               :command (or (and cfg (config-value :haake-command cfg))
                            "haake"))
      nil))

(defun %haake-cli-status-ok-from-config-p (cfg)
  (if (fboundp 'haake-cli-status-ok-p)
      (funcall (symbol-function 'haake-cli-status-ok-p)
               :command (or (and cfg (config-value :haake-command cfg))
                            "haake")
               :directory (and cfg (config-project-root cfg)))
      nil))

(defun %haake-cli-compatible-from-config-p (cfg)
  (if (fboundp 'haake-cli-compatible-p)
      (funcall (symbol-function 'haake-cli-compatible-p)
               :command (or (and cfg (config-value :haake-command cfg))
                            "haake")
               :directory (and cfg (config-project-root cfg)))
      nil))

(defun %resolve-memory-backend (&optional (cfg (current-config)))
  (let ((requested (%configured-memory-backend cfg)))
    (labels ((select-file (reason)
               (values (%make-file-backend-from-config cfg) reason requested))
             (select-haake (reason unavailable-reason)
               (cond
                 ((not (%haake-cli-available-from-config-p cfg))
                  (select-file unavailable-reason))
                 ((not (%haake-cli-status-ok-from-config-p cfg))
                  (select-file :haake-status-unavailable))
                 ((not (%haake-cli-compatible-from-config-p cfg))
                  (select-file :haake-cli-incompatible))
                 (t
                  (let ((backend (%make-haake-backend-from-config cfg)))
                    (if backend
                        (values backend reason requested)
                        (select-file :haake-backend-instantiation-failed)))))))
      (case requested
        (:file
         (select-file :configured-file))
        (:haake-cli
         (select-haake :configured-haake-cli :haake-cli-unavailable))
        (:haake-mcp
         (select-file :haake-mcp-not-implemented))
        (:auto
         (if (%autodetect-haake-enabled-p cfg)
             (select-haake :auto-detected-haake-cli :haake-cli-not-found)
             (select-file :haake-autodetect-disabled)))
        (otherwise
         (select-file :unknown-memory-backend-configured))))))

(defun %publish-memory-backend-selected (backend reason requested-backend)
  (publish (current-event-bus)
           (make-memory-backend-selected-event
            :backend (memory-backend-kind backend)
            :reason reason
            :requested-backend requested-backend)))

(defun current-memory-backend ()
  (or *memory-backend*
      (multiple-value-bind (backend reason requested)
          (%resolve-memory-backend)
        (setf *memory-backend* backend)
        (%publish-memory-backend-selected backend reason requested)
        backend)))

(defun file-memory-backend-p (value)
  (typep value 'file-memory-backend))

(defun session-memory-entries ()
  (copy-list *session-memory-entries*))

(defun %event-backend-name (backend)
  (memory-backend-kind backend))

(defun %publish-memory-updated (backend operation key value)
  (publish (current-event-bus)
           (make-memory-updated-event :backend (%event-backend-name backend)
                                      :operation operation
                                      :key key
                                      :value value)))

(defun %memory-path-for-scope (backend scope)
  (check-type backend file-memory-backend)
  (case scope
    (:global (file-memory-backend-global-path backend))
    (:project (file-memory-backend-project-path backend))
    (otherwise
     (error "Unsupported file memory scope ~S." scope))))

(defun %parse-memory-line (line scope source)
  (let ((trimmed (%trim-text line)))
    (cond
      ((or (zerop (length trimmed))
           (%string-prefix-p-ci "#" trimmed))
       nil)
      ((and (> (length trimmed) 4)
            (%string-prefix-p-ci "- [" trimmed))
       (let ((end (position #\] trimmed :start 3)))
         (when end
           (let* ((key (%trim-text (subseq trimmed 3 end)))
                  (rest (%trim-text (subseq trimmed (1+ end))))
                  (value (if (%string-prefix-p-ci ":" rest)
                             (%trim-text (subseq rest 1))
                             rest)))
             (when (plusp (length value))
               (make-memory-entry :key (if (plusp (length key))
                                           key
                                           (%normalize-memory-key value))
                                  :value value
                                  :scope scope
                                  :source source))))))
      ((and (> (length trimmed) 2)
            (%string-prefix-p-ci "- " trimmed))
       (let ((value (%trim-text (subseq trimmed 2))))
         (when (plusp (length value))
           (make-memory-entry :key (%normalize-memory-key value)
                              :value value
                              :scope scope
                              :source source))))
      (t nil))))

(defun %parse-memory-import-line (line)
  (let ((trimmed (%trim-text line)))
    (when (and (plusp (length trimmed))
               (char= (char trimmed 0) #\@))
      (let ((target (%trim-text (subseq trimmed 1))))
        (when (plusp (length target))
          (if (and (> (length target) 1)
                   (member (char target 0) '(#\" #\'))
                   (char= (char target 0) (char target (1- (length target)))))
              (subseq target 1 (1- (length target)))
              target))))))

(defun %resolve-memory-import-path (base-path import-spec)
  (let* ((base-directory (make-pathname :name nil
                                        :type nil
                                        :defaults base-path))
         (candidate (ignore-errors (merge-pathnames import-spec base-directory))))
    (and candidate
         (ignore-errors (probe-file candidate)))))

(defun %sort-memory-entries (entries)
  (sort (copy-list entries)
        #'string<
        :key (lambda (entry) (memory-entry-key entry))))

(defun %dedupe-memory-entries (entries)
  (let ((table (make-hash-table :test #'equal)))
    (dolist (entry entries)
      (setf (gethash (memory-entry-key entry) table) entry))
    (%sort-memory-entries
     (loop for entry being the hash-values of table collect entry))))

(defun %ensure-memory-file-header (path)
  (ensure-directories-exist path)
  (unless (probe-file path)
    (with-open-file (stream path
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (write-line "# Amoebum Memory" stream)
      (write-line "# Format: - [key] value" stream)
      (write-line "" stream))))

(defun %memory-entry-line (entry)
  (format nil "- [~A] ~A"
          (memory-entry-key entry)
          (memory-entry-value entry)))

(defun %write-memory-file (path entries)
  (%ensure-memory-file-header path)
  (with-open-file (stream path
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (write-line "# Amoebum Memory" stream)
    (write-line "# Format: - [key] value" stream)
    (write-line "" stream)
    (dolist (entry (%sort-memory-entries entries))
      (write-line (%memory-entry-line entry) stream)))
  path)

(defun %entries-by-key-table (entries)
  (let ((table (make-hash-table :test #'equal)))
    (dolist (entry entries)
      (setf (gethash (memory-entry-key entry) table) entry))
    table))

(defun %project-topic-memory-index-path (project-root)
  (merge-pathnames #P".amoebum/memory/MEMORY.md"
                   (uiop:ensure-directory-pathname project-root)))

(defun %resolved-path-equal-p (left right)
  (let ((left* (and left (ignore-errors (probe-file left))))
        (right* (and right (ignore-errors (probe-file right)))))
    (and left*
         right*
         (string= (coerce-path-string left*)
                  (coerce-path-string right*)))))

(defun %memory-source-scope-for-path (backend path &optional default-scope)
  (let* ((resolved (and path (ignore-errors (probe-file path))))
         (project-root (%memory-project-root backend))
         (topic-directory (%topic-memory-directory project-root))
         (source-path (and resolved (coerce-path-string resolved))))
    (cond
      ((and resolved
            (%resolved-path-equal-p resolved
                                    (file-memory-backend-global-path backend)))
       :global)
      ((and resolved
            (%resolved-path-equal-p resolved
                                    (file-memory-backend-project-path backend)))
       :project)
      ((and source-path
            (%path-under-directory-p source-path
                                     (coerce-path-string topic-directory)))
       (list :topic (%normalize-topic-name-from-path resolved)))
      (t
       default-scope))))

(defun %read-memory-file-data (path backend &key default-scope seen-paths)
  (let* ((resolved (and path (ignore-errors (probe-file path))))
         (seen (or seen-paths (make-hash-table :test #'equal))))
    (cond
      ((not resolved)
       (values '() '()))
      (t
       (let ((source-path (coerce-path-string resolved)))
         (if (gethash source-path seen)
             (values '() '())
             (let ((scope (%memory-source-scope-for-path backend resolved default-scope))
                   (entries '())
                   (local-entries '())
                   (source-specs '()))
               (setf (gethash source-path seen) t)
               (with-open-file (stream resolved :direction :input)
                 (loop for line = (read-line stream nil nil)
                       while line do
                         (let ((import-spec (%parse-memory-import-line line)))
                           (cond
                             (import-spec
                              (multiple-value-bind (imported-entries imported-source-specs)
                                  (%read-memory-file-data
                                   (%resolve-memory-import-path resolved import-spec)
                                   backend
                                   :default-scope scope
                                   :seen-paths seen)
                                (setf entries (append entries imported-entries)
                                      source-specs (append source-specs imported-source-specs))))
                             (t
                              (let ((parsed (%parse-memory-line line scope source-path)))
                                (when parsed
                                  (setf entries (append entries (list parsed))
                                        local-entries (append local-entries (list parsed)))))))))
               (values entries
                       (append
                        (list (list :path resolved
                                    :source-path source-path
                                    :scope scope
                                    :entries (%sort-memory-entries local-entries)))
                        source-specs))))))))))

(defun %read-memory-file (path scope source)
  (if (and path (probe-file path))
      (with-open-file (stream path :direction :input)
        (loop for line = (read-line stream nil nil)
              while line
              for parsed = (%parse-memory-line line scope source)
              when parsed
                collect parsed))
      '()))

(defun %memory-source-top-level-paths (backend scope)
  (let* ((project-root (%memory-project-root backend))
         (topic-index (%project-topic-memory-index-path project-root))
         (topic-files (%topic-memory-files project-root)))
    (remove nil
            (case scope
              (:global
               (list (file-memory-backend-global-path backend)))
              (:project
               (list (file-memory-backend-project-path backend)))
              (:topics
               (append (list topic-index) topic-files))
              (:effective
               (append (list (file-memory-backend-global-path backend)
                             topic-index)
                       topic-files
                       (list (file-memory-backend-project-path backend))))
              (otherwise
               '())))))

(defun %load-memory-source-data (backend scope)
  (let ((seen (make-hash-table :test #'equal))
        (entries '())
        (sources '()))
    (dolist (path (%memory-source-top-level-paths backend scope))
      (multiple-value-bind (path-entries path-sources)
          (%read-memory-file-data path backend :seen-paths seen)
        (setf entries (append entries path-entries)
              sources (append sources path-sources))))
    (values entries sources)))

(defun %file-memory-source-specs (backend &key (scope :effective))
  (nth-value 1 (%load-memory-source-data backend scope)))

(defun %memory-source-scope-label (scope)
  (case scope
    (:global "global")
    (:project "project")
    (:session "session")
    (otherwise
     (%entry-scope-signature scope))))

(defun %memory-entry-source-label (entry)
  (let ((source (memory-entry-source entry)))
    (cond
      ((pathnamep source)
       (namestring source))
      ((and (stringp source)
            (plusp (length (%trim-text source))))
       source)
      (t
       (%memory-source-scope-label (memory-entry-scope entry))))))

(defun %memory-entry-display-line (entry)
  (format nil "- [~A] ~A [source: ~A]"
          (memory-entry-key entry)
          (memory-entry-value entry)
          (%memory-entry-source-label entry)))

(defun %memory-source-summary-lines (backend)
  (if (not (file-memory-backend-p backend))
      '()
      (let ((sources (%file-memory-source-specs backend :scope :effective)))
        (if sources
            (loop for source in sources
                  collect (format nil "- ~A: ~A (~D entr~:@P)"
                                  (%memory-source-scope-label (getf source :scope))
                                  (or (getf source :source-path) "n/a")
                                  (length (getf source :entries))))
            '("(none)")))))

(defun %memory-entries-for-scope (backend scope)
  (%sort-memory-entries
   (nth-value 0 (%load-memory-source-data backend scope))))

(defun %topic-memory-entries (backend)
  (%memory-entries-for-scope backend :topics))

(defun %effective-memory-entries (backend)
  (check-type backend file-memory-backend)
  (let ((table (make-hash-table :test #'equal)))
    (dolist (entry (nth-value 0 (%load-memory-source-data backend :effective)))
      (setf (gethash (memory-entry-key entry) table) entry))
    (%sort-memory-entries
     (loop for entry being the hash-values of table collect entry))))

(defun %upsert-memory-entry (entries key value scope source)
  (let ((normalized-key (or (and key (plusp (length (%trim-text key))) (%trim-text key))
                            (%normalize-memory-key value)))
        (normalized-value (%collapse-whitespace value)))
    (when (zerop (length normalized-value))
      (error "Memory value must not be empty."))
    (let* ((without-key (remove normalized-key entries
                                :key #'memory-entry-key
                                :test #'equal))
           (entry (make-memory-entry :key normalized-key
                                     :value normalized-value
                                     :scope scope
                                     :source source)))
      (values entry (append without-key (list entry))))))

(defmethod memory-store ((backend file-memory-backend) key value
                         &key (scope :project) (source :manual))
  (unless (member scope '(:global :project) :test #'eq)
    (error "FILE memory backend only supports :GLOBAL and :PROJECT store scopes."))
  (let* ((path (%memory-path-for-scope backend scope))
         (entries (%read-memory-file path scope :file)))
    (multiple-value-bind (stored next-entries)
        (%upsert-memory-entry entries key value scope source)
      (%write-memory-file path (%dedupe-memory-entries next-entries))
      (push stored *session-memory-entries*)
      (%publish-memory-updated backend :store (memory-entry-key stored) (memory-entry-value stored))
      stored)))

(defmethod memory-list ((backend file-memory-backend) &key (scope :effective))
  (case scope
    (:global
     (%memory-entries-for-scope backend :global))
    (:project
     (%memory-entries-for-scope backend :project))
    (:topics
     (%topic-memory-entries backend))
    (:session
     (%sort-memory-entries *session-memory-entries*))
    (:effective
     (%effective-memory-entries backend))
    (:all
     (%sort-memory-entries
      (append (%effective-memory-entries backend)
              (memory-list backend :scope :session))))
    (otherwise
     (error "Unknown memory list scope ~S." scope))))

(defmethod memory-query ((backend file-memory-backend) query
                         &key (scope :effective) (limit 25))
  (let* ((needle (string-downcase (%trim-text (or query ""))))
         (matches
           (if (zerop (length needle))
               (memory-list backend :scope scope)
               (loop for entry in (memory-list backend :scope scope)
                     for haystack-key = (string-downcase (or (memory-entry-key entry) ""))
                     for haystack-value = (string-downcase (or (memory-entry-value entry) ""))
                     when (or (search needle haystack-key :test #'char=)
                              (search needle haystack-value :test #'char=))
                       collect entry))))
    (if (and (integerp limit) (>= limit 0))
        (subseq matches 0 (min limit (length matches)))
        matches)))

(defmethod memory-delete ((backend file-memory-backend) key &key (scope :project))
  (unless (member scope '(:global :project) :test #'eq)
    (error "FILE memory backend only supports :GLOBAL and :PROJECT delete scopes."))
  (let* ((normalized-key (%normalize-memory-key key))
         (path (%memory-path-for-scope backend scope))
         (entries (%read-memory-file path scope :file))
         (next (remove normalized-key entries
                       :key #'memory-entry-key
                       :test #'equal)))
    (when (/= (length entries) (length next))
      (%write-memory-file path next)
      (setf *session-memory-entries*
            (remove normalized-key *session-memory-entries*
                    :key #'memory-entry-key
                    :test #'equal))
      (%publish-memory-updated backend :delete normalized-key nil)
      t)))

(defmethod memory-forget ((backend file-memory-backend) &key (scope :session))
  (case scope
    (:session
     (let ((count (length *session-memory-entries*)))
       (setf *session-memory-entries* '())
       (%publish-memory-updated backend :clear-session nil nil)
       count))
    (:project
     (let* ((entries (memory-list backend :scope :project))
            (count (length entries)))
       (%write-memory-file (file-memory-backend-project-path backend) '())
       (%publish-memory-updated backend :clear-project nil nil)
       count))
    (:global
     (let* ((entries (memory-list backend :scope :global))
            (count (length entries)))
       (%write-memory-file (file-memory-backend-global-path backend) '())
       (%publish-memory-updated backend :clear-global nil nil)
       count))
    (:all
     (+ (memory-forget backend :scope :session)
        (memory-forget backend :scope :project)
        (memory-forget backend :scope :global)))
    (otherwise
     (error "Unknown memory forget scope ~S." scope))))

(defun default-memory-editor-runner (editor path)
  (uiop:run-program (list editor path)
                    :input *standard-input*
                    :output *standard-output*
                    :error-output *error-output*
                    :ignore-error-status t))

(defun memory-command-show (&key (backend (current-memory-backend)))
  (let ((effective (memory-list backend :scope :effective))
        (session (memory-list backend :scope :session)))
    (with-output-to-string (out)
      (format out "Memory backend: ~A~%" (%event-backend-name backend))
      (when (file-memory-backend-p backend)
        (format out "Loaded sources:~%")
        (dolist (line (%memory-source-summary-lines backend))
          (write-line line out)))
      (format out "Effective entries: ~D~%" (length effective))
      (if effective
          (dolist (entry effective)
            (write-line (%memory-entry-display-line entry) out))
          (format out "(none)~%"))
      (format out "Session entries: ~D~%" (length session)))))

(defun memory-command-edit (&key (backend (current-memory-backend)) editor)
  (if (typep backend 'file-memory-backend)
      (let* ((path (file-memory-backend-project-path backend))
             (editor-cmd (or editor
                             (uiop:getenv "AMOEBUM_EDITOR")
                             (uiop:getenv "VISUAL")
                             (uiop:getenv "EDITOR"))))
        (%ensure-memory-file-header path)
        (if (and (stringp editor-cmd) (plusp (length (%trim-text editor-cmd))))
            (progn
              (funcall (or *memory-editor-runner* #'default-memory-editor-runner)
                       editor-cmd
                       (namestring path))
              (format nil "Opened ~A using ~A." (namestring path) editor-cmd))
            (format nil "No editor configured; edit ~A manually." (namestring path))))
      (format nil "Backend ~A does not support /memory edit; use /memory show."
              (memory-backend-kind backend))))

(defun memory-command-clear (&key (backend (current-memory-backend)))
  (let ((cleared (memory-forget backend :scope :session)))
    (format nil "Cleared ~D session memor~:@P." cleared)))

(defun %utc-timestamp-string (&optional (timestamp (get-universal-time)))
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time timestamp 0)
    (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0DZ"
            year month day hour minute second)))

(defun %fnv1a-64-hash (text)
  (let ((hash #xcbf29ce484222325)
        (prime #x100000001B3)
        (modulus #x10000000000000000))
    (loop for char across (or text "") do
      (setf hash (logxor hash (char-code char)))
      (setf hash (mod (* hash prime) modulus)))
    hash))

(defun %entry-scope-signature (scope)
  (cond
    ((keywordp scope)
     (string-downcase (symbol-name scope)))
    ((and (consp scope) (eq (first scope) :topic))
     (format nil "topic/~A" (%trim-text (or (second scope) ""))))
    (t
     (%trim-text (princ-to-string scope)))))

(defun %memory-entry-source-hash (source-path entry)
  (let ((payload (format nil "~A|~A|~A|~A"
                         source-path
                         (%entry-scope-signature (memory-entry-scope entry))
                         (or (memory-entry-key entry) "")
                         (or (memory-entry-value entry) ""))))
    (format nil "~16,'0X" (%fnv1a-64-hash payload))))

(defun %topic-memory-directory (project-root)
  (merge-pathnames #P".amoebum/memory/" (uiop:ensure-directory-pathname project-root)))

(defun %topic-memory-files (project-root)
  (let* ((directory-path (%topic-memory-directory project-root))
         (pattern (merge-pathnames #P"*.md" directory-path)))
    (sort (remove-if
           (lambda (path)
             (let ((name (string-downcase (or (pathname-name path) ""))))
               (or (string= name "memory")
                   (%string-prefix-p-ci "haake-export" name))))
           (copy-list (directory pattern)))
          #'string<
          :key #'namestring)))

(defun %normalize-topic-name-from-path (path)
  (%normalize-memory-key (or (pathname-name path) "topic")))

(defun %memory-project-root (&optional backend)
  (uiop:ensure-directory-pathname
   (or (and (file-memory-backend-p backend)
            (file-memory-backend-project-root backend))
       (and (current-config) (config-project-root (current-config)))
       *default-pathname-defaults*)))

(defun %make-source-backend (backend)
  (if (file-memory-backend-p backend)
      backend
      (%make-file-backend-from-config (current-config))))

(defun %memory-import-state-path (&optional backend)
  (merge-pathnames #P".amoebum/memory/haake-import-state-v1.sexp"
                   (%memory-project-root backend)))

(defun %memory-import-failure-log-path (&optional backend)
  (merge-pathnames #P".amoebum/memory/haake-import-failures.log"
                   (%memory-project-root backend)))

(defun %default-memory-import-state ()
  (list :version 1 :updated-at nil :imports '()))

(defun %load-memory-import-state (&optional backend)
  (let ((path (%memory-import-state-path backend)))
    (if (probe-file path)
        (handler-case
            (with-open-file (stream path :direction :input)
              (let ((state (read stream nil nil)))
                (if (and (listp state)
                         (integerp (getf state :version))
                         (listp (getf state :imports)))
                    state
                    (%default-memory-import-state))))
          (error ()
            (%default-memory-import-state)))
        (%default-memory-import-state))))

(defun %write-memory-import-state (state &optional backend)
  (let ((path (%memory-import-state-path backend)))
    (ensure-directories-exist path)
    (with-open-file (stream path
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (with-standard-io-syntax
        (write state :stream stream :pretty t)))
    path))

(defun %append-memory-import-failure (backend source-path source-hash condition)
  (let ((path (%memory-import-failure-log-path backend)))
    (ensure-directories-exist path)
    (with-open-file (stream path
                            :direction :output
                            :if-exists :append
                            :if-does-not-exist :create)
      (format stream "~A | ~A | ~A | ~A~%"
              (%utc-timestamp-string)
              source-path
              source-hash
              condition))
    path))

(defun %import-state-known-hashes (state)
  (let ((table (make-hash-table :test #'equal)))
    (dolist (entry (or (getf state :imports) '()))
      (let ((hash (and (listp entry) (getf entry :source-hash))))
        (when (and (stringp hash) (plusp (length hash)))
          (setf (gethash hash table) t))))
    table))

(defun %collect-memory-import-sources (backend)
  (let ((source-backend (%make-source-backend backend)))
    (%file-memory-source-specs source-backend :scope :effective)))

(defun %collect-memory-import-candidates (backend)
  (loop for source in (%collect-memory-import-sources backend)
        append (loop for entry in (copy-list (getf source :entries))
                     collect (list :entry entry
                                   :scope (getf source :scope)
                                   :source-path (getf source :source-path)))))

(defun %parse-imported-id (stdout source-hash key)
  (or (loop for line in (uiop:split-string (or stdout "") :separator '(#\Newline))
            for trimmed = (%trim-text line)
            when (and (plusp (length trimmed))
                      (%string-prefix-p-ci "id" trimmed))
              do (let* ((separator (or (position #\: trimmed)
                                       (position #\Tab trimmed)
                                       (position #\Space trimmed)))
                        (raw (if separator
                                 (%trim-text (subseq trimmed (1+ separator)))
                                 "")))
                   (when (plusp (length raw))
                     (return raw))))
      (and (plusp (length (%trim-text key)))
           (%trim-text key))
      source-hash))

(defun %haake-backend-for-transfer (backend)
  (cond
    ((and (fboundp 'haake-cli-memory-backend-p)
          (funcall (symbol-function 'haake-cli-memory-backend-p) backend))
     (values backend nil))
    (t
     (let ((cfg (current-config)))
       (cond
         ((not (%haake-cli-available-from-config-p cfg))
          (values nil :haake-cli-unavailable))
         ((not (%haake-cli-status-ok-from-config-p cfg))
          (values nil :haake-status-unavailable))
         ((not (%haake-cli-compatible-from-config-p cfg))
          (values nil :haake-cli-incompatible))
         (t
          (let ((candidate (%make-haake-backend-from-config cfg)))
            (if candidate
                (values candidate nil)
                (values nil :haake-backend-instantiation-failed)))))))))

(defun %haake-metadata-arguments (source-path source-hash import-batch-id imported-at)
  (list "--metadata" (format nil "source_path=~A" source-path)
        "--metadata" (format nil "source_hash=~A" source-hash)
        "--metadata" (format nil "import_batch_id=~A" import-batch-id)
        "--metadata" (format nil "imported_at=~A" imported-at)))

(defun memory-import-to-haake (&key (backend (current-memory-backend)))
  (multiple-value-bind (haake-backend missing-reason)
      (%haake-backend-for-transfer backend)
    (unless haake-backend
      (return-from memory-import-to-haake
        (list :status :error
              :reason missing-reason
              :message "Haake backend is not available for import.")))
    (let* ((state (%load-memory-import-state backend))
           (known-hashes (%import-state-known-hashes state))
           (import-batch-id (format nil "batch-~D" (get-universal-time)))
           (imported-at (%utc-timestamp-string))
           (new-import-records '())
           (imported-count 0)
           (skipped-count 0)
           (failed-count 0)
           (failure-log-path nil))
      (dolist (candidate (%collect-memory-import-candidates backend))
        (let* ((entry (getf candidate :entry))
               (scope (getf candidate :scope))
               (source-path (getf candidate :source-path))
               (source-hash (%memory-entry-source-hash source-path entry)))
          (if (gethash source-hash known-hashes)
              (incf skipped-count)
              (handler-case
                  (let* ((result (%haake-cli-run
                                  haake-backend
                                  (append (list "memory"
                                                "insert"
                                                (%haake-scope-path haake-backend scope)
                                                (memory-entry-value entry)
                                                "-t"
                                                "semantic"
                                                "--key"
                                                (memory-entry-key entry)
                                                "--agent"
                                                (haake-cli-memory-backend-agent haake-backend))
                                          (%haake-metadata-arguments source-path
                                                                     source-hash
                                                                     import-batch-id
                                                                     imported-at))))
                         (imported-id (%parse-imported-id (getf result :stdout)
                                                          source-hash
                                                          (memory-entry-key entry))))
                    (push (list :source-path source-path
                                :source-hash source-hash
                                :scope scope
                                :key (memory-entry-key entry)
                                :value (memory-entry-value entry)
                                :imported-id imported-id
                                :import-batch-id import-batch-id
                                :imported-at imported-at)
                          new-import-records)
                    (setf (gethash source-hash known-hashes) t)
                    (incf imported-count))
                (error (condition)
                  (incf failed-count)
                  (setf failure-log-path
                        (%append-memory-import-failure backend
                                                       source-path
                                                       source-hash
                                                       condition)))))))
      (when (or (plusp imported-count) (plusp failed-count))
        (%write-memory-import-state
         (list :version 1
               :updated-at (%utc-timestamp-string)
               :imports (append (or (getf state :imports) '())
                                (nreverse new-import-records)))
         backend))
      (list :status (if (plusp failed-count) :partial :ok)
            :import-batch-id import-batch-id
            :imported imported-count
            :skipped skipped-count
            :failed failed-count
            :state-path (namestring (%memory-import-state-path backend))
            :failure-log-path (and failure-log-path (namestring failure-log-path))))))

(defun %state-topic-scope-name (scope)
  (when (and (consp scope) (eq (first scope) :topic))
    (%trim-text (princ-to-string (second scope)))))

(defun %import-state-topic-names (state)
  (sort (remove-duplicates
         (loop for entry in (or (getf state :imports) '())
               for scope = (and (listp entry) (getf entry :scope))
               for topic = (%state-topic-scope-name scope)
               when (and topic (plusp (length topic)))
                 collect topic)
         :test #'string-equal)
        #'string< :key #'string-downcase))

(defun %write-export-section (stream title entries)
  (format stream "## ~A~%" title)
  (if entries
      (dolist (entry (%sort-memory-entries entries))
        (write-line (%memory-entry-line entry) stream))
      (write-line "(none)" stream))
  (write-line "" stream))

(defun memory-export-from-haake (&key (backend (current-memory-backend)))
  (multiple-value-bind (haake-backend missing-reason)
      (%haake-backend-for-transfer backend)
    (unless haake-backend
      (return-from memory-export-from-haake
        (list :status :error
              :reason missing-reason
              :message "Haake backend is not available for export.")))
    (let* ((state (%load-memory-import-state backend))
           (topic-names (%import-state-topic-names state))
           (global-entries (memory-list haake-backend :scope :global))
           (project-entries (memory-list haake-backend :scope :project))
           (topic-entry-count 0)
           (output-path (merge-pathnames #P".amoebum/memory/haake-export-MEMORY.md"
                                         (%memory-project-root backend))))
      (ensure-directories-exist output-path)
      (with-open-file (stream output-path
                              :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create)
        (write-line "# Amoebum Memory" stream)
        (format stream "# Exported from Haake at ~A~%~%" (%utc-timestamp-string))
        (%write-export-section stream "Global" global-entries)
        (%write-export-section stream "Project" project-entries)
        (dolist (topic topic-names)
          (let ((entries (memory-list haake-backend :scope (list :topic topic))))
            (incf topic-entry-count (length entries))
            (%write-export-section stream
                                   (format nil "Topic: ~A" topic)
                                   entries))))
      (list :status :ok
            :output-path (namestring output-path)
            :global-count (length global-entries)
            :project-count (length project-entries)
            :topic-count topic-entry-count
            :topic-scope-count (length topic-names)))))

(defun %memory-command-option-value (tokens option)
  (let ((option-equals (format nil "~A=" option)))
    (loop for token in tokens
          for rest on tokens
          do (cond
               ((string-equal token option)
                (return (and (second rest) (%trim-text (second rest)))))
               ((%string-prefix-p-ci option-equals token)
                (return (%trim-text (subseq token (length option-equals)))))))))

(defun %format-memory-import-result (result)
  (if (eq (getf result :status) :error)
      (format nil "~A (~A)."
              (or (getf result :message) "Import failed")
              (or (getf result :reason) :unknown))
      (format nil
              "Import batch ~A finished: imported ~D, skipped ~D, failed ~D. State: ~A~@[. Failures: ~A~]"
              (getf result :import-batch-id)
              (or (getf result :imported) 0)
              (or (getf result :skipped) 0)
              (or (getf result :failed) 0)
              (or (getf result :state-path) "n/a")
              (getf result :failure-log-path))))

(defun %format-memory-export-result (result)
  (if (eq (getf result :status) :error)
      (format nil "~A (~A)."
              (or (getf result :message) "Export failed")
              (or (getf result :reason) :unknown))
      (format nil
              "Exported Haake memory snapshot to ~A (global ~D, project ~D, topic entries ~D across ~D topics)."
              (or (getf result :output-path) "n/a")
              (or (getf result :global-count) 0)
              (or (getf result :project-count) 0)
              (or (getf result :topic-count) 0)
              (or (getf result :topic-scope-count) 0))))

(defun %command-tokens (text)
  (let* ((trimmed (%trim-text text))
         (len (length trimmed))
         (tokens '())
         (start 0))
    (labels ((separatorp (char)
               (member char '(#\Space #\Tab #\Newline #\Return) :test #'char=)))
      (loop for index from 0 to len do
        (if (= index len)
            (when (< start index)
              (push (subseq trimmed start index) tokens))
            (when (separatorp (char trimmed index))
              (when (< start index)
                (push (subseq trimmed start index) tokens))
              (setf start (1+ index)))))
      (nreverse tokens))))

(defun memory-command-input-p (text)
  (let* ((trimmed (%trim-text text))
         (len (length trimmed)))
    (and (>= len 7)
         (string-equal "/memory" trimmed :end2 7)
         (or (= len 7)
             (member (char trimmed 7)
                     '(#\Space #\Tab #\Newline #\Return)
                     :test #'char=)))))

(defun run-memory-command (text &key (backend (current-memory-backend)) editor)
  (let ((trimmed (%trim-text text)))
    (unless (memory-command-input-p trimmed)
      (return-from run-memory-command (values nil nil)))
    (let* ((suffix (%trim-text (subseq trimmed 7)))
           (tokens (%command-tokens suffix))
           (subcommand (if tokens
                           (string-downcase (first tokens))
                           "show"))
           (tail (if tokens
                     (%trim-text (subseq suffix (min (length suffix)
                                                     (length (first tokens)))))
                     "")))
      (case (intern (string-upcase subcommand) :keyword)
        (:SHOW
         (values t (memory-command-show :backend backend)))
        (:EDIT
         (values t (memory-command-edit :backend backend :editor editor)))
        (:CLEAR
         (values t (memory-command-clear :backend backend)))
        (:REMEMBER
         (if (zerop (length tail))
             (values t "Usage: /memory remember <statement>")
             (let ((entry (memory-store backend nil tail :scope :project :source :memory-command)))
               (values t
                       (format nil "Remembered [~A] ~A"
                               (memory-entry-key entry)
                               (memory-entry-value entry))))))
        (:FORGET
         (if (zerop (length tail))
            (values t "Usage: /memory forget <statement-or-key>")
             (if (memory-delete backend tail :scope :project)
                 (values t (format nil "Forgot ~A." (%normalize-memory-key tail)))
                 (values t (format nil "No memory entry matched ~A." (%normalize-memory-key tail))))))
        (:IMPORT
         (let ((target (%memory-command-option-value (rest tokens) "--to")))
           (if (and target (string-equal (%trim-text target) "haake"))
               (values t (%format-memory-import-result (memory-import-to-haake :backend backend)))
               (values t "Usage: /memory import --to haake"))))
        (:EXPORT
         (let ((source (%memory-command-option-value (rest tokens) "--from")))
           (if (and source (string-equal (%trim-text source) "haake"))
               (values t (%format-memory-export-result (memory-export-from-haake :backend backend)))
               (values t "Usage: /memory export --from haake"))))
        (otherwise
         (values t
                 (format nil "Unknown /memory subcommand ~A. Use show|edit|clear|remember|forget|import|export."
                         subcommand)))))))

(defun %extract-after-prefix (text prefix)
  (if (%string-prefix-p-ci prefix text)
      (%trim-text (subseq text (length prefix)))
      nil))

(defun %extract-after-search (text token)
  (let ((pos (search token text :test #'char-equal)))
    (when pos
      (%trim-text (subseq text (+ pos (length token)))))))

(defun extract-durable-memory-candidate (text)
  (let* ((trimmed (%trim-text text))
         (remember-body (or (%extract-after-prefix trimmed "remember that ")
                            (%extract-after-prefix trimmed "remember ")
                            (%extract-after-prefix trimmed "please remember that ")
                            (%extract-after-prefix trimmed "please remember ")))
         (forget-body (or (%extract-after-prefix trimmed "forget ")
                          (%extract-after-prefix trimmed "please forget ")
                          (%extract-after-prefix trimmed "forget the ")))
         (preference-body (or (%extract-after-search trimmed "i always ")
                              (%extract-after-search trimmed "i prefer ")
                              (%extract-after-search trimmed "please always "))))
    (cond
      ((and remember-body (plusp (length remember-body)))
       (make-memory-candidate :kind :remember
                              :text remember-body
                              :key (%normalize-memory-key remember-body)
                              :confidence 0.95d0))
      ((and forget-body (plusp (length forget-body)))
       (let* ((normalized-forget
                (if (%string-suffix-p-ci " preference" forget-body)
                    (%trim-text (subseq forget-body
                                        0
                                        (- (length forget-body)
                                           (length " preference"))))
                    forget-body)))
         (make-memory-candidate :kind :forget
                                :text normalized-forget
                                :key (%normalize-memory-key normalized-forget)
                                :confidence 0.90d0)))
      ((and preference-body (plusp (length preference-body)))
       (make-memory-candidate :kind :preference
                              :text preference-body
                              :key (%normalize-memory-key preference-body)
                              :confidence 0.70d0))
      (t nil))))

(defun apply-memory-candidate (candidate &key (backend (current-memory-backend)))
  (when (memory-candidate-p candidate)
    (case (memory-candidate-kind candidate)
      (:remember
       (let ((entry (memory-store backend
                                  (memory-candidate-key candidate)
                                  (memory-candidate-text candidate)
                                  :scope :project
                                  :source :extracted)))
         (values :stored entry)))
      (:forget
       (if (memory-delete backend (memory-candidate-key candidate) :scope :project)
           (values :deleted (memory-candidate-key candidate))
           (values :not-found (memory-candidate-key candidate))))
      (:preference
       (values :candidate candidate))
      (otherwise
       (values :ignored candidate)))))
