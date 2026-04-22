(in-package :amoebum)

;;;; File-store persistence for memory entries.
;;;;
;;;; Decomposed from memory.lisp by NXT-388. Owns the on-disk
;;;; representation of memory entries (`# Amoebum Memory` markdown
;;;; format with `- [key] value` lines), the import/cycle-safe loader
;;;; for `@imports`, the topic-memory directory layout, scope-aware
;;;; reads, and all `file-memory-backend` method implementations
;;;; (store/list/query/delete/forget). On-disk format is unchanged:
;;;; existing `~/.amoebum/memory/MEMORY.md` and project-level
;;;; `.amoebum/MEMORY.md` files load identically.

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
               (amoebum.fp:filter-map
                (lambda (entry)
                  (let ((haystack-key (string-downcase (or (memory-entry-key entry) "")))
                        (haystack-value (string-downcase (or (memory-entry-value entry) ""))))
                    (when (or (search needle haystack-key :test #'char=)
                              (search needle haystack-value :test #'char=))
                      entry)))
                (memory-list backend :scope scope)))))
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
