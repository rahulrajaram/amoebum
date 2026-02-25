(in-package :amoebum)

(defparameter *extension-load-report* '())
(defparameter *loaded-extensions* '())
(defparameter *extension-last-discovered* '())
(defparameter *disabled-extensions* (make-hash-table :test #'equal))
(defparameter *extension-registry* (make-hash-table :test #'equal))
(defparameter *extension-watch-snapshot* (make-hash-table :test #'equal))
(defparameter *extension-hot-reload-enabled-p* t)
(defparameter *extension-hot-reload-interval-seconds* 1.0d0)
(defparameter *extension-hot-reload-thread* nil)
(defparameter *extension-hot-reload-running-p* nil)

;; Test and smoke harnesses can bind these to avoid mutating real user paths.
(defparameter *extensions-global-directory-override* nil)
(defparameter *extensions-project-directory-override* nil)

(defstruct (extension-load-record
            (:constructor make-extension-load-record
                (&key path
                 scope
                 name
                 version
                 dependencies
                 entry-point
                 manifest-path
                 status
                 message
                 (timestamp (get-universal-time)))))
  path
  (scope :project :type keyword)
  name
  version
  (dependencies '() :type list)
  entry-point
  manifest-path
  (status :loaded :type keyword)
  message
  (timestamp 0 :type integer))

(defstruct (extension-registry-entry
            (:constructor make-extension-registry-entry
                (&key name
                 version
                 dependencies
                 entry-point
                 manifest-path
                 extension-root
                 scope
                 enabled-p
                 status
                 loaded-at
                 last-write-date
                 message)))
  name
  version
  (dependencies '() :type list)
  entry-point
  manifest-path
  extension-root
  (scope :project :type keyword)
  (enabled-p t :type boolean)
  (status :loaded :type keyword)
  (loaded-at 0 :type integer)
  last-write-date
  message)

(defun %extension-trim (value)
  (if (stringp value)
      (string-trim '(#\Space #\Tab #\Newline #\Return) value)
      ""))

(defun %normalize-pathname (value)
  (cond
    ((pathnamep value) value)
    ((stringp value) (pathname value))
    (t nil)))

(defun %ensure-directory (value)
  (let ((pathname (%normalize-pathname value)))
    (and pathname
         (uiop:ensure-directory-pathname pathname))))

(defun %resolve-project-root (&optional project-root)
  (let* ((cfg (ignore-errors (current-config)))
         (candidate (or project-root
                        (and (config-p cfg) (config-project-root cfg))
                        (ignore-errors (uiop:getcwd))
                        *default-pathname-defaults*))
         (directory (%ensure-directory candidate)))
    (or (and directory
             (or (ignore-errors (uiop:ensure-directory-pathname (truename directory)))
                 directory))
        (uiop:ensure-directory-pathname *default-pathname-defaults*))))

(defun %global-extension-directory (&key global-directory)
  (or (%ensure-directory global-directory)
      (%ensure-directory *extensions-global-directory-override*)
      (uiop:ensure-directory-pathname
       (merge-pathnames #P".amoebum/extensions/" (user-homedir-pathname)))))

(defun %project-extension-directory (&key project-root project-directory)
  (or (%ensure-directory project-directory)
      (%ensure-directory *extensions-project-directory-override*)
      (uiop:ensure-directory-pathname
       (merge-pathnames #P".amoebum/extensions/"
                        (%resolve-project-root project-root)))))

(defun %extension-sort-key (path)
  (string-downcase
   (or (file-namestring path)
       (namestring path))))

(defun %canonical-extension-path (path)
  (let* ((pathname (%normalize-pathname path))
         (resolved (and pathname
                        (or (ignore-errors (truename pathname))
                            pathname))))
    (if resolved
        (namestring resolved)
        "")))

(defun %extension-key (path)
  (string-downcase (%canonical-extension-path path)))

(defun %extension-registry-key (name)
  (string-downcase (%extension-trim name)))

(defun %extension-match-target-p (target path-text)
  (let* ((needle (string-downcase (%extension-trim target)))
         (haystack (string-downcase path-text))
         (pathname (%normalize-pathname path-text))
         (filename (and pathname (file-namestring pathname)))
         (stem (and pathname (pathname-name pathname))))
    (and (plusp (length needle))
         (or (string= needle haystack)
             (and filename (string= needle (string-downcase filename)))
             (and stem (string= needle (string-downcase stem)))
             (search needle haystack :test #'char=)))))

(defun %list-extension-manifest-files (directory)
  (if (and directory (probe-file directory))
      (let ((files '()))
        (dolist (candidate (directory (merge-pathnames #P"*/extension.lisp" directory)))
          (unless (uiop:directory-pathname-p candidate)
            (push candidate files)))
        (sort files #'string< :key #'%extension-sort-key))
      '()))

(defun %list-legacy-extension-files (directory)
  (if (and directory (probe-file directory))
      (sort
       (remove-if
        (lambda (path)
          (or (uiop:directory-pathname-p path)
              (string-equal (pathname-name path) "extension")))
        (directory (merge-pathnames #P"*.lisp" directory)))
       #'string<
       :key #'%extension-sort-key)
      '()))

(defun %safe-file-write-date (path)
  (ignore-errors
    (file-write-date path)))

(defun %manifest-parent-directory (manifest-path)
  (uiop:pathname-directory-pathname manifest-path))

(defun %ensure-string (value)
  (cond
    ((null value) nil)
    ((stringp value) value)
    ((symbolp value) (string-downcase (symbol-name value)))
    (t (princ-to-string value))))

(defun %normalize-dependencies (value)
  (cond
    ((null value) '())
    ((listp value)
     (loop for dep in value
           for dep-name = (%ensure-string dep)
           when (and dep-name (plusp (length (%extension-trim dep-name))))
             collect dep-name))
    (t
     (let ((dep-name (%ensure-string value)))
       (if (and dep-name (plusp (length (%extension-trim dep-name))))
           (list dep-name)
           '())))))

(defun %read-manifest-form (manifest-path)
  (with-open-file (stream manifest-path :direction :input :external-format :utf-8)
    (let ((form (read stream nil nil)))
      (unless form
        (error "Manifest ~A is empty." manifest-path))
      form)))

(defun %manifest->metadata (manifest-path)
  (let* ((manifest-form (%read-manifest-form manifest-path))
         (name (or (getf manifest-form :name)
                   (pathname-name (%manifest-parent-directory manifest-path))))
         (version (or (getf manifest-form :version) "0.0.0"))
         (dependencies (%normalize-dependencies (getf manifest-form :dependencies)))
         (entry-point (or (getf manifest-form :entry-point)
                          "main.lisp")))
    (unless (listp manifest-form)
      (error "Manifest ~A must be a property list." manifest-path))
    (unless (and name (plusp (length (%extension-trim (%ensure-string name)))))
      (error "Manifest ~A missing :name." manifest-path))
    (unless (and entry-point (plusp (length (%extension-trim (%ensure-string entry-point)))))
      (error "Manifest ~A missing :entry-point." manifest-path))
    (list :kind :manifest
          :name (%ensure-string name)
          :version (%ensure-string version)
          :dependencies dependencies
          :entry-point (%ensure-string entry-point)
          :manifest-path manifest-path
          :extension-root (%manifest-parent-directory manifest-path)
          :path manifest-path
          :last-write-date (%safe-file-write-date manifest-path))))

(defun %legacy-file->metadata (file)
  (list :kind :legacy
        :name (pathname-name file)
        :version "0.0.0"
        :dependencies '()
        :entry-point (%canonical-extension-path file)
        :manifest-path nil
        :extension-root (uiop:pathname-directory-pathname file)
        :path file
        :last-write-date (%safe-file-write-date file)))

(defun discover-user-extension-files (&key project-root global-directory project-directory)
  (let* ((global-path (%global-extension-directory :global-directory global-directory))
         (project-path (%project-extension-directory :project-root project-root
                                                     :project-directory project-directory))
         (global-files (append (%list-extension-manifest-files global-path)
                               (%list-legacy-extension-files global-path)))
         (project-files (append (%list-extension-manifest-files project-path)
                                (%list-legacy-extension-files project-path))))
    (values global-files project-files)))

(defun extension-disabled-p (path)
  (not (null (gethash (%extension-key path) *disabled-extensions*))))

(defun list-extension-report ()
  (copy-list *extension-load-report*))

(defun list-loaded-extensions ()
  (copy-list *loaded-extensions*))

(defun list-extension-registry ()
  (let ((entries '()))
    (maphash (lambda (_key entry)
               (declare (ignore _key))
               (push entry entries))
             *extension-registry*)
    (sort entries #'string<
          :key (lambda (entry)
                 (string-downcase (or (extension-registry-entry-name entry) ""))))))

(defun extension-report-summary (&optional (report *extension-load-report*))
  (let ((loaded 0)
        (errors 0)
        (disabled 0))
    (dolist (entry report)
      (case (extension-load-record-status entry)
        (:loaded (incf loaded))
        (:error (incf errors))
        (:disabled (incf disabled))))
    (list :total (length report)
          :loaded loaded
          :errors errors
          :disabled disabled)))

(defun known-user-extension-paths ()
  (let ((seen (make-hash-table :test #'equal))
        (paths '()))
    (labels ((remember (path-text)
               (let ((trimmed (%extension-trim path-text)))
                 (when (plusp (length trimmed))
                   (let ((key (string-downcase trimmed)))
                     (unless (gethash key seen)
                       (setf (gethash key seen) t)
                       (push trimmed paths)))))))
      (dolist (path-text *extension-last-discovered*)
        (remember path-text))
      (dolist (entry *extension-load-report*)
        (remember (extension-load-record-path entry))
        (remember (extension-load-record-manifest-path entry))))
    (nreverse paths)))

(defun known-user-extension-names ()
  (let ((seen (make-hash-table :test #'equal))
        (names '()))
    (labels ((remember (value)
               (let ((trimmed (%extension-trim value)))
                 (when (plusp (length trimmed))
                   (let ((key (string-downcase trimmed)))
                     (unless (gethash key seen)
                       (setf (gethash key seen) t)
                       (push trimmed names)))))))
      (dolist (entry (list-extension-registry))
        (remember (extension-registry-entry-name entry)))
      (dolist (entry *extension-load-report*)
        (remember (extension-load-record-name entry))))
    (nreverse names)))

(defun %disable-extension-path (path-text)
  (setf (gethash (%extension-key path-text) *disabled-extensions*) t))

(defun %enable-extension-path (path-text)
  (remhash (%extension-key path-text) *disabled-extensions*))

(defun %matched-registry-paths-by-target (target)
  (let ((paths '()))
    (dolist (entry (list-extension-registry))
      (let ((entry-name (extension-registry-entry-name entry))
            (entry-path (or (extension-registry-entry-manifest-path entry)
                            (extension-registry-entry-entry-point entry))))
        (when (and entry-path
                   (or (%extension-match-target-p target entry-path)
                       (%extension-match-target-p target entry-name)))
          (push entry-path paths))))
    (nreverse paths)))

(defun disable-user-extension (target)
  (let* ((trimmed (%extension-trim target))
         (known-paths (known-user-extension-paths))
         (disabled '()))
    (cond
      ((zerop (length trimmed))
       (values '() 0))
      ((string-equal trimmed "all")
       (dolist (path-text (append known-paths (%matched-registry-paths-by-target "all")))
         (%disable-extension-path path-text)
         (push path-text disabled))
       (values (remove-duplicates (nreverse disabled) :test #'string-equal)
               (length (remove-duplicates disabled :test #'string-equal))))
      (t
       (dolist (path-text known-paths)
         (when (%extension-match-target-p trimmed path-text)
           (%disable-extension-path path-text)
           (push path-text disabled)))
       (dolist (path-text (%matched-registry-paths-by-target trimmed))
         (%disable-extension-path path-text)
         (push path-text disabled))
       (when (and (null disabled) (probe-file trimmed))
         (let ((resolved (%canonical-extension-path trimmed)))
           (when (plusp (length resolved))
             (%disable-extension-path resolved)
             (push resolved disabled))))
       (let ((result (remove-duplicates (nreverse disabled) :test #'string-equal)))
         (values result (length result)))))))

(defun enable-user-extension (target)
  (let* ((trimmed (%extension-trim target))
         (known-paths (known-user-extension-paths))
         (enabled '()))
    (cond
      ((zerop (length trimmed))
       (values '() 0))
      ((string-equal trimmed "all")
       (dolist (path-text known-paths)
         (%enable-extension-path path-text)
         (push path-text enabled))
       (values (remove-duplicates (nreverse enabled) :test #'string-equal)
               (length (remove-duplicates enabled :test #'string-equal))))
      (t
       (dolist (path-text known-paths)
         (when (%extension-match-target-p trimmed path-text)
           (%enable-extension-path path-text)
           (push path-text enabled)))
       (dolist (path-text (%matched-registry-paths-by-target trimmed))
         (%enable-extension-path path-text)
         (push path-text enabled))
       (when (and (null enabled) (probe-file trimmed))
         (let ((resolved (%canonical-extension-path trimmed)))
           (when (plusp (length resolved))
             (%enable-extension-path resolved)
             (push resolved enabled))))
       (let ((result (remove-duplicates (nreverse enabled) :test #'string-equal)))
         (values result (length result)))))))

(defun %publish-extension-loaded (path scope)
  (publish (current-event-bus)
           (make-extension-loaded-event :path path :scope scope)))

(defun %publish-extension-error (path scope condition-text)
  (publish (current-event-bus)
           (make-extension-error-event :path path
                                       :scope scope
                                       :condition condition-text)))

(defun %resolve-entry-point (metadata)
  (let* ((entry-point (getf metadata :entry-point))
         (root (or (getf metadata :extension-root)
                   *default-pathname-defaults*))
         (entry-text (%ensure-string entry-point)))
    (cond
      ((or (search "/" entry-text :test #'char=)
           (search "\\" entry-text :test #'char=)
           (search ".lisp" entry-text :test #'char-equal)
           (search ".asd" entry-text :test #'char-equal))
       (merge-pathnames (pathname entry-text)
                        (uiop:ensure-directory-pathname root)))
      (t
       entry-text))))

(defun %load-entry-point (resolved-entry-point)
  (cond
    ((pathnamep resolved-entry-point)
     (load resolved-entry-point :verbose nil :print nil)
     :direct-load)
    ((stringp resolved-entry-point)
     (asdf:load-system resolved-entry-point)
     :asdf)
    (t
     (error "Unsupported entry-point type ~S." resolved-entry-point))))

(defun %register-extension (metadata scope status message)
  (let* ((name (getf metadata :name))
         (key (%extension-registry-key name))
         (entry-point (%resolve-entry-point metadata))
         (manifest-path (getf metadata :manifest-path))
         (recorded-path (or manifest-path
                            (and (pathnamep entry-point)
                                 (%canonical-extension-path entry-point))
                            (%ensure-string entry-point)))
         (entry (make-extension-registry-entry
                 :name name
                 :version (getf metadata :version)
                 :dependencies (copy-list (getf metadata :dependencies))
                 :entry-point (if (pathnamep entry-point)
                                  (%canonical-extension-path entry-point)
                                  (%ensure-string entry-point))
                 :manifest-path (and manifest-path (%canonical-extension-path manifest-path))
                 :extension-root (%canonical-extension-path (getf metadata :extension-root))
                 :scope scope
                 :enabled-p (not (extension-disabled-p recorded-path))
                 :status status
                 :loaded-at (get-universal-time)
                 :last-write-date (getf metadata :last-write-date)
                 :message message)))
    (setf (gethash key *extension-registry*) entry)
    entry))

(defun %metadata-record-path (metadata)
  (or (and (getf metadata :manifest-path)
           (%canonical-extension-path (getf metadata :manifest-path)))
      (%canonical-extension-path (getf metadata :path))))

(defun %collect-extension-candidates (&key project-root global-directory project-directory)
  (multiple-value-bind (global-files project-files)
      (discover-user-extension-files :project-root project-root
                                     :global-directory global-directory
                                     :project-directory project-directory)
    (append (loop for file in global-files collect (cons :global file))
            (loop for file in project-files collect (cons :project file)))))

(defun %rebuild-extension-watch-snapshot (&optional (paths *extension-last-discovered*))
  (clrhash *extension-watch-snapshot*)
  (dolist (entry *extension-load-report*)
    (let ((manifest-path (extension-load-record-manifest-path entry))
          (entry-point (extension-load-record-entry-point entry)))
      (when (and (stringp manifest-path) (plusp (length (%extension-trim manifest-path))))
        (push manifest-path paths))
      (when (and (stringp entry-point)
                 (plusp (length (%extension-trim entry-point)))
                 (or (search ".lisp" entry-point :test #'char-equal)
                     (search "/" entry-point :test #'char=)
                     (search "\\" entry-point :test #'char=)))
        (push entry-point paths))))
  (dolist (path-text paths)
    (when (plusp (length (%extension-trim path-text)))
      (setf (gethash path-text *extension-watch-snapshot*)
            (%safe-file-write-date path-text))))
  *extension-watch-snapshot*)

(defun load-user-extensions (&key project-root global-directory project-directory
                                  (start-hot-reload *extension-hot-reload-enabled-p*))
  (let* ((candidates (%collect-extension-candidates :project-root project-root
                                                    :global-directory global-directory
                                                    :project-directory project-directory))
         (report '())
         (loaded '()))
    (clrhash *extension-registry*)
    (setf *extension-last-discovered*
          (mapcar (lambda (entry)
                    (%canonical-extension-path (cdr entry)))
                  candidates))
    (dolist (entry candidates)
      (let* ((scope (car entry))
             (file (cdr entry))
             (path-text (%canonical-extension-path file)))
        (handler-case
            (let* ((metadata
                     (if (and (string-equal (pathname-name file) "extension")
                              (string-equal (pathname-type file) "lisp"))
                         (%manifest->metadata file)
                         (%legacy-file->metadata file)))
                   (record-path (%metadata-record-path metadata))
                   (entry-point (%resolve-entry-point metadata)))
              (cond
                ((extension-disabled-p record-path)
                 (%register-extension metadata scope :disabled "disabled by /extensions disable")
                 (push (make-extension-load-record
                        :path record-path
                        :scope scope
                        :name (getf metadata :name)
                        :version (getf metadata :version)
                        :dependencies (copy-list (getf metadata :dependencies))
                        :entry-point (if (pathnamep entry-point)
                                         (%canonical-extension-path entry-point)
                                         (%ensure-string entry-point))
                        :manifest-path (and (getf metadata :manifest-path)
                                            (%canonical-extension-path (getf metadata :manifest-path)))
                        :status :disabled
                        :message "disabled by /extensions disable")
                       report))
                (t
                 (%load-entry-point entry-point)
                 (%register-extension metadata scope :loaded nil)
                 (%publish-extension-loaded record-path scope)
                 (let ((record
                         (make-extension-load-record
                          :path record-path
                          :scope scope
                          :name (getf metadata :name)
                          :version (getf metadata :version)
                          :dependencies (copy-list (getf metadata :dependencies))
                          :entry-point (if (pathnamep entry-point)
                                           (%canonical-extension-path entry-point)
                                           (%ensure-string entry-point))
                          :manifest-path (and (getf metadata :manifest-path)
                                              (%canonical-extension-path (getf metadata :manifest-path)))
                          :status :loaded)))
                   (push record report)
                   (push record loaded)))))
          (error (condition)
            (let ((message (princ-to-string condition)))
              (format *error-output*
                      "Extension load failed (~A): ~A~%"
                      path-text
                      message)
              (%publish-extension-error path-text scope message)
              (%register-extension
               (if (and (string-equal (pathname-name file) "extension")
                        (string-equal (pathname-type file) "lisp"))
                   (list :name (pathname-name (%manifest-parent-directory file))
                         :version "0.0.0"
                         :dependencies '()
                         :entry-point (namestring file)
                         :manifest-path file
                         :extension-root (%manifest-parent-directory file)
                         :path file
                         :last-write-date (%safe-file-write-date file))
                   (%legacy-file->metadata file))
               scope
               :error
               message)
              (push (make-extension-load-record
                     :path path-text
                     :scope scope
                     :status :error
                     :message message)
                    report))))))
    (setf *extension-load-report* (nreverse report)
          *loaded-extensions* (nreverse loaded))
    (%rebuild-extension-watch-snapshot)
    (if start-hot-reload
        (start-extension-hot-reload :project-root project-root
                                    :global-directory global-directory
                                    :project-directory project-directory)
        (stop-extension-hot-reload))
    *extension-load-report*))

(defun reload-user-extensions (&key project-root global-directory project-directory
                                    (start-hot-reload *extension-hot-reload-enabled-p*))
  (load-user-extensions :project-root project-root
                        :global-directory global-directory
                        :project-directory project-directory
                        :start-hot-reload start-hot-reload))

(defun check-extension-hot-reload (&key project-root global-directory project-directory
                                        (reload-on-change t)
                                        (start-hot-reload *extension-hot-reload-enabled-p*))
  (let* ((candidates (%collect-extension-candidates :project-root project-root
                                                    :global-directory global-directory
                                                    :project-directory project-directory))
         (current-paths (mapcar (lambda (entry)
                                  (%canonical-extension-path (cdr entry)))
                                candidates))
         (changed-p nil))
    (dolist (path-text current-paths)
      (let ((current (%safe-file-write-date path-text))
            (previous (gethash path-text *extension-watch-snapshot* :__missing__)))
        (when (or (eq previous :__missing__)
                  (not (eql previous current)))
          (setf changed-p t))))
    (maphash (lambda (path-text _value)
               (declare (ignore _value))
               (unless (member path-text current-paths :test #'string-equal)
                 (setf changed-p t)))
             *extension-watch-snapshot*)
    (when changed-p
      (if reload-on-change
          (progn
            (reload-user-extensions :project-root project-root
                                    :global-directory global-directory
                                    :project-directory project-directory
                                    :start-hot-reload start-hot-reload)
            t)
          (progn
            (%rebuild-extension-watch-snapshot current-paths)
            t)))))

(defun start-extension-hot-reload (&key project-root global-directory project-directory)
  (when (and *extension-hot-reload-thread*
             (bordeaux-threads:thread-alive-p *extension-hot-reload-thread*))
    (return-from start-extension-hot-reload *extension-hot-reload-thread*))
  (setf *extension-hot-reload-running-p* t)
  (setf *extension-hot-reload-thread*
        (bordeaux-threads:make-thread
         (lambda ()
           (loop while *extension-hot-reload-running-p* do
             (ignore-errors
               (check-extension-hot-reload :project-root project-root
                                           :global-directory global-directory
                                           :project-directory project-directory
                                           :reload-on-change t
                                           :start-hot-reload nil))
             (sleep (max 0.1d0 *extension-hot-reload-interval-seconds*))))
         :name "amoebum-extension-hot-reload"))
  *extension-hot-reload-thread*)

(defun stop-extension-hot-reload ()
  (let ((thread *extension-hot-reload-thread*))
    (setf *extension-hot-reload-running-p* nil
          *extension-hot-reload-thread* nil)
    (when (and thread
               (bordeaux-threads:thread-alive-p thread)
               (not (eq thread (bordeaux-threads:current-thread))))
      (ignore-errors
        (bordeaux-threads:join-thread thread)))
    t))
