(in-package :amoebum)

(defparameter *extension-load-report* '())
(defparameter *loaded-extensions* '())
(defparameter *extension-last-discovered* '())
(defparameter *disabled-extensions* (make-hash-table :test #'equal))

;; Test and smoke harnesses can bind these to avoid mutating real user paths.
(defparameter *extensions-global-directory-override* nil)
(defparameter *extensions-project-directory-override* nil)

(defstruct (extension-load-record
            (:constructor make-extension-load-record
                (&key path
                 scope
                 status
                 message
                 (timestamp (get-universal-time)))))
  path
  (scope :project :type keyword)
  (status :loaded :type keyword)
  message
  (timestamp 0 :type integer))

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

(defun %list-extension-files (directory)
  (if (and directory (probe-file directory))
      (sort (remove-if #'uiop:directory-pathname-p
                       (directory (merge-pathnames #P"*.lisp" directory)))
            #'string<
            :key #'%extension-sort-key)
      '()))

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

(defun discover-user-extension-files (&key project-root global-directory project-directory)
  (let* ((global-path (%global-extension-directory :global-directory global-directory))
         (project-path (%project-extension-directory :project-root project-root
                                                     :project-directory project-directory))
         (global-files (%list-extension-files global-path))
         (project-files (%list-extension-files project-path)))
    (values global-files project-files)))

(defun extension-disabled-p (path)
  (not (null (gethash (%extension-key path) *disabled-extensions*))))

(defun list-extension-report ()
  (copy-list *extension-load-report*))

(defun list-loaded-extensions ()
  (copy-list *loaded-extensions*))

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
        (remember (extension-load-record-path entry))))
    (nreverse paths)))

(defun disable-user-extension (target)
  (let* ((trimmed (%extension-trim target))
         (known-paths (known-user-extension-paths))
         (disabled '()))
    (cond
      ((zerop (length trimmed))
       (values '() 0))
      ((string-equal trimmed "all")
       (dolist (path-text known-paths)
         (setf (gethash (%extension-key path-text) *disabled-extensions*) t)
         (push path-text disabled))
       (values (nreverse disabled) (length disabled)))
      (t
       (dolist (path-text known-paths)
         (when (%extension-match-target-p trimmed path-text)
           (setf (gethash (%extension-key path-text) *disabled-extensions*) t)
           (push path-text disabled)))
       (when (and (null disabled) (probe-file trimmed))
         (let ((resolved (%canonical-extension-path trimmed)))
           (when (plusp (length resolved))
             (setf (gethash (%extension-key resolved) *disabled-extensions*) t)
             (push resolved disabled))))
       (values (nreverse disabled) (length disabled))))))

(defun %publish-extension-loaded (path scope)
  (publish (current-event-bus)
           (make-extension-loaded-event :path path :scope scope)))

(defun %publish-extension-error (path scope condition-text)
  (publish (current-event-bus)
           (make-extension-error-event :path path
                                       :scope scope
                                       :condition condition-text)))

(defun load-user-extensions (&key project-root global-directory project-directory)
  (multiple-value-bind (global-files project-files)
      (discover-user-extension-files :project-root project-root
                                     :global-directory global-directory
                                     :project-directory project-directory)
    (let* ((candidates
             (append (loop for file in global-files collect (cons :global file))
                     (loop for file in project-files collect (cons :project file))))
           (report '())
           (loaded '()))
      (setf *extension-last-discovered*
            (mapcar (lambda (entry)
                      (%canonical-extension-path (cdr entry)))
                    candidates))
      (dolist (entry candidates)
        (let* ((scope (car entry))
               (file (cdr entry))
               (path-text (%canonical-extension-path file)))
          (cond
            ((extension-disabled-p path-text)
             (push (make-extension-load-record
                    :path path-text
                    :scope scope
                    :status :disabled
                    :message "disabled by /extensions disable")
                   report))
            (t
             (handler-case
                 (progn
                   (load file :verbose nil :print nil)
                   (%publish-extension-loaded path-text scope)
                   (let ((record (make-extension-load-record
                                  :path path-text
                                  :scope scope
                                  :status :loaded)))
                     (push record report)
                     (push record loaded)))
               (error (condition)
                 (let ((message (princ-to-string condition)))
                   (format *error-output*
                           "Extension load failed (~A): ~A~%"
                           path-text
                           message)
                   (%publish-extension-error path-text scope message)
                   (push (make-extension-load-record
                          :path path-text
                          :scope scope
                          :status :error
                          :message message)
                         report))))))))
      (setf *extension-load-report* (nreverse report)
            *loaded-extensions* (nreverse loaded))
      *extension-load-report*)))

(defun reload-user-extensions (&key project-root global-directory project-directory)
  (load-user-extensions :project-root project-root
                        :global-directory global-directory
                        :project-directory project-directory))
