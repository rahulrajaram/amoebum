(in-package :amoebum)

;;; ---------------------------------------------------------------------------
;;; ASDF Extension Contract (I100)
;;;
;;; Discover, load, unload, and manage ASDF-based extensions.
;;; Discovery sources: Quicklisp local-projects, ~/.amoebum/systems/
;;; Each extension gets its own package and permission declarations.
;;; ---------------------------------------------------------------------------

;;; --- Extension Manifest ---

(defstruct (asdf-extension
            (:constructor make-asdf-extension
                (&key system-name description version
                      (status :discovered)
                      (loaded-at nil)
                      (permissions '())
                      (source-path nil))))
  (system-name "" :type string)
  (description "" :type string)
  (version "" :type string)
  (status :discovered :type keyword)
  (loaded-at nil :type (or null integer))
  (permissions '() :type list)
  (source-path nil :type (or null string pathname)))

(defvar *asdf-extension-registry* (make-hash-table :test #'equal)
  "Hash table mapping system-name -> asdf-extension.")

(defvar *asdf-extension-search-paths*
  (list (merge-pathnames #P".amoebum/systems/"
                         (user-homedir-pathname)))
  "Additional search paths for ASDF extensions.")

(defvar *asdf-extension-manifest-path* nil
  "Override path for the extension manifest file.")

;;; --- Discovery ---

(defun %asdf-extension-manifest-path (&key project-root)
  "Return the manifest file path."
  (or *asdf-extension-manifest-path*
      (merge-pathnames #P".amoebum/extensions/manifest.sexp"
                       (uiop:ensure-directory-pathname
                        (or project-root
                            (ignore-errors (uiop:getcwd))
                            *default-pathname-defaults*)))))

(defun %discover-asdf-systems-in-directory (directory)
  "Find all .asd files in DIRECTORY."
  (when (uiop:directory-exists-p directory)
    (let ((pattern (merge-pathnames "**/*.asd"
                                     (uiop:ensure-directory-pathname directory))))
      (directory pattern))))

(defun discover-asdf-extensions (&key project-root)
  "Discover ASDF extensions from configured search paths and Quicklisp."
  (let ((found '()))
    ;; Search ~/.amoebum/systems/
    (dolist (search-path *asdf-extension-search-paths*)
      (dolist (asd-file (%discover-asdf-systems-in-directory search-path))
        (let ((name (pathname-name asd-file)))
          (when (and name (not (gethash name *asdf-extension-registry*)))
            (let ((ext (make-asdf-extension
                        :system-name name
                        :source-path (namestring asd-file)
                        :status :discovered)))
              (setf (gethash name *asdf-extension-registry*) ext)
              (push ext found))))))
    ;; Search Quicklisp local-projects if available
    #+quicklisp
    (let ((ql-local (ignore-errors
                      (symbol-value (find-symbol "*LOCAL-PROJECT-DIRECTORIES*"
                                                 (find-package :ql))))))
      (when (listp ql-local)
        (dolist (dir ql-local)
          (dolist (asd-file (%discover-asdf-systems-in-directory dir))
            (let ((name (pathname-name asd-file)))
              (when (and name (not (gethash name *asdf-extension-registry*)))
                (let ((ext (make-asdf-extension
                            :system-name name
                            :source-path (namestring asd-file)
                            :status :discovered)))
                  (setf (gethash name *asdf-extension-registry*) ext)
                  (push ext found))))))))
    (nreverse found)))

;;; --- Lifecycle ---

(defun load-asdf-extension (system-name &key (verbose nil))
  "Load an ASDF extension by system name."
  (let ((ext (gethash system-name *asdf-extension-registry*)))
    ;; Register source path if known
    (when (and ext (asdf-extension-source-path ext))
      (let ((dir (uiop:pathname-directory-pathname
                  (pathname (asdf-extension-source-path ext)))))
        (when (uiop:directory-exists-p dir)
          (pushnew dir asdf:*central-registry* :test #'equal))))
    (handler-case
        (progn
          (asdf:load-system system-name :verbose verbose)
          (let ((entry (or ext (make-asdf-extension :system-name system-name))))
            (setf (asdf-extension-status entry) :loaded
                  (asdf-extension-loaded-at entry) (get-universal-time))
            ;; Try to get description from ASDF
            (let ((sys (asdf:find-system system-name nil)))
              (when sys
                (setf (asdf-extension-description entry)
                      (or (asdf:system-description sys) "")
                      (asdf-extension-version entry)
                      (or (asdf:component-version sys) ""))))
            (setf (gethash system-name *asdf-extension-registry*) entry)
            entry))
      (error (c)
        (let ((entry (or ext (make-asdf-extension :system-name system-name))))
          (setf (asdf-extension-status entry) :error)
          (setf (gethash system-name *asdf-extension-registry*) entry)
          (error "Failed to load extension ~A: ~A" system-name c))))))

(defun unload-asdf-extension (system-name)
  "Mark an ASDF extension as unloaded. Note: CL cannot truly unload systems."
  (let ((ext (gethash system-name *asdf-extension-registry*)))
    (when ext
      (setf (asdf-extension-status ext) :unloaded))
    ext))

(defun reload-asdf-extension (system-name &key (verbose nil))
  "Reload an ASDF extension."
  (unload-asdf-extension system-name)
  (load-asdf-extension system-name :verbose verbose))

;;; --- Registry Queries ---

(defun list-asdf-extensions (&key (status nil))
  "List all known ASDF extensions, optionally filtered by STATUS."
  (let ((result '()))
    (maphash (lambda (name ext)
               (declare (ignore name))
               (when (or (null status)
                         (eq status (asdf-extension-status ext)))
                 (push ext result)))
             *asdf-extension-registry*)
    (sort result #'string< :key #'asdf-extension-system-name)))

(defun find-asdf-extension (system-name)
  "Find an ASDF extension by system name."
  (gethash system-name *asdf-extension-registry*))

(defun clear-asdf-extensions ()
  "Clear the extension registry."
  (clrhash *asdf-extension-registry*))

;;; --- Manifest Persistence ---

(defun save-asdf-extension-manifest (&key project-root)
  "Save the extension manifest to disk."
  (let ((path (%asdf-extension-manifest-path :project-root project-root))
        (data '()))
    (maphash (lambda (name ext)
               (push (list :system-name name
                           :description (asdf-extension-description ext)
                           :version (asdf-extension-version ext)
                           :status (asdf-extension-status ext)
                           :source-path (asdf-extension-source-path ext)
                           :permissions (asdf-extension-permissions ext))
                     data))
             *asdf-extension-registry*)
    (ensure-directories-exist path)
    (with-open-file (stream path :direction :output
                                 :if-exists :supersede
                                 :if-does-not-exist :create
                                 :external-format :utf-8)
      (let ((*print-pretty* t))
        (prin1 data stream)
        (terpri stream)))
    path))

(defun load-asdf-extension-manifest (&key project-root)
  "Load the extension manifest from disk."
  (let ((path (%asdf-extension-manifest-path :project-root project-root)))
    (when (probe-file path)
      (handler-case
          (with-open-file (stream path :direction :input :external-format :utf-8)
            (let ((data (read stream nil nil)))
              (when (listp data)
                (dolist (entry data)
                  (let ((name (getf entry :system-name)))
                    (when name
                      (setf (gethash name *asdf-extension-registry*)
                            (make-asdf-extension
                             :system-name name
                             :description (or (getf entry :description) "")
                             :version (or (getf entry :version) "")
                             :status (or (getf entry :status) :discovered)
                             :source-path (getf entry :source-path)
                             :permissions (or (getf entry :permissions) '())))))))))
        (error () nil)))))
