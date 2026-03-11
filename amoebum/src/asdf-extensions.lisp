(in-package :amoebum)

;;; ---------------------------------------------------------------------------
;;; ASDF Extension Contract (I239)
;;;
;;; Discover, load, unload, and manage ASDF-based extensions.
;;; Discovery sources: ~/.amoebum/systems/, .amoebum/systems/, optional
;;; extra search paths.
;;; Lifecycle support includes dependency resolution with version constraints,
;;; initialize/shutdown hooks, and unload cleanup for tools/hooks/skills.
;;; ---------------------------------------------------------------------------

;;; --- Extension Manifest ---

(defstruct (asdf-extension
            (:constructor make-asdf-extension
                (&key system-name description version
                      (status :discovered)
                      (loaded-at nil)
                      (permissions '())
                      (source-path nil)
                      (dependencies '())
                      (initialize-hook nil)
                      (shutdown-hook nil)
                      (registered-tools '())
                      (registered-hooks '())
                      (registered-skills '())
                      (registered-slash-commands '()))))
  (system-name "" :type string)
  (description "" :type string)
  (version "" :type string)
  (status :discovered :type keyword)
  (loaded-at nil :type (or null integer))
  (permissions '() :type list)
  (source-path nil :type (or null string pathname))
  (dependencies '() :type list)
  initialize-hook
  shutdown-hook
  (registered-tools '() :type list)
  (registered-hooks '() :type list)
  (registered-skills '() :type list)
  (registered-slash-commands '() :type list))

(defvar *asdf-extension-registry* (make-hash-table :test #'equal)
  "Hash table mapping system-name -> asdf-extension.")

(defvar *asdf-extension-search-paths*
  (list (merge-pathnames #P".amoebum/systems/"
                         (user-homedir-pathname)))
  "Additional search paths for ASDF extensions.")

(defvar *asdf-extension-manifest-path* nil
  "Override path for the extension manifest file.")

;;; --- Discovery ---

(defun %asdf-trim (value)
  (if (stringp value)
      (string-trim '(#\Space #\Tab #\Newline #\Return) value)
      ""))

(defun %normalize-asdf-system-name (value)
  (let ((text (%asdf-trim
               (cond
                 ((null value) "")
                 ((stringp value) value)
                 ((or (symbolp value) (keywordp value)) (string value))
                 (t (princ-to-string value))))))
    (string-downcase text)))

(defun %asdf-extension-manifest-path (&key project-root)
  "Return the manifest file path."
  (or *asdf-extension-manifest-path*
      (merge-pathnames #P".amoebum/extensions/manifest.sexp"
                       (uiop:ensure-directory-pathname
                        (or project-root
                            (ignore-errors (uiop:getcwd))
                            *default-pathname-defaults*)))))

(defun %asdf-project-system-directory (&key project-root)
  (let* ((root (or project-root
                   (ignore-errors (uiop:getcwd))
                   *default-pathname-defaults*))
         (directory (ignore-errors
                      (uiop:ensure-directory-pathname
                       (or (ignore-errors (truename root))
                           root)))))
    (and directory
         (merge-pathnames #P".amoebum/systems/" directory))))

(defun %asdf-discovery-paths (&key project-root)
  (let ((seen (make-hash-table :test #'equal))
        (paths '()))
    (labels ((remember (path)
               (when path
                 (let* ((directory (ignore-errors (uiop:ensure-directory-pathname path)))
                        (text (and directory
                                   (namestring
                                    (or (ignore-errors (truename directory))
                                        directory)))))
                   (when (and text (plusp (length (%asdf-trim text))))
                     (let ((key (string-downcase text)))
                       (unless (gethash key seen)
                         (setf (gethash key seen) t)
                         (push (pathname text) paths))))))))
      (remember (%asdf-project-system-directory :project-root project-root))
      (dolist (search-path *asdf-extension-search-paths*)
        (remember search-path)))
    (nreverse paths)))

(defun %discover-asdf-systems-in-directory (directory)
  "Find all .asd files in DIRECTORY."
  (when (uiop:directory-exists-p directory)
    (let ((pattern (merge-pathnames "**/*.asd"
                                    (uiop:ensure-directory-pathname directory))))
      (directory pattern))))

(defun %asdf-dependency-entry (name constraint)
  (let ((dep-name (%normalize-asdf-system-name name))
        (dep-constraint (%asdf-trim
                         (cond
                           ((null constraint) "")
                           ((stringp constraint) constraint)
                           (t (princ-to-string constraint))))))
    (when (plusp (length dep-name))
      (cons dep-name dep-constraint))))

(defun %parse-asdf-dependency-spec (spec)
  (flet ((single (name &optional (constraint ""))
           (let ((entry (%asdf-dependency-entry name constraint)))
             (if entry (list entry) '())))
         (plist-constraint (plist)
           (or (getf plist :constraint)
               (let ((version (getf plist :version)))
                 (and version (format nil "=~A" version)))
               (let ((minimum (getf plist :minimum)))
                 (and minimum (format nil ">=~A" minimum)))
               (let ((range (getf plist :range)))
                 (when (and (listp range) (= (length range) 2))
                   (format nil ">=~A <=~A" (first range) (second range))))
               "")))
    (cond
      ((or (stringp spec) (symbolp spec) (keywordp spec))
       (single spec))
      ((and (consp spec) (keywordp (first spec)))
       (case (first spec)
         (:version (single (second spec) (format nil "=~A" (third spec))))
         ((:minimum :minimum-version :>=)
          (single (second spec) (format nil ">=~A" (third spec))))
         (:range
          (single (second spec)
                  (format nil ">=~A <=~A" (third spec) (fourth spec))))
         (:feature
          ;; ASDF feature-gated dependency. Keep the system name for ordering.
          (single (third spec)))
         (:require
          ;; ASDF module requirement; not an extension dependency.
          '())
         (otherwise
          (single (second spec) (or (third spec) "")))))
      ((consp spec)
       (let ((name (first spec))
             (rest (rest spec)))
         (cond
           ((null rest)
            (single name))
           ((and (= (length rest) 1)
                 (stringp (first rest)))
            (single name (format nil "=~A" (first rest))))
           ((and (keywordp (first rest))
                 (or (member :constraint rest :test #'eq)
                     (member :version rest :test #'eq)
                     (member :minimum rest :test #'eq)
                     (member :range rest :test #'eq)))
            (single name (plist-constraint rest)))
           (t
            (single name)))))
      (t
       '()))))

(defun %merge-asdf-dependencies (dependencies)
  (let ((table (make-hash-table :test #'equal))
        (order '()))
    (flet ((merge-constraint (existing incoming)
             (let ((left (%asdf-trim existing))
                   (right (%asdf-trim incoming)))
               (cond
                 ((zerop (length left)) right)
                 ((zerop (length right)) left)
                 (t (format nil "~A ~A" left right))))))
      (dolist (entry dependencies)
        (destructuring-bind (name . constraint) entry
          (unless (gethash name table)
            (push name order))
          (setf (gethash name table)
                (merge-constraint (gethash name table) constraint)))))
    (nreverse
     (mapcar (lambda (name)
               (cons name (or (gethash name table) "")))
             order))))

(defun %extract-asdf-system-dependencies (system)
  (let ((raw (ignore-errors (asdf:system-depends-on system))))
    (%merge-asdf-dependencies
     (loop for spec in raw
           append (%parse-asdf-dependency-spec spec)))))

(defun %asdf-system-metadata (system-name)
  (let ((system (ignore-errors (asdf:find-system system-name nil))))
    (values (or (and system (asdf:system-description system)) "")
            (or (and system (asdf:component-version system)) "0.0.0")
            (if system
                (%extract-asdf-system-dependencies system)
                '()))))

(defun %register-discovered-asdf-system (system-name asd-file)
  (let ((key (%normalize-asdf-system-name system-name)))
    (unless (plusp (length key))
      (return-from %register-discovered-asdf-system nil))
    (ignore-errors (asdf:load-asd asd-file))
    (multiple-value-bind (description version dependencies)
        (%asdf-system-metadata key)
      (let ((entry (or (gethash key *asdf-extension-registry*)
                       (make-asdf-extension :system-name key))))
        (setf (asdf-extension-source-path entry) (namestring asd-file)
              (asdf-extension-description entry) description
              (asdf-extension-version entry) version
              (asdf-extension-dependencies entry) dependencies
              (asdf-extension-status entry) (or (asdf-extension-status entry)
                                                :discovered))
        (setf (gethash key *asdf-extension-registry*) entry)
        entry))))

(defun discover-asdf-extensions (&key project-root)
  "Discover ASDF extensions from ~/.amoebum/systems and .amoebum/systems."
  (let ((found '()))
    (dolist (search-path (%asdf-discovery-paths :project-root project-root))
      (dolist (asd-file (%discover-asdf-systems-in-directory search-path))
        (let ((name (pathname-name asd-file)))
          (when name
            (let* ((key (%normalize-asdf-system-name name))
                   (already (gethash key *asdf-extension-registry*))
                   (ext (%register-discovered-asdf-system key asd-file)))
              (when (and ext (null already))
                (push ext found)))))))
    ;; Search Quicklisp local-projects if available.
    #+quicklisp
    (let ((ql-local (ignore-errors
                      (symbol-value (find-symbol "*LOCAL-PROJECT-DIRECTORIES*"
                                                 (find-package :ql))))))
      (when (listp ql-local)
        (dolist (dir ql-local)
          (dolist (asd-file (%discover-asdf-systems-in-directory dir))
            (let ((name (pathname-name asd-file)))
              (when name
                (let* ((key (%normalize-asdf-system-name name))
                       (already (gethash key *asdf-extension-registry*))
                       (ext (%register-discovered-asdf-system key asd-file)))
                  (when (and ext (null already))
                    (push ext found)))))))))
    (nreverse found)))

;;; --- Lifecycle ---

(defun %hash-table-keys (table)
  (let ((result '()))
    (when (hash-table-p table)
      (maphash (lambda (key _value)
                 (declare (ignore _value))
                 (push key result))
               table))
    (nreverse result)))

(defun %bound-hash-table-keys (symbol)
  (if (and (boundp symbol) (hash-table-p (symbol-value symbol)))
      (%hash-table-keys (symbol-value symbol))
      '()))

(defun %current-hook-keys ()
  (let ((entries (ignore-errors (list-hooks))))
    (loop for entry in entries
          collect (cons (hook-entry-hook-point entry)
                        (hook-entry-hook-id entry)))))

(defun %capture-asdf-registration-snapshot ()
  (list :tools (%bound-hash-table-keys '*tool-metadata*)
        :hooks (%current-hook-keys)
        :skills (%bound-hash-table-keys '*skill-registry*)
        :slash-commands (%bound-hash-table-keys '*slash-command-registry*)))

(defun %list-difference (after before &key (test #'equal))
  (loop for item in after
        unless (member item before :test test)
          collect item))

(defun %record-asdf-registration-delta (extension before after)
  (setf (asdf-extension-registered-tools extension)
        (%list-difference (getf after :tools) (getf before :tools) :test #'equal)
        (asdf-extension-registered-hooks extension)
        (%list-difference (getf after :hooks) (getf before :hooks) :test #'equal)
        (asdf-extension-registered-skills extension)
        (%list-difference (getf after :skills) (getf before :skills) :test #'equal)
        (asdf-extension-registered-slash-commands extension)
        (%list-difference (getf after :slash-commands)
                          (getf before :slash-commands)
                          :test #'equal))
  extension)

(defun %candidate-hook-packages (system-name)
  (let* ((name (string-upcase (%normalize-asdf-system-name system-name)))
         (slash->dash (substitute #\- #\/ name))
         (dot->dash (substitute #\- #\. name)))
    (remove-duplicates (list name slash->dash dot->dash) :test #'string=)))

(defun %find-extension-hook (system-name hook-name)
  (let ((hook-symbol-name (string-upcase hook-name)))
    (or
     (loop for package-name in (%candidate-hook-packages system-name)
           for package = (find-package package-name)
           when package
             do (multiple-value-bind (symbol _status)
                    (find-symbol hook-symbol-name package)
                  (declare (ignore _status))
                  (when (and symbol (fboundp symbol))
                    (return symbol))))
     (let ((needle (%normalize-asdf-system-name system-name)))
       (loop for package in (list-all-packages)
             for package-name = (string-downcase (package-name package))
             when (search needle package-name :test #'char=)
               do (multiple-value-bind (symbol _status)
                      (find-symbol hook-symbol-name package)
                    (declare (ignore _status))
                    (when (and symbol (fboundp symbol))
                      (return symbol))))))))

(defun %invoke-extension-hook (hook extension)
  (when (and hook (fboundp hook))
    (handler-case
        (funcall hook extension)
      (program-error ()
        ;; Allow zero-arity hook implementations.
        (funcall hook)))))

(defun %register-asd-source (source-path)
  (when source-path
    (let* ((asd-file (pathname source-path))
           (directory (uiop:pathname-directory-pathname asd-file)))
      (when (uiop:directory-exists-p directory)
        (pushnew directory asdf:*central-registry* :test #'equal))
      (when (probe-file asd-file)
        (ignore-errors (asdf:load-asd asd-file))))))

(defun %collect-asdf-extension-closure (root-name)
  (let ((seen (make-hash-table :test #'equal))
        (collected '()))
    (labels ((visit (name)
               (let* ((key (%normalize-asdf-system-name name))
                      (entry (gethash key *asdf-extension-registry*)))
                 (unless entry
                   (error "Unknown extension system ~A." name))
                 (unless (gethash key seen)
                   (setf (gethash key seen) t)
                   (dolist (dependency (asdf-extension-dependencies entry))
                     (let ((dep-name (%normalize-asdf-system-name (car dependency))))
                       (when (gethash dep-name *asdf-extension-registry*)
                         (visit dep-name))))
                   (push entry collected)))))
      (visit root-name))
    (nreverse collected)))

(defun %asdf-extension->manifest (extension known-table)
  (make-extension-manifest
   :name (asdf-extension-system-name extension)
   :version (or (%asdf-trim (asdf-extension-version extension)) "0.0.0")
   :dependencies
   (loop for dependency in (asdf-extension-dependencies extension)
         for dep-name = (%normalize-asdf-system-name (car dependency))
         for dep-constraint = (%asdf-trim (cdr dependency))
         when (gethash dep-name known-table)
           collect (cons dep-name dep-constraint))
   :entry-point (or (asdf-extension-source-path extension)
                    (asdf-extension-system-name extension))))

(defun %resolve-asdf-load-plan (root-name)
  (let* ((closure (%collect-asdf-extension-closure root-name))
         (known (make-hash-table :test #'equal))
         (manifests '()))
    (dolist (entry closure)
      (setf (gethash (asdf-extension-system-name entry) known) t))
    (dolist (entry closure)
      (push (%asdf-extension->manifest entry known) manifests))
    (multiple-value-bind (ordered _report)
        (resolve-extension-manifests (nreverse manifests) :errorp t)
      (declare (ignore _report))
      (mapcar #'extension-manifest-name ordered))))

(defun %load-asdf-extension-once (extension &key (verbose nil))
  (let* ((system-name (asdf-extension-system-name extension))
         (before (%capture-asdf-registration-snapshot)))
    (%register-asd-source (asdf-extension-source-path extension))
    (asdf:load-system system-name :verbose verbose)
    (multiple-value-bind (description version dependencies)
        (%asdf-system-metadata system-name)
      (setf (asdf-extension-description extension) description
            (asdf-extension-version extension) version
            (asdf-extension-dependencies extension) dependencies))
    (let ((initialize-hook (%find-extension-hook system-name "INITIALIZE-EXTENSION")))
      (setf (asdf-extension-initialize-hook extension) initialize-hook)
      (%invoke-extension-hook initialize-hook extension))
    (let ((after (%capture-asdf-registration-snapshot)))
      (%record-asdf-registration-delta extension before after))
    (setf (asdf-extension-status extension) :loaded
          (asdf-extension-loaded-at extension) (get-universal-time))
    (setf (gethash system-name *asdf-extension-registry*) extension)
    extension))

(defun %unregister-extension-tools (extension)
  (let ((toolset (and (boundp '*toolset*) *toolset*)))
    (dolist (tool-name (asdf-extension-registered-tools extension))
      (let ((key (%normalize-asdf-system-name tool-name)))
        (when (boundp '*tool-metadata*)
          (remhash key *tool-metadata*))
        (when (boundp '*tool-history*)
          (remhash key *tool-history*))
        (when (and toolset (pseudopod:toolset-p toolset))
          (remhash key (pseudopod::toolset-table toolset))))))
  t)

(defun %unregister-extension-hooks (extension)
  (dolist (hook-key (asdf-extension-registered-hooks extension))
    (ignore-errors
      (unregister-hook (car hook-key) (cdr hook-key))))
  t)

(defun %unregister-extension-skills (extension)
  (dolist (skill-name (asdf-extension-registered-skills extension))
    (when (boundp '*skill-registry*)
      (remhash (%normalize-asdf-system-name skill-name) *skill-registry*)))
  t)

(defun %unregister-extension-slash-commands (extension)
  (dolist (command-name (asdf-extension-registered-slash-commands extension))
    (when (boundp '*slash-command-registry*)
      (remhash (%normalize-asdf-system-name command-name) *slash-command-registry*)))
  t)

(defun load-asdf-extension (system-name &key (verbose nil) project-root)
  "Load an ASDF extension by system name, respecting dependency order."
  (let* ((key (%normalize-asdf-system-name system-name)))
    (discover-asdf-extensions :project-root project-root)
    (unless (gethash key *asdf-extension-registry*)
      (when (ignore-errors (asdf:find-system key nil))
        (let ((entry (make-asdf-extension :system-name key)))
          (multiple-value-bind (description version dependencies)
              (%asdf-system-metadata key)
            (setf (asdf-extension-description entry) description
                  (asdf-extension-version entry) version
                  (asdf-extension-dependencies entry) dependencies))
          (setf (gethash key *asdf-extension-registry*) entry))))
    (let ((root (gethash key *asdf-extension-registry*)))
      (unless root
        (error "Unknown extension system ~A. Discover it under ~~/.amoebum/systems/ or .amoebum/systems/."
               system-name))
      (handler-case
          (let ((plan (%resolve-asdf-load-plan key)))
            (dolist (name plan)
              (let ((entry (gethash name *asdf-extension-registry*)))
                (when (and entry (not (eq (asdf-extension-status entry) :loaded)))
                  (%load-asdf-extension-once entry :verbose verbose))))
            (or (gethash key *asdf-extension-registry*) root))
        (error (c)
          (setf (asdf-extension-status root) :error)
          (setf (gethash key *asdf-extension-registry*) root)
          (error "Failed to load extension ~A: ~A" system-name c))))))

(defun unload-asdf-extension (system-name)
  "Unload extension lifecycle state and deregister contributed tools/hooks."
  (let* ((key (%normalize-asdf-system-name system-name))
         (extension (gethash key *asdf-extension-registry*)))
    (when extension
      (handler-case
          (progn
            (let ((shutdown-hook (%find-extension-hook key "SHUTDOWN-EXTENSION")))
              (setf (asdf-extension-shutdown-hook extension) shutdown-hook)
              (%invoke-extension-hook shutdown-hook extension))
            (%unregister-extension-tools extension)
            (%unregister-extension-hooks extension)
            (%unregister-extension-skills extension)
            (%unregister-extension-slash-commands extension)
            (setf (asdf-extension-status extension) :unloaded
                  (asdf-extension-loaded-at extension) nil
                  (asdf-extension-registered-tools extension) '()
                  (asdf-extension-registered-hooks extension) '()
                  (asdf-extension-registered-skills extension) '()
                  (asdf-extension-registered-slash-commands extension) '())
            (setf (gethash key *asdf-extension-registry*) extension))
        (error (c)
          (setf (asdf-extension-status extension) :error)
          (setf (gethash key *asdf-extension-registry*) extension)
          (error "Failed to unload extension ~A: ~A" system-name c))))
    extension))

(defun reload-asdf-extension (system-name &key (verbose nil) project-root)
  "Reload an ASDF extension."
  (unload-asdf-extension system-name)
  (load-asdf-extension system-name :verbose verbose :project-root project-root))

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
  (gethash (%normalize-asdf-system-name system-name) *asdf-extension-registry*))

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
                           :permissions (asdf-extension-permissions ext)
                           :dependencies (copy-tree (asdf-extension-dependencies ext))
                           :initialize-hook (and (asdf-extension-initialize-hook ext)
                                                 (princ-to-string
                                                  (asdf-extension-initialize-hook ext)))
                           :shutdown-hook (and (asdf-extension-shutdown-hook ext)
                                               (princ-to-string
                                                (asdf-extension-shutdown-hook ext)))
                           :registered-tools (copy-list (asdf-extension-registered-tools ext))
                           :registered-hooks (copy-tree (asdf-extension-registered-hooks ext))
                           :registered-skills (copy-list (asdf-extension-registered-skills ext))
                           :registered-slash-commands
                           (copy-list (asdf-extension-registered-slash-commands ext)))
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
                  (let ((name (%normalize-asdf-system-name (getf entry :system-name))))
                    (when (plusp (length name))
                      (setf (gethash name *asdf-extension-registry*)
                            (make-asdf-extension
                             :system-name name
                             :description (or (getf entry :description) "")
                             :version (or (getf entry :version) "")
                             :status (or (getf entry :status) :discovered)
                             :source-path (getf entry :source-path)
                             :permissions (or (getf entry :permissions) '())
                             :dependencies (or (getf entry :dependencies) '())
                             :initialize-hook (getf entry :initialize-hook)
                             :shutdown-hook (getf entry :shutdown-hook)
                             :registered-tools (or (getf entry :registered-tools) '())
                             :registered-hooks (or (getf entry :registered-hooks) '())
                             :registered-skills (or (getf entry :registered-skills) '())
                             :registered-slash-commands
                             (or (getf entry :registered-slash-commands) '())))))))))
        (error () nil)))))
