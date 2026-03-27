(in-package :amoebum/test)

(def-suite extension-security-suite :in amoebum-suite)
(in-suite extension-security-suite)

(defun %hash-table-alist (table)
  (loop for key being the hash-keys of table
        using (hash-value value)
        collect (cons key value)))

(defun %restore-hash-table (table entries)
  (clrhash table)
  (dolist (entry entries)
    (setf (gethash (car entry) table) (cdr entry))))

(defun %write-i241-extension (project-dir name manifest-body entry-body)
  (let* ((root (merge-pathnames
                (make-pathname :directory `(:relative ,name))
                project-dir))
         (manifest (merge-pathnames #P"extension.lisp" root))
         (entry (merge-pathnames #P"main.lisp" root)))
    (%write-text-file manifest manifest-body)
    (%write-text-file entry entry-body)
    (values root manifest entry)))

(test manifest-extensions-load-in-isolated-package
  (let* ((old-report amoebum.extensions:*extension-load-report*)
         (old-loaded amoebum.extensions:*loaded-extensions*)
         (old-discovered amoebum.extensions:*extension-last-discovered*)
         (old-global amoebum.extensions:*extensions-global-directory-override*)
         (old-project amoebum.extensions:*extensions-project-directory-override*)
         (old-prompt amoebum.extensions:*extension-permission-prompt-function*)
         (old-disabled (%hash-table-alist amoebum.extensions:*disabled-extensions*))
         (old-registry (%hash-table-alist amoebum.extensions:*extension-registry*))
         (old-watch (%hash-table-alist amoebum.extensions:*extension-watch-snapshot*))
         (old-approvals (%hash-table-alist amoebum.extensions:*extension-permission-approvals*))
         (tmp-dir (%make-temp-directory "amoebum-ext-sec-isolation"))
         (global-dir (merge-pathnames #P"global/" tmp-dir))
         (project-dir (merge-pathnames #P"project/" tmp-dir)))
    (unwind-protect
         (progn
           (setf amoebum.extensions:*extensions-global-directory-override* global-dir
                 amoebum.extensions:*extensions-project-directory-override* project-dir
                 amoebum.extensions:*extension-permission-prompt-function*
                 (lambda (_name _permission _scope _metadata)
                   (declare (ignore _name _permission _scope _metadata))
                   :allow)
                 amoebum.extensions:*extension-load-report* '()
                 amoebum.extensions:*loaded-extensions* '()
                 amoebum.extensions:*extension-last-discovered* '())
           (clrhash amoebum.extensions:*disabled-extensions*)
           (clrhash amoebum.extensions:*extension-registry*)
           (clrhash amoebum.extensions:*extension-watch-snapshot*)
           (amoebum.extensions:clear-extension-permission-approvals)

           (%write-i241-extension
            project-dir
            "isolated-ext"
            "(:name \"isolated-ext\" :version \"1.0.0\" :permissions () :entry-point \"main.lisp\")"
            "(defparameter *security-local* 41) (setf *security-local* (+ *security-local* 1))")

           (let ((report (amoebum.extensions:load-user-extensions :project-root tmp-dir :start-hot-reload nil)))
             (is (= 1 (length report)))
             (is (eq :loaded (amoebum.extensions:extension-load-record-status (first report)))))

           (let* ((entry (first (amoebum.extensions:list-extension-registry)))
                  (package-name (amoebum.extensions:extension-registry-entry-package-name entry))
                  (package (and package-name (find-package package-name))))
             (is (stringp package-name))
             (is-true (search "AMOEBUM.EXT." package-name :test #'char-equal))
             (is-true package)
             (multiple-value-bind (symbol status)
                 (find-symbol "*SECURITY-LOCAL*" package)
               (is (eq :internal status))
               (is (= 42 (symbol-value symbol))))
             (multiple-value-bind (_symbol status)
                 (find-symbol "*SECURITY-LOCAL*" (find-package :amoebum))
               (declare (ignore _symbol))
               (is (null status)))))
      (amoebum.extensions:stop-extension-hot-reload)
      (setf amoebum.extensions:*extension-load-report* old-report
            amoebum.extensions:*loaded-extensions* old-loaded
            amoebum.extensions:*extension-last-discovered* old-discovered
            amoebum.extensions:*extensions-global-directory-override* old-global
            amoebum.extensions:*extensions-project-directory-override* old-project
            amoebum.extensions:*extension-permission-prompt-function* old-prompt)
      (%restore-hash-table amoebum.extensions:*disabled-extensions* old-disabled)
      (%restore-hash-table amoebum.extensions:*extension-registry* old-registry)
      (%restore-hash-table amoebum.extensions:*extension-watch-snapshot* old-watch)
      (%restore-hash-table amoebum.extensions:*extension-permission-approvals* old-approvals)
      (%delete-directory-tree-safe tmp-dir))))

(test unsafe-operations-require-declared-permissions
  (let* ((old-report amoebum.extensions:*extension-load-report*)
         (old-loaded amoebum.extensions:*loaded-extensions*)
         (old-discovered amoebum.extensions:*extension-last-discovered*)
         (old-global amoebum.extensions:*extensions-global-directory-override*)
         (old-project amoebum.extensions:*extensions-project-directory-override*)
         (old-prompt amoebum.extensions:*extension-permission-prompt-function*)
         (old-disabled (%hash-table-alist amoebum.extensions:*disabled-extensions*))
         (old-registry (%hash-table-alist amoebum.extensions:*extension-registry*))
         (old-watch (%hash-table-alist amoebum.extensions:*extension-watch-snapshot*))
         (old-approvals (%hash-table-alist amoebum.extensions:*extension-permission-approvals*))
         (tmp-dir (%make-temp-directory "amoebum-ext-sec-unsafe"))
         (global-dir (merge-pathnames #P"global/" tmp-dir))
         (project-dir (merge-pathnames #P"project/" tmp-dir)))
    (unwind-protect
         (progn
           (setf amoebum.extensions:*extensions-global-directory-override* global-dir
                 amoebum.extensions:*extensions-project-directory-override* project-dir
                 amoebum.extensions:*extension-permission-prompt-function*
                 (lambda (_name _permission _scope _metadata)
                   (declare (ignore _name _permission _scope _metadata))
                   :allow)
                 amoebum.extensions:*extension-load-report* '()
                 amoebum.extensions:*loaded-extensions* '()
                 amoebum.extensions:*extension-last-discovered* '())
           (clrhash amoebum.extensions:*disabled-extensions*)
           (clrhash amoebum.extensions:*extension-registry*)
           (clrhash amoebum.extensions:*extension-watch-snapshot*)
           (amoebum.extensions:clear-extension-permission-approvals)

           (%write-i241-extension
            project-dir
            "unsafe-ext"
            "(:name \"unsafe-ext\" :version \"1.0.0\" :permissions () :entry-point \"main.lisp\")"
            "(sb-ext:run-program \"/bin/echo\" '(\"hello\"))")

           (let* ((report (amoebum.extensions:load-user-extensions :project-root tmp-dir :start-hot-reload nil))
                  (record (first report))
                  (message (or (amoebum.extensions:extension-load-record-message record) "")))
             (is (= 1 (length report)))
             (is (eq :error (amoebum.extensions:extension-load-record-status record)))
             (is-true (search "run-program" message :test #'char-equal))
             (is-true (search "requires permission :SHELL" message :test #'char-equal))))
      (amoebum.extensions:stop-extension-hot-reload)
      (setf amoebum.extensions:*extension-load-report* old-report
            amoebum.extensions:*loaded-extensions* old-loaded
            amoebum.extensions:*extension-last-discovered* old-discovered
            amoebum.extensions:*extensions-global-directory-override* old-global
            amoebum.extensions:*extensions-project-directory-override* old-project
            amoebum.extensions:*extension-permission-prompt-function* old-prompt)
      (%restore-hash-table amoebum.extensions:*disabled-extensions* old-disabled)
      (%restore-hash-table amoebum.extensions:*extension-registry* old-registry)
      (%restore-hash-table amoebum.extensions:*extension-watch-snapshot* old-watch)
      (%restore-hash-table amoebum.extensions:*extension-permission-approvals* old-approvals)
      (%delete-directory-tree-safe tmp-dir))))

(test permission-prompt-approve-and-deny-paths
  (let* ((old-report amoebum.extensions:*extension-load-report*)
         (old-loaded amoebum.extensions:*loaded-extensions*)
         (old-discovered amoebum.extensions:*extension-last-discovered*)
         (old-global amoebum.extensions:*extensions-global-directory-override*)
         (old-project amoebum.extensions:*extensions-project-directory-override*)
         (old-prompt amoebum.extensions:*extension-permission-prompt-function*)
         (old-disabled (%hash-table-alist amoebum.extensions:*disabled-extensions*))
         (old-registry (%hash-table-alist amoebum.extensions:*extension-registry*))
         (old-watch (%hash-table-alist amoebum.extensions:*extension-watch-snapshot*))
         (old-approvals (%hash-table-alist amoebum.extensions:*extension-permission-approvals*))
         (tmp-dir (%make-temp-directory "amoebum-ext-sec-prompt"))
         (global-dir (merge-pathnames #P"global/" tmp-dir))
         (project-dir (merge-pathnames #P"project/" tmp-dir))
         (prompt-calls 0))
    (unwind-protect
         (progn
           (setf amoebum.extensions:*extensions-global-directory-override* global-dir
                 amoebum.extensions:*extensions-project-directory-override* project-dir
                 amoebum.extensions:*extension-load-report* '()
                 amoebum.extensions:*loaded-extensions* '()
                 amoebum.extensions:*extension-last-discovered* '())
           (clrhash amoebum.extensions:*disabled-extensions*)
           (clrhash amoebum.extensions:*extension-registry*)
           (clrhash amoebum.extensions:*extension-watch-snapshot*)
           (amoebum.extensions:clear-extension-permission-approvals)

           (%write-i241-extension
            project-dir
            "prompt-ext"
            "(:name \"prompt-ext\" :version \"1.0.0\" :permissions (:shell) :entry-point \"main.lisp\")"
            "(values)")

           (setf amoebum.extensions:*extension-permission-prompt-function*
                 (lambda (_name permission _scope _metadata)
                   (declare (ignore _name _scope _metadata))
                   (when (eq permission :shell)
                     (incf prompt-calls))
                   :allow))

           (let ((first-report (amoebum.extensions:load-user-extensions :project-root tmp-dir :start-hot-reload nil))
                 (second-report (amoebum.extensions:reload-user-extensions :project-root tmp-dir :start-hot-reload nil)))
             (is (eq :loaded (amoebum.extensions:extension-load-record-status (first first-report))))
             (is (eq :loaded (amoebum.extensions:extension-load-record-status (first second-report))))
             (is (= 1 prompt-calls)))

           (%write-i241-extension
            project-dir
            "prompt-denied-ext"
            "(:name \"prompt-denied-ext\" :version \"1.0.0\" :permissions (:filesystem) :entry-point \"main.lisp\")"
            "(values)")

           (setf amoebum.extensions:*extension-permission-prompt-function*
                 (lambda (_name _permission _scope _metadata)
                   (declare (ignore _name _permission _scope _metadata))
                   :deny))

           (let* ((report (amoebum.extensions:load-user-extensions :project-root tmp-dir :start-hot-reload nil))
                  (denied-record
                    (find "prompt-denied-ext"
                          report
                          :key #'amoebum.extensions:extension-load-record-name
                          :test #'string-equal))
                  (message (or (and denied-record
                                    (amoebum.extensions:extension-load-record-message denied-record))
                               "")))
             (is-true denied-record)
             (is (eq :error (amoebum.extensions:extension-load-record-status denied-record)))
             (is-true (search "permission :FILESYSTEM denied by user" message :test #'char-equal))))
      (amoebum.extensions:stop-extension-hot-reload)
      (setf amoebum.extensions:*extension-load-report* old-report
            amoebum.extensions:*loaded-extensions* old-loaded
            amoebum.extensions:*extension-last-discovered* old-discovered
            amoebum.extensions:*extensions-global-directory-override* old-global
            amoebum.extensions:*extensions-project-directory-override* old-project
            amoebum.extensions:*extension-permission-prompt-function* old-prompt)
      (%restore-hash-table amoebum.extensions:*disabled-extensions* old-disabled)
      (%restore-hash-table amoebum.extensions:*extension-registry* old-registry)
      (%restore-hash-table amoebum.extensions:*extension-watch-snapshot* old-watch)
      (%restore-hash-table amoebum.extensions:*extension-permission-approvals* old-approvals)
      (%delete-directory-tree-safe tmp-dir))))

(test extension-security-smoke-sentinel
  (is-true t)
  (format t "EXTENSION_SECURITY_SMOKE_OK~%"))
