(in-package :amoebum/test)

;;; ============================================================
;;; I239: Extension Lifecycle Management
;;; ============================================================

(def-suite extension-lifecycle-suite :in amoebum-suite)
(in-suite extension-lifecycle-suite)

(test extension-registry-entry-struct
  "extension-registry-entry should be constructable."
  (let ((entry (amoebum::make-extension-registry-entry
                :name "test-ext"
                :version "1.0.0"
                :dependencies '("dep-a")
                :scope :project
                :status :loaded)))
    (is (amoebum::extension-registry-entry-p entry))
    (is (string= "test-ext" (amoebum::extension-registry-entry-name entry)))
    (is (string= "1.0.0" (amoebum::extension-registry-entry-version entry)))
    (is (equal '("dep-a") (amoebum::extension-registry-entry-dependencies entry)))
    (is (eq :project (amoebum::extension-registry-entry-scope entry)))
    (is (eq :loaded (amoebum::extension-registry-entry-status entry)))))

(test extension-registry-list-and-clear
  "Extension registry should support list and clear."
  (let ((old-registry (copy-list (amoebum:list-extension-registry))))
    (unwind-protect
         (progn
           (clrhash amoebum::*extension-registry*)
           (setf (gethash "test-lifecycle"  amoebum::*extension-registry*)
                 (amoebum::make-extension-registry-entry
                  :name "test-lifecycle"
                  :version "0.1.0"
                  :status :loaded))
           (let ((entries (amoebum:list-extension-registry)))
             (is (= 1 (length entries)))
             (is (string= "test-lifecycle"
                           (amoebum::extension-registry-entry-name (first entries))))))
      ;; Restore
      (clrhash amoebum::*extension-registry*)
      (dolist (entry old-registry)
        (setf (gethash (string-downcase (amoebum::extension-registry-entry-name entry))
                       amoebum::*extension-registry*)
              entry)))))

(test load-user-extensions-with-manifest
  "load-user-extensions should load extensions with manifest."
  (let* ((old-report amoebum:*extension-load-report*)
         (old-loaded amoebum:*loaded-extensions*)
         (old-discovered amoebum::*extension-last-discovered*)
         (old-global amoebum:*extensions-global-directory-override*)
         (old-project amoebum:*extensions-project-directory-override*)
         (disabled-keys (%hash-table-keys amoebum:*disabled-extensions*))
         (tmp-dir (%make-temp-directory "amoebum-ext-lifecycle"))
         (global-dir (merge-pathnames #P"global/" tmp-dir))
         (project-dir (merge-pathnames #P"project/" tmp-dir))
         (ext-root (merge-pathnames #P"test-ext/" project-dir))
         (manifest-path (merge-pathnames #P"extension.lisp" ext-root))
         (entry-path (merge-pathnames #P"main.lisp" ext-root)))
    (unwind-protect
         (progn
           (setf amoebum:*extensions-global-directory-override* global-dir
                 amoebum:*extensions-project-directory-override* project-dir)
           (clrhash amoebum:*disabled-extensions*)
           (%write-text-file manifest-path
                             "(:name \"test-ext\" :version \"1.0.0\" :dependencies () :entry-point \"main.lisp\")")
           (%write-text-file entry-path
                             "(in-package :amoebum/test)")
           (let ((report (amoebum:load-user-extensions
                          :project-root tmp-dir
                          :start-hot-reload nil)))
             (is (listp report))
             (is (= 1 (length report)))
             (is (eq :loaded (amoebum::extension-load-record-status (first report))))))
      (setf amoebum:*extension-load-report* old-report
            amoebum:*loaded-extensions* old-loaded
            amoebum::*extension-last-discovered* old-discovered
            amoebum:*extensions-global-directory-override* old-global
            amoebum:*extensions-project-directory-override* old-project)
      (clrhash amoebum:*disabled-extensions*)
      (dolist (key disabled-keys)
        (setf (gethash key amoebum:*disabled-extensions*) t))
      (%delete-directory-tree-safe tmp-dir))))

(test reload-user-extensions-reloads
  "reload-user-extensions should re-discover and reload."
  (let* ((old-report amoebum:*extension-load-report*)
         (old-loaded amoebum:*loaded-extensions*)
         (old-discovered amoebum::*extension-last-discovered*)
         (old-global amoebum:*extensions-global-directory-override*)
         (old-project amoebum:*extensions-project-directory-override*)
         (disabled-keys (%hash-table-keys amoebum:*disabled-extensions*))
         (tmp-dir (%make-temp-directory "amoebum-ext-reload"))
         (global-dir (merge-pathnames #P"global/" tmp-dir))
         (project-dir (merge-pathnames #P"project/" tmp-dir)))
    (unwind-protect
         (progn
           (setf amoebum:*extensions-global-directory-override* global-dir
                 amoebum:*extensions-project-directory-override* project-dir)
           (clrhash amoebum:*disabled-extensions*)
           ;; Initially no extensions
           (amoebum:load-user-extensions :project-root tmp-dir :start-hot-reload nil)
           (is (= 0 (length amoebum:*extension-load-report*)))
           ;; Add an extension and reload
           (let ((ext-root (merge-pathnames #P"new-ext/" project-dir)))
             (%write-text-file (merge-pathnames #P"extension.lisp" ext-root)
                               "(:name \"new-ext\" :version \"0.1\" :entry-point \"main.lisp\")")
             (%write-text-file (merge-pathnames #P"main.lisp" ext-root)
                               "(in-package :amoebum/test)"))
           (amoebum:reload-user-extensions :project-root tmp-dir :start-hot-reload nil)
           (is (= 1 (length amoebum:*extension-load-report*))))
      (setf amoebum:*extension-load-report* old-report
            amoebum:*loaded-extensions* old-loaded
            amoebum::*extension-last-discovered* old-discovered
            amoebum:*extensions-global-directory-override* old-global
            amoebum:*extensions-project-directory-override* old-project)
      (clrhash amoebum:*disabled-extensions*)
      (dolist (key disabled-keys)
        (setf (gethash key amoebum:*disabled-extensions*) t))
      (%delete-directory-tree-safe tmp-dir))))

(test extension-load-error-handled-gracefully
  "Extensions that fail to load should be recorded as :error."
  (let* ((old-report amoebum:*extension-load-report*)
         (old-loaded amoebum:*loaded-extensions*)
         (old-discovered amoebum::*extension-last-discovered*)
         (old-global amoebum:*extensions-global-directory-override*)
         (old-project amoebum:*extensions-project-directory-override*)
         (disabled-keys (%hash-table-keys amoebum:*disabled-extensions*))
         (tmp-dir (%make-temp-directory "amoebum-ext-error"))
         (global-dir (merge-pathnames #P"global/" tmp-dir))
         (project-dir (merge-pathnames #P"project/" tmp-dir))
         (ext-root (merge-pathnames #P"bad-ext/" project-dir)))
    (unwind-protect
         (progn
           (setf amoebum:*extensions-global-directory-override* global-dir
                 amoebum:*extensions-project-directory-override* project-dir)
           (clrhash amoebum:*disabled-extensions*)
           (%write-text-file (merge-pathnames #P"extension.lisp" ext-root)
                             "(:name \"bad-ext\" :version \"0.1\" :entry-point \"main.lisp\")")
           (%write-text-file (merge-pathnames #P"main.lisp" ext-root)
                             "(error \"deliberate load failure\")")
           (let ((report (amoebum:load-user-extensions
                          :project-root tmp-dir :start-hot-reload nil)))
             (is (= 1 (length report)))
             (is (eq :error (amoebum::extension-load-record-status (first report))))))
      (setf amoebum:*extension-load-report* old-report
            amoebum:*loaded-extensions* old-loaded
            amoebum::*extension-last-discovered* old-discovered
            amoebum:*extensions-global-directory-override* old-global
            amoebum:*extensions-project-directory-override* old-project)
      (clrhash amoebum:*disabled-extensions*)
      (dolist (key disabled-keys)
        (setf (gethash key amoebum:*disabled-extensions*) t))
      (%delete-directory-tree-safe tmp-dir))))

(test extension-report-summary-counts
  "extension-report-summary should tally loaded/errors/disabled."
  (let ((report (list
                 (amoebum::make-extension-load-record :path "/a" :scope :project :status :loaded)
                 (amoebum::make-extension-load-record :path "/b" :scope :global :status :loaded)
                 (amoebum::make-extension-load-record :path "/c" :scope :project :status :error :message "fail")
                 (amoebum::make-extension-load-record :path "/d" :scope :project :status :disabled :message "off"))))
    (let ((summary (amoebum:extension-report-summary report)))
      (is (= 4 (getf summary :total)))
      (is (= 2 (getf summary :loaded)))
      (is (= 1 (getf summary :errors)))
      (is (= 1 (getf summary :disabled))))))
