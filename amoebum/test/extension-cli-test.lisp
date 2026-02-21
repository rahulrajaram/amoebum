(in-package :amoebum/test)

;;; ============================================================
;;; I242: Extension Introspection and CLI Functions
;;; ============================================================

(def-suite extension-cli-suite :in amoebum-suite)
(in-suite extension-cli-suite)

(test list-loaded-extensions-returns-list
  "list-loaded-extensions should return a list."
  (is (listp (amoebum:list-loaded-extensions))))

(test list-extension-report-returns-list
  "list-extension-report should return a list."
  (is (listp (amoebum:list-extension-report))))

(test list-extension-registry-returns-list
  "list-extension-registry should return a sorted list of entries."
  (let ((entries (amoebum:list-extension-registry)))
    (is (listp entries))
    (dolist (entry entries)
      (is (amoebum::extension-registry-entry-p entry)))))

(test known-user-extension-paths-returns-strings
  "known-user-extension-paths should return a list of strings."
  (let ((paths (amoebum:known-user-extension-paths)))
    (is (listp paths))
    (dolist (p paths)
      (is (stringp p)))))

(test known-user-extension-names-returns-strings
  "known-user-extension-names should return a list of strings."
  (let ((names (amoebum:known-user-extension-names)))
    (is (listp names))
    (dolist (n names)
      (is (stringp n)))))

(test extension-report-summary-structure
  "extension-report-summary should return plist with :total :loaded :errors :disabled."
  (let ((summary (amoebum:extension-report-summary)))
    (is (listp summary))
    (is (integerp (getf summary :total)))
    (is (integerp (getf summary :loaded)))
    (is (integerp (getf summary :errors)))
    (is (integerp (getf summary :disabled)))))

(test discover-user-extension-files-returns-values
  "discover-user-extension-files should return two values (global, project)."
  (let* ((tmp-dir (%make-temp-directory "amoebum-ext-cli"))
         (global-dir (merge-pathnames #P"global/" tmp-dir))
         (project-dir (merge-pathnames #P"project/" tmp-dir)))
    (unwind-protect
         (progn
           (ensure-directories-exist global-dir)
           (ensure-directories-exist project-dir)
           (multiple-value-bind (global-files project-files)
               (amoebum:discover-user-extension-files
                :global-directory global-dir
                :project-directory project-dir)
             (is (listp global-files))
             (is (listp project-files))))
      (%delete-directory-tree-safe tmp-dir))))

(test stop-extension-hot-reload-idempotent
  "stop-extension-hot-reload should be safe to call when not running."
  (finishes (amoebum:stop-extension-hot-reload)))
