(in-package :amoebum/test)

;;; ============================================================
;;; I241: Extension Security — Disable/Enable Control
;;; ============================================================

(def-suite extension-security-suite :in amoebum-suite)
(in-suite extension-security-suite)

(test extension-disabled-p-checks-hash
  "extension-disabled-p should check the disabled hash table."
  (let ((old-disabled (%hash-table-keys amoebum:*disabled-extensions*)))
    (unwind-protect
         (progn
           (clrhash amoebum:*disabled-extensions*)
           (is (not (amoebum:extension-disabled-p "/some/ext.lisp")))
           (setf (gethash (string-downcase "/some/ext.lisp")
                          amoebum:*disabled-extensions*)
                 t)
           (is (amoebum:extension-disabled-p "/some/ext.lisp")))
      (clrhash amoebum:*disabled-extensions*)
      (dolist (key old-disabled)
        (setf (gethash key amoebum:*disabled-extensions*) t)))))

(test disable-user-extension-by-name
  "disable-user-extension should mark extensions as disabled."
  (let* ((old-report amoebum:*extension-load-report*)
         (old-loaded amoebum:*loaded-extensions*)
         (old-discovered amoebum::*extension-last-discovered*)
         (old-global amoebum:*extensions-global-directory-override*)
         (old-project amoebum:*extensions-project-directory-override*)
         (disabled-keys (%hash-table-keys amoebum:*disabled-extensions*))
         (tmp-dir (%make-temp-directory "amoebum-ext-security"))
         (global-dir (merge-pathnames #P"global/" tmp-dir))
         (project-dir (merge-pathnames #P"project/" tmp-dir))
         (ext-root (merge-pathnames #P"my-ext/" project-dir)))
    (unwind-protect
         (progn
           (setf amoebum:*extensions-global-directory-override* global-dir
                 amoebum:*extensions-project-directory-override* project-dir)
           (clrhash amoebum:*disabled-extensions*)
           (%write-text-file (merge-pathnames #P"extension.lisp" ext-root)
                             "(:name \"my-ext\" :version \"0.1\" :entry-point \"main.lisp\")")
           (%write-text-file (merge-pathnames #P"main.lisp" ext-root)
                             "(in-package :amoebum/test)")
           ;; Load, then disable
           (amoebum:load-user-extensions :project-root tmp-dir :start-hot-reload nil)
           (is (= 1 (length amoebum:*extension-load-report*)))
           (amoebum:disable-user-extension "my-ext")
           ;; Reload should mark it disabled
           (amoebum:reload-user-extensions :project-root tmp-dir :start-hot-reload nil)
           (is (eq :disabled
                   (amoebum::extension-load-record-status
                    (first amoebum:*extension-load-report*)))))
      (setf amoebum:*extension-load-report* old-report
            amoebum:*loaded-extensions* old-loaded
            amoebum::*extension-last-discovered* old-discovered
            amoebum:*extensions-global-directory-override* old-global
            amoebum:*extensions-project-directory-override* old-project)
      (clrhash amoebum:*disabled-extensions*)
      (dolist (key disabled-keys)
        (setf (gethash key amoebum:*disabled-extensions*) t))
      (%delete-directory-tree-safe tmp-dir))))

(test enable-user-extension-re-enables
  "enable-user-extension should allow previously disabled extensions to load."
  (let ((old-disabled (%hash-table-keys amoebum:*disabled-extensions*)))
    (unwind-protect
         (progn
           (clrhash amoebum:*disabled-extensions*)
           ;; Directly mark as disabled via internal API (no path matching needed)
           (amoebum::%disable-extension-path "/fake/ext.lisp")
           (is (amoebum:extension-disabled-p "/fake/ext.lisp"))
           ;; Enable via internal API
           (amoebum::%enable-extension-path "/fake/ext.lisp")
           (is (not (amoebum:extension-disabled-p "/fake/ext.lisp"))))
      (clrhash amoebum:*disabled-extensions*)
      (dolist (key old-disabled)
        (setf (gethash key amoebum:*disabled-extensions*) t)))))

(test disable-all-extensions
  "Disabling multiple extensions via %disable-extension-path should mark them disabled."
  (let ((disabled-keys (%hash-table-keys amoebum:*disabled-extensions*)))
    (unwind-protect
         (progn
           (clrhash amoebum:*disabled-extensions*)
           ;; Directly disable two paths
           (amoebum::%disable-extension-path "/path/to/ext-a.lisp")
           (amoebum::%disable-extension-path "/path/to/ext-b.lisp")
           (is (amoebum:extension-disabled-p "/path/to/ext-a.lisp"))
           (is (amoebum:extension-disabled-p "/path/to/ext-b.lisp"))
           (is (= 2 (hash-table-count amoebum:*disabled-extensions*))))
      (clrhash amoebum:*disabled-extensions*)
      (dolist (key disabled-keys)
        (setf (gethash key amoebum:*disabled-extensions*) t)))))
