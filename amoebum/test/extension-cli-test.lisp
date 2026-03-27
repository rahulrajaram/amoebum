(in-package :amoebum/test)

;;; ============================================================
;;; I242: Extension Introspection and CLI Commands
;;; ============================================================

(def-suite extension-cli-suite :in amoebum-suite)
(in-suite extension-cli-suite)

(defvar *i242-extension-log* '())

(defun %i242-hash-table-snapshot (table)
  (let ((snapshot '()))
    (maphash (lambda (key value)
               (push (cons key value) snapshot))
             table)
    snapshot))

(defun %i242-restore-hash-table (table snapshot)
  (clrhash table)
  (dolist (entry snapshot)
    (setf (gethash (car entry) table) (cdr entry))))

(defun %i242-write-extension (root name version marker)
  (let* ((extension-root (merge-pathnames
                          (make-pathname :directory `(:relative ,name))
                          root))
         (manifest-path (merge-pathnames #P"extension.lisp" extension-root))
         (entry-path (merge-pathnames #P"main.lisp" extension-root)))
    (%write-text-file
     manifest-path
     (format nil
             "(:name ~S :version ~S :dependencies () :entry-point \"main.lisp\")~%"
             name
             version))
    (%write-text-file
     entry-path
     (format nil
             "(in-package :amoebum/test)
(setf *i242-extension-log* (append *i242-extension-log* (list ~S)))
(defparameter *i242-extension-introspection*
  '((deftool i242-demo-tool)
    (defhook :on-error i242-demo-hook)))
"
             marker))
    (values extension-root manifest-path entry-path)))

(test extension-introspection-and-cli-flow
  "list-extensions/describe-extension and /ext-* commands expose extension metadata and lifecycle controls."
  (let* ((tmp-root (%make-temp-directory "amoebum-i242"))
         (global-root (merge-pathnames #P"global/" tmp-root))
         (project-root (merge-pathnames #P"project/" tmp-root))
         (project-local-root (merge-pathnames #P".amoebum/extensions/" project-root))
         (old-report amoebum.extensions:*extension-load-report*)
         (old-loaded amoebum.extensions:*loaded-extensions*)
         (old-discovered amoebum.extensions:*extension-last-discovered*)
         (old-global-override amoebum.extensions:*extensions-global-directory-override*)
         (old-project-override amoebum.extensions:*extensions-project-directory-override*)
         (old-hot-reload-enabled amoebum.extensions:*extension-hot-reload-enabled-p*)
         (old-hot-reload-interval amoebum.extensions:*extension-hot-reload-interval-seconds*)
         (old-disabled (%i242-hash-table-snapshot amoebum.extensions:*disabled-extensions*))
         (old-registry (%i242-hash-table-snapshot amoebum.extensions:*extension-registry*))
         (old-watch-snapshot (%i242-hash-table-snapshot amoebum.extensions:*extension-watch-snapshot*)))
    (unwind-protect
        (progn
          (setf *i242-extension-log* '()
                amoebum.extensions:*extensions-global-directory-override* global-root
                amoebum.extensions:*extensions-project-directory-override* project-local-root
                amoebum.extensions:*extension-hot-reload-enabled-p* nil
                amoebum.extensions:*extension-hot-reload-interval-seconds* 0.05d0
                amoebum.extensions:*extension-load-report* '()
                amoebum.extensions:*loaded-extensions* '()
                amoebum.extensions:*extension-last-discovered* '())
          (clrhash amoebum.extensions:*disabled-extensions*)
          (clrhash amoebum.extensions:*extension-registry*)
          (clrhash amoebum.extensions:*extension-watch-snapshot*)
          (ensure-directories-exist (merge-pathnames #P".keep" global-root))
          (ensure-directories-exist (merge-pathnames #P".keep" project-local-root))

          (%i242-write-extension project-local-root "i242-alpha" "1.2.3" "i242-alpha-v1")

          (let ((report (amoebum.extensions:load-user-extensions :project-root project-root
                                                                 :start-hot-reload nil)))
            (is (= 1 (length report)))
            (is (= 1 (getf (amoebum.extensions:extension-report-summary report) :loaded 0))))

          (let* ((extensions (amoebum.extensions:list-extensions))
                 (entry (first extensions)))
            (is (= 1 (length extensions)))
            (is (string= "i242-alpha" (getf entry :name)))
            (is (string= "1.2.3" (getf entry :version)))
            (is (eq :loaded (getf entry :status)))
            (is (>= (getf entry :tool-count 0) 1))
            (is (>= (getf entry :hook-count 0) 1)))

          (let ((described (amoebum.extensions:describe-extension 'i242-alpha)))
            (is (listp described))
            (is (eq :loaded (getf described :status)))
            (is (string= "i242-alpha" (getf described :name))))

          (multiple-value-bind (handled result)
              (amoebum:dispatch-slash-command "/extensions list")
            (is-true handled)
            (let ((output (or (amoebum.commands:slash-command-result-output result) "")))
              (is-true (search "i242-alpha" output :test #'char-equal))
              (is-true (search "tools=" output :test #'char-equal))
              (is-true (search "hooks=" output :test #'char-equal))))

          (multiple-value-bind (handled result)
              (amoebum:dispatch-slash-command "/ext-unload i242-alpha")
            (is-true handled)
            (is-true (search "ext-unload" (or (amoebum.commands:slash-command-result-output result) "")
                             :test #'char-equal)))
          (let ((described (amoebum.extensions:describe-extension "i242-alpha")))
            (is (eq :disabled (getf described :status))))

          (multiple-value-bind (handled result)
              (amoebum:dispatch-slash-command "/ext-load i242-alpha")
            (is-true handled)
            (is-true (search "ext-load" (or (amoebum.commands:slash-command-result-output result) "")
                             :test #'char-equal)))
          (let ((described (amoebum.extensions:describe-extension "i242-alpha")))
            (is (eq :loaded (getf described :status))))

          (multiple-value-bind (handled result)
              (amoebum:dispatch-slash-command "/ext-reload i242-alpha")
            (is-true handled)
            (is-true (search "ext-reload" (or (amoebum.commands:slash-command-result-output result) "")
                             :test #'char-equal)))
          (let ((described (amoebum.extensions:describe-extension "i242-alpha")))
            (is (eq :loaded (getf described :status))))

          (is-true (amoebum:find-slash-command "ext-load"))
          (is-true (amoebum:find-slash-command "ext-unload"))
          (is-true (amoebum:find-slash-command "ext-reload")))
      (amoebum.extensions:stop-extension-hot-reload)
      (setf amoebum.extensions:*extension-load-report* old-report
            amoebum.extensions:*loaded-extensions* old-loaded
            amoebum.extensions:*extension-last-discovered* old-discovered
            amoebum.extensions:*extensions-global-directory-override* old-global-override
            amoebum.extensions:*extensions-project-directory-override* old-project-override
            amoebum.extensions:*extension-hot-reload-enabled-p* old-hot-reload-enabled
            amoebum.extensions:*extension-hot-reload-interval-seconds* old-hot-reload-interval
            *i242-extension-log* '())
      (%i242-restore-hash-table amoebum.extensions:*disabled-extensions* old-disabled)
      (%i242-restore-hash-table amoebum.extensions:*extension-registry* old-registry)
      (%i242-restore-hash-table amoebum.extensions:*extension-watch-snapshot* old-watch-snapshot)
      (%delete-directory-tree-safe tmp-root))))

(test extension-cli-smoke-sentinel
  (is-true t)
  (format t "EXTENSION_CLI_SMOKE_OK~%"))
