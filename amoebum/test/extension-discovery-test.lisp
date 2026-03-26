(in-package :amoebum/test)

(def-suite extension-discovery-suite
  :description "I240 extension manifest format and auto-discovery coverage."
  :in amoebum-suite)

(in-suite extension-discovery-suite)

(defvar *i240-extension-log* '())

(defun %i240-write-extension (root name version marker &key dependencies capabilities wrapped-manifest-p)
  (let* ((extension-root (merge-pathnames
                          (make-pathname :directory `(:relative ,name))
                          root))
         (manifest-path (merge-pathnames #P"extension.lisp" extension-root))
         (entry-path (merge-pathnames #P"main.lisp" extension-root))
         (manifest-form
           (if wrapped-manifest-p
               `(extension
                 (:name ,name
                  :version ,version
                  :dependencies ,(or dependencies '())
                  :capabilities ,(or capabilities '())
                  :entry-point "main.lisp"))
               `(:name ,name
                 :version ,version
                 :dependencies ,(or dependencies '())
                 :provides ,(or capabilities '())
                 :entry-point "main.lisp"))))
    (%write-text-file
     manifest-path
     (with-output-to-string (stream)
       (prin1 manifest-form stream)
       (terpri stream)))
    (%write-text-file
     entry-path
     (format nil
             "(in-package :amoebum/test)~%(setf *i240-extension-log* (append *i240-extension-log* (list ~S)))~%"
             marker))
    (values extension-root manifest-path entry-path)))

(test extension-manifest-parses-capabilities-from-sexp
  (let ((manifest
          (amoebum:parse-extension-manifest-sexp
           '(extension
             (:name "sample-discovery"
              :version "1.2.0"
              :dependencies (("dep-core" ">=1.0"))
              :capabilities (:tools :hooks)
              :entry-point "main.lisp")))))
    (is (string= "sample-discovery" (amoebum:extension-manifest-name manifest)))
    (is (string= "1.2.0" (amoebum:extension-manifest-version manifest)))
    (is (equal '(("dep-core" . ">=1.0"))
               (amoebum:extension-manifest-dependencies manifest)))
    (is (equal '(:tools :hooks)
               (amoebum:extension-manifest-capabilities manifest)))
    (is (equal '(:tools :hooks)
               (amoebum:extension-manifest-provides manifest)))))

(test extension-auto-discovery-finds-configured-directories
  (let* ((tmp-root (%make-temp-directory "amoebum-i240-discovery"))
         (global-root (merge-pathnames #P"global/" tmp-root))
         (project-root (merge-pathnames #P"project/" tmp-root))
         (project-local-root (merge-pathnames #P".amoebum/extensions/" project-root))
         (old-global-override amoebum.extensions:*extensions-global-directory-override*)
         (old-project-override amoebum.extensions:*extensions-project-directory-override*)
         (old-hot-reload-enabled amoebum.extensions:*extension-hot-reload-enabled-p*)
         (old-hot-reload-interval amoebum.extensions:*extension-hot-reload-interval-seconds*))
    (unwind-protect
        (progn
          (setf *i240-extension-log* '()
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
          (%i240-write-extension global-root "global-discovery" "1.0.0" "global-v1"
                                 :wrapped-manifest-p t
                                 :dependencies '(("shared-core" ">=1.0"))
                                 :capabilities '(:tools))
          (%i240-write-extension project-local-root "project-discovery" "1.1.0" "project-v1"
                                 :wrapped-manifest-p nil
                                 :capabilities '(:hooks))

          (multiple-value-bind (global-files project-files)
              (amoebum.extensions:discover-user-extension-files :project-root project-root)
            (is (= 1 (length global-files)))
            (is (= 1 (length project-files)))
            (is (string-equal "extension.lisp" (file-namestring (first global-files))))
            (is (string-equal "extension.lisp" (file-namestring (first project-files)))))

          (let ((report (amoebum.extensions:load-user-extensions :project-root project-root
                                                                 :start-hot-reload nil)))
            (is (= (getf (amoebum.extensions:extension-report-summary report) :loaded 0) 2))
            (is (= (length (amoebum.extensions:list-extension-registry)) 2))
            (is (equal *i240-extension-log* '("global-v1" "project-v1")))))
      (amoebum.extensions:stop-extension-hot-reload)
      (setf amoebum.extensions:*extensions-global-directory-override* old-global-override
            amoebum.extensions:*extensions-project-directory-override* old-project-override
            amoebum.extensions:*extension-hot-reload-enabled-p* old-hot-reload-enabled
            amoebum.extensions:*extension-hot-reload-interval-seconds* old-hot-reload-interval
            amoebum.extensions:*extension-load-report* '()
            amoebum.extensions:*loaded-extensions* '()
            amoebum.extensions:*extension-last-discovered* '()
            *i240-extension-log* '())
      (clrhash amoebum.extensions:*disabled-extensions*)
      (clrhash amoebum.extensions:*extension-registry*)
      (clrhash amoebum.extensions:*extension-watch-snapshot*)
      (%delete-directory-tree-safe tmp-root))))

(test extension-hot-reload-detects-manifest-and-entrypoint-changes
  (let* ((tmp-root (%make-temp-directory "amoebum-i240-reload"))
         (global-root (merge-pathnames #P"global/" tmp-root))
         (project-root (merge-pathnames #P"project/" tmp-root))
         (project-local-root (merge-pathnames #P".amoebum/extensions/" project-root))
         (old-global-override amoebum.extensions:*extensions-global-directory-override*)
         (old-project-override amoebum.extensions:*extensions-project-directory-override*)
         (old-hot-reload-enabled amoebum.extensions:*extension-hot-reload-enabled-p*)
         (old-hot-reload-interval amoebum.extensions:*extension-hot-reload-interval-seconds*))
    (unwind-protect
        (progn
          (setf *i240-extension-log* '()
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

          (ensure-directories-exist (merge-pathnames #P".keep" project-local-root))
          (multiple-value-bind (_root manifest-path entry-path)
              (%i240-write-extension project-local-root "hot-reload-ext" "1.0.0" "hot-reload-v1"
                                     :wrapped-manifest-p t
                                     :capabilities '(:tools))
            (declare (ignore _root))
            (let ((report (amoebum.extensions:load-user-extensions :project-root project-root
                                                                   :start-hot-reload nil)))
              (is (= (getf (amoebum.extensions:extension-report-summary report) :loaded 0) 1)))

            (sleep 1)
            (%write-text-file
             manifest-path
             "(:name \"hot-reload-ext\" :version \"1.2.0\" :dependencies () :provides (:tools :widgets) :entry-point \"main.lisp\")
")
            (%write-text-file
             entry-path
             "(in-package :amoebum/test)
(setf *i240-extension-log* (append *i240-extension-log* (list \"hot-reload-v2\")))
")

            (sleep 1)
            (is-true (amoebum.extensions:check-extension-hot-reload :project-root project-root
                                                                    :reload-on-change t
                                                                    :start-hot-reload nil))
            (let ((entry (find "hot-reload-ext"
                               (amoebum.extensions:list-extension-registry)
                               :key #'amoebum.extensions:extension-registry-entry-name
                               :test #'string-equal)))
              (is-true entry)
              (is (string= "1.2.0"
                           (amoebum.extensions:extension-registry-entry-version entry)))
              (is-true (member "hot-reload-v2" *i240-extension-log* :test #'string=)))))
      (amoebum.extensions:stop-extension-hot-reload)
      (setf amoebum.extensions:*extensions-global-directory-override* old-global-override
            amoebum.extensions:*extensions-project-directory-override* old-project-override
            amoebum.extensions:*extension-hot-reload-enabled-p* old-hot-reload-enabled
            amoebum.extensions:*extension-hot-reload-interval-seconds* old-hot-reload-interval
            amoebum.extensions:*extension-load-report* '()
            amoebum.extensions:*loaded-extensions* '()
            amoebum.extensions:*extension-last-discovered* '()
            *i240-extension-log* '())
      (clrhash amoebum.extensions:*disabled-extensions*)
      (clrhash amoebum.extensions:*extension-registry*)
      (clrhash amoebum.extensions:*extension-watch-snapshot*)
      (%delete-directory-tree-safe tmp-root))))

(test extension-discovery-smoke-sentinel
  (is-true t)
  (format t "EXTENSION_DISCOVERY_SMOKE_OK~%"))
