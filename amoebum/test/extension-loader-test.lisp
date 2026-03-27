(in-package :amoebum/test)

(def-suite extension-loader-suite
  :description "I232 extension loader auto-discovery and hot-reload coverage."
  :in amoebum-suite)

(in-suite extension-loader-suite)

(defvar *i232-extension-log* '())

(defun %i232-write-extension (root name version marker)
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
             "(setf amoebum/test::*i232-extension-log*
       (append amoebum/test::*i232-extension-log* (list ~S)))~%"
             marker))
    (values extension-root manifest-path entry-path)))

(test extension-loader-discovers-registry-and-hot-reload
  (let* ((tmp-root (%make-temp-directory "amoebum-i232"))
         (global-root (merge-pathnames #P"global/" tmp-root))
         (project-root (merge-pathnames #P"project/" tmp-root))
         (project-local-root (merge-pathnames #P".amoebum/extensions/" project-root))
         (old-global-override amoebum.extensions:*extensions-global-directory-override*)
         (old-project-override amoebum.extensions:*extensions-project-directory-override*)
         (old-hot-reload-enabled amoebum.extensions:*extension-hot-reload-enabled-p*)
         (old-hot-reload-interval amoebum.extensions:*extension-hot-reload-interval-seconds*))
    (unwind-protect
        (progn
          (setf *i232-extension-log* '()
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

          (multiple-value-bind (_groot gmanifest gentry)
              (%i232-write-extension global-root "global-alpha" "1.0.0" "global-alpha-v1")
            (declare (ignore _groot))
            (multiple-value-bind (_proot pmanifest pentry)
                (%i232-write-extension project-local-root "project-beta" "1.1.0" "project-beta-v1")
              (declare (ignore _proot))
              (let ((report (amoebum.extensions:load-user-extensions :project-root project-root
                                                                     :start-hot-reload nil)))
                (is (= (getf (amoebum.extensions:extension-report-summary report) :loaded 0) 2))
                (is (= (length (amoebum.extensions:list-extension-registry)) 2))
                (is (equal *i232-extension-log* '("global-alpha-v1" "project-beta-v1"))))

              (let ((names (mapcar #'amoebum.extensions:extension-registry-entry-name
                                   (amoebum.extensions:list-extension-registry))))
                (is-true (member "global-alpha" names :test #'string-equal))
                (is-true (member "project-beta" names :test #'string-equal)))

              (multiple-value-bind (handled result)
                  (amoebum:dispatch-slash-command "/extensions list")
                (is-true handled)
                (is-true (search "global-alpha" (amoebum.commands:slash-command-result-output result)
                                 :test #'char-equal))
                (is-true (search "project-beta" (amoebum.commands:slash-command-result-output result)
                                 :test #'char-equal)))

              (multiple-value-bind (handled result)
                  (amoebum:dispatch-slash-command "/extensions disable global-alpha")
                (is-true handled)
                (is-true (search "Disabled 1 extension" (amoebum.commands:slash-command-result-output result)
                                 :test #'char-equal)))

              (multiple-value-bind (handled result)
                  (amoebum:dispatch-slash-command "/extensions enable global-alpha")
                (is-true handled)
                (is-true (search "Enabled 1 extension" (amoebum.commands:slash-command-result-output result)
                                 :test #'char-equal)))

              (%write-text-file
               pmanifest
               "(:name \"project-beta\" :version \"1.2.0\" :dependencies () :entry-point \"main.lisp\")
")
              (%write-text-file
               pentry
               "(setf amoebum/test::*i232-extension-log*
       (append amoebum/test::*i232-extension-log* (list \"project-beta-v2\")))
")
              (sleep 1)
              (is-true (amoebum.extensions:check-extension-hot-reload :project-root project-root
                                                                      :reload-on-change t
                                                                      :start-hot-reload nil))

              (let ((beta (find "project-beta"
                                (amoebum.extensions:list-extension-registry)
                                :key #'amoebum.extensions:extension-registry-entry-name
                                :test #'string-equal)))
                (is-true beta)
                (is (string= "1.2.0" (amoebum.extensions:extension-registry-entry-version beta)))
                (is-true (member "project-beta-v2" *i232-extension-log* :test #'string=)))

              (is-true (probe-file gmanifest))
              (is-true (probe-file gentry)))))
      (amoebum.extensions:stop-extension-hot-reload)
      (setf amoebum.extensions:*extensions-global-directory-override* old-global-override
            amoebum.extensions:*extensions-project-directory-override* old-project-override
            amoebum.extensions:*extension-hot-reload-enabled-p* old-hot-reload-enabled
            amoebum.extensions:*extension-hot-reload-interval-seconds* old-hot-reload-interval
            amoebum.extensions:*extension-load-report* '()
            amoebum.extensions:*loaded-extensions* '()
            amoebum.extensions:*extension-last-discovered* '()
            *i232-extension-log* '())
      (clrhash amoebum.extensions:*disabled-extensions*)
      (clrhash amoebum.extensions:*extension-registry*)
      (clrhash amoebum.extensions:*extension-watch-snapshot*)
      (%delete-directory-tree-safe tmp-root))))

(test extension-loader-smoke-sentinel
  (is-true t)
  (format t "EXTENSION_LOADER_SMOKE_OK~%"))
