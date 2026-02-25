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
             "(in-package :amoebum/test)~%(setf *i232-extension-log* (append *i232-extension-log* (list ~S)))~%"
             marker))
    (values extension-root manifest-path entry-path)))

(test extension-loader-discovers-registry-and-hot-reload
  (let* ((tmp-root (%make-temp-directory "amoebum-i232"))
         (global-root (merge-pathnames #P"global/" tmp-root))
         (project-root (merge-pathnames #P"project/" tmp-root))
         (project-local-root (merge-pathnames #P".amoebum/extensions/" project-root))
         (old-global-override amoebum::*extensions-global-directory-override*)
         (old-project-override amoebum::*extensions-project-directory-override*)
         (old-hot-reload-enabled amoebum::*extension-hot-reload-enabled-p*)
         (old-hot-reload-interval amoebum::*extension-hot-reload-interval-seconds*))
    (unwind-protect
        (progn
          (setf *i232-extension-log* '()
                amoebum::*extensions-global-directory-override* global-root
                amoebum::*extensions-project-directory-override* project-local-root
                amoebum::*extension-hot-reload-enabled-p* nil
                amoebum::*extension-hot-reload-interval-seconds* 0.05d0
                amoebum::*extension-load-report* '()
                amoebum::*loaded-extensions* '()
                amoebum::*extension-last-discovered* '())
          (clrhash amoebum::*disabled-extensions*)
          (clrhash amoebum::*extension-registry*)
          (clrhash amoebum::*extension-watch-snapshot*)
          (ensure-directories-exist (merge-pathnames #P".keep" global-root))
          (ensure-directories-exist (merge-pathnames #P".keep" project-local-root))

          (multiple-value-bind (_groot gmanifest gentry)
              (%i232-write-extension global-root "global-alpha" "1.0.0" "global-alpha-v1")
            (declare (ignore _groot))
            (multiple-value-bind (_proot pmanifest pentry)
                (%i232-write-extension project-local-root "project-beta" "1.1.0" "project-beta-v1")
              (declare (ignore _proot))
              (let ((report (amoebum:load-user-extensions :project-root project-root
                                                          :start-hot-reload nil)))
                (is (= (getf (amoebum:extension-report-summary report) :loaded 0) 2))
                (is (= (length (amoebum:list-extension-registry)) 2))
                (is (equal *i232-extension-log* '("global-alpha-v1" "project-beta-v1"))))

              (let ((names (mapcar #'amoebum:extension-registry-entry-name
                                   (amoebum:list-extension-registry))))
                (is-true (member "global-alpha" names :test #'string-equal))
                (is-true (member "project-beta" names :test #'string-equal)))

              (multiple-value-bind (handled result)
                  (amoebum:dispatch-slash-command "/extensions list")
                (is-true handled)
                (is-true (search "global-alpha" (amoebum:slash-command-result-output result)
                                 :test #'char-equal))
                (is-true (search "project-beta" (amoebum:slash-command-result-output result)
                                 :test #'char-equal)))

              (multiple-value-bind (handled result)
                  (amoebum:dispatch-slash-command "/extensions disable global-alpha")
                (is-true handled)
                (is-true (search "Disabled 1 extension" (amoebum:slash-command-result-output result)
                                 :test #'char-equal)))

              (multiple-value-bind (handled result)
                  (amoebum:dispatch-slash-command "/extensions enable global-alpha")
                (is-true handled)
                (is-true (search "Enabled 1 extension" (amoebum:slash-command-result-output result)
                                 :test #'char-equal)))

              (%write-text-file
               pmanifest
               "(:name \"project-beta\" :version \"1.2.0\" :dependencies () :entry-point \"main.lisp\")
")
              (%write-text-file
               pentry
               "(in-package :amoebum/test)
(setf *i232-extension-log* (append *i232-extension-log* (list \"project-beta-v2\")))
")
              (sleep 1)
              (is-true (amoebum:check-extension-hot-reload :project-root project-root
                                                           :reload-on-change t
                                                           :start-hot-reload nil))

              (let ((beta (find "project-beta"
                                (amoebum:list-extension-registry)
                                :key #'amoebum:extension-registry-entry-name
                                :test #'string-equal)))
                (is-true beta)
                (is (string= "1.2.0" (amoebum:extension-registry-entry-version beta)))
                (is-true (member "project-beta-v2" *i232-extension-log* :test #'string=)))

              (is-true (probe-file gmanifest))
              (is-true (probe-file gentry)))))
      (amoebum:stop-extension-hot-reload)
      (setf amoebum::*extensions-global-directory-override* old-global-override
            amoebum::*extensions-project-directory-override* old-project-override
            amoebum::*extension-hot-reload-enabled-p* old-hot-reload-enabled
            amoebum::*extension-hot-reload-interval-seconds* old-hot-reload-interval
            amoebum::*extension-load-report* '()
            amoebum::*loaded-extensions* '()
            amoebum::*extension-last-discovered* '()
            *i232-extension-log* '())
      (clrhash amoebum::*disabled-extensions*)
      (clrhash amoebum::*extension-registry*)
      (clrhash amoebum::*extension-watch-snapshot*)
      (%delete-directory-tree-safe tmp-root))))

(test extension-loader-smoke-sentinel
  (is-true t)
  (format t "EXTENSION_LOADER_SMOKE_OK~%"))
