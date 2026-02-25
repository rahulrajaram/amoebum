(let* ((smoke-file (or *load-truename* *compile-file-truename*))
       (amoebum-dir (and smoke-file (make-pathname :name nil :type nil :defaults smoke-file)))
       (repo-root (and amoebum-dir (truename (merge-pathnames #P"../" amoebum-dir)))))
  (unless repo-root
    (error "Unable to resolve repository root from ~S" smoke-file))

  (load (merge-pathnames #P"ptui/.tools/quicklisp/setup.lisp" repo-root))
  (require :asdf)
  (let ((uiop-lisp-build (find-package "UIOP/LISP-BUILD")))
    (when uiop-lisp-build
      (let ((warnings-behaviour (find-symbol "*COMPILE-FILE-WARNINGS-BEHAVIOUR*"
                                             uiop-lisp-build))
            (failure-behaviour (find-symbol "*COMPILE-FILE-FAILURE-BEHAVIOUR*"
                                            uiop-lisp-build)))
        (when warnings-behaviour
          (setf (symbol-value warnings-behaviour) :ignore))
        (when failure-behaviour
          (setf (symbol-value failure-behaviour) :warn)))))

  (let* ((asdf-pkg (or (find-package "ASDF")
                       (error "Missing package ASDF")))
         (load-asd-sym (or (find-symbol "LOAD-ASD" asdf-pkg)
                           (error "Missing symbol LOAD-ASD in ASDF package")))
         (load-system-sym (or (find-symbol "LOAD-SYSTEM" asdf-pkg)
                              (error "Missing symbol LOAD-SYSTEM in ASDF package")))
         (load-asd-fn (symbol-function load-asd-sym))
         (load-system-fn (symbol-function load-system-sym)))
    (funcall load-asd-fn (merge-pathnames #P"pseudopod/pseudopod.asd" repo-root))
    (funcall load-asd-fn (merge-pathnames #P"sw4rm-sdk/sw4rm-sdk.asd" repo-root))
    (funcall load-asd-fn (merge-pathnames #P"ptui/ptui.asd" repo-root))
    (funcall load-asd-fn (merge-pathnames #P"amoebum/amoebum.asd" repo-root))
    (funcall load-system-fn :amoebum/test))

  (let* ((fiveam-pkg (or (find-package "IT.BESE.FIVEAM")
                         (find-package "FIVEAM")
                         (error "Missing FiveAM package")))
         (run-fn (symbol-function (or (find-symbol "RUN" fiveam-pkg)
                                      (error "Missing FiveAM RUN symbol"))))
         (results-status-fn (symbol-function (or (find-symbol "RESULTS-STATUS" fiveam-pkg)
                                                 (error "Missing FiveAM RESULTS-STATUS symbol"))))
         (explain-fn (symbol-function (or (find-symbol "EXPLAIN!" fiveam-pkg)
                                          (error "Missing FiveAM EXPLAIN! symbol"))))
         (suite-symbol (or (find-symbol "NOTIFICATION-DISPATCH-SUITE" "AMOEBUM/TEST")
                           (error "Missing AMOEBUM/TEST::NOTIFICATION-DISPATCH-SUITE")))
         (results (funcall run-fn suite-symbol)))
    (unless (funcall results-status-fn results)
      (funcall explain-fn results)
      (sb-ext:exit :code 1))
    (funcall explain-fn results))

  (format t "NOTIFICATION_DISPATCH_SMOKE_OK~%"))
