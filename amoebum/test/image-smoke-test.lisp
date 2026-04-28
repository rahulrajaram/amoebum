(let* ((smoke-file (or *load-truename* *compile-file-truename*))
       (test-dir (and smoke-file (make-pathname :name nil :type nil :defaults smoke-file)))
       (repo-root (and test-dir (truename (merge-pathnames #P"../../" test-dir)))))
  (unless repo-root
    (error "Unable to resolve repository root from ~S" smoke-file))

  #+sbcl
  (let ((home-root (merge-pathnames #P".tmp/home/" repo-root))
        (cache-root (merge-pathnames #P".tmp/xdg-cache/" repo-root))
        (tmp-root (merge-pathnames #P".tmp/" repo-root)))
    (ensure-directories-exist home-root)
    (ensure-directories-exist cache-root)
    (ensure-directories-exist tmp-root)
    (ignore-errors
      (require :sb-posix)
      (let ((setenv-sym (find-symbol "SETENV" "SB-POSIX")))
        (when setenv-sym
          (let ((setenv (symbol-function setenv-sym)))
            (funcall setenv "HOME" (namestring home-root) 1)
            (funcall setenv "XDG_CACHE_HOME" (namestring cache-root) 1)
            (funcall setenv "TMPDIR" (namestring tmp-root) 1))))))

  (let* ((local-quicklisp (merge-pathnames #P"ptui/.tools/quicklisp/setup.lisp" repo-root))
         (fallback-root #P"/home/rahul/Documents/amoebum/")
         (fallback-quicklisp (merge-pathnames #P"ptui/.tools/quicklisp/setup.lisp"
                                              fallback-root))
         (quicklisp-setup
           (cond
             ((probe-file fallback-quicklisp) fallback-quicklisp)
             ((probe-file local-quicklisp) local-quicklisp)
             (t (error "Unable to locate quicklisp setup at ~A or ~A."
                       local-quicklisp
                       fallback-quicklisp)))))
    (load quicklisp-setup))
  (require :asdf)

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
    (funcall load-system-fn "amoebum/test"))

  (let* ((test-pkg (or (find-package "AMOEBUM/TEST")
                       (error "Missing package AMOEBUM/TEST after load.")))
         (fiveam-pkg (or (find-package "FIVEAM")
                         (error "Missing package FIVEAM after load.")))
         (suite-sym (or (find-symbol "IMAGE-SUITE" test-pkg)
                        (error "Missing IMAGE-SUITE in AMOEBUM/TEST.")))
         (run-sym (or (find-symbol "RUN" fiveam-pkg)
                      (find-symbol "RUN!" fiveam-pkg)
                      (error "Missing RUN/RUN! in FIVEAM.")))
         (status-sym (or (find-symbol "RESULTS-STATUS" fiveam-pkg)
                         (error "Missing RESULTS-STATUS in FIVEAM.")))
         (run-fn (symbol-function run-sym))
         (status-fn (symbol-function status-sym))
         (result (funcall run-fn suite-sym))
         (passedp (if (typep result 'boolean)
                      result
                      (funcall status-fn result))))
    (unless passedp
      (sb-ext:exit :code 1))
    (sb-ext:exit :code 0)))
