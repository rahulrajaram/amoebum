(let* ((smoke-file (or *load-truename* *compile-file-truename*))
       (amoebum-dir (and smoke-file (make-pathname :name nil :type nil :defaults smoke-file)))
       (repo-root (and amoebum-dir (truename (merge-pathnames #P"../" amoebum-dir)))))
  (unless repo-root
    (error "Unable to resolve repository root from ~S" smoke-file))

  (load (merge-pathnames #P"ptui/.tools/quicklisp/setup.lisp" repo-root))
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
    (funcall load-asd-fn (merge-pathnames #P"ptui/ptui.asd" repo-root))
    (funcall load-asd-fn (merge-pathnames #P"amoebum/amoebum.asd" repo-root))
    (funcall load-system-fn "amoebum"))

  (let* ((amoebum-pkg (or (find-package "AMOEBUM")
                          (error "Missing package AMOEBUM after load.")))
         (uiop-pkg (or (find-package "UIOP")
                       (find-package "ASDF/UTILITY")
                       (error "Missing UIOP package after requiring ASDF.")))
         (symbol-in
           (lambda (name package)
             (or (find-symbol name package)
                 (error "Missing symbol ~A in package ~A." name (package-name package)))))
         (fn-in
           (lambda (name package)
             (symbol-function (funcall symbol-in name package))))
         (ensure-directory-pathname
           (symbol-function (funcall symbol-in "ENSURE-DIRECTORY-PATHNAME" uiop-pkg)))
         (temporary-directory
           (symbol-function (funcall symbol-in "TEMPORARY-DIRECTORY" uiop-pkg)))
         (reload-config-fn (funcall fn-in "RELOAD-CONFIG" amoebum-pkg))
         (setconfig-fn (funcall fn-in "SETCONFIG" amoebum-pkg))
         (make-event-bus-fn (funcall fn-in "MAKE-EVENT-BUS" amoebum-pkg))
         (subscribe-fn (funcall fn-in "SUBSCRIBE" amoebum-pkg))
         (current-memory-backend-fn (funcall fn-in "CURRENT-MEMORY-BACKEND" amoebum-pkg))
         (reset-memory-backend-fn (funcall fn-in "RESET-MEMORY-BACKEND" amoebum-pkg))
         (file-memory-backend-p-fn (funcall fn-in "FILE-MEMORY-BACKEND-P" amoebum-pkg))
         (haake-cli-memory-backend-p-fn (funcall fn-in "HAAKE-CLI-MEMORY-BACKEND-P" amoebum-pkg))
         (memory-store-fn (funcall fn-in "MEMORY-STORE" amoebum-pkg))
         (memory-query-fn (funcall fn-in "MEMORY-QUERY" amoebum-pkg))
         (memory-list-fn (funcall fn-in "MEMORY-LIST" amoebum-pkg))
         (memory-delete-fn (funcall fn-in "MEMORY-DELETE" amoebum-pkg))
         (memory-entry-key-fn (funcall fn-in "MEMORY-ENTRY-KEY" amoebum-pkg))
         (memory-entry-value-fn (funcall fn-in "MEMORY-ENTRY-VALUE" amoebum-pkg))
         (memory-entry-scope-fn (funcall fn-in "MEMORY-ENTRY-SCOPE" amoebum-pkg))
         (event-payload-fn (funcall fn-in "EVENT-PAYLOAD" amoebum-pkg))
         (memory-backend-selected-payload-backend-fn
          (funcall fn-in "MEMORY-BACKEND-SELECTED-PAYLOAD-BACKEND" amoebum-pkg))
         (memory-backend-selected-payload-reason-fn
          (funcall fn-in "MEMORY-BACKEND-SELECTED-PAYLOAD-REASON" amoebum-pkg))
         (memory-backend-selected-payload-requested-backend-fn
          (funcall fn-in "MEMORY-BACKEND-SELECTED-PAYLOAD-REQUESTED-BACKEND" amoebum-pkg))
         (memory-backend-selected-event-type
          (symbol-value
           (funcall symbol-in "+EVENT-TYPE-MEMORY-BACKEND-SELECTED+" amoebum-pkg)))
         (event-bus-sym (funcall symbol-in "*EVENT-BUS*" amoebum-pkg))
         (session-memory-sym (funcall symbol-in "*SESSION-MEMORY-ENTRIES*" amoebum-pkg))
         (availability-runner-sym
          (funcall symbol-in "*HAAKE-CLI-AVAILABILITY-RUNNER*" amoebum-pkg))
         (status-runner-sym
          (funcall symbol-in "*HAAKE-CLI-STATUS-RUNNER*" amoebum-pkg))
         (capability-runner-sym
          (funcall symbol-in "*HAAKE-CLI-CAPABILITY-RUNNER*" amoebum-pkg))
         (command-runner-sym
          (funcall symbol-in "*HAAKE-CLI-COMMAND-RUNNER*" amoebum-pkg)))
    (labels ((assert-true (condition format-string &rest format-args)
               (unless condition
                 (error (apply #'format nil format-string format-args))))
             (argv-contains-p (argv expected)
               (member expected argv :test #'string=)))
      (let* ((tmp-root
               (funcall ensure-directory-pathname
                        (merge-pathnames
                         (make-pathname :directory `(:relative ,(format nil "amoebum-i53-~A" (get-universal-time))))
                         (funcall temporary-directory))))
             (project-root (funcall ensure-directory-pathname (merge-pathnames #P"project/" tmp-root)))
             (old-event-bus (symbol-value event-bus-sym))
             (old-availability-runner (symbol-value availability-runner-sym))
             (old-status-runner (symbol-value status-runner-sym))
             (old-capability-runner (symbol-value capability-runner-sym))
             (old-command-runner (symbol-value command-runner-sym))
             (backend-events '())
             (captured-commands '()))
        (unwind-protect
             (progn
               (setf (symbol-value event-bus-sym) (funcall make-event-bus-fn :capacity 32))
               (funcall subscribe-fn
                        (symbol-value event-bus-sym)
                        memory-backend-selected-event-type
                        (lambda (event)
                          (setf backend-events
                                (append backend-events
                                        (list (funcall event-payload-fn event)))))
                        :priority 5)

               (funcall reload-config-fn :project-root project-root)
               (setf (symbol-value session-memory-sym) '())
               (setf (symbol-value availability-runner-sym)
                     (lambda (command)
                       (declare (ignore command))
                       nil))
               (setf (symbol-value status-runner-sym)
                     (lambda (command &key directory)
                       (declare (ignore command directory))
                       nil))
               (setf (symbol-value capability-runner-sym)
                     (lambda (command &key directory)
                       (declare (ignore command directory))
                       (list :exit-code 1 :stdout "" :stderr "")))
               (setf (symbol-value command-runner-sym)
                     (lambda (arguments &key directory input)
                       (declare (ignore arguments directory input))
                       (error "Haake command runner should not be called when unavailable.")))
               (funcall reset-memory-backend-fn nil)
               (let ((fallback-backend (funcall current-memory-backend-fn)))
                 (assert-true (funcall file-memory-backend-p-fn fallback-backend)
                              "Expected fallback to file backend when Haake is unavailable."))
               (let ((fallback-event (car (last backend-events))))
                 (assert-true fallback-event
                              "Expected memory:backend-selected event for fallback selection.")
                 (assert-true (eq (funcall memory-backend-selected-payload-backend-fn fallback-event)
                                  :file)
                              "Expected fallback backend payload to be :file.")
                 (assert-true (eq (funcall memory-backend-selected-payload-reason-fn fallback-event)
                                  :haake-cli-not-found)
                              "Expected fallback reason :haake-cli-not-found.")
                 (assert-true (eq (funcall memory-backend-selected-payload-requested-backend-fn fallback-event)
                                  :auto)
                              "Expected requested backend to remain :auto for fallback."))

               (setf (symbol-value availability-runner-sym)
                     (lambda (command)
                       (string= command "haake")))
               (setf (symbol-value status-runner-sym)
                     (lambda (command &key directory)
                       (declare (ignore directory))
                       (string= command "haake")))
               (setf (symbol-value capability-runner-sym)
                     (lambda (command &key directory)
                       (declare (ignore directory))
                       (if (string= command "haake")
                           (list :exit-code 0
                                 :stdout "insert query list delete"
                                 :stderr "")
                           (list :exit-code 1 :stdout "" :stderr ""))))
               (setf (symbol-value command-runner-sym)
                     (lambda (arguments &key directory input)
                       (declare (ignore directory input))
                       (setf captured-commands
                             (append captured-commands (list arguments)))
                       (let ((action (third arguments))
                             (scope (fourth arguments)))
                         (cond
                           ((string= action "query")
                            (list :exit-code 0
                                  :stdout (format nil "alpha~CUse bun~%beta~CRun tests~%"
                                                  #\Tab
                                                  #\Tab)
                                  :stderr ""))
                           ((and (string= action "list")
                                 (search "/topic/style" scope :test #'char=))
                            (list :exit-code 0
                                  :stdout (format nil "style-key~CUse snake_case~%" #\Tab)
                                  :stderr ""))
                           ((string= action "list")
                            (list :exit-code 0
                                  :stdout (format nil "project-pref~CUse bun~%" #\Tab)
                                  :stderr ""))
                           (t
                            (list :exit-code 0
                                  :stdout ""
                                  :stderr ""))))))
               (funcall setconfig-fn :haake-project-id "smoke-project")
               (funcall setconfig-fn :haake-command "haake")
               (funcall setconfig-fn :haake-agent "amoebum-smoke")
               (funcall setconfig-fn :haake-autodetect t)
               (funcall setconfig-fn :memory-backend :auto)
               (setf (symbol-value session-memory-sym) '())
               (funcall reset-memory-backend-fn nil)

               (let ((haake-backend (funcall current-memory-backend-fn)))
                 (assert-true (funcall haake-cli-memory-backend-p-fn haake-backend)
                              "Expected auto detection to select Haake backend.")
                 (funcall memory-store-fn
                          haake-backend
                          "pref-key"
                          "Use bun"
                          :scope :global
                          :source :smoke)
                 (let ((query-result (funcall memory-query-fn
                                              haake-backend
                                              "bun"
                                              :scope :project
                                              :limit 5))
                       (topic-result (funcall memory-list-fn
                                              haake-backend
                                              :scope '(:topic "style"))))
                   (assert-true (= (length query-result) 2)
                                "Expected Haake query to parse two rows, got ~D."
                                (length query-result))
                   (assert-true (string= (funcall memory-entry-key-fn (first query-result))
                                         "alpha")
                                "Expected first query key to be alpha.")
                   (assert-true (string= (funcall memory-entry-value-fn (first query-result))
                                         "Use bun")
                                "Expected first query value to be parsed.")
                   (assert-true (eq (funcall memory-entry-scope-fn (first query-result)) :project)
                                "Expected query scope mapping to project.")
                   (assert-true (= (length topic-result) 1)
                                "Expected topic list to return one row.")
                   (assert-true (eq (funcall memory-entry-scope-fn (first topic-result)) :topic)
                                "Expected topic scope mapping to :topic."))

                 (assert-true (funcall memory-delete-fn haake-backend "alpha" :scope :project)
                              "Expected Haake delete to return true on success."))

               (assert-true (some (lambda (argv)
                                    (and (argv-contains-p argv "insert")
                                         (argv-contains-p argv "global/preferences")))
                                  captured-commands)
                            "Expected store command to target global/preferences scope.")
               (assert-true (some (lambda (argv)
                                    (and (argv-contains-p argv "query")
                                         (argv-contains-p argv "project/smoke-project/preferences")))
                                  captured-commands)
                            "Expected query command to target project preference scope.")
               (assert-true (some (lambda (argv)
                                    (and (argv-contains-p argv "list")
                                         (argv-contains-p argv "project/smoke-project/topic/style")))
                                  captured-commands)
                            "Expected topic list command to target project topic scope.")
               (assert-true (some (lambda (argv)
                                    (and (argv-contains-p argv "delete")
                                         (argv-contains-p argv "project/smoke-project/preferences")))
                                  captured-commands)
                            "Expected delete command to target project preference scope.")

               (let ((haake-event (car (last backend-events))))
                 (assert-true haake-event
                              "Expected memory:backend-selected event for Haake selection.")
                 (assert-true (eq (funcall memory-backend-selected-payload-backend-fn haake-event)
                                  :haake-cli)
                              "Expected selected backend payload to be :haake-cli.")
                 (assert-true (eq (funcall memory-backend-selected-payload-reason-fn haake-event)
                                  :auto-detected-haake-cli)
                              "Expected Haake selection reason :auto-detected-haake-cli.")
                 (assert-true (eq (funcall memory-backend-selected-payload-requested-backend-fn haake-event)
                                  :auto)
                              "Expected Haake selection requested backend :auto.")))
          (setf (symbol-value event-bus-sym) old-event-bus
                (symbol-value availability-runner-sym) old-availability-runner
                (symbol-value status-runner-sym) old-status-runner
                (symbol-value capability-runner-sym) old-capability-runner
                (symbol-value command-runner-sym) old-command-runner)))))

  (format t "AMOEBUM_HAAKE_SMOKE_OK~%"))
