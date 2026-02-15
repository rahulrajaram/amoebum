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
         (symbol-in
           (lambda (name package)
             (or (find-symbol name package)
                 (error "Missing symbol ~A in package ~A." name (package-name package)))))
         (fn
           (lambda (name)
             (symbol-function (funcall symbol-in name amoebum-pkg))))
         (make-event-bus-fn (funcall fn "MAKE-EVENT-BUS"))
         (subscribe-fn (funcall fn "SUBSCRIBE"))
         (publish-fn (funcall fn "PUBLISH"))
         (make-event-fn (funcall fn "MAKE-EVENT"))
         (make-tool-invoked-event-fn (funcall fn "MAKE-TOOL-INVOKED-EVENT"))
         (make-tool-error-event-fn (funcall fn "MAKE-TOOL-ERROR-EVENT"))
         (make-permission-prompted-event-fn (funcall fn "MAKE-PERMISSION-PROMPTED-EVENT"))
         (filter-by-type-fn (funcall fn "FILTER-BY-TYPE"))
         (filter-by-severity-fn (funcall fn "FILTER-BY-SEVERITY"))
         (filter-by-source-fn (funcall fn "FILTER-BY-SOURCE"))
         (filter-and-fn (funcall fn "FILTER-AND"))
         (filter-or-fn (funcall fn "FILTER-OR"))
         (filter-not-fn (funcall fn "FILTER-NOT"))
         (filter-by-tool-fn (funcall fn "FILTER-BY-TOOL"))
         (filter-by-permission-mode-fn (funcall fn "FILTER-BY-PERMISSION-MODE"))
         (event-router-sym (funcall symbol-in "EVENT-ROUTER" amoebum-pkg))
         (event-type-tool-invoked
           (symbol-value (funcall symbol-in "+EVENT-TYPE-TOOL-INVOKED+" amoebum-pkg))))
    (labels ((assert-true (condition format-string &rest format-args)
               (unless condition
                 (error (apply #'format nil format-string format-args)))))
      (let* ((composed-filter
               (funcall filter-and-fn
                        (funcall filter-by-type-fn event-type-tool-invoked)
                        (funcall filter-by-source-fn :amoebum)
                        (funcall filter-not-fn (funcall filter-by-severity-fn :error))
                        (funcall filter-by-tool-fn "shell")
                        (funcall filter-by-permission-mode-fn :auto-edit)))
             (matching-event
               (funcall make-tool-invoked-event-fn
                        :tool-name "shell"
                        :args '(:command "ls")
                        :permission-mode :auto-edit
                        :request-id "evt-1"))
             (wrong-tool-event
               (funcall make-tool-invoked-event-fn
                        :tool-name "read-file"
                        :args '(:path "README.md")
                        :permission-mode :auto-edit
                        :request-id "evt-2"))
             (wrong-permission-event
               (funcall make-tool-invoked-event-fn
                        :tool-name "shell"
                        :args '(:command "pwd")
                        :permission-mode :ask
                        :request-id "evt-3")))
        (assert-true (funcall composed-filter matching-event)
                     "Expected composed filter to match tool:invoked shell auto-edit event.")
        (assert-true (not (funcall composed-filter wrong-tool-event))
                     "Expected composed filter to reject event with non-matching tool.")
        (assert-true (not (funcall composed-filter wrong-permission-event))
                     "Expected composed filter to reject event with non-matching permission mode."))

      (let ((or-filter (funcall filter-or-fn
                                (funcall filter-by-severity-fn :error)
                                (funcall filter-by-permission-mode-fn :on-request))))
        (assert-true (funcall or-filter
                              (funcall make-tool-error-event-fn
                                       :tool-name "shell"
                                       :args '(:command "bad")
                                       :condition "boom"
                                       :elapsed-ms 12
                                       :request-id "evt-4"))
                     "Expected OR filter to match error event.")
        (assert-true (funcall or-filter
                              (funcall make-permission-prompted-event-fn
                                       :tool-name "shell"
                                       :path nil
                                       :command "rm -rf /tmp/a"
                                       :reason "dangerous"
                                       :permission-mode :on-request))
                     "Expected OR filter to match permission-mode event.")
        (assert-true (not (funcall or-filter
                                   (funcall make-event-fn
                                            :type :custom-event
                                            :source :amoebum
                                            :severity :info
                                            :payload nil)))
                     "Expected OR filter to reject unrelated event."))

      (let* ((router
               (eval `(,event-router-sym
                        :name i79-smoke-router
                        ((:type ,event-type-tool-invoked)
                         (lambda (event)
                           (declare (ignore event))
                           :type))
                        ((:severity :error)
                         (lambda (event)
                           (declare (ignore event))
                           :severity))
                        ((:tool "shell")
                         (lambda (event)
                           (declare (ignore event))
                           :tool))
                        (t
                         (lambda (event)
                           (declare (ignore event))
                           :default))))))
        (assert-true (functionp router)
                     "Expected EVENT-ROUTER to produce a callable router.")
        (assert-true (fboundp 'i79-smoke-router)
                     "Expected EVENT-ROUTER :NAME to define I79-SMOKE-ROUTER.")
        (assert-true (eq (funcall router
                                  (funcall make-tool-invoked-event-fn
                                           :tool-name "shell"
                                           :args nil
                                           :permission-mode :auto-edit
                                           :request-id "evt-5"))
                         :type)
                     "Expected router to dispatch by :type clause first.")
        (assert-true (eq (funcall router
                                  (funcall make-tool-error-event-fn
                                           :tool-name "shell"
                                           :args nil
                                           :condition "err"
                                           :elapsed-ms 1
                                           :request-id "evt-6"))
                         :severity)
                     "Expected router to dispatch by :severity clause.")
        (assert-true (eq (funcall router
                                  (funcall make-permission-prompted-event-fn
                                           :tool-name "shell"
                                           :path nil
                                           :command "ls"
                                           :reason "manual confirm"
                                           :permission-mode :on-request))
                         :tool)
                     "Expected router to dispatch by :tool clause.")
        (assert-true (eq (funcall router
                                  (funcall make-event-fn
                                           :type :custom-other
                                           :source :external
                                           :severity :info
                                           :payload nil))
                         :default)
                     "Expected router default clause to catch unmatched events."))

      (let ((bus (funcall make-event-bus-fn :capacity 64))
            (all-count 0)
            (filtered-count 0))
        (funcall subscribe-fn
                 bus
                 :*
                 (lambda (event)
                   (declare (ignore event))
                   (incf all-count)))
        (funcall subscribe-fn
                 bus
                 :*
                 (lambda (event)
                   (declare (ignore event))
                   (incf filtered-count))
                 :filter (funcall filter-and-fn
                                  (funcall filter-by-severity-fn :error)
                                  (funcall filter-by-tool-fn "shell")))
        (funcall publish-fn
                 bus
                 (funcall make-tool-invoked-event-fn
                          :tool-name "shell"
                          :args nil
                          :permission-mode :auto-edit
                          :request-id "evt-7"))
        (funcall publish-fn
                 bus
                 (funcall make-tool-error-event-fn
                          :tool-name "read-file"
                          :args nil
                          :condition "missing"
                          :elapsed-ms 4
                          :request-id "evt-8"))
        (funcall publish-fn
                 bus
                 (funcall make-tool-error-event-fn
                          :tool-name "shell"
                          :args nil
                          :condition "fail"
                          :elapsed-ms 7
                          :request-id "evt-9"))
        (funcall publish-fn
                 bus
                 (funcall make-permission-prompted-event-fn
                          :tool-name "shell"
                          :path nil
                          :command "rm"
                          :reason "dangerous"
                          :permission-mode :on-request))
        (funcall publish-fn
                 bus
                 (funcall make-tool-error-event-fn
                          :tool-name "shell"
                          :args nil
                          :condition "another-fail"
                          :elapsed-ms 2
                          :request-id "evt-10"))
        (assert-true (= all-count 5)
                     "Expected unfiltered subscription to receive all events.")
        (assert-true (= filtered-count 2)
                     "Expected filtered subscription to receive only matching events.")
        (assert-true (< filtered-count all-count)
                     "Expected :filter to reduce callback invocations.")))

  (format t "AMOEBUM_EVENT_FILTERS_SMOKE_OK~%")))
