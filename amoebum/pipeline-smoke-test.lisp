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
    (funcall load-asd-fn (merge-pathnames #P"sw4rm-sdk/sw4rm-sdk.asd" repo-root))
    (funcall load-asd-fn (merge-pathnames #P"ptui/ptui.asd" repo-root))
    (funcall load-asd-fn (merge-pathnames #P"amoebum/amoebum.asd" repo-root))
    (funcall load-system-fn "amoebum"))

  (let* ((amoebum-pkg (or (find-package "AMOEBUM")
                          (error "Missing package AMOEBUM after load.")))
         (pseudopod-pkg (or (find-package "PSEUDOPOD")
                            (error "Missing package PSEUDOPOD after load.")))
         (symbol-in
           (lambda (name package)
             (or (find-symbol name package)
                 (error "Missing symbol ~A in package ~A." name (package-name package)))))
         (fn-in
           (lambda (name package)
             (symbol-function (funcall symbol-in name package))))
         (make-toolset-fn (funcall fn-in "MAKE-TOOLSET" pseudopod-pkg))
         (make-tool-call-fn (funcall fn-in "MAKE-TOOL-CALL" pseudopod-pkg))
         (deftool-sym (funcall symbol-in "DEFTOOL" amoebum-pkg))
         (toolset-sym (funcall symbol-in "*TOOLSET*" amoebum-pkg))
         (metadata-sym (funcall symbol-in "*TOOL-METADATA*" amoebum-pkg))
         (clear-hooks-fn (funcall fn-in "CLEAR-HOOKS" amoebum-pkg))
         (make-event-bus-fn (funcall fn-in "MAKE-EVENT-BUS" amoebum-pkg))
         (subscribe-fn (funcall fn-in "SUBSCRIBE" amoebum-pkg))
         (event-type-fn (funcall fn-in "EVENT-TYPE" amoebum-pkg))
         (make-context-fn (funcall fn-in "MAKE-AMOEBUM-CONTEXT" amoebum-pkg))
         (execute-tool-fn (funcall fn-in "EXECUTE-TOOL" amoebum-pkg))
         (context-tool-metrics-fn (funcall fn-in "CONTEXT-TOOL-METRICS" amoebum-pkg))
         (cached-tool-result-fn (funcall fn-in "CACHED-TOOL-RESULT" amoebum-pkg))
         (event-type-invoked (symbol-value (funcall symbol-in "+EVENT-TYPE-TOOL-INVOKED+" amoebum-pkg)))
         (event-type-completed (symbol-value (funcall symbol-in "+EVENT-TYPE-TOOL-COMPLETED+" amoebum-pkg)))
         (permission-denied-sym (funcall symbol-in "TOOL-PERMISSION-DENIED" amoebum-pkg))
         (missing-arg-sym (funcall symbol-in "TOOL-MISSING-ARGUMENT" amoebum-pkg))
         (counter-sym (intern "*I33-TOOL-BODY-COUNT*" amoebum-pkg))
         (*package* amoebum-pkg))
    (labels ((assert-true (condition format-string &rest format-args)
               (unless condition
                 (error (apply #'format nil format-string format-args))))
             (make-args (&rest pairs)
               (let ((table (make-hash-table :test #'equal)))
                 (loop for (key value) on pairs by #'cddr do
                       (setf (gethash key table) value))
                 table)))
      (setf (symbol-value toolset-sym) (funcall make-toolset-fn))
      (clrhash (symbol-value metadata-sym))
      (funcall clear-hooks-fn)
      (setf (symbol-value counter-sym) 0)

      (eval
       `(,deftool-sym i33-pipeline-tool
          ((value integer :description "Pipeline value." :required t))
          "I33 pipeline smoke test tool."
          (:permission :auto)
          (:dangerous nil)
          (:category :smoke)
          (:timeout 5)
          (incf (symbol-value ',counter-sym))
          (format nil "value=~D" value)))

      (let* ((event-bus (funcall make-event-bus-fn :capacity 16))
             (event-order '())
             (context (funcall make-context-fn
                               :toolset (symbol-value toolset-sym)
                               :permission-mode :full-auto
                               :event-bus event-bus)))
        (funcall subscribe-fn
                 event-bus
                 event-type-invoked
                 (lambda (event)
                   (setf event-order
                         (append event-order (list (funcall event-type-fn event)))))
                 :priority 10)
        (funcall subscribe-fn
                 event-bus
                 event-type-completed
                 (lambda (event)
                   (setf event-order
                         (append event-order (list (funcall event-type-fn event)))))
                 :priority 10)

        (let* ((arguments (make-args "value" 7))
               (call (funcall make-tool-call-fn
                              :name "i33-pipeline-tool"
                              :arguments "{\"value\":7}"))
               (result (funcall execute-tool-fn call context)))
          (assert-true (string= result "value=7")
                       "Expected execute-tool primary result, got ~S." result)
          (assert-true (= (symbol-value counter-sym) 1)
                       "Expected tool body to execute exactly once in allow path.")
          (assert-true (equal event-order
                              (list event-type-invoked event-type-completed))
                       "Expected invoked/completed event ordering, got ~S." event-order)
          (let ((metrics (funcall context-tool-metrics-fn context "i33-pipeline-tool")))
            (assert-true (= (getf metrics :count) 1)
                         "Expected metrics count=1, got ~S." metrics)
            (assert-true (eq (getf metrics :last-status) :ok)
                         "Expected last-status :ok, got ~S." metrics))
          (let ((cached (funcall cached-tool-result-fn
                                 context
                                 "i33-pipeline-tool"
                                 arguments)))
            (assert-true (string= cached "value=7")
                         "Expected result cache hit for completed tool call."))))

      (let* ((blocked-context
               (funcall make-context-fn
                        :toolset (symbol-value toolset-sym)
                        :permission-mode :supervised
                        :event-bus (funcall make-event-bus-fn :capacity 8)))
             (blocked-call
               (funcall make-tool-call-fn
                        :name "i33-pipeline-tool"
                        :arguments "{\"value\":1}"))
             (saw-permission-denied nil)
             (before-count (symbol-value counter-sym)))
        (handler-case
            (funcall execute-tool-fn blocked-call blocked-context)
          (error (condition)
            (when (typep condition permission-denied-sym)
              (setf saw-permission-denied t))))
        (assert-true saw-permission-denied
                     "Expected supervised permission gate to deny before tool body.")
        (assert-true (= (symbol-value counter-sym) before-count)
                     "Expected permission check to run before tool body execution."))

      (let* ((validation-context
               (funcall make-context-fn
                        :toolset (symbol-value toolset-sym)
                        :permission-mode :full-auto
                        :event-bus (funcall make-event-bus-fn :capacity 8)))
             (bad-call
               (funcall make-tool-call-fn
                        :name "i33-pipeline-tool"
                        :arguments "{}"))
             (saw-missing-arg nil)
             (before-count (symbol-value counter-sym)))
        (handler-case
            (funcall execute-tool-fn bad-call validation-context)
          (error (condition)
            (when (typep condition missing-arg-sym)
              (setf saw-missing-arg t))))
        (assert-true saw-missing-arg
                     "Expected before-method argument validation to signal TOOL-MISSING-ARGUMENT.")
        (assert-true (= (symbol-value counter-sym) before-count)
                     "Expected missing-arg validation to prevent tool body execution."))))

  (format t "AMOEBUM_PIPELINE_SMOKE_OK~%"))
