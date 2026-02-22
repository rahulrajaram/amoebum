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
                 (error "Missing symbol ~A in package ~A."
                        name
                        (package-name package)))))
         (fn-in
           (lambda (name package)
             (symbol-function (funcall symbol-in name package))))
         (make-toolset-fn (funcall fn-in "MAKE-TOOLSET" pseudopod-pkg))
         (find-tool-fn (funcall fn-in "FIND-TOOL" pseudopod-pkg))
         (tool-definition-fn-fn (funcall fn-in "TOOL-DEFINITION-FN" pseudopod-pkg))
         (deftool-sym (funcall symbol-in "DEFTOOL" amoebum-pkg))
         (toolset-sym (funcall symbol-in "*TOOLSET*" amoebum-pkg))
         (tool-metadata-sym (funcall symbol-in "*TOOL-METADATA*" amoebum-pkg))
         (tool-history-sym (funcall symbol-in "*TOOL-HISTORY*" amoebum-pkg))
         (tool-history-fn (funcall fn-in "TOOL-HISTORY" amoebum-pkg))
         (tool-metadata-timeout-fn (funcall fn-in "TOOL-METADATA-TIMEOUT-SECONDS" amoebum-pkg))
         (make-event-bus-fn (funcall fn-in "MAKE-EVENT-BUS" amoebum-pkg))
         (subscribe-fn (funcall fn-in "SUBSCRIBE" amoebum-pkg))
         (event-payload-fn (funcall fn-in "EVENT-PAYLOAD" amoebum-pkg))
         (event-bus-sym (funcall symbol-in "*EVENT-BUS*" amoebum-pkg))
         (event-type-tool-redefined
           (symbol-value (funcall symbol-in "+EVENT-TYPE-TOOL-REDEFINED+" amoebum-pkg)))
         (payload-diff-fn (funcall fn-in "TOOL-REDEFINED-PAYLOAD-METADATA-DIFF" amoebum-pkg))
         (dispatch-fn (funcall fn-in "DISPATCH-SLASH-COMMAND" amoebum-pkg))
         (result-output-fn (funcall fn-in "SLASH-COMMAND-RESULT-OUTPUT" amoebum-pkg)))
    (labels ((assert-true (condition format-string &rest format-args)
               (unless condition
                 (error (apply #'format nil format-string format-args))))
             (contains-text-p (haystack needle)
               (and (stringp haystack)
                    (search needle haystack :test #'char-equal))))
      (setf (symbol-value toolset-sym) (funcall make-toolset-fn))
      (clrhash (symbol-value tool-metadata-sym))
      (clrhash (symbol-value tool-history-sym))
      (setf (symbol-value event-bus-sym) (funcall make-event-bus-fn :capacity 64))

      (let ((captured-events '())
            (bus (symbol-value event-bus-sym)))
        (funcall subscribe-fn
                 bus
                 event-type-tool-redefined
                 (lambda (event)
                   (push event captured-events)))

        (eval
         `(,deftool-sym i75-reload-tool ()
            "I75 reload tool v1"
            (:permission :auto)
            (:dangerous nil)
            (:category :i75-v1)
            (:timeout 7)
            "v1"))

        (assert-true (null (funcall tool-history-fn "i75-reload-tool"))
                     "Expected no history entries after first definition.")

        (eval
         `(,deftool-sym i75-reload-tool ()
            "I75 reload tool v2"
            (:permission :auto)
            (:dangerous nil)
            (:category :i75-v2)
            (:timeout 11)
            "v2"))

        (let ((history (funcall tool-history-fn "i75-reload-tool")))
          (assert-true (= (length history) 1)
                       "Expected one history entry after redefinition, got ~D."
                       (length history))
          (assert-true (and (integerp (getf (first history) :timestamp))
                            (plusp (getf (first history) :timestamp)))
                       "Expected history entry timestamp to be populated, got ~S."
                       (getf (first history) :timestamp)))

        (assert-true (= (length captured-events) 1)
                     "Expected one tool:redefined event after redefinition, got ~D."
                     (length captured-events))
        (let* ((payload (funcall event-payload-fn (first captured-events)))
               (diff (funcall payload-diff-fn payload))
               (timeout-diff (find :timeout-seconds
                                   diff
                                   :key (lambda (entry) (getf entry :field))
                                   :test #'eq)))
          (assert-true timeout-diff
                       "Expected tool:redefined metadata diff to include :timeout-seconds, got ~S."
                       diff))

        (multiple-value-bind (handledp result)
            (funcall dispatch-fn "/tool-history i75-reload-tool")
          (let ((output (funcall result-output-fn result)))
            (assert-true handledp "Expected /tool-history to be handled.")
            (assert-true (contains-text-p output "Tool history for i75-reload-tool")
                         "Expected /tool-history output header, got ~S."
                         output)))

        (multiple-value-bind (handledp result)
            (funcall dispatch-fn "/tool-rollback i75-reload-tool")
          (let ((output (funcall result-output-fn result)))
            (assert-true handledp "Expected /tool-rollback to be handled.")
            (assert-true (contains-text-p output "Rolled back i75-reload-tool")
                         "Expected /tool-rollback success output, got ~S."
                         output)))

        (let* ((tool (funcall find-tool-fn (symbol-value toolset-sym) "i75-reload-tool"))
               (fn (funcall tool-definition-fn-fn tool))
               (args (make-hash-table :test #'equal))
               (result (funcall fn args))
               (metadata (gethash "i75-reload-tool" (symbol-value tool-metadata-sym)))
               (history-after-rollback (funcall tool-history-fn "i75-reload-tool")))
          (assert-true (string= result "v1")
                       "Expected rollback to restore v1 body, got ~S."
                       result)
          (assert-true (= (funcall tool-metadata-timeout-fn metadata) 7)
                       "Expected rollback to restore v1 metadata timeout, got ~S."
                       (funcall tool-metadata-timeout-fn metadata))
          (assert-true (= (length history-after-rollback) 1)
                       "Expected history to retain one version after rollback, got ~D."
                       (length history-after-rollback)))))

    (format t "AMOEBUM_TOOL_RELOAD_SMOKE_OK~%")))
