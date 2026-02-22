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
         (load-extensions-fn (funcall fn-in "LOAD-USER-EXTENSIONS" amoebum-pkg))
         (list-loaded-fn (funcall fn-in "LIST-LOADED-EXTENSIONS" amoebum-pkg))
         (list-report-fn (funcall fn-in "LIST-EXTENSION-REPORT" amoebum-pkg))
         (summary-fn (funcall fn-in "EXTENSION-REPORT-SUMMARY" amoebum-pkg))
         (record-path-fn (funcall fn-in "EXTENSION-LOAD-RECORD-PATH" amoebum-pkg))
         (record-scope-fn (funcall fn-in "EXTENSION-LOAD-RECORD-SCOPE" amoebum-pkg))
         (record-status-fn (funcall fn-in "EXTENSION-LOAD-RECORD-STATUS" amoebum-pkg))
         (dispatch-fn (funcall fn-in "DISPATCH-SLASH-COMMAND" amoebum-pkg))
         (result-output-fn (funcall fn-in "SLASH-COMMAND-RESULT-OUTPUT" amoebum-pkg))
         (make-event-bus-fn (funcall fn-in "MAKE-EVENT-BUS" amoebum-pkg))
         (subscribe-fn (funcall fn-in "SUBSCRIBE" amoebum-pkg))
         (event-history-fn (funcall fn-in "EVENT-HISTORY" amoebum-pkg))
         (event-type-fn (funcall fn-in "EVENT-TYPE" amoebum-pkg))
         (event-bus-sym (funcall symbol-in "*EVENT-BUS*" amoebum-pkg))
         (disabled-sym (funcall symbol-in "*DISABLED-EXTENSIONS*" amoebum-pkg))
         (report-sym (funcall symbol-in "*EXTENSION-LOAD-REPORT*" amoebum-pkg))
         (loaded-sym (funcall symbol-in "*LOADED-EXTENSIONS*" amoebum-pkg))
         (discover-sym (funcall symbol-in "*EXTENSION-LAST-DISCOVERED*" amoebum-pkg))
         (global-override-sym
           (funcall symbol-in "*EXTENSIONS-GLOBAL-DIRECTORY-OVERRIDE*" amoebum-pkg))
         (project-override-sym
           (funcall symbol-in "*EXTENSIONS-PROJECT-DIRECTORY-OVERRIDE*" amoebum-pkg))
         (order-sym (intern "*EXTENSIONS-SMOKE-ORDER*" amoebum-pkg))
         (event-type-loaded
           (symbol-value (funcall symbol-in "+EVENT-TYPE-EXTENSION-LOADED+" amoebum-pkg)))
         (event-type-error
           (symbol-value (funcall symbol-in "+EVENT-TYPE-EXTENSION-ERROR+" amoebum-pkg)))
         (temporary-directory-fn (funcall fn-in "TEMPORARY-DIRECTORY" uiop-pkg))
         (ensure-directory-pathname-fn (funcall fn-in "ENSURE-DIRECTORY-PATHNAME" uiop-pkg))
         (delete-directory-tree-fn (ignore-errors (funcall fn-in "DELETE-DIRECTORY-TREE" uiop-pkg))))
    (labels ((assert-true (condition format-string &rest format-args)
               (unless condition
                 (error (apply #'format nil format-string format-args))))
             (contains-text-p (haystack needle)
               (and (stringp haystack)
                    (search needle haystack :test #'char-equal)))
             (write-file (path content)
               (ensure-directories-exist path)
               (with-open-file (stream path
                                       :direction :output
                                       :if-exists :supersede
                                       :if-does-not-exist :create
                                       :external-format :utf-8)
                 (write-string content stream))))
      (let* ((old-event-bus (symbol-value event-bus-sym))
             (old-global-override (symbol-value global-override-sym))
             (old-project-override (symbol-value project-override-sym))
             (temp-root (funcall ensure-directory-pathname-fn
                                 (merge-pathnames
                                  (make-pathname :directory
                                                 `(:relative
                                                   ,(format nil "amoebum-i69-~A"
                                                            (get-universal-time))))
                                  (funcall temporary-directory-fn))))
             (global-dir (merge-pathnames #P"global-ext/" temp-root))
             (project-dir (merge-pathnames #P"project-ext/" temp-root))
             (global-a (merge-pathnames #P"00-global-alpha.lisp" global-dir))
             (global-z (merge-pathnames #P"10-global-zeta.lisp" global-dir))
             (project-a (merge-pathnames #P"20-project-alpha.lisp" project-dir))
             (project-bad (merge-pathnames #P"25-project-broken.lisp" project-dir))
             (project-z (merge-pathnames #P"30-project-gamma.lisp" project-dir))
             (bus (funcall make-event-bus-fn :capacity 64))
             (loaded-events '())
             (error-events '()))
        (unwind-protect
            (progn
              (setf (symbol-value event-bus-sym) bus
                    (symbol-value global-override-sym) global-dir
                    (symbol-value project-override-sym) project-dir
                    (symbol-value report-sym) '()
                    (symbol-value loaded-sym) '()
                    (symbol-value discover-sym) '()
                    (symbol-value order-sym) '())
              (clrhash (symbol-value disabled-sym))

              (funcall subscribe-fn
                       bus
                       event-type-loaded
                       (lambda (event)
                         (declare (ignore event))
                         (push event-type-loaded loaded-events)))
              (funcall subscribe-fn
                       bus
                       event-type-error
                       (lambda (event)
                         (declare (ignore event))
                         (push event-type-error error-events)))

              (write-file global-a
                          "(in-package :amoebum)
(defvar *extensions-smoke-order* '())
(setf *extensions-smoke-order* (append *extensions-smoke-order* (list \"global-alpha\")))
")
              (write-file global-z
                          "(in-package :amoebum)
(defvar *extensions-smoke-order* '())
(setf *extensions-smoke-order* (append *extensions-smoke-order* (list \"global-zeta\")))
")
              (write-file project-a
                          "(in-package :amoebum)
(defvar *extensions-smoke-order* '())
(setf *extensions-smoke-order* (append *extensions-smoke-order* (list \"project-alpha\")))
")
              (write-file project-bad
                          "(in-package :amoebum)
(error \"I69 project extension boom\")
")
              (write-file project-z
                          "(in-package :amoebum)
(defvar *extensions-smoke-order* '())
(setf *extensions-smoke-order* (append *extensions-smoke-order* (list \"project-gamma\")))
")

              (let* ((report (funcall load-extensions-fn))
                     (summary (funcall summary-fn report))
                     (loaded (funcall list-loaded-fn))
                     (paths (mapcar record-path-fn loaded))
                     (scopes (mapcar record-scope-fn loaded))
                     (statuses (mapcar record-status-fn report)))
                (assert-true (= (getf summary :loaded 0) 4)
                             "Expected 4 loaded extensions, got summary ~S." summary)
                (assert-true (= (getf summary :errors 0) 1)
                             "Expected 1 extension error, got summary ~S." summary)
                (assert-true (equal (symbol-value order-sym)
                                    '("global-alpha" "global-zeta" "project-alpha" "project-gamma"))
                             "Expected ordered extension side effects across scopes with isolation, got ~S."
                             (symbol-value order-sym))
                (assert-true (member :global scopes :test #'eq)
                             "Expected at least one global extension in loaded list, got scopes=~S." scopes)
                (assert-true (member :project scopes :test #'eq)
                             "Expected at least one project extension in loaded list, got scopes=~S." scopes)
                (assert-true (every (lambda (status) (member status '(:loaded :error :disabled) :test #'eq))
                                    statuses)
                             "Unexpected extension status values: ~S." statuses)
                (assert-true (some (lambda (path) (contains-text-p path "00-global-alpha.lisp")) paths)
                             "Expected global extension path in loaded list, paths=~S." paths)
                (assert-true (some (lambda (path) (contains-text-p path "30-project-gamma.lisp")) paths)
                             "Expected project extension path in loaded list, paths=~S." paths))

              (assert-true (= (length loaded-events) 4)
                           "Expected 4 extension:loaded events, got ~D." (length loaded-events))
              (assert-true (= (length error-events) 1)
                           "Expected 1 extension:error event, got ~D." (length error-events))

              (multiple-value-bind (handledp result)
                  (funcall dispatch-fn "/extensions")
                (declare (ignore result))
                (assert-true handledp "Expected /extensions to be handled."))

              (multiple-value-bind (handledp result)
                  (funcall dispatch-fn "/extensions list")
                (let ((output (and result (funcall result-output-fn result))))
                  (assert-true handledp "Expected /extensions list to be handled.")
                  (assert-true (contains-text-p output "loaded=4")
                               "Expected /extensions list output to include loaded count, got ~S." output)
                  (assert-true (contains-text-p output "errors=1")
                               "Expected /extensions list output to include error count, got ~S." output)
                  (assert-true (contains-text-p output "25-project-broken.lisp")
                               "Expected /extensions list output to include failing extension path, got ~S." output)))

              (multiple-value-bind (handledp result)
                  (funcall dispatch-fn "/extensions disable 00-global-alpha.lisp")
                (let ((output (and result (funcall result-output-fn result))))
                  (assert-true handledp "Expected /extensions disable to be handled.")
                  (assert-true (contains-text-p output "Disabled 1 extension(s)")
                               "Expected disable confirmation, got ~S." output)))

              (multiple-value-bind (handledp result)
                  (funcall dispatch-fn "/extensions reload")
                (let ((output (and result (funcall result-output-fn result))))
                  (assert-true handledp "Expected /extensions reload to be handled.")
                  (assert-true (contains-text-p output "loaded=3")
                               "Expected reload output to reflect one disabled extension, got ~S." output)
                  (assert-true (contains-text-p output "errors=1")
                               "Expected reload output to preserve isolated failure count, got ~S." output)))

              (let ((history (funcall list-report-fn)))
                (assert-true (some (lambda (event)
                                     (eq (funcall event-type-fn event) event-type-error))
                                   (funcall event-history-fn bus))
                             "Expected event history to include extension:error event.")
                (assert-true (some (lambda (entry)
                                     (and (eq (funcall record-status-fn entry) :disabled)
                                          (contains-text-p (funcall record-path-fn entry)
                                                           "00-global-alpha.lisp")))
                                   history)
                             "Expected disabled extension status after reload, history=~S." history)))
          (setf (symbol-value event-bus-sym) old-event-bus
                (symbol-value global-override-sym) old-global-override
                (symbol-value project-override-sym) old-project-override
                (symbol-value order-sym) '())
          (clrhash (symbol-value disabled-sym))
          (setf (symbol-value report-sym) '()
                (symbol-value loaded-sym) '()
                (symbol-value discover-sym) '())
          (when (and delete-directory-tree-fn
                     (probe-file temp-root))
            (ignore-errors
              (funcall delete-directory-tree-fn temp-root :validate t :if-does-not-exist :ignore)))))))

  (format t "AMOEBUM_EXTENSIONS_SMOKE_OK~%"))
