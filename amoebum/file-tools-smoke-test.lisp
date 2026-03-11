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
         (ensure-directory-pathname-fn (funcall fn-in "ENSURE-DIRECTORY-PATHNAME" uiop-pkg))
         (read-file-string-fn (funcall fn-in "READ-FILE-STRING" uiop-pkg))
         (find-tool-fn (funcall fn-in "FIND-TOOL" pseudopod-pkg))
         (tool-definition-fn-fn (funcall fn-in "TOOL-DEFINITION-FN" pseudopod-pkg))
         (toolset-sym (funcall symbol-in "*TOOLSET*" amoebum-pkg))
         (pdf-text-extractor-sym (funcall symbol-in "*PDF-TEXT-EXTRACTOR*" amoebum-pkg))
         (tool-argument-error-sym (funcall symbol-in "TOOL-ARGUMENT-ERROR" amoebum-pkg))
         (tool-error-reason-code-fn (funcall symbol-in "TOOL-ERROR-REASON-CODE" amoebum-pkg))
         (file-read-snapshots-sym (funcall symbol-in "*FILE-READ-SNAPSHOTS*" amoebum-pkg))
         (syntax-validator-runner-sym
           (funcall symbol-in "*EDIT-FILE-SYNTAX-VALIDATOR-RUNNER*" amoebum-pkg))
         (setconfig-fn (funcall fn-in "SETCONFIG" amoebum-pkg))
         (clear-permission-rules-fn (funcall fn-in "CLEAR-PERMISSION-RULES" amoebum-pkg))
         (add-permission-rule-fn (funcall fn-in "ADD-PERMISSION-RULE" amoebum-pkg)))
    (labels ((assert-true (condition format-string &rest format-args)
               (unless condition
                 (error (apply #'format nil format-string format-args))))
             (contains-substring-p (needle haystack)
               (not (null (search needle haystack :test #'char=))))
             (make-args (&rest key-values)
               (let ((args (make-hash-table :test #'equal)))
                 (loop for (key value) on key-values by #'cddr do
                       (setf (gethash key args) value))
                 args))
             (invoke-tool (tool-name &rest key-values)
               (let* ((toolset (symbol-value toolset-sym))
                      (tool (funcall find-tool-fn toolset tool-name)))
                 (assert-true tool "Expected tool ~S to be registered." tool-name)
                 (funcall (funcall tool-definition-fn-fn tool)
                          (apply #'make-args key-values)))))
      (funcall setconfig-fn :permission-mode :full-auto)
      (funcall clear-permission-rules-fn)
      (funcall setconfig-fn :edit-file-syntax-validators nil)
      (let ((read-snapshots (symbol-value file-read-snapshots-sym)))
        (when (hash-table-p read-snapshots)
          (clrhash read-snapshots)))

      (let* ((tmp-root
               (funcall ensure-directory-pathname-fn
                        (merge-pathnames
                         (make-pathname :directory `(:relative ".tmp-file-tools-smokes"
                                                             ,(format nil "amoebum-i27-~A"
                                                                      (get-universal-time))))
                         repo-root)))
             (read-source (merge-pathnames #P"fixtures/read-source.txt" tmp-root))
             (pdf-source (merge-pathnames #P"fixtures/sample.pdf" tmp-root))
             (image-source (merge-pathnames #P"fixtures/sample.png" tmp-root))
             (notebook-source (merge-pathnames #P"fixtures/sample.ipynb" tmp-root))
             (csv-source (merge-pathnames #P"fixtures/sample.csv" tmp-root))
             (tsv-source (merge-pathnames #P"fixtures/sample.tsv" tmp-root))
             (write-target (merge-pathnames #P"out/write-target.txt" tmp-root))
             (unread-edit-target (merge-pathnames #P"out/unread-edit-target.txt" tmp-root))
             (edit-target (merge-pathnames #P"out/edit-target.txt" tmp-root))
             (conflict-target (merge-pathnames #P"out/conflict-target.txt" tmp-root))
             (stale-write-target (merge-pathnames #P"out/stale-write-target.txt" tmp-root))
             (syntax-target (merge-pathnames #P"out/syntax-target.txt" tmp-root))
             (deny-target (merge-pathnames #P"blocked/deny.txt" tmp-root)))
        (ensure-directories-exist read-source)
        (with-open-file (stream read-source
                                :direction :output
                                :if-exists :supersede
                                :if-does-not-exist :create
                                :external-format :utf-8)
          (write-line "alpha" stream)
          (write-line "beta" stream)
          (write-line "gamma" stream)
          (write-line "delta" stream))
        (with-open-file (stream pdf-source
                                :direction :output
                                :if-exists :supersede
                                :if-does-not-exist :create
                                :external-format :utf-8)
          (write-line "%PDF-1.4" stream))
        (with-open-file (stream image-source
                                :direction :output
                                :if-exists :supersede
                                :if-does-not-exist :create
                                :element-type '(unsigned-byte 8))
          (write-sequence #(0 1 2 3 4) stream))
        (with-open-file (stream notebook-source
                                :direction :output
                                :if-exists :supersede
                                :if-does-not-exist :create
                                :external-format :utf-8)
          (write-string
           "{\"cells\":[{\"cell_type\":\"markdown\",\"source\":[\"# Title\\n\",\"Notebook intro\\n\"]},{\"cell_type\":\"code\",\"source\":[\"print('hello')\\n\"],\"outputs\":[{\"output_type\":\"stream\",\"name\":\"stdout\",\"text\":[\"hello from notebook\\n\"]}]}],\"metadata\":{\"language_info\":{\"name\":\"python\"}},\"nbformat\":4,\"nbformat_minor\":5}"
           stream))
        (with-open-file (stream csv-source
                                :direction :output
                                :if-exists :supersede
                                :if-does-not-exist :create
                                :external-format :utf-8)
          (write-line "name,score" stream)
          (write-line "alice,10" stream)
          (write-line "bob,200" stream))
        (with-open-file (stream tsv-source
                                :direction :output
                                :if-exists :supersede
                                :if-does-not-exist :create
                                :external-format :utf-8)
          (format stream "lang~Cfiles~%" #\Tab)
          (format stream "lisp~C42~%" #\Tab)
          (format stream "python~C108~%" #\Tab))

        (let ((read-result (invoke-tool "read-file"
                                        "path" (namestring read-source)
                                        "offset" 1
                                        "limit" 2)))
          (assert-true (contains-substring-p (format nil "2~Cbeta" #\Tab) read-result)
                       "Expected read-file output to include line 2 with content.")
          (assert-true (contains-substring-p (format nil "3~Cgamma" #\Tab) read-result)
                       "Expected read-file output to include line 3 with content.")
          (assert-true (not (contains-substring-p (format nil "1~Calpha" #\Tab) read-result))
                       "Expected read-file offset to skip earlier lines."))

        (invoke-tool "write-file"
                     "path" (namestring write-target)
                     "content" (format nil "one~%two~%"))
        (assert-true (probe-file write-target)
                     "Expected write-file to create target file.")
        (assert-true (string= (funcall read-file-string-fn write-target :external-format :utf-8)
                              (format nil "one~%two~%"))
                     "Expected write-file to persist provided content.")

        (invoke-tool "write-file"
                     "path" (namestring unread-edit-target)
                     "content" (format nil "one~%two~%"))
        (let ((saw-read-before-edit-gate nil))
          (handler-case
              (invoke-tool "edit-file"
                           "path" (namestring unread-edit-target)
                           "old-string" "two"
                           "new-string" "TWO")
            (condition (condition)
              (when (typep condition tool-argument-error-sym)
                (setf saw-read-before-edit-gate t)
                (assert-true (eq (funcall tool-error-reason-code-fn condition)
                                 :read-provenance-required)
                            "Expected unread edit-file to report read-provenance-required.")
                (assert-true
                 (contains-substring-p
                  "read-file is required before edit-file"
                  (princ-to-string condition))
                 "Expected unread edit-file path to include read-before-edit guidance."))))
          (assert-true saw-read-before-edit-gate
                       "Expected edit-file on unread path to signal tool-argument-error."))

        (invoke-tool "write-file"
                     "path" (namestring edit-target)
                     "content" (format nil "one~%two~%two~%"))
        (invoke-tool "read-file"
                     "path" (namestring edit-target))
        (let ((edit-result (invoke-tool "edit-file"
                                        "path" (namestring edit-target)
                                        "old-string" "two"
                                        "new-string" "TWO")))
          (assert-true (eq (getf edit-result :conflict-detected) nil)
                       "Expected no conflict warning when file is unchanged since read."))
        (let ((edited (funcall read-file-string-fn edit-target :external-format :utf-8)))
          (assert-true (contains-substring-p "TWO" edited)
                       "Expected edit-file to apply replacement.")
          (assert-true (not (contains-substring-p "two" edited))
                       "Expected edit-file to replace all exact matches."))

        (invoke-tool "write-file"
                     "path" (namestring conflict-target)
                     "content" (format nil "one~%two~%"))
        (invoke-tool "read-file"
                     "path" (namestring conflict-target))
        (with-open-file (stream conflict-target
                                :direction :output
                                :if-exists :supersede
                                :if-does-not-exist :create
                                :external-format :utf-8)
          (write-line "one" stream)
          (write-line "two" stream)
          (write-line "three" stream))
        (let ((saw-stale-edit-gate nil))
          (handler-case
              (invoke-tool "edit-file"
                           "path" (namestring conflict-target)
                           "old-string" "two"
                           "new-string" "TWO")
            (condition (condition)
              (when (typep condition tool-argument-error-sym)
                (setf saw-stale-edit-gate t)
                (assert-true (eq (funcall tool-error-reason-code-fn condition)
                                 :stale-content)
                            "Expected stale-file edit failure to report stale-content.")
                (assert-true
                 (contains-substring-p
                  "changed on disk after last read"
                  (princ-to-string condition))
                 "Expected stale-file edit failure to include stale-message."))))
          (assert-true saw-stale-edit-gate
                       "Expected stale-content edit to fail before applying replacement."))

        (let ((forced-edit-result (invoke-tool "edit-file"
                                              "path" (namestring conflict-target)
                                              "old-string" "two"
                                              "new-string" "TWO"
                                              "force" t)))
          (assert-true (eq (getf forced-edit-result :conflict-detected) t)
                       "Expected edit-file force to proceed with stale-content warning.")
          (assert-true
           (let ((warnings (getf forced-edit-result :warnings)))
             (and (listp warnings)
                  (some (lambda (warning)
                          (and (stringp warning)
                               (contains-substring-p
                                "changed on disk since the last read"
                                warning)))
                        warnings)))
           "Expected forced edit result to include a stale-content warning message."))

        (invoke-tool "write-file"
                     "path" (namestring stale-write-target)
                     "content" (format nil "one~%two~%"))
        (invoke-tool "read-file"
                     "path" (namestring stale-write-target))
        (with-open-file (stream stale-write-target
                                :direction :output
                                :if-exists :supersede
                                :if-does-not-exist :create
                                :external-format :utf-8)
          (write-line "one" stream)
          (write-line "changed" stream))
        (let ((saw-stale-write-gate nil))
          (handler-case
              (invoke-tool "write-file"
                           "path" (namestring stale-write-target)
                           "content" (format nil "one~%two~%three~%"))
            (condition (condition)
              (when (typep condition tool-argument-error-sym)
                (setf saw-stale-write-gate t)
                (assert-true (eq (funcall tool-error-reason-code-fn condition)
                                 :stale-content)
                            "Expected stale-content write failure to report stale-content."))))
          (assert-true saw-stale-write-gate
                       "Expected stale-content write to fail before overwrite."))

        (let ((forced-write-result (invoke-tool "write-file"
                                                "path" (namestring stale-write-target)
                                                "content" (format nil "one~%two~%three~%")
                                                "force" t)))
          (assert-true (eq (getf forced-write-result :conflict-detected) t)
                       "Expected write-file force to proceed with stale-content warning."))

        (let ((original-validator-runner (symbol-value syntax-validator-runner-sym))
              (captured-command nil))
          (unwind-protect
              (progn
                (setf (symbol-value syntax-validator-runner-sym)
                      (lambda (command)
                        (setf captured-command command)
                        (values "syntax ok" "" 0)))
                (funcall setconfig-fn
                         :edit-file-syntax-validators
                         '(("txt" . ("validator" "--check" "{path}"))))
                (invoke-tool "write-file"
                             "path" (namestring syntax-target)
                             "content" (format nil "alpha~%beta~%"))
                (invoke-tool "read-file"
                             "path" (namestring syntax-target))
                (let* ((syntax-result (invoke-tool "edit-file"
                                                   "path" (namestring syntax-target)
                                                   "old-string" "beta"
                                                   "new-string" "BETA"))
                       (validation (getf syntax-result :syntax-validation)))
                  (assert-true (equal captured-command
                                      (list "validator"
                                            "--check"
                                            (namestring syntax-target)))
                               "Expected syntax validator runner to receive configured command.")
                  (assert-true validation
                               "Expected edit-file to return syntax-validation payload when configured.")
                  (assert-true (eq (getf validation :ok) t)
                               "Expected syntax validation payload to report success.")))
            (setf (symbol-value syntax-validator-runner-sym) original-validator-runner)
            (funcall setconfig-fn :edit-file-syntax-validators nil)))

        (let ((original-pdf-extractor (symbol-value pdf-text-extractor-sym))
              (captured-pages nil))
          (unwind-protect
              (progn
                (setf (symbol-value pdf-text-extractor-sym)
                      (lambda (path pages)
                        (declare (ignore path))
                        (setf captured-pages pages)
                        "pdf page 1 text"))
                (let ((pdf-result (invoke-tool "read-file"
                                               "path" (namestring pdf-source)
                                               "pages" "1-2")))
                  (assert-true (string= captured-pages "1-2")
                               "Expected read-file pages argument to reach PDF extractor.")
                  (assert-true (contains-substring-p "pdf page 1 text" pdf-result)
                               "Expected read-file PDF path to return extractor output.")))
            (setf (symbol-value pdf-text-extractor-sym) original-pdf-extractor)))

        (let ((image-result (invoke-tool "read-file"
                                         "path" (namestring image-source))))
          (assert-true (eq (getf image-result :kind) :image)
                       "Expected image read result kind to be :image, got ~S."
                       image-result)
          (assert-true (string= (getf image-result :mime-type) "image/png")
                       "Expected PNG image mime type annotation.")
          (assert-true (string= (getf image-result :encoding) "base64")
                       "Expected image read result encoding to be base64.")
          (assert-true (string= (getf image-result :data) "AAECAwQ=")
                       "Expected deterministic base64 payload for sample PNG bytes."))

        (let ((notebook-result (invoke-tool "read-file"
                                            "path" (namestring notebook-source))))
          (assert-true (contains-substring-p "## Cell 1 (markdown)" notebook-result)
                       "Expected notebook render to include markdown cell heading.")
          (assert-true (contains-substring-p "```python" notebook-result)
                       "Expected notebook code cells to render fenced code blocks.")
          (assert-true (contains-substring-p "hello from notebook" notebook-result)
                       "Expected notebook render to include cell outputs."))

        (let ((csv-result (invoke-tool "read-file"
                                       "path" (namestring csv-source)
                                       "limit" 3))
              (tsv-result (invoke-tool "read-file"
                                       "path" (namestring tsv-source)
                                       "limit" 3)))
          (assert-true (contains-substring-p "name  | score" csv-result)
                       "Expected CSV preview header with aligned columns.")
          (assert-true (contains-substring-p "-+-" csv-result)
                       "Expected CSV preview separator row.")
          (assert-true (contains-substring-p "lang   | files" tsv-result)
                       "Expected TSV preview to use aligned table rendering."))

        (funcall clear-permission-rules-fn)
        (funcall add-permission-rule-fn
                 :effect :deny
                 :tool :write-file
                 :path (namestring deny-target)
                 :source :project)
        (let ((saw-deny nil))
          (handler-case
              (invoke-tool "write-file"
                           "path" (namestring deny-target)
                           "content" "blocked")
            (error ()
              (setf saw-deny t)))
          (assert-true saw-deny
                       "Expected path deny rule to block write-file execution.")))))

  (format t "AMOEBUM_EDIT_SAFETY_SMOKE_OK~%")
(format t "AMOEBUM_FILE_TOOLS_SMOKE_OK~%"))
