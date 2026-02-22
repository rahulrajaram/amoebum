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
         (elements-pkg (or (find-package "PTUI.UI.ELEMENTS")
                           (error "Missing package PTUI.UI.ELEMENTS after load.")))
         (symbol-in
           (lambda (name package)
             (or (find-symbol name package)
                 (error "Missing symbol ~A in package ~A." name (package-name package)))))
         (fn-in
           (lambda (name package)
             (symbol-function (funcall symbol-in name package))))
         (make-fuzzy-picker-state-fn (funcall fn-in "MAKE-FUZZY-PICKER-STATE" amoebum-pkg))
         (ensure-fuzzy-picker-index-fn (funcall fn-in "ENSURE-FUZZY-PICKER-INDEX!" amoebum-pkg))
         (fuzzy-picker-sync-input-fn (funcall fn-in "FUZZY-PICKER-SYNC-INPUT!" amoebum-pkg))
         (fuzzy-picker-step-fn (funcall fn-in "FUZZY-PICKER-STEP!" amoebum-pkg))
         (fuzzy-picker-scan-complete-p-fn (funcall fn-in "FUZZY-PICKER-STATE-SCAN-COMPLETE-P" amoebum-pkg))
         (fuzzy-picker-top-results-fn (funcall fn-in "FUZZY-PICKER-STATE-TOP-RESULTS" amoebum-pkg))
         (fuzzy-picker-selected-index-fn (funcall fn-in "FUZZY-PICKER-STATE-SELECTED-INDEX" amoebum-pkg))
         (fuzzy-picker-move-selection-fn (funcall fn-in "FUZZY-PICKER-MOVE-SELECTION!" amoebum-pkg))
         (fuzzy-picker-home-selection-fn (funcall fn-in "FUZZY-PICKER-HOME-SELECTION!" amoebum-pkg))
         (fuzzy-picker-end-selection-fn (funcall fn-in "FUZZY-PICKER-END-SELECTION!" amoebum-pkg))
         (fuzzy-picker-selected-path-fn (funcall fn-in "FUZZY-PICKER-SELECTED-PATH" amoebum-pkg))
         (fuzzy-picker-apply-selection-fn (funcall fn-in "FUZZY-PICKER-APPLY-SELECTION" amoebum-pkg))
         (make-fuzzy-picker-widget-fn (funcall fn-in "MAKE-FUZZY-PICKER-WIDGET" amoebum-pkg))
         (fuzzy-match-path-fn (funcall fn-in "FUZZY-MATCH-PATH" amoebum-pkg))
         (ui-element-children-fn (funcall fn-in "UI-ELEMENT-CHILDREN" elements-pkg))
         (ui-element-props-fn (funcall fn-in "UI-ELEMENT-PROPS" elements-pkg)))
    (labels ((assert-true (condition format-string &rest format-args)
               (unless condition
                 (error (apply #'format nil format-string format-args))))
             (write-file (path contents)
               (ensure-directories-exist path)
               (with-open-file (stream path
                                       :direction :output
                                       :if-does-not-exist :create
                                       :if-exists :supersede
                                       :external-format :utf-8)
                 (write-string contents stream)))
             (match-paths (matches)
               (mapcar fuzzy-match-path-fn matches))
             (tree-has-highlight-p (element)
               (let* ((props (funcall ui-element-props-fn element))
                      (segments (and (listp props)
                                     (getf props :styled-segments))))
                 (or (and (listp segments)
                          (some (lambda (segment)
                                  (and (consp segment)
                                       (eq (cdr segment) :context-yellow)))
                                segments))
                     (some #'tree-has-highlight-p
                           (funcall ui-element-children-fn element)))))
             (run-to-complete (state)
               (loop repeat 1000
                     until (funcall fuzzy-picker-scan-complete-p-fn state)
                     do (funcall fuzzy-picker-step-fn state))
               (assert-true (funcall fuzzy-picker-scan-complete-p-fn state)
                            "Expected fuzzy scan to complete within the bounded loop.")))
      (let* ((root-dir (make-pathname
                        :directory (list :absolute
                                         "tmp"
                                         (format nil "amoebum-fuzzy-picker-smoke-~D"
                                                 (get-universal-time)))
                        :name nil
                        :type nil))
             (gitignore (merge-pathnames #P".gitignore" root-dir))
             (state (funcall make-fuzzy-picker-state-fn
                             :visible-count 6
                             :batch-size 1)))
        (ensure-directories-exist (merge-pathnames #P"placeholder" root-dir))
        (write-file gitignore (format nil "ignored/~%*.tmp~%"))
        (write-file (merge-pathnames #P"src/chat.lisp" root-dir) "(defun chat () :ok)")
        (sleep 1)
        (write-file (merge-pathnames #P"src/changelog.lisp" root-dir) "(defun changes () :ok)")
        (write-file (merge-pathnames #P"docs/guide.md" root-dir) "# guide")
        (write-file (merge-pathnames #P"ignored/secret.txt" root-dir) "ignored")
        (write-file (merge-pathnames #P"scratch.tmp" root-dir) "ignored tmp")

        (funcall ensure-fuzzy-picker-index-fn state :root root-dir)
        (funcall fuzzy-picker-sync-input-fn state "open @ch" :root root-dir)

        ;; Batch-size=1 should leave scan incomplete after first sync, proving incremental streaming.
        (assert-true (not (funcall fuzzy-picker-scan-complete-p-fn state))
                     "Expected fuzzy scan to remain in-progress after first incremental step.")
        (run-to-complete state)

        (let ((paths (match-paths (funcall fuzzy-picker-top-results-fn state))))
          (assert-true (member "src/chat.lisp" paths :test #'string=)
                       "Expected src/chat.lisp in top fuzzy matches, got ~S." paths)
          (assert-true (not (member "ignored/secret.txt" paths :test #'string=))
                       "Expected ignored/secret.txt to be filtered by .gitignore.")
          (assert-true (not (member "scratch.tmp" paths :test #'string=))
                       "Expected scratch.tmp to be filtered by .gitignore."))

        (let ((widget (funcall make-fuzzy-picker-widget-fn state)))
          (assert-true (tree-has-highlight-p widget)
                       "Expected fuzzy picker widget rows with highlighted match spans."))

        (let ((initial-index (funcall fuzzy-picker-selected-index-fn state)))
          (funcall fuzzy-picker-move-selection-fn state 1)
          (assert-true (>= (funcall fuzzy-picker-selected-index-fn state) initial-index)
                       "Expected move-selection to advance the active row.")
          (funcall fuzzy-picker-home-selection-fn state)
          (assert-true (= (funcall fuzzy-picker-selected-index-fn state) 0)
                       "Expected HOME to move selection to index 0.")
          (funcall fuzzy-picker-end-selection-fn state)
          (assert-true (>= (funcall fuzzy-picker-selected-index-fn state) 0)
                       "Expected END to move selection to the last visible row."))

        (funcall fuzzy-picker-sync-input-fn state "open @ch" :root root-dir)
        (run-to-complete state)
        (let* ((selected (funcall fuzzy-picker-selected-path-fn state))
               (result (funcall fuzzy-picker-apply-selection-fn "open @ch" state selected)))
          (assert-true (and (stringp selected)
                            (plusp (length selected)))
                       "Expected selected fuzzy path to be non-empty.")
          (assert-true (search (concatenate 'string "@" selected) result :test #'char=)
                       "Expected selection replacement in input, got ~S." result))

        ;; Glob queries in @ mention flows should use glob candidate resolution directly.
        (funcall fuzzy-picker-sync-input-fn state "open @src/*.lisp" :root root-dir)
        (funcall fuzzy-picker-step-fn state :batch-size 1)
        (assert-true (funcall fuzzy-picker-scan-complete-p-fn state)
                     "Expected glob-based @ file discovery to resolve in a single step.")
        (let ((paths (match-paths (funcall fuzzy-picker-top-results-fn state))))
          (assert-true (equal paths '("src/changelog.lisp" "src/chat.lisp"))
                       "Expected glob @ discovery candidates in mtime order, got ~S."
                       paths)
          (assert-true (not (member "docs/guide.md" paths :test #'string=))
                       "Expected glob @ discovery to exclude non-matching files.")))))

  (format t "AMOEBUM_FUZZY_PICKER_SMOKE_OK~%"))
