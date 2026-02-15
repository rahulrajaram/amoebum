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
         (fn
           (lambda (name)
             (symbol-function (funcall symbol-in name amoebum-pkg))))
         (ensure-directory-pathname-fn
           (symbol-function (funcall symbol-in "ENSURE-DIRECTORY-PATHNAME" uiop-pkg)))
         (temporary-directory-fn
           (symbol-function (funcall symbol-in "TEMPORARY-DIRECTORY" uiop-pkg)))
         (find-theme-fn (funcall fn "FIND-SOUND-THEME"))
         (active-theme-name-fn (funcall fn "ACTIVE-SOUND-THEME-NAME"))
         (set-active-theme-fn (funcall fn "SET-ACTIVE-SOUND-THEME"))
         (resolve-sound-fn (funcall fn "RESOLVE-SOUND"))
         (resolve-active-path-fn (funcall fn "RESOLVE-ACTIVE-SOUND-PATH"))
         (dispatch-fn (funcall fn "DISPATCH-SLASH-COMMAND"))
         (result-output-fn (funcall fn "SLASH-COMMAND-RESULT-OUTPUT"))
         (setconfig-fn (funcall fn "SETCONFIG"))
         (load-config-fn (funcall fn "LOAD-CONFIG"))
         (defsoundtheme-sym (funcall symbol-in "DEFSOUNDTHEME" amoebum-pkg))
         (sound-theme-p-fn (symbol-function (funcall symbol-in "SOUND-THEME-P" amoebum-pkg)))
         (command-prober-sym (funcall symbol-in "*NOTIFICATION-COMMAND-PROBER*" amoebum-pkg))
         (command-runner-sym (funcall symbol-in "*NOTIFICATION-COMMAND-RUNNER*" amoebum-pkg))
         (active-theme-sym (funcall symbol-in "*ACTIVE-SOUND-THEME*" amoebum-pkg))
         (tmp-root
           (funcall ensure-directory-pathname-fn
                    (merge-pathnames
                     (make-pathname :directory
                                    `(:relative ,(format nil "amoebum-i80-~A" (get-universal-time))))
                     (funcall temporary-directory-fn))))
         (project-root (merge-pathnames #P"project/" tmp-root))
         (error-sound (merge-pathnames #P"sounds/error.wav" tmp-root))
         (complete-sound (merge-pathnames #P"sounds/complete.wav" tmp-root))
         (approval-sound (merge-pathnames #P"sounds/approval.wav" tmp-root))
         (original-prober (symbol-value command-prober-sym))
         (original-runner (symbol-value command-runner-sym))
         (original-active-theme (symbol-value active-theme-sym)))
    (labels ((assert-true (condition format-string &rest format-args)
               (unless condition
                 (error (apply #'format nil format-string format-args))))
             (contains-text-p (haystack needle)
               (and (stringp haystack)
                    (search needle haystack :test #'char-equal))))
      (unwind-protect
          (progn
            (ensure-directories-exist error-sound)
            (with-open-file (stream error-sound
                                    :direction :output
                                    :if-does-not-exist :create
                                    :if-exists :supersede)
              (write-line "RIFF" stream))
            (with-open-file (stream complete-sound
                                    :direction :output
                                    :if-does-not-exist :create
                                    :if-exists :supersede)
              (write-line "RIFF" stream))
            (with-open-file (stream approval-sound
                                    :direction :output
                                    :if-does-not-exist :create
                                    :if-exists :supersede)
              (write-line "RIFF" stream))

            (funcall load-config-fn :project-root project-root)
            (funcall setconfig-fn :notification-sound-enabled t)
            (funcall setconfig-fn :notification-sound-player "fake-player")
            (funcall setconfig-fn :notification-sound-error error-sound)
            (funcall setconfig-fn :notification-sound-task-complete complete-sound)
            (funcall setconfig-fn :notification-sound-approval-needed approval-sound)

            (let ((preview-invocations '()))
              (setf (symbol-value command-prober-sym)
                    (lambda (command)
                      (string= command "fake-player")))
              (setf (symbol-value command-runner-sym)
                    (lambda (arguments)
                      (push arguments preview-invocations)
                      (list :exit-code 0 :stdout "" :stderr "")))

              (eval `(,defsoundtheme-sym i80-parent ()
                       (:error "parent-error.wav")))
              (eval `(,defsoundtheme-sym i80-child (:inherits i80-parent)
                       (:task-complete "child-complete.wav")))

              (let ((child-theme (funcall find-theme-fn "i80-child")))
                (assert-true (funcall sound-theme-p-fn child-theme)
                             "Expected defsoundtheme to register SOUND-THEME for i80-child, got ~S."
                             child-theme)
                (assert-true (string= (funcall resolve-sound-fn "i80-child" :task-complete)
                                      "child-complete.wav")
                             "Expected child override sound to resolve for :task-complete.")
                (assert-true (string= (funcall resolve-sound-fn "i80-child" :error)
                                      "parent-error.wav")
                             "Expected inherited parent sound to resolve for :error.")
                (assert-true (null (funcall resolve-sound-fn "i80-child" :approval-needed))
                             "Expected unmapped categories to resolve to silence (NIL)."))

              (multiple-value-bind (handledp list-result)
                  (funcall dispatch-fn "/sounds")
                (assert-true handledp "Expected /sounds to be handled.")
                (assert-true (contains-text-p (funcall result-output-fn list-result) "standard")
                             "Expected /sounds list output to include built-in standard theme."))

              (multiple-value-bind (handledp set-result)
                  (funcall dispatch-fn "/sounds set minimal")
                (assert-true handledp "Expected /sounds set minimal to be handled.")
                (assert-true (contains-text-p (funcall result-output-fn set-result) "minimal")
                             "Expected /sounds set output to confirm minimal theme.")
                (assert-true (eq (funcall active-theme-name-fn) :minimal)
                             "Expected active theme to be :MINIMAL after set command."))

              (multiple-value-bind (handledp preview-result)
                  (funcall dispatch-fn "/sounds preview error")
                (assert-true handledp "Expected /sounds preview error to be handled.")
                (assert-true (and (stringp (funcall result-output-fn preview-result))
                                  (plusp (length (funcall result-output-fn preview-result))))
                             "Expected /sounds preview to return non-empty output.")
                (assert-true (= (length preview-invocations) 1)
                             "Expected one preview invocation, got ~S." preview-invocations)
                (assert-true (string= (first (first preview-invocations)) "fake-player")
                             "Expected preview to run fake-player command, got ~S."
                             preview-invocations))

              (funcall set-active-theme-fn :standard)
              (assert-true (equal (funcall resolve-active-path-fn :error)
                                  error-sound)
                           "Expected active standard theme to resolve configured error sound path."))))
        (setf (symbol-value command-prober-sym) original-prober
              (symbol-value command-runner-sym) original-runner
              (symbol-value active-theme-sym) original-active-theme))))

  (format t "AMOEBUM_SOUNDS_SMOKE_OK~%")
