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
         (defskill-sym (funcall symbol-in "DEFSKILL" amoebum-pkg))
         (dispatch-fn (funcall fn-in "DISPATCH-SLASH-COMMAND" amoebum-pkg))
         (list-skills-fn (funcall fn-in "LIST-SKILLS" amoebum-pkg))
         (describe-skill-fn (funcall fn-in "DESCRIBE-SKILL" amoebum-pkg))
         (skill-name-fn (funcall fn-in "SKILL-METADATA-NAME" amoebum-pkg))
         (skill-category-fn (funcall fn-in "SKILL-METADATA-CATEGORY" amoebum-pkg))
         (skill-metadata-p-fn (funcall fn-in "SKILL-METADATA-P" amoebum-pkg))
         (result-output-fn (funcall fn-in "SLASH-COMMAND-RESULT-OUTPUT" amoebum-pkg))
         (result-action-fn (funcall fn-in "SLASH-COMMAND-RESULT-ACTION" amoebum-pkg))
         (result-payload-fn (funcall fn-in "SLASH-COMMAND-RESULT-PAYLOAD" amoebum-pkg))
         (setconfig-fn (funcall fn-in "SETCONFIG" amoebum-pkg))
         (current-config-fn (funcall fn-in "CURRENT-CONFIG" amoebum-pkg))
         (config-model-fn (funcall fn-in "CONFIG-MODEL" amoebum-pkg))
         (config-mode-fn (funcall fn-in "CONFIG-PERMISSION-MODE" amoebum-pkg))
         (config-root-fn (funcall fn-in "CONFIG-PROJECT-ROOT" amoebum-pkg))
         (temporary-directory-fn (funcall fn-in "TEMPORARY-DIRECTORY" uiop-pkg))
         (ensure-directory-pathname-fn (funcall fn-in "ENSURE-DIRECTORY-PATHNAME" uiop-pkg))
         (run-program-fn (funcall fn-in "RUN-PROGRAM" uiop-pkg))
         (git-generator-sym
           (funcall symbol-in "*GIT-COMMIT-MESSAGE-GENERATOR*" amoebum-pkg))
         (review-generator-sym
           (funcall symbol-in "*SKILL-REVIEW-ANALYZER*" amoebum-pkg)))
    (labels ((assert-true (condition format-string &rest format-args)
               (unless condition
                 (error (apply #'format nil format-string format-args))))
             (contains-symbol-p (tree target)
               (cond
                 ((eq tree target) t)
                 ((consp tree)
                  (or (contains-symbol-p (car tree) target)
                      (contains-symbol-p (cdr tree) target)))
                 (t nil)))
             (contains-substring-p (needle haystack)
               (and (stringp haystack)
                    (search needle haystack :test #'char-equal)))
             (run-program-lines (&rest command)
               (multiple-value-bind (stdout stderr exit-code)
                   (funcall run-program-fn
                            command
                            :ignore-error-status t
                            :output :string
                            :error-output :string)
                 (values (or stdout "") (or stderr "") (or exit-code 0))))
             (run-git (repo-path &rest args)
               (multiple-value-bind (stdout stderr exit-code)
                   (apply #'run-program-lines (append (list "git" "-C" repo-path) args))
                 (unless (zerop exit-code)
                   (error "git ~{~A~^ ~} failed: ~A~%~A" args stdout stderr))
                 stdout))
             (write-text-file (path content)
               (ensure-directories-exist path)
               (with-open-file (stream path
                                       :direction :output
                                       :if-exists :supersede
                                       :if-does-not-exist :create
                                       :external-format :utf-8)
                 (write-string content stream)))
             (dispatch-command (line)
               (multiple-value-bind (handledp result)
                   (funcall dispatch-fn line)
                 (values handledp result
                         (and result (funcall result-output-fn result)))))
             (skill-name-text (metadata)
               (string-downcase (princ-to-string (funcall skill-name-fn metadata)))))
      (let* ((expanded
               (macroexpand-1
                `(,defskill-sym i68-expand-skill ((count :integer :required t :prompt "Count:"))
                   "Expansion smoke."
                   (:category :smoke)
                   (format nil "count=~D" count)))))
        (assert-true (and (consp expanded) (eq (car expanded) 'progn))
                     "Expected DEFSKILL expansion to be a PROGN.")
        (assert-true (contains-symbol-p expanded 'defun)
                     "Expected DEFSKILL expansion to include DEFUN.")
        (assert-true (contains-symbol-p expanded
                                        (funcall symbol-in "REGISTER-SKILL" amoebum-pkg))
                     "Expected DEFSKILL expansion to include REGISTER-SKILL.")
        (assert-true (contains-symbol-p expanded
                                        (funcall symbol-in "MAKE-SKILL-METADATA" amoebum-pkg))
                     "Expected DEFSKILL expansion to include MAKE-SKILL-METADATA."))

      (eval
       `(,defskill-sym i68-smoke-skill ((count :integer
                                               :required t
                                               :prompt "Provide count."
                                               :choices '(1 2 3)))
          "I68 custom smoke skill."
          (:category :smoke)
          (format nil "count=~D" count)))

      (multiple-value-bind (handledp _result output)
          (dispatch-command "/i68-smoke-skill")
        (declare (ignore _result))
        (assert-true handledp "Expected /i68-smoke-skill to be handled.")
        (assert-true (contains-substring-p "Missing required argument count" output)
                     "Expected missing-arg prompt output, got ~S." output))

      (multiple-value-bind (handledp _result output)
          (dispatch-command "/i68-smoke-skill 2")
        (declare (ignore _result))
        (assert-true handledp "Expected /i68-smoke-skill 2 to be handled.")
        (assert-true (contains-substring-p "count=2" output)
                     "Expected custom skill output count=2, got ~S." output))

      (let* ((skills (funcall list-skills-fn))
             (names (mapcar #'skill-name-text skills)))
        (dolist (required '("commit" "review" "compact" "status" "i68-smoke-skill"))
          (assert-true (member required names :test #'string=)
                       "Expected skill ~A in list-skills names=~S." required names)))

      (let ((commit-skill (funcall describe-skill-fn 'commit)))
        (assert-true (funcall skill-metadata-p-fn commit-skill)
                     "Expected describe-skill to return skill-metadata for commit.")
        (assert-true (eq (funcall skill-category-fn commit-skill) :git)
                     "Expected commit skill category :git."))

      (multiple-value-bind (handledp result _output)
          (dispatch-command "/compact 9")
        (declare (ignore _output))
        (assert-true handledp "Expected /compact to be handled.")
        (assert-true (eq (funcall result-action-fn result) :compact-chat)
                     "Expected /compact action :compact-chat, got ~S."
                     (funcall result-action-fn result))
        (assert-true (= (funcall result-payload-fn result) 9)
                     "Expected /compact payload 9, got ~S."
                     (funcall result-payload-fn result)))

      (let* ((old-config (funcall current-config-fn))
             (old-root (funcall config-root-fn old-config))
             (old-mode (funcall config-mode-fn old-config))
             (old-model (funcall config-model-fn old-config))
             (old-git-generator (symbol-value git-generator-sym))
             (old-review-generator (symbol-value review-generator-sym))
             (tmp-root
               (funcall ensure-directory-pathname-fn
                        (merge-pathnames
                         (make-pathname :directory
                                        `(:relative ,(format nil "amoebum-i68-~A"
                                                             (get-universal-time))))
                         (funcall temporary-directory-fn))))
             (repo-path (namestring tmp-root)))
        (unwind-protect
            (progn
              (ensure-directories-exist (merge-pathnames #P".keep" tmp-root))
              (run-git repo-path "init")
              (run-git repo-path "config" "user.name" "Amoebum Smoke")
              (run-git repo-path "config" "user.email" "amoebum-smoke@example.com")

              (write-text-file (merge-pathnames #P"README.md" tmp-root) "seed\n")
              (run-git repo-path "add" "--" "README.md")
              (run-git repo-path "commit" "-m" "chore: seed repo")
              (run-git repo-path "branch" "-m" "main")

              (funcall setconfig-fn :project-root tmp-root)
              (funcall setconfig-fn :permission-mode :yolo)
              (funcall setconfig-fn :model "i68-smoke-model")

              (write-text-file (merge-pathnames #P"commit.txt" tmp-root) "commit skill file\n")
              (run-git repo-path "add" "--" "commit.txt")
              (setf (symbol-value git-generator-sym)
                    (lambda (diff recent-subjects
                             &key model staged-paths project-root)
                      (declare (ignore diff recent-subjects model staged-paths project-root))
                      "feat: i68 commit smoke message"))

              (multiple-value-bind (handledp _result output)
                  (dispatch-command "/commit")
                (declare (ignore _result))
                (assert-true handledp "Expected /commit skill to be handled.")
                (assert-true (contains-substring-p "feat: i68 commit smoke message" output)
                             "Expected /commit output to include generated summary, got ~S."
                             output))
              (let ((subject (string-trim '(#\Space #\Tab #\Newline #\Return)
                                          (run-git repo-path "log" "-1" "--pretty=%s"))))
                (assert-true (string= subject "feat: i68 commit smoke message")
                             "Expected /commit to create generated commit message, got ~S."
                             subject))

              (run-git repo-path "checkout" "-b" "feature/i68-smoke-review")
              (write-text-file (merge-pathnames #P"review.txt" tmp-root)
                               "review diff content\n")
              (run-git repo-path "add" "--" "review.txt")
              (run-git repo-path "commit" "-m" "feat: add review fixture")

              (setf (symbol-value review-generator-sym)
                    (lambda (diff-data &key model)
                      (declare (ignore model))
                      (let ((summary (or (gethash "summary" diff-data) "")))
                        (format nil "I68 review sentinel (~A)" summary))))

              (multiple-value-bind (handledp _result output)
                  (dispatch-command "/review")
                (declare (ignore _result))
                (assert-true handledp "Expected /review skill to be handled.")
                (assert-true (contains-substring-p "I68 review sentinel" output)
                             "Expected /review output sentinel, got ~S."
                             output))

              (multiple-value-bind (handledp _result output)
                  (dispatch-command "/status")
                (declare (ignore _result))
                (assert-true handledp "Expected /status skill to be handled.")
                (assert-true (contains-substring-p "branch:" output)
                             "Expected /status output branch line, got ~S."
                             output)
                (assert-true (contains-substring-p "mode:" output)
                             "Expected /status output mode line, got ~S."
                             output)
                (assert-true (contains-substring-p "tokens:" output)
                             "Expected /status output tokens line, got ~S."
                             output))

              ;; NXT-024: Verify %skill-invoke-tool routes through execute-tool
              ;; CLOS pipeline by confirming that :supervised mode blocks tool
              ;; invocation (the :before method enforces permissions).
              (funcall setconfig-fn :permission-mode :supervised)
              (let* ((skill-invoke-fn
                       (funcall fn-in "%SKILL-INVOKE-TOOL" amoebum-pkg))
                     (permission-denied-sym
                       (funcall symbol-in "TOOL-PERMISSION-DENIED" amoebum-pkg))
                     (denied-p nil))
                (handler-case
                    (funcall skill-invoke-fn "git-status")
                  (condition (c)
                    (when (typep c permission-denied-sym)
                      (setf denied-p t))))
                (assert-true denied-p
                             "Expected %skill-invoke-tool to raise TOOL-PERMISSION-DENIED ~
                              in :supervised mode, proving execute-tool CLOS routing."))
              (funcall setconfig-fn :permission-mode :yolo))
          (setf (symbol-value git-generator-sym) old-git-generator
                (symbol-value review-generator-sym) old-review-generator)
          (funcall setconfig-fn :project-root old-root)
          (funcall setconfig-fn :permission-mode old-mode)
          (funcall setconfig-fn :model old-model)))))

  (format t "AMOEBUM_DEFSKILL_SMOKE_OK~%"))
