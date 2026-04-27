(let* ((smoke-file (or *load-truename* *compile-file-truename*))
       (amoebum-dir (and smoke-file (make-pathname :name nil :type nil :defaults smoke-file)))
       (repo-root (and amoebum-dir (truename (merge-pathnames #P"../" amoebum-dir))))
       (quicklisp-path
         (or (let ((env-path #+sbcl (sb-ext:posix-getenv "QUICKLISP_SETUP")
                             #-sbcl nil))
               (and env-path
                    (probe-file env-path)))
             (probe-file (merge-pathnames #P"ptui/.tools/quicklisp/setup.lisp" repo-root)))))
  (unless repo-root
    (error "Unable to resolve repository root from ~S" smoke-file))
  (unless quicklisp-path
    (error "Unable to resolve Quicklisp setup from QUICKLISP_SETUP or repo-local fallback."))

  (load quicklisp-path)
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
         (temporary-directory-fn (funcall fn-in "TEMPORARY-DIRECTORY" uiop-pkg))
         (ensure-directory-pathname-fn (funcall fn-in "ENSURE-DIRECTORY-PATHNAME" uiop-pkg))
         (run-program-fn (funcall fn-in "RUN-PROGRAM" uiop-pkg))
         (find-tool-fn (funcall fn-in "FIND-TOOL" pseudopod-pkg))
         (tool-definition-fn-fn (funcall fn-in "TOOL-DEFINITION-FN" pseudopod-pkg))
         (toolset-sym (funcall symbol-in "*TOOLSET*" amoebum-pkg))
         (setconfig-fn (funcall fn-in "SETCONFIG" amoebum-pkg))
         (current-config-fn (funcall fn-in "CURRENT-CONFIG" amoebum-pkg))
	         (config-value-fn (funcall fn-in "CONFIG-VALUE" amoebum-pkg))
	         (config-project-root-fn (funcall fn-in "CONFIG-PROJECT-ROOT" amoebum-pkg))
	         (git-generator-sym
	           (funcall symbol-in "*GIT-COMMIT-MESSAGE-GENERATOR*" amoebum-pkg))
	         (git-pr-generator-sym
	           (funcall symbol-in "*GIT-PR-DESCRIPTION-GENERATOR*" amoebum-pkg))
	         (git-pr-runner-sym
	           (funcall symbol-in "*GIT-PR-COMMAND-RUNNER*" amoebum-pkg)))
    (labels ((assert-true (condition format-string &rest format-args)
               (unless condition
                 (error (apply #'format nil format-string format-args))))
             (contains-substring-p (needle haystack)
               (and (stringp haystack)
                    (search needle haystack :test #'char-equal)))
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
                          (apply #'make-args key-values))))
             (write-text-file (path content)
               (ensure-directories-exist path)
               (with-open-file (stream path
                                       :direction :output
                                       :if-exists :supersede
                                       :if-does-not-exist :create
                                       :external-format :utf-8)
                 (write-string content stream)))
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
                   (error "git ~{~A~^ ~} failed: ~A~%~A"
                          args stdout stderr))
                 stdout))
             (git-contains-path-p (paths expected)
               (member expected paths :test #'string=)))
	      (let* ((old-config (funcall current-config-fn))
	             (old-project-root (funcall config-project-root-fn old-config))
	             (old-permission-mode
	               (funcall config-value-fn :permission-mode old-config))
	             (old-generator (symbol-value git-generator-sym))
	             (old-pr-generator (symbol-value git-pr-generator-sym))
	             (old-pr-runner (symbol-value git-pr-runner-sym))
	             (tmp-root
	               (funcall ensure-directory-pathname-fn
	                        (merge-pathnames
	                         (make-pathname :directory `(:relative ,(format nil "amoebum-i47-~A"
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
              (funcall setconfig-fn :permission-mode :full-auto)

              (write-text-file (merge-pathnames #P"staged.txt" tmp-root)
                               "staged content\n")
              (write-text-file (merge-pathnames #P"unstaged.txt" tmp-root)
                               "unstaged content\n")
              (run-git repo-path "add" "--" "staged.txt")

              (let* ((status (invoke-tool "git-status"))
                     (tracking (getf status :tracking))
                     (staged (getf status :staged))
                     (unstaged (getf status :unstaged)))
                (assert-true (stringp (getf status :branch))
                             "Expected git-status to include branch string.")
                (assert-true (and (listp tracking)
                                  (integerp (getf tracking :ahead))
                                  (integerp (getf tracking :behind)))
                             "Expected git-status tracking structure, got ~S." tracking)
                (assert-true (git-contains-path-p staged "staged.txt")
                             "Expected staged file in git-status output, got ~S." staged)
                (assert-true (git-contains-path-p unstaged "unstaged.txt")
                             "Expected unstaged file in git-status output, got ~S." unstaged))

              (let ((saw-sensitive-error nil))
                (write-text-file (merge-pathnames #P".env" tmp-root) "SECRET_TOKEN=x\n")
                (handler-case
                    (invoke-tool "git-commit" "files" '(".env"))
                  (error ()
                    (setf saw-sensitive-error t)))
                (assert-true saw-sensitive-error
                             "Expected git-commit to reject sensitive .env path."))

              (let ((saw-amend-error nil))
                (handler-case
                    (invoke-tool "git-commit" "amend" t)
                  (error ()
                    (setf saw-amend-error t)))
                (assert-true saw-amend-error
                             "Expected git-commit to block amend without allow-amend."))

              (setf (symbol-value git-generator-sym)
                    (lambda (diff recent-subjects
                             &key model staged-paths project-root)
                      (declare (ignore diff recent-subjects model staged-paths project-root))
                      "feat: add smoke-tested git commit flow"))

              (let* ((result (invoke-tool "git-commit"
                                          "files" '("staged.txt")
                                          "co-author" "Amoebum Bot <amoebum@example.com>"))
                     (sha (getf result :sha))
                     (source (getf result :message-source))
                     (summary (getf result :message-summary))
                     (commit-body
                       (run-git repo-path "log" "-1" "--pretty=%B")))
                (assert-true (and (stringp sha) (> (length sha) 6))
                             "Expected git-commit result to include commit sha, got ~S." sha)
                (assert-true (eq source :llm)
                             "Expected git-commit message source to be :llm, got ~S."
                             source)
                (assert-true (string= summary "feat: add smoke-tested git commit flow")
                             "Unexpected commit summary: ~S." summary)
                (assert-true (contains-substring-p "Co-Authored-By: Amoebum Bot <amoebum@example.com>"
                                                   commit-body)
                             "Expected commit body to include Co-Authored-By line, got ~S."
                             commit-body))

	              (multiple-value-bind (_stdout _stderr init-exit-code)
	                  (run-program-lines "git"
	                                     "init"
	                                     "--bare"
	                                     (namestring (merge-pathnames #P"origin.git" tmp-root)))
	                (declare (ignore _stdout _stderr))
	                (assert-true (zerop init-exit-code)
	                             "Expected to initialize bare origin remote, exit=~S."
	                             init-exit-code))
	              (run-git repo-path "remote" "add" "origin"
	                       (namestring (merge-pathnames #P"origin.git" tmp-root)))
	
	              (run-git repo-path "checkout" "-b" "feature/i47-smoke")
	              (write-text-file (merge-pathnames #P"feature.txt" tmp-root)
	                               "feature branch diff content\n")
	              (run-git repo-path "add" "--" "feature.txt")
	              (run-git repo-path "commit" "-m" "feat: add branch diff fixture")
	              (write-text-file (merge-pathnames #P"feature-extra.txt" tmp-root)
	                               "second feature commit\n")
	              (run-git repo-path "add" "--" "feature-extra.txt")
	              (run-git repo-path "commit" "-m" "fix: add second branch fixture")

	              (let* ((branch-diff (invoke-tool "git-diff-branch"))
	                     (branch (getf branch-diff :branch))
	                     (base-branch (getf branch-diff :base-branch))
                     (ahead-commits (getf branch-diff :ahead-commits))
                     (files-changed (getf branch-diff :files-changed))
                     (merge-base (getf branch-diff :merge-base))
                     (summary (getf branch-diff :summary))
                     (diff (getf branch-diff :diff)))
	                (assert-true (string= branch "feature/i47-smoke")
	                             "Expected git-diff-branch to report current feature branch, got ~S."
	                             branch)
                (assert-true (string= base-branch "main")
                             "Expected git-diff-branch to resolve base branch main, got ~S."
                             base-branch)
                (assert-true (integerp ahead-commits)
                             "Expected git-diff-branch to return integer ahead-commits, got ~S."
                             ahead-commits)
                (assert-true (>= ahead-commits 1)
                             "Expected feature branch to be at least one commit ahead, got ~S."
                             ahead-commits)
                (assert-true (git-contains-path-p files-changed "feature.txt")
                             "Expected branch diff to include feature.txt, got ~S."
                             files-changed)
                (assert-true (and (stringp merge-base) (> (length merge-base) 6))
                             "Expected merge-base hash from git-diff-branch, got ~S."
                             merge-base)
	                (assert-true (contains-substring-p "changed" summary)
	                             "Expected git-diff-branch summary shortstat, got ~S."
	                             summary)
	                (assert-true (contains-substring-p "feature branch diff content" diff)
	                             "Expected git-diff-branch diff text to include feature content, got ~S."
	                             diff))

	              (let ((captured-pr-command nil)
	                    (captured-subjects nil))
	                (setf (symbol-value git-pr-generator-sym)
	                      (lambda (commits &key branch base-branch range merge-base model project-root)
	                        (declare (ignore range merge-base model project-root))
	                        (setf captured-subjects
	                              (mapcar (lambda (commit) (getf commit :subject))
	                                      commits))
	                        (assert-true (string= branch "feature/i47-smoke")
	                                     "Expected PR generator branch to be feature/i47-smoke, got ~S."
	                                     branch)
	                        (assert-true (string= base-branch "main")
	                                     "Expected PR generator base branch main, got ~S."
	                                     base-branch)
	                        (list :title "feat: create smoke-test pull request"
	                              :body "## Summary
- add branch diff fixture
- add second branch fixture

## Test Plan
- [x] sbcl --script amoebum/git-smoke-test.lisp")))
	                (setf (symbol-value git-pr-runner-sym)
	                      (lambda (root command)
	                        (declare (ignore root))
	                        (setf captured-pr-command command)
	                        (list :stdout "https://github.com/example/amoebum/pull/47\n"
	                              :stderr ""
	                              :exit-code 0)))
	                (let* ((pr-result (invoke-tool "create-pr"))
	                       (url (getf pr-result :url))
	                       (commit-count (getf pr-result :commit-count))
	                       (auto-pushed-p (getf pr-result :auto-pushed-p))
	                       (description-source (getf pr-result :description-source))
	                       (upstream-output
	                         (run-git repo-path
	                                  "rev-parse"
	                                  "--abbrev-ref"
	                                  "--symbolic-full-name"
	                                  "@{u}")))
	                  (assert-true (string= url "https://github.com/example/amoebum/pull/47")
	                               "Expected create-pr to return PR URL, got ~S." url)
	                  (assert-true (= commit-count 2)
	                               "Expected create-pr to analyze both feature commits, got ~S."
	                               commit-count)
	                  (assert-true auto-pushed-p
	                               "Expected create-pr to push branch with -u when upstream missing.")
	                  (assert-true (eq description-source :llm)
	                               "Expected create-pr description source to be :llm, got ~S."
	                               description-source)
	                  (assert-true (= (length captured-subjects) 2)
	                               "Expected PR generator to receive two commits, got ~S."
	                               captured-subjects)
	                  (assert-true (member "feat: add branch diff fixture" captured-subjects
	                                       :test #'string=)
	                               "Expected first feature commit in PR generator input, got ~S."
	                               captured-subjects)
	                  (assert-true (member "fix: add second branch fixture" captured-subjects
	                                       :test #'string=)
	                               "Expected second feature commit in PR generator input, got ~S."
	                               captured-subjects)
	                  (assert-true (contains-substring-p "gh pr create" captured-pr-command)
	                               "Expected create-pr to invoke gh pr create, command=~S."
	                               captured-pr-command)
	                  (assert-true (contains-substring-p "--body-file - <<'" captured-pr-command)
	                               "Expected create-pr command to use HEREDOC body formatting, command=~S."
	                               captured-pr-command)
	                  (assert-true (contains-substring-p "origin/feature/i47-smoke" upstream-output)
	                               "Expected create-pr auto-push to establish upstream tracking, got ~S."
	                               upstream-output)))
	          (setf (symbol-value git-generator-sym) old-generator)
	          (setf (symbol-value git-pr-generator-sym) old-pr-generator)
	          (setf (symbol-value git-pr-runner-sym) old-pr-runner)
	          (funcall setconfig-fn :project-root old-project-root)
	          (funcall setconfig-fn :permission-mode old-permission-mode)))))

  (format t "AMOEBUM_GIT_SMOKE_OK~%")))
