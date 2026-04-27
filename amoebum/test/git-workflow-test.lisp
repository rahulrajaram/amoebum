(in-package :amoebum/test)

(def-suite git-workflow-suite
  :in amoebum-suite
  :description "Direct git tool workflow coverage for the control-plane focused verifier.")

(in-suite git-workflow-suite)

(defun %nxt433-git-tool-args (&rest key-values)
  (let ((args (make-hash-table :test #'equal)))
    (loop for (key value) on key-values by #'cddr do
      (setf (gethash key args) value))
    args))

(defun %nxt433-invoke-git-tool (tool-name &rest key-values)
  (let* ((tool (pseudopod:find-tool amoebum:*toolset* tool-name))
         (fn (and tool (pseudopod:tool-definition-fn tool))))
    (unless fn
      (error "Expected git tool ~S to be available." tool-name))
    (funcall fn (apply #'%nxt433-git-tool-args key-values))))

(defun %nxt433-run-git (root &rest args)
  (multiple-value-bind (stdout stderr exit-code)
      (uiop:run-program (append (list "git" "-C" (namestring root)) args)
                        :ignore-error-status t
                        :output :string
                        :error-output :string)
    (let ((trimmed-out (string-trim '(#\Space #\Tab #\Newline #\Return)
                                    (or stdout "")))
          (trimmed-err (string-trim '(#\Space #\Tab #\Newline #\Return)
                                    (or stderr ""))))
      (unless (zerop (or exit-code 1))
        (error "git ~{~A~^ ~} failed (~D): ~A"
               args
               exit-code
               (if (plusp (length trimmed-err))
                   trimmed-err
                   trimmed-out)))
      trimmed-out)))

(defun %nxt433-contains-substring-p (needle haystack)
  (and (stringp haystack)
       (search needle haystack :test #'char-equal)))

(defun %nxt433-setup-repo (root)
  (ensure-directories-exist root)
  (%nxt433-run-git root "init")
  (%nxt433-run-git root "config" "user.name" "Amoebum NXT-433")
  (%nxt433-run-git root "config" "user.email" "amoebum-nxt433@example.com")
  (%write-text-file (merge-pathnames #P"README.md" root) "seed\n")
  (%nxt433-run-git root "add" "--" "README.md")
  (%nxt433-run-git root "commit" "-m" "chore: seed")
  (%nxt433-run-git root "branch" "-M" "main")
  root)

(defmacro with-nxt433-git-repo ((root) &body body)
  `(let* ((tmp-root (%make-temp-directory "amoebum-nxt433"))
          (old-config (amoebum.config:current-config))
          (old-project-root (amoebum.config:config-project-root old-config))
          (old-mode (amoebum.config:config-permission-mode old-config))
          (old-generator amoebum::*git-commit-message-generator*)
          (old-pr-generator amoebum::*git-pr-description-generator*)
          (old-pr-runner amoebum::*git-pr-command-runner*))
     (unwind-protect
          (progn
            (%nxt433-setup-repo tmp-root)
            (amoebum.config:setconfig :project-root tmp-root)
            (amoebum.config:setconfig :permission-mode :full-auto)
            (let ((,root tmp-root))
              ,@body))
       (setf amoebum::*git-commit-message-generator* old-generator
             amoebum::*git-pr-description-generator* old-pr-generator
             amoebum::*git-pr-command-runner* old-pr-runner)
       (amoebum.config:setconfig :project-root old-project-root)
       (amoebum.config:setconfig :permission-mode old-mode)
       (%delete-directory-tree-safe tmp-root))))

(test nxt433-git-status-commit-diff-and-pr-flow
  (with-nxt433-git-repo (root)
    (%write-text-file (merge-pathnames #P"staged.txt" root) "staged content\n")
    (%write-text-file (merge-pathnames #P"unstaged.txt" root) "unstaged content\n")
    (%nxt433-run-git root "add" "--" "staged.txt")

    (let* ((status (%nxt433-invoke-git-tool "git-status"))
           (staged (getf status :staged))
           (unstaged (getf status :unstaged)))
      (is (member "staged.txt" staged :test #'string=))
      (is (member "unstaged.txt" unstaged :test #'string=)))

    (setf amoebum::*git-commit-message-generator*
          (lambda (&rest _)
            (declare (ignore _))
            "feat: add smoke-tested git commit flow"))
    (let* ((result (%nxt433-invoke-git-tool "git-commit" "files" '("staged.txt")))
           (summary (getf result :message-summary)))
      (is (string= summary "feat: add smoke-tested git commit flow")))

    (%nxt433-run-git root "checkout" "-b" "feature/nxt433")
    (%write-text-file (merge-pathnames #P"feature.txt" root)
                      "feature branch diff content\n")
    (%nxt433-run-git root "add" "--" "feature.txt")
    (%nxt433-run-git root "commit" "-m" "feat: add branch diff fixture")
    (%write-text-file (merge-pathnames #P"feature-extra.txt" root)
                      "second feature commit\n")
    (%nxt433-run-git root "add" "--" "feature-extra.txt")
    (%nxt433-run-git root "commit" "-m" "fix: add second branch fixture")

    (let* ((branch-diff (%nxt433-invoke-git-tool "git-diff-branch"))
           (files-changed (getf branch-diff :files-changed)))
      (is (string= (getf branch-diff :branch) "feature/nxt433"))
      (is (string= (getf branch-diff :base-branch) "main"))
      (is (member "feature.txt" files-changed :test #'string=)))

    (%nxt433-run-git root "init" "--bare" (namestring (merge-pathnames #P"origin.git" root)))
    (%nxt433-run-git root "remote" "add" "origin"
                     (namestring (merge-pathnames #P"origin.git" root)))

    (let ((captured-pr-command nil))
      (setf amoebum::*git-pr-description-generator*
            (lambda (&rest _)
              (declare (ignore _))
              (list :title "feat: create smoke-test pull request"
                    :body "## Summary\n- add branch diff fixture")))
      (setf amoebum::*git-pr-command-runner*
            (lambda (_root command)
              (declare (ignore _root))
              (setf captured-pr-command command)
              (list :stdout "https://github.com/example/amoebum/pull/433\n"
                    :stderr ""
                    :exit-code 0)))
      (let ((result (%nxt433-invoke-git-tool "create-pr")))
        (is (string= (getf result :url) "https://github.com/example/amoebum/pull/433"))
        (is (eq (getf result :description-source) :llm))
        (is (%nxt433-contains-substring-p "gh pr create" captured-pr-command))))))
