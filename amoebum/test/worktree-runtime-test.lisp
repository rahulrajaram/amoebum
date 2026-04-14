(in-package :amoebum/test)

(def-suite worktree-runtime-suite :in amoebum-suite)
(in-suite worktree-runtime-suite)

(defun %worktree-test-namestring (path)
  (namestring (uiop:ensure-directory-pathname path)))

(defun %worktree-test-run-program-ok (directory command)
  (multiple-value-bind (_stdout _stderr status)
      (uiop:run-program command
                        :directory directory
                        :output :string
                        :error-output :string
                        :ignore-error-status t)
    (declare (ignore _stdout _stderr))
    (= status 0)))

(defun %init-worktree-test-repo (repo-root)
  (ensure-directories-exist repo-root)
  (is (%worktree-test-run-program-ok repo-root '("git" "init")))
  (is (%worktree-test-run-program-ok repo-root '("git" "config" "user.email" "amoebum@example.com")))
  (is (%worktree-test-run-program-ok repo-root '("git" "config" "user.name" "Amoebum Test")))
  (%write-text-file (merge-pathnames #P"README.md" repo-root) "# worktree runtime\n")
  (is (%worktree-test-run-program-ok repo-root '("git" "add" "README.md")))
  (is (%worktree-test-run-program-ok repo-root '("git" "commit" "-m" "init"))))

(test worktree-runtime-defaults-follow-project-root
  "current-worktree-runtime should derive repo/scratch/lock roots from config project-root."
  (let* ((tmp-root (%make-temp-directory "amoebum-worktree-runtime"))
         (project-root (merge-pathnames #P"repo/" tmp-root)))
    (unwind-protect
         (let* ((cfg (amoebum.config:load-config
                      :project-root project-root
                      :global-config-path "/nonexistent/global.lisp"
                      :project-config-path "/nonexistent/project.lisp"
                      :environment-values nil
                      :cli-values nil))
                (runtime (amoebum:current-worktree-runtime cfg)))
           (is (string= (%worktree-test-namestring project-root)
                        (%worktree-test-namestring
                         (amoebum:worktree-runtime-repo-root runtime))))
           (is (string= (%worktree-test-namestring
                         (merge-pathnames #P".amoebum/worktrees/" project-root))
                        (%worktree-test-namestring
                         (amoebum:worktree-runtime-scratch-root runtime))))
           (is (string= (%worktree-test-namestring
                         (merge-pathnames #P".amoebum/worktrees/locks/" project-root))
                        (%worktree-test-namestring
                         (amoebum:worktree-runtime-lock-root runtime)))))
      (%delete-directory-tree-safe tmp-root))))

(test worktree-runtime-honours-explicit-scratch-and-lock-roots
  "make-worktree-runtime should accept explicit scratch and lock roots."
  (let* ((tmp-root (%make-temp-directory "amoebum-worktree-runtime"))
         (project-root (merge-pathnames #P"repo/" tmp-root))
         (scratch-root (merge-pathnames #P"scratch-root/" tmp-root))
         (lock-root (merge-pathnames #P"lock-root/" tmp-root))
         (runtime (amoebum:make-worktree-runtime
                   :project-root project-root
                   :scratch-root scratch-root
                   :lock-root lock-root)))
    (unwind-protect
         (progn
           (is (string= (%worktree-test-namestring scratch-root)
                        (%worktree-test-namestring
                         (amoebum:worktree-runtime-scratch-root runtime))))
           (is (string= (%worktree-test-namestring lock-root)
                        (%worktree-test-namestring
                         (amoebum:worktree-runtime-lock-root runtime)))))
      (%delete-directory-tree-safe tmp-root))))

(test worktree-runtime-spawn-collect-kill-roundtrip
  "The Amoebum wrapper should create, inspect, and remove a local worktree under the default scratch root."
  (let* ((tmp-root (%make-temp-directory "amoebum-worktree-runtime"))
         (repo-root (merge-pathnames #P"repo/" tmp-root))
         (runtime nil))
    (unwind-protect
        (progn
          (%init-worktree-test-repo repo-root)
          (setf runtime (amoebum:make-worktree-runtime :project-root repo-root))
          (let* ((record (amoebum:spawn-local-worktree runtime "wt-1" "feature/wt-1" :base-ref "HEAD"))
                 (path (amoebum:worktree-runtime-path runtime "wt-1")))
            (is (not (null record)))
            (is (probe-file path))
            (let ((collected (amoebum:collect-local-worktree runtime "wt-1")))
              (is (not (null (getf collected :record))))
              (is (not (null (getf collected :live))))
              (is (string= (namestring path)
                           (getf (getf collected :record) :path))))
            (is (eq t (amoebum:kill-local-worktree runtime "wt-1" :force t)))
            (is (null (probe-file path)))))
      (%delete-directory-tree-safe tmp-root))))
