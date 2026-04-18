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

(defun %worktree-test-run-program-output (directory command)
  (multiple-value-bind (stdout stderr status)
      (uiop:run-program command
                        :directory directory
                        :output :string
                        :error-output :string
                        :ignore-error-status t)
    (is (= status 0)
        "Command failed: ~S => ~A" command stderr)
    (string-trim '(#\Space #\Tab #\Newline #\Return) stdout)))

(defun %worktree-test-current-branch (directory)
  (%worktree-test-run-program-output
   directory
   '("git" "rev-parse" "--abbrev-ref" "HEAD")))

(defun %worktree-test-branch-exists-p (directory branch)
  (%worktree-test-run-program-ok
   directory
   (list "git" "rev-parse" "--verify" branch)))

(defun %worktree-test-show-file (directory revision relative-path)
  (%worktree-test-run-program-output
   directory
   (list "git" "show" (format nil "~A:~A" revision relative-path))))

(defun %worktree-test-commit-file (directory relative-path contents message)
  (%write-text-file (merge-pathnames relative-path directory) contents)
  (is (%worktree-test-run-program-ok
       directory
       (list "git" "add" relative-path)))
  (is (%worktree-test-run-program-ok
       directory
       (list "git" "commit" "-m" message))))

(defun %init-worktree-test-repo (repo-root)
  (ensure-directories-exist repo-root)
  (is (%worktree-test-run-program-ok repo-root '("git" "init")))
  (is (%worktree-test-run-program-ok repo-root '("git" "config" "user.email" "amoebum@example.com")))
  (is (%worktree-test-run-program-ok repo-root '("git" "config" "user.name" "Amoebum Test")))
  (%write-text-file (merge-pathnames #P"README.md" repo-root)
                    (format nil "# worktree runtime~%"))
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

(test worktree-runtime-remote-coordinator-dispatches-through-backend-neutral-seam
  "Remote runtimes should preserve metadata and dispatch create/inspect/collect/merge/kill through coordinator callbacks."
  (let* ((tmp-root (%make-temp-directory "amoebum-worktree-runtime"))
         (project-root (merge-pathnames #P"repo/" tmp-root))
         (spawn-call nil)
         (collect-call nil)
         (inspect-call nil)
         (merge-call nil)
         (kill-call nil)
         (coordinator
           (sw4rm-sdk:make-remote-worktree-coordinator
            :spawn-fn (lambda (worktree-id worktree-path branch &key base-ref)
                        (setf spawn-call (list :id worktree-id
                                               :path worktree-path
                                               :branch branch
                                               :base-ref base-ref))
                        (list :worktree-id worktree-id
                              :path worktree-path
                              :branch branch
                              :state :spawned))
            :collect-fn (lambda (worktree-id)
                          (setf collect-call worktree-id)
                          (list :record (list :worktree-id worktree-id
                                              :path "/remote/wt-347/"
                                              :branch "sw4rm/flow-347/node-a"
                                              :state :spawned)
                                :live (list :path "/remote/wt-347/"
                                            :branch "refs/heads/sw4rm/flow-347/node-a")))
            :inspect-fn (lambda (worktree-id &key worktree-path branch)
                          (setf inspect-call (list :id worktree-id
                                                   :path worktree-path
                                                   :branch branch))
                          (list :id worktree-id
                                :path (or worktree-path "/remote/wt-347/")
                                :branch (or branch "sw4rm/flow-347/node-a")
                                :lifecycle-state :active
                                :remote-state :ok))
            :merge-fn (lambda (request)
                        (setf merge-call request)
                        (list :merge-status :remote-merged
                              :target-ref (getf request :target-ref)
                              :backend :remote))
            :kill-fn (lambda (worktree-id &key force)
                       (setf kill-call (list :id worktree-id :force force))
                       t)))
         (runtime (amoebum:make-worktree-runtime
                   :project-root project-root
                   :backend :remote
                   :coordinator coordinator)))
    (unwind-protect
         (let* ((spawned (amoebum:spawn-worktree runtime
                                                 nil
                                                 nil
                                                 :workflow-id "Flow 347"
                                                 :node-id "Node A"
                                                 :base-ref "origin/main"))
                (collected (amoebum:collect-worktree runtime
                                                     "sw4rm-flow-347-node-a"))
                (inspected (amoebum:inspect-worktree runtime
                                                     "sw4rm-flow-347-node-a"
                                                     :worktree-branch "sw4rm/flow-347/node-a"))
                (merged (amoebum:merge-worktree
                         :runtime runtime
                         :worktree-id "sw4rm-flow-347-node-a"
                         :worktree-branch "sw4rm/flow-347/node-a"
                         :target-ref "sw4rm/workflow/flow-347"
                         :agent-id "agent-347"
                         :backend :swarm
                         :task "merge remote worktree"
                         :result '(:status :completed))))
           (declare (ignore spawned))
           (is (amoebum:worktree-runtime-remote-p runtime))
           (is (not (amoebum:worktree-runtime-local-p runtime)))
           (is (string= "sw4rm-flow-347-node-a" (getf spawn-call :id)))
           (is (string= "sw4rm/flow-347/node-a" (getf spawn-call :branch)))
           (is (string= "origin/main" (getf spawn-call :base-ref)))
           (is (search ".amoebum/worktrees/sw4rm-flow-347-node-a/"
                       (getf spawn-call :path)))
           (is (string= "sw4rm-flow-347-node-a" collect-call))
           (is (string= "sw4rm/flow-347/node-a"
                        (getf inspected :branch)))
           (is (eq :remote (getf inspected :backend)))
           (is (eq :active (getf inspected :lifecycle-state)))
           (is (string= "sw4rm-flow-347-node-a" (getf inspect-call :id)))
           (is (string= "sw4rm-flow-347-node-a"
                        (getf (getf collected :record) :worktree-id)))
           (is (eq :remote (getf (getf collected :status) :backend)))
           (is (eq :remote-merged (getf merged :merge-status)))
           (is (string= "sw4rm/workflow/flow-347" (getf merge-call :target-ref)))
           (is (string= "sw4rm-flow-347-node-a" (getf merge-call :worktree-id)))
           (is (string= "sw4rm/flow-347/node-a" (getf merge-call :worktree-branch)))
           (is (eq :swarm (getf merge-call :backend)))
           (is-true (getf merge-call :worktree))
           (is (eq t (amoebum:kill-worktree runtime "sw4rm-flow-347-node-a" :force t)))
           (is (string= "sw4rm-flow-347-node-a" (getf kill-call :id)))
           (is-true (getf kill-call :force)))
      (%delete-directory-tree-safe tmp-root))))

(test worktree-runtime-resolves-workflow-node-naming
  "resolve-worktree-metadata should use the sw4rm/<workflow>/<node> convention and local runtime path."
  (let* ((tmp-root (%make-temp-directory "amoebum-worktree-runtime"))
         (project-root (merge-pathnames #P"repo/" tmp-root))
         (runtime (amoebum:make-worktree-runtime :project-root project-root)))
    (unwind-protect
         (let* ((metadata (amoebum:resolve-worktree-metadata
                           :runtime runtime
                           :workflow-id "Feature Implementation"
                           :node-id "Review Pass"))
                (expected-id "sw4rm-feature-implementation-review-pass"))
           (is (typep metadata 'amoebum:worktree-metadata))
           (is (string= expected-id
                        (amoebum:worktree-metadata-id metadata)))
           (is (string= "sw4rm/feature-implementation/review-pass"
                        (amoebum:worktree-metadata-branch metadata)))
           (is (string= (%worktree-test-namestring
                         (amoebum:worktree-runtime-path runtime expected-id))
                        (amoebum:worktree-metadata-path metadata))))
      (%delete-directory-tree-safe tmp-root))))

(test worktree-runtime-resolves-local-fallback-naming
  "resolve-worktree-metadata should derive a deterministic local fallback branch from worktree-id."
  (let* ((tmp-root (%make-temp-directory "amoebum-worktree-runtime"))
         (project-root (merge-pathnames #P"repo/" tmp-root))
         (runtime (amoebum:make-worktree-runtime :project-root project-root)))
    (unwind-protect
         (let* ((metadata (amoebum:resolve-worktree-metadata
                           :runtime runtime
                           :worktree-id "Agent Retry #7"))
                (expected-id "agent-retry-7"))
           (is (typep metadata 'amoebum:worktree-metadata))
           (is (string= expected-id
                        (amoebum:worktree-metadata-id metadata)))
           (is (string= "sw4rm/local/agent-retry-7"
                        (amoebum:worktree-metadata-branch metadata)))
           (is (string= (%worktree-test-namestring
                         (amoebum:worktree-runtime-path runtime expected-id))
                        (amoebum:worktree-metadata-path metadata))))
      (%delete-directory-tree-safe tmp-root))))

(test worktree-runtime-explicit-branch-override-derives-safe-id
  "resolve-worktree-metadata should preserve explicit branch overrides and derive a safe worktree-id."
  (let* ((tmp-root (%make-temp-directory "amoebum-worktree-runtime"))
         (project-root (merge-pathnames #P"repo/" tmp-root))
         (runtime (amoebum:make-worktree-runtime :project-root project-root)))
    (unwind-protect
         (let ((metadata (amoebum:resolve-worktree-metadata
                          :runtime runtime
                          :workflow-id "wf-338"
                          :node-id "implement"
                          :worktree-branch "feature/custom-338")))
           (is (typep metadata 'amoebum:worktree-metadata))
           (is (string= "feature-custom-338"
                        (amoebum:worktree-metadata-id metadata)))
           (is (string= "feature/custom-338"
                        (amoebum:worktree-metadata-branch metadata))))
      (%delete-directory-tree-safe tmp-root))))

(test worktree-runtime-resolves-git-valid-workflow-merge-target
  "resolve-worktree-workflow-branch should map node branches onto a Git-valid workflow merge ref."
  (is (string= "sw4rm/workflow/feature-implementation"
               (amoebum:resolve-worktree-workflow-branch
                :worktree-branch "sw4rm/feature-implementation/review-pass"))))

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

(test worktree-runtime-spawn-accepts-workflow-node-naming
  "spawn-local-worktree should resolve workflow/node naming without bespoke caller-side branch plumbing."
  (let* ((tmp-root (%make-temp-directory "amoebum-worktree-runtime"))
         (repo-root (merge-pathnames #P"repo/" tmp-root))
         (runtime nil))
    (unwind-protect
        (progn
          (%init-worktree-test-repo repo-root)
          (setf runtime (amoebum:make-worktree-runtime :project-root repo-root))
          (let* ((expected-id "sw4rm-bug-fix-regression-test")
                 (record (amoebum:spawn-local-worktree runtime
                                                       nil
                                                       nil
                                                       :workflow-id "bug-fix"
                                                       :node-id "regression-test"
                                                       :base-ref "HEAD"))
                 (path (amoebum:worktree-runtime-path runtime expected-id)))
            (is (string= expected-id (getf record :worktree-id)))
            (is (string= "sw4rm/bug-fix/regression-test"
                         (getf record :branch)))
            (is (probe-file path))
            (is (eq t (amoebum:kill-local-worktree runtime expected-id :force t)))
            (is (null (probe-file path)))))
      (%delete-directory-tree-safe tmp-root))))

(test worktree-runtime-collect-reconstructs-live-record-with-fresh-runtime
  "collect-local-worktree should reconstruct live git state even when coordinator memory is fresh."
  (let* ((tmp-root (%make-temp-directory "amoebum-worktree-runtime"))
         (repo-root (merge-pathnames #P"repo/" tmp-root))
         (runtime nil)
         (fresh-runtime nil))
    (unwind-protect
        (progn
          (%init-worktree-test-repo repo-root)
          (setf runtime (amoebum:make-worktree-runtime :project-root repo-root))
          (amoebum:spawn-local-worktree runtime "wt-2" "feature/wt-2" :base-ref "HEAD")
          (setf fresh-runtime (amoebum:make-worktree-runtime :project-root repo-root))
          (let ((collected (amoebum:collect-local-worktree fresh-runtime "wt-2")))
            (is (string= "wt-2"
                         (getf (getf collected :record) :worktree-id)))
            (is (string= "feature/wt-2"
                         (getf (getf collected :record) :branch)))
            (is (not (null (getf collected :live)))))
          (is (eq t (amoebum:kill-local-worktree fresh-runtime "wt-2" :force t))))
      (%delete-directory-tree-safe tmp-root))))

(test worktree-runtime-cleanup-honours-grace-period-and-sweeps-safe-worktrees
  "cleanup-abandoned-local-worktrees should retain safe worktrees inside the grace window and delete them once expired."
  (let* ((tmp-root (%make-temp-directory "amoebum-worktree-runtime"))
         (repo-root (merge-pathnames #P"repo/" tmp-root))
         (runtime nil)
         (base-ref nil)
         (path nil))
    (unwind-protect
        (progn
          (%init-worktree-test-repo repo-root)
          (setf base-ref (%worktree-test-current-branch repo-root)
                runtime (amoebum:make-worktree-runtime :project-root repo-root))
          (amoebum:spawn-local-worktree runtime "wt-cleanup" "feature/wt-cleanup" :base-ref "HEAD")
          (setf path (amoebum:worktree-runtime-path runtime "wt-cleanup"))
          (amoebum:mark-local-worktree-abandoned
           :runtime runtime
           :worktree-id "wt-cleanup"
           :worktree-branch "feature/wt-cleanup"
           :status :failed
           :finished-at 100)
          (let ((retained (amoebum:cleanup-abandoned-local-worktree
                           :runtime runtime
                           :worktree-id "wt-cleanup"
                           :worktree-branch "feature/wt-cleanup"
                           :status :failed
                           :finished-at 100
                           :now 105
                           :grace-period-seconds 10
                           :base-ref base-ref)))
            (is (eq :retain (getf retained :action)))
            (is (probe-file path)))
          (let ((results (amoebum:cleanup-abandoned-local-worktrees
                          :runtime runtime
                          :now 111
                          :grace-period-seconds 10
                          :base-ref base-ref)))
            (is (= 1 (length results)))
            (is (eq :deleted (getf (first results) :action)))
            (is (null (probe-file path)))))
      (%delete-directory-tree-safe tmp-root))))

(test worktree-runtime-cleanup-retains-review-required-worktrees
  "cleanup-abandoned-local-worktree should preserve worktrees with unique commits for manual review."
  (let* ((tmp-root (%make-temp-directory "amoebum-worktree-runtime"))
         (repo-root (merge-pathnames #P"repo/" tmp-root))
         (runtime nil)
         (base-ref nil)
         (path nil))
    (unwind-protect
        (progn
          (%init-worktree-test-repo repo-root)
          (setf base-ref (%worktree-test-current-branch repo-root)
                runtime (amoebum:make-worktree-runtime :project-root repo-root))
          (amoebum:spawn-local-worktree runtime "wt-review" "feature/wt-review" :base-ref "HEAD")
          (setf path (amoebum:worktree-runtime-path runtime "wt-review"))
          (%worktree-test-commit-file path "feature.txt" "feature branch change\n" "feature work")
          (amoebum:mark-local-worktree-abandoned
           :runtime runtime
           :worktree-id "wt-review"
           :worktree-branch "feature/wt-review"
           :status :failed
           :finished-at 100)
          (let ((decision (amoebum:cleanup-abandoned-local-worktree
                           :runtime runtime
                           :worktree-id "wt-review"
                           :worktree-branch "feature/wt-review"
                           :status :failed
                           :finished-at 100
                           :now 101
                           :grace-period-seconds 0
                           :base-ref base-ref)))
            (is (eq :review-required (getf decision :action)))
            (is (= 1 (getf decision :unique-commit-count)))
            (is (probe-file path)))
          (is (eq t (amoebum:kill-local-worktree runtime "wt-review" :force t))))
      (%delete-directory-tree-safe tmp-root))))

(test worktree-runtime-preflight-detects-overlapping-file-conflicts
  "preflight-local-worktree-merge should return a machine-readable conflict result for overlapping file edits."
  (let* ((tmp-root (%make-temp-directory "amoebum-worktree-runtime"))
         (repo-root (merge-pathnames #P"repo/" tmp-root))
         (runtime nil)
         (base-ref nil)
         (path nil))
    (unwind-protect
        (progn
          (%init-worktree-test-repo repo-root)
          (setf base-ref (%worktree-test-current-branch repo-root)
                runtime (amoebum:make-worktree-runtime :project-root repo-root))
          (amoebum:spawn-local-worktree runtime "wt-conflict" "feature/wt-conflict" :base-ref "HEAD")
          (setf path (amoebum:worktree-runtime-path runtime "wt-conflict"))
          (%worktree-test-commit-file path
                                      "README.md"
                                      (format nil "# worktree runtime~%feature branch edit~%")
                                      "feature readme change")
          (%worktree-test-commit-file repo-root
                                      "README.md"
                                      (format nil "# worktree runtime~%main branch edit~%")
                                      "main readme change")
          (let ((preflight (amoebum:preflight-local-worktree-merge
                            :runtime runtime
                            :worktree-id "wt-conflict"
                            :worktree-branch "feature/wt-conflict"
                            :target-ref base-ref)))
            (is (eq :conflict (getf preflight :status)))
            (is (eq :file-overlap (getf preflight :conflict-kind)))
            (is-true (member "README.md"
                             (getf preflight :conflicts)
                             :test #'string=)))
          (is (eq t (amoebum:kill-local-worktree runtime "wt-conflict" :force t))))
      (%delete-directory-tree-safe tmp-root))))

(test worktree-runtime-preflight-reports-dirty-worktrees
  "preflight-local-worktree-merge should short-circuit dirty worktrees before overlap analysis."
  (let* ((tmp-root (%make-temp-directory "amoebum-worktree-runtime"))
         (repo-root (merge-pathnames #P"repo/" tmp-root))
         (runtime nil)
         (base-ref nil)
         (path nil))
    (unwind-protect
        (progn
          (%init-worktree-test-repo repo-root)
          (setf base-ref (%worktree-test-current-branch repo-root)
                runtime (amoebum:make-worktree-runtime :project-root repo-root))
          (amoebum:spawn-local-worktree runtime "wt-dirty" "feature/wt-dirty" :base-ref "HEAD")
          (setf path (amoebum:worktree-runtime-path runtime "wt-dirty"))
          (%write-text-file (merge-pathnames #P"dirty.txt" path) "pending edit\n")
          (let ((preflight (amoebum:preflight-local-worktree-merge
                            :runtime runtime
                            :worktree-id "wt-dirty"
                            :worktree-branch "feature/wt-dirty"
                            :target-ref base-ref)))
            (is (eq :dirty (getf preflight :status)))
            (is (not (null (getf preflight :dirty-p)))))
          (is (eq t (amoebum:kill-local-worktree runtime "wt-dirty" :force t))))
      (%delete-directory-tree-safe tmp-root))))

(test worktree-runtime-merge-clean-worktree-creates-workflow-target-branch
  "merge-local-worktree should create and merge into the workflow branch target, then restore the caller branch."
  (let* ((tmp-root (%make-temp-directory "amoebum-worktree-runtime"))
         (repo-root (merge-pathnames #P"repo/" tmp-root))
         (runtime nil)
         (base-ref nil)
         (path nil))
    (unwind-protect
        (progn
          (%init-worktree-test-repo repo-root)
          (setf base-ref (%worktree-test-current-branch repo-root)
                runtime (amoebum:make-worktree-runtime :project-root repo-root))
          (amoebum:clear-worktree-conflict-handoffs)
          (amoebum:spawn-local-worktree runtime
                                        "wt-merge"
                                        "sw4rm/merge-flow/node-1"
                                        :base-ref "HEAD")
          (setf path (amoebum:worktree-runtime-path runtime "wt-merge"))
          (%worktree-test-commit-file path
                                      "feature.txt"
                                      (format nil "merged change~%")
                                      "worktree merge change")
          (let ((merge (amoebum:merge-local-worktree
                        :runtime runtime
                        :worktree-id "wt-merge"
                        :worktree-branch "sw4rm/merge-flow/node-1")))
            (is (eq :merged (getf merge :merge-status)))
            (is (string= "sw4rm/workflow/merge-flow" (getf merge :target-ref)))
            (is-true (getf merge :workflow-target-p))
            (is-true (getf merge :created-target-p))
            (is-true (getf merge :deleted-worktree-p)))
          (is (string= base-ref (%worktree-test-current-branch repo-root)))
          (is-true (%worktree-test-branch-exists-p repo-root "sw4rm/workflow/merge-flow"))
          (is (string= "merged change"
                       (%worktree-test-show-file repo-root
                                                 "sw4rm/workflow/merge-flow"
                                                 "feature.txt")))
          (is (null (probe-file path))))
      (%delete-directory-tree-safe tmp-root))))

(test worktree-runtime-merge-blocks-dirty-target-with-user-edits
  "merge-local-worktree should still block unrelated target-repo dirt outside Amoebum-managed worktree state."
  (let* ((tmp-root (%make-temp-directory "amoebum-worktree-runtime"))
         (repo-root (merge-pathnames #P"repo/" tmp-root))
         (runtime nil)
         (path nil))
    (unwind-protect
        (progn
          (%init-worktree-test-repo repo-root)
          (setf runtime (amoebum:make-worktree-runtime :project-root repo-root))
          (amoebum:spawn-local-worktree runtime
                                        "wt-target-dirty"
                                        "sw4rm/dirty-target/node-1"
                                        :base-ref "HEAD")
          (setf path (amoebum:worktree-runtime-path runtime "wt-target-dirty"))
          (%worktree-test-commit-file path
                                      "feature.txt"
                                      (format nil "merged change~%")
                                      "worktree merge change")
          (%write-text-file (merge-pathnames #P"dirty.txt" repo-root)
                            (format nil "pending repo edit~%"))
          (let ((merge (amoebum:merge-local-worktree
                        :runtime runtime
                        :worktree-id "wt-target-dirty"
                        :worktree-branch "sw4rm/dirty-target/node-1")))
            (is (eq :blocked (getf merge :merge-status)))
            (is (eq :target-dirty (getf merge :reason)))
            (is (not (getf merge :created-target-p)))
            (is (not (getf merge :deleted-worktree-p))))
          (is (not (%worktree-test-branch-exists-p repo-root
                                                   "sw4rm/workflow/dirty-target")))
          (is (probe-file path))
          (is (eq t (amoebum:kill-local-worktree runtime "wt-target-dirty" :force t))))
      (%delete-directory-tree-safe tmp-root))))

(test worktree-runtime-merge-conflict-creates-manual-handoff
  "merge-local-worktree should surface overlapping edits as a manual worktree conflict handoff with negotiation metadata."
  (let* ((tmp-root (%make-temp-directory "amoebum-worktree-runtime"))
         (repo-root (merge-pathnames #P"repo/" tmp-root))
         (runtime nil)
         (path nil))
    (unwind-protect
        (progn
          (%init-worktree-test-repo repo-root)
          (setf runtime (amoebum:make-worktree-runtime :project-root repo-root))
          (amoebum:clear-worktree-conflict-handoffs)
          (amoebum:spawn-local-worktree runtime
                                        "wt-handoff"
                                        "sw4rm/conflict-flow/node-1"
                                        :base-ref "HEAD")
          (setf path (amoebum:worktree-runtime-path runtime "wt-handoff"))
          (%worktree-test-commit-file path
                                      "README.md"
                                      (format nil "# worktree runtime~%feature branch edit~%")
                                      "feature readme change")
          (%worktree-test-commit-file repo-root
                                      "README.md"
                                      (format nil "# worktree runtime~%main branch edit~%")
                                      "main readme change")
          (let* ((merge (amoebum:merge-local-worktree
                         :runtime runtime
                         :worktree-id "wt-handoff"
                         :worktree-branch "sw4rm/conflict-flow/node-1"
                         :agent-id "task-merge-handoff"
                         :backend :local
                         :task "resolve merge conflict"
                         :result '(:status :completed)))
                 (handoff-id (getf merge :handoff-id))
                 (snapshot (and handoff-id
                                (amoebum:find-worktree-conflict-handoff
                                 handoff-id
                                 :include-room-status-p t))))
            (is (eq :conflict-handoff (getf merge :merge-status)))
            (is (string= "sw4rm/workflow/conflict-flow" (getf merge :target-ref)))
            (is (eq :file-overlap (getf merge :reason)))
            (is (stringp handoff-id))
            (is (stringp (getf merge :negotiation-room-id)))
            (is (stringp (getf merge :artifact-id)))
            (is (= 1 (length (amoebum:list-worktree-conflict-handoffs))))
            (is (eq :pending (getf snapshot :status)))
            (is (string= "task-merge-handoff" (getf snapshot :agent-id)))
            (is-true (member "README.md"
                             (getf (getf snapshot :preflight) :conflicts)
                             :test #'string=))
            (is (stringp (getf snapshot :negotiation-room-id))))
          (is (probe-file path))
          (is (eq t (amoebum:kill-local-worktree runtime "wt-handoff" :force t))))
      (amoebum:clear-worktree-conflict-handoffs)
      (%delete-directory-tree-safe tmp-root))))

(test worktree-runtime-conflict-handoff-status-updates
  "Manual worktree conflict handoffs should materialize a resolution path and support terminal operator outcomes."
  (unwind-protect
      (progn
        (amoebum:clear-worktree-conflict-handoffs)
        (let* ((snapshot (amoebum:create-worktree-conflict-handoff
                          :worktree (amoebum:make-worktree-metadata
                                     :id "wt-344"
                                     :branch "sw4rm/manual/node"
                                     :path "/tmp/wt-344/")
                          :target-ref "sw4rm/manual"
                          :preflight '(:status :conflict
                                       :conflicts ("README.md")
                                       :conflict-kind :file-overlap)
                          :agent-id "swarm-344"
                          :backend :swarm
                          :task "manual conflict"
                          :result '(:status :completed)))
               (handoff-id (getf snapshot :handoff-id))
               (accepted (amoebum:accept-worktree-conflict-handoff
                          handoff-id
                          :note "taking ownership"))
               (resolved (amoebum:resolve-worktree-conflict-handoff
                          handoff-id
                          :note "merged by operator")))
          (is (stringp handoff-id))
          (is (eq :accepted (getf accepted :status)))
          (is (string= "taking ownership" (getf accepted :note)))
          (is (eq :active (getf (getf accepted :resolution) :status)))
          (is (eq :operator (getf (getf accepted :resolution) :owner)))
          (is (eq :resolved (getf resolved :status)))
          (is (eq :resolved (getf (getf resolved :resolution) :status)))
          (is (string= "merged by operator" (getf resolved :note))))
        (let* ((snapshot (amoebum:create-worktree-conflict-handoff
                          :worktree (amoebum:make-worktree-metadata
                                     :id "wt-344b"
                                     :branch "sw4rm/manual/node-b"
                                     :path "/tmp/wt-344b/")
                          :target-ref "sw4rm/manual"
                          :preflight '(:status :conflict
                                       :conflicts ("docs.txt")
                                       :conflict-kind :file-overlap)
                          :agent-id "swarm-344b"
                          :backend :swarm
                          :task "manual conflict"
                          :result '(:status :completed)))
               (handoff-id (getf snapshot :handoff-id)))
          (amoebum:accept-worktree-conflict-handoff handoff-id
                                                    :note "investigating")
          (let ((abandoned (amoebum:abandon-worktree-conflict-handoff
                            handoff-id
                            :note "operator declined")))
            (is (eq :abandoned (getf abandoned :status)))
            (is (eq :abandoned (getf (getf abandoned :resolution) :status)))
            (is (string= "operator declined" (getf abandoned :note))))))
    (amoebum:clear-worktree-conflict-handoffs)))
