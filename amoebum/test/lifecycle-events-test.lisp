(in-package :amoebum/test)

(def-suite lifecycle-events-suite
  :description "Lifecycle event type and git emission tests (I218)."
  :in amoebum-suite)

(in-suite lifecycle-events-suite)

(defun %git-tool-args (&rest kvs)
  (let ((table (make-hash-table :test #'equal)))
    (loop for (key value) on kvs by #'cddr do
      (setf (gethash key table) value))
    table))

(defun %invoke-git-tool (name &rest kvs)
  (let* ((tool (pseudopod:find-tool amoebum:*toolset* name))
         (fn (and tool (pseudopod:tool-definition-fn tool))))
    (unless fn
      (error "Expected git tool ~S to be available." name))
    (funcall fn (apply #'%git-tool-args kvs))))

(defun %event-history-of-type (bus event-type)
  (remove-if-not (lambda (event)
                   (eq (amoebum:event-type event) event-type))
                 (amoebum:event-history bus)))

(defun %run-git (repo-path &rest args)
  (multiple-value-bind (stdout stderr exit-code)
      (uiop:run-program (append (list "git" "-C" repo-path) args)
                        :ignore-error-status t
                        :output :string
                        :error-output :string)
    (unless (zerop exit-code)
      (error "git ~{~A~^ ~} failed: ~A~%~A" args stdout stderr))
    stdout))

(test lifecycle-event-constructors-round-trip
  (let* ((commit (amoebum:make-commit-event
                  :hash "abc123"
                  :message "feat: add lifecycle test"
                  :author "Amoebum Bot <amoebum@example.com>"
                  :files-changed '("a.txt" "b.txt")))
         (branch (amoebum:make-branch-event
                  :old-branch "main"
                  :new-branch "feature/i218"
                  :action :switch))
         (spawned (amoebum:make-agent-spawned-event
                   :agent-id "agent-1"
                   :agent-type :task
                   :parent-id "root-0"))
         (completed (amoebum:make-agent-completed-event
                     :agent-id "agent-1"
                     :result-status :completed
                     :elapsed-ms 125))
         (failed (amoebum:make-agent-error-event
                  :agent-id "agent-2"
                  :condition "timeout")))
    (is (amoebum:commit-event-p commit))
    (is (string= "abc123" (amoebum:commit-event-hash commit)))
    (is (eq (amoebum:commit-event-event-type commit) amoebum:+event-type-git-commit+))
    (is (amoebum:branch-event-p branch))
    (is (string= "main" (amoebum:branch-event-old-branch branch)))
    (is (eq (amoebum:branch-event-event-type branch) amoebum:+event-type-git-branch+))
    (is (amoebum:agent-spawned-event-p spawned))
    (is (eq (amoebum:agent-spawned-event-event-type spawned)
            amoebum:+event-type-agent-spawned+))
    (is (amoebum:agent-completed-event-p completed))
    (is (= 125 (amoebum:agent-completed-event-elapsed-ms completed)))
    (is (amoebum:agent-error-event-p failed))
    (is (eq (amoebum:agent-error-event-event-type failed)
            amoebum:+event-type-agent-error+))
    (is-true (amoebum:event-type-p amoebum:+event-type-git-commit+))
    (is-true (amoebum:event-type-p amoebum:+event-type-git-branch+))
    (is-true (amoebum:event-type-p amoebum:+event-type-agent-spawned+))
    (is-true (amoebum:event-type-p amoebum:+event-type-agent-completed+))
    (is-true (amoebum:event-type-p amoebum:+event-type-agent-error+))))

(test git-tools-publish-lifecycle-events
  (let* ((tmp-root (%make-temp-directory "amoebum-i218-git-events"))
         (repo-path (namestring tmp-root))
         (old-config (amoebum:current-config))
         (old-project-root (amoebum:config-project-root old-config))
         (old-permission-mode (amoebum:config-value :permission-mode old-config))
         (old-event-bus amoebum:*event-bus*)
         (old-message-generator amoebum::*git-commit-message-generator*)
         (bus (amoebum:make-event-bus :capacity 128)))
    (unwind-protect
        (progn
          (ensure-directories-exist (merge-pathnames #P".keep" tmp-root))
          (%run-git repo-path "init")
          (%run-git repo-path "config" "user.name" "Amoebum Tests")
          (%run-git repo-path "config" "user.email" "amoebum-tests@example.com")
          (%write-text-file (merge-pathnames #P"README.md" tmp-root) "seed\n")
          (%run-git repo-path "add" "--" "README.md")
          (%run-git repo-path "commit" "-m" "chore: seed repo")
          (%run-git repo-path "branch" "-m" "main")

          (setf amoebum:*event-bus* bus
                amoebum::*git-commit-message-generator*
                (lambda (diff recent-subjects &key model staged-paths project-root)
                  (declare (ignore diff recent-subjects model staged-paths project-root))
                  "feat: publish lifecycle events"))
          (amoebum:setconfig :project-root tmp-root)
          (amoebum:setconfig :permission-mode :full-auto)

          (%invoke-git-tool "git-status")
          (%write-text-file (merge-pathnames #P"tracked.txt" tmp-root) "payload\n")
          (%invoke-git-tool "git-commit" "files" '("tracked.txt"))
          (%invoke-git-tool "git-diff-branch")

          (let* ((branch-events (%event-history-of-type bus amoebum:+event-type-git-branch+))
                 (commit-events (%event-history-of-type bus amoebum:+event-type-git-commit+))
                 (last-branch (car (last branch-events)))
                 (last-commit (car (last commit-events)))
                 (branch-payload (and last-branch (amoebum:event-payload last-branch)))
                 (commit-payload (and last-commit (amoebum:event-payload last-commit))))
            (is (>= (length branch-events) 2)
                "Expected git-status and git-diff-branch to emit git:branch events.")
            (is (= 1 (length commit-events))
                "Expected git-commit to emit exactly one git:commit event.")
            (is (amoebum:branch-event-p branch-payload))
            (is (string= "main" (amoebum:branch-event-new-branch branch-payload)))
            (is (amoebum:commit-event-p commit-payload))
            (is (search "feat: publish lifecycle events"
                        (amoebum:commit-event-message commit-payload)
                        :test #'char-equal))
            (is (member "tracked.txt"
                        (amoebum:commit-event-files-changed commit-payload)
                        :test #'string=))))
      (setf amoebum:*event-bus* old-event-bus
            amoebum::*git-commit-message-generator* old-message-generator)
      (amoebum:setconfig :project-root old-project-root)
      (amoebum:setconfig :permission-mode old-permission-mode)
      (%delete-directory-tree-safe tmp-root))))

(test lifecycle-events-smoke-sentinel
  (is-true t)
  (format t "LIFECYCLE_EVENTS_SMOKE_OK~%"))
