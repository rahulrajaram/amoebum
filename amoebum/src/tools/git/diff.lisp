(in-package :amoebum)

(defun %git-staged-diff (root)
  (let* ((result (%git-run-command-or-error root '("diff" "--cached" "--")))
         (diff (or (getf result :stdout) "")))
    (if (> (length diff) *git-max-diff-chars*)
        (subseq diff 0 *git-max-diff-chars*)
        diff)))

(defun %git-diff-branch-data (&key project-root base-branch)
  (let* ((root (%git-ensure-work-tree (%git-project-root project-root)))
         (branch (%git-current-branch root))
         (resolved-base (%git-resolve-base-branch root base-branch))
         (merge-base (%git-merge-base root resolved-base))
         (range (format nil "~A..HEAD" merge-base))
         (diff-result (%git-run-command-or-error root
                                                 (list "diff" "--no-color" range "--")))
         (summary-result (%git-run-command-or-error root
                                                    (list "diff" "--shortstat" range "--")))
         (files-result (%git-run-command-or-error root
                                                  (list "diff" "--name-only" range "--")))
         (ahead-result (%git-run-command-or-error root
                                                  (list "rev-list" "--count" range)))
         (diff (or (getf diff-result :stdout) ""))
         (truncated-p (> (length diff) *git-max-branch-diff-chars*))
         (diff* (if truncated-p
                    (subseq diff 0 *git-max-branch-diff-chars*)
                    diff)))
    (%git-publish-lifecycle-event
     +event-type-git-branch+
     (make-branch-event :old-branch resolved-base
                        :new-branch branch
                        :action :diff)
     :severity :debug)
    (list :project-root (coerce-path-string root)
          :branch branch
          :base-branch resolved-base
          :merge-base merge-base
          :range range
          :ahead-commits (%git-safe-parse-int (%git-trim-whitespace
                                               (getf ahead-result :stdout)))
          :files-changed (%git-parse-lines-non-empty (getf files-result :stdout))
          :summary (%git-trim-whitespace (getf summary-result :stdout))
          :diff diff*
          :truncated-p truncated-p)))
