(in-package :amoebum)

;;; ---------------------------------------------------------------------------
;;; NXT-357: Worktree merge-target resolution and merge/preflight policy
;;;
;;; Keep merge-target discovery, clean-merge execution, and conflict handoff
;;; policy behind a dedicated module boundary while preserving the :amoebum API.
;;; ---------------------------------------------------------------------------

(defun %local-worktree-preflight-result (inspection status target-ref
                                          &key merge-base
                                               worktree-files
                                               target-files
                                               conflicts
                                               conflict-kind
                                               error-message)
  (append inspection
          (list :status status
                :target-ref target-ref
                :merge-base merge-base
                :worktree-files worktree-files
                :target-files target-files
                :conflict-kind conflict-kind
                :conflict-p (not (null conflicts))
                :conflicts (or conflicts '())
                :error-message error-message)))

(defun %git-diff-name-set (directory revision-range)
  (multiple-value-bind (files error-message)
      (%git-output-lines directory
                         (list "diff" "--name-only" revision-range))
    (if error-message
        (values nil error-message)
        (values (sort (remove-duplicates files :test #'string=)
                      #'string<)
                nil))))

(defun %compute-local-worktree-preflight-data (repo-root worktree-path target-ref)
  (multiple-value-bind (head head-error)
      (%git-output-string worktree-path '("rev-parse" "HEAD"))
    (if (null head)
        (values nil head-error)
        (multiple-value-bind (merge-base merge-base-error)
            (%git-output-string repo-root
                                (list "merge-base" target-ref head))
          (if (null merge-base)
              (values nil merge-base-error)
              (multiple-value-bind (worktree-files worktree-error)
                  (%git-diff-name-set worktree-path
                                      (format nil "~A..HEAD" merge-base))
                (if worktree-error
                    (values nil worktree-error)
                    (multiple-value-bind (target-files target-error)
                        (%git-diff-name-set repo-root
                                            (format nil "~A..~A"
                                                    merge-base
                                                    target-ref))
                      (if target-error
                          (values nil target-error)
                          (let ((conflicts (sort (intersection worktree-files
                                                             target-files
                                                             :test #'string=)
                                                 #'string<)))
                            (values (list :merge-base merge-base
                                          :worktree-files worktree-files
                                          :target-files target-files
                                          :conflicts conflicts
                                          :conflict-kind (and conflicts
                                                              :file-overlap))
                                    nil)))))))))))

(defun resolve-worktree-merge-target (&key runtime
                                           repo-root
                                           worktree
                                           worktree-id
                                           worktree-path
                                           worktree-branch
                                           target-ref)
  (multiple-value-bind (resolved-runtime metadata resolved-repo-root)
      (%resolve-worktree-runtime-and-metadata
       :runtime runtime
       :repo-root repo-root
       :worktree worktree
       :worktree-id worktree-id
       :worktree-path worktree-path
       :worktree-branch worktree-branch)
    (let* ((inspection (inspect-local-worktree
                        :runtime resolved-runtime
                        :repo-root resolved-repo-root
                        :worktree metadata
                        :base-ref target-ref))
           (resolved-base-ref (%normalize-worktree-string
                               (getf inspection :base-ref)))
           (workflow-target (and (null (%normalize-worktree-string target-ref))
                                 (resolve-worktree-workflow-branch
                                  :worktree metadata)))
           (resolved-target-ref (or (%normalize-worktree-string target-ref)
                                    workflow-target
                                    resolved-base-ref))
           (target-exists-p (and resolved-repo-root
                                 resolved-target-ref
                                 (%git-ref-exists-p resolved-repo-root
                                                    resolved-target-ref)))
           (preflight-target-ref (or (and target-exists-p resolved-target-ref)
                                     resolved-base-ref
                                     resolved-target-ref)))
      (append inspection
              (list :target-ref resolved-target-ref
                    :target-exists-p target-exists-p
                    :preflight-target-ref preflight-target-ref
                    :seed-ref resolved-base-ref
                    :workflow-branch workflow-target
                    :workflow-target-p (and workflow-target
                                            resolved-target-ref
                                            (string= workflow-target
                                                     resolved-target-ref)))))))

(defun preflight-local-worktree-merge (&key runtime
                                            repo-root
                                            worktree
                                            worktree-id
                                            worktree-path
                                            worktree-branch
                                            target-ref)
  (multiple-value-bind (resolved-runtime metadata resolved-repo-root)
      (%resolve-worktree-runtime-and-metadata
       :runtime runtime
       :repo-root repo-root
       :worktree worktree
       :worktree-id worktree-id
       :worktree-path worktree-path
       :worktree-branch worktree-branch)
    (let* ((inspection (inspect-local-worktree
                        :runtime resolved-runtime
                        :repo-root resolved-repo-root
                        :worktree metadata
                        :base-ref target-ref))
           (resolved-target-ref (or (%normalize-worktree-string target-ref)
                                    (getf inspection :base-ref)))
           (resolved-path (getf inspection :path)))
      (cond
        ((not (getf inspection :live-p))
         (%local-worktree-preflight-result inspection
                                           :missing
                                           resolved-target-ref))
        ((getf inspection :dirty-p)
         (%local-worktree-preflight-result inspection
                                           :dirty
                                           resolved-target-ref))
        ((null resolved-target-ref)
         (%local-worktree-preflight-result
          inspection
          :error
          nil
          :error-message "Unable to resolve merge target ref."))
        (t
         (multiple-value-bind (preflight-data error-message)
             (%compute-local-worktree-preflight-data
              resolved-repo-root
              resolved-path
              resolved-target-ref)
           (if error-message
               (%local-worktree-preflight-result
                inspection
                :error
                resolved-target-ref
                :error-message error-message)
               (%local-worktree-preflight-result
                inspection
                (if (getf preflight-data :conflicts)
                    :conflict
                    :clean)
                resolved-target-ref
                :merge-base (getf preflight-data :merge-base)
                :worktree-files (getf preflight-data :worktree-files)
                :target-files (getf preflight-data :target-files)
                :conflicts (getf preflight-data :conflicts)
                :conflict-kind (getf preflight-data :conflict-kind)))))))))

(defun %local-worktree-merge-result (merge-target merge-status
                                     &key preflight
                                          created-target-p
                                          deleted-worktree-p
                                          handoff
                                          error-message
                                          reason)
  (append merge-target
          (list :merge-status merge-status
                :preflight (%copy-worktree-data preflight)
                :created-target-p created-target-p
                :deleted-worktree-p deleted-worktree-p
                :handoff-id (and handoff (getf handoff :handoff-id))
                :negotiation-room-id (and handoff
                                          (getf handoff :negotiation-room-id))
                :artifact-id (and handoff (getf handoff :artifact-id))
                :reason reason
                :error-message error-message)))

(defun %worktree-git-error-message (args stderr)
  (or (%normalize-worktree-string stderr)
      (format nil "Git command failed: git ~{~A~^ ~}" args)))

(defun %ensure-local-worktree-merge-target! (merge-target preflight
                                                          repo-root
                                                          target-ref)
  (if (getf merge-target :target-exists-p)
      (values nil nil)
      (let ((seed-ref (%normalize-worktree-string
                       (getf merge-target :seed-ref))))
        (if (null seed-ref)
            (values nil
                    (%local-worktree-merge-result
                     merge-target
                     :blocked
                     :preflight preflight
                     :reason :missing-seed-ref))
            (multiple-value-bind (_stdout stderr status)
                (%run-worktree-git repo-root
                                   (list "branch" target-ref seed-ref))
              (declare (ignore _stdout))
              (if (zerop status)
                  (values t nil)
                  (values nil
                          (%local-worktree-merge-result
                           merge-target
                           :error
                           :preflight preflight
                           :error-message
                           (%worktree-git-error-message
                            (list "branch" target-ref seed-ref)
                            stderr)
                           :reason :create-target-failed))))))))

(defun %checkout-local-worktree-merge-target! (merge-target preflight
                                                               repo-root
                                                               head-state
                                                               target-ref
                                                               created-target-p)
  (if (and (not (getf head-state :detached-p))
           (string= (getf head-state :ref) target-ref))
      (values nil nil)
      (multiple-value-bind (_stdout stderr status)
          (%run-worktree-git repo-root
                             (list "checkout" target-ref))
        (declare (ignore _stdout))
        (if (zerop status)
            (values t nil)
            (values nil
                    (%local-worktree-merge-result
                     merge-target
                     :error
                     :preflight preflight
                     :created-target-p created-target-p
                     :error-message
                     (%worktree-git-error-message
                      (list "checkout" target-ref)
                      stderr)
                     :reason :checkout-target-failed))))))

(defun %merge-local-worktree-branch-into-target! (merge-target preflight
                                                                 repo-root
                                                                 worktree-branch
                                                                 created-target-p)
  (let ((args (list "merge" "--no-edit" worktree-branch)))
    (multiple-value-bind (_stdout stderr status)
        (%run-worktree-git repo-root args)
      (declare (ignore _stdout))
      (when (not (zerop status))
        (ignore-errors
          (%run-worktree-git repo-root '("merge" "--abort")))
        (%local-worktree-merge-result
         merge-target
         :error
         :preflight preflight
         :created-target-p created-target-p
         :error-message (%worktree-git-error-message args stderr)
         :reason :merge-command-failed)))))

(defun %maybe-delete-local-worktree-after-merge (runtime
                                                 merge-target
                                                 repo-root
                                                 worktree
                                                 worktree-id
                                                 worktree-path
                                                 worktree-branch)
  (eq t (kill-local-worktree
         (or runtime
             (make-worktree-runtime :repo-root repo-root))
         (or worktree-id
             (getf merge-target :id))
         :force t
         :worktree worktree
         :worktree-path worktree-path
         :worktree-branch worktree-branch)))

(defun %perform-clean-local-worktree-merge (&key runtime
                                                 merge-target
                                                 preflight
                                                 repo-root
                                                 target-ref
                                                 worktree
                                                 worktree-id
                                                 worktree-path
                                                 worktree-branch
                                                 delete-worktree-p)
  (let* ((head-state (%git-current-head-state repo-root))
         (restore-head-p nil)
         (created-target-p nil)
         (deleted-worktree-p nil))
    (unwind-protect
         (progn
           (multiple-value-bind (created-target-p* early-result)
               (%ensure-local-worktree-merge-target! merge-target
                                                     preflight
                                                     repo-root
                                                     target-ref)
             (when early-result
               (return-from %perform-clean-local-worktree-merge early-result))
             (setf created-target-p created-target-p*))
           (multiple-value-bind (restore-head-p* early-result)
               (%checkout-local-worktree-merge-target! merge-target
                                                       preflight
                                                       repo-root
                                                       head-state
                                                       target-ref
                                                       created-target-p)
             (when early-result
               (return-from %perform-clean-local-worktree-merge early-result))
             (setf restore-head-p restore-head-p*))
           (let ((merge-error
                   (%merge-local-worktree-branch-into-target! merge-target
                                                              preflight
                                                              repo-root
                                                              (%normalize-worktree-string
                                                               (getf merge-target :branch))
                                                              created-target-p)))
             (when merge-error
               (return-from %perform-clean-local-worktree-merge merge-error)))
           (when delete-worktree-p
             (setf deleted-worktree-p
                   (%maybe-delete-local-worktree-after-merge runtime
                                                             merge-target
                                                             repo-root
                                                             worktree
                                                             worktree-id
                                                             worktree-path
                                                             worktree-branch)))
           (%local-worktree-merge-result
            merge-target
            :merged
            :preflight preflight
            :created-target-p created-target-p
            :deleted-worktree-p deleted-worktree-p))
      (when restore-head-p
        (ignore-errors
          (%restore-git-head-state repo-root head-state))))))

(defun merge-local-worktree (&key runtime
                                  repo-root
                                  worktree
                                  worktree-id
                                  worktree-path
                                  worktree-branch
                                  target-ref
                                  (delete-worktree-p t)
                                  agent-id
                                  backend
                                  task
                                  result)
  (let* ((merge-target (resolve-worktree-merge-target
                        :runtime runtime
                        :repo-root repo-root
                        :worktree worktree
                        :worktree-id worktree-id
                        :worktree-path worktree-path
                        :worktree-branch worktree-branch
                        :target-ref target-ref))
         (resolved-repo-root (and (getf merge-target :repo-root)
                                  (pathname (getf merge-target :repo-root))))
         (resolved-target-ref (%normalize-worktree-string
                               (getf merge-target :target-ref)))
         (preflight-target-ref (%normalize-worktree-string
                                (getf merge-target :preflight-target-ref)))
         (resolved-worktree-branch (%normalize-worktree-string
                                    (getf merge-target :branch)))
         (status-runtime (or runtime
                             (%runtime-for-worktree-source
                              :repo-root resolved-repo-root
                              :worktree-path (or worktree-path
                                                 (getf merge-target :path)))))
         (ignored-status-path-prefixes
           (%managed-worktree-status-ignored-paths status-runtime
                                                   resolved-repo-root)))
    (cond
      ((null resolved-target-ref)
       (%local-worktree-merge-result merge-target
                                     :blocked
                                     :reason :missing-target))
      ((or (null preflight-target-ref)
           (null resolved-repo-root))
       (%local-worktree-merge-result merge-target
                                     :blocked
                                     :reason :missing-preflight-target))
      ((null resolved-worktree-branch)
       (%local-worktree-merge-result merge-target
                                     :blocked
                                     :reason :missing-worktree-branch))
      ((and resolved-worktree-branch
            (string= resolved-worktree-branch resolved-target-ref))
       (%local-worktree-merge-result merge-target
                                     :skipped
                                     :reason :self-target))
      (t
       (let ((preflight (preflight-local-worktree-merge
                         :runtime runtime
                         :repo-root resolved-repo-root
                         :worktree worktree
                         :worktree-id worktree-id
                         :worktree-path worktree-path
                         :worktree-branch worktree-branch
                         :target-ref preflight-target-ref)))
         (case (getf preflight :status)
           (:clean
            (multiple-value-bind (clean-p clean-error)
                (%git-clean-working-tree-p
                 resolved-repo-root
                 :ignored-path-prefixes ignored-status-path-prefixes)
              (cond
                (clean-error
                 (%local-worktree-merge-result merge-target
                                               :error
                                               :preflight preflight
                                               :error-message clean-error
                                               :reason :target-status-error))
                ((not clean-p)
                 (%local-worktree-merge-result merge-target
                                               :blocked
                                               :preflight preflight
                                               :reason :target-dirty))
                (t
                 (%perform-clean-local-worktree-merge
                  :runtime runtime
                  :merge-target merge-target
                  :preflight preflight
                  :repo-root resolved-repo-root
                  :target-ref resolved-target-ref
                  :worktree worktree
                  :worktree-id worktree-id
                  :worktree-path worktree-path
                  :worktree-branch worktree-branch
                  :delete-worktree-p delete-worktree-p)))))
           (:conflict
            (handler-case
                (let ((handoff (create-worktree-conflict-handoff
                                :worktree (or worktree
                                              (make-worktree-metadata
                                               :id (getf merge-target :id)
                                               :branch resolved-worktree-branch
                                               :path (getf merge-target :path)))
                                :target-ref resolved-target-ref
                                :preflight preflight
                                :agent-id agent-id
                                :backend backend
                                :task task
                                :result result)))
                  (%local-worktree-merge-result
                   merge-target
                   :conflict-handoff
                   :preflight preflight
                   :handoff handoff
                   :reason (or (getf preflight :conflict-kind)
                               :conflict)))
              (error (condition)
                (%local-worktree-merge-result
                 merge-target
                 :error
                 :preflight preflight
                 :error-message (princ-to-string condition)
                 :reason :handoff-creation-failed))))
           (otherwise
            (%local-worktree-merge-result
             merge-target
             :blocked
             :preflight preflight
             :reason (getf preflight :status)))))))))
