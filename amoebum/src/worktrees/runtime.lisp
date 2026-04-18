(in-package :amoebum)

;;; ---------------------------------------------------------------------------
;;; NXT-356: Worktree runtime construction, naming, and lifecycle dispatch
;;;
;;; Keep runtime construction and spawn/collect/inspect/kill ownership separate
;;; from merge policy so later tranches can peel merge semantics independently.
;;; ---------------------------------------------------------------------------

(defun %worktree-project-root (&key project-root config)
  (uiop:ensure-directory-pathname
   (or project-root
       (and (config-p config) (config-project-root config))
       (and (ignore-errors (current-config))
            (config-project-root (ignore-errors (current-config))))
       (ignore-errors (uiop:getcwd))
       *default-pathname-defaults*)))

(defun default-worktree-repo-root (&key project-root config)
  (%worktree-project-root :project-root project-root
                          :config config))

(defun default-worktree-scratch-root (&key project-root config)
  (merge-pathnames #P".amoebum/worktrees/"
                   (default-worktree-repo-root :project-root project-root
                                               :config config)))

(defun default-worktree-lock-root (&key project-root config scratch-root)
  (merge-pathnames #P"locks/"
                   (uiop:ensure-directory-pathname
                    (or scratch-root
                        (default-worktree-scratch-root :project-root project-root
                                                       :config config)))))

(defun %normalize-worktree-backend (value)
  (case value
    ((nil :local) :local)
    ((:remote :sw4rm :networked) :remote)
    (otherwise
     (error "Unsupported worktree backend ~S." value))))

(defun %default-worktree-backend (&optional (config (ignore-errors (current-config))))
  (let ((mode (ignore-errors (%configured-swarm-delegation-mode config))))
    (if (eq mode :networked)
        :remote
        :local)))

(defun make-worktree-runtime (&key project-root config repo-root scratch-root lock-root
                                   coordinator backend)
  (let* ((resolved-repo-root
           (uiop:ensure-directory-pathname
            (or repo-root
                (default-worktree-repo-root :project-root project-root
                                            :config config))))
         (resolved-scratch-root
           (uiop:ensure-directory-pathname
            (or scratch-root
                (default-worktree-scratch-root :project-root resolved-repo-root))))
         (resolved-lock-root
           (uiop:ensure-directory-pathname
            (or lock-root
                (default-worktree-lock-root :scratch-root resolved-scratch-root))))
         (resolved-backend (%normalize-worktree-backend
                            (or backend
                                (and coordinator :remote)
                                (%default-worktree-backend config))))
         (resolved-coordinator (or coordinator
                                  (sw4rm-sdk:make-git-worktree-coordinator
                                   resolved-repo-root
                                   :lock-dir resolved-lock-root))))
    (ensure-directories-exist resolved-scratch-root)
    (ensure-directories-exist resolved-lock-root)
    (%make-worktree-runtime
     :repo-root resolved-repo-root
     :scratch-root resolved-scratch-root
     :lock-root resolved-lock-root
     :backend resolved-backend
     :coordinator resolved-coordinator)))

(defun current-worktree-runtime (&optional (config (ignore-errors (current-config))))
  (make-worktree-runtime :config config))

(defun worktree-runtime-local-p (runtime)
  (eq (worktree-runtime-backend runtime) :local))

(defun worktree-runtime-remote-p (runtime)
  (eq (worktree-runtime-backend runtime) :remote))

(defun %worktree-id-path-component (worktree-id)
  (let* ((raw (string-downcase
               (string-trim '(#\Space #\Tab #\Newline #\Return)
                            (princ-to-string worktree-id))))
         (buffer (make-string-output-stream))
         (last-was-dash nil))
    (loop for ch across raw do
      (cond
        ((or (alphanumericp ch)
             (char= ch #\_)
             (char= ch #\-))
         (write-char ch buffer)
         (setf last-was-dash nil))
        (last-was-dash
         nil)
        (t
         (write-char #\- buffer)
         (setf last-was-dash t))))
    (let ((component (string-trim "-" (get-output-stream-string buffer))))
      (unless (plusp (length component))
        (error "Invalid worktree id ~S." worktree-id))
      component)))

(defun %normalize-worktree-name-segment (value)
  (let ((normalized (%normalize-worktree-string value)))
    (when normalized
      (let ((buffer (make-string-output-stream))
            (last-was-dash nil))
        (loop for ch across (string-downcase normalized) do
          (cond
            ((or (alphanumericp ch)
                 (char= ch #\_)
                 (char= ch #\-)
                 (char= ch #\.))
             (write-char ch buffer)
             (setf last-was-dash nil))
            (last-was-dash
             nil)
            (t
             (write-char #\- buffer)
             (setf last-was-dash t))))
        (let ((segment (string-trim "-." (get-output-stream-string buffer))))
          (unless (plusp (length segment))
            (error "Invalid worktree naming segment ~S." value))
          segment)))))

(defun resolve-worktree-id (&key worktree worktree-id worktree-branch workflow-id node-id)
  (let* ((base (coerce-worktree-metadata :worktree worktree))
         (explicit-id (or (%normalize-worktree-string worktree-id)
                          (and base (worktree-metadata-id base))))
         (explicit-branch (or (%normalize-worktree-string worktree-branch)
                              (and base (worktree-metadata-branch base))))
         (workflow-segment (%normalize-worktree-name-segment workflow-id))
         (node-segment (%normalize-worktree-name-segment node-id)))
    (cond
      (explicit-id
       (%worktree-id-path-component explicit-id))
      (explicit-branch
       (%worktree-id-path-component explicit-branch))
      ((and workflow-segment node-segment)
       (%worktree-id-path-component
        (format nil "sw4rm/~A/~A" workflow-segment node-segment)))
      (workflow-segment
       (%worktree-id-path-component
        (format nil "sw4rm/~A/local" workflow-segment)))
      (node-segment
       (%worktree-id-path-component
        (format nil "sw4rm/local/~A" node-segment)))
      (t
       nil))))

(defun resolve-worktree-branch (&key worktree worktree-branch worktree-id workflow-id node-id)
  (let* ((base (coerce-worktree-metadata :worktree worktree))
         (explicit-branch (or (%normalize-worktree-string worktree-branch)
                              (and base (worktree-metadata-branch base))))
         (workflow-segment (%normalize-worktree-name-segment workflow-id))
         (node-segment (%normalize-worktree-name-segment node-id))
         (local-segment (%normalize-worktree-name-segment
                         (or (%normalize-worktree-string worktree-id)
                             (and base (worktree-metadata-id base))
                             (resolve-worktree-id :worktree base
                                                  :workflow-id workflow-id
                                                  :node-id node-id)))))
    (cond
      (explicit-branch
       explicit-branch)
      ((and workflow-segment node-segment)
       (format nil "sw4rm/~A/~A" workflow-segment node-segment))
      (workflow-segment
       (format nil "sw4rm/~A/local" workflow-segment))
      (node-segment
       (format nil "sw4rm/local/~A" node-segment))
      (local-segment
       (format nil "sw4rm/local/~A" local-segment))
      (t
       nil))))

(defun resolve-worktree-workflow-branch (&key worktree worktree-branch)
  (let* ((base (coerce-worktree-metadata :worktree worktree
                                         :worktree-branch worktree-branch))
         (branch (%normalize-worktree-string
                  (or worktree-branch
                      (and base (worktree-metadata-branch base))))))
    (when branch
      (let ((segments (uiop:split-string branch :separator '(#\/))))
        (when (and (>= (length segments) 3)
                   (string= "sw4rm" (first segments)))
          (cond
            ((string= "local" (second segments))
             nil)
            ((and (string= "workflow" (second segments))
                  (>= (length segments) 3))
             (format nil "sw4rm/workflow/~A" (third segments)))
            (t
             (format nil "sw4rm/workflow/~A" (second segments)))))))))

(defun resolve-worktree-metadata (&key runtime
                                       worktree
                                       worktree-id
                                       worktree-branch
                                       worktree-path
                                       workflow-id
                                       node-id)
  (let* ((base (coerce-worktree-metadata :worktree worktree))
         (resolved-id (resolve-worktree-id :worktree base
                                           :worktree-id worktree-id
                                           :worktree-branch worktree-branch
                                           :workflow-id workflow-id
                                           :node-id node-id))
         (resolved-branch (resolve-worktree-branch :worktree base
                                                   :worktree-branch worktree-branch
                                                   :worktree-id (or worktree-id resolved-id)
                                                   :workflow-id workflow-id
                                                   :node-id node-id))
         (resolved-path (or (%normalize-worktree-path-string worktree-path)
                            (and base (worktree-metadata-path base))
                            (and runtime resolved-id
                                 (namestring
                                  (worktree-runtime-path runtime resolved-id))))))
    (when (or resolved-id resolved-branch resolved-path)
      (make-worktree-metadata :id resolved-id
                              :branch resolved-branch
                              :path resolved-path))))

(defun worktree-runtime-path (runtime worktree-id)
  (check-type runtime worktree-runtime)
  (merge-pathnames
   (format nil "~A/" (%worktree-id-path-component worktree-id))
   (worktree-runtime-scratch-root runtime)))

(defun %runtime-for-worktree-source (&key runtime repo-root worktree-path)
  (or runtime
      (and repo-root
           (make-worktree-runtime :repo-root repo-root))
      (let ((derived-root (%derive-worktree-repo-root worktree-path)))
        (and derived-root
             (make-worktree-runtime :repo-root derived-root)))))

(defun %resolve-worktree-runtime-and-metadata (&key runtime
                                                    repo-root
                                                    worktree
                                                    worktree-id
                                                    worktree-path
                                                    worktree-branch)
  (let* ((base (coerce-worktree-metadata :worktree worktree
                                         :worktree-id worktree-id
                                         :worktree-path worktree-path
                                         :worktree-branch worktree-branch))
         (resolved-runtime (%runtime-for-worktree-source
                            :runtime runtime
                            :repo-root repo-root
                            :worktree-path (and base
                                                (worktree-metadata-path base))))
         (resolved-metadata (resolve-worktree-metadata
                             :runtime resolved-runtime
                             :worktree base
                             :worktree-id worktree-id
                             :worktree-path worktree-path
                             :worktree-branch worktree-branch))
         (resolved-repo-root (or repo-root
                                 (and resolved-runtime
                                      (worktree-runtime-repo-root resolved-runtime))
                                 (%derive-worktree-repo-root
                                  (and resolved-metadata
                                       (worktree-metadata-path resolved-metadata))))))
    (values resolved-runtime resolved-metadata resolved-repo-root)))

(defun %find-live-local-worktree (repo-root metadata)
  (when (and repo-root metadata)
    (let* ((live-worktrees (sw4rm-sdk:git-worktree-list repo-root))
           (resolved-path (worktree-metadata-path metadata))
           (resolved-branch (worktree-metadata-branch metadata)))
      (or (and resolved-path
               (find (%worktree-path-namestring resolved-path)
                     live-worktrees
                     :key (lambda (item)
                            (%worktree-path-namestring (getf item :path)))
                     :test #'string=))
          (and resolved-branch
               (find resolved-branch
                     live-worktrees
                     :key (lambda (item)
                            (%strip-live-worktree-branch-ref
                             (getf item :branch)))
                     :test #'string=))
          nil))))

(defun %reconstruct-local-worktree-record (metadata live-record)
  (when (or metadata live-record)
    (list :worktree-id (or (and metadata (worktree-metadata-id metadata))
                           (and live-record
                                (let ((path (getf live-record :path)))
                                  (and path
                                       (file-namestring
                                        (pathname (%worktree-path-namestring path)))))))
          :path (or (and live-record
                         (%worktree-path-namestring (getf live-record :path)))
                    (and metadata (worktree-metadata-path metadata)))
          :branch (or (and live-record
                           (%strip-live-worktree-branch-ref
                            (getf live-record :branch)))
                      (and metadata (worktree-metadata-branch metadata)))
          :state (if live-record :spawned :missing)
          :updated-at (get-universal-time))))

(defun %ensure-worktree-runtime (runtime)
  (check-type runtime worktree-runtime)
  runtime)

(defun spawn-worktree (runtime worktree-id branch &key
                              base-ref
                              worktree
                              workflow-id
                              node-id
                              worktree-path)
  (%ensure-worktree-runtime runtime)
  (if (worktree-runtime-local-p runtime)
      (spawn-local-worktree runtime
                            worktree-id
                            branch
                            :base-ref base-ref
                            :worktree worktree
                            :workflow-id workflow-id
                            :node-id node-id
                            :worktree-path worktree-path)
      (let* ((metadata (resolve-worktree-metadata :runtime runtime
                                                  :worktree worktree
                                                  :worktree-id worktree-id
                                                  :worktree-branch branch
                                                  :worktree-path worktree-path
                                                  :workflow-id workflow-id
                                                  :node-id node-id))
             (resolved-id (and metadata (worktree-metadata-id metadata)))
             (resolved-branch (and metadata (worktree-metadata-branch metadata)))
             (resolved-path (and metadata (worktree-metadata-path metadata))))
        (unless resolved-id
          (error "Unable to resolve a remote worktree id from ~S."
                 (list :worktree worktree
                       :worktree-id worktree-id
                       :worktree-branch branch
                       :workflow-id workflow-id
                       :node-id node-id)))
        (sw4rm-sdk:spawn-worktree (worktree-runtime-coordinator runtime)
                                  resolved-id
                                  resolved-path
                                  resolved-branch
                                  :base-ref (or base-ref "HEAD")))))

(defun collect-worktree (runtime worktree-id &key worktree worktree-path worktree-branch)
  (%ensure-worktree-runtime runtime)
  (if (worktree-runtime-local-p runtime)
      (collect-local-worktree runtime
                              worktree-id
                              :worktree worktree
                              :worktree-path worktree-path
                              :worktree-branch worktree-branch)
      (let* ((metadata (resolve-worktree-metadata :runtime runtime
                                                  :worktree worktree
                                                  :worktree-id worktree-id
                                                  :worktree-path worktree-path
                                                  :worktree-branch worktree-branch))
             (resolved-id (or (and metadata (worktree-metadata-id metadata))
                              (%normalize-worktree-string worktree-id)))
             (collected (sw4rm-sdk:collect-worktree
                         (worktree-runtime-coordinator runtime)
                         resolved-id))
             (record (getf collected :record))
             (live (getf collected :live))
             (status (inspect-worktree runtime
                                       resolved-id
                                       :worktree metadata
                                       :worktree-path worktree-path
                                       :worktree-branch worktree-branch)))
        (list :record (or record
                          (and metadata
                               (list :worktree-id (worktree-metadata-id metadata)
                                     :path (worktree-metadata-path metadata)
                                     :branch (worktree-metadata-branch metadata)
                                     :state :remote)))
              :live live
              :status status))))

(defun inspect-worktree (runtime worktree-id &key
                                worktree
                                worktree-path
                                worktree-branch
                                base-ref)
  (%ensure-worktree-runtime runtime)
  (if (worktree-runtime-local-p runtime)
      (inspect-local-worktree :runtime runtime
                              :worktree worktree
                              :worktree-id worktree-id
                              :worktree-path worktree-path
                              :worktree-branch worktree-branch
                              :base-ref base-ref)
      (let* ((metadata (resolve-worktree-metadata :runtime runtime
                                                  :worktree worktree
                                                  :worktree-id worktree-id
                                                  :worktree-path worktree-path
                                                  :worktree-branch worktree-branch))
             (resolved-id (or (and metadata (worktree-metadata-id metadata))
                              (%normalize-worktree-string worktree-id)))
             (inspection (sw4rm-sdk:inspect-worktree
                          (worktree-runtime-coordinator runtime)
                          resolved-id
                          :worktree-path (and metadata
                                              (worktree-metadata-path metadata))
                          :branch (and metadata
                                       (worktree-metadata-branch metadata)))))
        (append (list :id (or (getf inspection :id)
                              (and metadata (worktree-metadata-id metadata)))
                      :path (or (getf inspection :path)
                                (and metadata (worktree-metadata-path metadata)))
                      :branch (or (getf inspection :branch)
                                  (and metadata (worktree-metadata-branch metadata)))
                      :repo-root (and (worktree-runtime-repo-root runtime)
                                      (%worktree-path-namestring
                                       (worktree-runtime-repo-root runtime)))
                      :backend :remote)
                inspection))))

(defun merge-worktree (&key runtime
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
  (%ensure-worktree-runtime runtime)
  (if (worktree-runtime-local-p runtime)
      (merge-local-worktree :runtime runtime
                            :repo-root repo-root
                            :worktree worktree
                            :worktree-id worktree-id
                            :worktree-path worktree-path
                            :worktree-branch worktree-branch
                            :target-ref target-ref
                            :delete-worktree-p delete-worktree-p
                            :agent-id agent-id
                            :backend backend
                            :task task
                            :result result)
      (sw4rm-sdk:merge-worktree
       (worktree-runtime-coordinator runtime)
       (list :repo-root (and repo-root (%worktree-path-namestring repo-root))
             :worktree (worktree-metadata-plist
                        (resolve-worktree-metadata :runtime runtime
                                                   :worktree worktree
                                                   :worktree-id worktree-id
                                                   :worktree-path worktree-path
                                                   :worktree-branch worktree-branch))
             :worktree-id (%normalize-worktree-string worktree-id)
             :worktree-path (%normalize-worktree-path-string worktree-path)
             :worktree-branch (%normalize-worktree-string worktree-branch)
             :target-ref (%normalize-worktree-string target-ref)
             :delete-worktree-p delete-worktree-p
             :agent-id (%normalize-worktree-string agent-id)
             :backend (or backend :remote)
             :task task
             :result result))))

(defun kill-worktree (runtime worktree-id &key force worktree worktree-path worktree-branch)
  (%ensure-worktree-runtime runtime)
  (if (worktree-runtime-local-p runtime)
      (kill-local-worktree runtime
                           worktree-id
                           :force force
                           :worktree worktree
                           :worktree-path worktree-path
                           :worktree-branch worktree-branch)
      (let* ((metadata (resolve-worktree-metadata :runtime runtime
                                                  :worktree worktree
                                                  :worktree-id worktree-id
                                                  :worktree-path worktree-path
                                                  :worktree-branch worktree-branch))
             (resolved-id (or (and metadata (worktree-metadata-id metadata))
                              (%normalize-worktree-string worktree-id))))
        (sw4rm-sdk:kill-worktree (worktree-runtime-coordinator runtime)
                                 resolved-id
                                 :force force))))

(defun spawn-local-worktree (runtime worktree-id branch &key
                                                       base-ref
                                                       worktree
                                                       workflow-id
                                                       node-id
                                                       worktree-path)
  (check-type runtime worktree-runtime)
  (ignore-errors
    (cleanup-abandoned-local-worktrees :runtime runtime))
  (let* ((metadata (resolve-worktree-metadata :runtime runtime
                                              :worktree worktree
                                              :worktree-id worktree-id
                                              :worktree-branch branch
                                              :worktree-path worktree-path
                                              :workflow-id workflow-id
                                              :node-id node-id))
         (resolved-id (and metadata (worktree-metadata-id metadata)))
         (resolved-branch (and metadata (worktree-metadata-branch metadata)))
         (resolved-path (and metadata (worktree-metadata-path metadata))))
    (unless resolved-id
      (error "Unable to resolve a local worktree id from ~S."
             (list :worktree worktree
                   :worktree-id worktree-id
                   :worktree-branch branch
                   :workflow-id workflow-id
                   :node-id node-id)))
    (%delete-worktree-marker
     (%worktree-abandoned-marker-path runtime resolved-id))
    (unless resolved-branch
      (error "Unable to resolve a local worktree branch from ~S."
             (list :worktree worktree
                   :worktree-id worktree-id
                   :worktree-branch branch
                   :workflow-id workflow-id
                   :node-id node-id)))
    (sw4rm-sdk:spawn-worktree
     (worktree-runtime-coordinator runtime)
     resolved-id
     resolved-path
     resolved-branch
     :base-ref (or base-ref "HEAD"))))

(defun collect-local-worktree (runtime worktree-id &key
                                               worktree
                                               worktree-path
                                               worktree-branch)
  (check-type runtime worktree-runtime)
  (ignore-errors
    (cleanup-abandoned-local-worktrees :runtime runtime))
  (multiple-value-bind (_ metadata repo-root)
      (%resolve-worktree-runtime-and-metadata
       :runtime runtime
       :worktree worktree
       :worktree-id worktree-id
       :worktree-path worktree-path
       :worktree-branch worktree-branch)
    (declare (ignore _))
    (let* ((collected (ignore-errors
                        (sw4rm-sdk:collect-worktree
                         (worktree-runtime-coordinator runtime)
                         (princ-to-string
                          (or (and metadata (worktree-metadata-id metadata))
                              worktree-id)))))
           (record (or (and collected (getf collected :record))
                       (%reconstruct-local-worktree-record
                        metadata
                        nil)))
           (live (or (and collected (getf collected :live))
                     (%find-live-local-worktree repo-root metadata))))
      (when (and live (or (null record)
                          (eq (getf record :state) :missing)))
        (setf record (%reconstruct-local-worktree-record metadata live)))
      (list :record record
            :live live
            :status (inspect-local-worktree
                     :runtime runtime
                     :repo-root repo-root
                     :worktree metadata)))))

(defun kill-local-worktree (runtime worktree-id &key
                                           force
                                           worktree
                                           worktree-path
                                           worktree-branch)
  (check-type runtime worktree-runtime)
  (multiple-value-bind (_ metadata repo-root)
      (%resolve-worktree-runtime-and-metadata
       :runtime runtime
       :worktree worktree
       :worktree-id worktree-id
       :worktree-path worktree-path
       :worktree-branch worktree-branch)
    (declare (ignore _))
    (let* ((resolved-id (or (and metadata (worktree-metadata-id metadata))
                            (and worktree-id
                                 (%normalize-worktree-string worktree-id))))
           (resolved-path (or (and metadata (worktree-metadata-path metadata))
                              (and resolved-id
                                   (%worktree-path-namestring
                                    (worktree-runtime-path runtime
                                                           resolved-id))))))
      (or (ignore-errors
            (sw4rm-sdk:kill-worktree
             (worktree-runtime-coordinator runtime)
             (princ-to-string resolved-id)
             :force force))
          (let* ((resolved-pathname (and resolved-path (pathname resolved-path)))
                 (path-missing-p (and resolved-pathname
                                      (not (probe-file resolved-pathname))))
                 (live (and (not path-missing-p)
                            repo-root
                            metadata
                            (%find-live-local-worktree repo-root metadata))))
            (cond
              (path-missing-p
               (when repo-root
                 (sw4rm-sdk:git-worktree-prune repo-root))
               (when resolved-id
                 (%delete-worktree-marker
                  (%worktree-abandoned-marker-path runtime resolved-id)))
               t)
              ((or live resolved-path)
               (sw4rm-sdk:git-worktree-remove
                repo-root
                (or (and live (%worktree-path-namestring (getf live :path)))
                    resolved-path)
                :force force)
               (sw4rm-sdk:git-worktree-prune repo-root)
               (when resolved-id
                 (%delete-worktree-marker
                  (%worktree-abandoned-marker-path runtime resolved-id)))
               t)))))))
