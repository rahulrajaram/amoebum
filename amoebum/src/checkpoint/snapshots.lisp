(in-package :amoebum)

(defun %ensure-conversation-for-checkpoint (conversation project-root)
  (or (and (typep conversation 'conversation-state) conversation)
      (conversation-load-latest :project-root project-root)
      (make-conversation-state :project-root project-root)))

(defun list-session-snapshots (&key limit)
  (let ((records '()))
    (dolist (path (%session-snapshot-files))
      (let* ((payload (%read-checkpoint-payload path))
             (snapshot-id (or (and (listp payload) (getf payload :snapshot-id))
                              (pathname-name path)))
             (created-at (%checkpoint-created-at payload path)))
        (push (%checkpoint-session-record snapshot-id
                                          path
                                          created-at
                                          :auto-p nil
                                          :trigger :snapshot)
              records)))
    (%checkpoint-limit-records (nreverse records) limit)))

(defun %resolve-session-snapshot-path (&key snapshot-id snapshot-path)
  (when snapshot-path
    (let ((resolved (probe-file snapshot-path)))
      (when resolved
        (return-from %resolve-session-snapshot-path resolved))))
  (let ((records (list-session-snapshots)))
    (when (null records)
      (return-from %resolve-session-snapshot-path nil))
    (cond
      ((or (null snapshot-id)
           (and (stringp snapshot-id)
                (zerop (length (%checkpoint-trim snapshot-id)))))
       (session-checkpoint-path (first records)))
      ((integerp snapshot-id)
       (let ((index (1- snapshot-id)))
         (and (>= index 0)
              (< index (length records))
              (session-checkpoint-path (nth index records)))))
      ((and (stringp snapshot-id)
            (ignore-errors (parse-integer snapshot-id)))
       (%resolve-session-snapshot-path
        :snapshot-id (parse-integer snapshot-id)))
      ((and (stringp snapshot-id)
            (probe-file snapshot-id))
       (probe-file snapshot-id))
      (t
       (let* ((needle (string-downcase (%checkpoint-trim (princ-to-string snapshot-id))))
              (match
                (find-if (lambda (entry)
                           (let* ((id (string-downcase
                                       (or (session-checkpoint-id entry) "")))
                                  (name (string-downcase
                                         (or (pathname-name
                                              (session-checkpoint-path entry))
                                             ""))))
                             (or (string= needle id)
                                 (string= needle name)
                                 (search needle id :test #'char=))))
                         records)))
         (and match (session-checkpoint-path match)))))))

(defun save-session-snapshot (&key
                                conversation
                                memory-backend
                                project-root
                                snapshot-id
                                (timestamp (get-universal-time)))
  (unless (session-persistence-enabled-p)
    (return-from save-session-snapshot nil))
  (let* ((resolved-project-root (%checkpoint-project-root :project-root project-root))
         (resolved-conversation
           (%ensure-conversation-for-checkpoint conversation resolved-project-root))
         (resolved-memory-backend (or memory-backend (current-memory-backend)))
         (manual-id (let ((trimmed (%checkpoint-trim (or snapshot-id ""))))
                      (and (plusp (length trimmed)) trimmed)))
         (resolved-path (if manual-id
                            (%session-snapshot-path manual-id)
                            (%next-session-snapshot-path :timestamp timestamp)))
         (resolved-id (or manual-id (pathname-name resolved-path)))
         (payload (%session-snapshot-payload
                   :snapshot-id resolved-id
                   :created-at timestamp
                   :project-root resolved-project-root
                   :conversation (%conversation->snapshot resolved-conversation)
                   :memory (%memory->snapshot resolved-memory-backend)
                   :agents (%agents->snapshot))))
    (conversation-save resolved-conversation :save-manifest-p t :save-fork-file-p t)
    (%write-checkpoint-payload resolved-path payload)
    (make-session-checkpoint
     :id resolved-id
     :path resolved-path
     :created-at timestamp
     :auto-p nil
     :trigger :snapshot)))

(defun load-session-snapshot (&key
                                snapshot-id
                                snapshot-path
                                project-root
                                memory-backend)
  (let* ((resolved-project-root (%checkpoint-project-root :project-root project-root))
         (resolved-path (%resolve-session-snapshot-path
                         :snapshot-id snapshot-id
                         :snapshot-path snapshot-path)))
    (unless resolved-path
      (error "Session snapshot ~S not found." (or snapshot-id snapshot-path)))
    (let* ((payload (%read-checkpoint-payload resolved-path))
           (snapshot-id* (or (and (listp payload) (getf payload :snapshot-id))
                             (pathname-name resolved-path)))
           (created-at (%checkpoint-created-at payload resolved-path))
           (snapshot (make-session-checkpoint
                      :id snapshot-id*
                      :path resolved-path
                      :created-at created-at
                      :auto-p nil
                      :trigger :snapshot))
           (project-root*
             (uiop:ensure-directory-pathname
              (or project-root
                  (and (listp payload)
                       (stringp (getf payload :project-root))
                       (pathname (getf payload :project-root)))
                  resolved-project-root)))
           (conversation-snapshot (and (listp payload) (getf payload :conversation)))
           (memory-snapshot (and (listp payload) (getf payload :memory)))
           (agents-snapshot (and (listp payload) (getf payload :agents)))
           (restored-conversation (%conversation-from-snapshot conversation-snapshot
                                                              project-root*)))
      (when (null memory-backend)
        (setf *memory-backend* nil))
      (let ((restored-memory-backend (or memory-backend (current-memory-backend))))
        (%restore-memory-from-snapshot memory-snapshot restored-memory-backend)
        (%restore-agents-from-snapshot agents-snapshot)
        (checkpoint-mark-activity)
        (list :snapshot snapshot
              :conversation restored-conversation
              :memory-backend restored-memory-backend
              :agents (list-agents :include-completed-p t))))))
