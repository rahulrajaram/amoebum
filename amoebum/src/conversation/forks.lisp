(in-package :amoebum)

(defun %conversation-normalize-fork-name (name &key (default +conversation-default-fork-name+))
  (let ((trimmed (%conversation-trim
                  (cond
                    ((stringp name) name)
                    ((symbolp name) (symbol-name name))
                    ((null name) "")
                    (t (princ-to-string name))))))
    (if (plusp (length trimmed))
        trimmed
        default)))

(defun %conversation-fork-name-equal (left right)
  (string-equal (%conversation-normalize-fork-name left)
                (%conversation-normalize-fork-name right)))

(defun %conversation-fork-token-char-p (char)
  (or (alphanumericp char)
      (member char '(#\- #\_) :test #'char=)))

(defun %conversation-fork-name->token (name)
  (let ((normalized (%conversation-normalize-fork-name name)))
    (if (zerop (length normalized))
        +conversation-default-fork-name+
        (let ((token
                (with-output-to-string (out)
                  (let ((separator-p nil))
                    (loop for char across normalized do
                      (cond
                        ((%conversation-fork-token-char-p char)
                         (write-char (char-downcase char) out)
                         (setf separator-p nil))
                        ((not separator-p)
                         (write-char #\- out)
                         (setf separator-p t))))))))
          (if (plusp (length token))
              token
              +conversation-default-fork-name+)))))

(defun %conversation-sanitize-branch-point (value)
  (and (integerp value) (>= value -1) value))

(defun conversation-fork-directory (session-id &key project-root)
  (let* ((trimmed (%conversation-trim session-id))
         (resolved (if (plusp (length trimmed))
                       trimmed
                       (%conversation-generate-session-id)))
         (directory-name (format nil "~A/" resolved)))
    (merge-pathnames (pathname directory-name)
                     (conversation-session-directory :project-root project-root))))

(defun conversation-fork-path (session-id fork-name &key project-root)
  (let* ((token (%conversation-fork-name->token fork-name))
         (filename (format nil "fork-~A.sexp" token)))
    (merge-pathnames (pathname filename)
                     (conversation-fork-directory session-id
                                                  :project-root project-root))))

(defun %conversation-fork-record (name &key branch-point message-count created-at updated-at)
  (let ((now (%conversation-now)))
    (amoebum.fp:update
     '()
     (:name (%conversation-normalize-fork-name name))
     (:branch-point (%conversation-sanitize-branch-point branch-point))
     (:message-count (if (and (integerp message-count) (>= message-count 0))
                         message-count
                         0))
     (:created-at (if (integerp created-at) created-at now))
     (:updated-at (if (integerp updated-at) updated-at now)))))

(defun %conversation-fork-record-name (record)
  (%conversation-normalize-fork-name (getf record :name)))

(defun %conversation-find-fork-record (forks fork-name)
  (find (%conversation-normalize-fork-name fork-name)
        forks
        :test #'%conversation-fork-name-equal
        :key #'%conversation-fork-record-name))

(defun %conversation-upsert-fork-record (forks record)
  (let ((normalized-record (%conversation-fork-record
                            (%conversation-fork-record-name record)
                            :branch-point (getf record :branch-point)
                            :message-count (getf record :message-count)
                            :created-at (getf record :created-at)
                            :updated-at (getf record :updated-at))))
    (if (%conversation-find-fork-record forks
                                        (%conversation-fork-record-name normalized-record))
        (mapcar (lambda (item)
                  (if (%conversation-fork-name-equal
                       (%conversation-fork-record-name item)
                       (%conversation-fork-record-name normalized-record))
                      normalized-record
                      item))
                forks)
        (append forks (list normalized-record)))))

(defun conversation-active-fork-name (conversation)
  (check-type conversation conversation-state)
  (%conversation-normalize-fork-name
   (conversation-state-active-fork conversation)))

(defun conversation-list-forks (conversation)
  (check-type conversation conversation-state)
  (copy-tree (conversation-state-forks conversation)))

(defun %conversation-refresh-active-fork-record! (conversation)
  (let* ((active (conversation-active-fork-name conversation))
         (entry-count (length (conversation-state-entries conversation)))
         (existing (%conversation-find-fork-record (conversation-state-forks conversation)
                                                   active))
         (branch-point (or (%conversation-sanitize-branch-point
                            (and existing (getf existing :branch-point)))
                           (%conversation-sanitize-branch-point
                            (conversation-state-fork-branch-point conversation))
                           (if (%conversation-fork-name-equal active
                                                              +conversation-default-fork-name+)
                               nil
                               (if (plusp entry-count)
                                   (1- entry-count)
                                   -1))))
         (created-at (or (and existing (getf existing :created-at))
                         (%conversation-now)))
         (updated-at (or (conversation-state-updated-at conversation)
                         (%conversation-now)))
         (record (%conversation-fork-record active
                                            :branch-point branch-point
                                            :message-count entry-count
                                            :created-at created-at
                                            :updated-at updated-at)))
    (setf (conversation-state-active-fork conversation) active
          (conversation-state-fork-branch-point conversation) branch-point
          (conversation-state-forks conversation)
          (%conversation-upsert-fork-record (conversation-state-forks conversation)
                                            record))
    record))

(defun %conversation-branch-point-from-index (entries message-index)
  (let* ((count (length entries))
         (index (cond
                  ((null message-index)
                   (if (plusp count)
                       (1- count)
                       -1))
                  ((integerp message-index)
                   message-index)
                  ((stringp message-index)
                   (or (ignore-errors (parse-integer (%conversation-trim message-index)))
                       (error "Message index must be an integer, received ~S."
                              message-index)))
                  (t
                   (error "Message index must be an integer, received ~S."
                          message-index)))))
    (cond
      ((zerop count)
       (unless (= index -1)
         (error "Cannot fork empty conversation at index ~S. Use -1 for empty fork."
                index))
       -1)
      ((or (< index -1) (>= index count))
       (error "Fork index ~S out of bounds for conversation length ~D."
              index
              count))
      (t
       index))))

(defun %conversation-fork-entry-prefix (entries branch-point)
  (if (= branch-point -1)
      '()
      (%conversation-copy-entries (subseq entries 0 (1+ branch-point)))))

(defun %conversation-active-fork-record (conversation)
  (%conversation-find-fork-record (conversation-state-forks conversation)
                                  (conversation-active-fork-name conversation)))

(defun fork-conversation (conversation fork-name
                        &key
                          message-index
                          (save-p t)
                          event-bus)
  (check-type conversation conversation-state)
  (let* ((target-name (%conversation-normalize-fork-name fork-name :default nil))
         (source-active (conversation-active-fork-name conversation))
         (entries (conversation-state-entries conversation))
         (branch-point (%conversation-branch-point-from-index entries message-index)))
    (when (%conversation-blank-p target-name)
      (error "Fork name must not be blank."))
    (when (%conversation-find-fork-record (conversation-state-forks conversation) target-name)
      (error "Fork ~S already exists." target-name))
    (%conversation-refresh-active-fork-record! conversation)
    (let* ((now (%conversation-now))
           (fork-entries (%conversation-fork-entry-prefix entries branch-point))
           (fork-record (%conversation-fork-record target-name
                                                   :branch-point branch-point
                                                   :message-count (length fork-entries)
                                                   :created-at now
                                                   :updated-at now))
           (next-forks (%conversation-upsert-fork-record
                        (conversation-state-forks conversation)
                        fork-record))
           (forked (%make-conversation-state
                    :session-id (conversation-state-session-id conversation)
                    :state (conversation-state-state conversation)
                    :entries fork-entries
                    :created-at (conversation-state-created-at conversation)
                    :updated-at now
                    :active-fork target-name
                    :fork-branch-point branch-point
                    :forks next-forks
                    :project-root (conversation-state-project-root conversation)
                    :session-path (conversation-state-session-path conversation))))
      (setf (conversation-state-forks conversation) next-forks
            (conversation-state-active-fork conversation) source-active)
      (when save-p
        (conversation-save conversation :save-manifest-p t :save-fork-file-p t)
        (conversation-save forked :save-manifest-p nil :save-fork-file-p t))
      (let ((bus (or event-bus (ignore-errors (current-event-bus)))))
        (when (event-bus-p bus)
          (publish bus
                   +event-type-conversation-forked+
                   :source :conversation
                   :payload (list :session-id (conversation-state-session-id conversation)
                                  :parent-fork source-active
                                  :child-fork target-name
                                  :branch-point branch-point
                                  :message-count (length fork-entries)))))
      forked)))

(defun conversation-switch-fork (conversation fork-name &key (save-p t))
  (check-type conversation conversation-state)
  (let* ((target-name (%conversation-normalize-fork-name fork-name))
         (current-name (conversation-active-fork-name conversation)))
    (when (%conversation-fork-name-equal target-name current-name)
      (return-from conversation-switch-fork conversation))
    (%conversation-refresh-active-fork-record! conversation)
    (let* ((session-id (conversation-state-session-id conversation))
           (root (conversation-state-project-root conversation))
           (manifest-path (conversation-session-path session-id :project-root root))
           (fork-path (conversation-fork-path session-id target-name
                                              :project-root root))
           (fork-payload (%conversation-read-sexp-file fork-path))
           (manifest-payload (%conversation-read-sexp-file manifest-path))
           (effective-payload
             (or fork-payload
                 (and (%conversation-fork-name-equal target-name
                                                     +conversation-default-fork-name+)
                      manifest-payload)))
           (fork-record
             (or (%conversation-find-fork-record (conversation-state-forks conversation)
                                                 target-name)
                 (let ((fresh (%conversation-fork-record target-name
                                                         :branch-point -1
                                                         :message-count 0)))
                   (setf (conversation-state-forks conversation)
                         (%conversation-upsert-fork-record
                          (conversation-state-forks conversation)
                          fresh))
                   fresh))))
      (unless effective-payload
        (error "Fork ~S has no persisted state at ~A."
               target-name
               (namestring fork-path)))
      (let* ((entries (%conversation-copy-entries
                       (mapcar #'%conversation-entry-coerce
                               (or (getf effective-payload :entries) '()))))
             (resolved-forks (%conversation-normalize-forks
                              (or (getf effective-payload :forks)
                                  (conversation-state-forks conversation))
                              target-name
                              entries))
             (resolved-record (%conversation-find-fork-record resolved-forks target-name))
             (branch-point (or (and resolved-record (getf resolved-record :branch-point))
                               (getf effective-payload :fork-branch-point)
                               (getf fork-record :branch-point)))
             (switched (%make-conversation-state
                        :session-id session-id
                        :state (%conversation-normalize-state
                                (or (getf effective-payload :state)
                                    (conversation-state-state conversation)))
                        :entries entries
                        :created-at (or (getf effective-payload :created-at)
                                        (conversation-state-created-at conversation))
                        :updated-at (or (getf effective-payload :updated-at)
                                        (%conversation-now))
                        :active-fork target-name
                        :fork-branch-point branch-point
                        :forks resolved-forks
                        :project-root root
                        :session-path (conversation-session-path session-id
                                                                 :project-root root))))
        (when save-p
          (conversation-save switched :save-manifest-p t :save-fork-file-p nil))
        switched))))
