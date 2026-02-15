(in-package :amoebum)

(defparameter +conversation-states+
  '(:idle
    :user-input
    :streaming
    :tool-executing
    :waiting-approval
    :error-recovery))

(defparameter +conversation-state-transitions+
  '((:idle . (:user-input :waiting-approval :error-recovery))
    (:user-input . (:idle :streaming :tool-executing :waiting-approval :error-recovery))
    (:streaming . (:idle :user-input :tool-executing :waiting-approval :error-recovery))
    (:tool-executing . (:idle :user-input :streaming :waiting-approval :error-recovery))
    (:waiting-approval . (:idle :user-input :tool-executing :error-recovery))
    (:error-recovery . (:idle :user-input))))

(define-condition invalid-conversation-transition (error)
  ((from :initarg :from
         :reader invalid-conversation-transition-from)
   (to :initarg :to
       :reader invalid-conversation-transition-to)
   (allowed :initarg :allowed
            :reader invalid-conversation-transition-allowed))
  (:report
   (lambda (condition stream)
     (format stream
             "Invalid conversation transition ~S -> ~S. Allowed: ~{~S~^, ~}."
             (invalid-conversation-transition-from condition)
             (invalid-conversation-transition-to condition)
             (invalid-conversation-transition-allowed condition)))))

(defstruct (conversation-history-entry
            (:constructor make-conversation-history-entry
                (&key (timestamp (get-universal-time))
                      (role "assistant")
                      (content "")
                      name
                      partial
                      tool-call-id)))
  (timestamp (get-universal-time) :type integer)
  (role "assistant" :type string)
  (content "" :type string)
  name
  partial
  tool-call-id)

(defstruct (conversation-history-search-result
            (:constructor make-conversation-history-search-result
                (&key index
                      score
                      entry
                      before
                      after)))
  (index 0 :type fixnum)
  (score 0 :type integer)
  entry
  before
  after)

(defstruct (conversation-state
            (:constructor %make-conversation-state
                (&key session-id
                      (state :idle)
                      (entries '())
                      (created-at (get-universal-time))
                      (updated-at (get-universal-time))
                      (active-fork "main")
                      (fork-branch-point nil)
                      (forks '())
                      project-root
                      session-path)))
  (session-id "" :type string)
  (state :idle)
  (entries '() :type list)
  (created-at (get-universal-time) :type integer)
  (updated-at (get-universal-time) :type integer)
  (active-fork "main" :type string)
  fork-branch-point
  (forks '() :type list)
  project-root
  session-path)

(defparameter +conversation-default-fork-name+ "main")

(defun %conversation-whitespace-char-p (char)
  (member char '(#\Space #\Tab #\Newline #\Return) :test #'char=))

(defun %conversation-trim (text)
  (if (stringp text)
      (string-trim '(#\Space #\Tab #\Newline #\Return) text)
      ""))

(defun %conversation-blank-p (text)
  (let ((trimmed (%conversation-trim text)))
    (zerop (length trimmed))))

(defun %conversation-now ()
  (get-universal-time))

(defun %conversation-default-project-root ()
  (let ((cfg (ignore-errors (current-config))))
    (or (and (config-p cfg)
             (config-project-root cfg))
        (ignore-errors (uiop:getcwd))
        *default-pathname-defaults*)))

(defun %conversation-normalize-project-root (project-root)
  (uiop:ensure-directory-pathname
   (cond
     ((pathnamep project-root)
      project-root)
     ((stringp project-root)
      (pathname project-root))
     ((null project-root)
      (let ((fallback (%conversation-default-project-root)))
        (if (pathnamep fallback)
            fallback
            (pathname (or fallback ".")))))
     (t
      (pathname (princ-to-string project-root))))))

(defun %conversation-generate-session-id ()
  (format nil "~8,'0X-~6,'0X"
          (%conversation-now)
          (random #xFFFFFF)))

(defun %conversation-normalize-state (state)
  (let ((normalized
          (cond
            ((keywordp state) state)
            ((symbolp state) (intern (string-upcase (symbol-name state)) :keyword))
            ((stringp state) (intern (string-upcase state) :keyword))
            (t :idle))))
    (if (member normalized +conversation-states+ :test #'eq)
        normalized
        :idle)))

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

(defun %conversation-copy-entry (entry)
  (check-type entry conversation-history-entry)
  (copy-conversation-history-entry entry))

(defun %conversation-copy-entries (entries)
  (mapcar (lambda (entry)
            (%conversation-copy-entry (%conversation-entry-coerce entry)))
          entries))

(defun %conversation-content-part-text (part)
  (let ((type (string-downcase (or (pseudopod:content-part-type part) "text"))))
    (cond
      ((string= type "text")
       (or (pseudopod:content-part-text part) ""))
      ((string= type "think")
       (or (pseudopod:content-part-think part) ""))
      (t
       (or (pseudopod:content-part-text part)
           (pseudopod:content-part-think part)
           "")))))

(defun %conversation-message-content->text (message)
  (let ((parts (pseudopod:message-content message)))
    (if (null parts)
        ""
        (with-output-to-string (out)
          (loop for part in parts
                for index from 0 do
                  (when (> index 0)
                    (write-char #\Newline out))
                  (write-string (%conversation-content-part-text part) out))))))

(defun conversation-message->entry (message &key timestamp)
  (check-type message pseudopod:message)
  (make-conversation-history-entry
   :timestamp (or timestamp (%conversation-now))
   :role (string-downcase (or (pseudopod:message-role message) "assistant"))
   :content (%conversation-message-content->text message)
   :name (pseudopod:message-name message)
   :partial (pseudopod:message-partial message)
   :tool-call-id (pseudopod:message-tool-call-id message)))

(defun conversation-entry->message (entry)
  (check-type entry conversation-history-entry)
  (pseudopod:make-message
   :role (conversation-history-entry-role entry)
   :name (conversation-history-entry-name entry)
   :content (conversation-history-entry-content entry)
   :tool-call-id (conversation-history-entry-tool-call-id entry)
   :partial (conversation-history-entry-partial entry)))

(defun %conversation-entry->sexp (entry)
  (list :timestamp (conversation-history-entry-timestamp entry)
        :role (conversation-history-entry-role entry)
        :content (conversation-history-entry-content entry)
        :name (conversation-history-entry-name entry)
        :partial (conversation-history-entry-partial entry)
        :tool-call-id (conversation-history-entry-tool-call-id entry)))

(defun %conversation-entry-from-sexp (payload)
  (let ((timestamp (or (getf payload :timestamp) (%conversation-now))))
    (make-conversation-history-entry
     :timestamp (if (integerp timestamp)
                    timestamp
                    (%conversation-now))
     :role (string-downcase
            (princ-to-string
             (or (getf payload :role) "assistant")))
     :content (or (getf payload :content) "")
     :name (let ((name (getf payload :name)))
             (and name (princ-to-string name)))
     :partial (getf payload :partial)
     :tool-call-id (let ((tool-call-id (getf payload :tool-call-id)))
                     (and tool-call-id (princ-to-string tool-call-id))))))

(defun %conversation-entry-coerce (entry)
  (cond
    ((typep entry 'conversation-history-entry)
     entry)
    ((and (listp entry) (keywordp (first entry)))
     (%conversation-entry-from-sexp entry))
    ((pseudopod:message-p entry)
     (conversation-message->entry entry))
    (t
     (make-conversation-history-entry
      :timestamp (%conversation-now)
      :role "assistant"
      :content (princ-to-string entry)))))

(defun conversation-session-directory (&key project-root)
  (merge-pathnames #P".amoebum/session/"
                   (%conversation-normalize-project-root project-root)))

(defun conversation-session-path (session-id &key project-root)
  (let* ((trimmed (%conversation-trim session-id))
         (resolved (if (plusp (length trimmed))
                       trimmed
                       (%conversation-generate-session-id)))
         (filename (format nil "~A.sexp" resolved)))
    (merge-pathnames (pathname filename)
                     (conversation-session-directory :project-root project-root))))

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
    (list :name (%conversation-normalize-fork-name name)
          :branch-point (%conversation-sanitize-branch-point branch-point)
          :message-count (if (and (integerp message-count) (>= message-count 0))
                             message-count
                             0)
          :created-at (if (integerp created-at) created-at now)
          :updated-at (if (integerp updated-at) updated-at now))))

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
    (if (null forks)
        (list normalized-record)
        (let ((existing (find normalized-record forks
                              :test #'%conversation-fork-name-equal
                              :key #'%conversation-fork-record-name)))
          (if existing
              (loop for item in forks
                    collect (if (%conversation-fork-name-equal
                                 (%conversation-fork-record-name item)
                                 (%conversation-fork-record-name normalized-record))
                                normalized-record
                                item))
              (append forks (list normalized-record)))))))

(defun %conversation-normalize-forks (forks active-fork entries)
  (let ((normalized '())
        (active (%conversation-normalize-fork-name active-fork))
        (entry-count (length entries)))
    (dolist (record forks)
      (when (listp record)
        (let* ((name (%conversation-normalize-fork-name (getf record :name)))
               (branch-point (%conversation-sanitize-branch-point
                              (getf record :branch-point)))
               (message-count (if (%conversation-fork-name-equal name active)
                                  entry-count
                                  (or (and (integerp (getf record :message-count))
                                           (max 0 (getf record :message-count)))
                                      0)))
               (created-at (or (getf record :created-at) (%conversation-now)))
               (updated-at (or (getf record :updated-at) (%conversation-now))))
          (setf normalized
                (%conversation-upsert-fork-record
                 normalized
                 (%conversation-fork-record name
                                            :branch-point branch-point
                                            :message-count message-count
                                            :created-at created-at
                                            :updated-at updated-at))))))
    (unless (%conversation-find-fork-record normalized active)
      (setf normalized
            (%conversation-upsert-fork-record
             normalized
             (%conversation-fork-record active
                                        :branch-point (if (%conversation-fork-name-equal
                                                           active
                                                           +conversation-default-fork-name+)
                                                          nil
                                                          (if (plusp entry-count)
                                                              (1- entry-count)
                                                              -1))
                                        :message-count entry-count))))
    normalized))

(defun conversation-active-fork-name (conversation)
  (check-type conversation conversation-state)
  (%conversation-normalize-fork-name
   (conversation-state-active-fork conversation)))

(defun conversation-list-forks (conversation)
  (check-type conversation conversation-state)
  (copy-tree (conversation-state-forks conversation)))

(defun make-conversation-state (&key
                                  project-root
                                  session-id
                                  (state :idle)
                                  entries
                                  (active-fork +conversation-default-fork-name+)
                                  (fork-branch-point nil)
                                  forks)
  (let* ((root (%conversation-normalize-project-root project-root))
         (resolved-session-id (let ((trimmed (%conversation-trim session-id)))
                                (if (plusp (length trimmed))
                                    trimmed
                                    (%conversation-generate-session-id))))
         (now (%conversation-now))
         (resolved-entries (%conversation-copy-entries
                            (mapcar #'%conversation-entry-coerce entries)))
         (resolved-active-fork (%conversation-normalize-fork-name active-fork))
         (resolved-forks (%conversation-normalize-forks forks
                                                        resolved-active-fork
                                                        resolved-entries))
         (session-path (conversation-session-path resolved-session-id
                                                 :project-root root)))
    (ensure-directories-exist session-path)
    (ensure-directories-exist
     (conversation-fork-path resolved-session-id
                             resolved-active-fork
                             :project-root root))
    (%make-conversation-state
     :session-id resolved-session-id
     :state (%conversation-normalize-state state)
     :entries resolved-entries
     :created-at now
     :updated-at now
     :active-fork resolved-active-fork
     :fork-branch-point (%conversation-sanitize-branch-point fork-branch-point)
     :forks resolved-forks
     :project-root root
     :session-path session-path)))

(defun conversation-transition-allowed-p (from to)
  (or (eq from to)
      (let ((allowed (cdr (assoc (%conversation-normalize-state from)
                                 +conversation-state-transitions+
                                 :test #'eq))))
        (member (%conversation-normalize-state to) allowed :test #'eq))))

(defun %conversation-write-sexp-file (path payload)
  (ensure-directories-exist path)
  (with-open-file (stream path
                          :direction :output
                          :if-does-not-exist :create
                          :if-exists :supersede
                          :external-format :utf-8)
    (let ((*print-pretty* t)
          (*print-readably* t))
      (prin1 payload stream)
      (terpri stream)))
  path)

(defun %conversation-read-sexp-file (path)
  (let ((resolved (and path (probe-file path))))
    (unless resolved
      (return-from %conversation-read-sexp-file nil))
    (handler-case
        (with-open-file (stream resolved :direction :input :external-format :utf-8)
          (read stream nil nil))
      (error ()
        nil))))

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

(defun %conversation-state-payload (conversation)
  (list :version 2
        :session-id (conversation-state-session-id conversation)
        :state (conversation-state-state conversation)
        :created-at (conversation-state-created-at conversation)
        :updated-at (conversation-state-updated-at conversation)
        :active-fork (conversation-state-active-fork conversation)
        :fork-branch-point (conversation-state-fork-branch-point conversation)
        :forks (copy-tree (conversation-state-forks conversation))
        :entries (mapcar #'%conversation-entry->sexp
                         (conversation-state-entries conversation))))

(defun conversation-save (conversation
                          &key
                            (save-manifest-p t)
                            (save-fork-file-p t))
  (check-type conversation conversation-state)
  (let* ((root (%conversation-normalize-project-root
                (conversation-state-project-root conversation)))
         (session-id (or (conversation-state-session-id conversation)
                         (%conversation-generate-session-id)))
         (manifest-path (or (conversation-state-session-path conversation)
                            (conversation-session-path session-id :project-root root))))
    (%conversation-refresh-active-fork-record! conversation)
    (let* ((payload (%conversation-state-payload conversation))
           (active-fork (conversation-active-fork-name conversation))
           (fork-path (conversation-fork-path session-id
                                              active-fork
                                              :project-root root)))
      (when save-manifest-p
        (%conversation-write-sexp-file manifest-path payload))
      (when save-fork-file-p
        (%conversation-write-sexp-file fork-path payload))
    (setf (conversation-state-project-root conversation) root
          (conversation-state-session-id conversation) session-id
          (conversation-state-session-path conversation) manifest-path)
      (if save-manifest-p
          manifest-path
          fork-path))))

(defun conversation-transition! (conversation next-state &key reason (save-p t))
  (declare (ignore reason))
  (check-type conversation conversation-state)
  (let* ((from (conversation-state-state conversation))
         (to (%conversation-normalize-state next-state))
         (allowed (cdr (assoc from +conversation-state-transitions+ :test #'eq))))
    (unless (conversation-transition-allowed-p from to)
      (error 'invalid-conversation-transition
             :from from
             :to to
             :allowed allowed))
    (setf (conversation-state-state conversation) to
          (conversation-state-updated-at conversation) (%conversation-now))
    (when save-p
      (conversation-save conversation))
    to))

(defun conversation-state-add-message (conversation message
                                        &key timestamp (save-p t))
  (check-type conversation conversation-state)
  (let* ((entry (if (typep message 'conversation-history-entry)
                    message
                    (conversation-message->entry message :timestamp timestamp)))
         (stamp (conversation-history-entry-timestamp entry)))
    (setf (conversation-state-entries conversation)
          (append (conversation-state-entries conversation)
                  (list entry))
          (conversation-state-updated-at conversation)
          (if (integerp stamp)
              stamp
              (%conversation-now)))
    (when save-p
      (conversation-save conversation))
    entry))

(defun conversation-reset! (conversation &key (save-p t))
  (check-type conversation conversation-state)
  (let ((now (%conversation-now))
        (active +conversation-default-fork-name+))
    (setf (conversation-state-entries conversation) '()
          (conversation-state-state conversation) :idle
          (conversation-state-updated-at conversation) now
          (conversation-state-active-fork conversation) active
          (conversation-state-fork-branch-point conversation) nil
          (conversation-state-forks conversation)
          (%conversation-normalize-forks '() active '()))
    (when save-p
      (conversation-save conversation))
    conversation))

(defun conversation-state-update-entry (conversation index message &key (save-p t))
  (check-type conversation conversation-state)
  (check-type index integer)
  (let* ((entries (conversation-state-entries conversation))
         (existing (nth index entries)))
    (unless existing
      (return-from conversation-state-update-entry nil))
    (let* ((timestamp (conversation-history-entry-timestamp existing))
           (updated-entry
             (if (typep message 'conversation-history-entry)
                 message
                 (conversation-message->entry message :timestamp timestamp)))
           (cell (nthcdr index entries)))
      (setf (car cell) updated-entry
            (conversation-state-updated-at conversation) (%conversation-now))
      (when save-p
        (conversation-save conversation))
      updated-entry)))

(defun conversation-state-messages (conversation)
  (check-type conversation conversation-state)
  (mapcar #'conversation-entry->message
          (conversation-state-entries conversation)))

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

(defun %conversation-session-id-from-path (path payload)
  (or (getf payload :session-id)
      (%conversation-trim (pathname-name path))
      (%conversation-generate-session-id)))

(defun conversation-load (session-path &key project-root)
  (let ((resolved-path (and session-path (probe-file session-path))))
    (unless resolved-path
      (return-from conversation-load nil))
    (let ((payload (%conversation-read-sexp-file resolved-path)))
      (unless (and (listp payload) (keywordp (first payload)))
        (return-from conversation-load nil))
      (let* ((root (%conversation-normalize-project-root
                    (or project-root
                        (make-pathname :name nil :type nil :defaults resolved-path))))
             (session-id (%conversation-session-id-from-path resolved-path payload))
             (manifest-path (conversation-session-path session-id :project-root root))
             (active-fork (%conversation-normalize-fork-name
                           (or (getf payload :active-fork)
                               +conversation-default-fork-name+)))
             (fork-path (conversation-fork-path session-id active-fork
                                                :project-root root))
             (fork-payload
               (and (probe-file fork-path)
                    (%conversation-read-sexp-file fork-path)))
             (effective (if (and (listp fork-payload)
                                 (keywordp (first fork-payload)))
                            fork-payload
                            payload))
             (entries (%conversation-copy-entries
                       (mapcar #'%conversation-entry-coerce
                               (or (getf effective :entries) '()))))
             (created-at (or (getf payload :created-at)
                             (getf effective :created-at)
                             (%conversation-now)))
             (updated-at (or (getf effective :updated-at)
                             (getf payload :updated-at)
                             created-at))
             (forks (%conversation-normalize-forks
                     (or (getf effective :forks)
                         (getf payload :forks))
                     active-fork
                     entries))
             (fork-record (%conversation-find-fork-record forks active-fork))
             (branch-point (or (and fork-record (getf fork-record :branch-point))
                               (getf effective :fork-branch-point)
                               (getf payload :fork-branch-point)))
             (conversation
               (%make-conversation-state
                :session-id session-id
                :state (%conversation-normalize-state
                        (or (getf effective :state) (getf payload :state)))
                :entries entries
                :created-at (if (integerp created-at)
                                created-at
                                (%conversation-now))
                :updated-at (if (integerp updated-at)
                                updated-at
                                (%conversation-now))
                :active-fork active-fork
                :fork-branch-point (%conversation-sanitize-branch-point branch-point)
                :forks forks
                :project-root root
                :session-path manifest-path)))
        conversation))))

(defun %conversation-file-write-date (path)
  (or (ignore-errors (file-write-date path))
      0))

(defun conversation-load-latest (&key project-root)
  (let* ((directory (conversation-session-directory :project-root project-root))
         (pattern (merge-pathnames #P"*.sexp" directory))
         (files (ignore-errors (directory pattern))))
    (when files
      (let ((latest (first (sort (copy-list files)
                                 #'>
                                 :key #'%conversation-file-write-date))))
        (conversation-load latest :project-root project-root)))))

(defun conversation-load-session (session-id &key project-root)
  (let ((session-path (conversation-session-path session-id
                                                 :project-root project-root)))
    (conversation-load session-path :project-root project-root)))

(defun %parse-integer-safe (text)
  (ignore-errors
    (parse-integer text)))

(defun %parse-history-iso-date (text)
  (when (and (= (length text) 10)
             (char= (char text 4) #\-)
             (char= (char text 7) #\-))
    (let ((year (%parse-integer-safe (subseq text 0 4)))
          (month (%parse-integer-safe (subseq text 5 7)))
          (day (%parse-integer-safe (subseq text 8 10))))
      (when (and year month day)
        (ignore-errors
          (encode-universal-time 0 0 0 day month year))))))

(defun %parse-history-iso-datetime (text)
  (let ((value (if (and (plusp (length text))
                        (char-equal (char text (1- (length text))) #\Z))
                   (subseq text 0 (1- (length text)))
                   text)))
    (when (and (>= (length value) 16)
               (char= (char value 4) #\-)
               (char= (char value 7) #\-)
               (member (char value 10) '(#\T #\Space) :test #'char=)
               (char= (char value 13) #\:))
      (let ((year (%parse-integer-safe (subseq value 0 4)))
            (month (%parse-integer-safe (subseq value 5 7)))
            (day (%parse-integer-safe (subseq value 8 10)))
            (hour (%parse-integer-safe (subseq value 11 13)))
            (minute (%parse-integer-safe (subseq value 14 16)))
            (second (if (and (>= (length value) 19)
                             (char= (char value 16) #\:))
                        (%parse-integer-safe (subseq value 17 19))
                        0)))
        (when (and year month day hour minute second)
          (ignore-errors
            (if (and (plusp (length text))
                     (char-equal (char text (1- (length text))) #\Z))
                (encode-universal-time second minute hour day month year 0)
                (encode-universal-time second minute hour day month year))))))))

(defun parse-history-timestamp (value)
  (cond
    ((null value)
     nil)
    ((integerp value)
     value)
    ((stringp value)
     (let ((trimmed (%conversation-trim value)))
       (or (%parse-integer-safe trimmed)
           (%parse-history-iso-datetime trimmed)
           (%parse-history-iso-date trimmed))))
    (t
     (parse-history-timestamp (princ-to-string value)))))

(defun %history-normalize-role (role)
  (let ((normalized
          (string-downcase
           (cond
             ((null role) "")
             ((stringp role) role)
             ((symbolp role) (symbol-name role))
             (t (princ-to-string role))))))
    (if (member normalized '("system" "user" "assistant" "tool") :test #'string=)
        normalized
        nil)))

(defun %history-normalize-tool-name (tool-name)
  (let ((trimmed (%conversation-trim
                  (cond
                    ((null tool-name) "")
                    ((stringp tool-name) tool-name)
                    ((symbolp tool-name) (symbol-name tool-name))
                    (t (princ-to-string tool-name))))))
    (if (plusp (length trimmed))
        (string-downcase trimmed)
        nil)))

(defun %history-tool-name (entry)
  (let ((name (conversation-history-entry-name entry)))
    (and name
         (plusp (length (%conversation-trim name)))
         (string-downcase (%conversation-trim name)))))

(defun %history-content-score (query content)
  (let ((needle (%conversation-trim query))
        (haystack (if (stringp content) content "")))
    (if (zerop (length needle))
        (values t 0)
        (let* ((match-index (search needle haystack :test #'char-equal))
               (prefix-match-p (and match-index (zerop match-index)))
               (exact-match-p (and match-index
                                   (= (length needle)
                                      (length (%conversation-trim haystack))))))
          (if (null match-index)
              (values nil 0)
              (values t
                      (+ 500
                         (if exact-match-p 500 0)
                         (if prefix-match-p 250 0)
                         (max 0 (- 200 (min 200 match-index)))
                         (min 100 (length needle)))))))))

(defun %history-entry-matches-tool-p (entry tool-filter)
  (if (null tool-filter)
      t
      (let ((entry-tool (%history-tool-name entry)))
        (and entry-tool
             (search tool-filter entry-tool :test #'char-equal)))))

(defun %history-search-result-better-p (left right)
  (let* ((left-score (conversation-history-search-result-score left))
         (right-score (conversation-history-search-result-score right)))
    (cond
      ((> left-score right-score)
       t)
      ((< left-score right-score)
       nil)
      (t
       (let* ((left-entry (conversation-history-search-result-entry left))
              (right-entry (conversation-history-search-result-entry right))
              (left-ts (conversation-history-entry-timestamp left-entry))
              (right-ts (conversation-history-entry-timestamp right-entry)))
         (cond
           ((> left-ts right-ts)
            t)
           ((< left-ts right-ts)
            nil)
           (t
            (> (conversation-history-search-result-index left)
               (conversation-history-search-result-index right)))))))))

(defun history-search (conversation
                       &key query role tool since until (limit 20))
  (check-type conversation conversation-state)
  (let* ((resolved-role (%history-normalize-role role))
         (resolved-tool (%history-normalize-tool-name tool))
         (since-ts (parse-history-timestamp since))
         (until-ts (parse-history-timestamp until))
         (max-results (if (and (integerp limit) (> limit 0))
                          limit
                          20))
         (entries (coerce (conversation-state-entries conversation) 'vector))
         (entry-count (length entries))
         (matches '()))
    (when (and since-ts until-ts (> since-ts until-ts))
      (return-from history-search '()))
    (loop for index from 0 below entry-count do
      (let* ((entry (aref entries index))
             (entry-role (string-downcase
                          (or (conversation-history-entry-role entry)
                              "assistant")))
             (stamp (conversation-history-entry-timestamp entry))
             (role-match-p (or (null resolved-role)
                               (string= resolved-role entry-role)))
             (time-match-p (and (or (null since-ts)
                                    (and (integerp stamp) (>= stamp since-ts)))
                                (or (null until-ts)
                                    (and (integerp stamp) (<= stamp until-ts)))))
             (tool-match-p (%history-entry-matches-tool-p entry resolved-tool)))
        (when (and role-match-p time-match-p tool-match-p)
          (multiple-value-bind (content-match-p content-score)
              (%history-content-score query (conversation-history-entry-content entry))
            (when content-match-p
              (let ((score (+ content-score
                              (if resolved-role 60 0)
                              (if resolved-tool 80 0))))
                (push (make-conversation-history-search-result
                       :index index
                       :score score
                       :entry (%conversation-copy-entry entry)
                       :before (and (> index 0)
                                    (%conversation-copy-entry
                                     (aref entries (1- index))))
                       :after (and (< (1+ index) entry-count)
                                   (%conversation-copy-entry
                                    (aref entries (1+ index)))))
                      matches)))))))
    (let ((ranked (sort matches #'%history-search-result-better-p)))
      (subseq ranked 0 (min max-results (length ranked))))))

(defun conversation-search-history (conversation
                                    &key query role tool since until (limit 20))
  (history-search conversation
                  :query query
                  :role role
                  :tool tool
                  :since since
                  :until until
                  :limit limit))

(defun format-history-timestamp (timestamp)
  (if (integerp timestamp)
      (multiple-value-bind (second minute hour day month year)
          (decode-universal-time timestamp)
        (format nil "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D"
                year month day hour minute second))
      "unknown-time"))

(defun format-history-entry-line (entry)
  (check-type entry conversation-history-entry)
  (let* ((role (string-upcase (or (conversation-history-entry-role entry) "assistant")))
         (timestamp (format-history-timestamp
                     (conversation-history-entry-timestamp entry)))
         (content (%conversation-trim (conversation-history-entry-content entry))))
    (format nil "[~A] ~A: ~A"
            timestamp
            role
            (if (%conversation-blank-p content)
                "(empty)"
                content))))
