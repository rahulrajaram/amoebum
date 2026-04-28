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
                      tool-calls
                      partial
                      tool-call-id)))
  (timestamp (get-universal-time) :type integer)
  (role "assistant" :type string)
  (content "" :type string)
  name
  (tool-calls '() :type list)
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
(defparameter *session-persistence-enabled* t
  "When NIL, skip conversation/checkpoint persistence for transient sessions.")

(defun session-persistence-enabled-p ()
  *session-persistence-enabled*)

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
         (session-path (and (session-persistence-enabled-p)
                            (conversation-session-path resolved-session-id
                                                       :project-root root))))
    (when session-path
      (ensure-directories-exist session-path)
      (ensure-directories-exist
       (conversation-fork-path resolved-session-id
                               resolved-active-fork
                               :project-root root)))
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
