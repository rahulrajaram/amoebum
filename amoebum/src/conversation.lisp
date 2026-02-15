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

(defstruct (conversation-state
            (:constructor %make-conversation-state
                (&key session-id
                      (state :idle)
                      (entries '())
                      (created-at (get-universal-time))
                      (updated-at (get-universal-time))
                      project-root
                      session-path)))
  (session-id "" :type string)
  (state :idle)
  (entries '() :type list)
  (created-at (get-universal-time) :type integer)
  (updated-at (get-universal-time) :type integer)
  project-root
  session-path)

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
  (let ((filename (format nil "~A.sexp"
                          (or (%conversation-trim session-id)
                              (%conversation-generate-session-id)))))
    (merge-pathnames (pathname filename)
                     (conversation-session-directory :project-root project-root))))

(defun make-conversation-state (&key project-root session-id (state :idle) entries)
  (let* ((root (%conversation-normalize-project-root project-root))
         (resolved-session-id (let ((trimmed (%conversation-trim session-id)))
                                (if (plusp (length trimmed))
                                    trimmed
                                    (%conversation-generate-session-id))))
         (now (%conversation-now))
         (resolved-entries (mapcar #'%conversation-entry-coerce entries))
         (session-path (conversation-session-path resolved-session-id
                                                 :project-root root)))
    (ensure-directories-exist session-path)
    (%make-conversation-state
     :session-id resolved-session-id
     :state (%conversation-normalize-state state)
     :entries resolved-entries
     :created-at now
     :updated-at now
     :project-root root
     :session-path session-path)))

(defun conversation-transition-allowed-p (from to)
  (or (eq from to)
      (let ((allowed (cdr (assoc (%conversation-normalize-state from)
                                 +conversation-state-transitions+
                                 :test #'eq))))
        (member (%conversation-normalize-state to) allowed :test #'eq))))

(defun conversation-save (conversation)
  (check-type conversation conversation-state)
  (let* ((root (%conversation-normalize-project-root
                (conversation-state-project-root conversation)))
         (session-id (or (conversation-state-session-id conversation)
                         (%conversation-generate-session-id)))
         (path (or (conversation-state-session-path conversation)
                   (conversation-session-path session-id :project-root root)))
         (payload
           (list :version 1
                 :session-id session-id
                 :state (conversation-state-state conversation)
                 :created-at (conversation-state-created-at conversation)
                 :updated-at (conversation-state-updated-at conversation)
                 :entries (mapcar #'%conversation-entry->sexp
                                  (conversation-state-entries conversation)))))
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
    (setf (conversation-state-project-root conversation) root
          (conversation-state-session-id conversation) session-id
          (conversation-state-session-path conversation) path)
    path))

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

(defun conversation-load (session-path &key project-root)
  (let ((resolved-path (and session-path (probe-file session-path))))
    (unless resolved-path
      (return-from conversation-load nil))
    (handler-case
        (with-open-file (stream resolved-path
                                :direction :input
                                :external-format :utf-8)
          (let ((payload (read stream nil nil)))
            (when (and (listp payload)
                       (keywordp (first payload)))
              (let* ((root (%conversation-normalize-project-root
                            (or project-root
                                (make-pathname :name nil
                                               :type nil
                                               :defaults resolved-path))))
                     (session-id (or (getf payload :session-id)
                                     (%conversation-trim
                                      (pathname-name resolved-path))
                                     (%conversation-generate-session-id)))
                     (state (%conversation-normalize-state (getf payload :state)))
                     (created-at (or (getf payload :created-at)
                                     (%conversation-now)))
                     (updated-at (or (getf payload :updated-at)
                                     created-at))
                     (entries (mapcar #'%conversation-entry-coerce
                                      (or (getf payload :entries) '())))
                     (conversation
                       (%make-conversation-state
                        :session-id session-id
                        :state state
                        :entries entries
                        :created-at (if (integerp created-at)
                                        created-at
                                        (%conversation-now))
                        :updated-at (if (integerp updated-at)
                                        updated-at
                                        (%conversation-now))
                        :project-root root
                        :session-path resolved-path)))
                conversation))))
      (error ()
        nil))))

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

(defun %history-content-matches-p (query content)
  (let ((needle (%conversation-trim query)))
    (if (zerop (length needle))
        t
        (and (stringp content)
             (search needle content :test #'char-equal)))))

(defun conversation-search-history (conversation
                                    &key query role since (limit 20))
  (check-type conversation conversation-state)
  (let* ((resolved-role (%history-normalize-role role))
         (since-ts (parse-history-timestamp since))
         (max-results (if (and (integerp limit) (> limit 0))
                          limit
                          20))
         (entries (reverse (copy-list (conversation-state-entries conversation))))
         (matches
           (loop for entry in entries
                 for entry-role = (string-downcase
                                   (or (conversation-history-entry-role entry)
                                       "assistant"))
                 for stamp = (conversation-history-entry-timestamp entry)
                 for content = (conversation-history-entry-content entry)
                 when (and (or (null resolved-role)
                               (string= resolved-role entry-role))
                           (or (null since-ts)
                               (and (integerp stamp) (>= stamp since-ts)))
                           (%history-content-matches-p query content))
                   collect entry)))
    (subseq matches 0 (min (length matches) max-results))))

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
