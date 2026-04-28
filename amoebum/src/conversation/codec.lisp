(in-package :amoebum)

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
   :tool-calls (copy-list (or (pseudopod:message-tool-calls message) '()))
   :partial (pseudopod:message-partial message)
   :tool-call-id (pseudopod:message-tool-call-id message)))

(defun conversation-entry->message (entry)
  (check-type entry conversation-history-entry)
  (pseudopod:make-message
   :role (conversation-history-entry-role entry)
   :name (conversation-history-entry-name entry)
   :content (conversation-history-entry-content entry)
   :tool-calls (copy-list (or (conversation-history-entry-tool-calls entry) '()))
   :tool-call-id (conversation-history-entry-tool-call-id entry)
   :partial (conversation-history-entry-partial entry)))

(defun %conversation-tool-call->sexp (tool-call)
  (let ((hash (pseudopod:tool-call-to-hash tool-call)))
    (when (hash-table-p hash)
      (jonathan:to-json hash))))

(defun %conversation-tool-call-from-sexp (payload)
  (cond
    ((pseudopod:tool-call-p payload)
     payload)
    ((hash-table-p payload)
     (pseudopod:hash-to-tool-call payload))
    ((stringp payload)
     (ignore-errors
       (pseudopod:hash-to-tool-call (jonathan:parse payload :as :hash-table))))
    (t
     nil)))

(defun %conversation-tool-calls->sexp (tool-calls)
  (let ((normalized
          (remove nil
                  (mapcar #'%conversation-tool-call->sexp
                          (or tool-calls '())))))
    (if normalized
        normalized
        nil)))

(defun %conversation-tool-calls-from-sexp (payload)
  (remove nil
          (mapcar #'%conversation-tool-call-from-sexp
                  (cond
                    ((null payload) '())
                    ((listp payload) payload)
                    ((vectorp payload) (coerce payload 'list))
                    (t (list payload))))))

(defun %conversation-entry->sexp (entry)
  (list :timestamp (conversation-history-entry-timestamp entry)
        :role (conversation-history-entry-role entry)
        :content (conversation-history-entry-content entry)
        :name (conversation-history-entry-name entry)
        :tool-calls (%conversation-tool-calls->sexp
                     (conversation-history-entry-tool-calls entry))
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
     :tool-calls (%conversation-tool-calls-from-sexp (getf payload :tool-calls))
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

(defun %conversation-serialize-entries (entries)
  (mapcar #'%conversation-entry->sexp entries))

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

(defun %conversation-state-payload (conversation)
  (amoebum.fp:update
   '()
   (:version 2)
   (:session-id (conversation-state-session-id conversation))
   (:state (conversation-state-state conversation))
   (:created-at (conversation-state-created-at conversation))
   (:updated-at (conversation-state-updated-at conversation))
   (:active-fork (conversation-state-active-fork conversation))
   (:fork-branch-point (conversation-state-fork-branch-point conversation))
   (:forks (copy-tree (conversation-state-forks conversation)))
   (:entries (%conversation-serialize-entries
              (conversation-state-entries conversation)))))
