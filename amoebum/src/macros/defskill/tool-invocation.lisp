(in-package :amoebum)

;;;; ---------------------------------------------------------------------------
;;;; Skill tool-invocation plumbing.
;;;;
;;;; Helpers used by built-in skills (`/commit`, `/review`, `/status`, ...) to
;;;; route tool calls through the full permission pipeline and to coerce
;;;; payloads coming back as JSON / plists / hash tables. `EXECUTE-TOOL` and
;;;; `MAKE-AMOEBUM-CONTEXT` are forward references resolved at call time.
;;;;
;;;; Behavior is preserved verbatim from the original
;;;; `amoebum/src/macros/defskill.lisp`; only file boundaries change.
;;;; ---------------------------------------------------------------------------

(defun %skill-plist-entry (value key)
  (cond
    ((hash-table-p value)
     (or (gethash key value)
         (and (keywordp key)
              (gethash (string-downcase (symbol-name key)) value))
         (and (keywordp key)
              (gethash (string-upcase (symbol-name key)) value))
         (and (stringp key)
              (gethash (intern (string-upcase key) :keyword) value))))
    ((and (listp value) (keywordp (first value)))
     (or (getf value key)
         (and (stringp key)
              (getf value (intern (string-upcase key) :keyword)))
         (and (keywordp key)
              (getf value (string-downcase (symbol-name key))))))
    (t
     nil)))

(defun %skill-json->data (value)
  (cond
    ((or (hash-table-p value)
         (and (listp value) (keywordp (first value))))
     value)
    ((stringp value)
     (handler-case
         (jonathan:parse value :as :hash-table)
       (error ()
         value)))
    (t
     value)))

(defun %skill-permission-mode ()
  (let ((cfg (ignore-errors (current-config))))
    (if (typep cfg 'config)
        (config-permission-mode cfg)
        :supervised)))

(defun %skill-tool-arguments (&rest pairs)
  (let ((table (make-hash-table :test #'equal)))
    (loop for (key value) on pairs by #'cddr
          do (setf (gethash (string-downcase
                             (if (symbolp key)
                                 (symbol-name key)
                                 (princ-to-string key)))
                            table)
                   value))
    table))

(defun %skill-invoke-tool (tool-name &optional arguments)
  "Invoke a tool through the full permission pipeline.
Routes through EXECUTE-TOOL (the CLOS generic with :before permission
enforcement) rather than INVOKE-TOOL-CALL (which bypasses permissions)."
  (let* ((payload (or arguments (make-hash-table :test #'equal)))
         (json-arguments (jonathan:to-json payload))
         (tool-call (pseudopod:make-tool-call
                     :name tool-name
                     :arguments json-arguments))
         (context (make-amoebum-context
                   :toolset *toolset*
                   :permission-mode (%skill-permission-mode)
                   :event-bus (and (boundp '*event-bus*) *event-bus*)
                   :hook-registry (and (boundp '*hook-registry*) *hook-registry*)
                   :initialize-notifications-p nil)))
    (execute-tool tool-call context)))

(defun %skill-message->text (message)
  (if (pseudopod:message-p message)
      (with-output-to-string (out)
        (loop for part in (pseudopod:message-content message)
              for index from 0 do
                (when (> index 0)
                  (write-char #\Newline out))
                (let ((type (string-downcase
                             (or (pseudopod:content-part-type part) "text"))))
                  (write-string
                   (cond
                     ((string= type "text")
                      (or (pseudopod:content-part-text part) ""))
                     ((string= type "think")
                      (or (pseudopod:content-part-think part) ""))
                     (t
                      (or (pseudopod:content-part-text part)
                          (pseudopod:content-part-think part)
                          "")))
                   out))))
      (princ-to-string message)))
