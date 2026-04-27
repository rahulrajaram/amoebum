(in-package :amoebum)

;;; NXT-431: normalize slash-command-shaped history entries through a tiny
;;; descriptor parser so history/snapshot helpers stop inlining ad hoc string
;;; handling inside the residual chat-state coordinator.

(defun %parse-chat-slash-command-descriptor (text)
  (let ((invocation (and (stringp text)
                         (parse-slash-command text))))
    (when invocation
      (list :name (slash-command-invocation-name invocation)
            :arguments-text (slash-command-invocation-arguments-text invocation)
            :argument-tokens (copy-list
                              (slash-command-invocation-argument-tokens invocation))))))

(defun %render-chat-slash-command-descriptor (descriptor &key fallback)
  (let ((name (getf descriptor :name))
        (arguments-text (getf descriptor :arguments-text)))
    (cond
      ((and (stringp name) (plusp (length name)))
       (if (and (stringp arguments-text)
                (plusp (length arguments-text)))
           (format nil "/~A ~A" name arguments-text)
           (format nil "/~A" name)))
      ((stringp fallback)
       fallback)
      (t
       ""))))

(defun %coerce-chat-history-entry-content (content)
  (let* ((raw (if (stringp content)
                  content
                  (princ-to-string (or content ""))))
         (descriptor (%parse-chat-slash-command-descriptor raw)))
    (%render-chat-slash-command-descriptor descriptor :fallback raw)))
