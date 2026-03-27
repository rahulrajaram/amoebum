(in-package :amoebum.commands.self-modify)

(defparameter +self-modify-usage+
  "Usage: /self-modify <lisp-form>
       /self-modify approve <id>
       /self-modify deny <id>
       /self-modify edit-approve <id> <lisp-form>
       /self-modify pending
       /self-modify history [limit]
       /self-modify undo")

(defstruct (self-modify-command
            (:constructor make-self-modify-command (action payload)))
  action
  payload)

(defun %self-modify-usage-result ()
  (make-slash-command-result :output +self-modify-usage+))

(defun %split-first-token (text)
  (let* ((trimmed (amoebum::%slash-trim text))
         (space (position #\Space trimmed)))
    (if space
        (values (amoebum::%slash-trim (subseq trimmed 0 space))
                (amoebum::%slash-trim (subseq trimmed (1+ space))))
        (values trimmed ""))))

(defun %parse-self-modify-command (input)
  (let ((trimmed (amoebum::%slash-trim input)))
    (cond
      ((zerop (length trimmed))
       (make-self-modify-command :usage ""))
      ((or (string-equal trimmed "pending")
           (string-equal trimmed "list"))
       (make-self-modify-command :pending ""))
      ((string-equal trimmed "undo")
       (make-self-modify-command :undo ""))
      ((or (string-equal trimmed "history")
           (amoebum::%starts-with-ci-p "history " trimmed))
       (make-self-modify-command
        :history
        (if (string-equal trimmed "history")
            ""
            (amoebum::%slash-trim (subseq trimmed (length "history"))))))
      ((amoebum::%starts-with-ci-p "approve " trimmed)
       (make-self-modify-command
        :approve
        (amoebum::%slash-trim (subseq trimmed (length "approve")))))
      ((amoebum::%starts-with-ci-p "deny " trimmed)
       (make-self-modify-command
        :deny
        (amoebum::%slash-trim (subseq trimmed (length "deny")))))
      ((amoebum::%starts-with-ci-p "edit-approve " trimmed)
       (make-self-modify-command
        :edit-approve
        (amoebum::%slash-trim (subseq trimmed (length "edit-approve")))))
      (t
       (make-self-modify-command :propose trimmed)))))

(defun %render-pending-modifications ()
  (let ((pending (amoebum::pending-modifications)))
    (if pending
        (with-output-to-string (out)
          (format out "Pending self-modifications (~D):~%" (length pending))
          (dolist (entry pending)
            (format out "~A~%"
                    (amoebum::render-modification-approval-widget entry))))
        "No pending self-modifications.")))

(defun %self-modify-handle-pending (_command)
  (make-slash-command-result :output (%render-pending-modifications)))

(defun %self-modify-handle-undo (_command)
  (handler-case
      (let ((entry (amoebum::undo-last-modification)))
        (make-slash-command-result
         :output
         (format nil "Rolled back self-modification ~A."
                 (amoebum::modification-entry-id entry))))
    (error (condition)
      (make-slash-command-result
       :output (format nil "Self-modify undo error: ~A" condition)))))

(defun %self-modify-history-limit (payload)
  (handler-case
      (if (zerop (length payload))
          20
          (max 1 (parse-integer payload)))
    (error ()
      nil)))

(defun %self-modify-handle-history (command)
  (let ((limit (%self-modify-history-limit
                (self-modify-command-payload command))))
    (if limit
        (make-slash-command-result
         :output (amoebum::modification-history-browser :limit limit))
        (make-slash-command-result
         :output
         "Self-modify history expects a numeric limit, e.g. /self-modify history 25."))))

(defun %self-modify-apply-entry (id success-message failure-prefix)
  (let ((entry (amoebum::apply-modification id)))
    (if (eq (amoebum::modification-entry-status entry) :applied)
        (make-slash-command-result
         :output (funcall success-message entry))
        (make-slash-command-result
         :output
         (format nil "~A~A"
                 failure-prefix
                 (or (amoebum::modification-entry-error-message entry)
                     "unknown error"))))))

(defun %self-modify-handle-approve (command)
  (let ((id (self-modify-command-payload command)))
    (if (zerop (length id))
        (%self-modify-usage-result)
        (handler-case
            (progn
              (amoebum::approve-modification id)
              (%self-modify-apply-entry
               id
               (lambda (entry)
                 (format nil
                         "Self-modification ~A approved and applied. Result: ~A"
                         id
                         (or (amoebum::modification-entry-result entry) "NIL")))
               (format nil
                       "Self-modification ~A approval succeeded but apply failed: "
                       id)))
          (error (condition)
            (make-slash-command-result
             :output (format nil "Self-modify approve error: ~A" condition)))))))

(defun %self-modify-handle-deny (command)
  (let ((id (self-modify-command-payload command)))
    (if (zerop (length id))
        (%self-modify-usage-result)
        (handler-case
            (let ((entry (amoebum::deny-modification id)))
              (make-slash-command-result
               :output
               (format nil "Self-modification ~A denied (status: ~A)."
                       id
                       (amoebum::modification-entry-status entry))))
          (error (condition)
            (make-slash-command-result
             :output (format nil "Self-modify deny error: ~A" condition)))))))

(defun %self-modify-handle-edit-approve (command)
  (multiple-value-bind (id edited-form)
      (%split-first-token (self-modify-command-payload command))
    (if (or (zerop (length id))
            (zerop (length edited-form)))
        (%self-modify-usage-result)
        (handler-case
            (progn
              (amoebum::edit-modification id edited-form :approve-p t)
              (%self-modify-apply-entry
               id
               (lambda (_entry)
                 (declare (ignore _entry))
                 (format nil
                         "Self-modification ~A edited, approved, and applied."
                         id))
               (format nil
                       "Self-modification ~A edit+approve failed during apply: "
                       id)))
          (error (condition)
            (make-slash-command-result
             :output (format nil "Self-modify edit-approve error: ~A" condition)))))))

(defun %self-modify-handle-proposal (command)
  (handler-case
      (let ((entry (amoebum::propose-modification
                    (self-modify-command-payload command))))
        (if (eq (amoebum::modification-entry-status entry) :approved)
            (%self-modify-apply-entry
             (amoebum::modification-entry-id entry)
             (lambda (applied)
               (format nil
                       "Self-modification auto-approved and applied (~A)."
                       (amoebum::modification-entry-id applied)))
             (format nil
                     "Self-modification auto-approved (~A) but apply failed: "
                     (amoebum::modification-entry-id entry)))
            (make-slash-command-result
             :output (amoebum::render-modification-approval-widget entry))))
    (error (condition)
      (make-slash-command-result
       :output (format nil "Self-modify proposal error: ~A" condition)))))

(defparameter +self-modify-command-dispatch+
  '((:usage . %self-modify-usage-result)
    (:pending . %self-modify-handle-pending)
    (:undo . %self-modify-handle-undo)
    (:history . %self-modify-handle-history)
    (:approve . %self-modify-handle-approve)
    (:deny . %self-modify-handle-deny)
    (:edit-approve . %self-modify-handle-edit-approve)
    (:propose . %self-modify-handle-proposal)))

(defun %self-modify-dispatch-handler (action)
  (or (cdr (assoc action +self-modify-command-dispatch+))
      #'%self-modify-usage-result))

(defun %self-modify-handler (_invocation arguments _context)
  (declare (ignore _invocation _context))
  (let ((command (%parse-self-modify-command
                  (or (gethash :FORM arguments) ""))))
    (funcall (%self-modify-dispatch-handler
              (self-modify-command-action command))
             command)))
