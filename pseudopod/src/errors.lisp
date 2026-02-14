(in-package :pseudopod)

(define-condition pseudopod-error (error)
  ((message :initarg :message
            :initform nil
            :reader pseudopod-error-message)
   (cause :initarg :cause
          :initform nil
          :reader pseudopod-error-cause))
  (:report (lambda (condition stream)
             (let ((message (pseudopod-error-message condition)))
               (if message
                   (write-string message stream)
                   (write-string "Pseudopod request failed." stream))))))

(define-condition pseudopod-api-error (pseudopod-error)
  ((status-code :initarg :status-code
                :initform nil
                :reader pseudopod-api-error-status-code)
   (body :initarg :body
         :initform nil
         :reader pseudopod-api-error-body)))

(define-condition pseudopod-auth-error (pseudopod-api-error) ())

(define-condition pseudopod-timeout-error (pseudopod-error) ())

(define-condition pseudopod-parse-error (pseudopod-error)
  ((payload :initarg :payload
            :initform nil
            :reader pseudopod-parse-error-payload)))

(defun %first-item (sequence)
  (cond
    ((null sequence) nil)
    ((listp sequence) (first sequence))
    ((vectorp sequence)
     (when (> (length sequence) 0)
       (aref sequence 0)))
    (t nil)))

(defun %sequence->list (sequence)
  (cond
    ((null sequence) nil)
    ((listp sequence) sequence)
    ((vectorp sequence)
     (loop for item across sequence collect item))
    (t nil)))

(defun %content-part-text (part)
  (cond
    ((stringp part) part)
    ((hash-table-p part)
     (or (let ((text (gethash "text" part)))
           (and (stringp text) text))
         (let ((content (gethash "content" part)))
           (and (stringp content) content))))
    (t nil)))

(defun %normalize-content (content)
  (cond
    ((stringp content) content)
    ((or (listp content) (vectorp content))
     (let ((parts (remove nil
                          (mapcar #'%content-part-text
                                  (%sequence->list content)))))
       (when parts
         (apply #'concatenate 'string parts))))
    (t nil)))

(defun extract-content (response)
  "Extract assistant message content from a chat-completion hash-table response."
  (let* ((choices (and (hash-table-p response) (gethash "choices" response)))
         (choice (%first-item choices))
         (message (and (hash-table-p choice) (gethash "message" choice)))
         (content (and (hash-table-p message) (gethash "content" message))))
    (%normalize-content content)))

(defun extract-role (response)
  "Extract assistant message role from a chat-completion hash-table response."
  (let* ((choices (and (hash-table-p response) (gethash "choices" response)))
         (choice (%first-item choices))
         (message (and (hash-table-p choice) (gethash "message" choice)))
         (role (and (hash-table-p message) (gethash "role" message))))
    (and (stringp role) role)))

(defun extract-usage (response)
  "Extract usage hash-table from a chat-completion hash-table response."
  (let ((usage (and (hash-table-p response) (gethash "usage" response))))
    (and (hash-table-p usage) usage)))
