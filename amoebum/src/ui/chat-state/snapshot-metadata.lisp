(in-package :amoebum)

;;; NXT-431: snapshot/tool metadata formatting extracted from chat-state so
;;; transcript rendering and stream failure presentation share one smaller,
;;; dedicated helper surface.

(defun %tool-display-label (key)
  (string-downcase (substitute #\- #\_ key)))

(defun %normalize-tool-json-key (key)
  (let ((text (cond
                ((stringp key) key)
                ((symbolp key) (symbol-name key))
                (t (princ-to-string key)))))
    (string-downcase
     (substitute #\_ #\-
                 (string-trim '(#\Space #\Tab #\Newline #\Return) text)))))

(defun %tool-json-field (object key)
  (when (hash-table-p object)
    (let ((normalized-target (%normalize-tool-json-key key)))
      (multiple-value-bind (value presentp) (gethash key object)
        (if presentp
            (values value t)
            (loop for existing-key being the hash-keys of object
                  for normalized-key = (%normalize-tool-json-key existing-key)
                  when (string= normalized-key normalized-target)
                    do (return (values (gethash existing-key object) t))
                  finally (return (values nil nil))))))))

(defun %trim-chat-error-text (text)
  (if (stringp text)
      (string-trim '(#\Space #\Tab #\Newline #\Return) text)
      ""))

(defun %parse-json-object-substring (text)
  (when (stringp text)
    (let ((start (position #\{ text)))
      (when start
        (ignore-errors
          (jonathan:parse (subseq text start) :as :hash-table))))))

(defun %stream-failure-provider-message (error-message)
  (let ((payload (%parse-json-object-substring error-message)))
    (when (hash-table-p payload)
      (multiple-value-bind (error-object presentp) (gethash "error" payload)
        (when (and presentp (hash-table-p error-object))
          (multiple-value-bind (message message-present-p)
              (gethash "message" error-object)
            (when (and message-present-p
                       (stringp message)
                       (plusp (length (%trim-chat-error-text message))))
              (%trim-chat-error-text message))))))))

(defun %stream-failure-summary-line (error-message)
  (let ((trimmed (%trim-chat-error-text error-message)))
    (cond
      ((zerop (length trimmed))
       "The provider request failed.")
      ((search "(status=429)" trimmed :test #'char=)
       "Provider request failed with HTTP 429.")
      (t
       trimmed))))

(defun %format-stream-failure-message (error-message)
  (let* ((trimmed (%trim-chat-error-text error-message))
         (provider-message (%stream-failure-provider-message trimmed))
         (summary-line (%stream-failure-summary-line trimmed))
         (retry-guidance
           (if (search "(status=429)" trimmed :test #'char=)
               "Retry your last message in a moment."
               "Review the error and retry when ready.")))
    (with-output-to-string (out)
      (write-string "[Stream failed]" out)
      (terpri out)
      (write-string summary-line out)
      (when (and (stringp provider-message)
                 (plusp (length provider-message))
                 (not (string= provider-message summary-line)))
        (terpri out)
        (write-string provider-message out))
      (terpri out)
      (write-string retry-guidance out))))

(defun %tool-json-scalar-string (value)
  (cond
    ((stringp value) value)
    ((numberp value) (princ-to-string value))
    ((or (eq value t) (eq value :true)) "true")
    ((or (null value) (eq value :false)) "false")
    ((symbolp value) (string-downcase (symbol-name value)))
    (t nil)))

(defun %tool-primary-display-label-p (label)
  (member label
          '("stdout" "output" "text" "content" "message" "summary" "result" "body")
          :test #'string=))

(defun %tool-json-blank-string-p (value)
  (or (null value)
      (and (stringp value)
           (zerop (length (string-trim '(#\Space #\Tab #\Newline #\Return) value))))))

(defun %tool-json-true-p (value)
  (or (eq value t)
      (eq value :true)
      (and (stringp value)
           (member (string-downcase
                    (string-trim '(#\Space #\Tab #\Newline #\Return) value))
                   '("true" "t" "yes" "y" "1")
                   :test #'string=))))

(defun %tool-json-zero-p (value)
  (cond
    ((null value) t)
    ((integerp value) (zerop value))
    ((and (realp value) (not (complexp value))) (zerop value))
    ((stringp value)
     (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) value)))
       (or (string= trimmed "")
           (string= trimmed "0"))))
    (t nil)))

(defun %tool-json-success-status-p (value)
  (or (null value)
      (and (stringp value)
           (member (string-downcase
                    (string-trim '(#\Space #\Tab #\Newline #\Return) value))
                   '("" "completed" "complete" "success" "succeeded" "ok" "done" "finished")
                   :test #'string=))
      (and (symbolp value)
           (member (string-downcase (symbol-name value))
                   '("completed" "complete" "success" "succeeded" "ok" "done" "finished")
                   :test #'string=))))

(defun %tool-json-clean-success-p (value primary-block-p)
  (and primary-block-p
       (multiple-value-bind (stderr stderr-present-p) (%tool-json-field value "stderr")
         (declare (ignore stderr-present-p))
         (%tool-json-blank-string-p stderr))
       (multiple-value-bind (error error-present-p) (%tool-json-field value "error")
         (declare (ignore error-present-p))
         (%tool-json-blank-string-p error))
       (multiple-value-bind (signal signal-present-p) (%tool-json-field value "signal")
         (declare (ignore signal-present-p))
         (%tool-json-zero-p signal))
       (multiple-value-bind (exit-code exit-code-present-p) (%tool-json-field value "exit_code")
         (declare (ignore exit-code-present-p))
         (%tool-json-zero-p exit-code))
       (multiple-value-bind (status status-present-p) (%tool-json-field value "status")
         (declare (ignore status-present-p))
         (%tool-json-success-status-p status))
       (multiple-value-bind (stdout-truncated truncated-present-p)
           (%tool-json-field value "stdout_truncated_p")
         (declare (ignore truncated-present-p))
         (not (%tool-json-true-p stdout-truncated)))
       (multiple-value-bind (stderr-truncated truncated-present-p)
           (%tool-json-field value "stderr_truncated_p")
         (declare (ignore truncated-present-p))
         (not (%tool-json-true-p stderr-truncated)))
       (multiple-value-bind (stdout-omitted omitted-present-p)
           (%tool-json-field value "stdout_omitted_chars")
         (declare (ignore omitted-present-p))
         (%tool-json-zero-p stdout-omitted))
       (multiple-value-bind (stderr-omitted omitted-present-p)
           (%tool-json-field value "stderr_omitted_chars")
         (declare (ignore omitted-present-p))
         (%tool-json-zero-p stderr-omitted))))

(defun %tool-json-display-text (value)
  (cond
    ((and (stringp value) (plusp (length value)))
     value)
    ((vectorp value)
     (let ((items (coerce value 'list)))
       (when (and items (every #'stringp items))
         (format nil "~{~A~^~%~}" items))))
    ((hash-table-p value)
     (let ((blocks '())
           (details '())
           (nested-text nil))
       (labels ((add-block (key)
                  (multiple-value-bind (field presentp) (%tool-json-field value key)
                    (when (and presentp (stringp field) (plusp (length field)))
                      (push (cons (%tool-display-label key) field) blocks))))
                (add-detail (key)
                  (multiple-value-bind (field presentp) (%tool-json-field value key)
                    (when presentp
                      (let ((text (%tool-json-scalar-string field)))
                        (when (and (stringp text) (plusp (length text)))
                          (push (format nil "~A: ~A"
                                        (%tool-display-label key)
                                        text)
                                details)))))))
         (dolist (key '("stdout" "output" "text" "content" "message" "summary" "result" "body"))
           (add-block key))
         (dolist (key '("stderr" "error"))
           (add-block key))
         (let ((clean-success-p (%tool-json-clean-success-p value (not (null blocks)))))
           (unless clean-success-p
             (dolist (key '("exit_code" "status" "signal"))
               (add-detail key)))
           (dolist (key '("stdout_truncated_p" "stderr_truncated_p"
                          "stdout_omitted_chars" "stderr_omitted_chars"))
             (add-detail key)))
         (when (and (null blocks) (null details))
           (dolist (nested-key '("payload" "result" "data"))
             (multiple-value-bind (nested presentp) (%tool-json-field value nested-key)
               (when (and presentp (null nested-text))
                 (setf nested-text (%tool-json-display-text nested)))))))
       (cond
         ((and (stringp nested-text) (plusp (length nested-text)))
          nested-text)
         ((or blocks details)
          (setf blocks (nreverse blocks)
                details (nreverse details))
          (if (and (= (length blocks) 1)
                   (null details)
                   (%tool-primary-display-label-p (caar blocks)))
              (cdar blocks)
              (with-output-to-string (out)
                (loop for (label . block) in blocks
                      for index from 0 do
                        (when (> index 0)
                          (terpri out)
                          (terpri out))
                        (format out "~A:~%~A" label block))
                (when details
                  (when blocks
                    (terpri out)
                    (terpri out))
                  (format out "~{~A~^~%~}" details)))))
         (t nil))))
    (t nil)))
