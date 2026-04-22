(in-package :amoebum)

;;;; Slash-command interface and natural-language candidate extraction.
;;;;
;;;; Decomposed from memory.lisp by NXT-388. Owns the `/memory ...`
;;;; subcommand parser (`run-memory-command`,
;;;; `memory-command-input-p`), the per-action helpers
;;;; (`memory-command-show`, `memory-command-edit`,
;;;; `memory-command-clear`), and the heuristic
;;;; `extract-durable-memory-candidate` /
;;;; `apply-memory-candidate` pair the chat input layer uses to convert
;;;; "remember that ..." / "forget ..." / preference phrases into
;;;; persistent memory edits.

(defparameter *memory-editor-runner* nil)

(defun default-memory-editor-runner (editor path)
  (uiop:run-program (list editor path)
                    :input *standard-input*
                    :output *standard-output*
                    :error-output *error-output*
                    :ignore-error-status t))

(defun memory-command-show (&key (backend (current-memory-backend)))
  (let ((effective (memory-list backend :scope :effective))
        (session (memory-list backend :scope :session)))
    (with-output-to-string (out)
      (format out "Memory backend: ~A~%" (%event-backend-name backend))
      (when (file-memory-backend-p backend)
        (format out "Loaded sources:~%")
        (dolist (line (%memory-source-summary-lines backend))
          (write-line line out)))
      (format out "Effective entries: ~D~%" (length effective))
      (if effective
          (dolist (entry effective)
            (write-line (%memory-entry-display-line entry) out))
          (format out "(none)~%"))
      (format out "Session entries: ~D~%" (length session)))))

(defun memory-command-edit (&key (backend (current-memory-backend)) editor)
  (if (typep backend 'file-memory-backend)
      (let* ((path (file-memory-backend-project-path backend))
             (editor-cmd (or editor
                             (uiop:getenv "AMOEBUM_EDITOR")
                             (uiop:getenv "VISUAL")
                             (uiop:getenv "EDITOR"))))
        (%ensure-memory-file-header path)
        (if (and (stringp editor-cmd) (plusp (length (%trim-text editor-cmd))))
            (progn
              (funcall (or *memory-editor-runner* #'default-memory-editor-runner)
                       editor-cmd
                       (namestring path))
              (format nil "Opened ~A using ~A." (namestring path) editor-cmd))
            (format nil "No editor configured; edit ~A manually." (namestring path))))
      (format nil "Backend ~A does not support /memory edit; use /memory show."
              (memory-backend-kind backend))))

(defun memory-command-clear (&key (backend (current-memory-backend)))
  (let ((cleared (memory-forget backend :scope :session)))
    (format nil "Cleared ~D session memor~:@P." cleared)))

(defun %memory-command-option-value (tokens option)
  (let ((option-equals (format nil "~A=" option)))
    (loop for token in tokens
          for rest on tokens
          do (cond
               ((string-equal token option)
                (return (and (second rest) (%trim-text (second rest)))))
               ((%string-prefix-p-ci option-equals token)
                (return (%trim-text (subseq token (length option-equals)))))))))

(defun %format-memory-import-result (result)
  (if (eq (getf result :status) :error)
      (format nil "~A (~A)."
              (or (getf result :message) "Import failed")
              (or (getf result :reason) :unknown))
      (format nil
              "Import batch ~A finished: imported ~D, skipped ~D, failed ~D. State: ~A~@[. Failures: ~A~]"
              (getf result :import-batch-id)
              (or (getf result :imported) 0)
              (or (getf result :skipped) 0)
              (or (getf result :failed) 0)
              (or (getf result :state-path) "n/a")
              (getf result :failure-log-path))))

(defun %format-memory-export-result (result)
  (if (eq (getf result :status) :error)
      (format nil "~A (~A)."
              (or (getf result :message) "Export failed")
              (or (getf result :reason) :unknown))
      (format nil
              "Exported Haake memory snapshot to ~A (global ~D, project ~D, topic entries ~D across ~D topics)."
              (or (getf result :output-path) "n/a")
              (or (getf result :global-count) 0)
              (or (getf result :project-count) 0)
              (or (getf result :topic-count) 0)
              (or (getf result :topic-scope-count) 0))))

(defun %command-tokens (text)
  (let* ((trimmed (%trim-text text))
         (len (length trimmed))
         (tokens '())
         (start 0))
    (labels ((separatorp (char)
               (member char '(#\Space #\Tab #\Newline #\Return) :test #'char=)))
      (loop for index from 0 to len do
        (if (= index len)
            (when (< start index)
              (push (subseq trimmed start index) tokens))
            (when (separatorp (char trimmed index))
              (when (< start index)
                (push (subseq trimmed start index) tokens))
              (setf start (1+ index)))))
      (nreverse tokens))))

(defun memory-command-input-p (text)
  (let* ((trimmed (%trim-text text))
         (len (length trimmed)))
    (and (>= len 7)
         (string-equal "/memory" trimmed :end2 7)
         (or (= len 7)
             (member (char trimmed 7)
                     '(#\Space #\Tab #\Newline #\Return)
                     :test #'char=)))))

(defun run-memory-command (text &key (backend (current-memory-backend)) editor)
  (let ((trimmed (%trim-text text)))
    (unless (memory-command-input-p trimmed)
      (return-from run-memory-command (values nil nil)))
    (let* ((suffix (%trim-text (subseq trimmed 7)))
           (tokens (%command-tokens suffix))
           (subcommand (if tokens
                           (string-downcase (first tokens))
                           "show"))
           (tail (if tokens
                     (%trim-text (subseq suffix (min (length suffix)
                                                     (length (first tokens)))))
                     "")))
      (case (intern (string-upcase subcommand) :keyword)
        (:SHOW
         (values t (memory-command-show :backend backend)))
        (:EDIT
         (values t (memory-command-edit :backend backend :editor editor)))
        (:CLEAR
         (values t (memory-command-clear :backend backend)))
        (:REMEMBER
         (if (zerop (length tail))
             (values t "Usage: /memory remember <statement>")
             (let ((entry (memory-store backend nil tail :scope :project :source :memory-command)))
               (values t
                       (format nil "Remembered [~A] ~A"
                               (memory-entry-key entry)
                               (memory-entry-value entry))))))
        (:FORGET
         (if (zerop (length tail))
            (values t "Usage: /memory forget <statement-or-key>")
             (if (memory-delete backend tail :scope :project)
                 (values t (format nil "Forgot ~A." (%normalize-memory-key tail)))
                 (values t (format nil "No memory entry matched ~A." (%normalize-memory-key tail))))))
        (:IMPORT
         (let ((target (%memory-command-option-value (rest tokens) "--to")))
           (if (and target (string-equal (%trim-text target) "haake"))
               (values t (%format-memory-import-result (memory-import-to-haake :backend backend)))
               (values t "Usage: /memory import --to haake"))))
        (:EXPORT
         (let ((source (%memory-command-option-value (rest tokens) "--from")))
           (if (and source (string-equal (%trim-text source) "haake"))
               (values t (%format-memory-export-result (memory-export-from-haake :backend backend)))
               (values t "Usage: /memory export --from haake"))))
        (otherwise
         (values t
                 (format nil "Unknown /memory subcommand ~A. Use show|edit|clear|remember|forget|import|export."
                         subcommand)))))))

(defun %extract-after-prefix (text prefix)
  (if (%string-prefix-p-ci prefix text)
      (%trim-text (subseq text (length prefix)))
      nil))

(defun %extract-after-search (text token)
  (let ((pos (search token text :test #'char-equal)))
    (when pos
      (%trim-text (subseq text (+ pos (length token)))))))

(defun extract-durable-memory-candidate (text)
  (let* ((trimmed (%trim-text text))
         (remember-body (or (%extract-after-prefix trimmed "remember that ")
                            (%extract-after-prefix trimmed "remember ")
                            (%extract-after-prefix trimmed "please remember that ")
                            (%extract-after-prefix trimmed "please remember ")))
         (forget-body (or (%extract-after-prefix trimmed "forget ")
                          (%extract-after-prefix trimmed "please forget ")
                          (%extract-after-prefix trimmed "forget the ")))
         (preference-body (or (%extract-after-search trimmed "i always ")
                              (%extract-after-search trimmed "i prefer ")
                              (%extract-after-search trimmed "please always "))))
    (cond
      ((and remember-body (plusp (length remember-body)))
       (make-memory-candidate :kind :remember
                              :text remember-body
                              :key (%normalize-memory-key remember-body)
                              :confidence 0.95d0))
      ((and forget-body (plusp (length forget-body)))
       (let* ((normalized-forget
                (if (%string-suffix-p-ci " preference" forget-body)
                    (%trim-text (subseq forget-body
                                        0
                                        (- (length forget-body)
                                           (length " preference"))))
                    forget-body)))
         (make-memory-candidate :kind :forget
                                :text normalized-forget
                                :key (%normalize-memory-key normalized-forget)
                                :confidence 0.90d0)))
      ((and preference-body (plusp (length preference-body)))
       (make-memory-candidate :kind :preference
                              :text preference-body
                              :key (%normalize-memory-key preference-body)
                              :confidence 0.70d0))
      (t nil))))

(defun apply-memory-candidate (candidate &key (backend (current-memory-backend)))
  (when (memory-candidate-p candidate)
    (case (memory-candidate-kind candidate)
      (:remember
       (let ((entry (memory-store backend
                                  (memory-candidate-key candidate)
                                  (memory-candidate-text candidate)
                                  :scope :project
                                  :source :extracted)))
         (values :stored entry)))
      (:forget
       (if (memory-delete backend (memory-candidate-key candidate) :scope :project)
           (values :deleted (memory-candidate-key candidate))
           (values :not-found (memory-candidate-key candidate))))
      (:preference
       (values :candidate candidate))
      (otherwise
       (values :ignored candidate)))))
