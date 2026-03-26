(in-package :amoebum.commands.index)

(defstruct (index-command-options
            (:constructor make-index-command-options
                (&key refresh-p
                      repo-map-token-target
                      systems)))
  refresh-p
  repo-map-token-target
  systems)

(defun %index-parse-token-target (token)
  (let ((parsed (ignore-errors (parse-integer token :junk-allowed t))))
    (when (and parsed (<= 300 parsed 8000))
      parsed)))

(defun %index-handle-refresh-option (tokens options)
  (declare (ignore tokens))
  (setf (index-command-options-refresh-p options) t)
  options)

(defun %index-handle-token-option (tokens options)
  (let ((candidate (and tokens (pop tokens))))
    (when candidate
      (let ((parsed (%index-parse-token-target candidate)))
        (when parsed
          (setf (index-command-options-repo-map-token-target options) parsed)))))
  options)

(defun %index-handle-system-option (tokens options)
  (let ((system-name (and tokens (pop tokens))))
    (when (and system-name (plusp (length system-name)))
      (setf (index-command-options-systems options)
            (append (index-command-options-systems options)
                    (list system-name)))))
  options)

(defparameter +index-option-dispatch+
  '(("--refresh" . %index-handle-refresh-option)
    ("--tokens" . %index-handle-token-option)
    ("--system" . %index-handle-system-option)))

(defun %index-dispatch-option (token)
  (cdr (assoc token +index-option-dispatch+ :test #'string-equal)))

(defun %parse-index-options (text)
  (let ((tokens (amoebum::%tokenize-command-arguments text))
        (options (make-index-command-options)))
    (loop while tokens do
      (let* ((token (pop tokens))
             (handler (%index-dispatch-option token)))
        (cond
          (handler
           (funcall handler tokens options))
          ((and (plusp (length token))
                (not (uiop:string-prefix-p "--" token)))
           (setf (index-command-options-systems options)
                 (append (index-command-options-systems options)
                         (list token)))))))
    options))

(defun %normalized-index-systems (options)
  (let ((systems (index-command-options-systems options)))
    (and systems (plusp (length systems)) systems)))

(defun %index-output (stats)
  (let ((systems-count (or (getf stats :systems) 0))
        (files-tracked (or (getf stats :files-tracked) 0))
        (files-changed (or (getf stats :files-changed) 0))
        (files-deleted (or (getf stats :files-deleted) 0))
        (entries (or (getf stats :entries) 0))
        (repo-tokens (or (getf stats :repo-map-tokens) 0))
        (reindexed-p (getf stats :reindexed-p)))
    (format nil "~A codebase index (~D system~:P, ~D file~:P tracked, ~D changed, ~D deleted, ~D entries, repo-map ~D tokens)."
            (if reindexed-p "Updated" "Index cache reused")
            systems-count
            files-tracked
            files-changed
            files-deleted
            entries
            repo-tokens)))

(defun %index-handler (_invocation arguments _context)
  (declare (ignore _invocation _context))
  (let* ((options (%parse-index-options (or (gethash :ARGS arguments) "")))
         (repo-map-token-target
           (or (index-command-options-repo-map-token-target options)
               amoebum::+default-repo-map-token-target+)))
    (handler-case
        (multiple-value-bind (index stats)
            (amoebum::ensure-project-codebase-index
             :refresh (index-command-options-refresh-p options)
             :systems (%normalized-index-systems options)
             :repo-map-token-target repo-map-token-target)
          (declare (ignore index))
          (make-slash-command-result :output (%index-output stats)))
      (error (condition)
        (make-slash-command-result
         :output (format nil "Index error: ~A" condition))))))
