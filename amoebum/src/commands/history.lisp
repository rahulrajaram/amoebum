(in-package :amoebum.commands.history)

(defparameter +history-command-usage+
  "/history [query...] [--role system|user|assistant|tool] [--tool NAME] [--since TIMESTAMP] [--until TIMESTAMP] [--limit N]")

(defstruct (history-filter-spec
            (:constructor make-history-filter-spec
                (key aliases parser)))
  key
  aliases
  parser)

(defparameter +history-filter-specs+
  (list (make-history-filter-spec :role '("--role" "-r") '%parse-history-role)
        (make-history-filter-spec :tool '("--tool" "-t") '%parse-history-tool)
        (make-history-filter-spec :since '("--since" "-s") '%parse-history-timestamp)
        (make-history-filter-spec :until '("--until" "-u") '%parse-history-timestamp)
        (make-history-filter-spec :limit '("--limit" "-n") '%parse-history-limit)))

(defun %history-filter-key-name (key)
  (string-downcase (symbol-name key)))

(defun %commands-history-normalize-role (value)
  (let ((normalized
          (string-downcase
           (cond
             ((null value) "")
             ((stringp value) value)
             ((symbolp value) (symbol-name value))
             (t (princ-to-string value))))))
    (if (member normalized '("system" "user" "assistant" "tool") :test #'string=)
        normalized
        nil)))

(defun %parse-history-role (value _source)
  (declare (ignore _source))
  (let ((normalized (%commands-history-normalize-role value)))
    (if normalized
        (values normalized nil)
        (values nil (format nil "Invalid role ~S." value)))))

(defun %parse-history-tool (value _source)
  (declare (ignore _source))
  (if (amoebum::%slash-blank-p value)
      (values nil "Tool filter must not be blank.")
      (values (amoebum::%slash-trim value) nil)))

(defun %parse-history-timestamp (value source)
  (let ((trimmed (amoebum::%slash-trim value)))
    (if (amoebum::parse-history-timestamp trimmed)
        (values trimmed nil)
        (values nil (format nil "Invalid timestamp ~S for ~A." value source)))))

(defun %parse-history-limit (value source)
  (let ((trimmed (amoebum::%slash-trim value)))
    (handler-case
        (let ((parsed (parse-integer trimmed)))
          (if (> parsed 0)
              (values parsed nil)
              (values nil (format nil "Limit must be positive, got ~S." value))))
      (error ()
        (values nil (format nil "Invalid integer ~S for ~A." value source))))))

(defun %history-token-key-value (token)
  (when (and (stringp token) (plusp (length token)))
    (let ((separator (or (position #\= token)
                         (position #\: token))))
      (when (and separator
                 (> separator 0)
                 (< (1+ separator) (length token)))
        (values (string-downcase (subseq token 0 separator))
                (amoebum::%slash-trim (subseq token (1+ separator))))))))

(defun %history-spec-for-option-token (token)
  (find-if (lambda (spec)
             (member token (history-filter-spec-aliases spec) :test #'string-equal))
           +history-filter-specs+))

(defun %history-spec-for-filter-key (key)
  (let ((normalized (string-downcase (string-left-trim "-" key))))
    (find-if (lambda (spec)
               (string= normalized
                        (%history-filter-key-name (history-filter-spec-key spec))))
             +history-filter-specs+)))

(defun %history-spec-source-label (spec source-kind)
  (ecase source-kind
    (:option
     (format nil "--~A" (%history-filter-key-name (history-filter-spec-key spec))))
    (:filter
     (format nil "~A filter" (%history-filter-key-name (history-filter-spec-key spec))))))

(defun %history-set-filter (filters key value)
  (let ((next-filters (copy-list filters)))
    (setf (getf next-filters key) value)
    next-filters))

(defun %history-apply-filter (filters spec value source-kind)
  (multiple-value-bind (parsed error)
      (funcall (history-filter-spec-parser spec)
               value
               (%history-spec-source-label spec source-kind))
    (values (if error
                filters
                (%history-set-filter filters
                                     (history-filter-spec-key spec)
                                     parsed))
            error)))

(defun %history-validate-range (filters)
  (let ((since-ts (amoebum::parse-history-timestamp (getf filters :since)))
        (until-ts (amoebum::parse-history-timestamp (getf filters :until))))
    (when (and since-ts until-ts (> since-ts until-ts))
      "Timestamp range is invalid: --since is later than --until.")))

(defun %history-parse-arguments (raw-arguments)
  (let ((tokens (amoebum::%tokenize-command-arguments (or raw-arguments "")))
        (filters (list :query "" :role nil :tool nil :since nil :until nil :limit 20))
        (query-tokens '())
        (errors '())
        (index 0))
    (labels ((next-token ()
               (prog1 (nth index tokens)
                 (incf index)))
             (consume-option-value (spec)
               (let ((value (nth index tokens)))
                 (if (or (null value) (amoebum::%slash-blank-p value))
                     (values nil
                             (format nil "Missing value for --~A."
                                     (%history-filter-key-name
                                      (history-filter-spec-key spec))))
                     (progn
                       (incf index)
                       (values value nil)))))
             (record-filter (spec value source-kind)
               (multiple-value-bind (next-filters error)
                   (%history-apply-filter filters spec value source-kind)
                 (setf filters next-filters)
                 (when error
                   (push error errors)))))
      (loop while (< index (length tokens)) do
        (let ((token (next-token)))
          (cond
            ((%history-spec-for-option-token token)
             (let ((spec (%history-spec-for-option-token token)))
               (multiple-value-bind (value error)
                   (consume-option-value spec)
                 (if error
                     (push error errors)
                     (record-filter spec value :option)))))
            (t
             (multiple-value-bind (key value)
                 (%history-token-key-value token)
               (let ((spec (and key (%history-spec-for-filter-key key))))
                 (if spec
                     (record-filter spec value :filter)
                     (push token query-tokens))))))))
      (let ((range-error (%history-validate-range filters)))
        (when range-error
          (push range-error errors)))
      (setf (getf filters :query)
            (if query-tokens
                (format nil "~{~A~^ ~}" (nreverse query-tokens))
                ""))
      (values filters (nreverse errors)))))

(defun %history-result-block (result ordinal)
  (check-type result amoebum::conversation-history-search-result)
  (let ((entry (amoebum::conversation-history-search-result-entry result))
        (before (amoebum::conversation-history-search-result-before result))
        (after (amoebum::conversation-history-search-result-after result)))
    (with-output-to-string (out)
      (format out "~D. ~A~%" ordinal (amoebum::format-history-entry-line entry))
      (when before
        (format out "   prev: ~A~%" (amoebum::format-history-entry-line before)))
      (when after
        (format out "   next: ~A~%" (amoebum::format-history-entry-line after)))
      (format out "   score: ~D"
              (amoebum::conversation-history-search-result-score result)))))

(defun %history-result-output (results &key role tool query since until limit)
  (if (null results)
      "No conversation history matches the provided filters."
      (with-output-to-string (out)
        (format out "History results (~D):~%" (length results))
        (when (or (and role (plusp (length role)))
                  (and tool (plusp (length tool)))
                  (and (stringp query) (plusp (length (amoebum::%slash-trim query))))
                  since
                  until)
          (format out "Filters:~@[ role=~A~]~@[ tool=~A~]~@[ query=~S~]~@[ since=~A~]~@[ until=~A~]~@[ limit=~D~]~%"
                  role
                  tool
                  (let ((trimmed (amoebum::%slash-trim query)))
                    (and (plusp (length trimmed)) trimmed))
                  since
                  until
                  limit))
        (loop for result in results
              for ordinal from 1 do
                (format out "~A~%" (%history-result-block result ordinal))))))

(defun %history-handler (_invocation arguments context)
  (declare (ignore _invocation))
  (let* ((raw-arguments (or (gethash :ARGS arguments) ""))
         (chat-state (amoebum::slash-command-context-chat-state context))
         (conversation (and (typep chat-state 'amoebum::chat-ui-state)
                            (amoebum::chat-ui-state-conversation chat-state))))
    (unless (typep conversation 'amoebum::conversation-state)
      (return-from %history-handler
        (make-slash-command-result
         :output "Conversation history is unavailable for this session."
         :echo-input-p t)))
    (multiple-value-bind (filters errors)
        (%history-parse-arguments raw-arguments)
      (if errors
          (make-slash-command-result
           :echo-input-p t
           :output (format nil "~{~A~%~}Usage: ~A"
                           errors
                           +history-command-usage+))
          (let* ((query (getf filters :query))
                 (role (getf filters :role))
                 (tool (getf filters :tool))
                 (since (getf filters :since))
                 (until (getf filters :until))
                 (limit (getf filters :limit))
                 (results (amoebum::history-search conversation
                                                   :query query
                                                   :role role
                                                   :tool tool
                                                   :since since
                                                   :until until
                                                   :limit limit)))
            (make-slash-command-result
             :echo-input-p t
             :output (%history-result-output results
                                             :role role
                                             :tool tool
                                             :query query
                                             :since since
                                             :until until
                                             :limit limit)))))))
