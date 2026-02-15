(in-package :amoebum)

(defstruct (slash-command-parameter
            (:constructor make-slash-command-parameter
                (&key name
                 (type :string)
                 (required-p nil)
                 default
                 choices
                 (greedy-p nil)
                 description)))
  name
  (type :string)
  (required-p nil :type boolean)
  default
  choices
  (greedy-p nil :type boolean)
  description)

(defstruct (slash-command
            (:constructor make-slash-command
                (&key name
                 description
                 usage
                 (aliases '())
                 (parameters '())
                 handler
                 completer)))
  name
  description
  usage
  (aliases '() :type list)
  (parameters '() :type list)
  handler
  completer)

(defstruct (slash-command-invocation
            (:constructor make-slash-command-invocation
                (&key input
                 name
                 (arguments-text "")
                 (argument-tokens '()))))
  input
  name
  (arguments-text "" :type string)
  (argument-tokens '() :type list))

(defstruct (slash-command-result
            (:constructor make-slash-command-result
                (&key
                   (handledp t)
                   (echo-input-p t)
                   output
                   (action :none)
                   payload)))
  (handledp t :type boolean)
  (echo-input-p t :type boolean)
  output
  action
  payload)

(defstruct (slash-command-context
            (:constructor make-slash-command-context
                (&key config memory-backend chat-state)))
  config
  memory-backend
  chat-state)

(defparameter *slash-command-registry* (make-hash-table :test #'equal))

(defparameter *memory-command-subcommands*
  '("show" "edit" "clear" "remember" "forget" "import" "export"))

(defun %slash-trim (text)
  (if (stringp text)
      (string-trim '(#\Space #\Tab #\Newline #\Return) text)
      ""))

(defun %slash-blank-p (text)
  (let ((trimmed (%slash-trim text)))
    (zerop (length trimmed))))

(defun %normalize-command-name (name)
  (let* ((raw (if (symbolp name)
                  (symbol-name name)
                  (princ-to-string name)))
         (trimmed (%slash-trim raw))
         (without-slash (if (and (plusp (length trimmed))
                                 (char= (char trimmed 0) #\/))
                            (subseq trimmed 1)
                            trimmed)))
    (string-downcase without-slash)))

(defun %command-name-keyword (name)
  (intern (string-upcase (%normalize-command-name name)) :keyword))

(defun %starts-with-ci-p (prefix text)
  (let ((prefix-len (length prefix))
        (text-len (length text)))
    (and (<= prefix-len text-len)
         (string-equal prefix text :end2 prefix-len))))

(defun %tokenize-command-arguments (text)
  (let ((length (length text))
        (index 0)
        (tokens '()))
    (labels ((peek-next-char ()
               (and (< index length)
                    (char text index)))
             (consume-next-char ()
               (prog1 (peek-next-char)
                 (incf index)))
             (whitespacep (char)
               (member char '(#\Space #\Tab #\Newline #\Return) :test #'char=))
             (skip-whitespace ()
               (loop while (and (< index length)
                                (whitespacep (peek-next-char)))
                     do (incf index)))
             (read-token ()
               (with-output-to-string (out)
                 (let ((quote-char nil))
                   (loop while (< index length) do
                     (let ((char (consume-next-char)))
                       (cond
                         ((and (null quote-char)
                               (whitespacep char))
                          (return))
                         ((and (null quote-char)
                               (member char '(#\" #\') :test #'char=))
                          (setf quote-char char))
                         ((and quote-char
                               (char= char quote-char))
                          (setf quote-char nil))
                         ((and (char= char #\\) (< index length))
                          (write-char (consume-next-char) out))
                         (t
                          (write-char char out)))))))))
      (loop do
        (skip-whitespace)
        (when (>= index length)
          (return))
        (let ((token (read-token)))
          (when (plusp (length token))
            (push token tokens))))
      (nreverse tokens))))

(defun slash-command-input-p (input)
  (let ((trimmed (%slash-trim input)))
    (and (plusp (length trimmed))
         (char= (char trimmed 0) #\/))))

(defun parse-slash-command (input)
  (let ((trimmed (%slash-trim input)))
    (unless (slash-command-input-p trimmed)
      (return-from parse-slash-command nil))
    (let* ((body (subseq trimmed 1))
           (space-pos (position-if (lambda (char)
                                     (member char '(#\Space #\Tab #\Newline #\Return)
                                             :test #'char=))
                                   body))
           (name (if space-pos
                     (subseq body 0 space-pos)
                     body))
           (arguments-text (if space-pos
                               (%slash-trim (subseq body (1+ space-pos)))
                               ""))
           (tokens (%tokenize-command-arguments arguments-text)))
      (if (%slash-blank-p name)
          nil
          (make-slash-command-invocation
           :input trimmed
           :name (%normalize-command-name name)
           :arguments-text arguments-text
           :argument-tokens tokens)))))

(defun clear-slash-commands ()
  (clrhash *slash-command-registry*)
  t)

(defun register-slash-command (command)
  (check-type command slash-command)
  (let ((name (%normalize-command-name (slash-command-name command))))
    (when (%slash-blank-p name)
      (error "Slash command name must not be blank."))
    (setf (gethash name *slash-command-registry*) command)
    (dolist (alias (slash-command-aliases command))
      (let ((alias-name (%normalize-command-name alias)))
        (when (plusp (length alias-name))
          (setf (gethash alias-name *slash-command-registry*) command)))
      command)))

(defun find-slash-command (name)
  (gethash (%normalize-command-name name) *slash-command-registry*))

(defun list-slash-commands ()
  (let ((seen (make-hash-table :test #'equal))
        (commands '()))
    (maphash (lambda (_name command)
               (declare (ignore _name))
               (let ((canonical (%normalize-command-name (slash-command-name command))))
                 (unless (gethash canonical seen)
                   (setf (gethash canonical seen) t)
                   (push command commands))))
             *slash-command-registry*)
    (sort commands #'string<
          :key (lambda (command)
                 (%normalize-command-name (slash-command-name command))))))

(defun %value-matches-choice-p (value choice)
  (or (equal value choice)
      (and (symbolp value)
           (symbolp choice)
           (string-equal (symbol-name value) (symbol-name choice)))
      (and (stringp value)
           (or (and (stringp choice)
                    (string-equal value choice))
               (and (symbolp choice)
                    (string-equal value (symbol-name choice)))))
      (and (symbolp value)
           (stringp choice)
           (string-equal (symbol-name value) choice))))

(defun %match-choice (value choices)
  (loop for choice in choices
        when (%value-matches-choice-p value choice)
          do (return choice)
        finally (return nil)))

(defun %parse-boolean-token (token)
  (cond
    ((or (string-equal token "t")
         (string-equal token "true")
         (string-equal token "yes")
         (string-equal token "on")
         (string-equal token "1"))
     t)
    ((or (string-equal token "nil")
         (string-equal token "false")
         (string-equal token "no")
         (string-equal token "off")
         (string-equal token "0"))
     nil)
    (t
     (error "Expected boolean value but received ~S." token))))

(defun %coerce-argument-token (token parameter)
  (let ((type (slash-command-parameter-type parameter)))
    (case type
      (:string token)
      (:integer
       (handler-case
           (parse-integer token)
         (error ()
           (error "Expected integer for ~A, received ~S."
                  (slash-command-parameter-name parameter)
                  token))))
      (:keyword
       (intern (string-upcase token) :keyword))
      (:boolean
       (%parse-boolean-token token))
      (otherwise
       token))))

(defun parse-slash-command-arguments (command invocation)
  (check-type command slash-command)
  (check-type invocation slash-command-invocation)
  (let ((arguments (make-hash-table :test #'equal))
        (tokens (copy-list (slash-command-invocation-argument-tokens invocation)))
        (errors '()))
    (dolist (parameter (slash-command-parameters command))
      (let* ((name (slash-command-parameter-name parameter))
             (key (%command-name-keyword name))
             (required-p (slash-command-parameter-required-p parameter))
             (greedy-p (slash-command-parameter-greedy-p parameter))
             (default (slash-command-parameter-default parameter))
             (raw-token
               (if greedy-p
                   (prog1
                       (and tokens
                            (with-output-to-string (out)
                              (loop for token in tokens
                                    for index from 0 do
                                      (when (> index 0)
                                        (write-char #\Space out))
                                      (write-string token out))))
                     (setf tokens '()))
                   (prog1 (first tokens)
                     (when tokens
                       (setf tokens (rest tokens)))))))
        (cond
          ((or (null raw-token) (zerop (length (%slash-trim raw-token))))
           (cond
             (required-p
              (push (format nil "Missing required argument ~A." name) errors))
             ((not (null default))
              (setf (gethash key arguments) default))))
          (t
           (handler-case
               (let* ((coerced (%coerce-argument-token raw-token parameter))
                      (choices (slash-command-parameter-choices parameter))
                      (matched (if choices
                                   (%match-choice coerced choices)
                                   coerced)))
                 (when (and choices (null matched))
                   (error "Argument ~A must be one of ~{~A~^, ~}."
                          name
                          (mapcar (lambda (choice)
                                    (if (symbolp choice)
                                        (string-downcase (symbol-name choice))
                                        (princ-to-string choice)))
                                  choices)))
                 (setf (gethash key arguments)
                       (if (and choices matched) matched coerced)))
             (error (condition)
               (push (princ-to-string condition) errors)))))))
    (when tokens
      (push (format nil "Too many arguments for /~A."
                    (slash-command-name command))
            errors))
    (values arguments (nreverse errors))))

(defun %command-usage (command)
  (or (slash-command-usage command)
      (let ((name (%normalize-command-name (slash-command-name command)))
            (parts '()))
        (dolist (parameter (slash-command-parameters command))
          (let* ((token (if (slash-command-parameter-greedy-p parameter)
                            (format nil "<~A...>" (slash-command-parameter-name parameter))
                            (format nil "<~A>" (slash-command-parameter-name parameter))))
                 (formatted (if (slash-command-parameter-required-p parameter)
                                token
                                (format nil "[~A]" token))))
            (push formatted parts)))
        (format nil "/~A~@[ ~{~A~^ ~}~]" name (nreverse parts)))))

(defun %help-listing ()
  (with-output-to-string (out)
    (format out "Available slash commands:~%")
    (dolist (command (list-slash-commands))
      (format out "~A~@[ - ~A~]~%"
              (%command-usage command)
              (slash-command-description command)))))

(defun %help-for-command (topic)
  (let ((command (find-slash-command topic)))
    (if (null command)
        (format nil "Unknown command /~A." topic)
        (with-output-to-string (out)
          (format out "~A~%" (%command-usage command))
          (when (slash-command-description command)
            (format out "~A~%" (slash-command-description command)))
          (when (slash-command-parameters command)
            (format out "Arguments:~%")
            (dolist (parameter (slash-command-parameters command))
              (format out "- ~A (~A)~@[ choices: ~{~A~^, ~}~]~@[ - ~A~]~%"
                      (slash-command-parameter-name parameter)
                      (slash-command-parameter-type parameter)
                      (and (slash-command-parameter-choices parameter)
                           (mapcar (lambda (choice)
                                     (if (symbolp choice)
                                         (string-downcase (symbol-name choice))
                                         (princ-to-string choice)))
                                   (slash-command-parameter-choices parameter)))
                      (slash-command-parameter-description parameter))))))))

(defun %help-handler (_invocation arguments _context)
  (declare (ignore _invocation _context))
  (let ((topic (gethash :TOPIC arguments)))
    (make-slash-command-result
     :output (if (and topic (plusp (length (%slash-trim topic))))
                 (%help-for-command topic)
                 (%help-listing))
     :echo-input-p t)))

(defun %current-config-safe ()
  (or (ignore-errors (current-config))
      (load-config)))

(defun %mode-handler (_invocation arguments context)
  (declare (ignore _invocation))
  (let* ((cfg (or (slash-command-context-config context)
                  (%current-config-safe)))
         (mode (gethash :MODE arguments)))
    (if mode
        (let ((next (setconfig :permission-mode mode)))
          (make-slash-command-result
           :output (format nil "Permission mode set to ~A."
                           (string-downcase (symbol-name next)))))
        (make-slash-command-result
         :output (format nil "Current permission mode: ~A."
                         (string-downcase
                          (symbol-name (config-permission-mode cfg))))))))

(defun %model-handler (_invocation arguments context)
  (declare (ignore _invocation))
  (let* ((cfg (or (slash-command-context-config context)
                  (%current-config-safe)))
         (model (gethash :MODEL arguments)))
    (if (and model (plusp (length (%slash-trim model))))
        (let ((next (setconfig :model model)))
          (make-slash-command-result
           :output (format nil "Model set to ~A." next)))
        (make-slash-command-result
         :output (format nil "Current model: ~A." (config-model cfg))))))

(defun %plan-status-output (active-p output-path)
  (if active-p
      "Plan mode is ON. PLAN MODE -- read-only."
      (if output-path
          (format nil "Plan mode is OFF. Last plan output: ~A." (namestring output-path))
          "Plan mode is OFF.")))

(defun %plan-handler (_invocation arguments _context)
  (declare (ignore _invocation _context))
  (let* ((state (or (gethash :STATE arguments) :toggle))
         (plan-state (current-plan-mode-state))
         (active-p (plan-mode-active-p plan-state)))
    (case state
      (:status
       (make-slash-command-result
        :output (%plan-status-output active-p (plan-mode-state-last-output-path plan-state))))
      (:on
       (if active-p
           (make-slash-command-result
            :output "Plan mode already enabled.")
           (progn
             (enter-plan-mode :state plan-state :clear-steps-p t)
             (setconfig :plan-mode t)
             (make-slash-command-result
              :output "Plan mode enabled. PLAN MODE -- read-only."))))
      (:off
       (if active-p
           (multiple-value-bind (_ output-path)
               (exit-plan-mode :state plan-state :reason :plan-command-exit)
             (declare (ignore _))
             (setconfig :plan-mode nil)
             (make-slash-command-result
              :output (if output-path
                          (format nil "Plan mode disabled. Plan written to ~A."
                                  (namestring output-path))
                          "Plan mode disabled.")))
           (make-slash-command-result
            :output "Plan mode already disabled.")))
      (otherwise
       (if active-p
           (multiple-value-bind (_ output-path)
               (toggle-plan-mode :state plan-state :reason :plan-command-toggle)
             (declare (ignore _))
             (setconfig :plan-mode nil)
             (make-slash-command-result
              :output (if output-path
                          (format nil "Plan mode disabled. Plan written to ~A."
                                  (namestring output-path))
                          "Plan mode disabled.")))
           (progn
             (toggle-plan-mode :state plan-state :reason :plan-command-toggle)
             (setconfig :plan-mode t)
             (make-slash-command-result
              :output "Plan mode enabled. PLAN MODE -- read-only.")))))))

(defun %memory-handler (_invocation arguments context)
  (declare (ignore _invocation))
  (let* ((tail (or (gethash :ARGS arguments) ""))
         (line (if (%slash-blank-p tail)
                   "/memory"
                   (format nil "/memory ~A" (%slash-trim tail))))
         (backend (or (slash-command-context-memory-backend context)
                      (current-memory-backend))))
    (multiple-value-bind (handledp response)
        (run-memory-command line :backend backend)
      (declare (ignore handledp))
      (make-slash-command-result
       :output response
       :echo-input-p t))))

(defun %clear-handler (_invocation _arguments _context)
  (declare (ignore _invocation _arguments _context))
  (make-slash-command-result
   :echo-input-p nil
   :output "Conversation cleared."
   :action :clear-chat))

(defun %compact-handler (_invocation arguments _context)
  (declare (ignore _invocation _context))
  (let ((keep-last-turns (or (gethash :KEEP-LAST arguments) 6)))
    (make-slash-command-result
     :echo-input-p t
     :output nil
     :action :compact-chat
     :payload keep-last-turns)))

(defun %history-normalize-role (value)
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

(defun %history-token-key-value (token)
  (when (and (stringp token) (plusp (length token)))
    (let ((separator (or (position #\= token)
                         (position #\: token))))
      (when (and separator
                 (> separator 0)
                 (< (1+ separator) (length token)))
        (values (string-downcase (subseq token 0 separator))
                (%slash-trim (subseq token (1+ separator))))))))

(defun %history-parse-arguments (raw-arguments)
  (let ((tokens (%tokenize-command-arguments (or raw-arguments "")))
        (query-tokens '())
        (role nil)
        (since nil)
        (limit 20)
        (errors '())
        (index 0))
    (labels ((next-token ()
               (prog1 (nth index tokens)
                 (incf index)))
             (peek-token ()
               (nth index tokens))
             (consume-option-value (name)
               (let ((value (peek-token)))
                 (if (or (null value) (%slash-blank-p value))
                     (push (format nil "Missing value for --~A." name) errors)
                     (progn
                       (incf index)
                       value)))))
      (loop while (< index (length tokens)) do
        (let ((token (next-token)))
          (cond
            ((or (string-equal token "--role")
                 (string-equal token "-r"))
             (let ((value (consume-option-value "role")))
               (when value
                 (let ((normalized (%history-normalize-role value)))
                   (if normalized
                       (setf role normalized)
                       (push (format nil "Invalid role ~S." value) errors))))))
            ((or (string-equal token "--since")
                 (string-equal token "-s"))
             (let ((value (consume-option-value "since")))
               (when value
                 (if (parse-history-timestamp value)
                     (setf since value)
                     (push (format nil "Invalid timestamp ~S for --since." value) errors)))))
            ((or (string-equal token "--limit")
                 (string-equal token "-n"))
             (let ((value (consume-option-value "limit")))
               (when value
                 (handler-case
                     (let ((parsed (parse-integer value)))
                       (if (> parsed 0)
                           (setf limit parsed)
                           (push (format nil "Limit must be positive, got ~S." value)
                                 errors)))
                   (error ()
                     (push (format nil "Invalid integer ~S for --limit." value)
                           errors))))))
            (t
             (multiple-value-bind (key value)
                 (%history-token-key-value token)
               (cond
                 ((and key (string= key "role"))
                  (let ((normalized (%history-normalize-role value)))
                    (if normalized
                        (setf role normalized)
                        (push (format nil "Invalid role ~S." value) errors))))
                 ((and key (string= key "since"))
                  (if (parse-history-timestamp value)
                      (setf since value)
                      (push (format nil "Invalid timestamp ~S for since filter." value)
                            errors)))
                 ((and key (string= key "limit"))
                  (handler-case
                      (let ((parsed (parse-integer value)))
                        (if (> parsed 0)
                            (setf limit parsed)
                            (push (format nil "Limit must be positive, got ~S." value)
                                  errors)))
                    (error ()
                      (push (format nil "Invalid integer ~S for limit filter." value)
                            errors))))
                 (t
                  (push token query-tokens)))))))))
    (values (list :query (if query-tokens
                             (format nil "~{~A~^ ~}" (nreverse query-tokens))
                             "")
                  :role role
                  :since since
                  :limit limit)
            (nreverse errors))))

(defun %history-result-output (entries &key role query since limit)
  (if (null entries)
      "No conversation history matches the provided filters."
      (with-output-to-string (out)
        (format out "History results (~D):~%" (length entries))
        (when (or (and role (plusp (length role)))
                  (and (stringp query) (plusp (length (%slash-trim query))))
                  since)
          (format out "Filters:~@[ role=~A~]~@[ query=~S~]~@[ since=~A~]~@[ limit=~D~]~%"
                  role
                  (let ((trimmed (%slash-trim query)))
                    (and (plusp (length trimmed)) trimmed))
                  since
                  limit))
        (dolist (entry entries)
          (format out "- ~A~%" (format-history-entry-line entry))))))

(defun %history-handler (_invocation arguments context)
  (declare (ignore _invocation))
  (let* ((raw-arguments (or (gethash :ARGS arguments) ""))
         (chat-state (slash-command-context-chat-state context))
         (conversation (and (typep chat-state 'chat-ui-state)
                            (chat-ui-state-conversation chat-state))))
    (unless (typep conversation 'conversation-state)
      (return-from %history-handler
        (make-slash-command-result
         :output "Conversation history is unavailable for this session."
         :echo-input-p t)))
    (multiple-value-bind (filters errors)
        (%history-parse-arguments raw-arguments)
      (if errors
          (make-slash-command-result
           :echo-input-p t
           :output (format nil "~{~A~%~}Usage: /history [query...] [--role ROLE] [--since TIMESTAMP] [--limit N]"
                           errors))
          (let* ((query (getf filters :query))
                 (role (getf filters :role))
                 (since (getf filters :since))
                 (limit (getf filters :limit))
                 (entries (conversation-search-history conversation
                                                      :query query
                                                      :role role
                                                      :since since
                                                      :limit limit)))
            (make-slash-command-result
             :echo-input-p t
             :output (%history-result-output entries
                                             :role role
                                             :query query
                                             :since since
                                             :limit limit)))))))

(defun %agent-status-text (status)
  (if (symbolp status)
      (string-downcase (symbol-name status))
      (princ-to-string status)))

(defun %agent-task-summary (agent)
  (let ((task (%slash-trim (agent-record-task agent))))
    (if (plusp (length task))
        task
        "(no task description)")))

(defun %agents-handler (_invocation _arguments _context)
  (declare (ignore _invocation _arguments _context))
  (let ((running (list-agents :include-completed-p nil)))
    (if (null running)
        (make-slash-command-result
         :output "No running agents.")
        (make-slash-command-result
         :output (with-output-to-string (out)
                   (format out "Running agents (~D):~%" (length running))
                   (dolist (agent running)
                     (format out "- ~A | ~A | ~A~@[ | ~A~]~%"
                             (agent-record-id agent)
                             (%agent-status-text (agent-record-status agent))
                             (string-downcase (symbol-name (agent-record-type agent)))
                             (%agent-task-summary agent))))))))

(defun %agent-output-body (agent output)
  (let* ((trimmed-output (%slash-trim output))
         (result (agent-record-result agent))
         (error-message (agent-record-error-message agent)))
    (cond
      ((plusp (length trimmed-output))
       trimmed-output)
      ((and result (not (null result)))
       (princ-to-string result))
      ((and (stringp error-message)
            (plusp (length (%slash-trim error-message))))
       (format nil "Error: ~A" error-message))
      (t
       "No output captured yet."))))

(defun %agent-handler (_invocation arguments _context)
  (declare (ignore _invocation _context))
  (let* ((agent-id (gethash :ID arguments))
         (action (or (gethash :ACTION arguments) :output))
         (agent (and agent-id (find-agent agent-id))))
    (unless agent
      (return-from %agent-handler
        (make-slash-command-result
         :output (format nil "Unknown agent id ~S." agent-id))))
    (case action
      (:cancel
       (if (cancel-agent agent-id)
           (make-slash-command-result
            :output (format nil "Cancel requested for agent ~A." agent-id))
           (make-slash-command-result
            :output (format nil "Failed to cancel agent ~A." agent-id))))
      (:output
       (multiple-value-bind (output status)
           (agent-output agent-id)
         (declare (ignore status))
         (let ((updated (or (find-agent agent-id) agent)))
           (make-slash-command-result
            :output (format nil "Agent ~A (~A) output:~%~A"
                            agent-id
                            (%agent-status-text (agent-record-status updated))
                            (%agent-output-body updated output))))))
      (otherwise
       (make-slash-command-result
        :output (format nil "Unsupported /agent action ~S." action))))))

(defun %command-name-completions (fragment)
  (let ((prefix (%normalize-command-name fragment)))
    (mapcar (lambda (command)
              (format nil "/~A" (%normalize-command-name (slash-command-name command))))
            (remove-if-not
             (lambda (command)
               (%starts-with-ci-p prefix
                                  (%normalize-command-name (slash-command-name command))))
             (list-slash-commands)))))

(defun %mode-arg-completer (_command _invocation _index fragment _prefix)
  (declare (ignore _command _invocation _index _prefix))
  (let ((prefix (%slash-trim fragment)))
    (loop for mode in *known-permission-modes*
          for text = (string-downcase (symbol-name mode))
          when (%starts-with-ci-p prefix text)
            collect text)))

(defun %help-arg-completer (_command _invocation _index fragment _prefix)
  (declare (ignore _command _invocation _index _prefix))
  (let ((prefix (%normalize-command-name fragment)))
    (loop for command in (list-slash-commands)
          for name = (%normalize-command-name (slash-command-name command))
          when (%starts-with-ci-p prefix name)
            collect name)))

(defun %plan-arg-completer (_command _invocation _index fragment _prefix)
  (declare (ignore _command _invocation _index _prefix))
  (let ((prefix (%slash-trim fragment)))
    (loop for option in '("on" "off" "status")
          when (%starts-with-ci-p prefix option)
            collect option)))

(defun %memory-arg-completer (_command _invocation index fragment prefix-tokens)
  (declare (ignore _command _invocation))
  (if (= index 0)
      (let ((prefix (%slash-trim fragment)))
        (loop for subcommand in *memory-command-subcommands*
              when (%starts-with-ci-p prefix subcommand)
                collect subcommand))
      (let ((head (and prefix-tokens (string-downcase (first prefix-tokens)))))
        (cond
          ((member head '("show" "edit" "clear") :test #'string=)
           '())
          ((string= head "import")
           (cond
             ((= index 1)
              (let ((prefix (%slash-trim fragment)))
                (if (%starts-with-ci-p prefix "--to")
                    '("--to")
                    '())))
             ((= index 2)
              (let ((prefix (%slash-trim fragment)))
                (if (%starts-with-ci-p prefix "haake")
                    '("haake")
                    '())))
             (t
              '())))
          ((string= head "export")
           (cond
             ((= index 1)
              (let ((prefix (%slash-trim fragment)))
                (if (%starts-with-ci-p prefix "--from")
                    '("--from")
                    '())))
             ((= index 2)
              (let ((prefix (%slash-trim fragment)))
                (if (%starts-with-ci-p prefix "haake")
                    '("haake")
                    '())))
             (t
              '())))
          (t
           nil)))))

(defun %agent-id-completions (fragment)
  (let ((prefix (%slash-trim fragment)))
    (loop for agent in (list-agents)
          for id = (agent-record-id agent)
          when (%starts-with-ci-p prefix id)
            collect id)))

(defun %agent-arg-completer (_command _invocation index fragment prefix-tokens)
  (declare (ignore _command _invocation))
  (cond
    ((= index 0)
     (%agent-id-completions fragment))
    ((= index 1)
     (if (and prefix-tokens (plusp (length (%slash-trim (first prefix-tokens)))))
         (let ((prefix (%slash-trim fragment)))
           (loop for option in '("cancel" "output")
                 when (%starts-with-ci-p prefix option)
                   collect option))
         '()))
    (t
     '())))

(defun %completion-arg-state (input)
  (let* ((trimmed (%slash-trim input))
         (body (subseq trimmed 1))
         (space-pos (position-if (lambda (char)
                                   (member char '(#\Space #\Tab #\Newline #\Return)
                                           :test #'char=))
                                 body)))
    (when space-pos
      (let* ((command (subseq body 0 space-pos))
             (arguments (subseq body (1+ space-pos)))
             (trailing-space-p (and (plusp (length input))
                                    (member (char input (1- (length input)))
                                            '(#\Space #\Tab #\Newline #\Return)
                                            :test #'char=)))
             (tokens (%tokenize-command-arguments arguments))
             (index (if trailing-space-p
                        (length tokens)
                        (max 0 (1- (length tokens)))))
             (prefix-tokens (if trailing-space-p
                                tokens
                                (if tokens (butlast tokens) '())))
             (fragment (if trailing-space-p
                           ""
                           (if tokens (car (last tokens)) ""))))
        (list :command (%normalize-command-name command)
              :tokens tokens
              :index index
              :prefix-tokens prefix-tokens
              :fragment fragment
              :arguments arguments)))))

(defun complete-slash-command-input (input)
  (let ((trimmed (%slash-trim input)))
    (unless (slash-command-input-p trimmed)
      (return-from complete-slash-command-input (values nil nil)))
    (let* ((body (subseq trimmed 1))
           (space-pos (position-if (lambda (char)
                                     (member char '(#\Space #\Tab #\Newline #\Return)
                                             :test #'char=))
                                   body)))
      (if (null space-pos)
          (let* ((matches (%command-name-completions body))
                 (sorted (sort (copy-list matches) #'string<)))
            (if (= (length sorted) 1)
                (values (format nil "~A " (first sorted)) sorted)
                (values nil sorted)))
          (let* ((state (%completion-arg-state trimmed))
                 (command (and state (find-slash-command (getf state :command))))
                 (fragment (or (getf state :fragment) ""))
                 (prefix-tokens (or (getf state :prefix-tokens) '()))
                 (index (or (getf state :index) 0))
                 (completer (and command (slash-command-completer command)))
                 (matches
                   (if (functionp completer)
                       (funcall completer command
                                (parse-slash-command trimmed)
                                index
                                fragment
                                prefix-tokens)
                       '()))
                 (sorted (sort (remove-duplicates (copy-list matches) :test #'string-equal)
                               #'string< :key #'string-downcase)))
            (if (and command (= (length sorted) 1))
                (let* ((chosen (first sorted))
                       (prefix (if prefix-tokens
                                   (format nil "~{~A~^ ~} " prefix-tokens)
                                   ""))
                       (replacement
                         (format nil "/~A ~A~A "
                                 (%normalize-command-name (slash-command-name command))
                                 prefix
                                 chosen)))
                  (values replacement sorted))
                (values nil sorted)))))))

(defun dispatch-slash-command (input &key config memory-backend chat-state)
  (let ((invocation (parse-slash-command input)))
    (unless invocation
      (return-from dispatch-slash-command (values nil nil)))
    (let* ((command (find-slash-command (slash-command-invocation-name invocation))))
      (unless command
        (return-from dispatch-slash-command
          (values t
                  (make-slash-command-result
                   :output (format nil "Unknown command /~A. Use /help."
                                   (slash-command-invocation-name invocation))
                   :echo-input-p t))))
      (multiple-value-bind (arguments errors)
          (parse-slash-command-arguments command invocation)
        (if errors
            (values t
                    (make-slash-command-result
                     :output (format nil "~{~A~%~}Usage: ~A"
                                     errors
                                     (%command-usage command))
                     :echo-input-p t))
            (let ((handler (slash-command-handler command))
                  (context (make-slash-command-context
                            :config config
                            :memory-backend memory-backend
                            :chat-state chat-state)))
              (handler-case
                   (let ((result
                           (if (functionp handler)
                               (funcall handler invocation arguments context)
                               (make-slash-command-result
                                :output (format nil "Command /~A has no handler."
                                                (slash-command-invocation-name invocation))))))
                     (values t
                             (cond
                               ((typep result 'slash-command-result) result)
                               ((stringp result)
                                (make-slash-command-result :output result))
                               (t
                                (make-slash-command-result
                                 :output (if result
                                             (princ-to-string result)
                                             nil))))))
                 (error (condition)
                   (values t
                           (make-slash-command-result
                            :output (format nil "Command /~A failed: ~A"
                                            (slash-command-invocation-name invocation)
                                            condition)
                            :echo-input-p t))))))))))

(defun register-builtin-slash-commands ()
  (register-slash-command
   (make-slash-command
    :name "help"
    :description "Show available slash commands or help for one command."
    :usage "/help [command]"
    :parameters
    (list (make-slash-command-parameter
           :name "topic"
           :type :string
           :required-p nil
           :description "Optional command name to describe."))
    :handler #'%help-handler
    :completer #'%help-arg-completer))
  (register-slash-command
   (make-slash-command
    :name "mode"
    :description "Show or set permission mode for this session."
    :usage "/mode [supervised|auto-edit|full-auto|yolo|no-confirm]"
    :parameters
    (list (make-slash-command-parameter
           :name "mode"
           :type :keyword
           :required-p nil
           :choices *known-permission-modes*
           :description "Target permission mode."))
    :handler #'%mode-handler
    :completer #'%mode-arg-completer))
  (register-slash-command
   (make-slash-command
    :name "model"
    :description "Show or set active model."
    :usage "/model [name]"
    :parameters
    (list (make-slash-command-parameter
           :name "model"
           :type :string
           :required-p nil
           :greedy-p t
           :description "Model name to use for this session."))
    :handler #'%model-handler))
  (register-slash-command
   (make-slash-command
    :name "plan"
    :description "Toggle plan mode (read/search only) and persist plan output on exit."
    :usage "/plan [on|off|status]"
    :parameters
    (list (make-slash-command-parameter
           :name "state"
           :type :keyword
           :required-p nil
           :choices '(:on :off :status)
           :description "Optional explicit plan mode action."))
    :handler #'%plan-handler
    :completer #'%plan-arg-completer))
  (register-slash-command
   (make-slash-command
    :name "memory"
    :description "Memory controls: show/edit/clear/remember/forget/import/export."
    :usage "/memory [show|edit|clear|remember <text>|forget <key>|import --to haake|export --from haake]"
    :parameters
    (list (make-slash-command-parameter
           :name "args"
           :type :string
           :required-p nil
           :greedy-p t
           :description "Memory subcommand and arguments."))
    :handler #'%memory-handler
    :completer #'%memory-arg-completer))
  (register-slash-command
   (make-slash-command
    :name "agents"
    :description "List currently running background agents."
    :usage "/agents"
    :handler #'%agents-handler))
  (register-slash-command
   (make-slash-command
    :name "agent"
    :description "Inspect or control a background agent."
    :usage "/agent <id> <cancel|output>"
    :parameters
    (list (make-slash-command-parameter
           :name "id"
           :type :string
           :required-p t
           :description "Agent identifier.")
          (make-slash-command-parameter
           :name "action"
           :type :keyword
           :required-p t
           :choices '(:cancel :output)
           :description "Agent action."))
    :handler #'%agent-handler
    :completer #'%agent-arg-completer))
  (register-slash-command
   (make-slash-command
    :name "clear"
    :description "Clear visible conversation history in the chat buffer."
    :usage "/clear"
    :handler #'%clear-handler))
  (register-slash-command
   (make-slash-command
    :name "compact"
    :description "Compress conversation context by summarizing older messages."
    :usage "/compact [keep-last-turns]"
    :parameters
    (list (make-slash-command-parameter
           :name "keep-last"
           :type :integer
           :required-p nil
           :default 6
           :description "How many recent turns to keep verbatim."))
    :handler #'%compact-handler))
  (register-slash-command
   (make-slash-command
    :name "history"
    :description "Search persisted conversation history by content, role, and timestamp."
    :usage "/history [query...] [--role system|user|assistant|tool] [--since TIMESTAMP] [--limit N]"
    :parameters
    (list (make-slash-command-parameter
           :name "args"
           :type :string
           :required-p nil
           :greedy-p t
           :description "Optional query text and filters."))
    :handler #'%history-handler))
  t)

(register-builtin-slash-commands)
