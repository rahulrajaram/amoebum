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
;; Defined in src/macros/deftool.lisp; declared here for compile/load order.
(defvar *tool-metadata*)
(defvar *tool-history*)

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

(defun %config-key-sort-token (key)
  (string-downcase
   (cond
     ((keywordp key) (symbol-name key))
     ((symbolp key) (symbol-name key))
     (t (princ-to-string key)))))

(defun %config-layer-label (layer)
  (case layer
    (:built-in "built-in")
    (:global "global")
    (:project "project")
    (:directory "directory")
    (:env "env")
    (:cli "cli")
    (:runtime "runtime")
    (otherwise "unknown")))

(defun %config-report-output (cfg)
  (let ((keys (sort (loop for key being the hash-keys of (config-values cfg)
                          collect key)
                    #'string<
                    :key #'%config-key-sort-token)))
    (with-output-to-string (out)
      (format out "Configuration values:~%")
      (dolist (key keys)
        (format out "- ~A = ~S (source: ~A)~%"
                (%config-key-sort-token key)
                (config-value key cfg)
                (%config-layer-label (config-layer-source key cfg)))))))

(defun %config-handler (_invocation _arguments context)
  (declare (ignore _invocation _arguments))
  (let ((cfg (or (slash-command-context-config context)
                 (%current-config-safe))))
    (make-slash-command-result
     :output (%config-report-output cfg)
     :echo-input-p t)))

(defun %plan-review-decision-label (decision)
  (case decision
    (:approved "approved")
    (:partially-approved "partially-approved")
    (:rejected "rejected")
    (:modification-requested "modification-requested")
    (:pending "pending")
    (otherwise
     (string-downcase (symbol-name (or decision :pending))))))

(defun %format-step-index-list (step-indexes)
  (if step-indexes
      (format nil "~{~D~^, ~}" step-indexes)
      "none"))

(defun %split-delimited (text delimiter)
  (let ((parts '())
        (start 0)
        (length (length text)))
    (loop for index from 0 to length do
      (when (or (= index length)
                (char= (char text index) delimiter))
        (push (subseq text start index) parts)
        (setf start (1+ index))))
    (nreverse parts)))

(defun %digit-string-p (text)
  (and (stringp text)
       (plusp (length text))
       (loop for char across text
             always (digit-char-p char))))

(defun %parse-step-index-fragment (fragment)
  (let ((trimmed (%slash-trim fragment)))
    (cond
      ((zerop (length trimmed))
       nil)
      ((%digit-string-p trimmed)
       (let ((value (parse-integer trimmed)))
         (when (>= value 1)
           (list value))))
      ((find #\- trimmed)
       (let* ((parts (%split-delimited trimmed #\-))
              (from-text (and (= (length parts) 2)
                              (%slash-trim (first parts))))
              (to-text (and (= (length parts) 2)
                            (%slash-trim (second parts)))))
         (when (and from-text
                    to-text
                    (%digit-string-p from-text)
                    (%digit-string-p to-text))
           (let ((from (parse-integer from-text))
                 (to (parse-integer to-text)))
             (when (and (>= from 1)
                        (>= to from))
               (loop for value from from to to
                     collect value))))))
      (t
       nil))))

(defun %parse-step-index-token (token)
  (let ((fragments (%split-delimited token #\,))
        (result '()))
    (dolist (fragment fragments)
      (let ((parsed (%parse-step-index-fragment fragment)))
        (unless parsed
          (return-from %parse-step-index-token nil))
        (setf result (append result parsed))))
    result))

(defun %parse-plan-step-approval-args (raw-args)
  (let ((tokens (%tokenize-command-arguments (or raw-args ""))))
    (if (null tokens)
        (values nil nil nil)
        (let ((indexes '())
              (invalid-tokens '())
              (saw-step-marker-p nil))
          (dolist (token tokens)
            (cond
              ((or (string-equal token "step")
                   (string-equal token "steps"))
               (setf saw-step-marker-p t))
              (t
               (let ((parsed (%parse-step-index-token token)))
                 (if parsed
                     (setf indexes (append indexes parsed))
                     (push token invalid-tokens))))))
          (cond
            ((and saw-step-marker-p invalid-tokens)
             (values nil
                     nil
                     (format nil
                             "Invalid step selector(s): ~{~A~^, ~}. Use step indexes like `1`, `1,3`, or `2-4`."
                             (nreverse invalid-tokens))))
            ((and saw-step-marker-p (null indexes))
             (values nil
                     nil
                     "Expected at least one step index after `step` or `steps`."))
            ((and indexes (null invalid-tokens))
             (values (sort (remove-duplicates indexes :test #'=) #'<)
                     t
                     nil))
            (t
             (values nil nil nil)))))))

(defun %parse-plan-step-reorder-token (token)
  (let* ((trimmed (%slash-trim token))
         (arrow-position (search "->" trimmed :test #'char=)))
    (cond
      ((zerop (length trimmed))
       nil)
      ((%digit-string-p trimmed)
       (list (parse-integer trimmed)))
      ((and arrow-position
            (> arrow-position 0)
            (< (+ arrow-position 2) (length trimmed)))
       (let ((from-text (%slash-trim (subseq trimmed 0 arrow-position)))
             (to-text (%slash-trim (subseq trimmed (+ arrow-position 2)))))
         (when (and (%digit-string-p from-text)
                    (%digit-string-p to-text))
           (list (parse-integer from-text)
                 (parse-integer to-text)))))
      (t
       nil))))

(defun %parse-plan-step-reorder-args (raw-args)
  (let ((tokens (%tokenize-command-arguments (or raw-args ""))))
    (if (null tokens)
        (values nil
                nil
                "Expected source and target step indexes (e.g. `/plan reorder 3 1`).")
        (let ((indexes '())
              (invalid-tokens '()))
          (dolist (token tokens)
            (if (member (string-downcase (%slash-trim token))
                        '("step" "steps" "to" "into" "position")
                        :test #'string=)
                nil
                (let ((parsed (%parse-plan-step-reorder-token token)))
                  (if parsed
                      (setf indexes (append indexes parsed))
                      (push token invalid-tokens)))))
          (cond
            (invalid-tokens
             (values nil
                     nil
                     (format nil
                             "Invalid reorder token(s): ~{~A~^, ~}. Use `/plan reorder <from> <to>`."
                             (nreverse invalid-tokens))))
            ((/= (length indexes) 2)
             (values nil
                     nil
                     "Expected exactly two step indexes for reorder (from and to)."))
            ((or (< (first indexes) 1)
                 (< (second indexes) 1))
             (values nil
                     nil
                     "Step indexes must be positive integers."))
            (t
             (values (first indexes)
                     (second indexes)
                     nil)))))))

(defun %plan-status-output (active-p
                            output-path
                            review-pending-p
                            review-decision
                            review-notes
                            step-count
                            approved-step-indexes
                            input-gating-snapshot)
  (labels ((input-gating-reason-label (reason)
             (case reason
               (:plan-mode-active "plan mode active")
               (:review-pending "review pending")
               (:review-not-approved "review decision not approved")
               (:awaiting-explicit-execute "awaiting explicit execute transition")
               (otherwise "open")))
           (input-gating-summary ()
             (let ((terminal-enabled-p
                     (not (null (getf input-gating-snapshot
                                      :terminal-stdin-enabled-p))))
                   (execution-enabled-p
                     (not (null (getf input-gating-snapshot
                                      :execution-pathways-enabled-p)))))
               (format nil
                       " Input gating: ~:[inactive~;active~] (~A). Terminal stdin: ~:[blocked~;enabled~]. Execution pathways: ~:[blocked~;enabled~]."
                       (not (null (getf input-gating-snapshot :active-p)))
                       (input-gating-reason-label
                        (getf input-gating-snapshot :reason))
                       terminal-enabled-p
                       execution-enabled-p))))
  (let ((approved-count (length approved-step-indexes)))
    (if active-p
        (with-output-to-string (out)
          (write-string "Plan mode is ON. PLAN MODE -- read-only [LOCK mutating tools blocked]." out)
          (when (> step-count 0)
            (format out " Approved steps: ~D/~D." approved-count step-count))
          (write-string (input-gating-summary) out))
        (with-output-to-string (out)
          (write-string "Plan mode is OFF." out)
          (when output-path
            (format out " Last plan output: ~A." (namestring output-path)))
          (when (> step-count 0)
            (format out " Approved steps: ~D/~D (~A)."
                    approved-count
                    step-count
                    (%format-step-index-list approved-step-indexes)))
          (when review-pending-p
            (write-string " Plan review pending. Use /plan review to inspect the latest captured plan."
                          out))
          (when (and (symbolp review-decision)
                     (not (eq review-decision :pending)))
            (format out " Last review decision: ~A."
                    (%plan-review-decision-label review-decision)))
          (when (and (stringp review-notes)
                     (plusp (length (%slash-trim review-notes))))
            (format out " Review notes: ~A." (%slash-trim review-notes)))
          (write-string (input-gating-summary) out))))))

(defun %plan-exit-output (plan-markdown output-path write-to-file-p)
  (with-output-to-string (out)
    (write-string "Plan mode disabled." out)
    (if output-path
        (format out " Plan written to ~A." (namestring output-path))
        (when write-to-file-p
          (write-string " Plan file output unavailable." out)))
    (unless write-to-file-p
      (write-string " Plan file output skipped." out))
    (when (and (stringp plan-markdown)
               (plusp (length (%slash-trim plan-markdown))))
      (format out "~%~%Plan captured in conversation:~%~%```markdown~%~A~%```"
              plan-markdown))))

(defun %plan-review-output (plan-markdown
                            review-decision
                            review-notes
                            approved-step-indexes
                            step-count
                            input-gating-snapshot)
  (labels ((input-gating-reason-label (reason)
             (case reason
               (:plan-mode-active "plan mode active")
               (:review-pending "review pending")
               (:review-not-approved "review decision not approved")
               (:awaiting-explicit-execute "awaiting explicit execute transition")
               (otherwise "open"))))
  (if (and (stringp plan-markdown)
           (plusp (length (%slash-trim plan-markdown))))
      (with-output-to-string (out)
        (format out "Plan review:~%")
        (format out "Input gating: ~:[inactive~;active~] (~A), terminal stdin ~:[blocked~;enabled~], execution pathways ~:[blocked~;enabled~].~%"
                (not (null (getf input-gating-snapshot :active-p)))
                (input-gating-reason-label (getf input-gating-snapshot :reason))
                (not (null (getf input-gating-snapshot :terminal-stdin-enabled-p)))
                (not (null (getf input-gating-snapshot :execution-pathways-enabled-p))))
        (when (symbolp review-decision)
          (format out "Current decision: ~A.~%"
                  (%plan-review-decision-label review-decision)))
        (when (and (stringp review-notes)
                   (plusp (length (%slash-trim review-notes))))
          (format out "Review notes: ~A~%" (%slash-trim review-notes)))
        (when (> step-count 0)
          (format out "Approved steps: ~D/~D (~A).~%"
                  (length approved-step-indexes)
                  step-count
                  (%format-step-index-list approved-step-indexes)))
        (format out "~%~A" plan-markdown))
      "No captured plan is available yet. Exit plan mode first to capture one.")))

(defun %plan-handler (_invocation arguments _context)
  (declare (ignore _invocation _context))
  (let* ((state (or (gethash :STATE arguments) :toggle))
         (raw-args (%slash-trim (or (gethash :ARGS arguments) "")))
         (plan-state (current-plan-mode-state))
         (active-p (plan-mode-active-p plan-state)))
    (labels ((captured-plan-available-p ()
               (and (stringp (plan-mode-state-last-plan-markdown plan-state))
                    (plusp (length (%slash-trim (plan-mode-state-last-plan-markdown plan-state))))))
             (ensure-plan-captured-for-review (&key (reason :plan-command-exit))
               (when active-p
                 (multiple-value-bind (_ output-path)
                     (exit-plan-mode :state plan-state
                                     :reason reason
                                     :write-output-p t)
                   (declare (ignore _ output-path))
                   (setconfig :plan-mode nil)
                   (setf active-p nil)))
               (captured-plan-available-p))
             (input-gating-snapshot ()
               (plan-input-gating-snapshot plan-state))
             (invalid-usage (detail)
               (make-slash-command-result
                :output (format nil "~A~%Usage: /plan [on|off|status|review|approve|reorder|reject|modify|request-modifications|request-changes] [args...] (approve accepts step selectors like `1`, `1,3`, `2-4`; reorder accepts `3 1` or `3->1`)"
                                detail)
                :echo-input-p t))
             (recompute-review-decision ()
               (let* ((available-step-indexes (plan-step-indexes plan-state))
                      (approved-step-indexes
                        (plan-mode-state-approved-step-indexes plan-state))
                      (step-count (length available-step-indexes))
                      (approved-count (length approved-step-indexes)))
                 (set-plan-review-decision
                  (cond
                    ((and (> step-count 0)
                          (= approved-count step-count))
                     :approved)
                    ((plusp approved-count)
                     :partially-approved)
                    (t
                     :pending))
                  :state plan-state)))
             (parse-write-to-file-p ()
               (if (%slash-blank-p raw-args)
                   (values t nil)
                   (handler-case
                       (values (%parse-boolean-token raw-args) nil)
                     (error ()
                       (values t "Expected optional write-to-file argument to be true/false for this action.")))))
             (decision-result (decision summary)
               (if (or (captured-plan-available-p)
                       (and (eq decision :approved)
                            (ensure-plan-captured-for-review
                             :reason :plan-command-approved-exit)))
                   (progn
                     (when (eq decision :approved)
                       (set-plan-step-approvals (plan-step-indexes plan-state)
                                                :state plan-state))
                     (when (member decision
                                   '(:rejected :modification-requested :pending)
                                   :test #'eq)
                       (clear-plan-step-approvals plan-state))
                     (set-plan-review-decision decision :notes raw-args :state plan-state)
                     (refresh-plan-review-markdown plan-state)
                     (make-slash-command-result
                      :output (with-output-to-string (out)
                                (write-string summary out)
                                (unless (%slash-blank-p raw-args)
                                  (format out " Notes recorded: ~A." raw-args)))))
                   (make-slash-command-result
                    :output "No captured plan is available yet. Exit plan mode first to capture one.")))
             (approve-steps-result ()
               (multiple-value-bind (requested-step-indexes step-request-p parse-error)
                   (%parse-plan-step-approval-args raw-args)
                 (cond
                   (parse-error
                    (invalid-usage parse-error))
                   ((not step-request-p)
                    (decision-result :approved "Plan approved."))
                   ((not (or (captured-plan-available-p)
                             (ensure-plan-captured-for-review
                              :reason :plan-command-approved-exit)))
                    (make-slash-command-result
                     :output "No captured plan is available yet. Exit plan mode first to capture one."))
                   (t
                    (let* ((available-step-indexes (plan-step-indexes plan-state))
                           (missing-step-indexes
                             (remove-if (lambda (index)
                                          (member index available-step-indexes :test #'=))
                                        requested-step-indexes)))
                      (cond
                        ((null available-step-indexes)
                         (make-slash-command-result
                          :output "No captured plan steps are available to approve."))
                        (missing-step-indexes
                         (make-slash-command-result
                          :output (format nil
                                          "Unknown plan step index(es): ~A. Available steps: ~A."
                                          (%format-step-index-list missing-step-indexes)
                                          (%format-step-index-list available-step-indexes))))
                        (t
                         (approve-plan-steps requested-step-indexes :state plan-state)
                         (let* ((approved-step-indexes
                                  (plan-mode-state-approved-step-indexes plan-state))
                                (approved-count (length approved-step-indexes))
                                (step-count (length available-step-indexes))
                                (all-approved-p (= approved-count step-count)))
                           (set-plan-review-decision (if all-approved-p
                                                         :approved
                                                         :partially-approved)
                                                     :state plan-state)
                           (refresh-plan-review-markdown plan-state)
                           (make-slash-command-result
                            :output (with-output-to-string (out)
                                      (format out "Approved step(s): ~A."
                                              (%format-step-index-list requested-step-indexes))
                                      (format out " Current approval: ~D/~D."
                                              approved-count
                                              step-count)
                                      (unless all-approved-p
                                        (let ((remaining-step-indexes
                                                (remove-if (lambda (index)
                                                             (member index approved-step-indexes :test #'=))
                                                           available-step-indexes)))
                                          (format out " Remaining steps: ~A."
                                                  (%format-step-index-list remaining-step-indexes))))))))))))))
             (reorder-steps-result ()
               (multiple-value-bind (from-index to-index parse-error)
                   (%parse-plan-step-reorder-args raw-args)
                 (cond
                   (parse-error
                    (invalid-usage parse-error))
                   ((not (captured-plan-available-p))
                    (make-slash-command-result
                     :output "No captured plan is available yet. Exit plan mode first to capture one."))
                   (t
                    (let* ((available-step-indexes (plan-step-indexes plan-state))
                           (unknown-indexes
                             (remove nil
                                     (list (and (not (member from-index available-step-indexes :test #'=))
                                                from-index)
                                           (and (not (member to-index available-step-indexes :test #'=))
                                                to-index)))))
                      (cond
                        ((null available-step-indexes)
                         (make-slash-command-result
                          :output "No captured plan steps are available to reorder."))
                        (unknown-indexes
                         (make-slash-command-result
                          :output (format nil
                                          "Unknown plan step index(es): ~A. Available steps: ~A."
                                          (%format-step-index-list unknown-indexes)
                                          (%format-step-index-list available-step-indexes))))
                        ((= from-index to-index)
                         (make-slash-command-result
                          :output (format nil
                                          "Step ~D is already at position ~D. No reorder needed."
                                          from-index
                                          to-index)))
                        (t
                         (reorder-plan-step from-index to-index :state plan-state)
                         (recompute-review-decision)
                         (refresh-plan-review-markdown plan-state)
                         (let* ((approved-step-indexes
                                  (plan-mode-state-approved-step-indexes plan-state))
                                (step-count (length available-step-indexes))
                                (approved-count (length approved-step-indexes)))
                           (make-slash-command-result
                            :output (with-output-to-string (out)
                                      (format out "Reordered step ~D to position ~D."
                                              from-index
                                              to-index)
                                      (format out " Approved steps: ~D/~D (~A)."
                                              approved-count
                                              step-count
                                              (%format-step-index-list approved-step-indexes)))))))))))))
      (case state
        (:status
         (if (%slash-blank-p raw-args)
             (make-slash-command-result
              :output (%plan-status-output active-p
                                           (plan-mode-state-last-output-path plan-state)
                                           (plan-mode-state-review-pending-p plan-state)
                                           (plan-mode-state-review-decision plan-state)
                                           (plan-mode-state-review-notes plan-state)
                                           (length (plan-mode-state-steps plan-state))
                                           (plan-mode-state-approved-step-indexes plan-state)
                                           (input-gating-snapshot)))
             (invalid-usage "The /plan status action does not accept extra arguments.")))
        (:review
         (if (%slash-blank-p raw-args)
             (progn
               (setf (plan-mode-state-review-last-presented-at plan-state) (get-universal-time))
               (make-slash-command-result
                :output (%plan-review-output (plan-mode-state-last-plan-markdown plan-state)
                                             (plan-mode-state-review-decision plan-state)
                                             (plan-mode-state-review-notes plan-state)
                                             (plan-mode-state-approved-step-indexes plan-state)
                                             (length (plan-mode-state-steps plan-state))
                                             (input-gating-snapshot))))
             (invalid-usage "The /plan review action does not accept extra arguments.")))
        (:approve
         (approve-steps-result))
        (:reorder
         (reorder-steps-result))
        (:reject
         (decision-result :rejected "Plan rejected."))
        ((:modify :request-modifications :request-changes)
         (decision-result :modification-requested
                          "Plan modifications requested. Re-enter /plan on to update the draft."))
        (:on
         (if (not (%slash-blank-p raw-args))
             (invalid-usage "The /plan on action does not accept extra arguments.")
             (if active-p
                 (make-slash-command-result
                  :output "Plan mode already enabled.")
                (progn
                   (enter-plan-mode :state plan-state :clear-steps-p t)
                   (setconfig :plan-mode t)
                   (make-slash-command-result
                    :output "Plan mode enabled. PLAN MODE -- read-only [LOCK mutating tools blocked].")))))
        (:off
         (multiple-value-bind (write-to-file-p parse-error)
             (parse-write-to-file-p)
           (if parse-error
               (invalid-usage parse-error)
               (if active-p
                   (let ((rendered-plan (plan-markdown :state plan-state
                                                       :reason :plan-command-exit)))
                     (multiple-value-bind (_ output-path)
                         (exit-plan-mode :state plan-state
                                         :reason :plan-command-exit
                                         :write-output-p write-to-file-p)
                       (declare (ignore _))
                       (setconfig :plan-mode nil)
                       (make-slash-command-result
                        :output (%plan-exit-output rendered-plan output-path write-to-file-p))))
                   (make-slash-command-result
                    :output "Plan mode already disabled.")))))
        (otherwise
         (multiple-value-bind (write-to-file-p parse-error)
             (parse-write-to-file-p)
           (if parse-error
               (invalid-usage parse-error)
               (if active-p
                   (let ((rendered-plan (plan-markdown :state plan-state
                                                       :reason :plan-command-toggle)))
                     (multiple-value-bind (_ _status output-path)
                         (toggle-plan-mode :state plan-state
                                           :reason :plan-command-toggle
                                           :write-output-p write-to-file-p)
                       (declare (ignore _ _status))
                       (setconfig :plan-mode nil)
                       (make-slash-command-result
                        :output (%plan-exit-output rendered-plan output-path write-to-file-p))))
                   (progn
                     (toggle-plan-mode :state plan-state :reason :plan-command-toggle)
                     (setconfig :plan-mode t)
                     (make-slash-command-result
                      :output "Plan mode enabled. PLAN MODE -- read-only [LOCK mutating tools blocked]."))))))))))

(defun %execute-handler (_invocation arguments _context)
  (declare (ignore _invocation _context))
  (let* ((args-text (or (and (hash-table-p arguments) (gethash :ARGS arguments)) ""))
         (interactive-p (or (search "--interactive" args-text)
                            (search "-i" args-text)))
         (plan-state (current-plan-mode-state))
         (active-p (plan-mode-active-p plan-state))
         (captured-plan (plan-mode-state-last-plan-markdown plan-state))
         (available-step-indexes (plan-step-indexes plan-state))
         (approved-step-indexes (or (plan-mode-state-approved-step-indexes plan-state) '()))
         (review-decision (plan-mode-state-review-decision plan-state)))
    (when active-p
      (multiple-value-bind (_ output-path)
          (exit-plan-mode :state plan-state
                          :reason :execute-transition
                          :write-output-p t)
        (declare (ignore _ output-path))
        (setconfig :plan-mode nil)
        (setf active-p nil
              captured-plan (plan-mode-state-last-plan-markdown plan-state)
              available-step-indexes (plan-step-indexes plan-state)
              approved-step-indexes (or (plan-mode-state-approved-step-indexes plan-state) '())
              review-decision (plan-mode-state-review-decision plan-state))))
    (cond
      ((or active-p
           (not (stringp captured-plan))
           (zerop (length (%slash-trim captured-plan)))
           (null available-step-indexes))
       (make-slash-command-result
        :output "No captured plan is available yet. Use /plan to draft a plan before /execute."))
      ((null approved-step-indexes)
       (make-slash-command-result
        :output "No approved steps are available for execution. Use /plan approve first."))
      ((not (member review-decision '(:approved :partially-approved) :test #'eq))
       (make-slash-command-result
        :output (format nil
                        "Plan review decision is ~A. Approve steps with /plan approve before /execute."
                        (%plan-review-decision-label review-decision))))
      (t
       ;; Explicitly disable plan-mode gating before execution handoff.
       (setconfig :plan-mode nil)
       (setf (plan-mode-state-review-pending-p plan-state) nil)
       (refresh-plan-review-markdown plan-state)
       (let* ((execution-state (initialize-plan-execution :plan-state plan-state))
              (approved-count (length approved-step-indexes))
              (step-count (length available-step-indexes))
              (next-step-index (plan-execution-next-step-index execution-state))
              (run-id (plan-execution-state-run-id execution-state)))
         (when interactive-p
           (setf (plan-execution-state-interactive-p execution-state) t))
         (plan-execution-append-output
          (format nil "LIVE> /execute accepted: run ~A with ~D approved step~:P."
                  run-id
                  approved-count)
          :phase :system
          :style :meta
          :state execution-state)
         (make-slash-command-result
          :output (with-output-to-string (out)
                    (write-string "Execution pathways re-enabled after user approval." out)
                    (write-string " Plan mode is OFF." out)
                    (format out " Approved steps: ~D/~D (~A)."
                            approved-count
                            step-count
                            (%format-step-index-list approved-step-indexes))
                    (format out " Execution run initialized: ~A." run-id)
                    (when interactive-p
                      (write-string " Interactive mode: each step requires approval." out))
                    (when next-step-index
                      (format out " Next approved step: ~D." next-step-index)))))))))

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

(defun %clear-confirmed-p (raw-arguments)
  (let ((tokens (%tokenize-command-arguments (or raw-arguments ""))))
    (loop for token in tokens
          thereis (member (string-downcase (%slash-trim token))
                          '("--yes" "yes" "confirm" "--confirm")
                          :test #'string=))))

(defun %clear-handler (_invocation arguments _context)
  (declare (ignore _invocation _context))
  (let ((raw-arguments (or (gethash :ARGS arguments) "")))
    (if (%clear-confirmed-p raw-arguments)
        (make-slash-command-result
         :echo-input-p nil
         :output "Conversation cleared."
         :action :clear-chat)
        (make-slash-command-result
         :echo-input-p t
         :output "Confirm clear with /clear --yes."))))

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
        (tool nil)
        (since nil)
        (until nil)
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
                 (let ((normalized (%commands-history-normalize-role value)))
                   (if normalized
                       (setf role normalized)
                       (push (format nil "Invalid role ~S." value) errors))))))
            ((or (string-equal token "--tool")
                 (string-equal token "-t"))
             (let ((value (consume-option-value "tool")))
               (when value
                 (if (%slash-blank-p value)
                     (push "Tool filter must not be blank." errors)
                     (setf tool (%slash-trim value))))))
            ((or (string-equal token "--since")
                 (string-equal token "-s"))
             (let ((value (consume-option-value "since")))
               (when value
                 (if (parse-history-timestamp value)
                     (setf since value)
                     (push (format nil "Invalid timestamp ~S for --since." value) errors)))))
            ((or (string-equal token "--until")
                 (string-equal token "-u"))
             (let ((value (consume-option-value "until")))
               (when value
                 (if (parse-history-timestamp value)
                     (setf until value)
                     (push (format nil "Invalid timestamp ~S for --until." value) errors)))))
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
                  (let ((normalized (%commands-history-normalize-role value)))
                    (if normalized
                        (setf role normalized)
                        (push (format nil "Invalid role ~S." value) errors))))
                 ((and key (string= key "tool"))
                  (if (%slash-blank-p value)
                      (push "Tool filter must not be blank." errors)
                      (setf tool (%slash-trim value))))
                 ((and key (string= key "since"))
                  (if (parse-history-timestamp value)
                      (setf since value)
                      (push (format nil "Invalid timestamp ~S for since filter." value)
                            errors)))
                 ((and key (string= key "until"))
                  (if (parse-history-timestamp value)
                      (setf until value)
                      (push (format nil "Invalid timestamp ~S for until filter." value)
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
    (let ((since-ts (parse-history-timestamp since))
          (until-ts (parse-history-timestamp until)))
      (when (and since-ts until-ts (> since-ts until-ts))
        (push "Timestamp range is invalid: --since is later than --until."
              errors)))
    (values (list :query (if query-tokens
                             (format nil "~{~A~^ ~}" (nreverse query-tokens))
                             "")
                  :role role
                  :tool tool
                  :since since
                  :until until
                  :limit limit)
            (nreverse errors))))

(defun %history-result-block (result ordinal)
  (check-type result conversation-history-search-result)
  (let ((entry (conversation-history-search-result-entry result))
        (before (conversation-history-search-result-before result))
        (after (conversation-history-search-result-after result)))
    (with-output-to-string (out)
      (format out "~D. ~A~%" ordinal (format-history-entry-line entry))
      (when before
        (format out "   prev: ~A~%" (format-history-entry-line before)))
      (when after
        (format out "   next: ~A~%" (format-history-entry-line after)))
      (format out "   score: ~D"
              (conversation-history-search-result-score result)))))

(defun %history-result-output (results &key role tool query since until limit)
  (if (null results)
      "No conversation history matches the provided filters."
      (with-output-to-string (out)
        (format out "History results (~D):~%" (length results))
        (when (or (and role (plusp (length role)))
                  (and tool (plusp (length tool)))
                  (and (stringp query) (plusp (length (%slash-trim query))))
                  since
                  until)
          (format out "Filters:~@[ role=~A~]~@[ tool=~A~]~@[ query=~S~]~@[ since=~A~]~@[ until=~A~]~@[ limit=~D~]~%"
                  role
                  tool
                  (let ((trimmed (%slash-trim query)))
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
           :output (format nil "~{~A~%~}Usage: /history [query...] [--role ROLE] [--tool NAME] [--since TIMESTAMP] [--until TIMESTAMP] [--limit N]"
                           errors))
          (let* ((query (getf filters :query))
                 (role (getf filters :role))
                 (tool (getf filters :tool))
                 (since (getf filters :since))
                 (until (getf filters :until))
                 (limit (getf filters :limit))
                 (results (history-search conversation
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

(defun %format-tool-history-timestamp (timestamp)
  (if (and (integerp timestamp) (plusp timestamp))
      (multiple-value-bind (second minute hour day month year)
          (decode-universal-time timestamp)
        (format nil "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D"
                year month day hour minute second))
      "unknown"))

(defun %tool-history-source-text (value)
  (cond
    ((pathnamep value) (namestring value))
    ((null value) nil)
    (t (princ-to-string value))))

(defun %hash-table-keys-local (table)
  (loop for key being the hash-keys of table
        collect key))

(defun %known-tool-names ()
  (let ((seen (make-hash-table :test #'equal))
        (names '()))
    (labels ((remember (value)
               (let ((text (%slash-trim (princ-to-string value))))
                 (when (plusp (length text))
                   (let ((key (string-downcase text)))
                     (unless (gethash key seen)
                       (setf (gethash key seen) t)
                       (push key names)))))))
      (when (and (boundp '*tool-metadata*)
                 (hash-table-p *tool-metadata*))
        (dolist (name (%hash-table-keys-local *tool-metadata*))
          (remember name)))
      (when (and (boundp '*tool-history*)
                 (hash-table-p *tool-history*))
        (dolist (name (%hash-table-keys-local *tool-history*))
          (remember name))))
    (sort names #'string<)))

(defun %tool-name-completions (fragment)
  (let ((prefix (%slash-trim fragment)))
    (loop for name in (%known-tool-names)
          when (%starts-with-ci-p prefix name)
            collect name)))

(defun %tool-history-handler (_invocation arguments _context)
  (declare (ignore _invocation _context))
  (let* ((tool-name (gethash :NAME arguments))
         (versions (and tool-name (tool-history tool-name))))
    (if (%slash-blank-p tool-name)
        (make-slash-command-result
         :echo-input-p t
         :output "Usage: /tool-history <tool-name>")
        (make-slash-command-result
         :echo-input-p t
         :output (if (null versions)
                     (format nil "No history available for tool ~A." tool-name)
                     (with-output-to-string (out)
                       (format out "Tool history for ~A (~D version~:P):~%"
                               (string-downcase tool-name)
                               (length versions))
                       (dolist (entry versions)
                         (let* ((version (getf entry :version))
                                (timestamp (getf entry :timestamp))
                                (source-file (%tool-history-source-text
                                              (getf entry :source-file)))
                                (source-line (getf entry :source-line)))
                           (format out "~D. ~A"
                                   version
                                   (%format-tool-history-timestamp timestamp))
                           (when source-file
                             (format out " source=~A" source-file))
                           (when source-line
                             (format out " line=~A" source-line))
                           (format out "~%")))))))))

(defun %tool-rollback-handler (_invocation arguments _context)
  (declare (ignore _invocation _context))
  (let* ((tool-name (gethash :NAME arguments))
         (version (or (gethash :VERSION arguments) 1)))
    (if (%slash-blank-p tool-name)
        (make-slash-command-result
         :echo-input-p t
         :output "Usage: /tool-rollback <tool-name> [version]")
        (handler-case
            (progn
              (rollback-tool tool-name :version version)
              (let ((remaining (length (tool-history tool-name))))
                (make-slash-command-result
                 :echo-input-p t
                 :output (format nil "Rolled back ~A to version ~D. History now has ~D entry~:P."
                                 (string-downcase tool-name)
                                 version
                                 remaining))))
          (error (condition)
            (make-slash-command-result
             :echo-input-p t
             :output (format nil "Tool rollback failed: ~A" condition)))))))

(defun %fork-usage ()
  "/fork <name> [message-index] | /fork <name> --at <message-index>")

(defun %fork-branch-point-text (value)
  (if (integerp value)
      (format nil "~D" value)
      "-"))

(defun %fork-parse-arguments (raw-arguments)
  (let* ((tokens (%tokenize-command-arguments (or raw-arguments "")))
         (name nil)
         (message-index nil)
         (errors '()))
    (cond
      ((null tokens)
       (push "Missing required fork name." errors))
      ((= (length tokens) 1)
       (setf name (first tokens)))
      ((and (= (length tokens) 3)
            (string-equal (second tokens) "--at"))
       (setf name (first tokens))
       (handler-case
           (setf message-index (parse-integer (third tokens)))
         (error ()
           (push (format nil "Fork message-index must be an integer, got ~S."
                         (third tokens))
                 errors))))
      ((= (length tokens) 2)
       (setf name (first tokens))
       (handler-case
           (setf message-index (parse-integer (second tokens)))
         (error ()
           (push (format nil "Fork message-index must be an integer, got ~S."
                         (second tokens))
                 errors))))
      (t
       (push (format nil "Unexpected arguments ~{~S~^ ~}." tokens) errors)))
    (values name message-index (nreverse errors))))

(defun %fork-chat-event-bus (chat-state)
  (or (and (typep chat-state 'chat-ui-state)
           (typep (chat-ui-state-status-bar-state chat-state) 'status-bar-state)
           (status-bar-state-event-bus (chat-ui-state-status-bar-state chat-state)))
      (current-event-bus)))

(defun %fork-handler (_invocation arguments context)
  (declare (ignore _invocation))
  (let* ((raw-arguments (or (gethash :ARGS arguments) ""))
         (chat-state (slash-command-context-chat-state context))
         (conversation (and (typep chat-state 'chat-ui-state)
                            (chat-ui-state-conversation chat-state))))
    (unless (typep conversation 'conversation-state)
      (return-from %fork-handler
        (make-slash-command-result
         :echo-input-p t
         :output "Conversation history is unavailable for this session.")))
    (multiple-value-bind (name message-index errors)
        (%fork-parse-arguments raw-arguments)
      (if errors
          (make-slash-command-result
           :echo-input-p t
           :output (format nil "~{~A~%~}Usage: ~A"
                           errors
                           (%fork-usage)))
          (handler-case
              (let* ((forked (fork-conversation conversation
                                                name
                                                :message-index message-index
                                                :save-p t
                                                :event-bus (%fork-chat-event-bus chat-state)))
                     (entry-count (length (conversation-state-entries forked)))
                     (branch-point (conversation-state-fork-branch-point forked)))
                (make-slash-command-result
                 :echo-input-p t
                 :output (format nil "Created fork ~A at message ~A (~D message~:P)."
                                 (conversation-active-fork-name forked)
                                 (%fork-branch-point-text branch-point)
                                 entry-count)))
            (error (condition)
              (make-slash-command-result
               :echo-input-p t
               :output (format nil "Failed to create fork: ~A" condition))))))))

(defun %forks-handler (_invocation _arguments context)
  (declare (ignore _invocation _arguments))
  (let* ((chat-state (slash-command-context-chat-state context))
         (conversation (and (typep chat-state 'chat-ui-state)
                            (chat-ui-state-conversation chat-state))))
    (unless (typep conversation 'conversation-state)
      (return-from %forks-handler
        (make-slash-command-result
         :echo-input-p t
         :output "Conversation history is unavailable for this session.")))
    (let* ((active (conversation-active-fork-name conversation))
           (forks (sort (copy-list (conversation-list-forks conversation))
                        #'string<
                        :key (lambda (record)
                               (string-downcase
                                (or (getf record :name) ""))))))
      (make-slash-command-result
       :echo-input-p t
       :output (if (null forks)
                   "No conversation forks available."
                   (with-output-to-string (out)
                     (format out "Conversation forks (~D):~%" (length forks))
                     (dolist (record forks)
                       (let* ((name (or (getf record :name) "unknown"))
                              (branch-point (getf record :branch-point))
                              (message-count (or (getf record :message-count) 0))
                              (active-marker
                                (if (string-equal name active) "*" " ")))
                         (format out "~A ~A (branch-point: ~A, messages: ~D)~%"
                                 active-marker
                                 name
                                 (%fork-branch-point-text branch-point)
                                 message-count)))))))))

(defun %switch-fork-handler (_invocation arguments context)
  (declare (ignore _invocation))
  (let* ((target (gethash :NAME arguments))
         (chat-state (slash-command-context-chat-state context))
         (conversation (and (typep chat-state 'chat-ui-state)
                            (chat-ui-state-conversation chat-state))))
    (unless (typep conversation 'conversation-state)
      (return-from %switch-fork-handler
        (make-slash-command-result
         :echo-input-p t
         :output "Conversation history is unavailable for this session.")))
    (when (%slash-blank-p target)
      (return-from %switch-fork-handler
        (make-slash-command-result
         :echo-input-p t
         :output "Usage: /switch-fork <name>")))
    (handler-case
        (let* ((switched (conversation-switch-fork conversation target :save-p t))
               (messages (conversation-state-messages switched)))
          (when (typep chat-state 'chat-ui-state)
            (setf (chat-ui-state-conversation chat-state) switched
                  (chat-ui-state-messages chat-state) messages
                  (chat-ui-state-message-scrollback-lines chat-state) 0
                  (chat-ui-state-max-message-scrollback-lines chat-state) 0))
          (make-slash-command-result
           :echo-input-p t
           :output (format nil "Switched to fork ~A (~D message~:P)."
                           (conversation-active-fork-name switched)
                           (length messages))))
      (error (condition)
        (make-slash-command-result
         :echo-input-p t
         :output (format nil "Failed to switch fork: ~A" condition))))))

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

(defun %extensions-usage ()
  "/extensions [list|reload|enable <all|name|path>|disable <all|name|path>]")

(defun %extension-scope-label (scope)
  (case scope
    (:global "global")
    (:project "project")
    (otherwise (string-downcase (symbol-name scope)))))

(defun %extension-status-label (status)
  (case status
    (:loaded "loaded")
    (:error "error")
    (:disabled "disabled")
    (otherwise (string-downcase (symbol-name status)))))

(defun %render-extensions-list ()
  (let* ((report (list-extension-report))
         (summary (extension-report-summary report)))
    (if (null report)
        "No extension scan has run yet. Use /extensions reload."
        (with-output-to-string (out)
          (format out "Extensions: total=~D loaded=~D errors=~D disabled=~D~%"
                  (getf summary :total 0)
                  (getf summary :loaded 0)
                  (getf summary :errors 0)
                  (getf summary :disabled 0))
          (dolist (entry report)
            (format out "- [~A/~A] ~A~@[ name=~A~]~@[ version=~A~]~@[ entry=~A~]~@[ -- ~A~]~%"
                    (%extension-status-label (extension-load-record-status entry))
                    (%extension-scope-label (extension-load-record-scope entry))
                    (extension-load-record-path entry)
                    (extension-load-record-name entry)
                    (extension-load-record-version entry)
                    (extension-load-record-entry-point entry)
                    (extension-load-record-message entry)))))))

(defun %extensions-join (tokens)
  (with-output-to-string (out)
    (loop for token in tokens
          for index from 0 do
            (when (> index 0)
              (write-char #\Space out))
            (write-string token out))))

(defun %extensions-known-targets ()
  (let ((targets (copy-list (known-user-extension-paths))))
    (when (null targets)
      (multiple-value-bind (global project)
          (discover-user-extension-files)
        (setf targets
              (append (mapcar #'namestring global)
                      (mapcar #'namestring project)))))
    (setf targets (append targets (known-user-extension-names)))
    (let ((seen (make-hash-table :test #'equal))
          (result '()))
      (labels ((remember (value)
                 (let ((trimmed (%slash-trim value)))
                   (when (plusp (length trimmed))
                     (let ((key (string-downcase trimmed)))
                       (unless (gethash key seen)
                         (setf (gethash key seen) t)
                         (push trimmed result)))))))
        (dolist (target targets)
          (remember target)
          (remember (file-namestring (pathname target)))))
      (nreverse result))))

(defun %extensions-handler (_invocation arguments context)
  (declare (ignore _invocation))
  (let* ((raw (or (gethash :ARGS arguments) ""))
         (tokens (%tokenize-command-arguments raw))
         (action-token (if tokens
                           (string-downcase (first tokens))
                           "list")))
    (labels ((invalid-usage (&optional details)
               (make-slash-command-result
                :echo-input-p t
                :output (format nil "~@[~A~%~]Usage: ~A"
                                details
                                (%extensions-usage)))))
      (cond
        ((member action-token '("list" "ls") :test #'string=)
         (if (> (length tokens) 1)
             (invalid-usage (format nil "Unexpected argument ~S." (second tokens)))
             (make-slash-command-result
              :echo-input-p t
              :output (%render-extensions-list))))
        ((string= action-token "reload")
         (if (> (length tokens) 1)
             (invalid-usage (format nil "Unexpected argument ~S." (second tokens)))
             (let* ((cfg (or (slash-command-context-config context)
                             (%current-config-safe)))
                    (project-root (and (config-p cfg)
                                       (config-project-root cfg)))
                    (report (reload-user-extensions :project-root project-root))
                    (summary (extension-report-summary report)))
               (make-slash-command-result
                :echo-input-p t
                :output (format nil "Reloaded extensions: loaded=~D errors=~D disabled=~D."
                                (getf summary :loaded 0)
                                (getf summary :errors 0)
                                (getf summary :disabled 0))))))
        ((string= action-token "disable")
         (let ((target (%extensions-join (rest tokens))))
           (if (%slash-blank-p target)
               (invalid-usage "Specify extension target or 'all'.")
               (multiple-value-bind (disabled-paths disabled-count)
                   (disable-user-extension target)
                 (if (zerop disabled-count)
                     (make-slash-command-result
                      :echo-input-p t
                      :output (format nil "No extensions matched ~S." target))
                     (make-slash-command-result
                      :echo-input-p t
                      :output (format nil "Disabled ~D extension(s). Reload to apply.~%~{~A~%~}"
                                      disabled-count
                                      disabled-paths)))))))
        ((string= action-token "enable")
         (let ((target (%extensions-join (rest tokens))))
           (if (%slash-blank-p target)
               (invalid-usage "Specify extension target or 'all'.")
               (multiple-value-bind (enabled-paths enabled-count)
                   (enable-user-extension target)
                 (if (zerop enabled-count)
                     (make-slash-command-result
                      :echo-input-p t
                      :output (format nil "No disabled extensions matched ~S." target))
                     (make-slash-command-result
                      :echo-input-p t
                      :output (format nil "Enabled ~D extension(s). Reload to apply.~%~{~A~%~}"
                                      enabled-count
                                      enabled-paths)))))))
        (t
         (invalid-usage (format nil "Unknown /extensions action ~S." action-token)))))))

(defun %checkpoint-usage ()
  "/checkpoint [save|list|restore <id>]")

(defun %format-checkpoint-timestamp (timestamp)
  (if (and (integerp timestamp) (plusp timestamp))
      (multiple-value-bind (second minute hour day month year)
          (decode-universal-time timestamp)
        (format nil "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D"
                year month day hour minute second))
      "unknown"))

(defun %render-checkpoint-list (&optional (checkpoints (list-session-checkpoints :limit 25)))
  (if (null checkpoints)
      "No checkpoints available."
      (with-output-to-string (out)
        (format out "Checkpoints (~D):~%" (length checkpoints))
        (loop for checkpoint in checkpoints
              for index from 1 do
                (format out "~2D. ~A ~A~@[ [~A]~]~%"
                        index
                        (session-checkpoint-id checkpoint)
                        (%format-checkpoint-timestamp
                         (session-checkpoint-created-at checkpoint))
                        (and (session-checkpoint-auto-p checkpoint)
                             (string-downcase
                              (symbol-name (session-checkpoint-trigger checkpoint)))))))))

(defun %checkpoint-context-conversation (context)
  (let* ((chat-state (slash-command-context-chat-state context))
         (conversation (and (typep chat-state 'chat-ui-state)
                            (chat-ui-state-conversation chat-state))))
    (if (typep conversation 'conversation-state)
        conversation
        nil)))

(defun %checkpoint-handler (_invocation arguments context)
  (declare (ignore _invocation))
  (let* ((raw (or (gethash :ARGS arguments) ""))
         (tokens (%tokenize-command-arguments raw))
         (action-token (if tokens
                           (string-downcase (first tokens))
                           "save"))
         (chat-state (slash-command-context-chat-state context))
         (conversation (%checkpoint-context-conversation context))
         (cfg (or (slash-command-context-config context)
                  (%current-config-safe))))
    (labels ((invalid-usage (&optional details)
               (make-slash-command-result
                :echo-input-p t
                :output (format nil "~@[~A~%~]Usage: ~A"
                                details
                                (%checkpoint-usage)))))
      (cond
        ((member action-token '("save" "now") :test #'string=)
         (if (> (length tokens) 1)
             (invalid-usage (format nil "Unexpected argument ~S." (second tokens)))
             (handler-case
                 (let ((checkpoint (checkpoint-session :conversation conversation
                                                       :config cfg
                                                       :trigger :manual
                                                       :auto-p nil)))
                   (make-slash-command-result
                    :echo-input-p t
                    :output (format nil "Saved checkpoint ~A."
                                    (session-checkpoint-id checkpoint))))
               (error (condition)
                 (make-slash-command-result
                  :echo-input-p t
                  :output (format nil "Checkpoint save failed: ~A" condition))))))
        ((member action-token '("list" "ls") :test #'string=)
         (if (> (length tokens) 1)
             (invalid-usage (format nil "Unexpected argument ~S." (second tokens)))
             (make-slash-command-result
              :echo-input-p t
              :output (%render-checkpoint-list))))
        ((string= action-token "restore")
         (let ((target (%extensions-join (rest tokens))))
           (if (%slash-blank-p target)
               (invalid-usage "Specify a checkpoint id, index, or path to restore.")
               (handler-case
                   (let* ((restored (restore-session :checkpoint-id target
                                                     :config cfg))
                          (checkpoint (getf restored :checkpoint))
                          (restored-conversation (getf restored :conversation)))
                     (when (and (typep chat-state 'chat-ui-state)
                                (typep restored-conversation 'conversation-state))
                       (setf (chat-ui-state-conversation chat-state) restored-conversation
                             (chat-ui-state-messages chat-state)
                             (conversation-state-messages restored-conversation)
                             (chat-ui-state-message-scrollback-lines chat-state) 0
                             (chat-ui-state-max-message-scrollback-lines chat-state) 0))
                     (make-slash-command-result
                      :echo-input-p t
                      :output (format nil
                                      "Restored checkpoint ~A (~D message~:P)."
                                      (session-checkpoint-id checkpoint)
                                      (length (conversation-state-entries
                                               restored-conversation)))))
                 (error (condition)
                   (make-slash-command-result
                    :echo-input-p t
                    :output (format nil "Checkpoint restore failed: ~A" condition)))))))
        (t
         (invalid-usage (format nil "Unknown /checkpoint action ~S." action-token)))))))

(defun %checkpoint-id-completions (fragment)
  (let ((prefix (%slash-trim fragment)))
    (loop for checkpoint in (list-session-checkpoints :limit 25)
          for id = (session-checkpoint-id checkpoint)
          when (%starts-with-ci-p prefix id)
            collect id)))

(defun %checkpoint-arg-completer (_command _invocation index fragment prefix-tokens)
  (declare (ignore _command _invocation))
  (let ((head (and prefix-tokens (string-downcase (first prefix-tokens))))
        (prefix (%slash-trim fragment)))
    (cond
      ((= index 0)
       (loop for option in '("save" "list" "restore")
             when (%starts-with-ci-p prefix option)
               collect option))
      ((and (string= head "restore") (= index 1))
       (%checkpoint-id-completions fragment))
      (t
       nil))))

(defun %sounds-usage ()
  "/sounds [list] | /sounds set <theme> | /sounds preview [category]")

(defun %notifications-usage ()
  "/notifications [list] | /notifications enable <backend> | /notifications disable <backend> | /notifications test-fire [event-type]")

(defun %speak-usage ()
  "/speak [last|on|off|status|stop|voice <name>]")

(defun %sound-theme-label (theme-name)
  (if theme-name
      (string-downcase (symbol-name theme-name))
      "none"))

(defun %sound-category-label (category)
  (string-downcase (symbol-name category)))

(defun %parse-sound-category (token)
  (when (and token (plusp (length (%slash-trim token))))
    (intern (string-upcase (%slash-trim token)) :keyword)))

(defun %render-sounds-list ()
  (let ((themes (list-sound-themes))
        (active-name (active-sound-theme-name)))
    (if (null themes)
        "No sound themes registered."
        (with-output-to-string (out)
          (format out "Sound themes (active: ~A):~%"
                  (%sound-theme-label active-name))
          (dolist (theme themes)
            (format out "- ~A~:[~; (active)~]~@[ parent=~A~] mappings=~D~%"
                    (%sound-theme-label (sound-theme-name theme))
                    (eq (sound-theme-name theme) active-name)
                    (and (sound-theme-parent theme)
                         (%sound-theme-label (sound-theme-parent theme)))
                    (hash-table-count (sound-theme-mappings theme))))))))

(defun %sound-preview-result-output (category theme-name success detail sound-path)
  (let ((category-label (%sound-category-label category))
        (theme-label (%sound-theme-label theme-name))
        (path-label (and sound-path (princ-to-string sound-path))))
    (cond
      (success
       (format nil "Previewed ~A using theme ~A (~A)."
               category-label
               theme-label
               path-label))
      ((eq detail :no-sound-configured)
       (format nil "Theme ~A has no sound for ~A."
               theme-label
               category-label))
      ((eq detail :backend-disabled)
       (format nil "Sound backend is disabled. Resolved path: ~A."
               path-label))
      ((eq detail :backend-unavailable)
       (format nil "Sound backend is unavailable. Resolved path: ~A."
               path-label))
      ((eq detail :missing-sound-file)
       (format nil "Resolved preview sound is missing: ~A."
               path-label))
      ((stringp detail)
       (format nil "Sound preview failed: ~A" detail))
      (t
       (format nil "Sound preview could not play (~S)." detail)))))

(defun %preview-sound (category config)
  (let ((theme-name (or (active-sound-theme-name) :standard)))
    (if (fboundp 'preview-notification-sound)
        (multiple-value-bind (success detail sound-path)
            (funcall (symbol-function 'preview-notification-sound)
                     :category category
                     :config config
                     :theme theme-name)
          (%sound-preview-result-output category theme-name success detail sound-path))
        (let ((sound-path (resolve-active-sound-path category :config config)))
          (%sound-preview-result-output category theme-name nil :backend-unavailable sound-path)))))

(defun %sounds-handler (_invocation arguments context)
  (declare (ignore _invocation))
  (let* ((raw (or (gethash :ARGS arguments) ""))
         (tokens (%tokenize-command-arguments raw))
         (action-token (if tokens
                           (string-downcase (first tokens))
                           "list"))
         (cfg (or (slash-command-context-config context)
                  (%current-config-safe))))
    (labels ((invalid-usage (&optional details)
               (make-slash-command-result
                :echo-input-p t
                :output (format nil "~@[~A~%~]Usage: ~A"
                                details
                                (%sounds-usage)))))
      (cond
        ((member action-token '("list" "ls") :test #'string=)
         (if (> (length tokens) 1)
             (invalid-usage (format nil "Unexpected argument ~S." (second tokens)))
             (make-slash-command-result
              :echo-input-p t
              :output (%render-sounds-list))))
        ((string= action-token "set")
         (let ((theme-token (second tokens)))
           (cond
             ((/= (length tokens) 2)
              (invalid-usage "Usage: /sounds set <theme>"))
             ((null (find-sound-theme theme-token))
              (invalid-usage (format nil "Unknown sound theme ~S." theme-token)))
             (t
              (let ((active (set-active-sound-theme theme-token)))
                (make-slash-command-result
                 :echo-input-p t
                 :output (format nil "Active sound theme set to ~A."
                                 (%sound-theme-label active))))))))
        ((string= action-token "preview")
         (let* ((category (or (%parse-sound-category (second tokens)) :error))
                (extra (third tokens)))
           (if extra
               (invalid-usage (format nil "Unexpected argument ~S." extra))
               (make-slash-command-result
                :echo-input-p t
                :output (%preview-sound category cfg)))))
        (t
         (invalid-usage (format nil "Unknown /sounds action ~S." action-token)))))))

(defun %notification-backend-label (backend-name)
  (string-downcase (symbol-name backend-name)))

(defun %notification-filter-label (filter)
  (cond
    ((eq filter :*)
     "*")
    ((listp filter)
     (format nil "~{~A~^, ~}"
             (mapcar (lambda (item)
                       (string-downcase (symbol-name item)))
                     filter)))
    (t
     (string-downcase (symbol-name filter)))))

(defun %ensure-notification-dispatcher-for-command (context)
  (let* ((cfg (or (slash-command-context-config context)
                  (%current-config-safe)))
         (bus (current-event-bus))
         (manager (ensure-notification-manager :config cfg :event-bus bus)))
    (or (and *notification-dispatcher*
             (notification-dispatcher-p *notification-dispatcher*)
             *notification-dispatcher*)
        (ensure-notification-dispatcher :manager manager :event-bus bus))))

(defun %render-notification-backend-list (dispatcher)
  (let ((entries (list-notification-dispatch-backends dispatcher)))
    (if (null entries)
        "No notification dispatch backends configured."
        (with-output-to-string (out)
          (format out "Notification backends (~D):~%" (length entries))
          (dolist (entry entries)
            (format out "- ~A enabled=~A priority=~D filter=~A~%"
                    (%notification-backend-label
                     (notification-dispatch-backend-name entry))
                    (if (notification-dispatch-backend-enabled-p entry)
                        "yes"
                        "no")
                    (notification-dispatch-backend-priority entry)
                    (%notification-filter-label
                     (notification-dispatch-backend-filter entry))))))))

(defun %parse-notification-event-type (token)
  (when (and token (plusp (length (%slash-trim token))))
    (%normalize-event-type token)))

(defun %notifications-handler (_invocation arguments context)
  (declare (ignore _invocation))
  (let* ((raw (or (gethash :ARGS arguments) ""))
         (tokens (%tokenize-command-arguments raw))
         (action-token (if tokens
                           (string-downcase (first tokens))
                           "list"))
         (dispatcher (%ensure-notification-dispatcher-for-command context)))
    (labels ((invalid-usage (&optional details)
               (make-slash-command-result
                :echo-input-p t
                :output (format nil "~@[~A~%~]Usage: ~A"
                                details
                                (%notifications-usage)))))
      (cond
        ((member action-token '("list" "ls") :test #'string=)
         (if (> (length tokens) 1)
             (invalid-usage (format nil "Unexpected argument ~S." (second tokens)))
             (make-slash-command-result
              :echo-input-p t
              :output (%render-notification-backend-list dispatcher))))
        ((member action-token '("enable" "disable") :test #'string=)
         (let ((backend-token (second tokens))
               (extra (third tokens)))
           (cond
             ((or (null backend-token) extra)
              (invalid-usage (format nil "Usage: /notifications ~A <backend>"
                                     action-token)))
             (t
              (handler-case
                  (let* ((entry (set-notification-dispatch-backend-enabled-p
                                 dispatcher
                                 backend-token
                                 (string= action-token "enable")))
                         (status (if (notification-dispatch-backend-enabled-p entry)
                                     "enabled"
                                     "disabled")))
                    (make-slash-command-result
                     :echo-input-p t
                     :output (format nil "Notification backend ~A is now ~A."
                                     (%notification-backend-label
                                      (notification-dispatch-backend-name entry))
                                     status)))
                (error (condition)
                  (invalid-usage (princ-to-string condition))))))))
        ((string= action-token "test-fire")
         (let* ((event-type (or (%parse-notification-event-type (second tokens))
                                +event-type-tool-error+))
                (extra (third tokens)))
           (if extra
               (invalid-usage (format nil "Unexpected argument ~S." extra))
               (multiple-value-bind (ok destination)
                   (fire-notification-dispatch-test :dispatcher dispatcher
                                                    :event-type event-type)
                 (make-slash-command-result
                  :echo-input-p t
                  :output (if ok
                              (format nil "Notification test dispatched via ~A for ~A."
                                      (%notification-backend-label destination)
                                      (string-downcase (symbol-name event-type)))
                              (format nil "Notification test fallback exhausted (~A) for ~A."
                                      destination
                                      (string-downcase (symbol-name event-type)))))))))
        (t
         (invalid-usage (format nil "Unknown /notifications action ~S." action-token)))))))

(defun %render-tts-status ()
  (let* ((cfg (%current-config-safe))
         (auto-p (and (config-p cfg)
                      (eq t (config-value :tts-auto-speak cfg))))
         (backend *tts-backend*)
         (active-voice
           (when (and backend (typep backend 'kokoro-tts-backend))
             (kokoro-tts-voice backend)))
         (configured-voice (and (config-p cfg) (config-value :tts-voice cfg)))
         (voice (or active-voice configured-voice *tts-default-voice*)))
    (format nil "TTS auto-speak: ~:[off~;on~], voice: ~A, speaking: ~:[no~;yes~]."
            auto-p
            voice
            (and backend (speaking-p backend)))))

(defun %speak-handler (_invocation arguments context)
  (declare (ignore _invocation))
  (let* ((raw (or (gethash :ARGS arguments) ""))
         (tokens (%tokenize-command-arguments raw))
         (action-token (if tokens
                           (string-downcase (first tokens))
                           "last"))
         (chat-state (slash-command-context-chat-state context)))
    (labels ((invalid-usage (&optional details)
               (make-slash-command-result
                :echo-input-p t
                :output (format nil "~@[~A~%~]Usage: ~A"
                                details
                                (%speak-usage)))))
      (cond
        ((member action-token '("last" "say" "speak") :test #'string=)
         (multiple-value-bind (ok text)
             (speak-last-assistant-response :chat-state chat-state)
           (if ok
               (make-slash-command-result
                :echo-input-p t
                :output (format nil "Speaking last assistant response (~D chars)."
                                (length (or text ""))))
               (make-slash-command-result
                :echo-input-p t
                :output "No assistant response available to speak yet."))))
        ((member action-token '("on" "off") :test #'string=)
         (let ((value (string= action-token "on")))
           (setconfig :tts-auto-speak value)
           (make-slash-command-result
            :echo-input-p t
            :output (format nil "TTS auto-speak ~:[disabled~;enabled~]." value))))
        ((member action-token '("status" "show") :test #'string=)
         (if (> (length tokens) 1)
             (invalid-usage (format nil "Unexpected argument ~S." (second tokens)))
             (make-slash-command-result
              :echo-input-p t
              :output (%render-tts-status))))
        ((string= action-token "stop")
         (let ((backend (or *tts-backend* (ensure-tts-backend))))
           (stop-speaking backend)
           (make-slash-command-result
            :echo-input-p t
            :output "TTS playback stopped.")))
        ((string= action-token "voice")
         (let ((voice-token (second tokens))
               (extra (third tokens)))
           (if (or (null voice-token) extra)
               (invalid-usage "Usage: /speak voice <name>")
               (let* ((trimmed (%slash-trim voice-token))
                      (backend (or *tts-backend*
                                   (ensure-tts-backend))))
                 (setconfig :tts-voice trimmed)
                 (set-voice backend trimmed)
                 (make-slash-command-result
                  :echo-input-p t
                  :output (format nil "TTS voice set to ~A." trimmed))))))
        (t
         (invalid-usage (format nil "Unknown /speak action ~S." action-token)))))))

(defun %hooks-usage ()
  "/hooks [list [hook-point]] | /hooks trace [limit] [hook-point]")

(defun %hook-point-name (hook-point)
  (string-downcase (subseq (symbol-name hook-point) 1)))

(defun %hook-point-definitions ()
  (let ((symbol (find-symbol "+HOOK-POINT-DEFINITIONS+" :amoebum)))
    (if (and symbol (boundp symbol))
        (symbol-value symbol)
        '())))

(defun %parse-hook-point-token (token)
  (when (and token (plusp (length (%slash-trim token))))
    (let ((candidate (intern (string-upcase (%slash-trim token)) :keyword)))
      (and (assoc candidate (%hook-point-definitions) :test #'eq)
           candidate))))

(defun %hook-point-token-completions (fragment)
  (let ((prefix (%slash-trim fragment)))
    (loop for spec in (%hook-point-definitions)
          for point = (car spec)
          for text = (%hook-point-name point)
          when (%starts-with-ci-p prefix text)
            collect text)))

(defun %render-hooks-list (&optional hook-point)
  (let ((entries (if hook-point
                     (list-hooks hook-point)
                     (list-hooks))))
    (if (null entries)
        (if hook-point
            (format nil "No hooks registered for ~A." (%hook-point-name hook-point))
            "No hooks registered.")
        (with-output-to-string (out)
          (format out "Registered hooks (~D):~%" (length entries))
          (dolist (entry entries)
            (format out "- ~A (~S) point=~A priority=~D async=~:[no~;yes~] enabled=~:[no~;yes~] on-error=~A budget=~Dms failures=~D/~D calls=~D total=~Dms~%"
                    (hook-entry-hook-id entry)
                    (hook-entry-hook-id entry)
                    (%hook-point-name (hook-entry-hook-point entry))
                    (hook-entry-priority entry)
                    (hook-entry-async-p entry)
                    (not (hook-entry-disabled-p entry))
                    (hook-entry-on-error entry)
                    (hook-entry-max-ms entry)
                    (hook-entry-consecutive-failures entry)
                    (hook-entry-failure-threshold entry)
                    (hook-entry-call-count entry)
                    (hook-entry-total-time-ms entry)))))))

(defun %render-hook-trace (&key (limit 20) hook-point)
  (let ((entries (hook-trace :limit limit :hook-point hook-point)))
    (if (null entries)
        "Hook trace is empty."
        (with-output-to-string (out)
          (format out "Hook trace (~D, newest first):~%" (length entries))
          (dolist (entry entries)
            (format out "- t=~D point=~A hook=~S status=~A elapsed=~Dms result=~S~@[ detail=~A~]~%"
                    (or (getf entry :timestamp) 0)
                    (%hook-point-name (getf entry :hook-point))
                    (getf entry :hook-id)
                    (getf entry :status)
                    (or (getf entry :elapsed-ms) 0)
                    (getf entry :result)
                    (getf entry :detail)))))))

(defun %hooks-handler (_invocation arguments _context)
  (declare (ignore _invocation _context))
  (let* ((raw (or (gethash :ARGS arguments) ""))
         (tokens (%tokenize-command-arguments raw))
         (action-token (and tokens (string-downcase (first tokens)))))
    (labels ((invalid-usage (&optional details)
               (make-slash-command-result
                :echo-input-p t
                :output (format nil "~@[~A~%~]Usage: ~A"
                                details
                                (%hooks-usage)))))
      (cond
        ((or (null action-token) (string= action-token "list"))
         (let* ((point-token (and (> (length tokens) 1) (second tokens)))
                (extra-token (and (> (length tokens) 2) (third tokens))))
           (when extra-token
             (return-from %hooks-handler
               (invalid-usage (format nil "Unexpected extra token ~S." extra-token))))
           (when (and point-token (null (%parse-hook-point-token point-token)))
             (return-from %hooks-handler
               (invalid-usage (format nil "Unknown hook-point ~S." point-token))))
           (make-slash-command-result
            :echo-input-p t
            :output (%render-hooks-list (%parse-hook-point-token point-token)))))
        ((string= action-token "trace")
         (let ((limit 20)
               (hook-point nil))
           (dolist (token (rest tokens))
             (cond
               ((ignore-errors (parse-integer token))
                (setf limit (parse-integer token)))
               ((%parse-hook-point-token token)
                (setf hook-point (%parse-hook-point-token token)))
               (t
                (return-from %hooks-handler
                  (invalid-usage (format nil "Unrecognized trace argument ~S." token))))))
           (when (<= limit 0)
             (return-from %hooks-handler
               (invalid-usage (format nil "Trace limit must be positive, got ~S." limit))))
           (make-slash-command-result
            :echo-input-p t
            :output (%render-hook-trace :limit limit :hook-point hook-point))))
        ((%parse-hook-point-token action-token)
         (make-slash-command-result
          :echo-input-p t
          :output (%render-hooks-list (%parse-hook-point-token action-token))))
        (t
         (invalid-usage (format nil "Unknown /hooks action ~S." action-token)))))))

(defun %lint-handler (_invocation arguments _context)
  (declare (ignore _invocation _context))
  (let* ((raw (or (gethash :PATHS arguments) ""))
         (paths (let ((tokens (%tokenize-command-arguments raw)))
                  (and tokens (not (null tokens)) tokens)))
         (run-symbol (find-symbol "RUN-MACRO-LINT" :amoebum))
         (format-symbol (find-symbol "FORMAT-MACRO-LINT-REPORT" :amoebum)))
    (cond
      ((or (null run-symbol)
           (null format-symbol)
           (not (fboundp run-symbol))
           (not (fboundp format-symbol)))
       (make-slash-command-result
        :echo-input-p t
        :output "/lint unavailable: compile validation module is not loaded."))
      (t
       (handler-case
           (let* ((report (if paths
                              (funcall (symbol-function run-symbol) :paths paths)
                              (funcall (symbol-function run-symbol))))
                  (output (funcall (symbol-function format-symbol) report)))
             (make-slash-command-result
              :echo-input-p t
              :output output))
         (error (condition)
           (make-slash-command-result
            :echo-input-p t
            :output (format nil "/lint failed: ~A" condition))))))))

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
    (loop for option in '("on" "off" "status" "review"
                          "approve" "reorder" "reject" "modify"
                          "request-modifications" "request-changes")
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

(defun %switch-fork-arg-completer (_command _invocation index fragment _prefix)
  (declare (ignore _command _invocation _prefix))
  (if (= index 0)
      (let ((prefix (%slash-trim fragment)))
        (loop for option in '("main")
              when (%starts-with-ci-p prefix option)
                collect option))
      '()))

(defun %tool-history-arg-completer (_command _invocation index fragment _prefix-tokens)
  (declare (ignore _command _invocation _prefix-tokens))
  (if (= index 0)
      (%tool-name-completions fragment)
      '()))

(defun %tool-rollback-arg-completer (_command _invocation index fragment _prefix-tokens)
  (declare (ignore _command _invocation _prefix-tokens))
  (cond
    ((= index 0)
     (%tool-name-completions fragment))
    ((= index 1)
     (let ((prefix (%slash-trim fragment)))
       (loop for option in '("1" "2" "3" "4" "5")
             when (%starts-with-ci-p prefix option)
               collect option)))
    (t
     '())))

(defun %hooks-arg-completer (_command _invocation index fragment prefix-tokens)
  (declare (ignore _command _invocation))
  (let ((head (and prefix-tokens (string-downcase (first prefix-tokens)))))
    (cond
      ((= index 0)
       (append (loop for option in '("list" "trace")
                     when (%starts-with-ci-p (%slash-trim fragment) option)
                       collect option)
               (%hook-point-token-completions fragment)))
      ((and (string= head "trace") (= index 1))
       (append (loop for option in '("10" "20" "50")
                     when (%starts-with-ci-p (%slash-trim fragment) option)
                       collect option)
               (%hook-point-token-completions fragment)))
      ((and (string= head "trace") (= index 2))
       (%hook-point-token-completions fragment))
      ((and (string= head "list") (= index 1))
       (%hook-point-token-completions fragment))
      (t
       nil))))

(defun %extensions-arg-completer (_command _invocation index fragment prefix-tokens)
  (declare (ignore _command _invocation))
  (let ((head (and prefix-tokens (string-downcase (first prefix-tokens))))
        (prefix (%slash-trim fragment)))
    (cond
      ((= index 0)
       (loop for option in '("list" "reload" "enable" "disable")
             when (%starts-with-ci-p prefix option)
               collect option))
      ((and (member head '("enable" "disable") :test #'string=) (= index 1))
       (let ((targets (append '("all") (%extensions-known-targets))))
         (loop for option in targets
               when (%starts-with-ci-p prefix option)
                 collect option)))
      (t
       nil))))

(defun %sounds-arg-completer (_command _invocation index fragment prefix-tokens)
  (declare (ignore _command _invocation))
  (let* ((head (and prefix-tokens (string-downcase (first prefix-tokens))))
         (prefix (%slash-trim fragment))
         (theme-options (mapcar #'%sound-theme-label (list-sound-theme-names)))
         (category-options '("error" "task-complete" "approval-needed")))
    (cond
      ((= index 0)
       (loop for option in '("list" "set" "preview")
             when (%starts-with-ci-p prefix option)
               collect option))
      ((and (string= head "set") (= index 1))
       (loop for option in theme-options
             when (%starts-with-ci-p prefix option)
               collect option))
      ((and (string= head "preview") (= index 1))
       (loop for option in category-options
             when (%starts-with-ci-p prefix option)
               collect option))
      (t
       nil))))

(defun %notifications-arg-completer (_command _invocation index fragment prefix-tokens)
  (declare (ignore _command _invocation))
  (let* ((head (and prefix-tokens (string-downcase (first prefix-tokens))))
         (prefix (%slash-trim fragment))
         (backends (mapcar (lambda (entry)
                             (%notification-backend-label
                              (notification-dispatch-backend-name entry)))
                           (list-notification-dispatch-backends
                            (or *notification-dispatcher*
                                (ignore-errors
                                  (%ensure-notification-dispatcher-for-command
                                   (make-slash-command-context))))))))
    (cond
      ((= index 0)
       (loop for option in '("list" "enable" "disable" "test-fire")
             when (%starts-with-ci-p prefix option)
               collect option))
      ((and (member head '("enable" "disable") :test #'string=) (= index 1))
       (loop for option in backends
             when (%starts-with-ci-p prefix option)
               collect option))
      ((and (string= head "test-fire") (= index 1))
       (loop for option in (mapcar (lambda (event-type)
                                     (string-downcase (symbol-name event-type)))
                                   +core-event-types+)
             when (%starts-with-ci-p prefix option)
               collect option))
      (t
       nil))))

(defun %speak-arg-completer (_command _invocation index fragment prefix-tokens)
  (declare (ignore _command _invocation))
  (let* ((prefix (%slash-trim fragment))
         (action (and prefix-tokens (string-downcase (first prefix-tokens)))))
    (cond
      ((= index 0)
       (loop for option in '("last" "on" "off" "status" "stop" "voice")
             when (%starts-with-ci-p prefix option)
               collect option))
      ((and (string= action "voice") (= index 1))
       (let* ((backend (or *tts-backend* (ignore-errors (ensure-tts-backend))))
              (voices (or (and backend (ignore-errors (list-voices backend)))
                          (copy-list *tts-default-voices*))))
         (loop for option in voices
               when (%starts-with-ci-p prefix option)
                 collect option)))
      (t nil))))

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

;;; ---------------------------------------------------------------------------
;;; Phase 5 command handlers (I95-I102)
;;; ---------------------------------------------------------------------------

(defvar *model-router* nil
  "Global model router instance, set during configuration.")

(defparameter +cost-default-interaction-count+ 5)

(defun %models-handler (_invocation arguments _context)
  (declare (ignore _invocation _context))
  (let ((filter (gethash :PROVIDER arguments)))
    (if *model-router*
        (let ((status (pseudopod:router-status *model-router*)))
          (with-output-to-string (out)
            (format out "Router strategy: ~A~%Providers: ~A healthy / ~A total~%"
                    (getf status :strategy)
                    (getf status :healthy-providers)
                    (getf status :total-providers))
            (dolist (p (getf status :providers))
              (when (or (null filter)
                        (string-equal filter (getf p :name)))
                (format out "  ~A (~A) ~:[UNHEALTHY~;OK~] — ~A reqs, ~A errs, ~Ams~%"
                        (getf p :name) (getf p :model) (getf p :healthy)
                        (getf p :requests) (getf p :errors) (getf p :last-latency-ms))))))
        (make-slash-command-result
         :output "No model router configured. Set up providers in config."))))

(defun %providers-handler (_invocation arguments _context)
  (declare (ignore _invocation _context))
  (let* ((raw-action (gethash :ACTION arguments))
         (action (and (stringp raw-action)
                      (string-downcase (%slash-trim raw-action)))))
    (cond
      ((string-equal action "on")
       (make-slash-command-result
        :echo-input-p t
        :action :toggle-provider-dashboard
        :payload :on))
      ((string-equal action "off")
       (make-slash-command-result
        :echo-input-p t
        :action :toggle-provider-dashboard
        :payload :off))
      (t
       (make-slash-command-result
        :echo-input-p t
        :action :toggle-provider-dashboard
        :payload :toggle)))))

(defun %slash-message-text (message)
  (cond
    ((typep message 'pseudopod:message)
     (with-output-to-string (out)
       (dolist (part (pseudopod:message-content message))
         (when (and (pseudopod:content-part-p part)
                    (stringp (pseudopod:content-part-text part)))
           (write-string (pseudopod:content-part-text part) out)
           (write-char #\Space out)))))
    ((stringp message)
     message)
    (t
     (princ-to-string message))))

(defun %cost-recent-interactions (messages count)
  (let ((all '())
        (pending-user nil))
    (dolist (message messages)
      (when (typep message 'pseudopod:message)
        (let ((role (string-downcase (or (pseudopod:message-role message) ""))))
          (cond
            ((string= role "user")
             (setf pending-user message))
            ((and (string= role "assistant") pending-user)
             (push (list :user pending-user :assistant message) all)
             (setf pending-user nil))))))
    (let ((ordered (nreverse all)))
      (if (<= (length ordered) count)
          ordered
          (subseq ordered (- (length ordered) count))))))

(defun %format-usd (amount)
  (format nil "$~,6F" (coerce amount 'double-float)))

(defun %cost-handler (_invocation arguments context)
  (declare (ignore _invocation))
  (let* ((chat-state (slash-command-context-chat-state context))
         (raw-count (gethash :COUNT arguments))
         (count (max 1 (or raw-count +cost-default-interaction-count+))))
    (cond
      ((null *model-router*)
       (make-slash-command-result
        :output "No model router configured. Set up providers in config."))
      ((not (typep chat-state 'chat-ui-state))
       (make-slash-command-result
        :output "Cost estimation requires an active chat session."))
      (t
       (let* ((messages (chat-ui-state-messages chat-state))
              (interactions (%cost-recent-interactions messages count)))
         (if (null interactions)
             (make-slash-command-result
              :output "No completed user/assistant interactions to estimate yet.")
             (let ((rows '())
                   (total 0.0d0)
                   (index 0))
               (dolist (interaction interactions)
                 (incf index)
                 (let* ((user-message (getf interaction :user))
                        (assistant-message (getf interaction :assistant))
                        (input-messages (list user-message))
                        (provider (or (pseudopod:router-select-provider
                                       *model-router*
                                       :messages input-messages)
                                      (first (pseudopod:model-router-providers *model-router*))))
                        (assistant-tokens
                          (if provider
                              (max 0
                                   (pseudopod:estimate-provider-tokens
                                    provider
                                    (%slash-message-text assistant-message)))
                              0))
                        (estimate
                          (if provider
                              (pseudopod:cost-estimate provider
                                                       input-messages
                                                       :output-tokens assistant-tokens)
                              (list :total-cost-usd 0.0d0
                                    :input-tokens 0
                                    :output-tokens assistant-tokens
                                    :provider "n/a"
                                    :model "n/a"))))
                   (incf total (getf estimate :total-cost-usd 0.0d0))
                   (push (format nil "~D. ~A (~A): in ~D tok, out ~D tok -> ~A"
                                 index
                                 (getf estimate :provider "n/a")
                                 (getf estimate :model "n/a")
                                 (getf estimate :input-tokens 0)
                                 (getf estimate :output-tokens 0)
                                 (%format-usd (getf estimate :total-cost-usd 0.0d0)))
                         rows)))
               (make-slash-command-result
                :output (with-output-to-string (out)
                          (format out "Estimated cost for last ~D interaction~:P:~%"
                                  (length interactions))
                          (dolist (row (nreverse rows))
                            (format out "~A~%" row))
                          (format out "Total: ~A" (%format-usd total)))))))))))

(defun %index-handler (_invocation arguments _context)
  (declare (ignore _invocation _context))
  (let ((args-text (or (gethash :ARGS arguments) "")))
    (handler-case
        (let ((refresh-p (search "--refresh" args-text :test #'char-equal)))
          (make-slash-command-result
           :output (format nil "~A codebase index."
                           (if refresh-p "Refreshed" "Generated"))))
      (error (c)
        (make-slash-command-result
         :output (format nil "Index error: ~A" c))))))

(defun %self-modify-handler (_invocation arguments _context)
  (declare (ignore _invocation _context))
  (let ((form-text (or (gethash :FORM arguments) "")))
    (if (zerop (length (%slash-trim form-text)))
        (make-slash-command-result
         :output "Usage: /self-modify <lisp-form>")
        (make-slash-command-result
         :output (format nil "Self-modify proposed: ~A~%Use /approvals to review."
                         form-text)))))

(defun %image-handler (_invocation arguments _context)
  (declare (ignore _invocation _context))
  (let* ((args-text (or (gethash :ARGS arguments) ""))
         (tokens (%tokenize-command-arguments args-text))
         (action (string-downcase (or (first tokens) "list"))))
    (cond
      ((string= action "save")
       (make-slash-command-result
        :output "Image save requested. Use save-amoebum-image for actual persistence."))
      ((string= action "restore")
       (make-slash-command-result
        :output "Image restore requested. Specify path to restore from."))
      ((string= action "rotate")
       (make-slash-command-result
        :output "Image rotation: keeping latest 5 images."))
      (t
       (make-slash-command-result
        :output "Image management: /image save|restore|list|rotate")))))

(defun %extensions-asdf-handler (_invocation arguments _context)
  (declare (ignore _invocation _context))
  (let* ((args-text (or (gethash :ARGS arguments) ""))
         (tokens (%tokenize-command-arguments args-text))
         (action (string-downcase (or (first tokens) "list"))))
    (cond
      ((string= action "discover")
       (make-slash-command-result
        :output "Discovering ASDF extensions in Quicklisp local-projects and ~/.amoebum/systems/..."))
      ((string= action "load")
       (let ((system-name (second tokens)))
         (if system-name
             (make-slash-command-result
              :output (format nil "Loading ASDF extension: ~A" system-name))
             (make-slash-command-result
              :output "Usage: /extensions-asdf load <system-name>"))))
      ((string= action "unload")
       (let ((system-name (second tokens)))
         (if system-name
             (make-slash-command-result
              :output (format nil "Unloading ASDF extension: ~A" system-name))
             (make-slash-command-result
              :output "Usage: /extensions-asdf unload <system-name>"))))
      (t
       (make-slash-command-result
        :output "ASDF extensions: /extensions-asdf list|load|unload|discover")))))

(defun %perf-handler (_invocation arguments _context)
  (%profile-handler _invocation arguments _context))

(defun %profile-handler (_invocation arguments _context)
  (declare (ignore _invocation _context))
  (let* ((args-text (or (gethash :ARGS arguments) ""))
         (tokens (%tokenize-command-arguments args-text))
         (action (string-downcase (or (first tokens) "report"))))
    (cond
      ((string= action "start")
       (start-profiling)
       (make-slash-command-result
        :output "Profiling started. Run /profile stop to capture a report."))
      ((string= action "stop")
       (let ((report (stop-profiling)))
         (make-slash-command-result
          :output (render-profiling-report-table :report report))))
      ((string= action "report")
       (make-slash-command-result
        :output (render-profiling-report-table :report (report-profiling))))
      (t
       (make-slash-command-result
        :output "Profiling: /profile start|stop|report")))))

(defun %voice-usage ()
  "/voice [on|off|toggle|status|language <code>]")

(defun %voice-handler (_invocation arguments _context)
  (declare (ignore _invocation _context))
  (let* ((args-text (or (gethash :ARGS arguments) ""))
         (tokens (%tokenize-command-arguments args-text))
         (action (if tokens
                     (string-downcase (first tokens))
                     "toggle"))
         (backend (ensure-asr-backend)))
    (labels ((status-output ()
               (format nil "Voice input ~:[disabled~;enabled~], listening=~:[no~;yes~], language=~A."
                       (voice-input-mode-enabled-p)
                       (listening-p backend)
                       (whisper-asr-backend-language backend)))
             (invalid-usage (&optional detail)
               (make-slash-command-result
                :echo-input-p t
                :output (format nil "~@[~A~%~]Usage: ~A"
                                detail
                                (%voice-usage)))))
      (cond
        ((member action '("on" "enable") :test #'string=)
         (enable-voice-input-mode :backend backend)
         (make-slash-command-result :echo-input-p t :output (status-output)))
        ((member action '("off" "disable") :test #'string=)
         (disable-voice-input-mode :backend backend)
         (make-slash-command-result :echo-input-p t :output (status-output)))
        ((string= action "toggle")
         (toggle-voice-input-mode :backend backend)
         (make-slash-command-result :echo-input-p t :output (status-output)))
        ((string= action "status")
         (make-slash-command-result :echo-input-p t :output (status-output)))
        ((member action '("language" "lang") :test #'string=)
         (let ((language (second tokens)))
           (if (or (null language)
                   (%slash-blank-p language))
               (invalid-usage "Missing language code.")
               (progn
                 (set-language backend language)
                 (make-slash-command-result
                  :echo-input-p t
                  :output (status-output))))))
        (t
         (invalid-usage (format nil "Unknown /voice action ~S." action)))))))

(defun %voice-arg-completer (_command _invocation index fragment _prefix-tokens)
  (if (= index 0)
      (let ((prefix (%slash-trim fragment)))
        (loop for option in '("on" "off" "toggle" "status" "language")
              when (%starts-with-ci-p prefix option)
                collect option))
      nil))

(defun %mcp-status-tool-count (server-name)
  (let ((normalized (string-downcase (%slash-trim (princ-to-string server-name))))
        (count 0))
    (maphash (lambda (_name binding)
               (declare (ignore _name))
               (when (string= (mcp-tool-binding-server-name binding) normalized)
                 (incf count)))
             *mcp-tool-binding-registry*)
    count))

(defun %mcp-status-handler (_invocation _arguments _context)
  (declare (ignore _invocation _arguments _context))
  (let ((servers '()))
    (maphash (lambda (_name server)
               (declare (ignore _name))
               (push server servers))
             *mcp-tool-server-registry*)
    (if (null servers)
        (make-slash-command-result
         :echo-input-p t
         :output "No MCP servers are currently registered.")
        (make-slash-command-result
         :echo-input-p t
         :output
         (with-output-to-string (out)
           (format out "MCP servers: ~D~%" (length servers))
           (dolist (server (sort (copy-list servers) #'string<
                                 :key #'mcp-server-name))
             (let* ((info (mcp-server-server-info server))
                    (status (if (mcp-server-running-p server) "running" "stopped"))
                    (protocol (or (and info (mcp-server-info-protocol-version info))
                                  "unknown"))
                    (match (if (and info (mcp-server-info-protocol-version-match-p info))
                               "match"
                               "mismatch"))
                    (capabilities (or (mcp-server-capability-summary server) '()))
                    (declared-count (length (or (and info (mcp-server-info-declared-tools info))
                                                '())))
                    (discovered-count (%mcp-status-tool-count (mcp-server-name server))))
               (format out
                       "- ~A (~A) protocol=~A [~A] capabilities=~:[none~;~{~A~^, ~}~] declared-tools=~D discovered-tools=~D~%"
                       (mcp-server-name server)
                       status
                       protocol
                       match
                       capabilities
                       capabilities
                       declared-count
                       discovered-count))))))))

(defun %mcp-auth-usage ()
  "/mcp-auth [list|set <server|default> <allow|deny|prompt>|clear <server|all>]")

(defun %mcp-auth-string (value)
  (cond
    ((null value) "")
    ((stringp value) value)
    ((symbolp value) (symbol-name value))
    (t (princ-to-string value))))

(defun %mcp-auth-normalize-server (value)
  (let ((trimmed (string-downcase (%slash-trim (%mcp-auth-string value)))))
    (cond
      ((or (string= trimmed "")
           (string= trimmed "*")
           (string= trimmed "default"))
       "default")
      ((uiop:string-prefix-p "mcp/" trimmed)
       (let* ((rest (subseq trimmed (length "mcp/")))
              (separator (position #\/ rest)))
         (if (and separator (> separator 0))
             (subseq rest 0 separator)
             rest)))
      (t trimmed))))

(defun %mcp-auth-normalize-decision (value)
  (let ((trimmed (string-downcase (%slash-trim (%mcp-auth-string value)))))
    (cond
      ((string= trimmed "allow") :allow)
      ((string= trimmed "deny") :deny)
      ((or (string= trimmed "prompt")
           (string= trimmed "ask")) :prompt)
      (t nil))))

(defun %mcp-auth-config-pairs (value)
  (cond
    ((hash-table-p value)
     (loop for key being the hash-keys of value using (hash-value decision)
           collect (cons key decision)))
    ((and (listp value)
          (every #'consp value))
     value)
    ((and (listp value)
          (evenp (length value)))
     (loop for (key decision) on value by #'cddr
           collect (cons key decision)))
    (t nil)))

(defun %mcp-auth-normalized-rules ()
  (let* ((cfg (current-config))
         (raw (and cfg (config-value :mcp-server-permissions cfg)))
         (rules '()))
    (dolist (entry (%mcp-auth-config-pairs raw))
      (let* ((server (%mcp-auth-normalize-server (car entry)))
             (decision (%mcp-auth-normalize-decision (cdr entry))))
        (when (and decision
                   server
                   (not (assoc server rules :test #'string=)))
          (push (cons server decision) rules))))
    (nreverse rules)))

(defun %mcp-auth-render-rules (&optional (rules (%mcp-auth-normalized-rules)))
  (with-output-to-string (out)
    (let ((default (or (cdr (assoc "default" rules :test #'string=))
                       :prompt)))
      (format out "MCP authorization rules:~%")
      (format out "- default: ~A~%"
              (string-downcase (symbol-name default)))
      (dolist (entry (sort (remove-if (lambda (entry)
                                        (string= (car entry) "default"))
                                      (copy-list rules))
                           #'string<
                           :key #'car))
        (format out "- ~A: ~A~%"
                (car entry)
                (string-downcase (symbol-name (cdr entry))))))))

(defun %mcp-auth-upsert-rule (rules server decision)
  (let* ((normalized-server (%mcp-auth-normalize-server server))
         (existing (assoc normalized-server rules :test #'string=)))
    (if existing
        (setf (cdr existing) decision)
        (push (cons normalized-server decision) rules))
    rules))

(defun %mcp-auth-remove-rule (rules server)
  (let ((normalized-server (%mcp-auth-normalize-server server)))
    (remove-if (lambda (entry)
                 (string= (car entry) normalized-server))
               rules)))

(defun %mcp-auth-known-server-names ()
  (let ((names '()))
    (dolist (entry (%mcp-auth-normalized-rules))
      (unless (string= (car entry) "default")
        (pushnew (car entry) names :test #'string=)))
    (sort names #'string<)))

(defun %mcp-auth-handler (_invocation arguments _context)
  (declare (ignore _invocation _context))
  (let* ((raw (or (gethash :ARGS arguments) ""))
         (tokens (%tokenize-command-arguments raw))
         (action (if tokens
                     (string-downcase (first tokens))
                     "list")))
    (labels ((invalid-usage (&optional detail)
               (make-slash-command-result
                :echo-input-p t
                :output (format nil "~@[~A~%~]Usage: ~A"
                                detail
                                (%mcp-auth-usage)))))
      (cond
        ((member action '("list" "ls") :test #'string=)
         (if (> (length tokens) 1)
             (invalid-usage (format nil "Unexpected argument ~S." (second tokens)))
             (make-slash-command-result
              :echo-input-p t
              :output (%mcp-auth-render-rules))))
        ((string= action "set")
         (let* ((server-token (second tokens))
                (decision-token (third tokens))
                (extra (fourth tokens))
                (server (%mcp-auth-normalize-server server-token))
                (decision (%mcp-auth-normalize-decision decision-token)))
           (cond
             (extra
              (invalid-usage (format nil "Unexpected argument ~S." extra)))
             ((or (null server-token) (%slash-blank-p server-token))
              (invalid-usage "Missing server token for set action."))
             ((null decision)
              (invalid-usage (format nil "Unknown MCP decision ~S." decision-token)))
             (t
              (let* ((updated (%mcp-auth-upsert-rule (%mcp-auth-normalized-rules)
                                                     server
                                                     decision))
                     (next-rules
                       (sort (copy-list updated) #'string< :key #'car)))
                (setconfig :mcp-server-permissions next-rules)
                (make-slash-command-result
                 :echo-input-p t
                 :output (format nil "Set MCP authorization for ~A to ~A."
                                 server
                                 (string-downcase (symbol-name decision)))))))))
        ((string= action "clear")
         (let ((target (second tokens))
               (extra (third tokens)))
           (cond
             (extra
              (invalid-usage (format nil "Unexpected argument ~S." extra)))
             ((or (null target) (%slash-blank-p target))
              (invalid-usage "Missing server token for clear action."))
             ((member (string-downcase (%slash-trim target))
                      '("all" "*")
                      :test #'string=)
              (setconfig :mcp-server-permissions nil)
              (make-slash-command-result
               :echo-input-p t
               :output "Cleared all MCP authorization overrides (default prompt)."))
             (t
              (let* ((server (%mcp-auth-normalize-server target))
                     (updated (%mcp-auth-remove-rule (%mcp-auth-normalized-rules)
                                                     server)))
                (setconfig :mcp-server-permissions updated)
                (make-slash-command-result
                 :echo-input-p t
                 :output (format nil "Cleared MCP authorization override for ~A."
                                 server)))))))
        (t
         (invalid-usage (format nil "Unknown /mcp-auth action ~S." action)))))))

(defun %mcp-auth-arg-completer (_command _invocation index fragment prefix-tokens)
  (declare (ignore _command _invocation))
  (let ((prefix (%slash-trim fragment))
        (action (and prefix-tokens (string-downcase (first prefix-tokens)))))
    (cond
      ((= index 0)
       (loop for option in '("list" "set" "clear")
             when (%starts-with-ci-p prefix option)
               collect option))
      ((and (string= action "set") (= index 1))
       (loop for option in (append '("default") (%mcp-auth-known-server-names))
             when (%starts-with-ci-p prefix option)
               collect option))
      ((and (string= action "set") (= index 2))
       (loop for option in '("allow" "deny" "prompt")
             when (%starts-with-ci-p prefix option)
               collect option))
      ((and (string= action "clear") (= index 1))
       (loop for option in (append '("all" "default") (%mcp-auth-known-server-names))
             when (%starts-with-ci-p prefix option)
               collect option))
      (t nil))))

(defun %spawn-handler (_invocation arguments _context)
  (declare (ignore _invocation _context))
  (let ((task-text (or (gethash :TASK arguments) "")))
    (if (zerop (length (%slash-trim task-text)))
        (make-slash-command-result
         :output "Usage: /spawn <task-description>")
        (handler-case
            (let* ((agent (spawn-agent task-text :agent-type :task))
                   (agent-id (agent-record-id agent)))
              (make-slash-command-result
               :echo-input-p t
               :output (format nil "Spawned agent ~A for task: ~A"
                               agent-id
                               task-text)))
          (error (condition)
            (make-slash-command-result
             :echo-input-p t
             :output (format nil "Failed to spawn agent: ~A" condition)))))))

(defun %approvals-handler (_invocation arguments _context)
  (declare (ignore _invocation _context))
  (let* ((args-text (or (gethash :ARGS arguments) ""))
         (tokens (%tokenize-command-arguments args-text))
         (action (string-downcase (or (first tokens) "status")))
         (policy-token (second tokens))
         (cfg (%current-config-safe))
         (current-policy (config-value :approval-policy cfg)))
    (cond
      ((or (string= action "status")
           (string= action "list"))
       (make-slash-command-result
        :output (format nil
                        "Approval policy: ~A (presets: untrusted, on-failure, on-request, never)."
                        (string-downcase
                         (symbol-name (or current-policy :on-request))))))
      ((string= action "set")
       (if (null policy-token)
           (make-slash-command-result
            :output "Usage: /approvals set <untrusted|on-failure|on-request|never>")
           (let ((normalized (%approval-policy-keyword policy-token)))
             (if (member normalized *known-approval-policies* :test #'eq)
                 (progn
                   (setconfig :approval-policy normalized)
                   (make-slash-command-result
                    :output (format nil "Approval policy set to ~A."
                                    (string-downcase (symbol-name normalized)))))
                 (make-slash-command-result
                  :output (format nil
                                  "Unknown approval policy ~S. Valid values: untrusted, on-failure, on-request, never."
                                  policy-token))))))
      (t
      (make-slash-command-result
       :output "Approvals: /approvals status | /approvals set <policy>")))))

(defun %permissions-usage ()
  "/permissions [stats|log [limit]|explain [decision-id|latest]]")

(defun %permissions-format-trace (trace)
  (if (null trace)
      "No permission decision trace available."
      (with-output-to-string (out)
        (format out "decision-id=~A decision=~A mode=~A tool=~A~%"
                (getf trace :decision-id)
                (getf trace :decision)
                (getf trace :permission-mode)
                (getf trace :tool))
        (when (getf trace :path)
          (format out "  path: ~A~%" (getf trace :path)))
        (when (getf trace :command)
          (format out "  command: ~A~%" (getf trace :command)))
        (dolist (phase (getf trace :evaluation-trace))
          (format out "  phase=~A matched-rule-id=~A specificity=~A effect=~A cache=~A~%"
                  (getf phase :phase)
                  (or (getf phase :matched-rule-id) "none")
                  (or (getf phase :specificity) 0)
                  (or (getf phase :effect) :none)
                  (or (getf phase :cache) :n/a))))))

(defun %permissions-handler (_invocation arguments _context)
  (declare (ignore _invocation _context))
  (let* ((raw (or (gethash :ARGS arguments) ""))
         (tokens (%tokenize-command-arguments raw))
         (action (if tokens (string-downcase (first tokens)) "stats")))
    (labels ((invalid-usage (&optional detail)
               (make-slash-command-result
                :echo-input-p t
                :output (format nil "~@[~A~%~]Usage: ~A"
                                detail
                                (%permissions-usage)))))
      (cond
        ((member action '("stats" "status") :test #'string=)
         (let ((metrics (permission-cache-metrics)))
           (make-slash-command-result
            :echo-input-p t
            :output (format nil "Permission cache: hits=~D misses=~D invalidations=~D rules-version=~D entries=~D"
                            (or (getf metrics :hits) 0)
                            (or (getf metrics :misses) 0)
                            (or (getf metrics :invalidations) 0)
                            (or (getf metrics :rules-version) 0)
                            (or (getf metrics :entries) 0)))))
        ((string= action "log")
         (let* ((limit-token (second tokens))
                (limit (if limit-token
                           (handler-case
                               (max 1 (parse-integer limit-token))
                             (error () nil))
                           5))
                (entries (and limit (permission-decision-history :limit limit))))
           (if (null limit)
               (invalid-usage (format nil "Invalid log limit ~S." limit-token))
               (make-slash-command-result
                :echo-input-p t
                :output (if entries
                            (with-output-to-string (out)
                              (format out "Permission decisions (~D):~%" (length entries))
                              (dolist (entry entries)
                                (format out "- ~A~%" (%permissions-format-trace entry))))
                            "No permission decisions recorded yet.")))))
        ((string= action "explain")
         (let* ((decision-id (or (second tokens) "latest"))
                (payload (explain-permission-decision :decision-id decision-id)))
           (if payload
               (make-slash-command-result
                :echo-input-p t
                :output (with-output-to-string (out)
                          (format out "Historical:~%~A~%Replay:~%~A"
                                  (%permissions-format-trace (getf payload :historical))
                                  (%permissions-format-trace (getf payload :replay))))
                :payload payload)
               (make-slash-command-result
                :echo-input-p t
                :output (format nil "No decision trace found for ~A." decision-id)))))
        (t
         (invalid-usage (format nil "Unknown /permissions action ~S." action)))))))

(defun %permissions-arg-completer (_command _invocation index fragment prefix-tokens)
  (declare (ignore _command _invocation))
  (let ((prefix (%slash-trim fragment))
        (action (and prefix-tokens (string-downcase (first prefix-tokens)))))
    (cond
      ((= index 0)
       (loop for option in '("stats" "log" "explain")
             when (%starts-with-ci-p prefix option)
               collect option))
      ((and (string= action "explain") (= index 1))
       (let* ((entries (permission-decision-history :limit 20))
              (ids (cons "latest"
                         (remove nil
                                 (mapcar (lambda (entry) (getf entry :decision-id)) entries)))))
         (loop for option in ids
               when (%starts-with-ci-p prefix option)
                 collect option)))
      (t nil))))

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
    :name "config"
    :description "Show resolved configuration values with layer provenance."
    :usage "/config"
    :handler #'%config-handler))
  (register-slash-command
   (make-slash-command
    :name "plan"
    :description "Toggle plan mode, review captured plan output, reorder steps, record review decisions, and approve specific steps."
    :usage "/plan [on|off|status|review|approve|reorder|reject|modify|request-modifications|request-changes] [args...]"
    :parameters
    (list (make-slash-command-parameter
           :name "state"
           :type :keyword
           :required-p nil
           :choices '(:on :off :status :review :approve :reorder :reject :modify
                      :request-modifications :request-changes)
           :description "Optional explicit plan mode action.")
          (make-slash-command-parameter
           :name "args"
           :type :string
           :required-p nil
           :greedy-p t
           :description "Optional action arguments (e.g. /plan off false, /plan approve 1,3, /plan reorder 3 1, /plan modify <notes>)."))
    :handler #'%plan-handler
    :completer #'%plan-arg-completer))
  (register-slash-command
   (make-slash-command
    :name "execute"
    :description "Transition from approved plan review into execution mode."
    :usage "/execute"
    :handler #'%execute-handler))
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
    :description "Reset the active conversation (requires confirmation)."
    :usage "/clear --yes"
    :parameters
    (list (make-slash-command-parameter
           :name "args"
           :type :string
           :required-p nil
           :greedy-p t
           :description "Pass --yes to confirm reset."))
    :handler #'%clear-handler))
  (register-slash-command
   (make-slash-command
    :name "history"
    :description "Search persisted conversation history by content/role/tool/time."
    :usage "/history [query...] [--role system|user|assistant|tool] [--tool NAME] [--since TIMESTAMP] [--until TIMESTAMP] [--limit N]"
    :parameters
    (list (make-slash-command-parameter
           :name "args"
           :type :string
           :required-p nil
           :greedy-p t
           :description "Optional query text and filters."))
    :handler #'%history-handler))
  (register-slash-command
   (make-slash-command
    :name "tool-history"
    :description "Show hot-reload history versions for a tool."
    :usage "/tool-history <tool-name>"
    :parameters
    (list (make-slash-command-parameter
           :name "name"
           :type :string
           :required-p t
           :description "Tool name to inspect."))
    :handler #'%tool-history-handler
    :completer #'%tool-history-arg-completer))
  (register-slash-command
   (make-slash-command
    :name "tool-rollback"
    :description "Restore a previous hot-reload version of a tool."
    :usage "/tool-rollback <tool-name> [version]"
    :parameters
    (list (make-slash-command-parameter
           :name "name"
           :type :string
           :required-p t
           :description "Tool name to roll back.")
          (make-slash-command-parameter
           :name "version"
           :type :integer
           :required-p nil
           :default 1
           :description "History version index (1 = most recent previous)."))
    :handler #'%tool-rollback-handler
    :completer #'%tool-rollback-arg-completer))
  (register-slash-command
   (make-slash-command
    :name "fork"
    :description "Create a named conversation fork at the current or specified message index."
    :usage (%fork-usage)
    :parameters
    (list (make-slash-command-parameter
           :name "args"
           :type :string
           :required-p nil
           :greedy-p t
           :description "Fork name and optional message index."))
    :handler #'%fork-handler))
  (register-slash-command
   (make-slash-command
    :name "forks"
    :description "List conversation forks with branch point and message count."
    :usage "/forks"
    :handler #'%forks-handler))
  (register-slash-command
   (make-slash-command
    :name "switch-fork"
    :description "Switch active conversation fork."
    :usage "/switch-fork <name>"
    :parameters
    (list (make-slash-command-parameter
           :name "name"
           :type :string
           :required-p t
           :description "Fork name to activate."))
    :handler #'%switch-fork-handler
    :completer #'%switch-fork-arg-completer))
  (register-slash-command
   (make-slash-command
    :name "extensions"
    :description "List, reload, enable, or disable user extensions."
    :usage (%extensions-usage)
    :parameters
    (list (make-slash-command-parameter
           :name "args"
           :type :string
           :required-p nil
           :greedy-p t
           :description "Optional subcommand and target."))
    :handler #'%extensions-handler
    :completer #'%extensions-arg-completer))
  (register-slash-command
   (make-slash-command
    :name "checkpoint"
    :description "Save a session checkpoint, list checkpoints, or restore one."
    :usage (%checkpoint-usage)
    :parameters
    (list (make-slash-command-parameter
           :name "args"
           :type :string
           :required-p nil
           :greedy-p t
           :description "Optional action: save, list, restore <id>."))
    :handler #'%checkpoint-handler
    :completer #'%checkpoint-arg-completer))
  (register-slash-command
   (make-slash-command
    :name "hooks"
    :description "Inspect hook registration state and recent hook trace events."
    :usage (%hooks-usage)
    :parameters
    (list (make-slash-command-parameter
           :name "args"
           :type :string
           :required-p nil
           :greedy-p t
           :description "Optional subcommand and arguments."))
    :handler #'%hooks-handler
    :completer #'%hooks-arg-completer))
  (register-slash-command
   (make-slash-command
    :name "mcp-status"
    :description "Show MCP server negotiation state, capabilities, and discovered tool counts."
    :usage "/mcp-status"
    :handler #'%mcp-status-handler))
  (register-slash-command
   (make-slash-command
    :name "mcp-auth"
    :description "Inspect or update MCP per-server authorization decisions."
    :usage (%mcp-auth-usage)
    :parameters
    (list (make-slash-command-parameter
           :name "args"
           :type :string
           :required-p nil
           :greedy-p t
           :description "Optional action: list, set <server> <allow|deny|prompt>, clear <server|all>."))
    :handler #'%mcp-auth-handler
    :completer #'%mcp-auth-arg-completer))
  (register-slash-command
   (make-slash-command
    :name "sounds"
    :description "List sound themes, set the active theme, or preview a theme sound."
    :usage (%sounds-usage)
    :parameters
    (list (make-slash-command-parameter
           :name "args"
           :type :string
           :required-p nil
           :greedy-p t
           :description "Optional action: list, set <theme>, preview [category]."))
    :handler #'%sounds-handler
    :completer #'%sounds-arg-completer))
  (register-slash-command
   (make-slash-command
    :name "notifications"
    :description "Inspect dispatch backends, toggle them, and fire a dispatch test event."
    :usage (%notifications-usage)
    :parameters
    (list (make-slash-command-parameter
           :name "args"
           :type :string
           :required-p nil
           :greedy-p t
           :description "Optional action: list, enable <backend>, disable <backend>, test-fire [event-type]."))
    :handler #'%notifications-handler
    :completer #'%notifications-arg-completer))
  (register-slash-command
   (make-slash-command
    :name "speak"
    :description "Speak the latest assistant response and manage TTS auto-speak."
    :usage (%speak-usage)
    :parameters
    (list (make-slash-command-parameter
           :name "args"
           :type :string
           :required-p nil
           :greedy-p t
           :description "Optional action: last, on, off, status, stop, voice <name>."))
    :handler #'%speak-handler
    :completer #'%speak-arg-completer))
  (register-slash-command
   (make-slash-command
    :name "lint"
    :description "Re-expand deftool/defhook/defwidget forms and report compile-time warnings/errors."
    :usage "/lint [path ...]"
    :parameters
    (list (make-slash-command-parameter
           :name "paths"
           :type :string
           :required-p nil
           :greedy-p t
           :description "Optional files/directories to scan; defaults to amoebum/src and ptui/src/widgets."))
    :handler #'%lint-handler))
  ;; --- Phase 5 commands (I95-I101) ---
  (register-slash-command
   (make-slash-command
    :name "models"
    :description "List configured providers, their models, and health status."
    :usage "/models [provider-name]"
    :parameters
    (list (make-slash-command-parameter
           :name "provider"
           :type :string
           :required-p nil
           :description "Optional provider name to inspect."))
    :handler #'%models-handler))
  (register-slash-command
   (make-slash-command
    :name "providers"
    :description "Toggle provider health dashboard visibility."
    :usage "/providers [on|off]"
    :parameters
    (list (make-slash-command-parameter
           :name "action"
           :type :string
           :required-p nil
           :description "Optional on/off to explicitly set visibility."))
    :handler #'%providers-handler))
  (register-slash-command
   (make-slash-command
    :name "cost"
    :description "Estimate cost of the most recent chat interactions."
    :usage "/cost [count]"
    :parameters
    (list (make-slash-command-parameter
           :name "count"
           :type :integer
           :required-p nil
           :description "Number of recent interactions to estimate (default 5)."))
    :handler #'%cost-handler))
  (register-slash-command
   (make-slash-command
    :name "index"
    :description "Generate or refresh the codebase symbol index and repo map."
    :usage "/index [--refresh] [--lang LANG]"
    :parameters
    (list (make-slash-command-parameter
           :name "args"
           :type :string
           :required-p nil
           :greedy-p t
           :description "Optional flags: --refresh to force rebuild."))
    :handler #'%index-handler))
  (register-slash-command
   (make-slash-command
    :name "self-modify"
    :description "Propose, approve, and apply a runtime code modification."
    :usage "/self-modify <lisp-form>"
    :parameters
    (list (make-slash-command-parameter
           :name "form"
           :type :string
           :required-p t
           :greedy-p t
           :description "Lisp form to evaluate in sandboxed context."))
    :handler #'%self-modify-handler))
  (register-slash-command
   (make-slash-command
    :name "image"
    :description "Save or restore a Lisp image snapshot."
    :usage "/image [save [path]|restore [path]|list|rotate]"
    :parameters
    (list (make-slash-command-parameter
           :name "args"
           :type :string
           :required-p nil
           :greedy-p t
           :description "Action: save, restore, list, or rotate."))
    :handler #'%image-handler))
  (register-slash-command
   (make-slash-command
    :name "extensions-asdf"
    :description "Manage ASDF-based extensions: discover, load, unload."
    :usage "/extensions-asdf [list|load <system>|unload <system>|discover]"
    :parameters
    (list (make-slash-command-parameter
           :name "args"
           :type :string
           :required-p nil
           :greedy-p t
           :description "Subcommand and optional system name."))
    :handler #'%extensions-asdf-handler))
  (register-slash-command
   (make-slash-command
    :name "perf"
    :description "Backward-compatible alias for /profile commands."
    :usage "/perf [start|stop|report]"
    :parameters
    (list (make-slash-command-parameter
           :name "args"
           :type :string
           :required-p nil
           :greedy-p t
           :description "Subcommand for profiling."))
    :handler #'%perf-handler))
  (register-slash-command
   (make-slash-command
    :name "profile"
    :description "Control SBCL statistical profiling and show reports."
    :usage "/profile [start|stop|report]"
    :parameters
    (list (make-slash-command-parameter
           :name "args"
           :type :string
           :required-p nil
           :greedy-p t
           :description "Subcommand for profiling."))
    :handler #'%profile-handler))
  (register-slash-command
   (make-slash-command
    :name "voice"
    :description "Toggle Whisper voice input and language."
    :usage (%voice-usage)
    :parameters
    (list (make-slash-command-parameter
           :name "args"
           :type :string
           :required-p nil
           :greedy-p t
           :description "Optional action: on, off, toggle, status, language <code>."))
    :handler #'%voice-handler
    :completer #'%voice-arg-completer))
  (register-slash-command
   (make-slash-command
    :name "spawn"
    :description "Spawn a sw4rm sub-agent for a task."
    :usage "/spawn <task-description>"
    :parameters
    (list (make-slash-command-parameter
           :name "task"
           :type :string
           :required-p t
           :greedy-p t
           :description "Task description for the spawned agent."))
    :handler #'%spawn-handler))
  (register-slash-command
   (make-slash-command
    :name "approvals"
    :description "Inspect or set approval policy presets."
    :usage "/approvals [status|set <untrusted|on-failure|on-request|never>]"
    :parameters
    (list (make-slash-command-parameter
           :name "args"
           :type :string
           :required-p nil
           :greedy-p t
           :description "Optional action: status or set <policy>."))
    :handler #'%approvals-handler))
  (register-slash-command
   (make-slash-command
    :name "permissions"
    :description "Inspect permission cache metrics and explain decision traces."
    :usage "/permissions [stats|log [limit]|explain [decision-id|latest]]"
    :parameters
    (list (make-slash-command-parameter
           :name "args"
           :type :string
           :required-p nil
           :greedy-p t
           :description "Optional action: stats, log, or explain <decision-id>."))
    :handler #'%permissions-handler
    :completer #'%permissions-arg-completer))
  t)

(register-builtin-slash-commands)
