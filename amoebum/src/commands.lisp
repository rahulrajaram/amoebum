(in-package :amoebum)

;;; Struct definitions, registry, string utilities, and registration functions
;;; are in src/commands-base.lisp and src/commands-registry.lisp.
;;; This file now contains the command parser, completer, and dispatcher core.

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

(defun %slash-choice-label (choice)
  (if (symbolp choice)
      (string-downcase (symbol-name choice))
      (princ-to-string choice)))

(defun %consume-parameter-token (parameter tokens)
  (if (slash-command-parameter-greedy-p parameter)
      (values
       (and tokens
            (with-output-to-string (out)
              (loop for token in tokens
                    for index from 0 do
                      (when (> index 0)
                        (write-char #\Space out))
                      (write-string token out))))
       '())
      (values (first tokens)
              (if tokens
                  (rest tokens)
                  '()))))

(defun %slash-argument-missing-p (raw-token)
  (or (null raw-token)
      (zerop (length (%slash-trim raw-token)))))

(defun %record-missing-parameter (parameter arguments errors)
  (let ((name (slash-command-parameter-name parameter))
        (key (%command-name-keyword (slash-command-parameter-name parameter)))
        (default (slash-command-parameter-default parameter)))
    (cond
      ((slash-command-parameter-required-p parameter)
       (push (format nil "Missing required argument ~A." name) errors))
      ((not (null default))
       (setf (gethash key arguments) default)))
    errors))

(defun %resolve-parameter-choice (parameter coerced)
  (let ((choices (slash-command-parameter-choices parameter)))
    (if choices
        (let ((matched (%match-choice coerced choices)))
          (unless matched
            (error "Argument ~A must be one of ~{~A~^, ~}."
                   (slash-command-parameter-name parameter)
                   (mapcar #'%slash-choice-label choices)))
          matched)
        coerced)))

(defun %store-parameter-token (parameter raw-token arguments errors)
  (handler-case
      (let* ((coerced (%coerce-argument-token raw-token parameter))
             (value (%resolve-parameter-choice parameter coerced)))
        (setf (gethash (%command-name-keyword (slash-command-parameter-name parameter))
                       arguments)
              value)
        (values errors t))
    (error (condition)
      (values (push (princ-to-string condition) errors) nil))))

(defun %parse-command-parameter (parameter tokens arguments errors)
  (multiple-value-bind (raw-token remaining-tokens)
      (%consume-parameter-token parameter tokens)
    (cond
      ((%slash-argument-missing-p raw-token)
       (values remaining-tokens
               (%record-missing-parameter parameter arguments errors)))
      (t
       (multiple-value-bind (updated-errors stored-p)
           (%store-parameter-token parameter raw-token arguments errors)
         (if (or stored-p
                 (slash-command-parameter-required-p parameter)
                 (null (slash-command-parameter-default parameter))
                 (slash-command-parameter-greedy-p parameter))
             (values remaining-tokens updated-errors)
             (progn
               (remhash (%command-name-keyword
                         (slash-command-parameter-name parameter))
                        arguments)
               (values tokens
                       (%record-missing-parameter parameter arguments errors)))))))))

(defun parse-slash-command-arguments (command invocation)
  (check-type command slash-command)
  (check-type invocation slash-command-invocation)
  (let ((arguments (make-hash-table :test #'equal))
        (tokens (copy-list (slash-command-invocation-argument-tokens invocation)))
        (errors '()))
    (dolist (parameter (slash-command-parameters command))
      (multiple-value-setq (tokens errors)
        (%parse-command-parameter parameter tokens arguments errors)))
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

(defun %command-name-completions (fragment)
  (let ((prefix (%normalize-command-name fragment)))
    (mapcar (lambda (command)
              (format nil "/~A" (%normalize-command-name (slash-command-name command))))
            (remove-if-not
             (lambda (command)
               (%starts-with-ci-p prefix
                                  (%normalize-command-name (slash-command-name command))))
             (list-slash-commands)))))

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

(defun %unknown-command-result (name)
  (make-slash-command-result
   :output (format nil "Unknown command /~A. Use /help." name)
   :echo-input-p t))

(defun %slash-argument-error-result (command errors)
  (make-slash-command-result
   :output (format nil "~{~A~%~}Usage: ~A"
                   errors
                   (%command-usage command))
   :echo-input-p t))

(defun %invoke-command-handler (command invocation arguments config memory-backend chat-state)
  (let* ((handler (slash-command-handler command))
         (context (make-slash-command-context
                   :config config
                   :memory-backend memory-backend
                   :chat-state chat-state)))
    (if (functionp handler)
        (funcall handler invocation arguments context)
        (make-slash-command-result
         :output (format nil "Command /~A has no handler."
                         (slash-command-invocation-name invocation))))))

(defun %coerce-command-result (result)
  (cond
    ((typep result 'slash-command-result) result)
    ((stringp result)
     (make-slash-command-result :output result))
    (t
     (make-slash-command-result
      :output (if result
                  (princ-to-string result)
                  nil)))))

(defun %slash-command-failure-result (invocation condition)
  (make-slash-command-result
   :output (format nil "Command /~A failed: ~A"
                   (slash-command-invocation-name invocation)
                   condition)
   :echo-input-p t))

(defun dispatch-slash-command (input &key config memory-backend chat-state)
  (let ((invocation (parse-slash-command input)))
    (unless invocation
      (return-from dispatch-slash-command (values nil nil)))
    (let* ((command (find-slash-command (slash-command-invocation-name invocation))))
      (unless command
        (return-from dispatch-slash-command
          (values t (%unknown-command-result
                     (slash-command-invocation-name invocation)))))
      (multiple-value-bind (arguments errors)
          (parse-slash-command-arguments command invocation)
        (if errors
            (values t (%slash-argument-error-result command errors))
            (handler-case
                (values t
                        (%coerce-command-result
                         (%invoke-command-handler command invocation arguments
                                                  config memory-backend chat-state)))
              (error (condition)
                (values t (%slash-command-failure-result invocation condition)))))))))
