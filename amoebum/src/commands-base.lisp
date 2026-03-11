(in-package :amoebum)

;;; Base structs, registry, and foundational functions for the slash-command system.
;;; Split from commands.lisp to break a circular load-order dependency:
;;;   - defskill.lisp defines the DEFSKILL macro (and built-in skills at EOF)
;;;   - defskill macro expansion generates MAKE-SLASH-COMMAND-PARAMETER,
;;;     REGISTER-SLASH-COMMAND, %NORMALIZE-COMMAND-NAME calls
;;;   - commands.lisp (loaded much later) uses *notification-dispatcher*,
;;;     *skill-registry*, *mcp-tool-*-registry*, +default-repo-map-token-target+
;;;
;;; Load order: commands-base → ... → defskill → ... → commands

;; --- String utilities (eval-when for defskill macro-expansion time) ---

(eval-when (:compile-toplevel :load-toplevel :execute)
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
    (intern (string-upcase (%normalize-command-name name)) :keyword)))

;; --- Struct definitions ---

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

;; --- Registry and registration ---

(defparameter *slash-command-registry* (make-hash-table :test #'equal))

;; Defined in src/macros/deftool.lisp; declared here for compile/load order.
(defvar *tool-metadata*)
(defvar *tool-history*)

(defparameter *memory-command-subcommands*
  '("show" "edit" "clear" "remember" "forget" "import" "export"))

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

;; --- Input parsing ---

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
