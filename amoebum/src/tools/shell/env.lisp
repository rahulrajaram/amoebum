(in-package :amoebum)

;;;; ---------------------------------------------------------------------------
;;;; Shell tool: environment preparation, normalization, and working-directory
;;;; resolution helpers.
;;;;
;;;; This module owns the pure transformations performed before a shell command
;;;; reaches the runtime layer:
;;;;   * default tunables (timeouts, output budgets, polling interval)
;;;;   * normalization of caller-supplied integers (timeout/byte/line caps)
;;;;   * coercion / persistence of the working directory
;;;;   * environment alist coercion, deduplication, and override merging
;;;;   * UTF-8 byte sizing and diagnostic-line append helpers used by the
;;;;     runtime monitor
;;;;
;;;; Behavior is preserved verbatim from the original
;;;; `amoebum/src/tools/shell.lisp`; only file boundaries change.
;;;; ---------------------------------------------------------------------------

(defparameter *shell-default-timeout-seconds* 120)
(defparameter *shell-max-timeout-seconds* 600)
(defparameter *shell-default-max-output-chars* 8192)
(defparameter *shell-default-max-output-bytes* (* 256 1024))
(defparameter *shell-default-max-output-lines* 4096)
(defparameter *shell-process-poll-interval-seconds* 0.02)
(defparameter *shell-working-directory* nil)

(defun %trim-whitespace (text)
  (string-trim '(#\Space #\Tab #\Newline #\Return) text))

(defun %normalize-timeout-seconds (timeout-seconds)
  (let ((value (or timeout-seconds *shell-default-timeout-seconds*)))
    (unless (integerp value)
      (error "TIMEOUT-SECONDS must be an integer, got ~S." timeout-seconds))
    (when (or (< value 1) (> value *shell-max-timeout-seconds*))
      (error "TIMEOUT-SECONDS must be between 1 and ~D, got ~S."
             *shell-max-timeout-seconds*
             value))
    value))

(defun %normalize-max-output-chars (max-output-chars)
  (let ((value (or max-output-chars *shell-default-max-output-chars*)))
    (unless (integerp value)
      (error "MAX-OUTPUT-CHARS must be an integer, got ~S." max-output-chars))
    (when (< value 1)
      (error "MAX-OUTPUT-CHARS must be positive, got ~S." value))
    value))

(defun %normalize-max-output-bytes (max-output-bytes)
  (let ((value (or max-output-bytes *shell-default-max-output-bytes*)))
    (unless (integerp value)
      (error "MAX-OUTPUT-BYTES must be an integer, got ~S." max-output-bytes))
    (when (< value 1)
      (error "MAX-OUTPUT-BYTES must be positive, got ~S." value))
    value))

(defun %normalize-max-output-lines (max-output-lines)
  (let ((value (or max-output-lines *shell-default-max-output-lines*)))
    (unless (integerp value)
      (error "MAX-OUTPUT-LINES must be an integer, got ~S." max-output-lines))
    (when (< value 1)
      (error "MAX-OUTPUT-LINES must be positive, got ~S." value))
    value))

(defun %coerce-directory-input (cwd)
  (cond
    ((pathnamep cwd) cwd)
    ((stringp cwd) (pathname cwd))
    (t (error "CWD must be a pathname, string, or NIL. Got ~S." cwd))))

(defun %current-shell-directory ()
  (or *shell-working-directory*
      (setf *shell-working-directory*
            (config-project-root (current-config)))))

(defun %resolve-shell-directory (cwd)
  (let* ((base (%current-shell-directory))
         (candidate
           (if cwd
               (let ((provided (%coerce-directory-input cwd)))
                 (if (uiop:absolute-pathname-p provided)
                     provided
                     (merge-pathnames provided base)))
               base))
         (resolved (or (ignore-errors (truename candidate)) candidate))
         (directory (uiop:ensure-directory-pathname resolved)))
    (unless (probe-file directory)
      (error "Shell working directory does not exist: ~A" (coerce-path-string directory)))
    directory))

(defun %persist-shell-directory (directory)
  (setf *shell-working-directory* (uiop:ensure-directory-pathname directory)))

(defun %normalize-command (command)
  (unless (stringp command)
    (error "COMMAND must be a string, got ~S." command))
  (let ((trimmed (%trim-whitespace command)))
    (when (zerop (length trimmed))
      (error "COMMAND must not be empty."))
    trimmed))

(defun %truncate-output (text max-output-chars)
  (let* ((value (or text ""))
         (length* (length value)))
    (if (> length* max-output-chars)
        (values (subseq value 0 max-output-chars)
                t
                (- length* max-output-chars))
        (values value nil 0))))

(defun %split-env-assignment (entry)
  (let ((pos (position #\= entry)))
    (if pos
        (values (subseq entry 0 pos)
                (subseq entry (1+ pos)))
        (values entry ""))))

(defun %coerce-process-env-pair (name value)
  (cons (intern (string-upcase name) :keyword)
        (or value "")))

(defun %coerce-process-env-entry (entry)
  (cond
    ((and (consp entry) (keywordp (car entry)))
     entry)
    ((and (consp entry) (stringp (car entry)))
     (%coerce-process-env-pair (car entry) (cdr entry)))
    ((stringp entry)
     (multiple-value-bind (name value)
         (%split-env-assignment entry)
       (%coerce-process-env-pair name value)))
    (t
     (error "Unsupported environment entry ~S." entry))))

(defun %coerce-process-env (env-vars)
  "Normalize ENV-VARS into the alist shape expected by RUN-PROGRAM :ENV."
  (when env-vars
    (mapcar #'%coerce-process-env-entry env-vars)))

(defun %process-env-entry-keyword (entry)
  (car entry))

(defun %process-env-override-pair (entry)
  (cond
    ((and (consp entry) (keywordp (car entry)))
     (cons (string-upcase (symbol-name (car entry)))
           (or (cdr entry) "")))
    ((and (consp entry) (stringp (car entry)))
     (cons (car entry) (or (cdr entry) "")))
    ((stringp entry)
     (multiple-value-bind (name value)
         (%split-env-assignment entry)
       (cons name value)))
    (t
     (error "Unsupported environment entry ~S." entry))))

(defun %dedupe-process-env (entries)
  (let ((seen (make-hash-table :test #'eq))
        (result '()))
    (dolist (entry (reverse entries) (nreverse result))
      (let ((key (%process-env-entry-keyword entry)))
        (unless (gethash key seen)
          (setf (gethash key seen) t)
          (push entry result))))))

(defun %effective-process-env (env-vars)
  (if (current-delegated-agent-id)
      (let* ((scoped-overrides (current-delegated-agent-secret-env-overrides))
             (shell-env (merge-shell-environment
                         (%default-shell-environment)
                         :env-overrides (append (mapcar #'%process-env-override-pair
                                                        (or env-vars '()))
                                                scoped-overrides)
                         :inherit-env-p t
                         :filter-sensitive-p t))
             (scoped-env
               (%coerce-process-env
                (shell-env-to-string-list (assemble-shell-env shell-env)))))
        (%dedupe-process-env scoped-env))
      (%coerce-process-env env-vars)))

(defun %utf8-char-size (char)
  (let ((code (char-code char)))
    (cond
      ((<= code #x7F) 1)
      ((<= code #x7FF) 2)
      ((<= code #xFFFF) 3)
      (t 4))))

(defun %append-diagnostic-line (text diagnostic)
  (let ((base (or text "")))
    (if (zerop (length base))
        diagnostic
        (format nil "~A~%~A" base diagnostic))))
