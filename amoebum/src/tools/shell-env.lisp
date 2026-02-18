(in-package :amoebum)

;;; ---------------------------------------------------------------------------
;;; Shell Environment Handling (I111)
;;;
;;; Manages working directory (cwd) and environment variables for shell tool
;;; commands.  Supports cwd/env inheritance from session state, explicit
;;; per-command overrides, and filtering of sensitive environment variables
;;; (API keys, tokens, secrets) from the subprocess environment.
;;; ---------------------------------------------------------------------------

;;; --- Sensitive variable patterns -------------------------------------------

(defparameter *shell-env-sensitive-patterns*
  '("API_KEY" "API_SECRET" "SECRET_KEY" "SECRET_TOKEN"
    "ACCESS_TOKEN" "AUTH_TOKEN" "PRIVATE_KEY"
    "PASSWORD" "PASSWD" "CREDENTIALS"
    "AWS_SECRET_ACCESS_KEY" "AWS_SESSION_TOKEN"
    "GITHUB_TOKEN" "GITLAB_TOKEN" "ANTHROPIC_API_KEY"
    "OPENAI_API_KEY" "HF_TOKEN" "HUGGING_FACE_HUB_TOKEN"
    "SLACK_TOKEN" "DISCORD_TOKEN" "TELEGRAM_TOKEN"
    "DATABASE_URL" "DATABASE_PASSWORD" "DB_PASSWORD"
    "STRIPE_SECRET" "TWILIO_AUTH_TOKEN"
    "NPM_TOKEN" "PYPI_TOKEN" "GEM_HOST_API_KEY"
    "DOCKER_PASSWORD" "REGISTRY_PASSWORD")
  "List of environment variable name substrings that indicate sensitive data.
Variables whose names contain any of these (case-insensitive) are filtered
from subprocess environments.")

;;; --- Shell environment struct ----------------------------------------------

(defstruct (shell-environment
            (:constructor make-shell-environment
                (&key (cwd nil)
                      (inherit-cwd-p t)
                      (env-overrides nil)
                      (inherit-env-p t)
                      (filter-sensitive-p t)
                      (sensitive-patterns nil)
                      (extra-path-dirs nil))))
  "Configuration struct for shell command environments.

Slots:
  CWD               - Explicit working directory (pathname or string or NIL).
  INHERIT-CWD-P     - When true, fall back to session cwd if CWD is NIL.
  ENV-OVERRIDES     - Alist of (\"VAR\" . \"VALUE\") to set/override in the
                      subprocess environment.  A NIL value removes the variable.
  INHERIT-ENV-P     - When true, subprocess inherits the current process env.
  FILTER-SENSITIVE-P - When true, sensitive variables are stripped from the
                       inherited environment.
  SENSITIVE-PATTERNS - Custom patterns to use instead of the default list.
                       NIL means use *shell-env-sensitive-patterns*.
  EXTRA-PATH-DIRS   - List of directory strings prepended to $PATH."
  (cwd nil)
  (inherit-cwd-p t)
  (env-overrides nil)
  (inherit-env-p t)
  (filter-sensitive-p t)
  (sensitive-patterns nil)
  (extra-path-dirs nil))

;;; --- Sensitive variable filtering ------------------------------------------

(defun %sensitive-var-p (var-name &optional patterns)
  "Return T if VAR-NAME (a string) matches any sensitive pattern."
  (let ((effective-patterns (or patterns *shell-env-sensitive-patterns*))
        (upper (string-upcase var-name)))
    (some (lambda (pattern)
            (search (string-upcase pattern) upper :test #'char=))
          effective-patterns)))

(defun %env-var-name (entry)
  "Extract the variable name from an \"NAME=VALUE\" environment entry string."
  (let ((pos (position #\= entry)))
    (if pos
        (subseq entry 0 pos)
        entry)))

(defun %env-var-value (entry)
  "Extract the value from an \"NAME=VALUE\" environment entry string."
  (let ((pos (position #\= entry)))
    (if pos
        (subseq entry (1+ pos))
        "")))

(defun filter-sensitive-env (env-alist &optional patterns)
  "Remove entries from ENV-ALIST whose keys match sensitive patterns.
ENV-ALIST is a list of (NAME . VALUE) cons cells."
  (remove-if (lambda (pair)
               (%sensitive-var-p (car pair) patterns))
             env-alist))

(defun %current-process-env-alist ()
  "Return the current process environment as an alist of (NAME . VALUE)."
  #+sbcl
  (mapcar (lambda (entry)
            (cons (%env-var-name entry) (%env-var-value entry)))
          (sb-ext:posix-environ))
  #-sbcl
  nil)

;;; --- CWD resolution --------------------------------------------------------

(defun resolve-shell-env-cwd (shell-env)
  "Resolve the effective working directory for SHELL-ENV.
Returns a directory pathname.  Signals an error if the directory does not exist."
  (let* ((explicit-cwd (shell-environment-cwd shell-env))
         (inherit-p (shell-environment-inherit-cwd-p shell-env))
         (raw-cwd (cond
                    (explicit-cwd explicit-cwd)
                    (inherit-p (%current-shell-directory))
                    (t *default-pathname-defaults*))))
    (%resolve-shell-directory raw-cwd)))

;;; --- Environment assembly --------------------------------------------------

(defun %prepend-path-dirs (env-alist extra-dirs)
  "Prepend EXTRA-DIRS to the PATH entry in ENV-ALIST, returning new alist."
  (if (null extra-dirs)
      env-alist
      (let* ((path-pair (assoc "PATH" env-alist :test #'string=))
             (old-path (if path-pair (cdr path-pair) ""))
             (extra-str (format nil "~{~A~^:~}" extra-dirs))
             (new-path (if (and old-path (> (length old-path) 0))
                           (format nil "~A:~A" extra-str old-path)
                           extra-str))
             (without-path (remove "PATH" env-alist :test #'string= :key #'car)))
        (cons (cons "PATH" new-path) without-path))))

(defun %apply-env-overrides (env-alist overrides)
  "Apply OVERRIDES to ENV-ALIST.  Each override is (NAME . VALUE).
A NIL value removes the variable."
  (let ((result (copy-list env-alist)))
    (dolist (override overrides)
      (let ((name (car override))
            (value (cdr override)))
        (setf result (remove name result :test #'string= :key #'car))
        (when value
          (push (cons name value) result))))
    result))

(defun assemble-shell-env (shell-env)
  "Build the full environment variable alist for a shell command.
Returns an alist of (NAME . VALUE) cons cells suitable for passing to
subprocess creation."
  (let* ((inherit-p (shell-environment-inherit-env-p shell-env))
         (filter-p (shell-environment-filter-sensitive-p shell-env))
         (patterns (shell-environment-sensitive-patterns shell-env))
         (overrides (shell-environment-env-overrides shell-env))
         (extra-path (shell-environment-extra-path-dirs shell-env))
         ;; Start with inherited or empty env
         (base-env (if inherit-p
                       (%current-process-env-alist)
                       nil))
         ;; Filter sensitive vars
         (filtered-env (if filter-p
                           (filter-sensitive-env base-env patterns)
                           base-env))
         ;; Apply overrides
         (overridden-env (%apply-env-overrides filtered-env overrides))
         ;; Prepend extra PATH dirs
         (final-env (%prepend-path-dirs overridden-env extra-path)))
    final-env))

(defun shell-env-to-string-list (env-alist)
  "Convert an environment alist to a list of \"NAME=VALUE\" strings."
  (mapcar (lambda (pair)
            (format nil "~A=~A" (car pair) (cdr pair)))
          env-alist))

;;; --- Integration with shell tool -------------------------------------------

(defun %default-shell-environment ()
  "Return a default shell-environment with standard settings."
  (make-shell-environment))

(defun merge-shell-environment (base &key cwd env-overrides
                                          inherit-cwd-p inherit-env-p
                                          filter-sensitive-p extra-path-dirs)
  "Create a new shell-environment by merging overrides onto BASE.
Only non-NIL keyword arguments replace the corresponding BASE slot."
  (make-shell-environment
   :cwd (or cwd (shell-environment-cwd base))
   :inherit-cwd-p (if (null inherit-cwd-p)
                       (shell-environment-inherit-cwd-p base)
                       inherit-cwd-p)
   :env-overrides (append (or env-overrides nil)
                          (shell-environment-env-overrides base))
   :inherit-env-p (if (null inherit-env-p)
                       (shell-environment-inherit-env-p base)
                       inherit-env-p)
   :filter-sensitive-p (if (null filter-sensitive-p)
                            (shell-environment-filter-sensitive-p base)
                            filter-sensitive-p)
   :sensitive-patterns (shell-environment-sensitive-patterns base)
   :extra-path-dirs (append (or extra-path-dirs nil)
                            (shell-environment-extra-path-dirs base))))

(defun describe-shell-environment (shell-env &optional (stream *standard-output*))
  "Print a human-readable summary of SHELL-ENV."
  (format stream "Shell Environment:~%")
  (format stream "  CWD:              ~A~%"
          (or (shell-environment-cwd shell-env) "(inherit from session)"))
  (format stream "  Inherit CWD:      ~A~%"
          (shell-environment-inherit-cwd-p shell-env))
  (format stream "  Inherit ENV:      ~A~%"
          (shell-environment-inherit-env-p shell-env))
  (format stream "  Filter sensitive:  ~A~%"
          (shell-environment-filter-sensitive-p shell-env))
  (format stream "  ENV overrides:    ~D~%"
          (length (shell-environment-env-overrides shell-env)))
  (format stream "  Extra PATH dirs:  ~D~%"
          (length (shell-environment-extra-path-dirs shell-env)))
  shell-env)
