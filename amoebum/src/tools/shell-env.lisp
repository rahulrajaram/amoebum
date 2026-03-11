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

(defparameter *shell-profile-candidate-files*
  '((:bash . (".bash_profile" ".bash_login" ".profile" ".bashrc"))
    (:zsh . (".zshenv" ".zprofile" ".zshrc" ".zlogin"))
    (:sh . (".profile")))
  "Per-shell-family profile files sourced for shell initialization.")

(defparameter *shell-project-env-relative-path* ".amoebum/env"
  "Default project-relative environment overlay file path.")

(defparameter *shell-project-path-augmentation-relative-dirs*
  '("node_modules/.bin" ".venv/bin")
  "Project-relative PATH entries prepended when present on disk.")

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

(defun %trim-shell-env-text (value)
  (if (stringp value)
      (string-trim '(#\Space #\Tab #\Newline #\Return) value)
      ""))

(defun %unquote-shell-env-value (value)
  (let* ((trimmed (%trim-shell-env-text value))
         (length* (length trimmed)))
    (if (and (>= length* 2)
             (or (and (char= (char trimmed 0) #\")
                      (char= (char trimmed (1- length*)) #\"))
                 (and (char= (char trimmed 0) #\')
                      (char= (char trimmed (1- length*)) #\'))))
        (subseq trimmed 1 (1- length*))
        trimmed)))

(defun %shell-env-assignment-valid-name-p (name)
  (and (stringp name)
       (> (length name) 0)
       (let ((first (char name 0)))
         (and (or (alpha-char-p first) (char= first #\_))
              (loop for char across name
                    always (or (alpha-char-p char)
                               (digit-char-p char)
                               (char= char #\_)))))))

(defun %parse-project-env-assignment (line)
  (let* ((trimmed (%trim-shell-env-text line)))
    (cond
      ((or (zerop (length trimmed))
           (char= (char trimmed 0) #\#))
       nil)
      (t
       (let* ((without-export
                (if (and (>= (length trimmed) 7)
                         (string-equal "export " trimmed :end2 7))
                    (%trim-shell-env-text (subseq trimmed 7))
                    trimmed))
              (eq-pos (position #\= without-export)))
         (when (and eq-pos (> eq-pos 0))
           (let* ((name (%trim-shell-env-text (subseq without-export 0 eq-pos)))
                  (value (%unquote-shell-env-value
                          (subseq without-export (1+ eq-pos)))))
             (when (%shell-env-assignment-valid-name-p name)
               (cons name value)))))))))

(defun %shell-family-from-executable (shell-executable)
  (let ((lower (string-downcase (or shell-executable ""))))
    (cond
      ((search "zsh" lower :test #'char=) :zsh)
      ((search "bash" lower :test #'char=) :bash)
      (t :sh))))

(defun %resolve-shell-executable (shell-executable)
  (let ((candidate (%trim-shell-env-text shell-executable)))
    (cond
      ((> (length candidate) 0) candidate)
      ((uiop:getenv "SHELL")
       (%trim-shell-env-text (uiop:getenv "SHELL")))
      (t "/bin/bash"))))

(defun %resolve-home-file (relative-path)
  (merge-pathnames relative-path (user-homedir-pathname)))

(defun %shell-env-single-quote (text)
  (with-output-to-string (stream)
    (write-char #\' stream)
    (loop for char across (or text "") do
          (if (char= char #\')
              (write-string "'\"'\"'" stream)
              (write-char char stream)))
    (write-char #\' stream)))

(defun filter-sensitive-env (env-alist &optional patterns)
  "Remove entries from ENV-ALIST whose keys match sensitive patterns.
ENV-ALIST is a list of (NAME . VALUE) cons cells."
  (remove-if (lambda (pair)
               (%sensitive-var-p (car pair) patterns))
             env-alist))

(defun resolve-shell-runtime-executable (&optional shell-executable)
  "Return the shell executable used for command execution."
  (%resolve-shell-executable shell-executable))

(defun resolve-shell-profile-files (&optional shell-executable)
  "Return existing profile files for SHELL-EXECUTABLE's shell family."
  (let* ((resolved-shell (%resolve-shell-executable shell-executable))
         (family (%shell-family-from-executable resolved-shell))
         (candidates (cdr (assoc family *shell-profile-candidate-files*))))
    (loop for relative in candidates
          for path = (%resolve-home-file relative)
          when (probe-file path)
            collect (coerce-path-string path))))

(defun wrap-command-with-shell-profile-init (command profile-files)
  "Prefix COMMAND with silent profile initialization from PROFILE-FILES."
  (if (null profile-files)
      command
      (with-output-to-string (stream)
        (dolist (profile profile-files)
          (let ((quoted (%shell-env-single-quote profile)))
            (format stream "if [ -f ~A ]; then . ~A >/dev/null 2>&1 || true; fi; "
                    quoted quoted)))
        (write-string command stream))))

(defun %resolve-project-env-path (cwd env-path)
  (let* ((directory (%resolve-shell-directory cwd))
         (pathname*
           (cond
             ((pathnamep env-path) env-path)
             ((stringp env-path) (pathname env-path))
             (t (pathname *shell-project-env-relative-path*)))))
    (if (uiop:absolute-pathname-p pathname*)
        pathname*
        (merge-pathnames pathname* directory))))

(defun load-project-env-overrides (&key cwd (env-path *shell-project-env-relative-path*))
  "Load NAME=VALUE assignments from project env file.
Returns an alist suitable for `shell-environment-env-overrides`."
  (let ((path (%resolve-project-env-path cwd env-path)))
    (if (not (probe-file path))
        nil
        (let ((overrides '()))
          (with-open-file (stream path
                                  :direction :input
                                  :if-does-not-exist nil)
            (loop for line = (read-line stream nil nil)
                  while line do
                    (let ((assignment (%parse-project-env-assignment line)))
                      (when assignment
                        (push assignment overrides)))))
          (nreverse overrides)))))

(defun default-project-path-augmentation-dirs (&key cwd)
  "Return existing project-local PATH augmentation directories."
  (let ((directory (%resolve-shell-directory cwd)))
    (loop for relative in *shell-project-path-augmentation-relative-dirs*
          for path = (merge-pathnames relative directory)
          for normalized = (uiop:ensure-directory-pathname path)
          when (probe-file normalized)
            collect (coerce-path-string normalized))))

(defun %prepare-shell-runtime (cwd init-shell-profile-p init-project-env-p
                               prepend-project-path-p
                               &optional shell-executable)
  "Resolve shell runtime settings for command execution.

Returns four values:
  1. Effective working directory pathname
  2. Shell executable string
  3. Existing shell profile files to source (or NIL)
  4. Environment entries as a list of \"NAME=VALUE\" strings"
  (let* ((directory (%resolve-shell-directory cwd))
         (resolved-shell (resolve-shell-runtime-executable shell-executable))
         (profiles (when init-shell-profile-p
                     (resolve-shell-profile-files resolved-shell)))
         (project-overrides (when init-project-env-p
                              (load-project-env-overrides :cwd directory)))
         (extra-path-dirs (when prepend-project-path-p
                            (default-project-path-augmentation-dirs
                             :cwd directory)))
         (shell-env (make-shell-environment
                     :cwd directory
                     :inherit-cwd-p nil
                     :env-overrides project-overrides
                     :inherit-env-p t
                     :filter-sensitive-p t
                     :extra-path-dirs extra-path-dirs))
         (env-vars (shell-env-to-string-list (assemble-shell-env shell-env))))
    (values directory resolved-shell profiles env-vars)))

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
