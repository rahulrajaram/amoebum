(in-package :amoebum)

;;; ---------------------------------------------------------------------------
;;; Write Safety Checks (I107)
;;;
;;; Enforces amoebum-side safety checks before whole-file writes:
;;;   1. Forbidden system directory paths are blocked.
;;;   2. Deny-pattern file names (e.g. .env, credentials) are blocked.
;;;   3. Permission policy rules are consulted via check-permission.
;;;   4. Clear error messages describe why a write was denied.
;;; ---------------------------------------------------------------------------

;;; --- Sensitive path prefixes ------------------------------------------------

(defparameter *write-forbidden-path-prefixes*
  '("/etc/"
    "/usr/"
    "/bin/"
    "/sbin/"
    "/boot/"
    "/dev/"
    "/proc/"
    "/sys/"
    "/lib/"
    "/lib64/"
    "/var/run/"
    "/var/lock/"
    "/run/")
  "Absolute directory prefixes that are unconditionally forbidden for writes.")

;;; --- Deny patterns for sensitive file names ---------------------------------

(defparameter *write-deny-filename-patterns*
  '("(?i)^\\.(env|env\\.local|env\\.production|env\\.staging)$"
    "(?i)^credentials\\.json$"
    "(?i)^secrets?\\.ya?ml$"
    "(?i)^\\.npmrc$"
    "(?i)^\\.pypirc$"
    "(?i)^id_rsa$"
    "(?i)^id_ed25519$"
    "(?i)^\\.ssh/.*"
    "(?i)^(token|auth)\\.json$"
    "(?i)^\\.?aws/credentials$"
    "(?i)^\\.netrc$"
    "(?i)^\\.pgpass$")
  "Regex patterns matching sensitive file basenames that must not be written.")

;;; --- Condition for write denial ---------------------------------------------

(define-condition write-safety-denied (tool-permission-denied)
  ((path :initarg :path
         :initform nil
         :reader write-safety-denied-path)
   (denial-reason :initarg :denial-reason
                  :initform nil
                  :reader write-safety-denied-denial-reason))
  (:report (lambda (condition stream)
             (let ((path (write-safety-denied-path condition))
                   (denial (write-safety-denied-denial-reason condition)))
               (format stream "Write denied~@[ for ~A~]: ~A"
                       path
                       (or denial "blocked by write safety policy"))))))

;;; --- Internal helpers -------------------------------------------------------

(defun %write-safety-path-text (path)
  "Coerce PATH to a string for matching.  Returns NIL for nil input."
  (typecase path
    (null nil)
    (pathname (namestring path))
    (string path)
    (t (prin1-to-string path))))

(defun %write-safety-normalize-path (path)
  "Normalize PATH: resolve, lowercase-for-matching, ensure slashes."
  (let ((text (%write-safety-path-text path)))
    (when (and text (plusp (length text)))
      (%normalize-slashes text))))

(defun %write-path-under-forbidden-prefix-p (normalized-path)
  "Return the forbidden prefix if NORMALIZED-PATH starts with one, else NIL."
  (dolist (prefix *write-forbidden-path-prefixes*)
    (when (uiop:string-prefix-p prefix normalized-path)
      (return prefix))))

(defun %write-filename-matches-deny-pattern-p (path)
  "Return matching pattern if the basename of PATH matches a deny pattern."
  (let* ((text (%write-safety-path-text path))
         (basename (file-namestring (pathname text))))
    (when (and basename (plusp (length basename)))
      (dolist (pattern *write-deny-filename-patterns*)
        (when (cl-ppcre:scan pattern basename)
          (return pattern))))))

;;; --- Public API -------------------------------------------------------------

(defun check-write-safety (path &key (tool "write-file")
                                     (permission-mode nil)
                                     (rules *permission-rules*))
  "Validate that a write to PATH is allowed by safety policy.
Returns T if the write is allowed.  Signals WRITE-SAFETY-DENIED if blocked.

Checks applied in order:
  1. Path must not be nil or empty.
  2. Path must not be under a forbidden system directory prefix.
  3. Filename must not match sensitive file deny patterns.
  4. Path must be allowed by the permission rule set (check-permission)."
  (let* ((text (%write-safety-path-text path))
         (normalized (%write-safety-normalize-path path)))
    ;; 1. Nil/empty path
    (unless (and normalized (plusp (length normalized)))
      (error 'write-safety-denied
             :tool-name (%tool-name tool)
             :path text
             :denial-reason "path is nil or empty"
             :reason "path is nil or empty"))
    ;; 2. Forbidden system directory
    (let ((forbidden-prefix (%write-path-under-forbidden-prefix-p normalized)))
      (when forbidden-prefix
        (error 'write-safety-denied
               :tool-name (%tool-name tool)
               :path text
               :denial-reason (format nil "path under forbidden system directory ~A"
                                      forbidden-prefix)
               :reason (format nil "path under forbidden system directory ~A"
                               forbidden-prefix))))
    ;; 3. Deny-pattern filename match
    (let ((deny-pattern (%write-filename-matches-deny-pattern-p path)))
      (when deny-pattern
        (error 'write-safety-denied
               :tool-name (%tool-name tool)
               :path text
               :denial-reason (format nil "filename matches sensitive file pattern ~A"
                                      deny-pattern)
               :reason (format nil "filename matches sensitive file pattern ~A"
                               deny-pattern))))
    ;; 4. Permission rules
    (let ((decision (check-permission :tool tool
                                       :path text
                                       :permission-mode permission-mode
                                       :rules rules)))
      (when (eq decision :deny)
        (error 'write-safety-denied
               :tool-name (%tool-name tool)
               :path text
               :denial-reason "blocked by permission rule"
               :reason "blocked by permission rule")))
    t))

(defun write-safety-check-p (path &key (tool "write-file")
                                       (permission-mode nil)
                                       (rules *permission-rules*))
  "Non-signaling variant: return T if write is allowed, NIL plus reason otherwise."
  (handler-case
      (progn
        (check-write-safety path :tool tool
                                  :permission-mode permission-mode
                                  :rules rules)
        (values t nil))
    (write-safety-denied (c)
      (values nil (write-safety-denied-denial-reason c)))))
