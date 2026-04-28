(in-package :amoebum)

;;; Residual permissions facade. Allow/deny outcomes and decision-trace
;;; fields are byte-stable: the cache, history, plan-mode (with mode
;;; default-decision dispatch and MCP permission lookups), and
;;; session-memory clusters live in src/permissions/*.lisp; this file
;;; owns the rule struct, dangerous-command catalogue, generic
;;; tool/path/command helpers, and the argument-pattern matching
;;; utilities. See NXT-440 for the split rationale.

(defparameter *permission-path-case-sensitive-p*
  (not (or (member :windows *features*)
           (member :win32 *features*)
           (member :mswindows *features*))))
(defparameter *permission-path-unicode-normalization-form* :nfc)

(defparameter *dangerous-command-patterns*
  '("(?i)\\brm\\s+[^\\n]*-rf\\b"
    "(?i)\\brm\\s+[^\\n]*-fr\\b"
    "(?i)\\bchmod\\s+-R\\s+777\\b"
    "(?i)\\bdd\\s+if="
    "(?i)\\bgit\\s+checkout\\s+(?:--\\s+)?\\.(?:\\s|$)"
    "(?i)\\bgit\\s+clean\\s+[^\\n]*-[^\\s\\n]*f[^\\s\\n]*\\b"
    "(?i)\\bgit\\s+push\\s+[^\\n]*--force(?:-with-lease)?[^\\n]*(?:\\s|/)(?:main|master)(?:\\s|$)"
    "(?i)\\bmkfs(?:\\.[A-Za-z0-9_+-]+)?\\b"
    "(?i)\\bgit\\s+push\\s+[^\\n]*--force(?:-with-lease)?\\b"
    "(?i)\\bgit\\s+reset\\s+--hard\\b"
    "(?i)\\bdocker\\s+system\\s+prune\\s+-af\\b"
    "(?i)\\bnpm\\s+publish\\b"
    "(?i)\\bcargo\\s+publish\\b"
    "(?i)^\\s*sudo\\b"
    "(?i)>\\s*/dev/sd[a-z]\\b"))

(defparameter *shell-tool-names*
  '("bash" "bash-exec" "shell" "sh"))

(defparameter *auto-edit-tool-names*
  '("read-file" "write-file" "edit-file" "glob-files" "grep-content"))

(defstruct (permission-rule
            (:constructor make-permission-rule
                (&key id effect path command tool arguments (source :project))))
  id
  effect
  path
  command
  tool
  arguments
  source)

(defun %tool-name (tool)
  (cond
    ((null tool) nil)
    ((stringp tool) (string-downcase tool))
    ((symbolp tool) (string-downcase (symbol-name tool)))
    (t (string-downcase (prin1-to-string tool)))))

(defun %path-string (path)
  (typecase path
    (null nil)
    (pathname (namestring path))
    (string path)
    (t (prin1-to-string path))))

(defun %policy-command-text (canonical fallback-command)
  (let* ((policy-key (and canonical
                          (command-canonical-form-policy-key canonical)))
         (normalized (and canonical
                          (command-canonical-form-normalized canonical)))
         (separator-p (and normalized
                           (or (search "|" normalized)
                               (search ";" normalized)
                               (search "&&" normalized)))))
    (cond
      (separator-p normalized)
      (policy-key policy-key)
      (normalized normalized)
      (t (%command-string fallback-command)))))

(defun %normalize-slashes (string)
  (cl-ppcre:regex-replace-all "/+" (substitute #\/ #\\ string) "/"))

(defun %path-has-trailing-separator-p (string)
  (and (> (length string) 0)
       (let ((last (char string (1- (length string)))))
         (or (char= last #\/)
             (char= last #\\)))))

(defun %trim-trailing-slash (string)
  (if (and (> (length string) 1)
           (char= (char string (1- (length string))) #\/))
      (subseq string 0 (1- (length string)))
      string))

(defun %trim-path-whitespace (string)
  (string-trim '(#\Space #\Tab #\Newline #\Return) string))

(defun %windows-drive-path-p (path)
  (and (stringp path)
       (>= (length path) 3)
       (alpha-char-p (char path 0))
       (char= (char path 1) #\:)
       (char= (char path 2) #\/)))

(defun %absolute-path-p (path)
  (or (uiop:string-prefix-p "/" path)
      (%windows-drive-path-p path)))

(defun %cwd-path ()
  (%trim-trailing-slash (%normalize-slashes (namestring (uiop:getcwd)))))

(defun %ensure-absolute-path (path)
  (if (%absolute-path-p path)
      path
      (let ((cwd (%cwd-path)))
        (if (string= cwd "/")
            (concatenate 'string "/" path)
            (concatenate 'string cwd "/" path)))))

(defun %split-path-prefix (path)
  (cond
    ((uiop:string-prefix-p "/" path)
     (values "/" (subseq path 1)))
    ((%windows-drive-path-p path)
     (values (subseq path 0 2) (subseq path 3)))
    (t
     (values "" path))))

(defun %collapse-dot-segments (path)
  (multiple-value-bind (prefix remainder)
      (%split-path-prefix path)
    (let ((segments nil))
      (dolist (segment (uiop:split-string remainder :separator "/"))
        (cond
          ((or (string= segment "")
               (string= segment "."))
           nil)
          ((string= segment "..")
           (when segments
             (pop segments)))
          (t
           (push segment segments))))
      (let ((ordered (nreverse segments)))
        (cond
          ((string= prefix "/")
           (if ordered
               (concatenate 'string "/" (format nil "~{~A~^/~}" ordered))
               "/"))
          ((%windows-drive-path-p (concatenate 'string prefix "/"))
           (if ordered
               (format nil "~A/~{~A~^/~}" prefix ordered)
               (format nil "~A/" prefix)))
          (ordered
           (format nil "~{~A~^/~}" ordered))
          (t
           prefix))))))

(defun %normalize-drive-letter (path-text)
  (if (and (stringp path-text)
           (>= (length path-text) 2)
           (alpha-char-p (char path-text 0))
           (char= (char path-text 1) #\:))
      (concatenate 'string (string (char-downcase (char path-text 0)))
                   (subseq path-text 1))
      path-text))

(defun %normalize-request-path (path)
  (%normalize-path path :resolve-symlinks-p nil))

(defun %project-root-path ()
  (let ((root (%path-approval-project-root)))
    (and root
         (%trim-trailing-slash
          (%normalize-slashes (namestring root))))))

(defun %relative-path-text-p (path)
  (let* ((raw (%path-string path))
         (trimmed (and raw (%trim-path-whitespace raw))))
    (and trimmed
         (> (length trimmed) 0)
         (not (%absolute-path-p (%normalize-slashes (%normalize-drive-letter trimmed)))))))

(defun %resolve-path-against-project-root (path &key (resolve-symlinks-p t))
  (let* ((raw (%path-string path))
         (trimmed (and raw (%trim-path-whitespace raw))))
    (when (and trimmed (> (length trimmed) 0))
      (let* ((slash-normalized (%normalize-slashes (%normalize-drive-letter trimmed)))
             (candidate
               (if (%relative-path-text-p slash-normalized)
                   (let ((root (%project-root-path)))
                     (if (and root (> (length root) 0))
                         (if (string= root "/")
                             (concatenate 'string "/" slash-normalized)
                             (concatenate 'string root "/" slash-normalized))
                         slash-normalized))
                   slash-normalized)))
        (%normalize-path candidate :resolve-symlinks-p resolve-symlinks-p)))))

(defun %path-traversal-attempt-p (path)
  (let* ((raw (%path-string path))
         (trimmed (and raw (%trim-path-whitespace raw))))
    (and trimmed
         (loop for segment in (uiop:split-string (substitute #\/ #\\ trimmed)
                                                 :separator "/")
               thereis (string= segment "..")))))

(defun %path-outside-project-root-p (path project-root)
  (and project-root
       path
       (not (%path-under-directory-p path project-root))))

(defun %normalize-path (path &key (resolve-symlinks-p t))
  (let ((raw (%path-string path)))
    (when raw
      (let* ((trimmed (%normalize-drive-letter (%trim-path-whitespace raw))))
        (when (> (length trimmed) 0)
          (let* ((slash-normalized (%normalize-slashes trimmed))
                 (absolute (%ensure-absolute-path slash-normalized))
                 (collapsed (%collapse-dot-segments absolute))
                 (resolved (and resolve-symlinks-p
                                (or (ignore-errors
                                      (truename (pathname collapsed)))
                                    (probe-file collapsed))))
                 (canonical (if resolved
                                (%normalize-slashes (namestring resolved))
                                collapsed)))
            (%trim-trailing-slash canonical)))))))

(defun %contains-glob-char-p (string)
  (and string
       (loop for ch across string
             thereis (find ch "*?[]{}" :test #'char=))))

(defun %normalize-permission-command (command)
  (let* ((raw (%command-string command))
         (trimmed (%trim-command-whitespace raw)))
    (when (> (length trimmed) 0)
      trimmed)))

(defun %string-prefix-ci-p (prefix value)
  (and (stringp prefix)
       (stringp value)
       (<= (length prefix) (length value))
       (string-equal prefix value :end2 (length prefix))))

(defun %command-regex-body (pattern)
  (cond
    ((%string-prefix-ci-p "regex:" pattern)
     (subseq pattern (length "regex:")))
    ((%string-prefix-ci-p "re:" pattern)
     (subseq pattern (length "re:")))
    (t nil)))

(defun %command-pattern-kind (pattern)
  (let* ((normalized (%normalize-permission-command pattern))
         (regex-body (and normalized (%command-regex-body normalized)))
         (length* (and normalized (length normalized))))
    (cond
      ((or (null normalized)
           (string= normalized "")
           (string= normalized "*"))
       :wildcard)
      (regex-body
       :regex)
      ((and length*
            (> length* 1)
            (= (count #\* normalized) 1)
            (char= (char normalized (1- length*)) #\*)
            (not (find #\? normalized :test #'char=))
            (not (find #\[ normalized :test #'char=))
            (not (find #\] normalized :test #'char=))
            (not (find #\{ normalized :test #'char=))
            (not (find #\} normalized :test #'char=)))
       :prefix)
      ((%contains-glob-char-p normalized)
       :glob)
      (t
       :exact))))

;;; --- Argument Pattern Selector Dispatch Table (FP-Refine Phase 2, Target 3) ---

(defparameter +argument-pattern-selectors+
  '(("program:"    . :program)
    ("prog:"       . :program)
    ("flag:"       . :flag)
    ("flags:"      . :flag)
    ("option:"     . :flag)
    ("options:"    . :flag)
    ("positional:" . :positional)
    ("position:"   . :positional)
    ("pos:"        . :positional)
    ("arg:"        . :argument)
    ("args:"       . :argument)
    ("token:"      . :token)
    ("argv:"       . :token))
  "Dispatch table mapping prefix strings to argument selector keywords.
Used by %argument-pattern-components to classify argument patterns.")

(defun %argument-pattern-components (pattern)
  (let ((normalized (%normalize-permission-command pattern)))
    (when normalized
      (let ((match (assoc-if (lambda (prefix)
                               (%string-prefix-ci-p prefix normalized))
                             +argument-pattern-selectors+)))
        (if match
            (values (cdr match)
                    (%trim-command-whitespace
                     (subseq normalized (length (car match)))))
            (values :argument normalized))))))

(defun %validate-argument-pattern (argument-pattern)
  (let ((normalized (%normalize-permission-command argument-pattern)))
    (when normalized
      (multiple-value-bind (_selector pattern)
          (%argument-pattern-components normalized)
        (declare (ignore _selector))
        (when (string= pattern "")
          (error "Argument pattern must not be empty, got ~S."
                 argument-pattern))
        (let ((kind (%command-pattern-kind pattern)))
          (when (eq kind :regex)
            (let ((regex-body (%command-regex-body pattern)))
              (when (or (null regex-body)
                        (string= (%trim-command-whitespace regex-body) ""))
                (error "Argument regex pattern must not be empty, got ~S."
                       argument-pattern))
              (handler-case
                  (cl-ppcre:create-scanner regex-body)
                (error (condition)
                  (error "Invalid argument regex pattern ~S: ~A"
                         argument-pattern
                         condition))))))))
    normalized))

(defun %normalize-rule-arguments (arguments)
  (cond
    ((null arguments) nil)
    ((listp arguments)
     (loop for argument-pattern in arguments
           for normalized = (%validate-argument-pattern argument-pattern)
           when normalized
             collect normalized))
    (t
     (let ((normalized (%validate-argument-pattern arguments)))
       (if normalized
           (list normalized)
           nil)))))

(defun %argument-pattern-candidates (argument-profile selector)
  (case selector
    (:program
     (let ((program (getf argument-profile :program)))
       (if program
           (list program)
           '())))
    (:flag
     (copy-list (or (getf argument-profile :flags) '())))
    (:positional
     (copy-list (or (getf argument-profile :positionals) '())))
    (:token
     (copy-list (or (getf argument-profile :argv) '())))
    (otherwise
     (copy-list (or (getf argument-profile :arguments) '())))))

(defun %argument-pattern-matches-p (argument-pattern argument-profile)
  (multiple-value-bind (selector matcher)
      (%argument-pattern-components argument-pattern)
    (and matcher
         (let ((candidates (%argument-pattern-candidates argument-profile selector)))
           (loop for candidate in candidates
                 thereis (%command-matches-pattern-p candidate
                                                    matcher
                                                    :command-normalized-p t))))))

(defun %rule-arguments-match-p (rule canonical-command)
  (let ((rule-arguments (permission-rule-arguments rule)))
    (if (null rule-arguments)
        t
        (let ((argument-profile
                (%command-argument-profile-from-canonical canonical-command)))
          (and argument-profile
               (loop for argument-pattern in rule-arguments
                     always (%argument-pattern-matches-p argument-pattern
                                                        argument-profile)))))))

(defun dangerous-command-p (command &optional (patterns *dangerous-command-patterns*))
  (let ((reasons (%command-danger-reason-codes command patterns)))
    (and reasons (plusp (length reasons)))))
