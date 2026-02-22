(in-package :amoebum)

(defparameter *permission-rules* nil)
(defparameter *path-approval-memory* '())
(defparameter *path-approval-memory-limit* 256)
(defparameter *path-approval-persistence-relative-path* #P".amoebum/permissions.lisp")
(defvar *path-approval-memory-loaded-p* nil)

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

(defparameter *plan-mode-blocked-tool-names*
  '("write-file" "edit-file"))

(defstruct (permission-rule
            (:constructor make-permission-rule
                (&key effect path command tool (source :project))))
  effect
  path
  command
  tool
  source)

(defstruct (path-approval-entry
            (:constructor make-path-approval-entry
                (&key tool path scope created-at uses-remaining)))
  tool
  path
  scope
  created-at
  uses-remaining)

(defstruct (command-canonical-form
            (:constructor make-command-canonical-form
                (&key raw normalized policy-key executable argv operators wrappers commands
                      ast operator-metadata canonical-signature dangerous-reason-codes)))
  raw
  normalized
  policy-key
  executable
  argv
  operators
  wrappers
  commands
  ast
  operator-metadata
  canonical-signature
  dangerous-reason-codes)

(defparameter *last-command-canonicalization-trace* nil)

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

(defun %command-string (command)
  (typecase command
    (null nil)
    (string command)
    (symbol (symbol-name command))
    (pathname (namestring command))
    (t (prin1-to-string command))))

(defun %command-list-string (command)
  (when (and (listp command) command)
    (%trim-command-whitespace
     (format nil "~{~A~^ ~}"
             (loop for item in command
                   for value = (%trim-command-whitespace (%command-string item))
                   when (and (stringp value)
                            (plusp (length value)))
                     collect value)))))

(defun %command-raw-text (command)
  (let ((raw (if (listp command)
                 (%command-list-string command)
                 (%command-string command))))
    (when raw
      (let ((trimmed (%trim-command-whitespace raw)))
        (when (> (length trimmed) 0)
          trimmed)))))

(defun %shell-whitespace-char-p (char)
  (or (char= char #\Space)
      (char= char #\Tab)
      (char= char #\Newline)
      (char= char #\Return)))

(defun %shell-operator-at (text index)
  (let* ((len (length text))
         (remaining (- len index)))
    (flet ((prefix-p (token)
             (and (>= remaining (length token))
                  (string= token text
                           :start1 0
                           :end1 (length token)
                           :start2 index
                           :end2 (+ index (length token))))))
      (cond
        ((prefix-p "&&") (values "&&" 2))
        ((prefix-p "||") (values "||" 2))
        ((prefix-p "|&") (values "|&" 2))
        ((prefix-p "2>>") (values "2>>" 3))
        ((prefix-p "2>") (values "2>" 2))
        ((prefix-p "&>") (values "&>" 2))
        ((prefix-p ">>") (values ">>" 2))
        ((prefix-p "<<") (values "<<" 2))
        ((find (char text index) "|;&()<>"
               :test #'char=)
         (values (string (char text index)) 1))
        (t
         (values nil 0))))))

(defun %shell-safe-char-p (char)
  (or (and (>= (char-code char) (char-code #\a))
           (<= (char-code char) (char-code #\z)))
      (and (>= (char-code char) (char-code #\A))
           (<= (char-code char) (char-code #\Z)))
      (and (>= (char-code char) (char-code #\0))
           (<= (char-code char) (char-code #\9)))
      (find char "-._/:=+%@,"
            :test #'char=)))

(defun %shell-single-quote (text)
  (with-output-to-string (stream)
    (write-char #\' stream)
    (loop for char across text do
          (if (char= char #\')
              (write-string "'\"'\"'" stream)
              (write-char char stream)))
    (write-char #\' stream)))

(defun %canonical-shell-word (word)
  (let ((text (or word "")))
    (if (and (> (length text) 0)
             (loop for char across text
                   always (%shell-safe-char-p char)))
        text
        (%shell-single-quote text))))

(defun %tokenize-shell-command (text)
  (let ((tokens '())
        (in-single-p nil)
        (in-double-p nil)
        (escape-next-p nil)
        (buffer (make-string-output-stream))
        (len (length text)))
    (labels ((emit-word ()
               (let ((value (get-output-stream-string buffer)))
                 (when (> (length value) 0)
                   (push (cons :word value) tokens))))
             (emit-operator (value)
               (push (cons :operator value) tokens)))
      (loop for index from 0 below len do
            (let ((char (char text index)))
              (cond
                (escape-next-p
                 (write-char char buffer)
                 (setf escape-next-p nil))
                (in-single-p
                 (if (char= char #\')
                     (setf in-single-p nil)
                     (write-char char buffer)))
                (in-double-p
                 (cond
                   ((char= char #\\)
                    (if (< (1+ index) len)
                        (progn
                          (incf index)
                          (write-char (char text index) buffer))
                        (write-char char buffer)))
                   ((char= char #\")
                    (setf in-double-p nil))
                   (t
                    (write-char char buffer))))
                (t
                 (cond
                   ((char= char #\\)
                    (setf escape-next-p t))
                   ((char= char #\')
                    (setf in-single-p t))
                   ((char= char #\")
                    (setf in-double-p t))
                   ((%shell-whitespace-char-p char)
                    (emit-word))
                   (t
                    (multiple-value-bind (operator width)
                        (%shell-operator-at text index)
                      (if operator
                          (progn
                            (emit-word)
                            (emit-operator operator)
                            (incf index (1- width)))
                          (write-char char buffer)))))))))
      (when escape-next-p
        (write-char #\\ buffer))
      (emit-word)
      (nreverse tokens))))

(defun %separator-operator-p (operator)
  (member operator '("|" "||" "&&" ";" "&")
          :test #'string=))

(defun %redirection-operator-p (operator)
  (member operator '(">" ">>" "<" "<<" "2>" "2>>" "&>")
          :test #'string=))

(defun %canonicalize-shell-tokens (tokens)
  (when tokens
    (with-output-to-string (stream)
      (loop for token in tokens
            for index from 0 do
              (when (> index 0)
                (write-char #\Space stream))
              (ecase (car token)
                (:word
                 (write-string (%canonical-shell-word (cdr token)) stream))
                (:operator
                 (write-string (cdr token) stream)))))))

(defun %tokens->command-segments (tokens)
  (let ((segments '())
        (operators '())
        (current '()))
    (labels ((flush-segment ()
               (when current
                 (push (nreverse current) segments)
                 (setf current '()))))
      (dolist (token tokens)
        (ecase (car token)
          (:word
           (push (cdr token) current))
          (:operator
           (let ((operator (cdr token)))
             (push operator operators)
             (when (%separator-operator-p operator)
               (flush-segment))))))
      (flush-segment))
    (values (nreverse segments) (nreverse operators))))

(defun %tokens->ast (tokens)
  (let ((ast '())
        (current-argv '())
        (current-redirections '())
        (command-index 0))
    (labels ((flush-command ()
               (when (or current-argv current-redirections)
                 (push (list :type :command
                             :index command-index
                             :argv (nreverse current-argv)
                             :redirections (nreverse current-redirections))
                       ast)
                 (incf command-index)
                 (setf current-argv '()
                       current-redirections '()))))
      (dolist (token tokens)
        (ecase (car token)
          (:word
           (push (cdr token) current-argv))
          (:operator
           (let ((operator (cdr token)))
             (if (%separator-operator-p operator)
                 (progn
                   (flush-command)
                   (push (list :type :operator
                               :value operator)
                         ast))
                 (push operator current-redirections))))))
      (flush-command)
      (nreverse ast))))

(defun %command-env-assignment-p (value)
  (and (stringp value)
       (> (length value) 1)
       (let ((equals (position #\= value)))
         (and equals
              (> equals 0)
              (let ((first (char value 0)))
                (or (char= first #\_)
                    (alpha-char-p first)))
              (loop for index from 1 below equals
                    for char = (char value index)
                    always (or (char= char #\_)
                               (alpha-char-p char)
                               (digit-char-p char)))))))

(defun %canonicalize-env-assignments (assignments)
  (let ((table (make-hash-table :test #'equal))
        (keys '()))
    (dolist (assignment assignments)
      (when (%command-env-assignment-p assignment)
        (let ((key (subseq assignment 0 (position #\= assignment))))
          (unless (gethash key table)
            (push key keys))
          (setf (gethash key table) assignment))))
    (loop for key in (sort (copy-list keys)
                           #'string<
                           :key #'string-downcase)
          collect (gethash key table))))

(defun %unwrap-env-command (argv)
  (if (and argv (string= (string-downcase (first argv)) "env"))
      (let ((remaining (rest argv))
            (options '())
            (assignments '()))
        (loop while remaining do
              (let ((token (first remaining)))
                (cond
                  ((string= token "--")
                   (setf remaining (rest remaining))
                   (return))
                  ((and (stringp token)
                        (> (length token) 1)
                        (char= (char token 0) #\-)
                        (not (%command-env-assignment-p token)))
                   (push token options)
                   (setf remaining (rest remaining))
                   (when (and remaining
                              (member token '("-u" "--unset")
                                      :test #'string=))
                     (push (first remaining) options)
                     (setf remaining (rest remaining))))
                  (t
                   (return)))))
        (loop while (and remaining
                         (%command-env-assignment-p (first remaining))) do
              (push (first remaining) assignments)
              (setf remaining (rest remaining)))
        (if remaining
            (values remaining
                    (list :type :env
                          :options (nreverse options)
                          :assignments (%canonicalize-env-assignments
                                        (nreverse assignments))))
            (values argv nil)))
      (values argv nil)))

(defun %unwrap-shell-c-command (argv)
  (let ((program (and argv (string-downcase (first argv)))))
    (if (member program '("bash" "sh")
                :test #'string=)
        (block done
          (let ((remaining (rest argv))
                (options '()))
            (loop while remaining do
                  (let ((token (first remaining)))
                    (cond
                      ((member token '("-c" "-lc" "-cl")
                               :test #'string=)
                       (let ((script (second remaining))
                             (tail (cddr remaining)))
                         (unless (stringp script)
                           (return-from done (values argv nil)))
                         (let* ((nested (canonicalize-permission-command script))
                                (nested-argv (if nested
                                                 (command-canonical-form-argv nested)
                                                 nil))
                                (wrapper (list :type :shell
                                               :program program
                                               :options (nreverse (cons token options))
                                               :raw-script script
                                               :normalized-script
                                               (and nested
                                                    (command-canonical-form-normalized nested)))))
                           (return-from done
                             (values (append nested-argv tail) wrapper)))))
                      ((and (stringp token)
                            (> (length token) 1)
                            (char= (char token 0) #\-))
                       (push token options)
                       (setf remaining (rest remaining)))
                      (t
                       (return-from done (values argv nil)))))))
          (values argv nil))
        (values argv nil))))

(defun %canonical-argv-string (argv)
  (when argv
    (format nil "~{~A~^ ~}"
            (mapcar #'%canonical-shell-word argv))))

(defun %command-policy-key (argv wrappers)
  (let ((prefixes
          (loop for wrapper in wrappers
                when (eq (getf wrapper :type) :env)
                  collect (let ((assignments (getf wrapper :assignments))
                                (options (getf wrapper :options)))
                            (string-trim
                             '(#\Space)
                             (format nil "env ~@[~{~A~^ ~} ~]~:[~;~{~A~^ ~}~]"
                                     options
                                     assignments
                                     assignments))))))
    (let ((base (%canonical-argv-string argv)))
      (if (or prefixes base)
          (string-trim
           '(#\Space)
           (format nil "~@[~{~A~^ ~} ~]~A"
                   prefixes
                   (or base "")))
          nil))))

(defun %operator-metadata (operators)
  (list :operators operators
        :contains-pipeline (member "|" operators :test #'string=)
        :contains-logical-and (member "&&" operators :test #'string=)
        :contains-logical-or (member "||" operators :test #'string=)
        :contains-separator (member ";" operators :test #'string=)
        :contains-background (member "&" operators :test #'string=)
        :contains-redirection
        (loop for operator in operators
              thereis (%redirection-operator-p operator))))

(defun %wrapper-signature (wrappers)
  (when wrappers
    (format nil "~{~A~^|~}"
            (loop for wrapper in wrappers
                  collect (case (getf wrapper :type)
                            (:env
                             (format nil "env[~{~A~^,~}]"
                                     (or (getf wrapper :assignments) '())))
                            (:shell
                             (format nil "shell[~A]"
                                     (or (getf wrapper :program) "")))
                            (otherwise
                             (format nil "wrapper[~A]"
                                     (or (getf wrapper :type) ""))))))))

(defun %command-canonical-signature (argv wrappers operators)
  (let* ((argv* (%canonical-argv-string argv))
         (wrapper* (%wrapper-signature wrappers))
         (operators* (and operators
                          (format nil "~{~A~^,~}" operators))))
    (format nil "argv=~A|wrappers=~A|operators=~A"
            (or argv* "")
            (or wrapper* "")
            (or operators* ""))))

(defparameter *interactive-command-programs*
  '("vim" "vi" "nvim" "nano" "emacs" "less" "more" "man" "top" "htop" "watch" "tailf"))

(defun %segment->executable (segment)
  (loop for token in segment
        unless (%command-env-assignment-p token)
          do (return (string-downcase token))
        finally (return nil)))

(defun %segment-has-argv-flag-p (segment flag)
  (member flag segment :test #'string=))

(defun %command-danger-reason-codes (canonical &optional (patterns *dangerous-command-patterns*))
  (let* ((canonical* (if (typep canonical 'command-canonical-form)
                         canonical
                         (canonicalize-permission-command canonical)))
         (normalized (and canonical* (command-canonical-form-normalized canonical*)))
         (wrappers (and canonical* (command-canonical-form-wrappers canonical*)))
         (commands (and canonical* (command-canonical-form-commands canonical*)))
         (reasons '()))
    (when (and normalized
               (loop for pattern in patterns
                     thereis (cl-ppcre:scan pattern normalized)))
      (push :dangerous-pattern-match reasons))
    (dolist (command commands)
      (let ((executable (%segment->executable command)))
        (when (and executable
                   (member executable *interactive-command-programs* :test #'string=))
          (push :interactive-command-class reasons))
        (when (and executable
                   (string= executable "ssh")
                   (not (%segment-has-argv-flag-p command "-T")))
          (push :interactive-ssh-session reasons))))
    (when wrappers
      (dolist (wrapper wrappers)
        (when (eq (getf wrapper :type) :shell)
          (let* ((normalized-script (getf wrapper :normalized-script))
                 (nested (and normalized-script
                              (canonicalize-permission-command normalized-script)))
                 (nested-reasons (and nested
                                      (%command-danger-reason-codes nested patterns))))
            (when nested-reasons
              (dolist (reason nested-reasons)
                (push reason reasons))
              (push :shell-wrapper-expanded reasons)))))
      (when (and reasons
                 (find :env wrappers
                       :key (lambda (wrapper) (getf wrapper :type))
                       :test #'eq))
        (push :env-wrapper-expanded reasons)))
    (nreverse (remove-duplicates reasons :test #'eq))))

(defun canonicalize-permission-command (command)
  (let ((raw (%command-raw-text command)))
    (when raw
      (let* ((tokens (if (listp command)
                         (loop for item in command
                               for value = (%trim-command-whitespace (%command-string item))
                               when (and (stringp value) (plusp (length value)))
                                 collect (cons :word value))
                         (%tokenize-shell-command raw)))
            (normalized (%canonicalize-shell-tokens tokens)))
        (multiple-value-bind (commands operators)
            (%tokens->command-segments tokens)
          (let* ((primary-argv (copy-list (or (first commands) '())))
                 (wrappers '()))
            (loop repeat 8 do
                  (let ((changed-p nil))
                    (multiple-value-bind (after-env env-wrapper)
                        (%unwrap-env-command primary-argv)
                      (when env-wrapper
                        (setf primary-argv after-env
                              changed-p t)
                        (push env-wrapper wrappers)))
                    (multiple-value-bind (after-shell shell-wrapper)
                        (%unwrap-shell-c-command primary-argv)
                      (when shell-wrapper
                        (setf primary-argv after-shell
                              changed-p t)
                        (push shell-wrapper wrappers)))
                    (unless changed-p
                      (return))))
            (let* ((normalized-wrappers (nreverse wrappers))
                   (operator-metadata (%operator-metadata operators))
                   (canonical-signature (%command-canonical-signature primary-argv
                                                                      normalized-wrappers
                                                                      operators))
                   (canonical
                     (make-command-canonical-form
                      :raw raw
                      :normalized normalized
                      :policy-key (%command-policy-key primary-argv normalized-wrappers)
                      :executable (first primary-argv)
                      :argv primary-argv
                      :operators operators
                      :wrappers normalized-wrappers
                      :commands commands
                      :ast (%tokens->ast tokens)
                      :operator-metadata operator-metadata
                      :canonical-signature canonical-signature)))
              (setf (command-canonical-form-dangerous-reason-codes canonical)
                    (%command-danger-reason-codes canonical))
              (setf *last-command-canonicalization-trace*
                    (list :raw (command-canonical-form-raw canonical)
                          :normalized (command-canonical-form-normalized canonical)
                          :policy-key (command-canonical-form-policy-key canonical)
                          :canonical-signature canonical-signature
                          :operator-metadata operator-metadata
                          :wrappers (command-canonical-form-wrappers canonical)
                          :operators (command-canonical-form-operators canonical)
                          :dangerous-reason-codes
                          (command-canonical-form-dangerous-reason-codes canonical)))
              canonical)))))))

(defun command-canonicalization-trace ()
  *last-command-canonicalization-trace*)

(defun %permission-command-cache-key (tool canonical)
  (list :tool (%tool-name tool)
        :canonical-signature (and canonical
                                  (command-canonical-form-canonical-signature canonical))
        :operator-metadata (and canonical
                                (command-canonical-form-operator-metadata canonical))))

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

(defun %join-path-segments (segments)
  (if segments
      (format nil "~{~A~^/~}" segments)
      ""))

(defun normalize-permission-path (path &key (preserve-trailing-slash-p nil))
  (let* ((raw (%path-string path))
         (trimmed (and raw (string-trim '(#\Space #\Tab #\Newline #\Return) raw))))
    (when (and trimmed (> (length trimmed) 0))
      (let* ((had-trailing-separator-p (%path-has-trailing-separator-p trimmed))
             (source (substitute #\/ #\\ trimmed))
             (kind :relative)
             (root "")
             (rest source))
        (labels ((ascii-alpha-p (char)
                   (or (and (>= (char-code char) (char-code #\a))
                            (<= (char-code char) (char-code #\z)))
                       (and (>= (char-code char) (char-code #\A))
                            (<= (char-code char) (char-code #\Z)))))
                 (root-only-p (candidate)
                   (case kind
                     (:absolute (string= candidate "/"))
                     (:drive (string= candidate root))
                     (:unc (string= candidate root))
                     (:relative (string= candidate "."))
                     (otherwise nil))))
          (cond
            ((and (>= (length source) 2)
                  (char= (char source 1) #\:)
                  (ascii-alpha-p (char source 0)))
             (setf kind :drive
                   root (format nil "~A:/" (string-downcase (subseq source 0 1)))
                   rest (string-left-trim "/" (subseq source 2))))
            ((uiop:string-prefix-p "//" source)
             (setf kind :unc)
             (let* ((parts (uiop:split-string (subseq source 2) :separator "/"))
                    (server (first parts))
                    (share (second parts))
                    (remaining (cddr parts)))
               (setf root (cond
                            ((and server share)
                             (format nil "//~A/~A"
                                     (string-downcase server)
                                     (string-downcase share)))
                            (server
                             (format nil "//~A" (string-downcase server)))
                            (t "//"))
                     rest (%join-path-segments remaining))))
            ((uiop:string-prefix-p "/" source)
             (setf kind :absolute
                   root "/"
                   rest (string-left-trim "/" source))))
          (let ((segments '()))
            (dolist (segment (if (string= rest "")
                                 '()
                                 (uiop:split-string rest :separator "/")))
              (cond
                ((or (string= segment "")
                     (string= segment "."))
                 nil)
                ((string= segment "..")
                 (if (and segments
                          (not (string= (car segments) "..")))
                     (pop segments)
                     (when (eq kind :relative)
                       (push segment segments))))
                (t
                 (push segment segments))))
            (let* ((normalized-segments (nreverse segments))
                   (joined (%join-path-segments normalized-segments))
                   (normalized
                     (case kind
                       (:absolute (if (string= joined "")
                                      "/"
                                      (concatenate 'string "/" joined)))
                       (:drive (if (string= joined "")
                                   root
                                   (concatenate 'string root joined)))
                       (:unc (if (string= joined "")
                                 root
                                 (concatenate 'string root "/" joined)))
                       (otherwise (if (string= joined "")
                                      "."
                                      joined)))))
              (if (and preserve-trailing-slash-p
                       had-trailing-separator-p
                       (not (root-only-p normalized))
                       (not (char= (char normalized (1- (length normalized))) #\/)))
                  (concatenate 'string normalized "/")
                  normalized))))))))

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

(defun %normalize-pattern-path (pattern)
  (let ((raw (%path-string pattern)))
    (when raw
      (let* ((trimmed (%normalize-drive-letter (%trim-path-whitespace raw))))
        (when (> (length trimmed) 0)
          (let ((slash-normalized (%normalize-slashes trimmed)))
            (if (%contains-glob-char-p slash-normalized)
                (%trim-trailing-slash
                 (%collapse-dot-segments
                  (%ensure-absolute-path slash-normalized)))
                (%normalize-path slash-normalized))))))))

(defun %normalize-path-approval-scope (scope)
  (let ((normalized
          (cond
            ((keywordp scope) scope)
            ((stringp scope)
             (intern (string-upcase
                      (string-trim '(#\Space #\Tab #\Newline #\Return) scope))
                     :keyword))
            ((symbolp scope)
             (intern (string-upcase (symbol-name scope)) :keyword))
            (t nil))))
    (case normalized
      (:once :once)
      (:session :session)
      (:always :always)
      (otherwise nil))))

(defun %path-approval-project-root (&optional project-root)
  (let ((candidate
          (or project-root
              (ignore-errors (config-project-root (current-config)))
              *default-pathname-defaults*)))
    (uiop:ensure-directory-pathname
     (or (ignore-errors (truename candidate))
         candidate))))

(defun path-approval-store-path (&key project-root)
  (merge-pathnames *path-approval-persistence-relative-path*
                   (%path-approval-project-root project-root)))

(defun %path-approval-persistent-p (entry)
  (eq (path-approval-entry-scope entry) :always))

(defun %path-approval-match-p (entry tool-name normalized-path &optional scope)
  (and (string= (path-approval-entry-tool entry) tool-name)
       (string= (path-approval-entry-path entry) normalized-path)
       (or (null scope)
           (eq (path-approval-entry-scope entry) scope))))

(defun %path-approval-entry-sort-key (entry)
  (or (path-approval-entry-created-at entry) 0))

(defun %normalize-persisted-path-approval-entry (entry)
  (let* ((tool (%tool-name (getf entry :tool)))
         (path (%normalize-path (getf entry :path))))
    (when (and tool path)
      (make-path-approval-entry
       :tool tool
       :path path
       :scope :always
       :created-at (or (getf entry :created-at) (get-universal-time))
       :uses-remaining nil))))

(defun %trim-path-approval-memory ()
  (when (> (length *path-approval-memory*) *path-approval-memory-limit*)
    (setf *path-approval-memory*
          (subseq
           (sort (copy-list *path-approval-memory*) #'>
                 :key #'%path-approval-entry-sort-key)
           0
           *path-approval-memory-limit*))))

(defun %serialize-path-approval-memory ()
  (let ((entries
          (loop for entry in (sort (copy-list *path-approval-memory*) #'>
                                   :key #'%path-approval-entry-sort-key)
                when (%path-approval-persistent-p entry)
                collect (list :tool (path-approval-entry-tool entry)
                              :path (path-approval-entry-path entry)
                              :scope :always
                              :created-at (path-approval-entry-created-at entry)))))
    (list :version 1 :entries entries)))

(defun %read-path-approval-memory-form (path)
  (when (probe-file path)
    (with-open-file (stream path
                            :direction :input
                            :if-does-not-exist nil
                            :external-format :utf-8)
      (with-standard-io-syntax
        (read stream nil nil)))))

(defun save-path-approvals (&key project-root)
  (let ((path (path-approval-store-path :project-root project-root))
        (payload (%serialize-path-approval-memory)))
    (ensure-directories-exist path)
    (with-open-file (stream path
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create
                            :external-format :utf-8)
      (with-standard-io-syntax
        (write payload :stream stream :escape t :circle nil :pretty t)))
    (length (getf payload :entries))))

(defun %merge-persistent-path-approval-entry (entry)
  (when entry
    (setf *path-approval-memory*
          (cons entry
                (remove-if (lambda (candidate)
                             (%path-approval-match-p candidate
                                                     (path-approval-entry-tool entry)
                                                     (path-approval-entry-path entry)
                                                     :always))
                           *path-approval-memory*)))))

(defun load-path-approvals (&key project-root)
  (let* ((path (path-approval-store-path :project-root project-root))
         (payload (%read-path-approval-memory-form path))
         (raw-entries (and (listp payload) (getf payload :entries)))
         (loaded 0))
    (when (listp raw-entries)
      (dolist (entry raw-entries)
        (let ((normalized (%normalize-persisted-path-approval-entry entry)))
          (when normalized
            (%merge-persistent-path-approval-entry normalized)
            (incf loaded)))))
    (%trim-path-approval-memory)
    (setf *path-approval-memory-loaded-p* t)
    loaded))

(defun %ensure-path-approvals-loaded ()
  (unless *path-approval-memory-loaded-p*
    (ignore-errors
      (load-path-approvals))
    (setf *path-approval-memory-loaded-p* t)))

(defun clear-path-approvals (&key (include-persistent nil) project-root)
  (%ensure-path-approvals-loaded)
  (let ((before (length *path-approval-memory*)))
    (if include-persistent
        (setf *path-approval-memory* '())
        (setf *path-approval-memory*
              (remove-if-not #'%path-approval-persistent-p *path-approval-memory*)))
    (when include-persistent
      (let ((path (path-approval-store-path :project-root project-root)))
        (when (probe-file path)
          (ignore-errors
            (delete-file path)))))
    (max 0 (- before (length *path-approval-memory*)))))

(defun list-path-approvals (&key scope)
  (%ensure-path-approvals-loaded)
  (let ((normalized-scope (and scope (%normalize-path-approval-scope scope))))
    (when (and scope (null normalized-scope))
      (error "Unknown path approval scope ~S. Expected :once, :session, or :always." scope))
    (sort
     (copy-list
      (if normalized-scope
          (remove-if-not (lambda (entry)
                           (eq (path-approval-entry-scope entry) normalized-scope))
                         *path-approval-memory*)
          *path-approval-memory*))
     #'>
     :key #'%path-approval-entry-sort-key)))

(defun remember-path-approval (&key tool path (scope :session) (persist-p t) project-root)
  (%ensure-path-approvals-loaded)
  (let* ((tool-name (%tool-name tool))
         (normalized-path (%normalize-path path))
         (normalized-scope (%normalize-path-approval-scope scope)))
    (unless tool-name
      (error "Path approvals require a tool name, got ~S." tool))
    (unless normalized-path
      (error "Path approvals require a path, got ~S." path))
    (unless normalized-scope
      (error "Unknown path approval scope ~S. Expected :once, :session, or :always." scope))
    (setf *path-approval-memory*
          (cons (make-path-approval-entry
                 :tool tool-name
                 :path normalized-path
                 :scope normalized-scope
                 :created-at (get-universal-time)
                 :uses-remaining (when (eq normalized-scope :once) 1))
                (remove-if (lambda (entry)
                             (%path-approval-match-p entry tool-name normalized-path))
                           *path-approval-memory*)))
    (%trim-path-approval-memory)
    (when (and persist-p (eq normalized-scope :always))
      (save-path-approvals :project-root project-root))
    (first *path-approval-memory*)))

(defun forget-path-approval (&key tool path scope (persist-p t) project-root)
  (%ensure-path-approvals-loaded)
  (let* ((tool-name (%tool-name tool))
         (normalized-path (%normalize-path path))
         (normalized-scope (and scope (%normalize-path-approval-scope scope)))
         (before (length *path-approval-memory*)))
    (when (and scope (null normalized-scope))
      (error "Unknown path approval scope ~S. Expected :once, :session, or :always." scope))
    (setf *path-approval-memory*
          (remove-if
           (lambda (entry)
             (and (or (null tool-name)
                      (string= (path-approval-entry-tool entry) tool-name))
                  (or (null normalized-path)
                      (string= (path-approval-entry-path entry) normalized-path))
                  (or (null normalized-scope)
                      (eq (path-approval-entry-scope entry) normalized-scope))))
           *path-approval-memory*))
    (let ((removed (max 0 (- before (length *path-approval-memory*)))))
      (when (and persist-p (> removed 0))
        (save-path-approvals :project-root project-root))
      removed)))

(defun %path-memory-allows-p (tool path)
  (%ensure-path-approvals-loaded)
  (let* ((tool-name (%tool-name tool))
         (normalized-path (%normalize-path path))
         (entry (and tool-name
                     normalized-path
                     (find-if (lambda (candidate)
                                (%path-approval-match-p candidate tool-name normalized-path))
                              *path-approval-memory*))))
    (when entry
      (when (eq (path-approval-entry-scope entry) :once)
        (let ((remaining (or (path-approval-entry-uses-remaining entry) 1)))
          (if (<= remaining 1)
              (setf *path-approval-memory*
                    (delete entry *path-approval-memory* :test #'eq))
              (setf (path-approval-entry-uses-remaining entry)
                    (1- remaining)))))
      t)))

(defun %contains-glob-char-p (string)
  (and string
       (loop for ch across string
             thereis (find ch "*?[]{}" :test #'char=))))

(defun %trim-command-whitespace (value)
  (if (stringp value)
      (string-trim '(#\Space #\Tab #\Newline #\Return) value)
      ""))

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

(defun %path-kind (pattern)
  (let* ((raw (%path-string pattern))
         (normalized (and raw
                          (%normalize-slashes (%trim-path-whitespace raw)))))
    (cond
      ((or (null normalized)
           (string= normalized "")
           (member normalized '("*" "**" "**/*" "/*") :test #'string=))
       :wildcard)
      ((and (> (length normalized) 0)
            (char= (char normalized (1- (length normalized))) #\/)
            (not (%contains-glob-char-p normalized)))
       :directory)
      ((%contains-glob-char-p normalized) :glob)
      (t :exact))))

(defun %regex-escape-char (char stream)
  (when (find char "\\.^$|()[]{}+?" :test #'char=)
    (write-char #\\ stream))
  (write-char char stream))

(defun %glob->regex (pattern)
  (let* ((source (or (%normalize-pattern-path pattern) ""))
         (len (length source)))
    (with-output-to-string (stream)
      (write-char #\^ stream)
      (loop for i from 0 below len do
            (let ((ch (char source i)))
              (cond
                ((char= ch #\*)
                 (if (and (< (1+ i) len)
                          (char= (char source (1+ i)) #\*))
                     (progn
                       (incf i)
                       ;; Treat **/ as zero-or-more directory segments so
                       ;; patterns like src/**/*.lisp also match src/main.lisp.
                       (if (and (< (1+ i) len)
                                (char= (char source (1+ i)) #\/))
                           (progn
                             (incf i)
                             (write-string "(?:[^/]+/)*" stream))
                           (write-string ".*" stream)))
                     (write-string "[^/]*" stream)))
                ((char= ch #\?)
                 (write-string "[^/]" stream))
                ((char= ch #\[)
                 (let ((close (position #\] source :start (1+ i))))
                   (if close
                       (progn
                         (write-string (subseq source i (1+ close)) stream)
                         (setf i close))
                       (%regex-escape-char ch stream))))
                ((char= ch #\{)
                 (let ((close (position #\} source :start (1+ i))))
                   (if close
                       (let ((inner (subseq source (1+ i) close))
                             (alternatives ()))
                         (setf alternatives
                               (uiop:split-string inner :separator ","))
                         (write-string "(?:" stream)
                         (loop for alt in alternatives
                               for idx from 0 do
                                 (when (> idx 0)
                                   (write-char #\| stream))
                                 (write-string (cl-ppcre:quote-meta-chars alt) stream))
                         (write-char #\) stream)
                         (setf i close))
                       (%regex-escape-char ch stream))))
                (t
                 (%regex-escape-char ch stream)))))
      (write-char #\$ stream))))

(defun %command-glob->regex (pattern)
  (let* ((source (or (%normalize-permission-command pattern) ""))
         (len (length source)))
    (with-output-to-string (stream)
      (write-char #\^ stream)
      (loop for i from 0 below len do
            (let ((ch (char source i)))
              (cond
                ((char= ch #\*)
                 (write-string ".*" stream))
                ((char= ch #\?)
                 (write-char #\. stream))
                ((char= ch #\[)
                 (let ((close (position #\] source :start (1+ i))))
                   (if close
                       (progn
                         (write-string (subseq source i (1+ close)) stream)
                         (setf i close))
                       (%regex-escape-char ch stream))))
                ((char= ch #\{)
                 (let ((close (position #\} source :start (1+ i))))
                   (if close
                       (let* ((inner (subseq source (1+ i) close))
                              (alternatives (uiop:split-string inner :separator ",")))
                         (write-string "(?:" stream)
                         (loop for alt in alternatives
                               for idx from 0 do
                                 (when (> idx 0)
                                   (write-char #\| stream))
                                 (write-string (cl-ppcre:quote-meta-chars alt) stream))
                         (write-char #\) stream)
                         (setf i close))
                       (%regex-escape-char ch stream))))
                (t
                 (%regex-escape-char ch stream)))))
      (write-char #\$ stream))))

(defun %path-under-directory-p (path directory-pattern)
  (let* ((path* (%normalize-path path))
         (dir* (%trim-trailing-slash (%normalize-pattern-path directory-pattern)))
         (prefix (if (string= dir* "/")
                     "/"
                     (concatenate 'string dir* "/"))))
    (and path*
         dir*
         (or (string= path* dir*)
             (uiop:string-prefix-p prefix path*)))))

(defun %path-matches-pattern-p (path pattern &key (path-normalized-p nil))
  (let ((kind (%path-kind pattern))
        (candidate (if path-normalized-p
                       path
                       (%normalize-path path))))
    (and candidate
         (case kind
           (:wildcard t)
           (:exact (string= candidate (%normalize-pattern-path pattern)))
           (:directory (%path-under-directory-p candidate pattern))
           (:glob (cl-ppcre:scan (%glob->regex pattern) candidate))
           (otherwise nil)))))

(defun %command-matches-pattern-p (command pattern &key (command-normalized-p nil))
  (let* ((candidate (if command-normalized-p
                        command
                        (%normalize-permission-command command)))
         (normalized-pattern (%normalize-permission-command pattern))
         (kind (%command-pattern-kind normalized-pattern)))
    (and candidate
         (case kind
           (:wildcard t)
           (:exact (string= candidate normalized-pattern))
           (:prefix (uiop:string-prefix-p
                     (subseq normalized-pattern 0 (1- (length normalized-pattern)))
                     candidate))
           (:glob (cl-ppcre:scan (%command-glob->regex normalized-pattern)
                                 candidate))
           (:regex (let ((regex-body (%command-regex-body normalized-pattern)))
                     (and regex-body
                          (cl-ppcre:scan regex-body candidate))))
           (otherwise nil)))))

(defun %tool-matches-rule-p (tool rule-tool)
  (let ((tool-name (%tool-name tool))
        (rule-tool-name (%tool-name rule-tool)))
    (or (null rule-tool-name)
        (and tool-name (string= tool-name rule-tool-name)))))

(defun %rule-matches-p (rule tool path command)
  (and (%tool-matches-rule-p tool (permission-rule-tool rule))
       (let ((rule-path (permission-rule-path rule)))
         (if rule-path
             (%path-matches-pattern-p path rule-path :path-normalized-p t)
             t))
       (let ((rule-command (permission-rule-command rule)))
         (if rule-command
             (%command-matches-pattern-p command
                                         rule-command
                                         :command-normalized-p t)
             t))))

(defun %path-specificity-score (rule)
  (case (%path-kind (permission-rule-path rule))
    (:exact 300)
    (:glob 200)
    (:directory 100)
    (:wildcard 0)
    (otherwise 0)))

(defun %command-specificity-score (rule)
  (case (%command-pattern-kind (permission-rule-command rule))
    (:exact 300)
    (:prefix 200)
    (:glob 150)
    (:regex 100)
    (:wildcard 0)
    (otherwise 0)))

(defun %specificity-score (rule)
  (+ (%path-specificity-score rule)
     (%command-specificity-score rule)))

(defun %scope-score (rule)
  (case (permission-rule-source rule)
    (:project 10)
    (:global 0)
    (otherwise 0)))

(defun %deny-rule-p (rule)
  (eq (permission-rule-effect rule) :deny))

(defun %better-rule-p (candidate best)
  (cond
    ((null best) t)
    ((> (%specificity-score candidate) (%specificity-score best)) t)
    ((< (%specificity-score candidate) (%specificity-score best)) nil)
    ((> (%scope-score candidate) (%scope-score best)) t)
    ((< (%scope-score candidate) (%scope-score best)) nil)
    ((and (%deny-rule-p candidate)
          (not (%deny-rule-p best)))
     t)
    (t nil)))

(defun clear-permission-rules ()
  (setf *permission-rules* nil))

(defun %validate-command-pattern (command-pattern)
  (let ((normalized (%normalize-permission-command command-pattern)))
    (when normalized
      (let ((kind (%command-pattern-kind normalized)))
        (when (eq kind :regex)
          (let ((regex-body (%command-regex-body normalized)))
            (when (or (null regex-body)
                      (string= (%trim-command-whitespace regex-body) ""))
              (error "Command regex pattern must not be empty, got ~S."
                     command-pattern))
            (handler-case
                (cl-ppcre:create-scanner regex-body)
              (error (condition)
                (error "Invalid command regex pattern ~S: ~A"
                       command-pattern
                       condition)))))))
    normalized))

(defun add-permission-rule (&key effect path command tool (source :project))
  (unless (member effect '(:allow :deny) :test #'eq)
    (error "Permission rule EFFECT must be :allow or :deny, got ~S." effect))
  (let ((rule (make-permission-rule :effect effect
                                    :path path
                                    :command (%validate-command-pattern command)
                                    :tool tool
                                    :source source)))
    (push rule *permission-rules*)
    rule))

(defun evaluate-path-permission (&key tool path (rules *permission-rules*))
  (let ((best nil)
        (normalized-path (%normalize-path path)))
    (when normalized-path
      (dolist (rule rules)
        (when (%rule-matches-p rule tool normalized-path nil)
          (when (%better-rule-p rule best)
            (setf best rule)))))
    (and best (permission-rule-effect best))))

(defun evaluate-command-permission (&key tool command path (rules *permission-rules*))
  (let ((best nil)
        (normalized-command (%normalize-permission-command command))
        (normalized-path (and path (%normalize-path path))))
    (when normalized-command
      (dolist (rule rules)
        (when (%rule-matches-p rule tool normalized-path normalized-command)
          (when (%better-rule-p rule best)
            (setf best rule)))))
    (and best (permission-rule-effect best))))

(defun dangerous-command-p (command &optional (patterns *dangerous-command-patterns*))
  (let ((reasons (%command-danger-reason-codes command patterns)))
    (and reasons (plusp (length reasons)))))

(defun %normalize-approval-policy (value)
  (let ((normalized
          (cond
            ((keywordp value) value)
            ((stringp value)
             (intern (string-upcase
                      (string-trim '(#\Space #\Tab #\Newline #\Return)
                                   (substitute #\- #\_ value)))
                     :keyword))
            ((symbolp value)
             (intern (string-upcase (symbol-name value)) :keyword))
            (t nil))))
    (case normalized
      (:UNTRUSTED :untrusted)
      (:ON-FAILURE :on-failure)
      (:ON_FAILURE :on-failure)
      (:ON-REQUEST :on-request)
      (:ON_REQUEST :on-request)
      (:NEVER :never)
      (otherwise nil))))

(defun %approval-policy->permission-mode (approval-policy)
  (case (%normalize-approval-policy approval-policy)
    (:untrusted :supervised)
    (:on-request :supervised)
    (:on-failure :auto-edit)
    (:never :yolo)
    (otherwise nil)))

(defun %configured-approval-policy ()
  (ignore-errors
    (config-value :approval-policy (current-config))))

(defun %effective-permission-mode (mode &optional approval-policy)
  (or
   (case mode
     ((:supervised :auto-edit :full-auto :yolo) mode)
     (:no-confirm :yolo)
     (:untrusted :supervised)
     (:on-request :supervised)
     (:on-failure :auto-edit)
     (:never :yolo)
     (otherwise nil))
   (%approval-policy->permission-mode approval-policy)
   (let ((cfg-mode (ignore-errors (config-permission-mode (current-config)))))
     (or (case cfg-mode
           ((:supervised :auto-edit :full-auto :yolo) cfg-mode)
           (:no-confirm :yolo)
           (:untrusted :supervised)
           (:on-request :supervised)
           (:on-failure :auto-edit)
           (:never :yolo)
           (otherwise nil))
         (%approval-policy->permission-mode (%configured-approval-policy))
         :supervised))))

(defun %shell-tool-p (tool command)
  (or command
      (member (%tool-name tool) *shell-tool-names* :test #'string=)))

(defun %mcp-tool-server-name (tool-name)
  (when (and tool-name
             (uiop:string-prefix-p "mcp/" tool-name))
    (let* ((rest (subseq tool-name (length "mcp/")))
           (separator (position #\/ rest)))
      (and separator
           (> separator 0)
           (subseq rest 0 separator)))))

(defun %normalize-mcp-permission-decision (value)
  (let ((normalized
          (cond
            ((keywordp value) value)
            ((stringp value)
             (intern (string-upcase
                      (string-trim '(#\Space #\Tab #\Newline #\Return) value))
                     :keyword))
            ((symbolp value)
             (intern (string-upcase (symbol-name value)) :keyword))
            (t nil))))
    (when (member normalized '(:allow :deny :prompt) :test #'eq)
      normalized)))

(defun %mcp-permission-key-kind (key server-name)
  (let ((normalized (%tool-name key)))
    (cond
      ((null normalized) nil)
      ((member normalized '("*" "default") :test #'string=) :default)
      ((uiop:string-prefix-p "mcp/" normalized)
       (let* ((rest (subseq normalized (length "mcp/")))
              (separator (position #\/ rest))
              (entry-server (if separator
                                (subseq rest 0 separator)
                                rest)))
         (if (string= entry-server server-name)
             :server
             nil)))
      ((string= normalized server-name) :server)
      (t nil))))

(defun %mcp-permission-config-pairs (value)
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

(defun %mcp-server-config-decision (server-name)
  (let ((pairs (%mcp-permission-config-pairs
                (ignore-errors (config-value :mcp-server-permissions
                                             (current-config)))))
        (default nil))
    (dolist (entry pairs)
      (let* ((kind (%mcp-permission-key-kind (car entry) server-name))
             (decision (%normalize-mcp-permission-decision (cdr entry))))
        (when decision
          (case kind
            (:server (return-from %mcp-server-config-decision decision))
            (:default (unless default
                        (setf default decision)))))))
    default))

(defun %mode-default-decision (mode tool path command)
  (case mode
    (:supervised :prompt)
    (:auto-edit
     (cond
       ((%shell-tool-p tool command) :prompt)
       ((or path
            (member (%tool-name tool) *auto-edit-tool-names* :test #'string=))
        :allow)
       (t :prompt)))
    (:full-auto :allow)
    (:yolo :allow)
    (otherwise :prompt)))

(defun %plan-mode-enabled-p ()
  (not (null (ignore-errors (config-value :plan-mode (current-config))))))

(defun %plan-mode-blocked-p (tool command)
  (let ((tool-name (%tool-name tool)))
    (and (%plan-mode-enabled-p)
         (or (%shell-tool-p tool command)
             (member tool-name *plan-mode-blocked-tool-names* :test #'string=)))))

(defun check-permission (&key tool path command dangerous-p permission-mode approval-policy
                           (rules *permission-rules*))
  (let* ((tool-name (%tool-name tool))
         (mode (%effective-permission-mode permission-mode approval-policy))
         (normalized-path (%normalize-path path))
         (canonical-command (canonicalize-permission-command command))
         (command-cache-key (%permission-command-cache-key tool canonical-command))
         (policy-command-text
           (let* ((policy-key (and canonical-command
                                   (command-canonical-form-policy-key canonical-command)))
                  (normalized (and canonical-command
                                   (command-canonical-form-normalized canonical-command)))
                  (separator-p (and normalized
                                    (or (search "|" normalized)
                                        (search ";" normalized)
                                        (search "&&" normalized)))))
             (cond
               (separator-p normalized)
               (policy-key policy-key)
               (normalized normalized)
               (t (%command-string command)))))
         (mcp-server-name (%mcp-tool-server-name tool-name))
         (mcp-decision (and mcp-server-name
                            (or (%mcp-server-config-decision mcp-server-name)
                                :prompt)))
         (path-decision (and normalized-path
                             (evaluate-path-permission :tool tool
                                                       :path normalized-path
                                                       :rules rules)))
         (command-decision (and policy-command-text
                                (evaluate-command-permission :tool tool
                                                             :path normalized-path
                                                             :command policy-command-text
                                                             :rules rules)))
         (decision (cond
                     ((%plan-mode-blocked-p tool policy-command-text) :deny)
                     ((or (eq path-decision :deny)
                          (eq command-decision :deny))
                      :deny)
                     ((and path (%path-memory-allows-p tool path)) :allow)
                     ((eq command-decision :allow) :allow)
                     ((eq path-decision :allow) :allow)
                     (mcp-decision mcp-decision)
                     (t (%mode-default-decision mode tool normalized-path policy-command-text)))))
    (when *last-command-canonicalization-trace*
      (setf *last-command-canonicalization-trace*
            (append *last-command-canonicalization-trace*
                    (list :command-cache-key command-cache-key))))
    (if (and (eq decision :allow)
             (not (eq mode :yolo))
             (or dangerous-p
                 (dangerous-command-p canonical-command)))
        :prompt
        decision)))
