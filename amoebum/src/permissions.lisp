(in-package :amoebum)

(defparameter *permission-rules* nil)

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
                (&key effect path tool (source :project))))
  effect
  path
  tool
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

(defun %command-string (command)
  (typecase command
    (null nil)
    (string command)
    (symbol (symbol-name command))
    (pathname (namestring command))
    (t (prin1-to-string command))))

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

(defun %normalize-path (path &key (preserve-trailing-slash-p nil))
  (normalize-permission-path path :preserve-trailing-slash-p preserve-trailing-slash-p))

(defun %contains-glob-char-p (string)
  (and string
       (loop for ch across string
             thereis (find ch "*?[]{}" :test #'char=))))

(defun %path-kind (pattern)
  (let* ((raw (%path-string pattern))
         (normalized (%normalize-path raw :preserve-trailing-slash-p t)))
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
  (let* ((source (or (%normalize-path pattern :preserve-trailing-slash-p t) ""))
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

(defun %path-under-directory-p (path directory-pattern)
  (let* ((path* (%normalize-path path))
         (dir* (%normalize-path directory-pattern :preserve-trailing-slash-p t))
         (dir-root (and dir* (%trim-trailing-slash dir*)))
         (prefix (if (string= dir-root "/")
                     "/"
                     (concatenate 'string dir-root "/"))))
    (and path*
         dir-root
         (or (string= path* dir-root)
             (uiop:string-prefix-p prefix path*)))))

(defun %path-matches-pattern-p (path pattern &key (path-normalized-p nil))
  (let ((kind (%path-kind pattern))
        (candidate (if path-normalized-p
                       path
                       (%normalize-path path))))
    (and candidate
         (case kind
           (:wildcard t)
           (:exact (string= candidate (%normalize-path pattern)))
           (:directory (%path-under-directory-p candidate pattern))
           (:glob (cl-ppcre:scan (%glob->regex pattern) candidate))
           (otherwise nil)))))

(defun %tool-matches-rule-p (tool rule-tool)
  (let ((tool-name (%tool-name tool))
        (rule-tool-name (%tool-name rule-tool)))
    (or (null rule-tool-name)
        (and tool-name (string= tool-name rule-tool-name)))))

(defun %rule-matches-p (rule tool path)
  (and (%tool-matches-rule-p tool (permission-rule-tool rule))
       (let ((rule-path (permission-rule-path rule)))
         (if rule-path
             (%path-matches-pattern-p path rule-path :path-normalized-p t)
             t))))

(defun %specificity-score (rule)
  (case (%path-kind (permission-rule-path rule))
    (:exact 300)
    (:glob 200)
    (:directory 100)
    (:wildcard 0)
    (otherwise 0)))

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

(defun add-permission-rule (&key effect path tool (source :project))
  (unless (member effect '(:allow :deny) :test #'eq)
    (error "Permission rule EFFECT must be :allow or :deny, got ~S." effect))
  (let ((rule (make-permission-rule :effect effect
                                    :path path
                                    :tool tool
                                    :source source)))
    (push rule *permission-rules*)
    rule))

(defun evaluate-path-permission (&key tool path (rules *permission-rules*))
  (let ((best nil)
        (normalized-path (%normalize-path path)))
    (when normalized-path
      (dolist (rule rules)
        (when (%rule-matches-p rule tool normalized-path)
          (when (%better-rule-p rule best)
            (setf best rule)))))
    (and best (permission-rule-effect best))))

(defun dangerous-command-p (command &optional (patterns *dangerous-command-patterns*))
  (let ((command-string (%command-string command)))
    (and command-string
         (loop for pattern in patterns
               thereis (cl-ppcre:scan pattern command-string)))))

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
         (mcp-server-name (%mcp-tool-server-name tool-name))
         (mcp-decision (and mcp-server-name
                            (or (%mcp-server-config-decision mcp-server-name)
                                :prompt)))
         (path-decision (and normalized-path
                             (evaluate-path-permission :tool tool
                                                       :path normalized-path
                                                       :rules rules)))
         (decision (cond
                     ((%plan-mode-blocked-p tool command) :deny)
                     ((eq path-decision :deny) :deny)
                     ((eq path-decision :allow) :allow)
                     (mcp-decision mcp-decision)
                     (t (%mode-default-decision mode tool normalized-path command)))))
    (if (and (eq decision :allow)
             (not (eq mode :yolo))
             (or dangerous-p
                 (dangerous-command-p command)))
        :prompt
        decision)))
