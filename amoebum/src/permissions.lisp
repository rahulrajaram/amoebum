(in-package :amoebum)

(defparameter *permission-rules* nil)
(defparameter *permission-rules-version* 0)
(defparameter *permission-evaluation-cache* (make-hash-table :test #'equal))
(defparameter *permission-cache-hits* 0)
(defparameter *permission-cache-misses* 0)
(defparameter *permission-cache-invalidations* 0)
(defparameter *permission-cache-invalidation-events* '())
(defparameter *permission-cache-invalidation-events-limit* 128)
(defparameter *permission-decision-history* '())
(defparameter *permission-decision-history-limit* 256)
(defparameter *permission-decision-sequence* 0)
(defparameter *last-permission-decision-trace* nil)
(defparameter *path-approval-memory* '())
(defparameter *path-approval-memory-limit* 256)
(defparameter *path-approval-persistence-relative-path* #P".amoebum/permissions.lisp")
(defvar *path-approval-memory-loaded-p* nil)
(defparameter *permission-path-case-sensitive-p*
  (not (or (member :windows *features*)
           (member :win32 *features*)
           (member :mswindows *features*))))
(defparameter *permission-path-unicode-normalization-form* :nfc)
(defparameter *permission-path-identity-check-cache* (make-hash-table :test #'equal))
(defparameter *permission-path-identity-check-cache-limit* 512)
(defparameter *permission-path-identity-recheck-hook* nil)

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

(defparameter *plan-mode-readonly-allowed-tool-names*
  '("read-file" "glob-files" "grep-content" "search-project"))

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

(defstruct (path-approval-entry
            (:constructor make-path-approval-entry
                (&key tool path scope created-at uses-remaining)))
  tool
  path
  scope
  created-at
  uses-remaining)

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

(defun %permission-path-parent-directory (path)
  (let ((text (%path-string path)))
    (when (and text (> (length text) 0))
      (let* ((pathname (pathname text))
             (directory (make-pathname :name nil :type nil :defaults pathname))
             (directory-text (%path-string directory)))
        (when (and directory-text (> (length directory-text) 0))
          (%normalize-path directory-text))))))

(defun %optional-package (designator)
  (or (find-package designator)
      (ignore-errors (require designator))
      (find-package designator)))

(defun %unicode-normalize-path (path)
  (let ((text (%path-string path)))
    (if (or (null text)
            (null *permission-path-unicode-normalization-form*))
        text
        (let* ((package (%optional-package :sb-unicode))
               (normalize-symbol (and package
                                      (find-symbol "NORMALIZE-STRING" package)))
               (normalize-fn (and normalize-symbol
                                  (fboundp normalize-symbol)
                                  (symbol-function normalize-symbol))))
          (if normalize-fn
              (or (ignore-errors
                    (funcall normalize-fn
                             text
                             *permission-path-unicode-normalization-form*))
                  text)
              text)))))

(defun %fold-path-identity-text (path)
  (let ((normalized (%unicode-normalize-path path)))
    (if (and normalized (not *permission-path-case-sensitive-p*))
        (string-downcase normalized)
        normalized)))

(defun %sb-posix-function (name)
  (let* ((package (%optional-package :sb-posix))
         (symbol (and package (find-symbol name package))))
    (and symbol
         (fboundp symbol)
         (symbol-function symbol))))

(defun %path-inode-signature (path &key (follow-symlinks-p t))
  (let* ((candidate (%normalize-request-path path))
         (stat-fn (%sb-posix-function (if follow-symlinks-p
                                          "STAT"
                                          "LSTAT")))
         (dev-fn (%sb-posix-function "STAT-DEV"))
         (ino-fn (%sb-posix-function "STAT-INO")))
    (when (and candidate stat-fn dev-fn ino-fn)
      (let ((stat (ignore-errors (funcall stat-fn candidate))))
        (when stat
          (let ((dev (ignore-errors (funcall dev-fn stat)))
                (ino (ignore-errors (funcall ino-fn stat))))
            (when (and dev ino)
              (list :dev dev :ino ino))))))))

(defun %capture-path-identity (path)
  (let* ((request-path (%normalize-request-path path))
         (target-path (%normalize-path path))
         (effective-path (or target-path request-path))
         (parent-path (%permission-path-parent-directory effective-path)))
    (when effective-path
      (list :request-path request-path
            :target-path effective-path
            :target-folded (%fold-path-identity-text effective-path)
            :target-inode (%path-inode-signature effective-path)
            :parent-path parent-path
            :parent-folded (and parent-path
                                (%fold-path-identity-text parent-path))
            :parent-inode (and parent-path
                               (%path-inode-signature parent-path))
            :captured-at (get-universal-time)))))

(defun %identity-field-matches-p (left right &key (stringp nil))
  (or (null left)
      (null right)
      (if stringp
          (string= left right)
          (equal left right))))

(defun %path-identity-records-compatible-p (expected observed)
  (and expected
       observed
       (%identity-field-matches-p (getf expected :target-folded)
                                  (getf observed :target-folded)
                                  :stringp t)
       (%identity-field-matches-p (getf expected :target-inode)
                                  (getf observed :target-inode))
       (%identity-field-matches-p (getf expected :parent-folded)
                                  (getf observed :parent-folded)
                                  :stringp t)
       (%identity-field-matches-p (getf expected :parent-inode)
                                  (getf observed :parent-inode))))

(defun %permission-path-identity-cache-key (tool path)
  (let ((tool-name (%tool-name tool))
        (request-path (%normalize-request-path path)))
    (when (and tool-name request-path)
      (list :tool tool-name :request-path request-path))))

(defun %trim-permission-path-identity-cache ()
  (when (> (hash-table-count *permission-path-identity-check-cache*)
           *permission-path-identity-check-cache-limit*)
    (clrhash *permission-path-identity-check-cache*)))

(defun %record-permission-path-identity-check (tool path decision decision-id)
  (let* ((cache-key (%permission-path-identity-cache-key tool path))
         (snapshot (%capture-path-identity path)))
    (when (and cache-key snapshot)
      (setf (gethash cache-key *permission-path-identity-check-cache*)
            (list :decision decision
                  :decision-id decision-id
                  :identity snapshot))
      (%trim-permission-path-identity-cache))
    snapshot))

(defun %path-identity-equal-p (left right)
  (let ((left-text (%path-string left))
        (right-text (%path-string right)))
    (and left-text
         right-text
         (or (string= left-text right-text)
             (let ((left-folded (%fold-path-identity-text left-text))
                   (right-folded (%fold-path-identity-text right-text)))
               (and left-folded
                    right-folded
                    (string= left-folded right-folded)))
             (let ((left-inode (%path-inode-signature left-text))
                   (right-inode (%path-inode-signature right-text)))
               (and left-inode
                    right-inode
                    (equal left-inode right-inode)))))))

(defun %assert-path-identity-stable-at-use-time (&key tool path)
  (let* ((cache-key (%permission-path-identity-cache-key tool path))
         (cached (and cache-key
                      (gethash cache-key *permission-path-identity-check-cache*)))
         (expected (and (listp cached) (getf cached :identity))))
    (when (and expected (functionp *permission-path-identity-recheck-hook*))
      (funcall *permission-path-identity-recheck-hook*
               :tool (%tool-name tool)
               :request-path (%normalize-request-path path)
               :expected expected))
    (let ((observed (and expected (%capture-path-identity path))))
      (when (and expected
                 (not (%path-identity-records-compatible-p expected observed)))
        (error 'tool-permission-denied
               :tool-name (%tool-name tool)
               :arguments nil
               :reason-code :path-identity-changed
               :message (format nil "Path identity changed for ~A before use-time recheck."
                                (%path-string path))
               :reason "canonical path identity changed before file use")))))

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

(defun %argument-pattern-components (pattern)
  (let ((normalized (%normalize-permission-command pattern)))
    (when normalized
      (flet ((consume (prefix selector)
               (values selector
                       (%trim-command-whitespace
                        (subseq normalized (length prefix))))))
        (cond
          ((%string-prefix-ci-p "program:" normalized)
           (consume "program:" :program))
          ((%string-prefix-ci-p "prog:" normalized)
           (consume "prog:" :program))
          ((%string-prefix-ci-p "flag:" normalized)
           (consume "flag:" :flag))
          ((%string-prefix-ci-p "flags:" normalized)
           (consume "flags:" :flag))
          ((%string-prefix-ci-p "option:" normalized)
           (consume "option:" :flag))
          ((%string-prefix-ci-p "options:" normalized)
           (consume "options:" :flag))
          ((%string-prefix-ci-p "positional:" normalized)
           (consume "positional:" :positional))
          ((%string-prefix-ci-p "position:" normalized)
           (consume "position:" :positional))
          ((%string-prefix-ci-p "pos:" normalized)
           (consume "pos:" :positional))
          ((%string-prefix-ci-p "arg:" normalized)
           (consume "arg:" :argument))
          ((%string-prefix-ci-p "args:" normalized)
           (consume "args:" :argument))
          ((%string-prefix-ci-p "token:" normalized)
           (consume "token:" :token))
          ((%string-prefix-ci-p "argv:" normalized)
           (consume "argv:" :token))
          (t
           (values :argument normalized)))))))

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
                     (concatenate 'string dir* "/")))
         (path-folded (%fold-path-identity-text path*))
         (dir-folded (%fold-path-identity-text dir*))
         (prefix-folded (and dir-folded
                             (if (string= dir-folded "/")
                                 "/"
                                 (concatenate 'string dir-folded "/")))))
    (and path*
         dir*
         (or (%path-identity-equal-p path* dir*)
             (uiop:string-prefix-p prefix path*)
             (and path-folded
                  prefix-folded
                  (uiop:string-prefix-p prefix-folded path-folded))))))

(defun %path-matches-pattern-p (path pattern &key (path-normalized-p nil))
  (let ((kind (%path-kind pattern))
        (candidate (if path-normalized-p
                       path
                       (%normalize-path path))))
    (and candidate
         (case kind
           (:wildcard t)
           (:exact (%path-identity-equal-p candidate
                                           (%normalize-pattern-path pattern)))
           (:directory (%path-under-directory-p candidate pattern))
           (:glob (cl-ppcre:scan (%glob->regex pattern) candidate))
           (otherwise nil)))))

(defun %command-matches-pattern-p (command pattern &key (command-normalized-p nil))
  (labels ((%pipeline-segment-candidates (value)
             (let ((canonical (canonicalize-permission-command value)))
               (when canonical
                 (remove-duplicates
                  (loop for segment in (command-canonical-form-commands canonical)
                        for text = (%canonical-argv-string segment)
                        when (and (stringp text)
                                  (> (length text) 0))
                          collect text)
                  :test #'string=))))
           (%matches-single-candidate-p (candidate normalized-pattern kind)
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
               (otherwise nil))))
    (let* ((candidate (if command-normalized-p
                          command
                          (%normalize-permission-command command)))
           (normalized-pattern (%normalize-permission-command pattern))
           (kind (%command-pattern-kind normalized-pattern))
           (candidates (and candidate
                            (remove-duplicates
                             (cons candidate
                                   (%pipeline-segment-candidates candidate))
                             :test #'string=))))
      (and candidates
           (loop for candidate-text in candidates
                 thereis (%matches-single-candidate-p candidate-text
                                                     normalized-pattern
                                                     kind))))))

(defun %tool-matches-rule-p (tool rule-tool)
  (let ((tool-name (%tool-name tool))
        (rule-tool-name (%tool-name rule-tool)))
    (or (null rule-tool-name)
        (and tool-name (string= tool-name rule-tool-name)))))

(defun %rule-matches-p (rule tool path command canonical-command)
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
             t))
       (%rule-arguments-match-p rule canonical-command)))

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

(defun %argument-pattern-specificity-score (argument-pattern)
  (multiple-value-bind (_selector matcher)
      (%argument-pattern-components argument-pattern)
    (declare (ignore _selector))
    (case (%command-pattern-kind matcher)
      (:exact 70)
      (:prefix 50)
      (:glob 40)
      (:regex 30)
      (:wildcard 10)
      (otherwise 0))))

(defun %argument-specificity-score (rule)
  (let ((arguments (permission-rule-arguments rule)))
    (if arguments
        (+ 25
           (loop for argument-pattern in arguments
                 sum (%argument-pattern-specificity-score argument-pattern)))
        0)))

(defun %specificity-score (rule)
  (+ (%path-specificity-score rule)
     (%command-specificity-score rule)
     (%argument-specificity-score rule)))

(defun %scope-score (rule)
  (case (permission-rule-source rule)
    (:session 30)
    (:extension 20)
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

(defun %permission-rule-id (rule)
  (or (permission-rule-id rule)
      (setf (permission-rule-id rule)
            (format nil "~A-~8,'0X"
                    (string-downcase
                     (symbol-name (or (permission-rule-source rule) :unknown)))
                    (ldb (byte 32 0)
                         (sxhash
                          (list (permission-rule-effect rule)
                                (permission-rule-path rule)
                                (permission-rule-command rule)
                                (permission-rule-tool rule)
                                (permission-rule-arguments rule)
                                (permission-rule-source rule))))))))

(defun %permission-cache-note-invalidation (reason)
  (incf *permission-cache-invalidations*)
  (push (list :timestamp (get-universal-time)
              :reason reason
              :rules-version *permission-rules-version*)
        *permission-cache-invalidation-events*)
  (when (> (length *permission-cache-invalidation-events*)
           *permission-cache-invalidation-events-limit*)
    (setf *permission-cache-invalidation-events*
          (subseq *permission-cache-invalidation-events*
                  0
                  *permission-cache-invalidation-events-limit*))))

(defun clear-permission-cache (&key (reason :manual))
  (clrhash *permission-evaluation-cache*)
  (%permission-cache-note-invalidation reason)
  t)

(defun permission-cache-metrics ()
  (list :hits *permission-cache-hits*
        :misses *permission-cache-misses*
        :invalidations *permission-cache-invalidations*
        :rules-version *permission-rules-version*
        :entries (hash-table-count *permission-evaluation-cache*)))

(defun permission-cache-invalidation-events (&key (limit 20))
  (subseq *permission-cache-invalidation-events*
          0
          (min (max 0 limit)
               (length *permission-cache-invalidation-events*))))

(defun clear-permission-rules ()
  (setf *permission-rules* nil)
  (incf *permission-rules-version*)
  (clear-permission-cache :reason :rules-cleared))

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

(defun add-permission-rule (&key effect path command tool arguments (source :project))
  (unless (member effect '(:allow :deny) :test #'eq)
    (error "Permission rule EFFECT must be :allow or :deny, got ~S." effect))
  (let ((rule (make-permission-rule :effect effect
                                    :path path
                                    :command (%validate-command-pattern command)
                                    :tool tool
                                    :arguments (%normalize-rule-arguments arguments)
                                    :source source)))
    (%permission-rule-id rule)
    (push rule *permission-rules*)
    (incf *permission-rules-version*)
    (clear-permission-cache :reason :rule-added)
    rule))

(defun %effective-permission-rules (rules)
  (cond
    ((policy-rule-registry-p rules)
     (policy-rule-registry-composed-rules rules))
    ((policy-rule-table-p rules)
     (copy-list (policy-rule-table-rules rules)))
    (t rules)))

(defun %permission-cache-key (phase tool path command rules)
  (when (eq rules *permission-rules*)
    (list :phase phase
          :tool (%tool-name tool)
          :path path
          :command command
          :rules-version *permission-rules-version*)))

(defun %rule-trace-entry (phase rule)
  (when rule
    (list :phase phase
          :matched-rule-id (%permission-rule-id rule)
          :specificity (%specificity-score rule)
          :effect (permission-rule-effect rule)
          :source (permission-rule-source rule)
          :tool (%tool-name (permission-rule-tool rule))
          :path (permission-rule-path rule)
          :command (permission-rule-command rule)
          :arguments (permission-rule-arguments rule))))

(defun %evaluate-rule-phase (phase tool path command rules &key canonical-command)
  (let* ((effective-rules (%effective-permission-rules rules))
         (cache-key (%permission-cache-key phase tool path command rules))
         (cached (and cache-key
                      (multiple-value-list
                       (gethash cache-key *permission-evaluation-cache*)))))
    (when (and cached (second cached))
      (incf *permission-cache-hits*)
      (let* ((entry (first cached))
             (rule (getf entry :rule))
             (trace (append (getf entry :trace)
                            (list :cache :hit))))
        (return-from %evaluate-rule-phase
          (values (and rule (permission-rule-effect rule))
                  trace
                  rule
                  :hit))))
    (incf *permission-cache-misses*)
    (let ((best nil))
      (when (or path command)
        (dolist (rule effective-rules)
          (when (%rule-matches-p rule tool path command canonical-command)
            (when (%better-rule-p rule best)
              (setf best rule)))))
      (let ((trace (append (%rule-trace-entry phase best)
                           (list :cache :miss))))
        (when cache-key
          (setf (gethash cache-key *permission-evaluation-cache*)
                (list :rule best :trace (%rule-trace-entry phase best))))
        (values (and best (permission-rule-effect best))
                trace
                best
                :miss)))))

(defun evaluate-path-permission (&key tool path (rules *permission-rules*) (with-trace-p nil))
  (let ((normalized-path (%normalize-path path)))
    (multiple-value-bind (decision trace)
        (%evaluate-rule-phase :path tool normalized-path nil rules)
      (if with-trace-p
          (values decision trace)
          decision))))

(defun evaluate-command-permission (&key tool command path (rules *permission-rules*)
                                      canonical-command
                                      (with-trace-p nil))
  (let* ((canonical (or canonical-command
                        (and command
                             (canonicalize-permission-command command))))
         (policy-command (%policy-command-text canonical command))
         (normalized-command (%normalize-permission-command policy-command))
        (normalized-path (and path (%normalize-path path))))
    (multiple-value-bind (decision trace)
        (%evaluate-rule-phase :command
                              tool
                              normalized-path
                              normalized-command
                              rules
                              :canonical-command canonical)
      (if with-trace-p
          (values decision trace)
          decision))))

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
  (cfg :approval-policy))

(defun %effective-permission-mode (mode &optional approval-policy)
  (or
   (case mode
     ((:supervised :auto-edit :full-auto :yolo :plan) mode)
     (:no-confirm :yolo)
     (:untrusted :supervised)
     (:on-request :supervised)
     (:on-failure :auto-edit)
     (:never :yolo)
     (otherwise nil))
   (%approval-policy->permission-mode approval-policy)
   (let ((cfg-mode (ignore-errors (config-permission-mode (current-config)))))
     (or (case cfg-mode
           ((:supervised :auto-edit :full-auto :yolo :plan) cfg-mode)
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
                (cfg :mcp-server-permissions)))
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
    (:plan :prompt)
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
  (not (null (cfg :plan-mode))))

(defun plan-mode-mutating-tools-blocked-p (&optional
                                             (config (ignore-errors (current-config)))
                                             plan-mode-enabled-override)
  (let ((plan-mode-enabled-p
          (if (null plan-mode-enabled-override)
              (and (config-p config)
                   (not (null (config-value :plan-mode config))))
              (not (null plan-mode-enabled-override)))))
    (and plan-mode-enabled-p
         (not (null *plan-mode-blocked-tool-names*))
         (not (null *shell-tool-names*)))))

(defun %plan-mode-blocked-p (tool command &optional plan-mode-enabled-override)
  (let ((tool-name (%tool-name tool)))
    (and (plan-mode-mutating-tools-blocked-p (ignore-errors (current-config))
                                             plan-mode-enabled-override)
         (or (%shell-tool-p tool command)
             (member tool-name *plan-mode-blocked-tool-names* :test #'string=)))))

(defun %plan-mode-readonly-allowed-p (tool)
  (let ((tool-name (%tool-name tool)))
    (and (%plan-mode-enabled-p)
         (member tool-name
                 *plan-mode-readonly-allowed-tool-names*
                 :test #'string=))))

(defun %plan-mode-actionable-reason ()
  "Plan mode is read-only. Review the captured plan, approve allowed steps with /plan approve, then run /execute to re-enable mutating tools.")

(defun %plan-mode-block-reason (tool-name command)
  (let ((normalized-tool (or tool-name "unknown-tool")))
    (if (and (stringp command) (> (length command) 0))
        (format nil "Plan mode blocked mutating tool ~A for command ~S."
                normalized-tool
                command)
        (format nil "Plan mode blocked mutating tool ~A."
                normalized-tool))))

(defun clear-permission-decision-history ()
  (setf *permission-decision-history* '()
        *permission-decision-sequence* 0
        *last-permission-decision-trace* nil)
  t)

(defun permission-decision-history (&key (limit 20))
  (subseq *permission-decision-history*
          0
          (min (max 0 limit) (length *permission-decision-history*))))

(defun %next-permission-decision-id ()
  (incf *permission-decision-sequence*)
  (format nil "perm-~D" *permission-decision-sequence*))

(defun %record-permission-decision (trace)
  (setf *last-permission-decision-trace* trace
        *permission-decision-history* (cons trace *permission-decision-history*))
  (when (> (length *permission-decision-history*) *permission-decision-history-limit*)
    (setf *permission-decision-history*
          (subseq *permission-decision-history* 0 *permission-decision-history-limit*)))
  trace)

(defun last-permission-decision-trace ()
  *last-permission-decision-trace*)
