(in-package :amoebum)

;;; ---------------------------------------------------------------------------
;;; Inter-user handoff-context shaping (NXT-378)
;;; ---------------------------------------------------------------------------

(defun %coordination-trim-string (value)
  (if (stringp value)
      (string-trim '(#\Space #\Tab #\Newline #\Return) value)
      (string-trim '(#\Space #\Tab #\Newline #\Return)
                   (princ-to-string value))))

(defun %coordination-require-string (value field-name)
  (let ((trimmed (%coordination-trim-string value)))
    (when (zerop (length trimmed))
      (error "~A must be a non-empty string." field-name))
    trimmed))

(defun %coordination-normalize-token (value)
  (let* ((trimmed (%coordination-require-string value "coordination token"))
         (text (string-downcase trimmed)))
    (coerce (loop for ch across text
                  collect (if (or (alphanumericp ch)
                                  (char= ch #\-)
                                  (char= ch #\_))
                              ch
                              #\-))
            'string)))

(defparameter +provider-secret-key-aliases+
  '(("anthropic-provider" . "ANTHROPIC_API_KEY")
    ("anthropic" . "ANTHROPIC_API_KEY")
    ("openai-compatible-provider" . "OPENAI_API_KEY")
    ("openai-compat" . "OPENAI_API_KEY")
    ("openai" . "OPENAI_API_KEY")
    ("kimi-provider" . "MOONSHOT_API_KEY")
    ("kimi" . "MOONSHOT_API_KEY")
    ("moonshot" . "MOONSHOT_API_KEY")))

(defparameter +delegation-sensitive-context-keys+
  '("provider-secrets"
    "provider-secret"
    "provider-credentials"
    "provider-credential"
    "provider-api-key"
    "api-key"
    "api_key"
    "anthropic_api_key"
    "openai_api_key"
    "moonshot_api_key"))

(defconstant +handoff-context-default-max-bytes+ 65536
  "Default serialized size ceiling for coding-task handoff context packets.")

(defconstant +handoff-context-min-max-bytes+ 1024
  "Smallest supported serialized size ceiling for handoff context packets.")

(defun %provider-secret-key-id (raw-key)
  (let* ((text (%coordination-require-string raw-key "provider secret key"))
         (down (string-downcase text))
         (alias (cdr (assoc down +provider-secret-key-aliases+ :test #'string=))))
    (or alias (string-upcase down))))

(defun %provider-secret-pairs (provider-secrets)
  (cond
    ((null provider-secrets) nil)
    ((and (listp provider-secrets)
          (every (lambda (entry)
                   (and (consp entry)
                        (not (null (car entry)))))
                 provider-secrets))
     (mapcar (lambda (entry)
               (let ((tail (cdr entry)))
                 (cons (car entry)
                       (if (and (consp tail) (null (cdr tail)))
                           (car tail)
                           tail))))
             provider-secrets))
    ((and (listp provider-secrets)
          (evenp (length provider-secrets)))
     (loop for (key value) on provider-secrets by #'cddr
           collect (cons key value)))
    (t
     (error "provider-secrets must be an alist or plist, got ~S" provider-secrets))))

(defun %coordination-plist-like-p (value)
  (and (listp value)
       (evenp (length value))
       (loop for (key _value) on value by #'cddr
             always (or (keywordp key)
                        (symbolp key)
                        (stringp key)))))

(defun %delegation-sensitive-context-key-p (key)
  (let* ((text (%coordination-trim-string key))
         (down (string-downcase text)))
    (member down +delegation-sensitive-context-keys+ :test #'string=)))

(defun %sanitize-delegation-context (context)
  (cond
    ((null context) "")
    ((hash-table-p context)
     (let ((clean (make-hash-table :test (hash-table-test context))))
       (maphash (lambda (key value)
                  (unless (%delegation-sensitive-context-key-p key)
                    (setf (gethash key clean)
                          (%sanitize-delegation-context value))))
                context)
       clean))
    ((%coordination-plist-like-p context)
     (let ((clean '()))
       (loop for (key value) on context by #'cddr do
         (unless (%delegation-sensitive-context-key-p key)
           (setf clean
                 (append clean
                         (list key (%sanitize-delegation-context value))))))
       clean))
    ((and (listp context)
          (every #'consp context))
     (let ((clean '()))
       (dolist (entry context (nreverse clean))
         (unless (%delegation-sensitive-context-key-p (car entry))
           (push (cons (car entry)
                       (%sanitize-delegation-context (cdr entry)))
                 clean)))))
    ((listp context)
     (mapcar #'%sanitize-delegation-context context))
    (t context)))

(defun %take-list-prefix (items limit)
  "Return up to LIMIT elements from ITEMS."
  (if (and (integerp limit) (>= limit 0))
      (loop for item in items
            for index from 0
            while (< index limit)
            collect item)
      (copy-list items)))

(defun %safe-git-status-snapshot (&optional project-root)
  "Return a compact git status snapshot or NIL on failure."
  (handler-case
      (let* ((status (%git-status-data :project-root project-root))
             (tracking (copy-tree (or (getf status :tracking) '())))
             (staged (copy-list (or (getf status :staged) '())))
             (unstaged (copy-list (or (getf status :unstaged) '())))
             (untracked (copy-list (or (getf status :untracked) '()))))
        (list :project-root (getf status :project-root)
              :branch (getf status :branch)
              :tracking tracking
              :staged staged
              :unstaged unstaged
              :untracked untracked))
    (error ()
      nil)))

(defun %trim-git-status-snapshot (snapshot limit)
  "Return SNAPSHOT with file lists capped to LIMIT entries each."
  (if (null snapshot)
      nil
      (list :project-root (getf snapshot :project-root)
            :branch (getf snapshot :branch)
            :tracking (copy-tree (or (getf snapshot :tracking) '()))
            :staged (%take-list-prefix (or (getf snapshot :staged) '()) limit)
            :unstaged (%take-list-prefix (or (getf snapshot :unstaged) '()) limit)
            :untracked (%take-list-prefix (or (getf snapshot :untracked) '()) limit))))

(defun %take-last-list-items (items limit)
  "Return the trailing LIMIT elements from ITEMS."
  (let* ((values (copy-list (or items '())))
         (count (length values)))
    (cond
      ((or (null limit) (>= limit count))
       values)
      ((<= limit 0)
       '())
      (t
       (nthcdr (- count limit) values)))))

(defun %conversation-handoff-snapshot (conversation &key (entry-limit 12))
  "Return a bounded conversation snapshot suitable for delegation."
  (let ((snapshot (and (typep conversation 'conversation-state)
                       (%conversation->snapshot conversation))))
    (when snapshot
      (list :session-id (getf snapshot :session-id)
            :state (getf snapshot :state)
            :created-at (getf snapshot :created-at)
            :updated-at (getf snapshot :updated-at)
            :active-fork (getf snapshot :active-fork)
            :fork-branch-point (getf snapshot :fork-branch-point)
            :forks (copy-tree (or (getf snapshot :forks) '()))
            :entry-count (length (or (getf snapshot :entries) '()))
            :entries (%take-last-list-items (or (getf snapshot :entries) '())
                                            entry-limit)))))

(defun %trim-memory-snapshot-scope (entries limit)
  "Return up to LIMIT serialized memory entries."
  (%take-list-prefix (or entries '()) limit))

(defun %memory-handoff-snapshot (backend &key (entry-limit 8))
  "Return a bounded memory snapshot suitable for delegation."
  (let ((snapshot (and backend (%memory->snapshot backend))))
    (when snapshot
      (list :backend-kind (getf snapshot :backend-kind)
            :effective-count (length (or (getf snapshot :effective) '()))
            :global-count (length (or (getf snapshot :global) '()))
            :project-count (length (or (getf snapshot :project) '()))
            :session-count (length (or (getf snapshot :session) '()))
            :effective (%trim-memory-snapshot-scope (getf snapshot :effective)
                                                    entry-limit)
            :global (%trim-memory-snapshot-scope (getf snapshot :global)
                                                 entry-limit)
            :project (%trim-memory-snapshot-scope (getf snapshot :project)
                                                  entry-limit)
            :session (%trim-memory-snapshot-scope (getf snapshot :session)
                                                  entry-limit)))))

(defun %resolve-handoff-project-root (sanitized-context)
  "Resolve the project root hinted by SANITIZED-CONTEXT, if any."
  (let ((project-root (and (listp sanitized-context)
                           (getf sanitized-context :project-root))))
    (cond
      ((pathnamep project-root)
       (uiop:ensure-directory-pathname project-root))
      ((and (stringp project-root) (plusp (length project-root)))
       (uiop:ensure-directory-pathname (pathname project-root)))
      (t
       nil))))

(defun %handoff-context-max-bytes (budget)
  "Return the serialized size ceiling implied by BUDGET."
  (let* ((explicit (or (and (listp budget) (getf budget :context-max-bytes))
                       (and (listp budget) (getf budget :max-context-bytes))))
         (token-budget (and (listp budget)
                            (getf budget :token-budget-remaining))))
    (cond
      ((and (integerp explicit) (> explicit 0))
       (max +handoff-context-min-max-bytes+
            (min explicit +handoff-context-default-max-bytes+)))
      ((and (integerp token-budget) (> token-budget 0))
       (max +handoff-context-min-max-bytes+
            (min +handoff-context-default-max-bytes+
                 (* 4 token-budget))))
      (t
       +handoff-context-default-max-bytes+))))

(defun %handoff-context-budget-mode (max-bytes)
  "Pick a detail mode for MAX-BYTES."
  (cond
    ((<= max-bytes 4096) :compact)
    ((<= max-bytes 12288) :operator)
    (t :verbose)))

(defun %current-ide-context ()
  "Return the globally attached IDE context when available."
  (let ((symbol (find-symbol "*IDE-CONTEXT*" :amoebum)))
    (when (and symbol (boundp symbol))
      (symbol-value symbol))))

(defun %handoff-ide-packet (sanitized-context max-bytes)
  "Return a bounded IDE/file context packet."
  (let* ((ctx (or (and (listp sanitized-context)
                       (getf sanitized-context :ide-context))
                  (%current-ide-context)))
         (mode (%handoff-context-budget-mode max-bytes))
         (budget (max 1 (floor max-bytes 4))))
    (when (ide-context-p ctx)
      (ide-context-build-packet ctx :mode mode :budget budget))))

(defun %extract-handoff-context-extras (sanitized-context)
  "Return stable extra keys from SANITIZED-CONTEXT not promoted into the packet."
  (cond
    ((not (listp sanitized-context))
     sanitized-context)
    (t
     (let ((extras '()))
       (loop for (key value) on sanitized-context by #'cddr do
         (unless (member key '(:conversation :memory-backend :worktree :ide-context :project-root)
                         :test #'eq)
           (setf extras (append extras (list key value)))))
       extras))))

(defun %coding-task-context-packet (sanitized-context budget)
  "Build a stable structured packet for coding-task delegation."
  (let* ((max-bytes (%handoff-context-max-bytes budget))
         (mode (%handoff-context-budget-mode max-bytes))
         (conversation (and (listp sanitized-context)
                            (getf sanitized-context :conversation)))
         (memory-backend (and (listp sanitized-context)
                              (getf sanitized-context :memory-backend)))
         (worktree (or (and (listp sanitized-context)
                            (getf sanitized-context :worktree))
                       (current-delegated-agent-worktree)))
         (project-root (%resolve-handoff-project-root sanitized-context))
         (entry-limit (ecase mode
                        (:compact 4)
                        (:operator 8)
                        (:verbose 16)))
         (memory-limit (ecase mode
                         (:compact 3)
                         (:operator 6)
                         (:verbose 10)))
         (git-limit (ecase mode
                      (:compact 3)
                      (:operator 6)
                      (:verbose 12))))
    (list :schema-version 1
          :packet-kind "coding-task-context"
          :compression-mode mode
          :max-bytes max-bytes
          :generated-at (get-universal-time)
          :conversation (%conversation-handoff-snapshot conversation
                                                    :entry-limit entry-limit)
          :files (%handoff-ide-packet sanitized-context max-bytes)
          :git (%trim-git-status-snapshot (%safe-git-status-snapshot project-root)
                                          git-limit)
          :memory (%memory-handoff-snapshot memory-backend
                                            :entry-limit memory-limit)
          :worktree (worktree-metadata-plist worktree)
          :extras (%extract-handoff-context-extras sanitized-context))))

(defun %coding-task-context-packet-size (packet)
  "Return the serialized size of PACKET in bytes."
  (length (jonathan:to-json packet)))

(defun %fit-coding-task-context-packet (packet)
  "Shrink PACKET until it fits its declared :MAX-BYTES ceiling."
  (let* ((max-bytes (or (getf packet :max-bytes)
                        +handoff-context-default-max-bytes+))
         (fitted (copy-tree packet)))
    (labels ((size-fits-p ()
               (<= (%coding-task-context-packet-size fitted) max-bytes))
             (conversation-entries ()
               (and (getf fitted :conversation)
                    (getf (getf fitted :conversation) :entries)))
             (memory-scope (scope)
               (and (getf fitted :memory)
                    (getf (getf fitted :memory) scope)))
             (halve-list (items)
               (%take-last-list-items items (max 1 (floor (length items) 2)))))
      (loop repeat 12
            until (size-fits-p)
            do (cond
                 ((and (listp (getf fitted :extras))
                       (plusp (length (getf fitted :extras))))
                  (setf (getf fitted :extras) '()))
                 ((and (listp (conversation-entries))
                       (> (length (conversation-entries)) 1))
                  (setf (getf (getf fitted :conversation) :entries)
                        (halve-list (conversation-entries))))
                 ((and (listp (memory-scope :effective))
                       (> (length (memory-scope :effective)) 1))
                  (setf (getf (getf fitted :memory) :effective)
                        (%take-list-prefix (memory-scope :effective)
                                           (max 1 (floor (length (memory-scope :effective)) 2)))))
                 ((and (listp (memory-scope :project))
                       (> (length (memory-scope :project)) 1))
                  (setf (getf (getf fitted :memory) :project)
                        (%take-list-prefix (memory-scope :project)
                                           (max 1 (floor (length (memory-scope :project)) 2)))))
                 ((and (listp (memory-scope :global))
                       (> (length (memory-scope :global)) 1))
                  (setf (getf (getf fitted :memory) :global)
                        (%take-list-prefix (memory-scope :global)
                                           (max 1 (floor (length (memory-scope :global)) 2)))))
                 ((and (listp (memory-scope :session))
                       (> (length (memory-scope :session)) 1))
                  (setf (getf (getf fitted :memory) :session)
                        (%take-list-prefix (memory-scope :session)
                                           (max 1 (floor (length (memory-scope :session)) 2)))))
                 ((and (getf fitted :files)
                       (eq (getf (getf fitted :files) :mode) :verbose))
                  (setf (getf fitted :files)
                        (let ((files (copy-tree (getf fitted :files))))
                          (setf (getf files :mode) :operator
                                (getf files :selections)
                                (%take-list-prefix (or (getf files :selections) '()) 5))
                          files)))
                 ((and (getf fitted :files)
                       (eq (getf (getf fitted :files) :mode) :operator))
                  (setf (getf fitted :files)
                        (let ((files (copy-tree (getf fitted :files))))
                          (setf (getf files :mode) :compact
                                (getf files :selections) '()
                                (getf files :diagnostics)
                                (%take-list-prefix (or (getf files :diagnostics) '()) 3))
                          files)))
                 ((getf fitted :git)
                  (setf (getf fitted :git)
                        (%trim-git-status-snapshot (getf fitted :git) 1)))
                 (t
                  (return))))
      (unless (size-fits-p) (setf (getf fitted :extras) '() (getf fitted :files) nil (getf fitted :git) nil) (when (getf fitted :conversation) (setf (getf (getf fitted :conversation) :entries) '())) (when (getf fitted :memory) (dolist (scope '(:effective :global :project :session)) (setf (getf (getf fitted :memory) scope) '()))) (unless (size-fits-p) (setf (getf fitted :conversation) nil (getf fitted :memory) nil (getf fitted :worktree) nil))) fitted)))

(defun %serialize-coding-task-context (sanitized-context budget)
  "Serialize SANITIZED-CONTEXT as a bounded structured handoff packet."
  (let* ((packet (%coding-task-context-packet sanitized-context budget))
         (fitted (%fit-coding-task-context-packet packet))
         (max-bytes (or (getf fitted :max-bytes)
                        +handoff-context-default-max-bytes+)))
    (sw4rm-sdk:serialize-handoff-context fitted :max-bytes max-bytes)))

(defun %deserialize-handoff-context-safely (snapshot)
  "Best-effort decode for a handoff context snapshot."
  (cond
    ((or (null snapshot)
         (and (stringp snapshot) (zerop (length snapshot))))
     nil)
    ((stringp snapshot)
     (ignore-errors (sw4rm-sdk:deserialize-handoff-context snapshot)))
    ((listp snapshot)
     snapshot)
    (t
     nil)))

(defun %annotate-handoff-payload-context (payload)
  "Attach parsed context metadata to PAYLOAD when present."
  (let* ((snapshot (and (listp payload) (getf payload :context-snapshot)))
         (packet (%deserialize-handoff-context-safely snapshot)))
    (append payload
            (when packet
              (list :context-packet packet))
            (when (stringp snapshot)
              (list :context-snapshot-size-bytes (length snapshot))))))
