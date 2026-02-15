(in-package :amoebum)

(defclass memory-backend () ())

(defclass file-memory-backend (memory-backend)
  ((global-path
    :initarg :global-path
    :reader file-memory-backend-global-path)
   (project-path
    :initarg :project-path
    :reader file-memory-backend-project-path)
   (project-root
    :initarg :project-root
    :reader file-memory-backend-project-root)))

(defstruct (memory-entry
            (:constructor make-memory-entry
                (&key key value scope source
                 (created-at (get-universal-time)))))
  key
  value
  scope
  source
  created-at)

(defstruct (memory-candidate
            (:constructor make-memory-candidate
                (&key kind text key confidence)))
  kind
  text
  key
  confidence)

(defparameter *memory-backend* nil)
(defparameter *session-memory-entries* '())
(defparameter *memory-editor-runner* nil)

(defgeneric memory-store (backend key value &key scope source))
(defgeneric memory-query (backend query &key scope limit))
(defgeneric memory-list (backend &key scope))
(defgeneric memory-delete (backend key &key scope))
(defgeneric memory-forget (backend &key scope))

(defun %trim-text (text)
  (if (stringp text)
      (string-trim '(#\Space #\Tab #\Newline #\Return) text)
      ""))

(defun %string-prefix-p-ci (prefix text)
  (let ((prefix-len (length prefix))
        (text-len (length text)))
    (and (<= prefix-len text-len)
         (string-equal prefix text :end2 prefix-len))))

(defun %string-suffix-p-ci (suffix text)
  (let ((suffix-len (length suffix))
        (text-len (length text)))
    (and (<= suffix-len text-len)
         (string-equal suffix text :start2 (- text-len suffix-len)))))

(defun %collapse-whitespace (text)
  (let ((trimmed (%trim-text text)))
    (with-output-to-string (out)
      (loop with in-space = nil
            for char across trimmed do
              (if (member char '(#\Space #\Tab #\Newline #\Return) :test #'char=)
                  (unless in-space
                    (write-char #\Space out)
                    (setf in-space t))
                  (progn
                    (write-char char out)
                    (setf in-space nil)))))))

(defun %normalize-memory-key (text)
  (let* ((collapsed (%collapse-whitespace text))
         (downcased (string-downcase collapsed)))
    (if (zerop (length downcased))
        (format nil "entry-~D" (get-universal-time))
        (let ((normalized
                (with-output-to-string (out)
                  (loop with pending-separator = nil
                        for char across downcased do
                          (cond
                            ((or (alphanumericp char) (char= char #\_))
                             (when pending-separator
                               (write-char #\- out)
                               (setf pending-separator nil))
                             (write-char char out))
                            (t
                             (setf pending-separator t)))))))
          (if (zerop (length normalized))
              (format nil "entry-~D" (get-universal-time))
              normalized)))))

(defun %default-global-memory-path ()
  (merge-pathnames #P".amoebum/memory/MEMORY.md" (user-homedir-pathname)))

(defun %default-project-memory-path (&optional project-root)
  (let* ((cfg (or (ignore-errors (current-config)) nil))
         (root (or project-root
                   (and cfg (config-project-root cfg))
                   *default-pathname-defaults*)))
    (merge-pathnames #P".amoebum/MEMORY.md" (uiop:ensure-directory-pathname root))))

(defun make-file-memory-backend (&key global-path project-path project-root)
  (make-instance 'file-memory-backend
                 :global-path (or global-path (%default-global-memory-path))
                 :project-path (or project-path (%default-project-memory-path project-root))
                 :project-root (uiop:ensure-directory-pathname
                                (or project-root
                                    (make-pathname :name nil
                                                   :type nil
                                                   :defaults (or project-path
                                                                 (%default-project-memory-path)))))))

(defun reset-memory-backend (&optional backend)
  (setf *memory-backend* backend))

(defun current-memory-backend ()
  (or *memory-backend*
      (setf *memory-backend* (make-file-memory-backend))))

(defun file-memory-backend-p (value)
  (typep value 'file-memory-backend))

(defun session-memory-entries ()
  (copy-list *session-memory-entries*))

(defun %event-backend-name (backend)
  (if (typep backend 'file-memory-backend)
      :file
      :unknown))

(defun %publish-memory-updated (backend operation key value)
  (publish (current-event-bus)
           (make-memory-updated-event :backend (%event-backend-name backend)
                                      :operation operation
                                      :key key
                                      :value value)))

(defun %memory-path-for-scope (backend scope)
  (check-type backend file-memory-backend)
  (case scope
    (:global (file-memory-backend-global-path backend))
    (:project (file-memory-backend-project-path backend))
    (otherwise
     (error "Unsupported file memory scope ~S." scope))))

(defun %parse-memory-line (line scope source)
  (let ((trimmed (%trim-text line)))
    (cond
      ((or (zerop (length trimmed))
           (%string-prefix-p-ci "#" trimmed))
       nil)
      ((and (> (length trimmed) 4)
            (%string-prefix-p-ci "- [" trimmed))
       (let ((end (position #\] trimmed :start 3)))
         (when end
           (let* ((key (%trim-text (subseq trimmed 3 end)))
                  (rest (%trim-text (subseq trimmed (1+ end))))
                  (value (if (%string-prefix-p-ci ":" rest)
                             (%trim-text (subseq rest 1))
                             rest)))
             (when (plusp (length value))
               (make-memory-entry :key (if (plusp (length key))
                                           key
                                           (%normalize-memory-key value))
                                  :value value
                                  :scope scope
                                  :source source))))))
      ((and (> (length trimmed) 2)
            (%string-prefix-p-ci "- " trimmed))
       (let ((value (%trim-text (subseq trimmed 2))))
         (when (plusp (length value))
           (make-memory-entry :key (%normalize-memory-key value)
                              :value value
                              :scope scope
                              :source source))))
      (t nil))))

(defun %read-memory-file (path scope source)
  (if (and path (probe-file path))
      (with-open-file (stream path :direction :input)
        (loop for line = (read-line stream nil nil)
              while line
              for parsed = (%parse-memory-line line scope source)
              when parsed
                collect parsed))
      '()))

(defun %sort-memory-entries (entries)
  (sort (copy-list entries)
        #'string<
        :key (lambda (entry) (memory-entry-key entry))))

(defun %dedupe-memory-entries (entries)
  (let ((table (make-hash-table :test #'equal)))
    (dolist (entry entries)
      (setf (gethash (memory-entry-key entry) table) entry))
    (%sort-memory-entries
     (loop for entry being the hash-values of table collect entry))))

(defun %ensure-memory-file-header (path)
  (ensure-directories-exist path)
  (unless (probe-file path)
    (with-open-file (stream path
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (write-line "# Amoebum Memory" stream)
      (write-line "# Format: - [key] value" stream)
      (write-line "" stream))))

(defun %memory-entry-line (entry)
  (format nil "- [~A] ~A"
          (memory-entry-key entry)
          (memory-entry-value entry)))

(defun %write-memory-file (path entries)
  (%ensure-memory-file-header path)
  (with-open-file (stream path
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (write-line "# Amoebum Memory" stream)
    (write-line "# Format: - [key] value" stream)
    (write-line "" stream)
    (dolist (entry (%sort-memory-entries entries))
      (write-line (%memory-entry-line entry) stream)))
  path)

(defun %entries-by-key-table (entries)
  (let ((table (make-hash-table :test #'equal)))
    (dolist (entry entries)
      (setf (gethash (memory-entry-key entry) table) entry))
    table))

(defun %effective-memory-entries (backend)
  (check-type backend file-memory-backend)
  (let* ((global (%read-memory-file (file-memory-backend-global-path backend) :global :file))
         (project (%read-memory-file (file-memory-backend-project-path backend) :project :file))
         (table (%entries-by-key-table global)))
    (dolist (entry project)
      (setf (gethash (memory-entry-key entry) table) entry))
    (%sort-memory-entries
     (loop for entry being the hash-values of table collect entry))))

(defun %upsert-memory-entry (entries key value scope source)
  (let ((normalized-key (or (and key (plusp (length (%trim-text key))) (%trim-text key))
                            (%normalize-memory-key value)))
        (normalized-value (%collapse-whitespace value)))
    (when (zerop (length normalized-value))
      (error "Memory value must not be empty."))
    (let* ((without-key (remove normalized-key entries
                                :key #'memory-entry-key
                                :test #'equal))
           (entry (make-memory-entry :key normalized-key
                                     :value normalized-value
                                     :scope scope
                                     :source source)))
      (values entry (append without-key (list entry))))))

(defmethod memory-store ((backend file-memory-backend) key value
                         &key (scope :project) (source :manual))
  (unless (member scope '(:global :project) :test #'eq)
    (error "FILE memory backend only supports :GLOBAL and :PROJECT store scopes."))
  (let* ((path (%memory-path-for-scope backend scope))
         (entries (%read-memory-file path scope :file)))
    (multiple-value-bind (stored next-entries)
        (%upsert-memory-entry entries key value scope source)
      (%write-memory-file path (%dedupe-memory-entries next-entries))
      (push stored *session-memory-entries*)
      (%publish-memory-updated backend :store (memory-entry-key stored) (memory-entry-value stored))
      stored)))

(defmethod memory-list ((backend file-memory-backend) &key (scope :effective))
  (case scope
    (:global
     (%sort-memory-entries
      (%read-memory-file (file-memory-backend-global-path backend) :global :file)))
    (:project
     (%sort-memory-entries
      (%read-memory-file (file-memory-backend-project-path backend) :project :file)))
    (:session
     (%sort-memory-entries *session-memory-entries*))
    (:effective
     (%effective-memory-entries backend))
    (:all
     (%sort-memory-entries
      (append (%effective-memory-entries backend)
              (memory-list backend :scope :session))))
    (otherwise
     (error "Unknown memory list scope ~S." scope))))

(defmethod memory-query ((backend file-memory-backend) query
                         &key (scope :effective) (limit 25))
  (let* ((needle (string-downcase (%trim-text (or query ""))))
         (matches
           (if (zerop (length needle))
               (memory-list backend :scope scope)
               (loop for entry in (memory-list backend :scope scope)
                     for haystack-key = (string-downcase (or (memory-entry-key entry) ""))
                     for haystack-value = (string-downcase (or (memory-entry-value entry) ""))
                     when (or (search needle haystack-key :test #'char=)
                              (search needle haystack-value :test #'char=))
                       collect entry))))
    (if (and (integerp limit) (>= limit 0))
        (subseq matches 0 (min limit (length matches)))
        matches)))

(defmethod memory-delete ((backend file-memory-backend) key &key (scope :project))
  (unless (member scope '(:global :project) :test #'eq)
    (error "FILE memory backend only supports :GLOBAL and :PROJECT delete scopes."))
  (let* ((normalized-key (%normalize-memory-key key))
         (path (%memory-path-for-scope backend scope))
         (entries (%read-memory-file path scope :file))
         (next (remove normalized-key entries
                       :key #'memory-entry-key
                       :test #'equal)))
    (when (/= (length entries) (length next))
      (%write-memory-file path next)
      (setf *session-memory-entries*
            (remove normalized-key *session-memory-entries*
                    :key #'memory-entry-key
                    :test #'equal))
      (%publish-memory-updated backend :delete normalized-key nil)
      t)))

(defmethod memory-forget ((backend file-memory-backend) &key (scope :session))
  (case scope
    (:session
     (let ((count (length *session-memory-entries*)))
       (setf *session-memory-entries* '())
       (%publish-memory-updated backend :clear-session nil nil)
       count))
    (:project
     (let* ((entries (memory-list backend :scope :project))
            (count (length entries)))
       (%write-memory-file (file-memory-backend-project-path backend) '())
       (%publish-memory-updated backend :clear-project nil nil)
       count))
    (:global
     (let* ((entries (memory-list backend :scope :global))
            (count (length entries)))
       (%write-memory-file (file-memory-backend-global-path backend) '())
       (%publish-memory-updated backend :clear-global nil nil)
       count))
    (:all
     (+ (memory-forget backend :scope :session)
        (memory-forget backend :scope :project)
        (memory-forget backend :scope :global)))
    (otherwise
     (error "Unknown memory forget scope ~S." scope))))

(defun default-memory-editor-runner (editor path)
  (uiop:run-program (list editor path)
                    :input *standard-input*
                    :output *standard-output*
                    :error-output *error-output*
                    :ignore-error-status t))

(defun memory-command-show (&key (backend (current-memory-backend)))
  (let ((effective (memory-list backend :scope :effective))
        (session (memory-list backend :scope :session)))
    (with-output-to-string (out)
      (format out "Memory backend: ~A~%" (%event-backend-name backend))
      (format out "Effective entries: ~D~%" (length effective))
      (if effective
          (dolist (entry effective)
            (format out "- [~A] ~A~%"
                    (memory-entry-key entry)
                    (memory-entry-value entry)))
          (format out "(none)~%"))
      (format out "Session entries: ~D~%" (length session)))))

(defun memory-command-edit (&key (backend (current-memory-backend)) editor)
  (check-type backend file-memory-backend)
  (let* ((path (file-memory-backend-project-path backend))
         (editor-cmd (or editor
                         (uiop:getenv "AMOEBUM_EDITOR")
                         (uiop:getenv "VISUAL")
                         (uiop:getenv "EDITOR"))))
    (%ensure-memory-file-header path)
    (if (and (stringp editor-cmd) (plusp (length (%trim-text editor-cmd))))
        (progn
          (funcall (or *memory-editor-runner* #'default-memory-editor-runner)
                   editor-cmd
                   (namestring path))
          (format nil "Opened ~A using ~A." (namestring path) editor-cmd))
        (format nil "No editor configured; edit ~A manually." (namestring path)))))

(defun memory-command-clear (&key (backend (current-memory-backend)))
  (let ((cleared (memory-forget backend :scope :session)))
    (format nil "Cleared ~D session memor~:@P." cleared)))

(defun %command-tokens (text)
  (let* ((trimmed (%trim-text text))
         (len (length trimmed))
         (tokens '())
         (start 0))
    (labels ((separatorp (char)
               (member char '(#\Space #\Tab #\Newline #\Return) :test #'char=)))
      (loop for index from 0 to len do
        (if (= index len)
            (when (< start index)
              (push (subseq trimmed start index) tokens))
            (when (separatorp (char trimmed index))
              (when (< start index)
                (push (subseq trimmed start index) tokens))
              (setf start (1+ index)))))
      (nreverse tokens))))

(defun memory-command-input-p (text)
  (let* ((trimmed (%trim-text text))
         (len (length trimmed)))
    (and (>= len 7)
         (string-equal "/memory" trimmed :end2 7)
         (or (= len 7)
             (member (char trimmed 7)
                     '(#\Space #\Tab #\Newline #\Return)
                     :test #'char=)))))

(defun run-memory-command (text &key (backend (current-memory-backend)) editor)
  (let ((trimmed (%trim-text text)))
    (unless (memory-command-input-p trimmed)
      (return-from run-memory-command (values nil nil)))
    (let* ((suffix (%trim-text (subseq trimmed 7)))
           (tokens (%command-tokens suffix))
           (subcommand (if tokens
                           (string-downcase (first tokens))
                           "show"))
           (tail (if tokens
                     (%trim-text (subseq suffix (min (length suffix)
                                                     (length (first tokens)))))
                     "")))
      (case (intern (string-upcase subcommand) :keyword)
        (:SHOW
         (values t (memory-command-show :backend backend)))
        (:EDIT
         (values t (memory-command-edit :backend backend :editor editor)))
        (:CLEAR
         (values t (memory-command-clear :backend backend)))
        (:REMEMBER
         (if (zerop (length tail))
             (values t "Usage: /memory remember <statement>")
             (let ((entry (memory-store backend nil tail :scope :project :source :memory-command)))
               (values t
                       (format nil "Remembered [~A] ~A"
                               (memory-entry-key entry)
                               (memory-entry-value entry))))))
        (:FORGET
         (if (zerop (length tail))
             (values t "Usage: /memory forget <statement-or-key>")
             (if (memory-delete backend tail :scope :project)
                 (values t (format nil "Forgot ~A." (%normalize-memory-key tail)))
                 (values t (format nil "No memory entry matched ~A." (%normalize-memory-key tail))))))
        (otherwise
         (values t
                 (format nil "Unknown /memory subcommand ~A. Use show|edit|clear." subcommand)))))))

(defun %extract-after-prefix (text prefix)
  (if (%string-prefix-p-ci prefix text)
      (%trim-text (subseq text (length prefix)))
      nil))

(defun %extract-after-search (text token)
  (let ((pos (search token text :test #'char-equal)))
    (when pos
      (%trim-text (subseq text (+ pos (length token)))))))

(defun extract-durable-memory-candidate (text)
  (let* ((trimmed (%trim-text text))
         (remember-body (or (%extract-after-prefix trimmed "remember that ")
                            (%extract-after-prefix trimmed "remember ")
                            (%extract-after-prefix trimmed "please remember that ")
                            (%extract-after-prefix trimmed "please remember ")))
         (forget-body (or (%extract-after-prefix trimmed "forget ")
                          (%extract-after-prefix trimmed "please forget ")
                          (%extract-after-prefix trimmed "forget the ")))
         (preference-body (or (%extract-after-search trimmed "i always ")
                              (%extract-after-search trimmed "i prefer ")
                              (%extract-after-search trimmed "please always "))))
    (cond
      ((and remember-body (plusp (length remember-body)))
       (make-memory-candidate :kind :remember
                              :text remember-body
                              :key (%normalize-memory-key remember-body)
                              :confidence 0.95d0))
      ((and forget-body (plusp (length forget-body)))
       (let* ((normalized-forget
                (if (%string-suffix-p-ci " preference" forget-body)
                    (%trim-text (subseq forget-body
                                        0
                                        (- (length forget-body)
                                           (length " preference"))))
                    forget-body)))
         (make-memory-candidate :kind :forget
                                :text normalized-forget
                                :key (%normalize-memory-key normalized-forget)
                                :confidence 0.90d0)))
      ((and preference-body (plusp (length preference-body)))
       (make-memory-candidate :kind :preference
                              :text preference-body
                              :key (%normalize-memory-key preference-body)
                              :confidence 0.70d0))
      (t nil))))

(defun apply-memory-candidate (candidate &key (backend (current-memory-backend)))
  (when (memory-candidate-p candidate)
    (case (memory-candidate-kind candidate)
      (:remember
       (let ((entry (memory-store backend
                                  (memory-candidate-key candidate)
                                  (memory-candidate-text candidate)
                                  :scope :project
                                  :source :extracted)))
         (values :stored entry)))
      (:forget
       (if (memory-delete backend (memory-candidate-key candidate) :scope :project)
           (values :deleted (memory-candidate-key candidate))
           (values :not-found (memory-candidate-key candidate))))
      (:preference
       (values :candidate candidate))
      (otherwise
       (values :ignored candidate)))))
