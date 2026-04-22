(in-package :amoebum)

;;;; Memory backend protocol, common helpers, and autodetection.
;;;;
;;;; Decomposed from memory.lisp by NXT-388. Owns the backend protocol
;;;; (`memory-backend-kind`, `memory-store`, `memory-query`,
;;;; `memory-list`, `memory-delete`, `memory-forget`), the
;;;; `file-memory-backend` class plus its constructor and default-path
;;;; helpers, the `memory-entry`/`memory-candidate` value types, the
;;;; `*memory-backend*` selection state, and the `%resolve-memory-backend`
;;;; autodetect chain that prefers the Haake CLI when available and
;;;; falls back to the file backend otherwise.

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

(defgeneric memory-backend-kind (backend))
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

(defmethod memory-backend-kind ((backend memory-backend))
  (declare (ignore backend))
  :unknown)

(defmethod memory-backend-kind ((backend file-memory-backend))
  (declare (ignore backend))
  :file)

(defun %call-if-fbound (symbol &rest args)
  (when (fboundp symbol)
    (apply (symbol-function symbol) args)))

(defun %configured-memory-backend (cfg)
  (let ((configured (or (and cfg (config-memory-backend cfg))
                        (and cfg (config-value :memory-backend cfg))
                        :auto)))
    (if (keywordp configured)
        configured
        (intern (string-upcase (princ-to-string configured)) :keyword))))

(defun %autodetect-haake-enabled-p (cfg)
  (let ((value (and cfg (config-value :haake-autodetect cfg))))
    (if (null value)
        t
        (not (null value)))))

(defun %make-file-backend-from-config (cfg)
  (make-file-memory-backend
   :project-root (and cfg (config-project-root cfg))))

(defun %make-haake-backend-from-config (cfg)
  (%call-if-fbound 'make-haake-cli-memory-backend
                   :command (or (and cfg (config-value :haake-command cfg))
                                "haake")
                   :project-id (and cfg (config-value :haake-project-id cfg))
                   :agent (or (and cfg (config-value :haake-agent cfg))
                              "amoebum")
                   :project-root (and cfg (config-project-root cfg))))

(defun %haake-cli-available-from-config-p (cfg)
  (if (fboundp 'haake-cli-available-p)
      (funcall (symbol-function 'haake-cli-available-p)
               :command (or (and cfg (config-value :haake-command cfg))
                            "haake"))
      nil))

(defun %haake-cli-status-ok-from-config-p (cfg)
  (if (fboundp 'haake-cli-status-ok-p)
      (funcall (symbol-function 'haake-cli-status-ok-p)
               :command (or (and cfg (config-value :haake-command cfg))
                            "haake")
               :directory (and cfg (config-project-root cfg)))
      nil))

(defun %haake-cli-compatible-from-config-p (cfg)
  (if (fboundp 'haake-cli-compatible-p)
      (funcall (symbol-function 'haake-cli-compatible-p)
               :command (or (and cfg (config-value :haake-command cfg))
                            "haake")
               :directory (and cfg (config-project-root cfg)))
      nil))

(defun %resolve-memory-backend (&optional (cfg (current-config)))
  (let ((requested (%configured-memory-backend cfg)))
    (labels ((select-file (reason)
               (values (%make-file-backend-from-config cfg) reason requested))
             (select-haake (reason unavailable-reason)
               (cond
                 ((not (%haake-cli-available-from-config-p cfg))
                  (select-file unavailable-reason))
                 ((not (%haake-cli-status-ok-from-config-p cfg))
                  (select-file :haake-status-unavailable))
                 ((not (%haake-cli-compatible-from-config-p cfg))
                  (select-file :haake-cli-incompatible))
                 (t
                  (let ((backend (%make-haake-backend-from-config cfg)))
                    (if backend
                        (values backend reason requested)
                        (select-file :haake-backend-instantiation-failed)))))))
      (case requested
        (:file
         (select-file :configured-file))
        (:haake-cli
         (select-haake :configured-haake-cli :haake-cli-unavailable))
        (:haake-mcp
         (select-file :haake-mcp-not-implemented))
        (:auto
         (if (%autodetect-haake-enabled-p cfg)
             (select-haake :auto-detected-haake-cli :haake-cli-not-found)
             (select-file :haake-autodetect-disabled)))
        (otherwise
         (select-file :unknown-memory-backend-configured))))))

(defun %publish-memory-backend-selected (backend reason requested-backend)
  (publish (current-event-bus)
           (make-memory-backend-selected-event
            :backend (memory-backend-kind backend)
            :reason reason
            :requested-backend requested-backend)))

(defun current-memory-backend ()
  (or *memory-backend*
      (multiple-value-bind (backend reason requested)
          (%resolve-memory-backend)
        (setf *memory-backend* backend)
        (%publish-memory-backend-selected backend reason requested)
        backend)))

(defun file-memory-backend-p (value)
  (typep value 'file-memory-backend))

(defun session-memory-entries ()
  (copy-list *session-memory-entries*))

(defun %event-backend-name (backend)
  (memory-backend-kind backend))

(defun %publish-memory-updated (backend operation key value)
  (publish (current-event-bus)
           (make-memory-updated-event :backend (%event-backend-name backend)
                                      :operation operation
                                      :key key
                                      :value value)))

(defun %entry-scope-signature (scope)
  (cond
    ((keywordp scope)
     (string-downcase (symbol-name scope)))
    ((and (consp scope) (eq (first scope) :topic))
     (format nil "topic/~A" (%trim-text (or (second scope) ""))))
    (t
     (%trim-text (princ-to-string scope)))))
