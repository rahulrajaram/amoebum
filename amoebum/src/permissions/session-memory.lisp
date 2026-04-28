(in-package :amoebum)

;;; Path-approval session memory.
;;;
;;; Extracted mechanically from src/permissions.lisp for NXT-440. Owns the
;;; in-memory list of approved (tool, path, scope) entries, the persistence
;;; layer for :always-scope approvals, and the lookup helper consulted by
;;; the evaluation pipeline (%path-memory-allows-p). Allow/deny semantics
;;; for matching, scope decay, and persistence are preserved exactly.

(defparameter *path-approval-memory* '())
(defparameter *path-approval-memory-limit* 256)
(defparameter *path-approval-persistence-relative-path* #P".amoebum/permissions.lisp")
(defvar *path-approval-memory-loaded-p* nil)

(defstruct (path-approval-entry
            (:constructor make-path-approval-entry
                (&key tool path scope created-at uses-remaining)))
  tool
  path
  scope
  created-at
  uses-remaining)

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
