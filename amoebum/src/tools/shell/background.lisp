(in-package :amoebum)

;;;; ---------------------------------------------------------------------------
;;;; Shell tool: background-task ownership.
;;;;
;;;; Tracks asynchronous shell invocations spawned via the bash-exec tool so
;;;; that the `/background` and `/jobs` slash commands can list, snapshot,
;;;; fetch, and clean up running tasks.  This module owns:
;;;;   * the `shell-task` defstruct + thread-safe registry
;;;;   * task lifecycle helpers (id generation, snapshot, status query)
;;;;   * the worker thread that runs `%run-shell-command` in the background
;;;;     and mutates the task entry on completion / failure
;;;;
;;;; All function names, signatures, locking, and lifecycle ordering match
;;;; the original `amoebum/src/tools/shell.lisp` verbatim.
;;;; ---------------------------------------------------------------------------

(defstruct (shell-task
            (:constructor make-shell-task
                (&key id command cwd timeout-seconds max-output-chars
                 max-output-bytes max-output-lines
                 status started-at)))
  id
  command
  cwd
  timeout-seconds
  max-output-chars
  max-output-bytes
  max-output-lines
  status
  started-at
  finished-at
  result)

(defparameter *shell-task-table* (make-hash-table :test #'equal))
#+sb-thread
(defparameter *shell-task-lock*
  (sb-thread:make-mutex :name "amoebum-shell-task-lock"))

(defmacro %with-shell-task-lock (&body body)
  #+sb-thread
  `(sb-thread:with-mutex (*shell-task-lock*)
     ,@body)
  #-sb-thread
  `(progn ,@body))

(defun %random-base36-string (length)
  (let ((alphabet "0123456789abcdefghijklmnopqrstuvwxyz"))
    (coerce
     (loop repeat length
           collect (char alphabet (random (length alphabet))))
     'string)))

(defun %make-shell-task-id ()
  (format nil "shell-task-~D-~A"
          (get-universal-time)
          (%random-base36-string 8)))

(defun %store-shell-task (task)
  (%with-shell-task-lock
    (setf (gethash (shell-task-id task) *shell-task-table*) task))
  task)

(defun %find-shell-task (task-id)
  (%with-shell-task-lock
    (gethash task-id *shell-task-table*)))

(defun %shell-task-terminal-status-p (status)
  (member status '(:completed :timeout :failed :cancelled) :test #'eq))

(defun %snapshot-shell-task-unlocked (task)
  (let ((result (shell-task-result task)))
    (list :task-id (shell-task-id task)
          :status (shell-task-status task)
          :command (shell-task-command task)
          :cwd (coerce-path-string (shell-task-cwd task))
          :timeout-seconds (shell-task-timeout-seconds task)
          :max-output-chars (shell-task-max-output-chars task)
          :max-output-bytes (shell-task-max-output-bytes task)
          :max-output-lines (shell-task-max-output-lines task)
          :started-at (shell-task-started-at task)
          :finished-at (shell-task-finished-at task)
          :result result
          :stdout (and result (getf result :stdout))
          :stderr (and result (getf result :stderr))
          :exit-code (and result (getf result :exit-code))
          :timed-out (and result (getf result :timed-out)))))

(defun %snapshot-shell-task (task)
  (%with-shell-task-lock
    (%snapshot-shell-task-unlocked task)))

(defun %list-shell-tasks (&key (include-finished t))
  (%with-shell-task-lock
    (let ((snapshots '()))
      (maphash (lambda (_ task)
                 (declare (ignore _))
                 (when (or include-finished
                           (not (%shell-task-terminal-status-p
                                 (shell-task-status task))))
                   (push (%snapshot-shell-task-unlocked task) snapshots)))
               *shell-task-table*)
      (setf snapshots
            (sort snapshots #'< :key (lambda (snapshot)
                                       (or (getf snapshot :started-at) 0))))
      (list :count (length snapshots)
            :tasks snapshots))))

(defun %cleanup-shell-tasks (&key (include-running nil))
  (%with-shell-task-lock
    (let ((remove-ids '()))
      (maphash (lambda (task-id task)
                 (when (or include-running
                           (%shell-task-terminal-status-p
                            (shell-task-status task)))
                   (push task-id remove-ids)))
               *shell-task-table*)
      (dolist (task-id remove-ids)
        (remhash task-id *shell-task-table*))
      (let ((sorted-ids (sort remove-ids #'string<)))
        (list :removed-count (length sorted-ids)
              :removed-task-ids sorted-ids
              :remaining-count (hash-table-count *shell-task-table*))))))

(defun %task-error-result (command cwd condition)
  (list :status :failed
        :command command
        :cwd (coerce-path-string cwd)
        :stdout ""
        :stderr (princ-to-string condition)
        :exit-code 1
        :timed-out nil
        :runaway-output-p nil
        :runaway-output-reason nil
        :output-bytes 0
        :output-lines 0
        :output-byte-limit nil
        :output-line-limit nil
        :stdout-truncated-p nil
        :stderr-truncated-p nil
        :stdout-omitted-chars 0
        :stderr-omitted-chars 0))

(defun %task-result-status (result)
  (let ((status (getf result :status)))
    (case status
      (:completed :completed)
      (:timeout :timeout)
      (:failed :failed)
      (otherwise :failed))))

(defun %start-background-shell-task (command cwd timeout-seconds
                                   max-output-chars max-output-bytes max-output-lines
                                   &key shell-executable profile-files env-vars)
  #-sb-thread
  (error "Background shell execution requires SBCL thread support.")
  #+sb-thread
  (let* ((task-id (%make-shell-task-id))
         (task (make-shell-task
                :id task-id
                :command command
                :cwd cwd
                :timeout-seconds timeout-seconds
                :max-output-chars max-output-chars
                :max-output-bytes max-output-bytes
                :max-output-lines max-output-lines
                :status :running
                :started-at (get-universal-time))))
    (%store-shell-task task)
    (sb-thread:make-thread
     (lambda ()
       (let ((result (handler-case
                         (%run-shell-command command
                                             cwd
                                             timeout-seconds
                                             max-output-chars
                                             :max-output-bytes max-output-bytes
                                             :max-output-lines max-output-lines
                                             :shell-executable shell-executable
                                             :profile-files profile-files
                                             :env-vars env-vars)
                       (error (condition)
                         (%task-error-result command cwd condition)))))
         (%with-shell-task-lock
           (setf (shell-task-result task) result
                 (shell-task-status task)
                 (%task-result-status result)
                 (shell-task-finished-at task) (get-universal-time)))))
     :name (format nil "amoebum-shell-task-~A" task-id))
    (%snapshot-shell-task task)))

(defun %fetch-shell-task (task-id)
  (let ((normalized-task-id (%trim-whitespace task-id)))
    (when (zerop (length normalized-task-id))
      (error "TASK-ID must not be empty."))
    (let ((task (%find-shell-task normalized-task-id)))
      (unless task
        (error "Unknown bash-exec TASK-ID: ~S." normalized-task-id))
      (%snapshot-shell-task task))))
