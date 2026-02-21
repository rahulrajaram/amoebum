(in-package :amoebum)

;;; ============================================================
;;; I261: Parallel Worker Fan-Out and Join
;;;
;;; Spawn multiple workers concurrently, await all or any,
;;; with group-level timeout and result aggregation.
;;; ============================================================

;;; --- Worker group ---

(defstruct (worker-group
            (:constructor %make-worker-group
                (&key id worker-ids created-at timeout-seconds)))
  (id nil :type (or null string))
  (worker-ids nil :type list)
  (created-at 0 :type integer)
  (timeout-seconds nil :type (or null integer))
  (status :running :type keyword))  ; :running, :completed, :timed-out

(defparameter *worker-group-registry* (make-hash-table :test #'equal))
(defparameter *next-group-sequence* 0)

#+sb-thread
(defparameter *worker-group-lock*
  (sb-thread:make-mutex :name "amoebum-worker-group-lock"))

(defmacro %with-group-lock (&body body)
  #+sb-thread
  `(sb-thread:with-mutex (*worker-group-lock*) ,@body)
  #-sb-thread
  `(progn ,@body))

(defun %next-group-id ()
  (%with-group-lock
    (incf *next-group-sequence*)
    (format nil "wg-~4,'0D" *next-group-sequence*)))

;;; --- Fan-out ---

(defun fan-out-workers (specs &key (timeout-seconds nil) (supervisor nil))
  "Spawn multiple workers concurrently from SPECS.
   Each spec is a plist (:type :command :label :timeout-seconds :cwd).
   Returns (values group-id worker-ids).
   TIMEOUT-SECONDS is the overall group timeout."
  (let* ((sup (or supervisor (ensure-worker-supervisor)))
         (group-id (%next-group-id))
         (worker-ids '()))
    (dolist (spec specs)
      (let* ((type (or (getf spec :type) :shell))
             (command (getf spec :command))
             (label (getf spec :label))
             (worker-timeout (getf spec :timeout-seconds))
             (cwd (getf spec :cwd))
             (worker (supervisor-spawn sup type command
                                       :label (or label (format nil "~A fanout" group-id))
                                       :timeout-seconds (or worker-timeout 120)
                                       :cwd cwd)))
        (push (worker-record-id worker) worker-ids)))
    (setf worker-ids (nreverse worker-ids))
    ;; Register the group
    (let ((group (%make-worker-group
                  :id group-id
                  :worker-ids worker-ids
                  :created-at (get-universal-time)
                  :timeout-seconds timeout-seconds)))
      (%with-group-lock
        (setf (gethash group-id *worker-group-registry*) group)))
    (values group-id worker-ids)))

;;; --- Join all ---

(defun join-worker-group (group-id &key (poll-interval 0.5))
  "Block until all workers in GROUP-ID complete or group times out.
   On group timeout, cancels remaining workers.
   Returns list of (worker-id status result) triples in submission order."
  (let ((group (%with-group-lock
                 (gethash group-id *worker-group-registry*))))
    (unless group
      (return-from join-worker-group nil))
    (let ((worker-ids (worker-group-worker-ids group))
          (deadline (when (worker-group-timeout-seconds group)
                     (+ (get-universal-time)
                        (worker-group-timeout-seconds group)))))
      (loop
        (let ((all-done t)
              (results '()))
          (dolist (wid worker-ids)
            (let ((status (worker-status wid)))
              (unless (or (null status) (%worker-terminal-status-p status))
                (setf all-done nil))
              (push (list wid status (worker-result wid)) results)))
          ;; All workers done
          (when all-done
            (%with-group-lock
              (setf (worker-group-status group) :completed))
            (return (nreverse results)))
          ;; Group timeout
          (when (and deadline (>= (get-universal-time) deadline))
            ;; Cancel remaining non-terminal workers
            (dolist (wid worker-ids)
              (let ((status (worker-status wid)))
                (when (and status (not (%worker-terminal-status-p status)))
                  (worker-cancel wid))))
            (%with-group-lock
              (setf (worker-group-status group) :timed-out))
            ;; Collect final results
            (return (nreverse
                     (mapcar (lambda (wid)
                               (list wid (worker-status wid) (worker-result wid)))
                             worker-ids))))
          (sleep poll-interval))))))

;;; --- Race (first completed) ---

(defun race-worker-group (group-id &key (poll-interval 0.5) (cancel-remaining t))
  "Block until any worker in GROUP-ID completes.
   If CANCEL-REMAINING, cancel other workers after first finishes.
   Returns (values winner-id winner-status winner-result)."
  (let ((group (%with-group-lock
                 (gethash group-id *worker-group-registry*))))
    (unless group
      (return-from race-worker-group (values nil nil nil)))
    (let ((worker-ids (worker-group-worker-ids group))
          (deadline (when (worker-group-timeout-seconds group)
                     (+ (get-universal-time)
                        (worker-group-timeout-seconds group)))))
      (loop
        ;; Check each worker
        (dolist (wid worker-ids)
          (let ((status (worker-status wid)))
            (when (and status (%worker-terminal-status-p status))
              ;; Found a completed worker
              (when cancel-remaining
                (dolist (other worker-ids)
                  (unless (string= other wid)
                    (ignore-errors (worker-cancel other)))))
              (%with-group-lock
                (setf (worker-group-status group) :completed))
              (return-from race-worker-group
                (values wid status (worker-result wid))))))
        ;; Group timeout
        (when (and deadline (>= (get-universal-time) deadline))
          (dolist (wid worker-ids)
            (ignore-errors (worker-cancel wid)))
          (%with-group-lock
            (setf (worker-group-status group) :timed-out))
          (return-from race-worker-group (values nil :timeout nil)))
        (sleep poll-interval)))))

;;; --- Result aggregation ---

(defgeneric merge-worker-results (result-type results)
  (:documentation
   "Merge a list of worker result plists according to RESULT-TYPE.
    RESULTS is a list of (worker-id status result) triples.
    Returns a merged result."))

(defmethod merge-worker-results ((result-type (eql :concat)) results)
  "Concatenate all worker output buffers."
  (with-output-to-string (out)
    (dolist (triple results)
      (let* ((wid (first triple))
             (output (worker-output wid)))
        (when (and output (plusp (length output)))
          (write-string output out)
          (terpri out))))))

(defmethod merge-worker-results ((result-type (eql :list)) results)
  "Return results as a list of plists."
  (mapcar (lambda (triple)
            (list :worker-id (first triple)
                  :status (second triple)
                  :result (third triple)
                  :output (worker-output (first triple))))
          results))

(defmethod merge-worker-results ((result-type (eql :first-success)) results)
  "Return the first successful result, or NIL."
  (let ((success (find :completed results :key #'second)))
    (when success
      (list :worker-id (first success)
            :status :completed
            :result (third success)
            :output (worker-output (first success))))))

;;; --- Group info ---

(defun find-worker-group (group-id)
  "Find a worker group by ID."
  (%with-group-lock
    (gethash group-id *worker-group-registry*)))

(defun worker-group-results (group-id)
  "Return current results for all workers in GROUP-ID."
  (let ((group (find-worker-group group-id)))
    (when group
      (mapcar (lambda (wid)
                (list wid (worker-status wid) (worker-result wid)))
              (worker-group-worker-ids group)))))

(defun clear-worker-groups ()
  "Clear all worker groups."
  (%with-group-lock
    (clrhash *worker-group-registry*)
    (setf *next-group-sequence* 0))
  t)
