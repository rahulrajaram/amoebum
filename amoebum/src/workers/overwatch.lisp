(in-package :amoebum)

;;; ============================================================
;;; I259: Overwatch Worker Backend
;;;
;;; Delegates shell/process workers to the Overwatch process
;;; supervision service (HTTP API on 127.0.0.1:8765).
;;; Falls back to in-process execution when unavailable.
;;;
;;; Overwatch API:
;;;   POST /run     — submit job → {task_id, status}
;;;   GET  /status/{id}  — poll → TaskStatus
;;;   GET  /output/{id}  — full stdout/stderr → TaskResult
;;;   POST /cancel/{id}  — cancel running task
;;; ============================================================

;;; --- Configuration ---

(defparameter *overwatch-host* "127.0.0.1")
(defparameter *overwatch-port* 8765)
(defparameter *overwatch-poll-interval-seconds* 0.5)
(defparameter *overwatch-connect-timeout-seconds* 3)

;;; --- HTTP transport (injectable for testing) ---

(defparameter *overwatch-http-request-function* nil
  "Override for testing. Signature: (method url &key content) → (values body status-code).
   METHOD is :get or :post. URL is a string. CONTENT is a string (JSON body) or NIL.")

(defun %overwatch-base-url ()
  (format nil "http://~A:~D" *overwatch-host* *overwatch-port*))

(defun %overwatch-http-request (method url &key content)
  "Perform an HTTP request. Returns (values body-string status-code)."
  (if *overwatch-http-request-function*
      (funcall *overwatch-http-request-function* method url :content content)
      (%overwatch-real-http-request method url :content content)))

(defun %overwatch-real-http-request (method url &key content)
  "Real HTTP implementation using curl subprocess."
  (let* ((curl-args (list "curl" "-s" "-w" "\\n%{http_code}"
                          "--connect-timeout"
                          (princ-to-string *overwatch-connect-timeout-seconds*)))
         (curl-args (if content
                        (append curl-args
                                (list "-X" (string-upcase (symbol-name method))
                                      "-H" "Content-Type: application/json"
                                      "-d" content
                                      url))
                        (append curl-args
                                (when (eq method :post)
                                  (list "-X" "POST"))
                                (list url)))))
    (handler-case
        (multiple-value-bind (stdout stderr exit-code)
            (uiop:run-program curl-args
                              :ignore-error-status t
                              :output :string
                              :error-output :string)
          (declare (ignore stderr))
          (if (and (integerp exit-code) (zerop exit-code) (plusp (length stdout)))
              (let* ((lines (uiop:split-string stdout :separator '(#\Newline)))
                     (status-line (car (last lines)))
                     (body (format nil "~{~A~^~%~}"
                                   (butlast lines))))
                (values body (parse-integer status-line :junk-allowed t)))
              (values "" 0)))
      (error ()
        (values "" 0)))))

;;; --- JSON helpers (minimal, no dependency) ---

(defun %ow-json-get (json key)
  "Extract a string/number value for KEY from a flat JSON object.
   Very minimal parser — handles simple {\"key\": \"value\"} or {\"key\": number}."
  (let* ((needle (format nil "\"~A\"" key))
         (pos (search needle json)))
    (when pos
      (let* ((after-key (+ pos (length needle)))
             (colon-pos (position #\: json :start after-key)))
        (when colon-pos
          (let* ((value-start (position-if-not
                               (lambda (c) (member c '(#\Space #\Tab)))
                               json :start (1+ colon-pos))))
            (when value-start
              (cond
                ;; String value
                ((char= (char json value-start) #\")
                 (let ((end (position #\" json :start (1+ value-start))))
                   (when end
                     (subseq json (1+ value-start) end))))
                ;; Number value
                ((digit-char-p (char json value-start))
                 (let ((end (or (position-if
                                 (lambda (c) (not (or (digit-char-p c) (char= c #\.))))
                                 json :start value-start)
                                (length json))))
                   (parse-integer (subseq json value-start end) :junk-allowed t)))
                ;; null
                ((and (>= (- (length json) value-start) 4)
                      (string= "null" (subseq json value-start (+ value-start 4))))
                 nil)
                (t nil)))))))))

(defun %ow-json-object (&rest pairs)
  "Build a simple JSON object string from keyword-value pairs."
  (format nil "{~{~A~^, ~}}"
          (loop for (key value) on pairs by #'cddr
                collect (format nil "\"~A\": ~A"
                                (string-downcase (symbol-name key))
                                (cond
                                  ((null value) "null")
                                  ((integerp value) (princ-to-string value))
                                  ((stringp value) (format nil "\"~A\"" value))
                                  ((listp value) ; JSON array of strings
                                   (format nil "[~{\"~A\"~^, ~}]" value))
                                  (t (format nil "\"~A\"" value)))))))

;;; --- Overwatch status mapping ---

(defun %ow-status-to-worker-status (ow-status)
  "Map Overwatch TaskStatus string to worker status keyword."
  (let ((upper (string-upcase (or ow-status ""))))
    (cond
      ((string= upper "PENDING") :pending)
      ((string= upper "RUNNING") :running)
      ((string= upper "WAITING_FOR_INPUT") :running)
      ((string= upper "COMPLETED") :completed)
      ((string= upper "FAILED") :failed)
      ((string= upper "TIMED_OUT") :timeout)
      ((string= upper "STALLED") :failed)
      ((string= upper "CANCELLED") :cancelled)
      (t :failed))))

;;; --- Overwatch supervisor ---

(defclass overwatch-supervisor (worker-supervisor)
  ((fallback :initarg :fallback
             :initform nil
             :accessor overwatch-fallback-supervisor
             :documentation "Fallback supervisor when overwatch is unavailable.")))

(defmethod supervisor-spawn ((supervisor overwatch-supervisor) type command
                             &key label timeout-seconds max-output-chars
                                  cwd max-retries worktree)
  (declare (ignore max-output-chars))
  ;; Only shell and process types delegate to overwatch
  (unless (member type '(:shell :process) :test #'eq)
    ;; Agent type falls back to in-process
    (return-from supervisor-spawn
      (supervisor-spawn (ensure-overwatch-fallback supervisor) type command
                        :label label :timeout-seconds timeout-seconds
                        :cwd cwd :max-retries max-retries
                        :worktree worktree)))
  ;; Try to submit to overwatch
  (let* ((worker-id (%next-worker-id type))
         (now (%worker-now))
         (worker (%make-worker-record
                  :id worker-id
                  :type type
                  :label (or label (if (stringp command)
                                       (subseq command 0 (min 60 (length command)))
                                       "task"))
                  :command command
                  :status :pending
                  :created-at now
                  :backend :overwatch
                  :max-retries (or max-retries 0))))
    (%store-worker worker)
    (%publish-worker-event +event-type-worker-spawned+ worker)
    (let ((submitted (%overwatch-submit-job
                      worker command
                      :cwd cwd
                      :timeout-seconds timeout-seconds)))
      (if submitted
          ;; Start background polling thread
          (%start-overwatch-poller worker)
          ;; Fallback to in-process if submission failed
          (progn
            (%with-worker-lock
              (setf (worker-record-backend worker) :in-process))
            (%spawn-shell-worker worker command
                                :cwd cwd
                                :timeout-seconds (or timeout-seconds 120)
                                :max-output-chars 8192))))
    worker))

(defun ensure-overwatch-fallback (supervisor)
  (or (overwatch-fallback-supervisor supervisor)
      (setf (overwatch-fallback-supervisor supervisor)
            (make-instance 'in-process-supervisor))))

(defun %overwatch-submit-job (worker command &key cwd timeout-seconds)
  "Submit a job to Overwatch. Returns T on success, NIL on failure."
  (handler-case
      (let* ((cmd-list (if (stringp command)
                           (list "bash" "-lc" command)
                           (if (listp command)
                               command
                               (list (princ-to-string command)))))
             (body (%ow-json-object
                    :command cmd-list
                    :cwd (or cwd "/tmp")
                    :profile_name "generic"
                    :soft_timeout_sec (or timeout-seconds 120)))
             (url (format nil "~A/run" (%overwatch-base-url))))
        (multiple-value-bind (response-body status-code)
            (%overwatch-http-request :post url :content body)
          (when (and (integerp status-code) (= status-code 200))
            (let ((task-id (%ow-json-get response-body "task_id")))
              (when task-id
                (%with-worker-lock
                  (setf (worker-record-inner-id worker) task-id
                        (worker-record-status worker) :running
                        (worker-record-started-at worker) (%worker-now)))
                (%publish-worker-event +event-type-worker-started+ worker)
                t)))))
    (error ()
      nil)))

(defun %start-overwatch-poller (worker)
  "Start a background thread that polls overwatch for worker status."
  #+sb-thread
  (sb-thread:make-thread
   (lambda ()
     (let ((task-id (worker-record-inner-id worker)))
       (loop
         (let ((status (%overwatch-poll-status task-id)))
           (when (null status)
             ;; Overwatch unreachable — mark as failed
             (%with-worker-lock
               (setf (worker-record-status worker) :failed
                     (worker-record-finished-at worker) (%worker-now)
                     (worker-record-error-message worker) "Overwatch unreachable during polling"))
             (%publish-worker-event +event-type-worker-failed+ worker :severity :error)
             (return))
           (let ((worker-status (%ow-status-to-worker-status status)))
             (when (%worker-terminal-status-p worker-status)
               ;; Fetch full output
               (let ((output (%overwatch-fetch-output task-id)))
                 (%with-worker-lock
                   (setf (worker-record-status worker) worker-status
                         (worker-record-finished-at worker) (%worker-now)
                         (worker-record-output-buffer worker)
                         (or (getf output :stdout) "")
                         (worker-record-exit-code worker)
                         (getf output :exit-code)
                         (worker-record-result worker)
                         (list :status worker-status
                               :stdout (or (getf output :stdout) "")
                               :stderr (or (getf output :stderr) "")
                               :exit-code (getf output :exit-code)))))
               (%publish-worker-event
                (if (eq worker-status :completed)
                    +event-type-worker-completed+
                    +event-type-worker-failed+)
                worker
                :severity (if (eq worker-status :completed) :info :error))
               (return))))
         (sleep *overwatch-poll-interval-seconds*))))
   :name (format nil "amoebum-overwatch-poller-~A" (worker-record-id worker))))

(defun %overwatch-poll-status (task-id)
  "Poll overwatch for task status. Returns status string or NIL on failure."
  (handler-case
      (let ((url (format nil "~A/status/~A" (%overwatch-base-url) task-id)))
        (multiple-value-bind (body status-code)
            (%overwatch-http-request :get url)
          (when (and (integerp status-code) (= status-code 200))
            (%ow-json-get body "status"))))
    (error () nil)))

(defun %overwatch-fetch-output (task-id)
  "Fetch full output from overwatch. Returns plist (:stdout :stderr :exit-code)."
  (handler-case
      (let ((url (format nil "~A/output/~A" (%overwatch-base-url) task-id)))
        (multiple-value-bind (body status-code)
            (%overwatch-http-request :get url)
          (if (and (integerp status-code) (= status-code 200))
              (list :stdout (or (%ow-json-get body "stdout") "")
                    :stderr (or (%ow-json-get body "stderr") "")
                    :exit-code (%ow-json-get body "exit_code"))
              (list :stdout "" :stderr "" :exit-code nil))))
    (error ()
      (list :stdout "" :stderr "" :exit-code nil))))

;;; --- Overwatch health check ---

(defun overwatch-available-p ()
  "Check if Overwatch service is reachable."
  (handler-case
      (let ((url (format nil "~A/status/health-probe" (%overwatch-base-url))))
        (multiple-value-bind (_body status-code)
            (%overwatch-http-request :get url)
          (declare (ignore _body))
          ;; 404 means server is up (no /health endpoint, returns 404 for unknown IDs)
          (and (integerp status-code)
               (or (= status-code 200) (= status-code 404)))))
    (error () nil)))

;;; --- Delegate cancel/status/result/list to parent protocol ---
;;; These all use the shared worker registry, so they work identically
;;; to in-process-supervisor.

(defmethod supervisor-status ((supervisor overwatch-supervisor) worker-id)
  (let ((worker (%find-worker worker-id)))
    (if worker
        (worker-record-status worker)
        nil)))

(defmethod supervisor-result ((supervisor overwatch-supervisor) worker-id)
  (let ((worker (%find-worker worker-id)))
    (when (and worker (%worker-terminal-status-p (worker-record-status worker)))
      (worker-record-result worker))))

(defmethod supervisor-cancel ((supervisor overwatch-supervisor) worker-id)
  (let ((worker (%find-worker worker-id)))
    (unless worker (return-from supervisor-cancel nil))
    (when (%worker-terminal-status-p (worker-record-status worker))
      (return-from supervisor-cancel nil))
    ;; Try to cancel via overwatch if it's an overwatch-backed worker
    (when (eq (worker-record-backend worker) :overwatch)
      (let ((task-id (worker-record-inner-id worker)))
        (when task-id
          (ignore-errors
            (%overwatch-http-request
             :post (format nil "~A/cancel/~A" (%overwatch-base-url) task-id))))))
    (%with-worker-lock
      (setf (worker-record-status worker) :cancelled
            (worker-record-finished-at worker) (%worker-now)))
    (%publish-worker-event +event-type-worker-cancelled+ worker :severity :warning)
    t))

(defmethod supervisor-list ((supervisor overwatch-supervisor) &key (include-finished t))
  (%with-worker-lock
    (let ((workers '()))
      (maphash (lambda (_id worker)
                 (declare (ignore _id))
                 (when (or include-finished
                           (not (%worker-terminal-status-p
                                 (worker-record-status worker))))
                   (push worker workers)))
               *worker-registry*)
      (sort workers #'< :key #'worker-record-created-at))))

(defmethod supervisor-output ((supervisor overwatch-supervisor) worker-id)
  (let ((worker (%find-worker worker-id)))
    (when worker
      (worker-record-output-buffer worker))))

;;; --- Backend selection ---

(defun select-worker-backend (&key (backend :auto))
  "Select a worker supervisor backend.
   BACKEND: :auto, :in-process, :overwatch."
  (ecase backend
    (:auto
     (if (overwatch-available-p)
         (make-instance 'overwatch-supervisor)
         (make-instance 'in-process-supervisor)))
    (:in-process
     (make-instance 'in-process-supervisor))
    (:overwatch
     (make-instance 'overwatch-supervisor))))
