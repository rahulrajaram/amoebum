(in-package :pseudopod)

;;; ---------------------------------------------------------------------------
;;; I110: Command execution primitive
;;;
;;; Executes a shell command as a subprocess, capturing stdout, stderr, and
;;; exit code.  Returns a structured COMMAND-RESULT.  Supports configurable
;;; timeout (default 120 s) and signals PSEUDOPOD-COMMAND-TIMEOUT on expiry.
;;; ---------------------------------------------------------------------------

;;; ---- Condition types ----

(define-condition pseudopod-command-error (pseudopod-error)
  ((command :initarg :command
            :initform nil
            :reader pseudopod-command-error-command))
  (:report (lambda (condition stream)
             (format stream "Command error~@[ for ~S~]: ~A"
                     (pseudopod-command-error-command condition)
                     (or (pseudopod-error-message condition)
                         "unknown command error")))))

(define-condition pseudopod-command-timeout (pseudopod-command-error)
  ((timeout-seconds :initarg :timeout-seconds
                    :initform nil
                    :reader pseudopod-command-timeout-seconds))
  (:report (lambda (condition stream)
             (format stream "Command timed out after ~A seconds: ~S"
                     (or (pseudopod-command-timeout-seconds condition) "?")
                     (or (pseudopod-command-error-command condition) "<unknown>")))))

;;; ---- Result struct ----

(defstruct (command-result (:constructor %make-command-result))
  "Structured result of executing a shell command."
  (stdout      "" :type string)
  (stderr      "" :type string)
  (exit-code   -1 :type integer)
  (duration-ms  0 :type integer)
  (command     "" :type string))

;;; ---- Default timeout ----

(defparameter *default-command-timeout* 120
  "Default timeout in seconds for command execution.")

;;; ---- Internal helpers ----

(defun %read-stream-to-string (stream)
  "Read all available content from STREAM into a string.
Returns an empty string if STREAM is NIL or unreadable."
  (handler-case
      (if stream
          (let ((content (make-string-output-stream)))
            (loop for line = (read-line stream nil nil)
                  while line
                  do (write-line line content))
            ;; Remove trailing newline added by write-line on last iteration
            (let ((result (get-output-stream-string content)))
              (if (and (plusp (length result))
                       (char= (char result (1- (length result))) #\Newline))
                  (subseq result 0 (1- (length result)))
                  result)))
          "")
    (error () "")))

(defun %current-time-ms ()
  "Return current time in milliseconds (internal real time converted)."
  (values (round (* 1000 (/ (get-internal-real-time)
                            internal-time-units-per-second)))))

;;; ---- Public API ----

(defun run-command (command &key (timeout *default-command-timeout*)
                                 (shell "/bin/sh"))
  "Execute COMMAND as a subprocess and return a COMMAND-RESULT.

COMMAND is a string passed to SHELL (default /bin/sh) via -c.
TIMEOUT is the maximum seconds to wait (default *DEFAULT-COMMAND-TIMEOUT*,
which is 120).  If the process exceeds TIMEOUT, it is killed and
PSEUDOPOD-COMMAND-TIMEOUT is signalled.

Returns a COMMAND-RESULT struct with:
  - STDOUT:      captured standard output (string)
  - STDERR:      captured standard error (string)
  - EXIT-CODE:   process exit code (integer)
  - DURATION-MS: wall-clock milliseconds elapsed (integer)
  - COMMAND:     the original command string"
  (check-type command string)
  (check-type timeout (integer 1))
  (let ((process nil)
        (start-ms (%current-time-ms)))
    (unwind-protect
        (progn
          (setf process
                (sb-ext:run-program shell (list "-c" command)
                                    :search t
                                    :wait nil
                                    :output :stream
                                    :error :stream))
          ;; Wait for process with timeout
          (let ((deadline (+ (get-internal-real-time)
                             (* timeout internal-time-units-per-second))))
            (loop
              (when (not (sb-ext:process-alive-p process))
                (return))
              (when (>= (get-internal-real-time) deadline)
                ;; Kill the timed-out process
                (handler-case
                    (progn
                      (sb-ext:process-kill process 15 :process) ; SIGTERM
                      (sleep 0.1)
                      (when (sb-ext:process-alive-p process)
                        (sb-ext:process-kill process 9 :process))) ; SIGKILL
                  (error () nil))
                (handler-case (sb-ext:process-wait process)
                  (error () nil))
                (error 'pseudopod-command-timeout
                       :command command
                       :timeout-seconds timeout
                       :message (format nil "Command timed out after ~A seconds"
                                        timeout)))
              (sleep 0.05)))
          ;; Process finished normally -- collect output
          (let* ((stdout (%read-stream-to-string
                          (sb-ext:process-output process)))
                 (stderr (%read-stream-to-string
                          (sb-ext:process-error process)))
                 (exit-code (sb-ext:process-exit-code process))
                 (end-ms (%current-time-ms)))
            (%make-command-result
             :stdout stdout
             :stderr stderr
             :exit-code exit-code
             :duration-ms (max 0 (- end-ms start-ms))
             :command command)))
      ;; Cleanup: ensure process is closed
      (when process
        (handler-case
            (progn
              (when (sb-ext:process-alive-p process)
                (sb-ext:process-kill process 9 :process)
                (sb-ext:process-wait process))
              (sb-ext:process-close process))
          (error () nil))))))
