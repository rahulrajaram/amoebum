(in-package :amoebum)

(defparameter *mcp-server-initialize-timeout-seconds* 10.0d0)
(defparameter *mcp-server-ping-timeout-seconds* 5.0d0)
(defparameter *mcp-server-health-check-interval-seconds* 30.0d0)
(defparameter *mcp-server-restart-backoff-base-seconds* 1.0d0)
(defparameter *mcp-server-restart-backoff-max-seconds* 30.0d0)
(defparameter *mcp-server-shutdown-grace-seconds* 5.0d0)
(defparameter *mcp-server-post-term-grace-seconds* 1.0d0)
(defparameter *mcp-server-post-kill-grace-seconds* 1.0d0)
(defparameter *mcp-server-poll-interval-seconds* 0.05d0)

(defstruct (mcp-server
            (:constructor %make-mcp-server
                (&key
                   name
                   command
                   (args nil)
                   cwd
                   (initialize-timeout-seconds
                    *mcp-server-initialize-timeout-seconds*)
                   (ping-timeout-seconds *mcp-server-ping-timeout-seconds*)
                   (health-check-interval-seconds
                    *mcp-server-health-check-interval-seconds*)
                   (restart-backoff-base-seconds
                    *mcp-server-restart-backoff-base-seconds*)
                   (restart-backoff-max-seconds
                    *mcp-server-restart-backoff-max-seconds*)
                   (shutdown-grace-seconds *mcp-server-shutdown-grace-seconds*)
                   (auto-restart-p t))))
  name
  command
  args
  cwd
  (initialize-timeout-seconds *mcp-server-initialize-timeout-seconds* :type real)
  (ping-timeout-seconds *mcp-server-ping-timeout-seconds* :type real)
  (health-check-interval-seconds *mcp-server-health-check-interval-seconds*
   :type real)
  (restart-backoff-base-seconds *mcp-server-restart-backoff-base-seconds*
   :type real)
  (restart-backoff-max-seconds *mcp-server-restart-backoff-max-seconds*
   :type real)
  (shutdown-grace-seconds *mcp-server-shutdown-grace-seconds* :type real)
  (auto-restart-p t :type boolean)
  (running-p nil :type boolean)
  (restarting-p nil :type boolean)
  process
  jsonrpc-client
  monitor-thread
  (restart-count 0 :type integer)
  (consecutive-restart-failures 0 :type integer)
  last-error
  (lock (bordeaux-threads:make-lock "amoebum-mcp-server-lock")))

(defmacro %with-mcp-server-lock ((server) &body body)
  `(bordeaux-threads:with-lock-held ((mcp-server-lock ,server))
     ,@body))

(defun %normalize-mcp-server-name (name command)
  (let ((normalized
          (if name
              (string-trim '(#\Space #\Tab #\Newline #\Return)
                           (princ-to-string name))
              (string-trim '(#\Space #\Tab #\Newline #\Return)
                           (princ-to-string command)))))
    (unless (> (length normalized) 0)
      (error "MCP server NAME must not be empty."))
    normalized))

(defun %normalize-mcp-server-command (command)
  (unless (stringp command)
    (error "MCP server COMMAND must be a string, got ~S." command))
  (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) command)))
    (unless (> (length trimmed) 0)
      (error "MCP server COMMAND must not be empty."))
    trimmed))

(defun %normalize-mcp-server-args (args)
  (unless (listp args)
    (error "MCP server ARGS must be a list of strings, got ~S." args))
  (loop for arg in args
        collect
        (if (stringp arg)
            arg
            (error "MCP server arg must be a string, got ~S." arg))))

(defun %normalize-mcp-server-cwd (cwd)
  (cond
    ((null cwd) nil)
    ((pathnamep cwd) cwd)
    ((stringp cwd) (pathname cwd))
    (t (error "MCP server CWD must be a pathname, string, or NIL, got ~S." cwd))))

(defun %normalize-mcp-server-positive-real (value label)
  (unless (and (realp value) (> value 0))
    (error "~A must be a positive real number, got ~S." label value))
  (float value 1.0d0))

(defun %mcp-server-running-p (server)
  (%with-mcp-server-lock (server)
    (mcp-server-running-p server)))

(defun %mcp-server-active-connection (server)
  (%with-mcp-server-lock (server)
    (values (mcp-server-process server)
            (mcp-server-jsonrpc-client server))))

(defun %mcp-server-take-active-connection (server)
  (%with-mcp-server-lock (server)
    (prog1
        (values (mcp-server-process server)
                (mcp-server-jsonrpc-client server))
      (setf (mcp-server-process server) nil
            (mcp-server-jsonrpc-client server) nil))))

(defun %mcp-server-set-active-connection (server process client)
  (%with-mcp-server-lock (server)
    (setf (mcp-server-process server) process
          (mcp-server-jsonrpc-client server) client))
  server)

(defun %mcp-server-process-alive-p (process)
  (and process
       #+sbcl
       (ignore-errors (sb-ext:process-alive-p process))
       #-sbcl
       nil))

(defun %mcp-server-wait-for-process-exit (process timeout-seconds)
  (let* ((seconds (max 0.0d0 (float timeout-seconds 1.0d0)))
         (deadline (+ (get-internal-real-time)
                      (ceiling (* seconds internal-time-units-per-second)))))
    (loop
      do
         (unless (%mcp-server-process-alive-p process)
           #+sbcl
           (ignore-errors (sb-ext:process-wait process))
           (return t))
         (when (>= (get-internal-real-time) deadline)
           (return nil))
         (sleep *mcp-server-poll-interval-seconds*))))

(defun %mcp-server-signal-process (process signal)
  (when process
    #+sbcl
    (ignore-errors
      (sb-ext:process-kill process signal))
    #-sbcl
    nil))

(defun %mcp-server-initialize-params ()
  (let ((params (make-hash-table :test #'equal))
        (capabilities (make-hash-table :test #'equal))
        (client-info (make-hash-table :test #'equal)))
    (setf (gethash "protocolVersion" params) "2024-11-05"
          (gethash "capabilities" params) capabilities
          (gethash "name" client-info) "amoebum"
          (gethash "version" client-info) "0.1.0"
          (gethash "clientInfo" params) client-info)
    params))

(defun %mcp-server-send-shutdown-sequence (client)
  (when client
    (ignore-errors
      (mcp-jsonrpc-send-notification client "shutdown"))
    (ignore-errors
      (mcp-jsonrpc-send-notification client "exit"))
    (ignore-errors
      (mcp-jsonrpc-stop-reader client))))

(defun %mcp-server-close-process (process)
  (when process
    #+sbcl
    (ignore-errors
      (sb-ext:process-close process))
    #-sbcl
    nil))

(defun %mcp-server-shutdown-connection (process client shutdown-grace-seconds)
  (%mcp-server-send-shutdown-sequence client)
  (unless (%mcp-server-wait-for-process-exit process shutdown-grace-seconds)
    (%mcp-server-signal-process process 15)
    (unless (%mcp-server-wait-for-process-exit process *mcp-server-post-term-grace-seconds*)
      (%mcp-server-signal-process process 9)
      (%mcp-server-wait-for-process-exit process *mcp-server-post-kill-grace-seconds*)))
  (%mcp-server-close-process process)
  (not (%mcp-server-process-alive-p process)))

(defun %mcp-server-spawn-process (server)
  #+sbcl
  (sb-ext:run-program (mcp-server-command server)
                      (mcp-server-args server)
                      :search t
                      :wait nil
                      :directory (mcp-server-cwd server)
                      :input :stream
                      :output :stream
                      :error *error-output*)
  #-sbcl
  (error "MCP server lifecycle requires SBCL run-program support."))

(defun %mcp-server-start-connection (server)
  (let* ((process (%mcp-server-spawn-process server))
         (client (make-mcp-jsonrpc-client
                  :input-stream #+sbcl (sb-ext:process-output process)
                  :output-stream #+sbcl (sb-ext:process-input process)
                  :start-reader-p t)))
    (handler-case
        (progn
          (mcp-jsonrpc-send-request
           client
           "initialize"
           :params (%mcp-server-initialize-params)
           :timeout-seconds (mcp-server-initialize-timeout-seconds server))
          (mcp-jsonrpc-send-notification client "initialized")
          (%mcp-server-set-active-connection server process client))
      (error (condition)
        (%mcp-server-shutdown-connection process client
                                         (mcp-server-shutdown-grace-seconds server))
        (error condition)))))

(defun %mcp-server-monitor-sleep (server seconds)
  (let ((remaining (max 0.0d0 (float seconds 1.0d0))))
    (loop while (> remaining 0.0d0) do
      (unless (%mcp-server-running-p server)
        (return nil))
      (let ((slice (min remaining *mcp-server-poll-interval-seconds*)))
        (sleep slice)
        (decf remaining slice)))
    t))

(defun %mcp-server-next-restart-delay (server)
  (%with-mcp-server-lock (server)
    (let* ((attempt (1+ (mcp-server-consecutive-restart-failures server)))
           (base (mcp-server-restart-backoff-base-seconds server))
           (max-delay (mcp-server-restart-backoff-max-seconds server))
           (delay (min max-delay
                       (* base (expt 2 (1- attempt))))))
      (setf (mcp-server-consecutive-restart-failures server) attempt)
      (float delay 1.0d0))))

(defun mcp-server-health-check (server)
  (unless (mcp-server-p server)
    (error "SERVER must be an MCP-SERVER, got ~S." server))
  (multiple-value-bind (process client)
      (%mcp-server-active-connection server)
    (cond
      ((or (null process)
           (null client)
           (not (%mcp-server-process-alive-p process)))
       nil)
      (t
       (handler-case
           (progn
             (mcp-jsonrpc-send-request
              client
              "ping"
              :timeout-seconds (mcp-server-ping-timeout-seconds server))
             t)
         (mcp-timeout () nil)
         (error () nil))))))

(defun mcp-server-restart (server &key reason)
  (declare (ignore reason))
  (unless (mcp-server-p server)
    (error "SERVER must be an MCP-SERVER, got ~S." server))
  (unless (%mcp-server-running-p server)
    (return-from mcp-server-restart nil))
  (let ((should-restart nil))
    (%with-mcp-server-lock (server)
      (unless (mcp-server-restarting-p server)
        (setf (mcp-server-restarting-p server) t
              should-restart t)))
    (unless should-restart
      (return-from mcp-server-restart nil))
    (unwind-protect
        (let ((delay (%mcp-server-next-restart-delay server)))
          (multiple-value-bind (process client)
              (%mcp-server-take-active-connection server)
            (%mcp-server-shutdown-connection process
                                             client
                                             (mcp-server-shutdown-grace-seconds server)))
          (%mcp-server-monitor-sleep server delay)
          (if (%mcp-server-running-p server)
              (handler-case
                  (progn
                    (%mcp-server-start-connection server)
                    (%with-mcp-server-lock (server)
                      (incf (mcp-server-restart-count server))
                      (setf (mcp-server-consecutive-restart-failures server) 0
                            (mcp-server-last-error server) nil))
                    t)
                (error (condition)
                  (%with-mcp-server-lock (server)
                    (setf (mcp-server-last-error server) condition))
                  nil))
              nil))
      (%with-mcp-server-lock (server)
        (setf (mcp-server-restarting-p server) nil)))))

(defun %mcp-server-monitor-loop (server)
  (unwind-protect
      (loop while (%mcp-server-running-p server) do
        (unless (%mcp-server-monitor-sleep server
                                           (mcp-server-health-check-interval-seconds
                                            server))
          (return))
        (when (and (%mcp-server-running-p server)
                   (mcp-server-auto-restart-p server)
                   (not (mcp-server-health-check server)))
          (mcp-server-restart server :reason :health-check)))
    (%with-mcp-server-lock (server)
      (setf (mcp-server-monitor-thread server) nil))))

(defun %mcp-server-start-monitor (server)
  (%with-mcp-server-lock (server)
    (unless (mcp-server-monitor-thread server)
      (setf (mcp-server-monitor-thread server)
            (bordeaux-threads:make-thread
             (lambda ()
               (%mcp-server-monitor-loop server))
             :name (format nil "amoebum-mcp-server-~A-monitor"
                           (mcp-server-name server)))))))

(defun make-mcp-server (&key
                          name
                          command
                          (args nil)
                          cwd
                          (initialize-timeout-seconds
                            *mcp-server-initialize-timeout-seconds*)
                          (ping-timeout-seconds *mcp-server-ping-timeout-seconds*)
                          (health-check-interval-seconds
                            *mcp-server-health-check-interval-seconds*)
                          (restart-backoff-base-seconds
                            *mcp-server-restart-backoff-base-seconds*)
                          (restart-backoff-max-seconds
                            *mcp-server-restart-backoff-max-seconds*)
                          (shutdown-grace-seconds *mcp-server-shutdown-grace-seconds*)
                          (auto-restart-p t))
  (let ((normalized-command (%normalize-mcp-server-command command)))
    (%make-mcp-server
     :name (%normalize-mcp-server-name name normalized-command)
     :command normalized-command
     :args (%normalize-mcp-server-args args)
     :cwd (%normalize-mcp-server-cwd cwd)
     :initialize-timeout-seconds
     (%normalize-mcp-server-positive-real initialize-timeout-seconds
                                          "INITIALIZE-TIMEOUT-SECONDS")
     :ping-timeout-seconds
     (%normalize-mcp-server-positive-real ping-timeout-seconds
                                          "PING-TIMEOUT-SECONDS")
     :health-check-interval-seconds
     (%normalize-mcp-server-positive-real health-check-interval-seconds
                                          "HEALTH-CHECK-INTERVAL-SECONDS")
     :restart-backoff-base-seconds
     (%normalize-mcp-server-positive-real restart-backoff-base-seconds
                                          "RESTART-BACKOFF-BASE-SECONDS")
     :restart-backoff-max-seconds
     (%normalize-mcp-server-positive-real restart-backoff-max-seconds
                                          "RESTART-BACKOFF-MAX-SECONDS")
     :shutdown-grace-seconds
     (%normalize-mcp-server-positive-real shutdown-grace-seconds
                                          "SHUTDOWN-GRACE-SECONDS")
     :auto-restart-p (not (null auto-restart-p)))))

(defun mcp-server-start (server)
  (unless (mcp-server-p server)
    (error "SERVER must be an MCP-SERVER, got ~S." server))
  (let ((already-running nil))
    (%with-mcp-server-lock (server)
      (if (mcp-server-running-p server)
          (setf already-running t)
          (setf (mcp-server-running-p server) t
                (mcp-server-last-error server) nil)))
    (unless already-running
      (handler-case
          (progn
            (%mcp-server-start-connection server)
            (%mcp-server-start-monitor server))
        (error (condition)
          (%with-mcp-server-lock (server)
            (setf (mcp-server-running-p server) nil
                  (mcp-server-last-error server) condition))
          (multiple-value-bind (process client)
              (%mcp-server-take-active-connection server)
            (%mcp-server-shutdown-connection process
                                             client
                                             (mcp-server-shutdown-grace-seconds
                                              server)))
          (error condition))))
    server))

(defun mcp-server-stop (server)
  (unless (mcp-server-p server)
    (error "SERVER must be an MCP-SERVER, got ~S." server))
  (let (monitor-thread)
    (%with-mcp-server-lock (server)
      (setf (mcp-server-running-p server) nil
            monitor-thread (mcp-server-monitor-thread server)
            (mcp-server-monitor-thread server) nil))
    (when monitor-thread
      (ignore-errors
        (unless (eq monitor-thread (bordeaux-threads:current-thread))
          (bordeaux-threads:join-thread monitor-thread))))
    (multiple-value-bind (process client)
        (%mcp-server-take-active-connection server)
      (%mcp-server-shutdown-connection process
                                       client
                                       (mcp-server-shutdown-grace-seconds
                                        server)))))
