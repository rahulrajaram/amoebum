(in-package :amoebum)

(defparameter *shell-default-timeout-seconds* 120)
(defparameter *shell-max-timeout-seconds* 600)
(defparameter *shell-default-max-output-chars* 8192)
(defparameter *shell-working-directory* nil)

(defstruct (shell-task
            (:constructor make-shell-task
                (&key id command cwd timeout-seconds max-output-chars
                 status started-at)))
  id
  command
  cwd
  timeout-seconds
  max-output-chars
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

(defun %trim-whitespace (text)
  (string-trim '(#\Space #\Tab #\Newline #\Return) text))

(defun %normalize-timeout-seconds (timeout-seconds)
  (let ((value (or timeout-seconds *shell-default-timeout-seconds*)))
    (unless (integerp value)
      (error "TIMEOUT-SECONDS must be an integer, got ~S." timeout-seconds))
    (when (or (< value 1) (> value *shell-max-timeout-seconds*))
      (error "TIMEOUT-SECONDS must be between 1 and ~D, got ~S."
             *shell-max-timeout-seconds*
             value))
    value))

(defun %normalize-max-output-chars (max-output-chars)
  (let ((value (or max-output-chars *shell-default-max-output-chars*)))
    (unless (integerp value)
      (error "MAX-OUTPUT-CHARS must be an integer, got ~S." max-output-chars))
    (when (< value 1)
      (error "MAX-OUTPUT-CHARS must be positive, got ~S." value))
    value))

(defun %coerce-directory-input (cwd)
  (cond
    ((pathnamep cwd) cwd)
    ((stringp cwd) (pathname cwd))
    (t (error "CWD must be a pathname, string, or NIL. Got ~S." cwd))))

(defun %current-shell-directory ()
  (or *shell-working-directory*
      (setf *shell-working-directory*
            (config-project-root (current-config)))))

(defun %resolve-shell-directory (cwd)
  (let* ((base (%current-shell-directory))
         (candidate
           (if cwd
               (let ((provided (%coerce-directory-input cwd)))
                 (if (uiop:absolute-pathname-p provided)
                     provided
                     (merge-pathnames provided base)))
               base))
         (resolved (or (ignore-errors (truename candidate)) candidate))
         (directory (uiop:ensure-directory-pathname resolved)))
    (unless (probe-file directory)
      (error "Shell working directory does not exist: ~A" (%path-text directory)))
    directory))

(defun %persist-shell-directory (directory)
  (setf *shell-working-directory* (uiop:ensure-directory-pathname directory)))

(defun %normalize-command (command)
  (unless (stringp command)
    (error "COMMAND must be a string, got ~S." command))
  (let ((trimmed (%trim-whitespace command)))
    (when (zerop (length trimmed))
      (error "COMMAND must not be empty."))
    trimmed))

(defun %truncate-output (text max-output-chars)
  (let* ((value (or text ""))
         (length* (length value)))
    (if (> length* max-output-chars)
        (values (subseq value 0 max-output-chars)
                t
                (- length* max-output-chars))
        (values value nil 0))))

(defun %run-shell-command (command cwd timeout-seconds max-output-chars)
  (handler-case
      (multiple-value-bind (stdout stderr exit-code)
          #+sbcl
          (sb-ext:with-timeout timeout-seconds
            (uiop:run-program (list "bash" "-lc" command)
                              :directory cwd
                              :ignore-error-status t
                              :output :string
                              :error-output :string))
          #-sbcl
          (uiop:run-program (list "bash" "-lc" command)
                            :directory cwd
                            :ignore-error-status t
                            :output :string
                            :error-output :string)
        (multiple-value-bind (stdout* stdout-truncated-p stdout-omitted)
            (%truncate-output stdout max-output-chars)
          (multiple-value-bind (stderr* stderr-truncated-p stderr-omitted)
              (%truncate-output stderr max-output-chars)
            (list :status :completed
                  :command command
                  :cwd (%path-text cwd)
                  :stdout stdout*
                  :stderr stderr*
                  :exit-code exit-code
                  :timed-out nil
                  :stdout-truncated-p stdout-truncated-p
                  :stderr-truncated-p stderr-truncated-p
                  :stdout-omitted-chars stdout-omitted
                  :stderr-omitted-chars stderr-omitted))))
    #+sbcl
    (sb-ext:timeout ()
      (list :status :timeout
            :command command
            :cwd (%path-text cwd)
            :stdout ""
            :stderr ""
            :exit-code nil
            :timed-out t
            :stdout-truncated-p nil
            :stderr-truncated-p nil
            :stdout-omitted-chars 0
            :stderr-omitted-chars 0))))

(defun %ensure-shell-permission (command)
  (let ((decision (check-permission :tool :bash-exec
                                    :command command
                                    :dangerous-p (dangerous-command-p command))))
    (case decision
      (:allow t)
      (:deny
       (error "Permission denied for bash-exec command: ~S." command))
      (:prompt
       (error "bash-exec command requires explicit approval: ~S." command))
      (otherwise
       (error "Unexpected permission decision ~S for command ~S." decision command)))))

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

(defun %snapshot-shell-task (task)
  (%with-shell-task-lock
    (list :task-id (shell-task-id task)
          :status (shell-task-status task)
          :command (shell-task-command task)
          :cwd (%path-text (shell-task-cwd task))
          :started-at (shell-task-started-at task)
          :finished-at (shell-task-finished-at task)
          :result (shell-task-result task))))

(defun %start-background-shell-task (command cwd timeout-seconds max-output-chars)
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
                :status :running
                :started-at (get-universal-time))))
    (%store-shell-task task)
    (sb-thread:make-thread
     (lambda ()
       (let ((result (%run-shell-command command
                                         cwd
                                         timeout-seconds
                                         max-output-chars)))
         (%with-shell-task-lock
           (setf (shell-task-result task) result
                 (shell-task-status task)
                 (if (eq (getf result :status) :completed)
                     :completed
                     :timeout)
                 (shell-task-finished-at task) (get-universal-time)))))
     :name (format nil "amoebum-shell-task-~A" task-id))
    (%snapshot-shell-task task)))

(defun %execute-shell-command (command cwd timeout-seconds max-output-chars background)
  (%ensure-shell-permission command)
  (let ((directory (%resolve-shell-directory cwd)))
    (%persist-shell-directory directory)
    (if background
        (%start-background-shell-task command directory timeout-seconds max-output-chars)
        (%run-shell-command command directory timeout-seconds max-output-chars))))

(defun %fetch-shell-task (task-id)
  (let ((normalized-task-id (%trim-whitespace task-id)))
    (when (zerop (length normalized-task-id))
      (error "TASK-ID must not be empty."))
    (let ((task (%find-shell-task normalized-task-id)))
      (unless task
        (error "Unknown bash-exec TASK-ID: ~S." normalized-task-id))
      (%snapshot-shell-task task))))

(deftool bash-exec ((command (or null string)
                      :description "Shell command to execute in bash -lc"
                      :default nil)
                    (cwd (or null pathname)
                     :description "Optional working directory; persists across calls"
                     :default nil)
                    (timeout-seconds (or null integer)
                     :description "Command timeout in seconds (1-600)"
                     :default nil)
                    (max-output-chars (or null integer)
                     :description "Maximum captured characters for stdout/stderr"
                     :default nil)
                    (background boolean
                     :description "Run command asynchronously and return task ID"
                     :default nil)
                    (task-id (or null string)
                     :description "Background task ID to poll for completion"
                     :default nil))
  "Execute shell commands with stdout/stderr capture, timeout, and background polling."
  (:permission :full-auto)
  (:dangerous t)
  (:category :shell)
  (:timeout 600)
  (let ((timeout (%normalize-timeout-seconds timeout-seconds))
        (max-output (%normalize-max-output-chars max-output-chars)))
    (cond
      ((and command task-id)
       (error "Provide either COMMAND or TASK-ID, not both."))
      (task-id
       (%fetch-shell-task task-id))
      ((null command)
       (error "COMMAND is required unless TASK-ID is provided."))
      (t
       (%execute-shell-command (%normalize-command command)
                               cwd
                               timeout
                               max-output
                               background)))))
