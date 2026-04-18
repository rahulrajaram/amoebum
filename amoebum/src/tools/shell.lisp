(in-package :amoebum)

(defparameter *shell-default-timeout-seconds* 120)
(defparameter *shell-max-timeout-seconds* 600)
(defparameter *shell-default-max-output-chars* 8192)
(defparameter *shell-default-max-output-bytes* (* 256 1024))
(defparameter *shell-default-max-output-lines* 4096)
(defparameter *shell-process-poll-interval-seconds* 0.02)
(defparameter *shell-working-directory* nil)

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

(defun %normalize-max-output-bytes (max-output-bytes)
  (let ((value (or max-output-bytes *shell-default-max-output-bytes*)))
    (unless (integerp value)
      (error "MAX-OUTPUT-BYTES must be an integer, got ~S." max-output-bytes))
    (when (< value 1)
      (error "MAX-OUTPUT-BYTES must be positive, got ~S." value))
    value))

(defun %normalize-max-output-lines (max-output-lines)
  (let ((value (or max-output-lines *shell-default-max-output-lines*)))
    (unless (integerp value)
      (error "MAX-OUTPUT-LINES must be an integer, got ~S." max-output-lines))
    (when (< value 1)
      (error "MAX-OUTPUT-LINES must be positive, got ~S." value))
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
      (error "Shell working directory does not exist: ~A" (coerce-path-string directory)))
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

(defun %split-env-assignment (entry)
  (let ((pos (position #\= entry)))
    (if pos
        (values (subseq entry 0 pos)
                (subseq entry (1+ pos)))
        (values entry ""))))

(defun %coerce-process-env-pair (name value)
  (cons (intern (string-upcase name) :keyword)
        (or value "")))

(defun %coerce-process-env-entry (entry)
  (cond
    ((and (consp entry) (keywordp (car entry)))
     entry)
    ((and (consp entry) (stringp (car entry)))
     (%coerce-process-env-pair (car entry) (cdr entry)))
    ((stringp entry)
     (multiple-value-bind (name value)
         (%split-env-assignment entry)
       (%coerce-process-env-pair name value)))
    (t
     (error "Unsupported environment entry ~S." entry))))

(defun %coerce-process-env (env-vars)
  "Normalize ENV-VARS into the alist shape expected by RUN-PROGRAM :ENV."
  (when env-vars
    (mapcar #'%coerce-process-env-entry env-vars)))

(defun %process-env-entry-keyword (entry)
  (car entry))

(defun %process-env-override-pair (entry)
  (cond
    ((and (consp entry) (keywordp (car entry)))
     (cons (string-upcase (symbol-name (car entry)))
           (or (cdr entry) "")))
    ((and (consp entry) (stringp (car entry)))
     (cons (car entry) (or (cdr entry) "")))
    ((stringp entry)
     (multiple-value-bind (name value)
         (%split-env-assignment entry)
       (cons name value)))
    (t
     (error "Unsupported environment entry ~S." entry))))

(defun %dedupe-process-env (entries)
  (let ((seen (make-hash-table :test #'eq))
        (result '()))
    (dolist (entry (reverse entries) (nreverse result))
      (let ((key (%process-env-entry-keyword entry)))
        (unless (gethash key seen)
          (setf (gethash key seen) t)
          (push entry result))))))

(defun %effective-process-env (env-vars)
  (if (current-delegated-agent-id)
      (let* ((scoped-overrides (current-delegated-agent-secret-env-overrides))
             (shell-env (merge-shell-environment
                         (%default-shell-environment)
                         :env-overrides (append (mapcar #'%process-env-override-pair
                                                        (or env-vars '()))
                                                scoped-overrides)
                         :inherit-env-p t
                         :filter-sensitive-p t))
             (scoped-env
               (%coerce-process-env
                (shell-env-to-string-list (assemble-shell-env shell-env)))))
        (%dedupe-process-env scoped-env))
      (%coerce-process-env env-vars)))

(defun %utf8-char-size (char)
  (let ((code (char-code char)))
    (cond
      ((<= code #x7F) 1)
      ((<= code #x7FF) 2)
      ((<= code #xFFFF) 3)
      (t 4))))

(defun %append-diagnostic-line (text diagnostic)
  (let ((base (or text "")))
    (if (zerop (length base))
        diagnostic
        (format nil "~A~%~A" base diagnostic))))

(defun %bash-exec-running-under-pipeline-p ()
  (let ((tool-name (and (stringp *pipeline-current-tool-name*)
                        (normalize-name *pipeline-current-tool-name*))))
    (string= tool-name "bash-exec")))

#+sbcl
(defun %shell-process-alive-p (process)
  (and process
       (ignore-errors (sb-ext:process-alive-p process))))

#+sbcl
(defun %wait-for-shell-process-exit (process timeout-seconds)
  (let ((deadline (+ (get-internal-real-time)
                     (ceiling (* (max 0.0d0 (float timeout-seconds 1.0d0))
                                 internal-time-units-per-second)))))
    (loop
      do
         (unless (%shell-process-alive-p process)
           (ignore-errors (sb-ext:process-wait process))
           (return t))
         (when (>= (get-internal-real-time) deadline)
           (return nil))
         (sleep *shell-process-poll-interval-seconds*))))

#+sbcl
(defun %terminate-shell-process (process)
  (when process
    (ignore-errors
      (uiop:terminate-process process))
    (ignore-errors
      (sb-ext:process-kill process 15 :process))
    (ignore-errors
      (sb-ext:process-kill process 15 :process-group))
    (unless (%wait-for-shell-process-exit process 0.2d0)
      (ignore-errors
        (sb-ext:process-kill process 9 :process))
      (ignore-errors
        (sb-ext:process-kill process 9 :process-group))
      (ignore-errors
        (sb-ext:process-kill process 9))
      (%wait-for-shell-process-exit process 0.5d0))))

#+sbcl
(defun %capture-shell-stream (stream max-output-chars monitor-char-fn)
  (let ((capture (make-string-output-stream))
        (captured 0)
        (truncated-p nil)
        (omitted-chars 0))
    (loop for char = (read-char stream nil :eof)
          until (eq char :eof) do
            (funcall monitor-char-fn char)
            (if (< captured max-output-chars)
                (progn
                  (write-char char capture)
                  (incf captured))
                (progn
                  (setf truncated-p t)
                  (incf omitted-chars))))
    (values (get-output-stream-string capture)
            truncated-p
            omitted-chars)))

#+sbcl
(defstruct (shell-run-context
            (:constructor make-shell-run-context
                (&key command cwd timeout-seconds max-output-chars
                 max-output-bytes max-output-lines resolved-shell
                 process-env prepared-command invocation invocation-text
                 policy-command-text)))
  command
  cwd
  timeout-seconds
  max-output-chars
  max-output-bytes
  max-output-lines
  resolved-shell
  process-env
  prepared-command
  invocation
  invocation-text
  policy-command-text)

#+sbcl
(defstruct (shell-run-state
            (:constructor make-shell-run-state ()))
  process
  stdout-thread
  stderr-thread
  (stdout "")
  (stderr "")
  stdout-truncated-p
  stderr-truncated-p
  (stdout-omitted 0)
  (stderr-omitted 0)
  (total-output-bytes 0)
  (total-output-lines 0)
  termination-cause
  (monitor-lock (sb-thread:make-mutex :name "amoebum-shell-output-monitor")))

#+sbcl
(defun %make-shell-run-context (command cwd timeout-seconds
                                max-output-chars max-output-bytes max-output-lines
                                &key shell-executable profile-files env-vars)
  (let* ((resolved-shell (or shell-executable "bash"))
         (process-env (%effective-process-env env-vars))
         (prepared-command (if profile-files
                               (wrap-command-with-shell-profile-init
                                command profile-files)
                               command))
         (invocation (list resolved-shell "-lc" prepared-command)))
    (make-shell-run-context
     :command command
     :cwd cwd
     :timeout-seconds timeout-seconds
     :max-output-chars max-output-chars
     :max-output-bytes max-output-bytes
     :max-output-lines max-output-lines
     :resolved-shell resolved-shell
     :process-env process-env
     :prepared-command prepared-command
     :invocation invocation
     :invocation-text (%command->string invocation)
     :policy-command-text (or command prepared-command))))

#+sbcl
(defun %shell-set-termination-cause (state cause)
  (sb-thread:with-mutex ((shell-run-state-monitor-lock state))
    (when (null (shell-run-state-termination-cause state))
      (setf (shell-run-state-termination-cause state) cause)
      t)))

#+sbcl
(defun %shell-current-termination-cause (state)
  (sb-thread:with-mutex ((shell-run-state-monitor-lock state))
    (shell-run-state-termination-cause state)))

#+sbcl
(defun %shell-monitor-char (state context char)
  (let ((cause nil))
    (sb-thread:with-mutex ((shell-run-state-monitor-lock state))
      (incf (shell-run-state-total-output-bytes state) (%utf8-char-size char))
      (when (char= char #\Newline)
        (incf (shell-run-state-total-output-lines state)))
      (when (null (shell-run-state-termination-cause state))
        (cond
          ((> (shell-run-state-total-output-bytes state)
              (shell-run-context-max-output-bytes context))
           (setf (shell-run-state-termination-cause state) :output-bytes
                 cause :output-bytes))
          ((> (shell-run-state-total-output-lines state)
              (shell-run-context-max-output-lines context))
           (setf (shell-run-state-termination-cause state) :output-lines
                 cause :output-lines)))))
    (when cause
      (%terminate-shell-process (shell-run-state-process state)))))

#+sbcl
(defun %shell-permission-trace-reason (trace)
  (or (and (listp trace) (getf trace :actionable-reason))
      (and (listp trace) (getf trace :reason))
      "approval required"))

#+sbcl
(defun %shell-signal-prompt-denial (decision)
  (let* ((trace (last-permission-decision-trace))
         (reason (%shell-permission-trace-reason trace))
         (reason-code (and (listp trace) (getf trace :reason-code))))
    (error 'tool-permission-denied
           :tool-name :bash-exec
           :arguments nil
           :reason-code reason-code
           :reason reason
           :message (format nil
                            "Permission decision ~A for tool ~S: ~A."
                            decision
                            :bash-exec
                            reason))))

(defun %git-push-option-consumes-next-p (token)
  (member token '("--repo" "--receive-pack" "--exec" "-o" "--push-option")
          :test #'string=))

(defun %parse-git-push-segment (segment)
  (when (and (listp segment)
             (>= (length segment) 2)
             (string= "git" (string-downcase (or (first segment) "")))
             (string= "push" (string-downcase (or (second segment) ""))))
    (let ((remaining (cddr segment))
          (options '())
          (positionals '())
          (end-of-options-p nil))
      (loop while remaining do
        (let ((token (first remaining)))
          (cond
            ((and (not end-of-options-p) (string= token "--"))
             (setf end-of-options-p t)
             (setf remaining (rest remaining)))
            ((and (not end-of-options-p)
                  (stringp token)
                  (> (length token) 1)
                  (char= (char token 0) #\-))
             (push token options)
             (setf remaining (rest remaining))
             (when (and remaining
                        (%git-push-option-consumes-next-p token)
                        (not (search "=" token)))
               (setf remaining (rest remaining))))
            (t
             (push token positionals)
             (setf remaining (rest remaining))))))
      (let* ((ordered-positionals (nreverse positionals))
             (remote (first ordered-positionals))
             (refspecs (rest ordered-positionals)))
        (list :remote remote
              :refspecs refspecs
              :options (nreverse options)
              :implicit-p (null refspecs))))))

(defun %normalize-git-push-refspec-branch (refspec)
  (let* ((raw (%normalize-worktree-string refspec))
         (clean (and raw
                     (if (and (> (length raw) 0)
                              (char= #\+ (char raw 0)))
                         (subseq raw 1)
                         raw))))
    (when clean
      (let ((colon (position #\: clean :from-end t)))
        (cond
          (colon
           (let ((destination (subseq clean (1+ colon))))
             (cond
               ((zerop (length destination)) :delete)
               ((string= destination "HEAD") nil)
               (t (%strip-live-worktree-branch-ref destination)))))
          ((string= clean "HEAD")
           nil)
          (t
           (%strip-live-worktree-branch-ref clean)))))))

(defun %shell-worktree-branch-denial-text (allowed-branch requested-branches)
  (let ((requested (remove-duplicates
                    (remove nil requested-branches)
                    :test #'string=)))
    (format nil
            "Delegated agent ~A may only push branch ~A~@[; attempted ~{~A~^, ~}~]."
            (or (current-delegated-agent-id) "<unknown>")
            allowed-branch
            requested)))

(defun %shell-signal-worktree-branch-denial (allowed-branch requested-branches)
  (let ((reason (%shell-worktree-branch-denial-text
                 allowed-branch
                 requested-branches)))
    (error 'tool-permission-denied
           :tool-name :bash-exec
           :arguments nil
           :reason-code :worktree-branch-scope
           :reason reason
           :message reason)))

(defun %shell-enforce-worktree-branch-scope (context)
  (let ((allowed-branch (current-delegated-agent-push-branch)))
    (when allowed-branch
      (let* ((canonical (canonicalize-permission-command
                         (shell-run-context-policy-command-text context)))
             (segments (and canonical
                            (command-canonical-form-commands canonical))))
        (dolist (segment segments)
          (let ((push-request (%parse-git-push-segment segment)))
            (when push-request
              (let* ((options (getf push-request :options))
                     (refspecs (getf push-request :refspecs))
                     (requested-branches
                       (mapcar #'%normalize-git-push-refspec-branch refspecs)))
                (when (or (getf push-request :implicit-p)
                          (find-if (lambda (option)
                                     (member option
                                             '("--all" "--mirror" "--tags"
                                               "--delete" "-d")
                                             :test #'string=))
                                   options)
                          (null requested-branches)
                          (find :delete requested-branches)
                          (find nil requested-branches)
                          (not (every (lambda (branch)
                                        (string= branch allowed-branch))
                                      requested-branches)))
                  (%shell-signal-worktree-branch-denial
                   allowed-branch
                   requested-branches))))))))))

#+sbcl
(defun %shell-permission-decision (policy-command-text)
  (check-permission :tool :bash-exec
                    :command policy-command-text
                    :dangerous-p (dangerous-command-p policy-command-text)))

#+sbcl
(defun %shell-ensure-execution-permitted (context)
  (let ((policy-command-text (shell-run-context-policy-command-text context)))
	(when (sandbox-read-only-p)
	      (error 'sandbox-violation
	             :operation :bash-exec
	             :reason "sandbox mode read-only denies shell execution"
	             :details (shell-run-context-invocation-text context)))
	    (%shell-enforce-worktree-branch-scope context)
	    (let ((decision (%shell-permission-decision policy-command-text)))
	      (when (and (eq decision :prompt)
	                 (not (%bash-exec-running-under-pipeline-p)))
	        (%shell-signal-prompt-denial decision)))
	    (%assert-permission-allowed :tool :bash-exec
	                                :command policy-command-text
	                                :dangerous-p (dangerous-command-p policy-command-text))))

#+sbcl
(defun %shell-spawn-process (context state)
  (setf (shell-run-state-process state)
        (if (shell-run-context-process-env context)
            (sb-ext:run-program (shell-run-context-resolved-shell context)
                                (list "-lc" (shell-run-context-prepared-command context))
                                :search t
                                :wait nil
                                :directory (shell-run-context-cwd context)
                                :env (shell-run-context-process-env context)
                                :input nil
                                :output :stream
                                :error :stream)
            (sb-ext:run-program (shell-run-context-resolved-shell context)
                                (list "-lc" (shell-run-context-prepared-command context))
                                :search t
                                :wait nil
                                :directory (shell-run-context-cwd context)
                                :input nil
                                :output :stream
                                :error :stream))))

#+sbcl
(defun %shell-start-capture-thread (state context stream-reader setter thread-name)
  (sb-thread:make-thread
   (lambda ()
     (funcall setter
              (multiple-value-list
               (%capture-shell-stream (funcall stream-reader (shell-run-state-process state))
                                      (shell-run-context-max-output-chars context)
                                      (lambda (char)
                                        (%shell-monitor-char state context char))))))
   :name thread-name))

#+sbcl
(defun %shell-start-capture-threads (context state)
  (setf (shell-run-state-stdout-thread state)
        (%shell-start-capture-thread
         state
         context
         #'sb-ext:process-output
         (lambda (values)
           (destructuring-bind (stdout stdout-truncated-p stdout-omitted) values
             (setf (shell-run-state-stdout state) stdout
                   (shell-run-state-stdout-truncated-p state) stdout-truncated-p
                   (shell-run-state-stdout-omitted state) stdout-omitted)))
         "amoebum-shell-stdout-reader"))
  (setf (shell-run-state-stderr-thread state)
        (%shell-start-capture-thread
         state
         context
         #'sb-ext:process-error
         (lambda (values)
           (destructuring-bind (stderr stderr-truncated-p stderr-omitted) values
             (setf (shell-run-state-stderr state) stderr
                   (shell-run-state-stderr-truncated-p state) stderr-truncated-p
                   (shell-run-state-stderr-omitted state) stderr-omitted)))
         "amoebum-shell-stderr-reader")))

#+sbcl
(defun %shell-await-monitored-process (context state)
  (let ((deadline (+ (get-internal-real-time)
                     (* (shell-run-context-timeout-seconds context)
                        internal-time-units-per-second))))
    (loop
      (unless (%shell-process-alive-p (shell-run-state-process state))
        (return))
      (when (>= (get-internal-real-time) deadline)
        (when (%shell-set-termination-cause state :timeout)
          (%terminate-shell-process (shell-run-state-process state)))
        (return))
      (when (member (%shell-current-termination-cause state)
                    '(:output-bytes :output-lines)
                    :test #'eq)
        (return))
      (sleep *shell-process-poll-interval-seconds*))))

#+sbcl
(defun %shell-collect-process-exit (state)
  (unless (%wait-for-shell-process-exit (shell-run-state-process state) 1.0d0)
    (%terminate-shell-process (shell-run-state-process state))
    (%wait-for-shell-process-exit (shell-run-state-process state) 1.0d0)))

#+sbcl
(defun %shell-join-capture-thread (thread)
  (when thread
    (ignore-errors
      (sb-ext:with-timeout 2
        (sb-thread:join-thread thread)))))

#+sbcl
(defun %shell-join-capture-threads (state)
  (%shell-join-capture-thread (shell-run-state-stdout-thread state))
  (%shell-join-capture-thread (shell-run-state-stderr-thread state)))

#+sbcl
(defun %shell-base-result (context state)
  (list :command (shell-run-context-command context)
        :cwd (coerce-path-string (shell-run-context-cwd context))
        :stdout (shell-run-state-stdout state)
        :stderr (shell-run-state-stderr state)
        :output-bytes (shell-run-state-total-output-bytes state)
        :output-lines (shell-run-state-total-output-lines state)
        :output-byte-limit (shell-run-context-max-output-bytes context)
        :output-line-limit (shell-run-context-max-output-lines context)
        :stdout-truncated-p (shell-run-state-stdout-truncated-p state)
        :stderr-truncated-p (shell-run-state-stderr-truncated-p state)
        :stdout-omitted-chars (shell-run-state-stdout-omitted state)
        :stderr-omitted-chars (shell-run-state-stderr-omitted state)))

#+sbcl
(defun %shell-timeout-result (context state)
  (append (list :status :timeout
                :exit-code nil
                :timed-out t
                :runaway-output-p nil
                :runaway-output-reason nil)
          (%shell-base-result context state)))

#+sbcl
(defun %shell-runaway-output-result (context state cause)
  (let ((diagnostic (format nil
                            "Process terminated: output limit exceeded (~A > ~D, bytes=~D lines=~D)."
                            (if (eq cause :output-bytes)
                                "bytes"
                                "lines")
                            (if (eq cause :output-bytes)
                                (shell-run-context-max-output-bytes context)
                                (shell-run-context-max-output-lines context))
                            (shell-run-state-total-output-bytes state)
                            (shell-run-state-total-output-lines state)))
        (result (%shell-base-result context state)))
    (setf (getf result :stderr)
          (%append-diagnostic-line (shell-run-state-stderr state) diagnostic))
    (append (list :status :failed
                  :exit-code nil
                  :timed-out nil
                  :runaway-output-p t
                  :runaway-output-reason (if (eq cause :output-bytes)
                                             :byte-limit
                                             :line-limit))
            result)))

#+sbcl
(defun %shell-completed-result (context state)
  (append (list :status :completed
                :exit-code (ignore-errors
                             (sb-ext:process-exit-code (shell-run-state-process state)))
                :timed-out nil
                :runaway-output-p nil
                :runaway-output-reason nil)
          (%shell-base-result context state)))

#+sbcl
(defun %shell-collect-result (context state)
  (let ((cause (%shell-current-termination-cause state)))
    (cond
      ((eq cause :timeout)
       (%shell-timeout-result context state))
      ((member cause '(:output-bytes :output-lines) :test #'eq)
       (%shell-runaway-output-result context state cause))
      (t
       (%shell-completed-result context state)))))

#+sbcl
(defun %shell-close-process (state)
  (let ((process (shell-run-state-process state)))
    (when process
      (when (%shell-process-alive-p process)
        (%terminate-shell-process process)
        (%wait-for-shell-process-exit process 0.5d0))
      (ignore-errors
        (sb-ext:process-close process)))))

#+sbcl
 (defun %shell-execute-command-phases (command cwd timeout-seconds
                                      max-output-chars max-output-bytes max-output-lines
                                      &key shell-executable profile-files env-vars)
  (let ((context (%make-shell-run-context command
                                          cwd
                                          timeout-seconds
                                          max-output-chars
                                          max-output-bytes
                                          max-output-lines
                                          :shell-executable shell-executable
                                          :profile-files profile-files
                                          :env-vars env-vars))
        (state (make-shell-run-state)))
    (unwind-protect
         (progn
           (%shell-ensure-execution-permitted context)
           (%shell-spawn-process context state)
           (%shell-start-capture-threads context state)
           (%shell-await-monitored-process context state)
           (%shell-collect-process-exit state)
           (%shell-join-capture-threads state)
           (%shell-collect-result context state))
      (%shell-close-process state))))

(defun %run-shell-command (command cwd timeout-seconds max-output-chars
                           &key shell-executable profile-files env-vars
                             max-output-bytes max-output-lines)
  (let ((max-output-bytes* (%normalize-max-output-bytes max-output-bytes))
        (max-output-lines* (%normalize-max-output-lines max-output-lines)))
    #+sbcl
    (%shell-execute-command-phases command
                                   cwd
                                   timeout-seconds
                                   max-output-chars
                                   max-output-bytes*
                                   max-output-lines*
                                   :shell-executable shell-executable
                                   :profile-files profile-files
                                   :env-vars env-vars)
    #-sbcl
    (let* ((resolved-shell (or shell-executable "bash"))
           (process-env (%effective-process-env env-vars))
           (prepared-command (if profile-files
                                 (wrap-command-with-shell-profile-init
                                  command profile-files)
                                 command)))
      (handler-case
          (multiple-value-bind (stdout stderr exit-code)
              (safe-run-program (list resolved-shell "-lc" prepared-command)
                                :tool :bash-exec
                                :directory cwd
                                :env process-env
                                :ignore-error-status t
                                :output :string
                                :error-output :string)
            (multiple-value-bind (stdout* stdout-truncated-p stdout-omitted)
                (%truncate-output stdout max-output-chars)
              (multiple-value-bind (stderr* stderr-truncated-p stderr-omitted)
                  (%truncate-output stderr max-output-chars)
                (list :status :completed
                      :command command
                      :cwd (coerce-path-string cwd)
                      :stdout stdout*
                      :stderr stderr*
                      :exit-code exit-code
                      :timed-out nil
                      :runaway-output-p nil
                      :runaway-output-reason nil
                      :output-bytes (+ (length (or stdout ""))
                                       (length (or stderr "")))
                      :output-lines (+ (count #\Newline (or stdout ""))
                                       (count #\Newline (or stderr "")))
                      :output-byte-limit max-output-bytes*
                      :output-line-limit max-output-lines*
                      :stdout-truncated-p stdout-truncated-p
                      :stderr-truncated-p stderr-truncated-p
                      :stdout-omitted-chars stdout-omitted
                      :stderr-omitted-chars stderr-omitted))))
        (error (condition)
          (list :status :failed
                :command command
                :cwd (coerce-path-string cwd)
                :stdout ""
                :stderr (princ-to-string condition)
                :exit-code nil
                :timed-out nil
                :runaway-output-p nil
                :runaway-output-reason nil
                :output-bytes 0
                :output-lines 0
                :output-byte-limit max-output-bytes*
                :output-line-limit max-output-lines*
                :stdout-truncated-p nil
                :stderr-truncated-p nil
                :stdout-omitted-chars 0
                :stderr-omitted-chars 0))))))

;; Permission checking delegated to pipeline chokepoint (%check-permission-or-signal)

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

(defstruct (shell-execution-options
            (:constructor make-shell-execution-options
                (&key shell-executable background-p
                 enable-profile-init-p enable-project-env-p)))
  shell-executable
  background-p
  enable-profile-init-p
  enable-project-env-p)

(defun %decode-shell-execution-options (shell-or-background
                                        init-shell-profile-p
                                        init-project-env-p
                                        background)
  (let ((legacy-call-p (and (null init-shell-profile-p)
                            (null init-project-env-p)
                            (null background))))
    (make-shell-execution-options
     :shell-executable (unless legacy-call-p shell-or-background)
     :background-p (if legacy-call-p shell-or-background background)
     :enable-profile-init-p (and (not legacy-call-p) init-shell-profile-p)
     :enable-project-env-p (and (not legacy-call-p) init-project-env-p))))

(defun %run-shell-command-with-runtime (command directory timeout-seconds
                                        max-output-chars max-output-bytes max-output-lines
                                        resolved-shell profiles env-vars background-p)
  (if background-p
      (%start-background-shell-task command
                                    directory
                                    timeout-seconds
                                    max-output-chars
                                    max-output-bytes
                                    max-output-lines
                                    :shell-executable resolved-shell
                                    :profile-files profiles
                                    :env-vars env-vars)
      (%run-shell-command command
                          directory
                          timeout-seconds
                          max-output-chars
                          :max-output-bytes max-output-bytes
                          :max-output-lines max-output-lines
                          :shell-executable resolved-shell
                          :profile-files profiles
                          :env-vars env-vars)))

(defun %execute-shell-command (command cwd timeout-seconds
                               max-output-chars max-output-bytes max-output-lines
                               &optional shell-or-background
                                 init-shell-profile-p
                                 init-project-env-p
                                 background)
  (let ((options (%decode-shell-execution-options shell-or-background
                                                  init-shell-profile-p
                                                  init-project-env-p
                                                  background)))
    (multiple-value-bind (directory resolved-shell profiles env-vars)
        (%prepare-shell-runtime cwd
                                (shell-execution-options-enable-profile-init-p options)
                                (shell-execution-options-enable-project-env-p options)
                                (shell-execution-options-enable-project-env-p options)
                                (shell-execution-options-shell-executable options))
      (%persist-shell-directory directory)
      (%run-shell-command-with-runtime
       command
       directory
       timeout-seconds
       max-output-chars
       max-output-bytes
       max-output-lines
       resolved-shell
       profiles
       env-vars
       (shell-execution-options-background-p options)))))

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
                    (max-output-bytes (or null integer)
                     :description "Maximum combined stdout/stderr bytes before forced termination"
                     :default nil)
                    (max-output-lines (or null integer)
                     :description "Maximum combined stdout/stderr lines before forced termination"
                     :default nil)
                    (background boolean
                     :description "Run command asynchronously and return task ID"
                     :default nil)
                    (task-id (or null string)
                     :description "Background task ID to poll for completion"
                     :default nil)
                    (list-tasks boolean
                     :description "List background shell tasks"
                     :default nil)
                    (include-finished boolean
                     :description "Include completed tasks when LIST-TASKS is true"
                     :default t)
                    (cleanup-completed boolean
                     :description "Remove completed/failed/timed-out background tasks"
                     :default nil)
                    (include-running boolean
                     :description "Also remove running tasks when CLEANUP-COMPLETED is true"
                     :default nil))
  "Execute shell commands with stdout/stderr capture and background task retrieval."
  (:permission :full-auto)
  (:dangerous t)
  (:category :shell)
  (:timeout 600)
  (let ((mode-count (+ (if command 1 0)
                       (if task-id 1 0)
                       (if list-tasks 1 0)
                       (if cleanup-completed 1 0))))
    (when (> mode-count 1)
      (error "Choose exactly one mode: COMMAND, TASK-ID, LIST-TASKS, or CLEANUP-COMPLETED."))
    (cond
      (cleanup-completed
       (%cleanup-shell-tasks :include-running include-running))
      (list-tasks
       (%list-shell-tasks :include-finished include-finished))
      (task-id
       (%fetch-shell-task task-id))
      ((null command)
       (error "COMMAND is required unless TASK-ID, LIST-TASKS, or CLEANUP-COMPLETED is provided."))
      (t
       (let ((timeout (%normalize-timeout-seconds timeout-seconds))
             (max-output (%normalize-max-output-chars max-output-chars))
             (max-output-bytes* (%normalize-max-output-bytes max-output-bytes))
             (max-output-lines* (%normalize-max-output-lines max-output-lines)))
         (%execute-shell-command (%normalize-command command)
                                 cwd
                                 timeout
                                 max-output
                                 max-output-bytes*
                                 max-output-lines*
                                 nil
                                 t
                                 t
                                 background))))))
