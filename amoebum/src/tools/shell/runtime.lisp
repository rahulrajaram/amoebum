(in-package :amoebum)

;;;; ---------------------------------------------------------------------------
;;;; Shell tool: monitored runtime execution.
;;;;
;;;; Owns the security-critical hot path:
;;;;   * permission check at the command boundary
;;;;     (sandbox guard, branch-scope enforcement, allowlist decision)
;;;;   * process spawn via SBCL `run-program`
;;;;   * stdout/stderr capture threads with byte/line monitor
;;;;   * timeout enforcement (SIGTERM -> grace -> SIGKILL kill chain)
;;;;   * output-truncation budget enforcement (byte and line caps)
;;;;   * result collection (timeout / runaway-output / completed)
;;;;
;;;; All function names, signatures, and step ordering preserve the original
;;;; behavior in `amoebum/src/tools/shell.lisp` verbatim.  The
;;;; non-SBCL fallback `%run-shell-command` (and the SBCL phase orchestrator
;;;; `%shell-execute-command-phases`) live here since they implement the
;;;; runtime contract observed by callers in `tools/shell.lisp` (synchronous
;;;; invocation) and `tools/shell/background.lisp` (async invocation).
;;;; ---------------------------------------------------------------------------

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
