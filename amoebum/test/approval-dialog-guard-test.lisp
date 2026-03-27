(in-package :amoebum/test)

(def-suite approval-dialog-guard-suite
  :description "Approval dialog safety tests."
  :in amoebum-suite)

(in-suite approval-dialog-guard-suite)

(defmacro with-approval-log-environment ((runtime-log crash-log) &body body)
  `(let ((original-runtime-log (uiop:getenv "AMOEBUM_RUNTIME_LOG_FILE"))
         (original-crash-log (uiop:getenv "AMOEBUM_CRASH_LOG_FILE")))
     (unwind-protect
          (progn
            (setf (uiop:getenv "AMOEBUM_RUNTIME_LOG_FILE")
                  (and ,runtime-log (namestring ,runtime-log)))
            (setf (uiop:getenv "AMOEBUM_CRASH_LOG_FILE")
                  (and ,crash-log (namestring ,crash-log)))
            ,@body)
       (setf (uiop:getenv "AMOEBUM_RUNTIME_LOG_FILE") (or original-runtime-log ""))
       (setf (uiop:getenv "AMOEBUM_CRASH_LOG_FILE") (or original-crash-log ""))
       (setf amoebum::*approval-ui-active-p* nil)
       (bt:with-lock-held (amoebum::*pending-approval-lock*)
         (setf amoebum::*pending-approval* nil)))))

(defun %approval-log-lines (path)
  (with-open-file (stream path :direction :input)
    (loop for line = (read-line stream nil nil)
          while line
          collect line)))

(defun %approval-log-entries (path)
  (with-open-file (stream path :direction :input)
    (loop for line = (read-line stream nil nil)
          while line
          collect (jonathan:parse line :as :hash-table))))

(defun %approval-wait-until (predicate &key (timeout-seconds 1.0d0) (sleep-seconds 0.01d0))
  (let ((deadline (+ (get-internal-real-time)
                     (round (* timeout-seconds internal-time-units-per-second)))))
    (loop
      when (funcall predicate) do (return t)
      when (>= (get-internal-real-time) deadline) do (return nil)
      do (sleep sleep-seconds))))

(defun %approval-test-chat-state ()
  (let ((*default-pathname-defaults*
          (pathname "/home/rahul/Documents/amoebum/"))
        (amoebum::*current-config* nil))
    (let ((state (amoebum.ui:make-chat-ui-state)))
      (setf (amoebum::chat-ui-state-tree-browser-state state)
            (amoebum::make-empty-tree-browser-state :label "files"))
      (setf (amoebum::tree-browser-state-active-p
             (amoebum::chat-ui-state-tree-browser-state state))
            nil)
      state)))

(defun %render-approval-test-ui (state &key (cols 84) (rows 20))
  (let ((*default-pathname-defaults*
          (pathname "/home/rahul/Documents/amoebum/"))
        (amoebum::*current-config* nil))
    (amoebum:render-chat-ui-buffer
     state
     (ptui.core.types:make-size cols rows))))

(defun %approval-buffer-lines (buffer)
  (let ((cols (ptui.core.types:cell-buffer-cols buffer))
        (rows (ptui.core.types:cell-buffer-rows buffer))
        (cells (ptui.core.types:cell-buffer-cells buffer)))
    (loop for row from 0 below rows
          collect
          (string-right-trim
           '(#\Space)
           (with-output-to-string (out)
             (loop for col from 0 below cols
                   for index = (+ col (* row cols))
                   for cell = (svref cells index)
                   for glyph = (ptui.core.types:cell-glyph cell)
                   do (when (plusp (length glyph))
                        (write-string glyph out))))))))

(test approval-timeout-deny-is-explicit
  (let* ((tmp-root (%make-temp-directory "amoebum-approval-timeout"))
         (runtime-log (merge-pathnames #P"runtime/runtime.log" tmp-root))
         (crash-log (merge-pathnames #P"runtime/crash.log" tmp-root)))
    (unwind-protect
        (with-approval-log-environment (runtime-log crash-log)
          (let ((amoebum::*approval-ui-active-p* t)
                (amoebum::+approval-wait-timeout-seconds+ 0)
                (amoebum::+approval-wait-poll-seconds+ 0.01))
            (let ((pending (amoebum:wait-for-pending-approval
                            "bash-exec"
                            '(:command "rm -rf /tmp/test")
                            :command "rm -rf /tmp/test"
                            :reason "dangerous command")))
              (is (eq :deny (amoebum:pending-approval-decision pending)))
              (is (eq :timeout (amoebum::pending-approval-decision-source pending)))
              (is (null amoebum::*pending-approval*))
              (is-true (some (lambda (line)
                               (search "approval-timeout-deny" line :test #'char-equal))
                             (%approval-log-lines runtime-log))))))
      (%delete-directory-tree-safe tmp-root))))

(test approval-timeout-env-override-is-respected
  (let ((original-timeout-env (uiop:getenv "AMOEBUM_APPROVAL_WAIT_TIMEOUT_SECONDS"))
        (original-poll-env (uiop:getenv "AMOEBUM_APPROVAL_WAIT_POLL_SECONDS")))
    (unwind-protect
        (progn
          (setf (uiop:getenv "AMOEBUM_APPROVAL_WAIT_TIMEOUT_SECONDS") "0")
          (setf (uiop:getenv "AMOEBUM_APPROVAL_WAIT_POLL_SECONDS") "0.05")
          (is (= 0 (amoebum::%approval-wait-timeout-seconds)))
          (is (< (abs (- (amoebum::%approval-wait-poll-seconds) 0.05d0))
                 1d-6)))
      (setf (uiop:getenv "AMOEBUM_APPROVAL_WAIT_TIMEOUT_SECONDS")
            (or original-timeout-env ""))
      (setf (uiop:getenv "AMOEBUM_APPROVAL_WAIT_POLL_SECONDS")
            (or original-poll-env "")))))

(test approval-key-handler-fails-closed
  (let* ((tmp-root (%make-temp-directory "amoebum-approval-key"))
         (runtime-log (merge-pathnames #P"runtime/runtime.log" tmp-root))
         (crash-log (merge-pathnames #P"runtime/crash.log" tmp-root))
         (original-confirm (symbol-function 'amoebum:approval-dialog-confirm!)))
    (unwind-protect
        (with-approval-log-environment (runtime-log crash-log)
          (let ((dialog (amoebum:make-approval-dialog-state :active-p t)))
            (amoebum:approval-dialog-activate! dialog
                                               "bash-exec"
                                               :command "rm -rf /tmp/test"
                                               :decision-id "key-001")
            (bt:with-lock-held (amoebum::*pending-approval-lock*)
              (setf amoebum::*pending-approval*
                    (amoebum::%make-pending-approval
                     :tool-name "bash-exec"
                     :arguments '(:command "rm -rf /tmp/test")
                     :command "rm -rf /tmp/test"
                     :reason "dangerous command"
                     :decision-id "key-001")))
            (setf (symbol-function 'amoebum:approval-dialog-confirm!)
                  (lambda (&rest _args)
                    (declare (ignore _args))
                    (error "confirm boom")))
            (is-true (amoebum:approval-dialog-handle-key! dialog :enter))
            (is (not (amoebum:approval-dialog-state-active-p dialog)))
            (is (eq :deny
                    (amoebum:pending-approval-decision amoebum::*pending-approval*)))
            (is (eq :ui-error
                    (amoebum::pending-approval-decision-source amoebum::*pending-approval*)))
            (is-true (probe-file crash-log))))
      (setf (symbol-function 'amoebum:approval-dialog-confirm!) original-confirm)
      (%delete-directory-tree-safe tmp-root))))

(test approval-key-handler-left-and-right-move-selection
  (let ((dialog (amoebum:make-approval-dialog-state :active-p t)))
    (amoebum:approval-dialog-activate! dialog "bash-exec" :decision-id "move-001")
    (is-true (amoebum:approval-dialog-handle-key! dialog :right))
    (is (eq :deny (amoebum:approval-dialog-state-selected-option dialog)))
    (is-true (amoebum:approval-dialog-handle-key! dialog :left))
    (is (eq :approve (amoebum:approval-dialog-state-selected-option dialog)))))

(test approval-render-failure-falls-back-safely
  (let* ((tmp-root (%make-temp-directory "amoebum-approval-render"))
         (runtime-log (merge-pathnames #P"runtime/runtime.log" tmp-root))
         (crash-log (merge-pathnames #P"runtime/crash.log" tmp-root))
         (original-maker (symbol-function 'amoebum:make-approval-dialog-widget)))
    (unwind-protect
        (with-approval-log-environment (runtime-log crash-log)
          (let* ((state (%approval-test-chat-state))
                 (dialog (amoebum::chat-ui-state-approval-dialog-state state)))
            (amoebum:chat-ui-add-message state "assistant" "I need approval to run this tool.")
            (amoebum:approval-dialog-activate! dialog
                                               "bash-exec"
                                               :command "rm -rf /tmp/test"
                                               :decision-id "render-001")
            (bt:with-lock-held (amoebum::*pending-approval-lock*)
              (setf amoebum::*pending-approval*
                    (amoebum::%make-pending-approval
                     :tool-name "bash-exec"
                     :arguments '(:command "rm -rf /tmp/test")
                     :command "rm -rf /tmp/test"
                     :reason "dangerous command"
                     :decision-id "render-001")))
            (setf (symbol-function 'amoebum:make-approval-dialog-widget)
                  (lambda (&rest _args)
                    (declare (ignore _args))
                    (error "render boom")))
            (let ((widget (amoebum::%chat-approval-dialog-widget state dialog)))
              (is-true widget)
              (is (not (amoebum:approval-dialog-state-active-p dialog)))
              (is (eq :deny
                      (amoebum:pending-approval-decision amoebum::*pending-approval*)))
              (is (eq :ui-error
                      (amoebum::pending-approval-decision-source amoebum::*pending-approval*)))
              (is-true (probe-file crash-log))
              (is-true
               (some (lambda (entry)
                       (string= "approval-ui-error" (gethash "kind" entry)))
                     (%approval-log-entries crash-log))))))
      (setf (symbol-function 'amoebum:make-approval-dialog-widget) original-maker)
      (%delete-directory-tree-safe tmp-root))))

(test approval-panel-render-cycle-falls-back-safely
  (let* ((tmp-root (%make-temp-directory "amoebum-approval-panel-cycle"))
         (runtime-log (merge-pathnames #P"runtime/runtime.log" tmp-root))
         (crash-log (merge-pathnames #P"runtime/crash.log" tmp-root))
         (original-panel-widget (symbol-function 'amoebum::%chat-approval-dialog-widget))
         (attempt-count 0))
    (unwind-protect
        (with-approval-log-environment (runtime-log crash-log)
          (let* ((state (%approval-test-chat-state))
                 (dialog (amoebum::chat-ui-state-approval-dialog-state state))
                 (worker-result nil))
            (setf (symbol-function 'amoebum::%chat-approval-dialog-widget)
                  (lambda (chat-state approval-state)
                    (incf attempt-count)
                    (if (= attempt-count 1)
                        (error "panel render boom")
                        (funcall original-panel-widget
                                 chat-state
                                 approval-state))))
            (amoebum:chat-ui-add-message state "assistant"
                                         "I need approval to run this tool.")
            (setf amoebum::*approval-ui-active-p* t)
            (let ((worker (bt:make-thread
                           (lambda ()
                             (setf worker-result
                                   (amoebum:wait-for-pending-approval
                                    "bash-exec"
                                    '(:command "rm -rf /tmp/test")
                                    :command "rm -rf /tmp/test"
                                    :reason "destructive command"
                                    :decision-id "panel-render-cycle-001")))
                           :name "approval-panel-render-cycle-test")))
              (unwind-protect
                  (progn
                    (is-true
                     (%approval-wait-until
                      (lambda ()
                        (bt:with-lock-held (amoebum::*pending-approval-lock*)
                          (not (null amoebum::*pending-approval*))))
                      :timeout-seconds 1.5d0))
                    (let ((rendered (%render-approval-test-ui state :cols 84 :rows 20)))
                      (is-true rendered))
                    (bt:join-thread worker)
                    (is-false (amoebum:approval-dialog-state-active-p dialog))
                    (is (null amoebum::*pending-approval*))
                    (is (eq :deny
                            (amoebum:pending-approval-decision worker-result)))
                    (is (eq :ui-error
                            (amoebum::pending-approval-decision-source worker-result)))
                    (is (<= 1 attempt-count 2))
                    (is-true
                     (some (lambda (line)
                             (search "Approval dialog failed during render-cycle" line
                                     :test #'char-equal))
                           (%approval-log-lines runtime-log)))
                    (is-true
                     (some (lambda (entry)
                             (string= "approval-ui-error" (gethash "kind" entry)))
                           (%approval-log-entries crash-log))))
                (ignore-errors
                  (when (bt:thread-alive-p worker)
                    (bt:destroy-thread worker)))))))
      (setf (symbol-function 'amoebum::%chat-approval-dialog-widget)
            original-panel-widget)
      (%delete-directory-tree-safe tmp-root))))
(test approval-ctrl-c-cancels-active-request
  (let* ((tmp-root (%make-temp-directory "amoebum-approval-cancel"))
         (runtime-log (merge-pathnames #P"runtime/runtime.log" tmp-root))
         (crash-log (merge-pathnames #P"runtime/crash.log" tmp-root)))
    (unwind-protect
        (with-approval-log-environment (runtime-log crash-log)
          (let* ((state (%approval-test-chat-state))
                 (stream-state (amoebum::chat-ui-state-stream-state state))
                 (worker-result nil))
            (setf (amoebum::token-stream-state-status stream-state) :running
                  (amoebum::token-stream-state-cancel-requested-p stream-state) nil
                  amoebum::*approval-ui-active-p* t)
            (let ((worker (bt:make-thread
                           (lambda ()
                             (setf worker-result
                                   (amoebum:wait-for-pending-approval
                                    "bash-exec"
                                    '(:command "ls -la")
                                    :command "ls -la"
                                    :reason "interactive approval"
                                    :decision-id "cancel-001"
                                    :cancel-thunk (lambda ()
                                                    (amoebum:token-stream-cancel-requested-p
                                                     stream-state)))))
                           :name "approval-cancel-test")))
              (unwind-protect
                  (progn
                    (is-true
                     (%approval-wait-until
                      (lambda ()
                        (bt:with-lock-held (amoebum::*pending-approval-lock*)
                          (not (null amoebum::*pending-approval*))))
                      :timeout-seconds 1.5d0))
                    (multiple-value-bind (_state disposition)
                        (amoebum.ui:handle-chat-ui-event
                         state
                         (ptui.core.events:make-key-event :ctrl-c :ctrlp t))
                      (declare (ignore _state))
                      (is (eq :consume disposition)))
                    (bt:join-thread worker)
                    (is-true (amoebum:token-stream-cancel-requested-p stream-state))
                    (is (eq :deny
                            (amoebum:pending-approval-decision worker-result)))
                    (is (eq :cancelled
                            (amoebum::pending-approval-decision-source worker-result)))
                    (is (null amoebum::*pending-approval*))
                    (is-true (some (lambda (line)
                                     (search "approval-cancel-deny" line
                                             :test #'char-equal))
                                   (%approval-log-lines runtime-log)))
                    (is (not (probe-file crash-log))))
                (ignore-errors
                  (when (bt:thread-alive-p worker)
                    (bt:destroy-thread worker)))))))
      (%delete-directory-tree-safe tmp-root))))

(test stream-ctrl-c-consumes-event-during-active-stream
  (let ((state (%approval-test-chat-state)))
    (let ((stream-state (amoebum::chat-ui-state-stream-state state)))
      (setf (amoebum::token-stream-state-status stream-state) :running
            (amoebum::token-stream-state-cancel-requested-p stream-state) nil)
      (multiple-value-bind (updated-state disposition)
          (amoebum.ui:handle-chat-ui-event
           state
           (ptui.core.events:make-key-event :ctrl-c :ctrlp t))
        (is (eq state updated-state))
        (is (eq :consume disposition))
        (is-true (amoebum:token-stream-cancel-requested-p stream-state))))))

(test idle-ctrl-c-requires-second-press-to-exit
  (let ((state (%approval-test-chat-state)))
    (multiple-value-bind (updated-state first-disposition)
        (amoebum.ui:handle-chat-ui-event
         state
         (ptui.core.events:make-key-event :ctrl-c :ctrlp t))
      (is (eq state updated-state))
      (is (eq :consume first-disposition))
      (is-true (amoebum::%chat-exit-warning-active-p state))
      (is-true
       (some (lambda (line)
               (search "Press Ctrl-C again within 1.5 seconds to exit."
                       line
                       :test #'char-equal))
             (%approval-buffer-lines (%render-approval-test-ui state :cols 84 :rows 20)))))
    (multiple-value-bind (updated-state second-disposition)
        (amoebum.ui:handle-chat-ui-event
         state
         (ptui.core.events:make-key-event :ctrl-c :ctrlp t))
      (is (eq state updated-state))
      (is (eq :quit second-disposition))
      (is-false (amoebum::%chat-exit-warning-active-p state)))))

(test heap-monitor-wait-stops-promptly-after-shutdown-request
  (let ((stop-p nil)
        (stop-thread nil))
    (unwind-protect
        (progn
          (setf stop-thread
                (bt:make-thread
                 (lambda ()
                   (sleep 0.05d0)
                   (setf stop-p t))
                 :name "heap-monitor-stop-test"))
          (let* ((started (get-internal-real-time))
                 (stopped (amoebum::%chat-sleep-until-stop
                           (lambda ()
                             stop-p)
                           :seconds 1.0d0
                           :poll-seconds 0.01d0))
                 (elapsed (/ (- (get-internal-real-time) started)
                             internal-time-units-per-second)))
            (is-true stopped)
            (is (< elapsed 0.5d0))))
      (when stop-thread
        (ignore-errors (bt:join-thread stop-thread))))))

(test text-q-is-routed-without-triggering-quit
  (let ((state (%approval-test-chat-state)))
    (multiple-value-bind (updated-state disposition)
        (amoebum.ui:handle-chat-ui-event
         state
         (ptui.core.events:make-key-event :text :text? "q"))
      (is (eq state updated-state))
      (is (eq :consume disposition))
      (is (string= "q" (amoebum::chat-ui-state-input-text state))))))

(test approval-ctrl-c-consumes-event-while-waiting-for-decision
  (let ((state (%approval-test-chat-state))
        (worker-result nil))
    (setf amoebum::*approval-ui-active-p* t)
    (let ((worker (bt:make-thread
                   (lambda ()
                     (setf worker-result
                           (amoebum:wait-for-pending-approval
                            "bash-exec"
                            '(:command "ls -la")
                            :command "ls -la"
                            :reason "interactive approval"
                            :decision-id "cancel-user-001")))
                   :name "approval-user-deny-test")))
      (unwind-protect
          (progn
            (is-true
             (%approval-wait-until
              (lambda ()
                (bt:with-lock-held (amoebum::*pending-approval-lock*)
                  (not (null amoebum::*pending-approval*))))
              :timeout-seconds 1.5d0))
            (multiple-value-bind (_state disposition)
                (amoebum.ui:handle-chat-ui-event
                 state
                 (ptui.core.events:make-key-event :ctrl-c :ctrlp t))
              (declare (ignore _state))
              (is (eq :consume disposition)))
            (bt:join-thread worker)
            (is (eq :deny
                    (amoebum:pending-approval-decision worker-result)))
            (is (eq :user
                    (amoebum::pending-approval-decision-source worker-result)))
            (is (null amoebum::*pending-approval*)))
        (ignore-errors
          (when (bt:thread-alive-p worker)
            (bt:destroy-thread worker)))))))

(test approval-denial-reason-reflects-decision-source
  (let ((original-wait (symbol-function 'amoebum:wait-for-pending-approval)))
    (unwind-protect
        (dolist (scenario '((:timeout "approval request timed out")
                            (:cancelled "approval was cancelled")
                            (:ui-error "approval dialog failed")
                            (:noninteractive "approval UI was inactive in non-interactive mode")))
          (destructuring-bind (decision-source expected-reason) scenario
            (setf (symbol-function 'amoebum:wait-for-pending-approval)
                  (lambda (tool-name arguments
                           &key path command reason decision-id cancel-thunk)
                    (declare (ignore arguments path command reason cancel-thunk))
                    (let ((pending (amoebum::%make-pending-approval
                                    :tool-name tool-name
                                    :decision-id (or decision-id "approval-source-001"))))
                      (setf (amoebum:pending-approval-decision pending) :deny
                            (amoebum::pending-approval-decision-source pending)
                            decision-source)
                      pending)))
            (handler-case
                (progn
                  (let ((arguments (make-hash-table :test #'equal)))
                    (setf (gethash "command" arguments) "printf ok")
                    (amoebum::%check-permission-or-signal
                     "bash-exec"
                     arguments
                     (amoebum:make-amoebum-context
                      :permission-mode :supervised
                      :initialize-notifications-p nil)))
                  (fail "Expected tool permission denial for source ~S." decision-source))
              (amoebum:tool-permission-denied (condition)
                (is (string= expected-reason
                             (amoebum:tool-error-reason condition)))
                (is-true
                 (search expected-reason
                         (princ-to-string condition)
                         :test #'char-equal))))))
      (setf (symbol-function 'amoebum:wait-for-pending-approval) original-wait))))
