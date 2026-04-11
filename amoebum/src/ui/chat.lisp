(in-package :amoebum)

;;; Forward declaration - defined in prompt-input.lisp which is loaded before this file
(declaim (ftype function chat-panel-handle-input-key))

;;; NXT-278: chat-ui-state struct, chat constants, and low-level state helpers
;;; live in src/ui/chat-state.lisp, which loads immediately before this file.
;;; Both files share the :amoebum package, so call sites are unchanged.

;;; The residual chat.lisp file intentionally keeps only the runtime
;;; lifecycle shell and top-level entrypoint. Low-level state/input/render/
;;; stream helpers now live in chat-state.lisp, chat-input.lisp,
;;; chat-render.lisp, and chat-stream.lisp.

(defun %make-demo-chat-state ()
  (make-chat-ui-state :stream-runner #'demo-stream-runner
                      :demo-mode-p t
                      :stream-client nil))

(defun %resolve-chat-launch-state (&key initial-state demo)
  (cond
    (demo
     (%make-demo-chat-state))
    (initial-state
     initial-state)
    (t
     (chat-ui-restore-latest-session (make-chat-ui-state)))))

(defun %chat-log-path ()
  (merge-pathnames "runtime/ptui.log"
                   (ensure-directories-exist
                    (merge-pathnames ".amoebum/"
                                     (user-homedir-pathname)))))

(defun %open-chat-log-stream ()
  (let ((log-path (%chat-log-path)))
    (ignore-errors
      (ensure-directories-exist log-path)
      (open log-path
            :direction :output
            :if-exists :append
            :if-does-not-exist :create))))

(defun %maybe-checkpoint-chat-session (chat-state demo)
  (when (and chat-state (not demo))
    (ignore-errors
      (let ((conversation (chat-ui-state-conversation chat-state)))
        (when conversation
          (checkpoint-session :conversation conversation
                              :trigger :exit
                              :auto-p t))))))

(defun %call-with-chat-runtime-shell (thunk &key demo)
  (load-user-extensions)
  (setf *approval-ui-active-p* t)
  (let ((chat-state nil)
        (log-stream nil)
        (heap-monitor-stop-p nil)
        (heap-monitor-thread nil))
    (unwind-protect
        (progn
          ;; Redirect ptui log output to a file so it doesn't corrupt the TUI
          (setf log-stream (%open-chat-log-stream)
                ptui.util.log:*log-output* log-stream)
          ;; Start periodic heap monitor (every 30s -> runtime.log)
          (enable-gc-telemetry)
          (setf heap-monitor-thread
                (bt:make-thread
                 (lambda ()
                   (loop until heap-monitor-stop-p
                         do (ignore-errors
                              (let ((mem (memory-statistics)))
                                (log-runtime-event
                                 :level :info
                                 :kind "heap-snapshot"
                                 :source :profiler
                                 :message "Periodic heap snapshot."
                                 :details
                                 (list :dynamic-usage-mb
                                       (round (getf mem :dynamic-usage-mb) 0.1)
                                       :gc-run-time-s
                                       (getf mem :gc-run-time)
                                       :message-count
                                       (if chat-state
                                           (length (chat-ui-state-messages chat-state))
                                           0)))))
                            (%chat-sleep-until-stop
                             (lambda ()
                               heap-monitor-stop-p)
                             :seconds +heap-monitor-snapshot-interval-seconds+
                             :poll-seconds +heap-monitor-stop-poll-seconds+)))
                 :name "heap-monitor"))
          (setf chat-state (funcall thunk))
          chat-state)
      (setf heap-monitor-stop-p t)
      (when heap-monitor-thread
        (ignore-errors (bt:join-thread heap-monitor-thread)))
      (disable-gc-telemetry)
      (setf *approval-ui-active-p* nil)
      (setf ptui.util.log:*log-output* nil)
      (when log-stream
        (ignore-errors (close log-stream)))
      (%maybe-checkpoint-chat-session chat-state demo))))

(defun run-chat-ui (&key (backend :auto) (fps 20) initial-state demo)
  (let ((*session-persistence-enabled* (and *session-persistence-enabled*
                                            (not demo))))
    (let ((resolved-state (%resolve-chat-launch-state
                           :initial-state initial-state
                           :demo demo)))
      (checkpoint-mark-activity)
      (%chat-mark-activity)
      (%call-with-chat-runtime-shell
       (lambda ()
         (let ((chat-state (ensure-chat-ui-state resolved-state)))
           (ptui.engine.loop:run #'render-chat-ui-buffer
                                 :backend backend
                                 :fps fps
                                 :initial-state chat-state
                                 :event-bus (current-event-bus)
                                 :on-event #'handle-chat-ui-event)
           chat-state))
       :demo demo))))
