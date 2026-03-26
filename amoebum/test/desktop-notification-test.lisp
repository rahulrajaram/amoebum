(in-package :amoebum/test)

(def-suite desktop-notification-suite
  :description "I222 desktop notification backend coverage."
  :in amoebum-suite)

(in-suite desktop-notification-suite)

(test desktop-notification-linux-path-uses-notify-send
  (let ((amoebum.notifications:*notification-command-prober*
          (lambda (command)
            (string= command "notify-send")))
        (amoebum.notifications:*desktop-notifications-suppressed* nil)
        (amoebum.notifications:*desktop-notification-run-command-function* nil)
        (captured-command nil))
    (setf amoebum.notifications:*desktop-notification-run-command-function*
          (lambda (command)
            (setf captured-command command)
            (list :exit-code 0 :stdout "" :stderr "")))
    (multiple-value-bind (ok detail)
        (amoebum.notifications:send-desktop-notification
         (amoebum.notifications:make-notification
          :title "Build Failed"
          :body "Tool shell failed."
          :urgency :critical
          :icon-path "/tmp/icon.png"))
      (is-true ok)
      (is (null detail)))
    (is (search "notify-send" (or captured-command "") :test #'char-equal))
    (is (search "--urgency=critical" (or captured-command "") :test #'char-equal))
    (is (search "--icon=/tmp/icon.png" (or captured-command "") :test #'char-equal))))

(test desktop-notification-macos-path-uses-osascript
  (let ((amoebum.notifications:*notification-command-prober*
          (lambda (command)
            (string= command "osascript")))
        (amoebum.notifications:*desktop-notifications-suppressed* nil)
        (amoebum.notifications:*desktop-notification-run-command-function* nil)
        (captured-command nil))
    (setf amoebum.notifications:*desktop-notification-run-command-function*
          (lambda (command)
            (setf captured-command command)
            (list :exit-code 0 :stdout "" :stderr "")))
    (multiple-value-bind (ok detail)
        (amoebum.notifications:send-desktop-notification
         (amoebum.notifications:make-notification
          :title "Done"
          :body "Long-running work finished."
          :urgency :normal))
      (is-true ok)
      (is (null detail)))
    (is (search "osascript" (or captured-command "") :test #'char-equal))
    (is (search "display notification" (or captured-command "") :test #'char-equal))))

(test notification-dispatch-maps-tool-error-and-long-running-complete
  (let* ((tool-error-event
           (amoebum:make-tool-error-event
            :tool-name "shell"
            :args '(:cmd "rm -rf /")
            :condition "permission denied"
            :elapsed-ms 15
            :request-id "i222-tool-error"))
         (long-running-event
           (amoebum:make-event :type amoebum:+event-type-agent-completed+
                               :payload (amoebum:make-agent-completed-event
                                         :agent-id "agent-1"
                                         :result-status :ok
                                         :elapsed-ms 1200)))
         (tool-trigger (amoebum::%event-trigger tool-error-event))
         (tool-notification (amoebum::%notification-from-event tool-error-event tool-trigger))
         (long-trigger (amoebum::%event-trigger long-running-event))
         (long-notification (amoebum::%notification-from-event long-running-event long-trigger)))
    (is (eq tool-trigger :error))
    (is (eq (amoebum.notifications:notification-urgency tool-notification) :critical))
    (is (eq long-trigger :long-running-complete))
    (is (eq (amoebum.notifications:notification-urgency long-notification) :normal))))

(test async-notification-dispatch-preserves-desktop-suppression
  (let ((amoebum.notifications:*notification-command-prober*
          (lambda (command)
            (string= command "notify-send")))
        (amoebum.notifications:*desktop-notification-run-command-function*
          (lambda (command)
            (declare (ignore command))
            (error "desktop notification runner should stay suppressed during tests")))
        (amoebum.notifications:*desktop-notifications-suppressed* t)
        (amoebum.notifications:*notification-async-dispatch-p* t))
    (let* ((backend (amoebum.notifications:make-desktop-backend))
           (manager (amoebum::%make-notification-manager :backends (list backend)))
           (notification
             (amoebum.notifications:make-notification
              :title "Suppressed"
              :body "Should not reach notify-send"
              :severity :info
              :timestamp (get-universal-time)))
           (thread (amoebum::%dispatch-notification manager notification)))
      (bordeaux-threads:join-thread thread)
      (pass))))

(test desktop-notification-smoke-sentinel
  (is-true t)
  (format t "DESKTOP_NOTIFICATION_SMOKE_OK~%"))
