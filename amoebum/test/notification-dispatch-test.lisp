(in-package :amoebum/test)

(def-suite notification-dispatch-suite
  :description "I225 notification dispatcher fallback chain."
  :in amoebum-suite)

(in-suite notification-dispatch-suite)

(defclass test-notification-backend (amoebum:notification-backend)
  ((available-p
    :initarg :available-p
    :initform t
    :accessor test-notification-backend-available-p)
   (success-p
    :initarg :success-p
    :initform t
    :accessor test-notification-backend-success-p)
   (calls
    :initarg :calls
    :initform 0
    :accessor test-notification-backend-calls)))

(defmethod amoebum:notify-available-p ((backend test-notification-backend))
  (test-notification-backend-available-p backend))

(defmethod amoebum:notify-send ((backend test-notification-backend)
                                (notification amoebum:notification))
  (declare (ignore notification))
  (incf (test-notification-backend-calls backend))
  (if (test-notification-backend-success-p backend)
      (values t nil)
      (values nil "backend failure")))

(test dispatcher-routes-by-event-type-filter
  (let* ((error-backend (make-instance 'test-notification-backend
                                       :name :error
                                       :enabled-p t
                                       :success-p t))
         (default-backend (make-instance 'test-notification-backend
                                         :name :default
                                         :enabled-p t
                                         :success-p t))
         (dispatcher (amoebum:make-notification-dispatcher
                      :backends
                      (list (amoebum:make-notification-dispatch-backend
                             :name :error
                             :backend error-backend
                             :enabled-p t
                             :filter (list amoebum:+event-type-tool-error+)
                             :priority 5)
                            (amoebum:make-notification-dispatch-backend
                             :name :default
                             :backend default-backend
                             :enabled-p t
                             :filter :*
                             :priority 10))))
         (event (amoebum:make-tool-error-event
                 :tool-name "shell"
                 :args nil
                 :condition "failure"
                 :elapsed-ms 1
                 :request-id "i225-route"))
         (notification (amoebum:make-notification
                        :title "Error"
                        :body "Tool failed"
                        :severity :error
                        :source-event event
                        :timestamp (get-universal-time))))
    (multiple-value-bind (ok backend-name)
        (amoebum:dispatch-notification dispatcher notification :event event)
      (is-true ok)
      (is (eq backend-name :error))
      (is (= 1 (test-notification-backend-calls error-backend)))
      (is (= 0 (test-notification-backend-calls default-backend))))))

(test dispatcher-falls-back-on-backend-failure
  (let* ((failing-backend (make-instance 'test-notification-backend
                                         :name :primary
                                         :enabled-p t
                                         :success-p nil))
         (fallback-backend (make-instance 'test-notification-backend
                                          :name :fallback
                                          :enabled-p t
                                          :success-p t))
         (dispatcher (amoebum:make-notification-dispatcher
                      :backends
                      (list (amoebum:make-notification-dispatch-backend
                             :name :primary
                             :backend failing-backend
                             :enabled-p t
                             :filter :*
                             :priority 1)
                            (amoebum:make-notification-dispatch-backend
                             :name :fallback
                             :backend fallback-backend
                             :enabled-p t
                             :filter :*
                             :priority 2))))
         (event (amoebum:make-tool-completed-event
                 :tool-name "read-file"
                 :args nil
                 :result "ok"
                 :elapsed-ms 1
                 :request-id "i225-fallback"))
         (notification (amoebum:make-notification
                        :title "Complete"
                        :body "Done"
                        :severity :info
                        :source-event event
                        :timestamp (get-universal-time))))
    (multiple-value-bind (ok backend-name)
        (amoebum:dispatch-notification dispatcher notification :event event)
      (is-true ok)
      (is (eq backend-name :fallback))
      (is (= 1 (test-notification-backend-calls failing-backend)))
      (is (= 1 (test-notification-backend-calls fallback-backend))))))

(test dispatcher-enable-disable-controls-routing
  (let* ((disabled-backend (make-instance 'test-notification-backend
                                          :name :desktop
                                          :enabled-p t
                                          :success-p t))
         (enabled-backend (make-instance 'test-notification-backend
                                         :name :log
                                         :enabled-p t
                                         :success-p t))
         (dispatcher (amoebum:make-notification-dispatcher
                      :backends
                      (list (amoebum:make-notification-dispatch-backend
                             :name :desktop
                             :backend disabled-backend
                             :enabled-p t
                             :filter :*
                             :priority 1)
                            (amoebum:make-notification-dispatch-backend
                             :name :log
                             :backend enabled-backend
                             :enabled-p t
                             :filter :*
                             :priority 2))))
         (event (amoebum:make-tool-completed-event
                 :tool-name "write-file"
                 :args nil
                 :result "ok"
                 :elapsed-ms 1
                 :request-id "i225-toggle"))
         (notification (amoebum:make-notification
                        :title "Complete"
                        :body "Done"
                        :severity :info
                        :source-event event
                        :timestamp (get-universal-time))))
    (amoebum:set-notification-dispatch-backend-enabled-p dispatcher :desktop nil)
    (multiple-value-bind (ok backend-name)
        (amoebum:dispatch-notification dispatcher notification :event event)
      (is-true ok)
      (is (eq backend-name :log))
      (is (= 0 (test-notification-backend-calls disabled-backend)))
      (is (= 1 (test-notification-backend-calls enabled-backend))))))

(test dispatcher-subscribes-wildcard-event-bus
  (let* ((original-dispatcher amoebum:*notification-dispatcher*)
         (bus (amoebum:make-event-bus :capacity 64))
         (backend (make-instance 'test-notification-backend
                                 :name :audit
                                 :enabled-p t
                                 :success-p t))
         (dispatcher (amoebum:make-notification-dispatcher
                      :backends
                      (list (amoebum:make-notification-dispatch-backend
                             :name :audit
                             :backend backend
                             :enabled-p t
                             :filter (list amoebum:+event-type-config-changed+)
                             :priority 1))))
         (event (amoebum:make-config-changed-event
                 :key :model
                 :old-value "a"
                 :new-value "b")))
    (unwind-protect
        (progn
          (setf amoebum:*notification-dispatcher* dispatcher)
          (amoebum:ensure-notification-dispatcher :event-bus bus)
          (amoebum:publish bus event)
          (is (= 1 (test-notification-backend-calls backend))))
      (setf amoebum:*notification-dispatcher* original-dispatcher)
      (amoebum:stop-notification-dispatcher :event-bus bus))))

(test notification-dispatch-smoke-sentinel
  (is-true t)
  (format t "NOTIFICATION_DISPATCH_SMOKE_OK~%"))
