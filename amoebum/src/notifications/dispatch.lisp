(in-package :amoebum)

(defstruct (notification-dispatch-backend
            (:constructor make-notification-dispatch-backend
                (&key name backend
                 (enabled-p t)
                 (filter :*)
                 (priority 100))))
  name
  backend
  (enabled-p t :type boolean)
  (filter :*)
  (priority 100 :type integer))

(defstruct (notification-dispatcher
            (:constructor %make-notification-dispatcher
                (&key (backends '())
                 event-bus
                 subscription-id)))
  (backends '() :type list)
  event-bus
  subscription-id)

(defparameter *notification-dispatcher* nil)

(defun %notification-backend-default-filter (backend-name)
  (case backend-name
    (:desktop (list +event-type-tool-completed+
                    +event-type-tool-error+
                    +event-type-permission-prompted+))
    (:sound (list +event-type-tool-completed+
                  +event-type-tool-error+
                  +event-type-permission-prompted+))
    (:log :*)
    (otherwise :*)))

(defun %notification-backend-default-priority (backend-name)
  (case backend-name
    (:desktop 10)
    (:sound 20)
    (:webhook 30)
    (:log 100)
    (otherwise 50)))

(defun %normalize-dispatch-filter (filter)
  (cond
    ((or (eq filter :*)
         (string= (princ-to-string filter) "*"))
     :*)
    ((null filter)
     '())
    ((listp filter)
     (mapcar #'%normalize-event-type filter))
    ((vectorp filter)
     (map 'list #'%normalize-event-type filter))
    (t
     (list (%normalize-event-type filter)))))

(defun %dispatch-backend-name (entry)
  (or (notification-dispatch-backend-name entry)
      (let ((backend (notification-dispatch-backend-backend entry)))
        (if (and backend (typep backend 'notification-backend))
            (backend-name backend)
            :unknown))))

(defun %dispatch-backend-matches-event-p (entry event)
  (let ((filter (%normalize-dispatch-filter
                 (notification-dispatch-backend-filter entry))))
    (or (eq filter :*)
        (and (event-p event)
             (member (event-type event) filter :test #'eq)))))

(defun %dispatch-backend-available-p (entry)
  (let ((backend (notification-dispatch-backend-backend entry)))
    (if (and backend (typep backend 'notification-backend))
        (notify-available-p backend)
        t)))

(defun %dispatch-backend-send (entry notification)
  (let ((backend (notification-dispatch-backend-backend entry)))
    (if (and backend (typep backend 'notification-backend))
        (notify-send backend notification)
        (values nil :missing-backend))))

(defun %sorted-dispatch-backends (entries)
  (sort (copy-list entries)
        #'<
        :key #'notification-dispatch-backend-priority))

(defun make-notification-dispatcher (&key backends event-bus)
  (%make-notification-dispatcher
   :backends (%sorted-dispatch-backends (or backends '()))
   :event-bus event-bus
   :subscription-id nil))

(defun set-notification-dispatcher-backends (dispatcher backends)
  (check-type dispatcher notification-dispatcher)
  (setf (notification-dispatcher-backends dispatcher)
        (%sorted-dispatch-backends backends))
  dispatcher)

(defun %dispatcher-backends-from-manager (manager)
  (loop for backend in (notification-manager-backends manager)
        collect (make-notification-dispatch-backend
                 :name (backend-name backend)
                 :backend backend
                 :enabled-p (backend-enabled-p backend)
                 :filter (%notification-backend-default-filter
                          (backend-name backend))
                 :priority (%notification-backend-default-priority
                            (backend-name backend)))))

(defun find-notification-dispatch-backend (dispatcher backend-name)
  (check-type dispatcher notification-dispatcher)
  (find (if (keywordp backend-name)
            backend-name
            (intern (string-upcase (princ-to-string backend-name)) :keyword))
        (notification-dispatcher-backends dispatcher)
        :key #'%dispatch-backend-name
        :test #'eq))

(defun set-notification-dispatch-backend-enabled-p (dispatcher backend-name enabled-p)
  (let ((entry (find-notification-dispatch-backend dispatcher backend-name)))
    (unless entry
      (error "Unknown notification backend ~S." backend-name))
    (setf (notification-dispatch-backend-enabled-p entry) (not (null enabled-p)))
    (let ((backend (notification-dispatch-backend-backend entry)))
      (when (and backend (typep backend 'notification-backend))
        (setf (backend-enabled-p backend)
              (notification-dispatch-backend-enabled-p entry))))
    entry))

(defun list-notification-dispatch-backends (&optional (dispatcher *notification-dispatcher*))
  (if (and dispatcher (notification-dispatcher-p dispatcher))
      (copy-list (notification-dispatcher-backends dispatcher))
      '()))

(defun %fallback-notification-from-event (event)
  (make-notification
   :title "Amoebum Event"
   :body (format nil "~A from ~A."
                 (string-downcase (symbol-name (event-type event)))
                 (string-downcase (symbol-name (event-source event))))
   :severity (event-severity event)
   :category :general
   :source-event event
   :timestamp (event-timestamp event)
   :urgency :normal
   :timeout-ms 5000))

(defun dispatch-notification (target notification &key event)
  (cond
    ((notification-manager-p target)
     (dispatch-notification-manager target notification))
    ((notification-dispatcher-p target)
     (let* ((effective-event (or event (notification-source-event notification)))
            (matched nil))
       (dolist (entry (notification-dispatcher-backends target)
                      (values nil :no-match))
         (when (and (notification-dispatch-backend-enabled-p entry)
                    (%dispatch-backend-matches-event-p entry effective-event))
           (setf matched t)
           (handler-case
               (if (%dispatch-backend-available-p entry)
                   (multiple-value-bind (ok detail)
                     (%dispatch-backend-send entry notification)
                     (if ok
                         (return-from dispatch-notification
                           (values t (%dispatch-backend-name entry)))
                         (progn
                           (ptui.util.log:log-warn
                            "notification backend ~A failed: ~A"
                            (%dispatch-backend-name entry)
                            detail)
                           nil)))
                   (ptui.util.log:log-warn
                    "notification backend ~A unavailable, trying fallback."
                    (%dispatch-backend-name entry)))
             (error (condition)
               (ptui.util.log:log-warn
                "notification backend ~A raised error: ~A"
                (%dispatch-backend-name entry)
                condition)))))
       (if matched
           (values nil :all-failed)
           (values nil :no-match))))
    (t
     (error "Unsupported dispatch target ~S." target))))

(defun %notification-dispatch-handler (dispatcher event)
  (let* ((trigger (%event-trigger event))
         (notification (if trigger
                           (%notification-from-event event trigger)
                           (%fallback-notification-from-event event))))
    (dispatch-notification dispatcher notification :event event)))

(defun ensure-notification-dispatcher (&key manager event-bus)
  (let* ((bus (or event-bus
                  (and manager (notification-manager-event-bus manager))
                  (current-event-bus)))
         (backends (cond
                     ((and manager (notification-manager-p manager))
                      (%dispatcher-backends-from-manager manager))
                     ((and *notification-dispatcher*
                           (notification-dispatcher-p *notification-dispatcher*))
                      (notification-dispatcher-backends *notification-dispatcher*))
                     (t
                      '())))
         (dispatcher (or (and *notification-dispatcher*
                              (notification-dispatcher-p *notification-dispatcher*)
                              *notification-dispatcher*)
                         (make-notification-dispatcher :backends backends
                                                       :event-bus bus))))
    (setf (notification-dispatcher-backends dispatcher)
          (%sorted-dispatch-backends backends))
    (setf (notification-dispatcher-event-bus dispatcher) bus)
    (when (notification-dispatcher-subscription-id dispatcher)
      (ignore-errors
        (unsubscribe bus (notification-dispatcher-subscription-id dispatcher))))
    (setf (notification-dispatcher-subscription-id dispatcher)
          (subscribe bus "*"
                     (lambda (event)
                       (%notification-dispatch-handler dispatcher event))
                     :priority 35))
    (setf *notification-dispatcher* dispatcher)))

(defun stop-notification-dispatcher (&key (event-bus nil event-bus-supplied-p))
  (let* ((dispatcher *notification-dispatcher*)
         (bus (cond
                (event-bus-supplied-p event-bus)
                ((and dispatcher (notification-dispatcher-p dispatcher))
                 (notification-dispatcher-event-bus dispatcher))
                (t nil))))
    (when (and dispatcher
               (notification-dispatcher-p dispatcher)
               bus
               (notification-dispatcher-subscription-id dispatcher))
      (ignore-errors
        (unsubscribe bus (notification-dispatcher-subscription-id dispatcher)))
      (setf (notification-dispatcher-subscription-id dispatcher) nil))
    (when (and dispatcher (notification-dispatcher-p dispatcher))
      (setf (notification-dispatcher-event-bus dispatcher) nil))
    dispatcher))

(defun fire-notification-dispatch-test (&key (dispatcher *notification-dispatcher*)
                                          (event-type +event-type-tool-error+)
                                          (title "Dispatcher Test")
                                          (body "Notification dispatch test"))
  (unless (and dispatcher (notification-dispatcher-p dispatcher))
    (error "Notification dispatcher is not initialized."))
  (let* ((event (make-event :type event-type
                            :source :amoebum
                            :severity :info
                            :payload nil))
         (notification (make-notification :title title
                                          :body body
                                          :severity :info
                                          :category :general
                                          :source-event event
                                          :timestamp (get-universal-time))))
    (dispatch-notification dispatcher notification :event event)))
