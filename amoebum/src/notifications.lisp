(in-package :amoebum)

(defstruct (notification
            (:constructor make-notification
                (&key
                   (title "")
                   (body "")
                   (severity :info)
                   (category :general)
                   source-event
                   (timestamp 0)
                   (urgency :normal)
                   (icon nil)
                   (icon-path nil)
                   (actions '())
                   (timeout-ms 5000))))
  (title "" :type string)
  (body "" :type string)
  (severity :info :type keyword)
  (category :general :type keyword)
  source-event
  (timestamp 0 :type integer)
  (urgency :normal :type keyword)
  icon
  icon-path
  (actions '() :type list)
  (timeout-ms 5000 :type integer))

(defclass notification-backend ()
  ((name
    :initarg :name
    :reader backend-name
    :type keyword)
   (enabled-p
    :initarg :enabled-p
    :initform t
    :accessor backend-enabled-p
    :type boolean)
   (config
    :initarg :config
    :initform nil
    :accessor backend-config)))

(defclass sound-backend (notification-backend)
  ((player-command
    :initarg :player-command
    :initform nil
    :accessor sound-backend-player-command)
   (theme
    :initarg :theme
    :initform nil
    :accessor sound-backend-theme)
   (sound-map
    :initarg :sound-map
    :initform (make-hash-table :test #'eq)
    :accessor sound-backend-sound-map)))

(defclass desktop-backend (notification-backend)
  ((command
    :initarg :command
    :initform nil
    :accessor desktop-backend-command)
   (app-name
    :initarg :app-name
    :initform "Amoebum"
    :accessor desktop-backend-app-name)
   (default-icon
    :initarg :default-icon
    :initform nil
    :accessor desktop-backend-default-icon)))

(defclass log-backend (notification-backend)
  ((path
    :initarg :path
    :initform nil
    :accessor log-backend-path)
   (lock
    :initarg :lock
    :initform (bordeaux-threads:make-lock "amoebum-audit-log-lock")
    :accessor log-backend-lock)
   (include-event-payload-p
    :initarg :include-event-payload-p
    :initform t
    :accessor log-backend-include-event-payload-p)))

(defgeneric notify-send (backend notification)
  (:documentation "Best-effort dispatch of NOTIFICATION through BACKEND."))

(defgeneric notify-available-p (backend)
  (:documentation "Whether BACKEND can currently dispatch notifications."))

(defgeneric notify-teardown (backend)
  (:documentation "Release backend resources and return NIL."))

(defmethod notify-teardown ((backend notification-backend))
  (declare (ignore backend))
  nil)

(defparameter *notification-command-runner* nil)
(defparameter *notification-command-prober* nil)
(defparameter *notification-async-dispatch-p* t)
(defparameter *notification-manager-registry* (make-hash-table :test #'eq))

(declaim (special *desktop-notification-run-command-function*
                  *desktop-notifications-suppressed*))

;; Implemented in src/notifications/desktop.lisp (I222).
(declaim (ftype function send-desktop-notification))
(declaim (ftype function desktop-notification-available-p))

(defstruct (notification-manager
            (:constructor %make-notification-manager
                (&key event-bus
                 (backends '())
                 (enabled-events '())
                 (subscription-ids '()))))
  event-bus
  (backends '() :type list)
  (enabled-events '() :type list)
  (subscription-ids '() :type list))

(defun %nonblank-string (value)
  (let ((text (and (stringp value)
                   (string-trim '(#\Space #\Tab #\Newline #\Return) value))))
    (and text (plusp (length text)) text)))

(defun %notify-shell-single-quote (text)
  (format nil "'~A'"
          (with-output-to-string (out)
            (loop for char across (or text "") do
              (if (char= char #\')
                  (write-string "'\"'\"'" out)
                  (write-char char out))))))

(defun default-notification-command-prober (command)
  (let ((safe-command (%nonblank-string command)))
    (if (null safe-command)
        nil
        (multiple-value-bind (stdout stderr exit-code)
            (uiop:run-program
             (list "sh" "-lc"
                   (format nil "command -v ~A >/dev/null 2>&1"
                           (%notify-shell-single-quote safe-command)))
             :ignore-error-status t
             :output :string
             :error-output :string)
          (declare (ignore stdout stderr))
          (zerop (or exit-code 1))))))

(defun notification-command-available-p (command)
  (handler-case
      (funcall (or *notification-command-prober*
                   #'default-notification-command-prober)
               command)
    (error ()
      nil)))

(defun default-notification-command-runner (arguments)
  (multiple-value-bind (stdout stderr exit-code)
      (uiop:run-program arguments
                        :ignore-error-status t
                        :output :string
                        :error-output :string)
    (list :exit-code (or exit-code 1)
          :stdout (or stdout "")
          :stderr (or stderr ""))))

(defun %notify-normalize-command-result (result)
  (if (listp result)
      (list :exit-code (or (getf result :exit-code) 1)
            :stdout (or (getf result :stdout) "")
            :stderr (or (getf result :stderr) ""))
      (list :exit-code 1
            :stdout ""
            :stderr (princ-to-string result))))

(defun notification-run-command (arguments)
  (%notify-normalize-command-result
   (funcall (or *notification-command-runner*
                #'default-notification-command-runner)
            arguments)))

(defun %default-log-path ()
  (merge-pathnames #P".amoebum/audit/events.jsonl"
                   (user-homedir-pathname)))

(defun %notification-platform ()
  #+darwin :macos
  #+linux :linux
  #-(or darwin linux) :other)

(defun %detected-sound-player ()
  (let ((candidates
          (case (%notification-platform)
            (:linux '("paplay" "aplay"))
            (:macos '("afplay"))
            (otherwise '()))))
    (find-if #'notification-command-available-p candidates)))

(defun %json-escape (text)
  (with-output-to-string (out)
    (loop for char across (or text "") do
      (case char
        (#\" (write-string "\\\"" out))
        (#\\ (write-string "\\\\" out))
        (#\Backspace (write-string "\\b" out))
        (#\Page (write-string "\\f" out))
        (#\Newline (write-string "\\n" out))
        (#\Return (write-string "\\r" out))
        (#\Tab (write-string "\\t" out))
        (otherwise (write-char char out))))))

(defun %json-string-or-null (value)
  (if value
      (format nil "\"~A\"" (%json-escape (princ-to-string value)))
      "null"))

(defun %notification-json-line (backend notification)
  (let* ((source-event (notification-source-event notification))
         (payload-string
           (and (log-backend-include-event-payload-p backend)
                source-event
                (prin1-to-string (event-payload source-event))))
         (type-string (and source-event (string-downcase (symbol-name (event-type source-event)))))
         (severity-string (string-downcase (symbol-name (notification-severity notification))))
         (urgency-string (string-downcase (symbol-name (notification-urgency notification))))
         (category-string (string-downcase (symbol-name (notification-category notification)))))
    (format nil
            "{\"ts\":~A,\"seq\":~A,\"type\":~A,\"severity\":~A,\"title\":~A,\"body\":~A,\"category\":~A,\"urgency\":~A,\"payload\":~A}"
            (notification-timestamp notification)
            (if (and source-event (integerp (event-seq source-event)))
                (event-seq source-event)
                "null")
            (%json-string-or-null type-string)
            (%json-string-or-null severity-string)
            (%json-string-or-null (notification-title notification))
            (%json-string-or-null (notification-body notification))
            (%json-string-or-null category-string)
            (%json-string-or-null urgency-string)
            (%json-string-or-null payload-string))))

(defmethod notify-available-p ((backend sound-backend))
  (let ((command (or (sound-backend-player-command backend)
                     (%detected-sound-player))))
    (setf (sound-backend-player-command backend) command)
    (and command
         (notification-command-available-p command))))

(defmethod notify-available-p ((backend desktop-backend))
  (desktop-notification-available-p backend))

(defmethod notify-available-p ((backend log-backend))
  (handler-case
      (let ((path (or (log-backend-path backend)
                      (%default-log-path))))
        (setf (log-backend-path backend) path)
        (ensure-directories-exist path)
        t)
    (error ()
      nil)))

(defun %sound-path-string (sound-path)
  (typecase sound-path
    (pathname (namestring sound-path))
    (string sound-path)
    (t (princ-to-string sound-path))))

(defun %sound-backend-theme-name (backend)
  (or (sound-backend-theme backend)
      (active-sound-theme-name)
      :standard))

(defun %sound-path-for-notification (backend notification)
  (let* ((category (notification-category notification))
         (override-map (sound-backend-sound-map backend))
         (cfg (or (backend-config backend) (current-config))))
    (or (and override-map
             (multiple-value-bind (override presentp)
                 (gethash category override-map)
               (and presentp override)))
        (resolve-sound-path (%sound-backend-theme-name backend)
                            category
                            :config cfg))))

(defmethod notify-send ((backend sound-backend) (notification notification))
  (let* ((theme-name (%sound-backend-theme-name backend))
         (sound-path (%sound-path-for-notification backend notification))
         (command (sound-backend-player-command backend)))
    (cond
      ((null sound-path)
       (values nil :no-sound-configured))
      ((not (notify-available-p backend))
       (values nil :backend-unavailable))
      ((null (probe-file sound-path))
       (ptui.util.log:log-warn
        "notification sound missing: theme=~A category=~A path=~A"
        theme-name
        (notification-category notification)
        sound-path)
       (values nil :missing-sound-file))
      (t
       (let* ((sound-path-string (%sound-path-string sound-path))
              (result (notification-run-command (list command sound-path-string)))
              (exit-code (getf result :exit-code))
              (stderr (getf result :stderr)))
         (if (zerop exit-code)
             (values t nil)
             (progn
               (ptui.util.log:log-warn
                "sound backend command failed: command=~A exit=~S stderr=~S"
                command
                exit-code
                stderr)
               (values nil stderr))))))))

(defmethod notify-send ((backend desktop-backend) (notification notification))
  (send-desktop-notification notification :backend backend))

(defmethod notify-send ((backend log-backend) (notification notification))
  (let ((path (or (log-backend-path backend)
                  (%default-log-path))))
    (handler-case
        (progn
          (ensure-directories-exist path)
          (with-open-file (stream path
                                  :direction :output
                                  :if-exists :append
                                  :if-does-not-exist :create)
            (write-line (%notification-json-line backend notification) stream)
            (finish-output stream))
          (values t nil))
      (error (condition)
        (ptui.util.log:log-warn "log backend failed for ~A: ~A" path condition)
        (values nil (princ-to-string condition))))))

(defun %normalize-trigger (value)
  (cond
    ((keywordp value) value)
    ((stringp value) (intern (string-upcase value) :keyword))
    ((symbolp value) (intern (string-upcase (symbol-name value)) :keyword))
    (t nil)))

(defun %normalize-trigger-list (events)
  (remove nil
          (mapcar #'%normalize-trigger
                  (cond
                    ((null events) '())
                    ((listp events) events)
                    ((vectorp events) (coerce events 'list))
                    (t (list events))))))

(defun %config-enabled-events (cfg)
  (if (not (config-value :notifications-enabled cfg))
      '()
      (let ((raw (config-value :notification-events cfg)))
        (if raw
            (%normalize-trigger-list raw)
            '(:task-complete :error :approval-needed :long-running-complete)))))

(defun make-sound-backend (&key config (enabled-p t) player-command sound-map theme)
  (let* ((cfg (or config (current-config)))
         (resolved-enabled (and enabled-p
                                (not (null (config-value :notification-sound-enabled cfg)))))
         (resolved-command (or player-command
                               (config-value :notification-sound-player cfg)))
         (resolved-map (or sound-map
                           (make-hash-table :test #'eq))))
    (make-instance 'sound-backend
                   :name :sound
                   :enabled-p resolved-enabled
                   :config cfg
                   :player-command resolved-command
                   :theme theme
                   :sound-map resolved-map)))

(defun make-desktop-backend (&key config (enabled-p t) command)
  (let* ((cfg (or config (current-config)))
         (resolved-enabled (and enabled-p
                                (not (null (config-value :notification-desktop-enabled cfg)))))
         (resolved-command (or command
                               (config-value :notification-desktop-command cfg))))
    (make-instance 'desktop-backend
                   :name :desktop
                   :enabled-p resolved-enabled
                   :config cfg
                   :command resolved-command
                   :app-name "Amoebum")))

(defun make-log-backend (&key config (enabled-p t) path)
  (let* ((cfg (or config (current-config)))
         (resolved-enabled (and enabled-p
                                (not (null (config-value :notification-log-enabled cfg)))))
         (resolved-path (or path
                            (config-value :notification-log-path cfg)
                            (%default-log-path))))
    (make-instance 'log-backend
                   :name :log
                   :enabled-p resolved-enabled
                   :config cfg
                   :path resolved-path
                   :include-event-payload-p t)))

(defun preview-notification-sound (&key (category :error) config theme)
  (let* ((cfg (or config (current-config)))
         (backend (make-sound-backend :config cfg :theme theme))
         (sound-path (resolve-sound-path (or theme (active-sound-theme-name))
                                         category
                                         :config cfg)))
    (cond
      ((null sound-path)
       (values nil :no-sound-configured nil))
      ((not (backend-enabled-p backend))
       (values nil :backend-disabled sound-path))
      ((not (notify-available-p backend))
       (values nil :backend-unavailable sound-path))
      (t
       (multiple-value-bind (ok detail)
           (notify-send backend
                        (make-notification :title "Sound Preview"
                                           :body "Preview"
                                           :severity :info
                                           :category category
                                           :timestamp (get-universal-time)))
         (values ok detail sound-path))))))

(defun %default-backends (cfg)
  (let ((base-backends
          (list (make-sound-backend :config cfg)
                (make-desktop-backend :config cfg)
                (make-log-backend :config cfg))))
    (if (fboundp 'make-webhook-backends)
        (append base-backends (funcall (symbol-function 'make-webhook-backends) :config cfg))
        base-backends)))

(defun %event-trigger (event)
  (let ((event-type (event-type event)))
    (cond
      ((eq event-type +event-type-tool-completed+) :task-complete)
      ((eq event-type +event-type-tool-error+) :error)
      ((eq event-type +event-type-agent-completed+) :long-running-complete)
      ((eq event-type +event-type-permission-prompted+) :approval-needed)
      (t nil))))

(defun %notification-from-event (event trigger)
  (let* ((payload (event-payload event))
         (title "Amoebum")
         (body "")
         (severity :info)
         (urgency :normal))
    (case trigger
      (:task-complete
       (setf title "Task Complete"
             severity :info
             urgency :normal
             body (if (tool-completed-payload-p payload)
                      (format nil "Tool ~A completed in ~Dms."
                              (or (tool-completed-payload-tool-name payload) "unknown")
                              (or (tool-completed-payload-elapsed-ms payload) 0))
                      "Task completed.")))
      (:error
       (setf title "Task Error"
             severity :error
             urgency :critical
             body (if (tool-error-payload-p payload)
                      (format nil "Tool ~A failed: ~A"
                              (or (tool-error-payload-tool-name payload) "unknown")
                              (or (tool-error-payload-condition payload) "unknown error"))
                      "An error occurred.")))
      (:long-running-complete
       (setf title "Long-Running Task Complete"
             severity :info
             urgency :normal
             body "Long-running work completed."))
      (:approval-needed
       (setf title "Approval Needed"
             severity :warning
             urgency :critical
             body (if (permission-prompted-payload-p payload)
                      (format nil "Permission prompt for tool ~A (~A)."
                              (or (permission-prompted-payload-tool-name payload) "unknown")
                              (or (permission-prompted-payload-reason payload) "approval required"))
                      "Approval requested."))))
    (make-notification
     :title title
     :body body
     :severity severity
     :category trigger
     :source-event event
     :timestamp (event-timestamp event)
     :urgency urgency
     :timeout-ms (if (eq trigger :approval-needed) 0 5000))))

(defun dispatch-notification-manager (manager notification)
  (dolist (backend (notification-manager-backends manager))
    (when (backend-enabled-p backend)
      (handler-case
          (when (notify-available-p backend)
            (notify-send backend notification))
        (error (condition)
          (ptui.util.log:log-warn
           "notification backend ~A failed: ~A"
           (backend-name backend)
           condition)))))
  notification)

(defun %dispatch-notification (manager notification)
  (let ((desktop-notifications-suppressed-p *desktop-notifications-suppressed*)
        (notification-command-runner *notification-command-runner*)
        (notification-command-prober *notification-command-prober*)
        (desktop-notification-run-command-function
          *desktop-notification-run-command-function*))
    (if *notification-async-dispatch-p*
        (bordeaux-threads:make-thread
         (lambda ()
           ;; Preserve notification suppression and command mocks across
           ;; background delivery so test verification never escapes to the OS.
           (let ((*desktop-notifications-suppressed* desktop-notifications-suppressed-p)
                 (*notification-command-runner* notification-command-runner)
                 (*notification-command-prober* notification-command-prober)
                 (*desktop-notification-run-command-function*
                   desktop-notification-run-command-function))
             (dispatch-notification manager notification)))
         :name "amoebum-notification-dispatch")
        (dispatch-notification manager notification))))

(defun %notification-event-handler (manager event)
  (let ((trigger (%event-trigger event)))
    (when (and trigger
               (member trigger (notification-manager-enabled-events manager) :test #'eq))
      (%dispatch-notification manager
                              (%notification-from-event event trigger)))))

(defun %probe-backends! (manager)
  (dolist (backend (notification-manager-backends manager))
    (when (backend-enabled-p backend)
      (unless (notify-available-p backend)
        (setf (backend-enabled-p backend) nil)
        (ptui.util.log:log-warn
         "notification backend unavailable at startup; disabling ~A"
         (backend-name backend)))))
  manager)

(defun %subscribe-manager (manager)
  (let* ((bus (notification-manager-event-bus manager))
         (ids
           (list
            (subscribe bus +event-type-tool-completed+
                       (lambda (event)
                         (%notification-event-handler manager event))
                       :priority 40)
            (subscribe bus +event-type-tool-error+
                       (lambda (event)
                         (%notification-event-handler manager event))
                       :priority 40)
            (subscribe bus +event-type-agent-completed+
                       (lambda (event)
                         (%notification-event-handler manager event))
                       :priority 40)
            (subscribe bus +event-type-permission-prompted+
                       (lambda (event)
                         (%notification-event-handler manager event))
                       :priority 40))))
    (setf (notification-manager-subscription-ids manager) ids)
    manager))

(defun make-notification-manager (&key config event-bus backends enabled-events)
  (let* ((cfg (or config (current-config)))
         (bus (or event-bus (current-event-bus)))
         (resolved-events (or enabled-events (%config-enabled-events cfg)))
         (manager (%make-notification-manager
                   :event-bus bus
                   :enabled-events resolved-events
                   :backends (or backends (%default-backends cfg)))))
    (%probe-backends! manager)
    (%subscribe-manager manager)
    (when (fboundp 'ensure-notification-dispatcher)
      (ignore-errors
        (ensure-notification-dispatcher :manager manager :event-bus bus)))
    manager))

(defun stop-notification-manager (&optional manager-or-bus)
  (let* ((manager
           (cond
             ((notification-manager-p manager-or-bus) manager-or-bus)
             ((event-bus-p manager-or-bus)
              (gethash manager-or-bus *notification-manager-registry*))
             (t
              (gethash (current-event-bus) *notification-manager-registry*))))
         (bus (and manager (notification-manager-event-bus manager))))
    (when manager
      (dolist (subscription-id (notification-manager-subscription-ids manager))
        (ignore-errors
          (unsubscribe bus subscription-id)))
      (dolist (backend (notification-manager-backends manager))
        (ignore-errors
          (notify-teardown backend)))
      (when (fboundp 'stop-notification-dispatcher)
        (ignore-errors
          (stop-notification-dispatcher :event-bus bus)))
      (setf (notification-manager-subscription-ids manager) '())
      (when bus
        (remhash bus *notification-manager-registry*)))
    manager))

(defun stop-all-notification-managers ()
  (let ((managers
          (loop for manager being the hash-values of *notification-manager-registry*
                collect manager)))
    (dolist (manager managers)
      (stop-notification-manager manager)))
  (clrhash *notification-manager-registry*)
  t)

(defun ensure-notification-manager (&key config event-bus)
  (let* ((bus (or event-bus (current-event-bus)))
         (existing (gethash bus *notification-manager-registry*)))
    (if existing
        existing
        (setf (gethash bus *notification-manager-registry*)
              (make-notification-manager :config (or config (current-config))
                                         :event-bus bus)))))
