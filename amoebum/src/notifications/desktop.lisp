(in-package :amoebum)

(defparameter *desktop-notification-run-command-function* #'pseudopod:run-command)

(defun %desktop-shell-quote (text)
  (format nil "'~A'"
          (with-output-to-string (out)
            (loop for char across (or text "") do
              (if (char= char #\')
                  (write-string "'\"'\"'" out)
                  (write-char char out))))))

(defun %desktop-command-string (arguments)
  (with-output-to-string (stream)
    (loop for argument in arguments
          for firstp = t then nil do
            (unless firstp
              (write-char #\Space stream))
            (write-string (%desktop-shell-quote (or argument "")) stream))))

(defun %desktop-normalize-run-result (result)
  (cond
    ((pseudopod:command-result-p result)
     (list :exit-code (or (pseudopod:command-result-exit-code result) 1)
           :stdout (or (pseudopod:command-result-stdout result) "")
           :stderr (or (pseudopod:command-result-stderr result) "")))
    ((listp result)
     (list :exit-code (or (getf result :exit-code) 1)
           :stdout (or (getf result :stdout) "")
           :stderr (or (getf result :stderr) "")))
    (t
     (list :exit-code 1
           :stdout ""
           :stderr (princ-to-string result)))))

(defun %desktop-run-command (arguments)
  (%desktop-normalize-run-result
   (funcall *desktop-notification-run-command-function*
            (%desktop-command-string arguments))))

(defun %desktop-icon-path (notification backend)
  (let ((icon-path
          (or (ignore-errors (notification-icon-path notification))
              (notification-icon notification)
              (desktop-backend-default-icon backend))))
    (typecase icon-path
      (pathname (namestring icon-path))
      (string icon-path)
      (t (and icon-path (princ-to-string icon-path))))))

(defun %desktop-detect-platform-command (&optional command)
  (let ((explicit (%nonblank-string command)))
    (cond
      ((and explicit (string-equal explicit "notify-send"))
       (values :linux "notify-send"))
      ((and explicit (string-equal explicit "osascript"))
       (values :macos "osascript"))
      ((not (uiop:os-unix-p))
       (values :other nil))
      ((notification-command-available-p "notify-send")
       (values :linux "notify-send"))
      ((notification-command-available-p "osascript")
       (values :macos "osascript"))
      (t
       (values :other nil)))))

(defun %desktop-urgency-flag (urgency)
  (case urgency
    (:low "low")
    (:critical "critical")
    (otherwise "normal")))

(defun %notify-send-command (notification backend)
  (let* ((title (notification-title notification))
         (body (notification-body notification))
         (icon-path (%desktop-icon-path notification backend)))
    (append (list "notify-send"
                  (format nil "--app-name=~A" (desktop-backend-app-name backend))
                  (format nil "--urgency=~A"
                          (%desktop-urgency-flag (notification-urgency notification)))
                  (format nil "--expire-time=~D"
                          (max 0 (notification-timeout-ms notification))))
            (when icon-path
              (list (format nil "--icon=~A" icon-path)))
            (list title body))))

(defun %osascript-command (notification)
  (let ((title (%json-escape (notification-title notification)))
        (body (%json-escape (notification-body notification))))
    (list "osascript"
          "-e"
          (format nil "display notification \"~A\" with title \"~A\""
                  body
                  title))))

(defun desktop-notification-available-p (&optional backend)
  (multiple-value-bind (_platform command)
      (%desktop-detect-platform-command (and backend (desktop-backend-command backend)))
    (declare (ignore _platform))
    (when (and backend command)
      (setf (desktop-backend-command backend) command))
    (not (null command))))

(defun send-desktop-notification (notification &key backend)
  (let ((resolved-backend (or backend (make-desktop-backend))))
    (if (not (desktop-notification-available-p resolved-backend))
        (values nil :backend-unavailable)
        (multiple-value-bind (platform command)
            (%desktop-detect-platform-command (desktop-backend-command resolved-backend))
          (declare (ignore command))
          (let* ((arguments
                   (case platform
                     (:linux (%notify-send-command notification resolved-backend))
                     (:macos (%osascript-command notification))
                     (otherwise nil)))
                 (result (and arguments (%desktop-run-command arguments)))
                 (exit-code (and result (getf result :exit-code)))
                 (stderr (and result (getf result :stderr))))
            (if (and arguments (zerop (or exit-code 1)))
                (values t nil)
                (progn
                  (ptui.util.log:log-warn
                   "desktop backend command failed: command=~S exit=~S stderr=~S"
                   arguments
                   exit-code
                   stderr)
                  (values nil (or stderr :command-failed)))))))))
