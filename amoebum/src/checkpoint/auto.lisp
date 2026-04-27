(in-package :amoebum)

(defun checkpoint-auto-idle-seconds (&optional (config (current-config)))
  (let ((value (and (config-p config)
                    (config-value :auto-checkpoint-idle-seconds config))))
    (if (and (integerp value) (>= value 0))
        value
        1800)))

(defun checkpoint-mark-activity (&optional (timestamp (get-universal-time)))
  (setf *checkpoint-last-activity-at* timestamp)
  timestamp)

(defun maybe-auto-checkpoint (&key
                                conversation
                                config
                                memory-backend
                                project-root
                                event-bus
                                (busy-p nil)
                                (timestamp (get-universal-time)))
  (let* ((resolved-config (or config (current-config)))
         (interval (checkpoint-auto-idle-seconds resolved-config)))
    (when (or busy-p (<= interval 0))
      (return-from maybe-auto-checkpoint nil))
    (when (and (integerp *checkpoint-last-activity-at*)
               (>= (- timestamp *checkpoint-last-activity-at*) interval)
               (or (null *checkpoint-last-auto-checkpoint-at*)
                   (>= (- timestamp *checkpoint-last-auto-checkpoint-at*) interval)))
      (checkpoint-session :conversation conversation
                          :config resolved-config
                          :memory-backend memory-backend
                          :project-root project-root
                          :event-bus event-bus
                          :trigger :idle
                          :auto-p t
                          :timestamp timestamp))))
