(in-package :amoebum/test)

;;; ============================================================
;;; I250: Automatic Checkpointing with Rotation
;;; ============================================================

(def-suite checkpoint-rotation-suite :in amoebum-suite)
(in-suite checkpoint-rotation-suite)

(test checkpoint-auto-idle-seconds-from-config
  "checkpoint-auto-idle-seconds should read from config."
  (let ((seconds (amoebum:checkpoint-auto-idle-seconds)))
    (is (integerp seconds))
    (is (>= seconds 0))))

(test checkpoint-mark-activity-updates-timestamp
  "checkpoint-mark-activity should update the last activity timestamp."
  (let ((old amoebum::*checkpoint-last-activity-at*))
    (unwind-protect
         (let ((ts (amoebum:checkpoint-mark-activity 12345)))
           (is (= 12345 ts))
           (is (= 12345 amoebum::*checkpoint-last-activity-at*)))
      (setf amoebum::*checkpoint-last-activity-at* old))))

(test maybe-auto-checkpoint-skips-when-busy
  "maybe-auto-checkpoint should skip when busy."
  (is (null (amoebum:maybe-auto-checkpoint :busy-p t))))

(test maybe-auto-checkpoint-skips-when-zero-interval
  "maybe-auto-checkpoint should skip when interval is 0."
  (let ((old-config amoebum::*current-config*))
    (unwind-protect
         (progn
           (amoebum:setconfig :auto-checkpoint-idle-seconds 0)
           (is (null (amoebum:maybe-auto-checkpoint))))
      (setf amoebum::*current-config* old-config))))

(test maybe-auto-checkpoint-fires-when-idle
  "maybe-auto-checkpoint should fire when idle long enough."
  (let* ((old-config amoebum::*current-config*)
         (old-activity amoebum::*checkpoint-last-activity-at*)
         (old-auto amoebum::*checkpoint-last-auto-checkpoint-at*)
         (old-override amoebum::*checkpoint-directory-override*)
         (old-bus amoebum::*event-bus*)
         (tmp-dir (%make-temp-directory "amoebum-checkpoint-auto"))
         (bus (amoebum:make-event-bus)))
    (unwind-protect
         (progn
           (setf amoebum::*checkpoint-directory-override* tmp-dir
                 amoebum::*event-bus* bus
                 amoebum::*checkpoint-last-auto-checkpoint-at* nil)
           (amoebum:setconfig :auto-checkpoint-idle-seconds 1)
           ;; Simulate activity 10 seconds ago
           (let ((now (get-universal-time)))
             (amoebum:checkpoint-mark-activity (- now 10))
             (let ((result (amoebum:maybe-auto-checkpoint
                            :project-root tmp-dir
                            :event-bus bus
                            :timestamp now)))
               (is (not (null result)))
               (is (amoebum::session-checkpoint-p result))
               (is (amoebum::session-checkpoint-auto-p result)))))
      (setf amoebum::*current-config* old-config
            amoebum::*checkpoint-last-activity-at* old-activity
            amoebum::*checkpoint-last-auto-checkpoint-at* old-auto
            amoebum::*checkpoint-directory-override* old-override
            amoebum::*event-bus* old-bus)
      (%delete-directory-tree-safe tmp-dir))))

(test list-session-checkpoints-with-limit
  "list-session-checkpoints should respect limit."
  (let* ((old-override amoebum::*checkpoint-directory-override*)
         (old-bus amoebum::*event-bus*)
         (tmp-dir (%make-temp-directory "amoebum-checkpoint-list"))
         (bus (amoebum:make-event-bus)))
    (unwind-protect
         (progn
           (setf amoebum::*checkpoint-directory-override* tmp-dir
                 amoebum::*event-bus* bus)
           ;; Create 3 checkpoints
           (dotimes (i 3)
             (amoebum:checkpoint-session :project-root tmp-dir
                                          :event-bus bus
                                          :trigger :manual
                                          :timestamp (+ (get-universal-time) i)))
           (let ((all (amoebum:list-session-checkpoints :project-root tmp-dir)))
             (is (= 3 (length all))))
           (let ((limited (amoebum:list-session-checkpoints
                           :project-root tmp-dir :limit 2)))
             (is (= 2 (length limited)))))
      (setf amoebum::*checkpoint-directory-override* old-override
            amoebum::*event-bus* old-bus)
      (%delete-directory-tree-safe tmp-dir))))

(test checkpoint-id-from-time-format
  "Checkpoint ID should be formatted as ISO-like timestamp."
  (let ((id (amoebum::%checkpoint-id-from-time)))
    (is (stringp id))
    (is (search "T" id))
    (is (search "Z" id))))
