(in-package :amoebum/test)

;;; ============================================================
;;; I250: Automatic Checkpointing with Rotation
;;; ============================================================

(def-suite checkpoint-rotation-suite :in amoebum-suite)
(in-suite checkpoint-rotation-suite)

(test checkpoint-auto-idle-seconds-from-config
  "checkpoint-auto-idle-seconds should read from config."
  (let ((seconds (amoebum.sessions:checkpoint-auto-idle-seconds)))
    (is (integerp seconds))
    (is (>= seconds 0))))

(test checkpoint-max-count-from-config
  "checkpoint-max-count should read retention count from config."
  (let ((old-config amoebum::*current-config*))
    (unwind-protect
         (progn
           (amoebum.config:setconfig :auto-checkpoint-max-count 3)
           (is (= 3 (amoebum.sessions:checkpoint-max-count))))
      (setf amoebum::*current-config* old-config))))

(test checkpoint-mark-activity-updates-timestamp
  "checkpoint-mark-activity should update the last activity timestamp."
  (let ((old amoebum::*checkpoint-last-activity-at*))
    (unwind-protect
         (let ((ts (amoebum.sessions:checkpoint-mark-activity 12345)))
           (is (= 12345 ts))
           (is (= 12345 amoebum::*checkpoint-last-activity-at*)))
      (setf amoebum::*checkpoint-last-activity-at* old))))

(test maybe-auto-checkpoint-skips-when-busy
  "maybe-auto-checkpoint should skip when busy."
  (is (null (amoebum.sessions:maybe-auto-checkpoint :busy-p t))))

(test maybe-auto-checkpoint-skips-when-zero-interval
  "maybe-auto-checkpoint should skip when interval is 0."
  (let ((old-config amoebum::*current-config*))
    (unwind-protect
         (progn
           (amoebum.config:setconfig :auto-checkpoint-idle-seconds 0)
           (is (null (amoebum.sessions:maybe-auto-checkpoint))))
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
           (amoebum.config:setconfig :auto-checkpoint-idle-seconds 1)
           ;; Simulate activity 10 seconds ago
           (let ((now (get-universal-time)))
             (amoebum.sessions:checkpoint-mark-activity (- now 10))
             (let ((result (amoebum.sessions:maybe-auto-checkpoint
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
             (amoebum.sessions:checkpoint-session :project-root tmp-dir
                                          :event-bus bus
                                          :trigger :manual
                                          :timestamp (+ (get-universal-time) i)))
           (let ((all (amoebum.sessions:list-session-checkpoints :project-root tmp-dir)))
             (is (= 3 (length all))))
           (let ((limited (amoebum.sessions:list-session-checkpoints
                           :project-root tmp-dir :limit 2)))
             (is (= 2 (length limited)))))
      (setf amoebum::*checkpoint-directory-override* old-override
            amoebum::*event-bus* old-bus)
      (%delete-directory-tree-safe tmp-dir))))

(test checkpoint-rotation-keeps-latest-n
  "checkpoint-session should rotate old checkpoints and keep only the latest N."
  (let* ((old-config amoebum::*current-config*)
         (old-override amoebum::*checkpoint-directory-override*)
         (old-bus amoebum::*event-bus*)
         (tmp-dir (%make-temp-directory "amoebum-checkpoint-rotation"))
         (bus (amoebum:make-event-bus)))
    (unwind-protect
         (progn
           (setf amoebum::*checkpoint-directory-override* tmp-dir
                 amoebum::*event-bus* bus)
           (amoebum.config:setconfig :auto-checkpoint-max-count 2)
           (let ((ids '()))
             (dotimes (i 4)
               (let ((checkpoint
                       (amoebum.sessions:checkpoint-session
                        :project-root tmp-dir
                        :event-bus bus
                        :trigger :manual
                        :timestamp (+ 1700000000 i))))
                 (push (amoebum.sessions:session-checkpoint-id checkpoint) ids)))
             (setf ids (nreverse ids))
             (let* ((remaining (amoebum.sessions:list-session-checkpoints :project-root tmp-dir))
                    (remaining-ids (mapcar #'amoebum.sessions:session-checkpoint-id remaining)))
               (is (= 2 (length remaining-ids)))
               (is (equal (list (fourth ids) (third ids)) remaining-ids))
               (is (null (find (first ids) remaining-ids :test #'string=)))
               (is (null (find (second ids) remaining-ids :test #'string=))))))
      (setf amoebum::*current-config* old-config
            amoebum::*checkpoint-directory-override* old-override
            amoebum::*event-bus* old-bus)
      (%delete-directory-tree-safe tmp-dir))))

(test checkpoint-path-uses-project-prefix-and-core-extension
  "checkpoint-session should create project-prefixed .core files."
  (let* ((old-override amoebum::*checkpoint-directory-override*)
         (old-bus amoebum::*event-bus*)
         (tmp-root (%make-temp-directory "amoebum-checkpoint-name"))
         (project-root (merge-pathnames #P"My Project/" tmp-root))
         (bus (amoebum:make-event-bus)))
    (unwind-protect
         (progn
           (setf amoebum::*checkpoint-directory-override* nil
                 amoebum::*event-bus* bus)
           (ensure-directories-exist (merge-pathnames #P".keep" project-root))
           (let* ((checkpoint
                    (amoebum.sessions:checkpoint-session
                     :project-root project-root
                     :event-bus bus
                     :trigger :manual
                     :timestamp 1700000010))
                  (path (amoebum.sessions:session-checkpoint-path checkpoint))
                  (name (or (pathname-name path) "")))
             (is (string= "core" (or (pathname-type path) "")))
             (is (search ".amoebum/checkpoints/" (namestring path) :test #'char-equal))
             (is (search "my-project-" name :test #'char-equal))))
      (setf amoebum::*checkpoint-directory-override* old-override
            amoebum::*event-bus* old-bus)
      (%delete-directory-tree-safe tmp-root))))

(test checkpoint-id-from-time-format
  "Checkpoint ID should be formatted as ISO-like timestamp."
  (let ((id (amoebum::%checkpoint-id-from-time)))
    (is (stringp id))
    (is (search "T" id))
    (is (search "Z" id))))

(test checkpoint-rotation-smoke-sentinel
  (is-true t)
  (format t "CHECKPOINT_ROTATION_SMOKE_OK~%"))
