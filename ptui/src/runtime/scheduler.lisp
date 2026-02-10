(defpackage :ptui.runtime.scheduler
  (:use :cl)
  (:import-from :bordeaux-threads
                #:make-lock
                #:with-lock-held)
  (:export #:scheduler
           #:make-scheduler
           #:schedule-interval
           #:schedule-once
           #:cancel-task
           #:scheduler-next-timeout-ms
           #:scheduler-run-due))

(in-package :ptui.runtime.scheduler)

(defstruct (scheduled-task
            (:constructor make-scheduled-task
                (&key id due-ms interval-ms fn cancelledp)))
  (id 0 :type fixnum)
  (due-ms 0 :type integer)
  (interval-ms nil :type (or null (integer 0 *)))
  (fn (lambda ()) :type function)
  (cancelledp nil :type boolean))

(defstruct (scheduler (:constructor %make-scheduler (&key lock next-id tasks)))
  ;; All schedule/cancel/run operations are serialized by this lock.
  (lock (make-lock "ptui-scheduler-lock"))
  (next-id 0 :type fixnum)
  (tasks '() :type list))

(defun make-scheduler ()
  (%make-scheduler))

(defun %next-task-id (sched)
  (setf (scheduler-next-id sched)
        (the fixnum (1+ (scheduler-next-id sched)))))

(defun %schedule (sched ms fn interval-ms)
  (check-type ms (integer 0 *))
  (check-type fn function)
  (let ((now-ms (ptui.util.time:monotonic-ms)))
    (with-lock-held ((scheduler-lock sched))
      (let* ((task-id (%next-task-id sched))
             (task (make-scheduled-task
                    :id task-id
                    :due-ms (+ now-ms ms)
                    :interval-ms interval-ms
                    :fn fn
                    :cancelledp nil)))
        (push task (scheduler-tasks sched))
        task-id))))

(defun schedule-interval (sched ms fn)
  (%schedule sched ms fn ms))

(defun schedule-once (sched ms fn)
  (%schedule sched ms fn nil))

(defun cancel-task (sched task-id)
  (with-lock-held ((scheduler-lock sched))
    (let ((cancelledp nil))
      (dolist (task (scheduler-tasks sched) cancelledp)
        (when (and (= (scheduled-task-id task) task-id)
                   (not (scheduled-task-cancelledp task)))
          (setf (scheduled-task-cancelledp task) t
                cancelledp t))))))

(defun scheduler-next-timeout-ms (sched)
  (let ((now-ms (ptui.util.time:monotonic-ms)))
    (with-lock-held ((scheduler-lock sched))
      (let ((nearest nil))
        (dolist (task (scheduler-tasks sched))
          (unless (scheduled-task-cancelledp task)
            (let ((due-ms (scheduled-task-due-ms task)))
              (when (or (null nearest) (< due-ms nearest))
                (setf nearest due-ms)))))
        (and nearest (max 0 (- nearest now-ms)))))))

(defun scheduler-run-due (sched)
  (let ((due-tasks '())
        (executed 0))
    (with-lock-held ((scheduler-lock sched))
      (let ((now-ms (ptui.util.time:monotonic-ms))
            (kept '()))
        (dolist (task (scheduler-tasks sched))
          (cond
            ((scheduled-task-cancelledp task)
             nil)
            ((<= (scheduled-task-due-ms task) now-ms)
             (push task due-tasks))
            (t
             (push task kept))))
        (setf (scheduler-tasks sched) kept)))
    (dolist (task due-tasks executed)
      (unless (scheduled-task-cancelledp task)
        (funcall (scheduled-task-fn task))
        (incf executed)
        (let ((interval-ms (scheduled-task-interval-ms task)))
          (when interval-ms
            (with-lock-held ((scheduler-lock sched))
              (unless (scheduled-task-cancelledp task)
                (setf (scheduled-task-due-ms task)
                      (+ (ptui.util.time:monotonic-ms) interval-ms))
                (push task (scheduler-tasks sched))))))))))
