(defpackage :ptui.runtime.queue
  (:use :cl)
  (:import-from :bordeaux-threads
                #:make-lock
                #:with-lock-held)
  (:export #:event-queue
           #:make-event-queue
           #:queue-push
           #:queue-pop-all))

(in-package :ptui.runtime.queue)

(defstruct (event-queue (:constructor %make-event-queue (&key lock events)))
  ;; Push + pop-all are serialized by this mutex.
  (lock (make-lock "ptui-event-queue-lock"))
  (events '() :type list))

(defun make-event-queue ()
  (%make-event-queue))

(defun queue-push (q event)
  (with-lock-held ((event-queue-lock q))
    (setf (event-queue-events q)
          (cons event (event-queue-events q))))
  nil)

(defun queue-pop-all (q)
  (with-lock-held ((event-queue-lock q))
    (let* ((events (event-queue-events q))
           (count (length events)))
      (setf (event-queue-events q) '())
      (values (nreverse events) count))))
