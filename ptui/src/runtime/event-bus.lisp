(defpackage :ptui.runtime.event-bus
  (:use :cl)
  (:import-from :bordeaux-threads
                #:make-lock
                #:with-lock-held)
  (:export #:event-bus
           #:event-envelope
           #:event-envelope-sequence
           #:event-envelope-timestamp-ms
           #:event-envelope-payload
           #:make-event-bus
           #:publish
           #:subscribe
           #:unsubscribe))

(in-package :ptui.runtime.event-bus)

(eval-when (:compile-toplevel :load-toplevel :execute)
  #+sbcl
  (require :sb-concurrency))

#-sbcl
(defstruct (fallback-queue
            (:constructor %make-fallback-queue (&key lock items)))
  (lock (make-lock "ptui-event-bus-fallback-queue-lock"))
  (items '() :type list))

#-sbcl
(defun %make-ring-buffer ()
  (%make-fallback-queue))

#+sbcl
(defun %make-ring-buffer ()
  (sb-concurrency:make-queue :name "ptui-event-bus"))

#-sbcl
(defun %ring-buffer-enqueue (ring-buffer value)
  (with-lock-held ((fallback-queue-lock ring-buffer))
    (setf (fallback-queue-items ring-buffer)
          (nconc (fallback-queue-items ring-buffer)
                 (list value))))
  value)

#+sbcl
(defun %ring-buffer-enqueue (ring-buffer value)
  (sb-concurrency:enqueue value ring-buffer)
  value)

(defstruct (event-envelope
            (:constructor make-event-envelope (&key sequence timestamp-ms payload)))
  (sequence 0 :type (unsigned-byte 64))
  (timestamp-ms 0 :type integer)
  (payload nil))

(defstruct (event-bus
            (:constructor %make-event-bus
                (&key ring-buffer subscription-table subscription-lock sequence-counter sequence-lock next-subscription-id)))
  (ring-buffer (%make-ring-buffer))
  (subscription-table (make-hash-table :test #'eql))
  (subscription-lock (make-lock "ptui-event-bus-subscriptions-lock"))
  (sequence-counter 0 :type (unsigned-byte 64))
  ;; Non-SBCL runtimes use this lock for portable monotonic sequence assignment.
  (sequence-lock (make-lock "ptui-event-bus-sequence-lock"))
  (next-subscription-id 0 :type (unsigned-byte 64)))

(defun make-event-bus ()
  (%make-event-bus))

#+sbcl
(defun %next-sequence (bus)
  (1+ (sb-ext:atomic-incf (event-bus-sequence-counter bus))))

#-sbcl
(defun %next-sequence (bus)
  (with-lock-held ((event-bus-sequence-lock bus))
    (incf (event-bus-sequence-counter bus))))

(defun %snapshot-subscribers (bus)
  (with-lock-held ((event-bus-subscription-lock bus))
    (loop for handler being the hash-values of (event-bus-subscription-table bus)
          collect handler)))

(defun publish (bus payload)
  (let* ((sequence (%next-sequence bus))
         (event (make-event-envelope
                 :sequence sequence
                 :timestamp-ms (ptui.util.time:monotonic-ms)
                 :payload payload))
         (handlers (%snapshot-subscribers bus)))
    (%ring-buffer-enqueue (event-bus-ring-buffer bus) event)
    (dolist (handler handlers)
      (funcall handler event))
    event))

(defun subscribe (bus handler)
  (check-type handler function)
  (with-lock-held ((event-bus-subscription-lock bus))
    (let ((subscription-id (incf (event-bus-next-subscription-id bus))))
      (setf (gethash subscription-id (event-bus-subscription-table bus)) handler)
      subscription-id)))

(defun unsubscribe (bus subscription-id)
  (with-lock-held ((event-bus-subscription-lock bus))
    (not (null (remhash subscription-id (event-bus-subscription-table bus))))))
