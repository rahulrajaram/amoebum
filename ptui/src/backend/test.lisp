(defpackage :ptui.backend.test
  (:use :cl)
  (:export #:make-test-backend
           #:test-backend-buffer
           #:test-backend-inject-events))

(in-package :ptui.backend.test)

(defclass test-backend (ptui.backend.protocol:terminal-backend)
  ((cols :initarg :cols :reader test-backend-cols)
   (rows :initarg :rows :reader test-backend-rows)
   (buffer :accessor test-backend-buffer)
   (pending-events :initform '() :accessor test-backend-pending-events))
  (:documentation "In-memory terminal backend for snapshot testing."))

(defun make-test-backend (&key (cols 80) (rows 24))
  (let ((backend (make-instance 'test-backend
                                :cols cols
                                :rows rows
                                :caps (ptui.term.caps:probe-terminal-caps))))
    (setf (test-backend-buffer backend)
          (ptui.render.buffer:make-buffer cols rows))
    backend))

(defmethod ptui.backend.protocol:backend-init ((backend test-backend))
  nil)

(defmethod ptui.backend.protocol:backend-shutdown ((backend test-backend))
  nil)

(defmethod ptui.backend.protocol:backend-poll-events ((backend test-backend))
  (let ((events (test-backend-pending-events backend)))
    (setf (test-backend-pending-events backend) '())
    events))

(defmethod ptui.backend.protocol:backend-size ((backend test-backend))
  (ptui.core.types:make-size (test-backend-cols backend)
                              (test-backend-rows backend)))

(defmethod ptui.backend.protocol:backend-commit ((backend test-backend) draw-ops)
  (let ((buf (test-backend-buffer backend)))
    (dolist (op draw-ops)
      (when (listp op)
        (case (first op)
          (:draw-text
           (destructuring-bind (_ x y styled-text &rest keys) op
             (declare (ignore _))
             (apply #'ptui.render.buffer:buffer-draw-text buf x y styled-text keys)))
          (:fill-rect
           (destructuring-bind (_ rect cell) op
             (declare (ignore _))
             (ptui.render.buffer:buffer-fill-rect buf rect cell)))
          (:clear
           (ptui.render.buffer:buffer-clear buf)))))
    (length draw-ops)))

(defun test-backend-inject-events (backend events)
  "Inject synthetic input events into the test backend's pending queue."
  (setf (test-backend-pending-events backend)
        (append (test-backend-pending-events backend)
                (if (listp events) events (list events)))))
