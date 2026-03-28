(defpackage :ptui.test.lower-runtime-contract
  (:use :cl :fiveam)
  (:export #:run-all #:ptui-lower-runtime-contract-suite))

(in-package :ptui.test.lower-runtime-contract)

(def-suite ptui-lower-runtime-contract-suite
  :description "PTUI lower-runtime contracts: caps, signals, event-bus, and backend protocol.")

(in-suite ptui-lower-runtime-contract-suite)

(defmacro with-temp-functions (bindings &body body)
  (let ((saved-fns
          (loop for binding in bindings
                collect (gensym "ORIGINAL-FN-"))))
    `(let ,(loop for (name fn) in bindings
                 for saved in saved-fns
                 collect `(,saved (symbol-function ',name)))
       (unwind-protect
            (progn
              ,@(loop for (name fn) in bindings
                      collect `(setf (symbol-function ',name) ,fn))
              ,@body)
         ,@(loop for (name fn) in bindings
                 for saved in saved-fns
                 collect `(setf (symbol-function ',name) ,saved))))))

(defun make-env (entries)
  (lambda (name)
    (cdr (assoc name entries :test #'string=))))

(defun %make-signal-read-stub (entries)
  (let ((pending (copy-list entries)))
    (lambda (signo-ptr)
      (if pending
          (destructuring-bind (status . signo) (pop pending)
            (when (= status 1)
              (setf (cffi:mem-ref signo-ptr :int) signo))
            status)
          0))))

(defclass contract-backend (ptui.backend.protocol:terminal-backend)
  ((cols :initarg :cols :reader contract-backend-cols)
   (rows :initarg :rows :reader contract-backend-rows)
   (pending-events :initarg :pending-events :accessor contract-backend-pending-events)
   (startedp :initform nil :accessor contract-backend-startedp)
   (stoppedp :initform nil :accessor contract-backend-stoppedp)
   (commit-log :initform '() :accessor contract-backend-commit-log)))

(defun make-contract-backend (&key
                               (caps (ptui.term.caps:probe-terminal-caps
                                      :env (make-env '(("TERM" . "xterm-256color")))))
                               (cols 12)
                               (rows 6)
                               (events '()))
  (make-instance 'contract-backend
                 :caps caps
                 :cols cols
                 :rows rows
                 :pending-events (copy-list events)))

(defmethod ptui.backend.protocol:backend-init ((backend contract-backend))
  (setf (contract-backend-startedp backend) t)
  nil)

(defmethod ptui.backend.protocol:backend-shutdown ((backend contract-backend))
  (setf (contract-backend-stoppedp backend) t)
  nil)

(defmethod ptui.backend.protocol:backend-poll-events ((backend contract-backend))
  (prog1 (copy-list (contract-backend-pending-events backend))
    (setf (contract-backend-pending-events backend) '())))

(defmethod ptui.backend.protocol:backend-size ((backend contract-backend))
  (ptui.core.types:make-size (contract-backend-cols backend)
                             (contract-backend-rows backend)))

(defmethod ptui.backend.protocol:backend-commit ((backend contract-backend) draw-ops)
  (push (copy-tree draw-ops) (contract-backend-commit-log backend))
  (length draw-ops))

(test lower-runtime-caps-supports-multiple-env-sources
  (let ((env-hash (make-hash-table :test #'equal)))
    (setf (gethash "TERM" env-hash) "xterm-256color")
    (let ((caps (ptui.term.caps:probe-terminal-caps :env env-hash)))
      (is (string= "xterm-256color" (ptui.term.caps:terminal-caps-term caps)))
      (is-true (ptui.term.caps:terminal-caps-256colorp caps))))
  (let ((caps (ptui.term.caps:probe-terminal-caps
               :env '(("TERM" . "xterm")
                      ("COLORTERM" . "truecolor")))))
    (is-true (ptui.term.caps:terminal-caps-truecolorp caps))
    (is-true (ptui.term.caps:terminal-caps-256colorp caps))))

(test lower-runtime-caps-disable-interactive-features-for-dumb-terminals
  (let ((caps (ptui.term.caps:probe-terminal-caps
               :env (make-env '(("TERM" . "dumb")
                                ("COLORTERM" . nil))))))
    (is (string= "dumb" (ptui.term.caps:terminal-caps-term caps)))
    (is-false (ptui.term.caps:terminal-caps-truecolorp caps))
    (is-false (ptui.term.caps:terminal-caps-256colorp caps))
    (is-false (ptui.term.caps:terminal-caps-mousep caps))
    (is-false (ptui.term.caps:terminal-caps-alt-screenp caps))))

(test lower-runtime-signals-init-validates-native-setup
  (let ((ensure-calls 0))
    (with-temp-functions
        ((ptui.term.tty::%ensure-native-loaded
          (lambda ()
            (incf ensure-calls)
            nil))
         (ptui.term.signals::%ptui-signals-init (lambda () 0))
         (ptui.term.signals::%ptui-signals-fd (lambda () 7)))
      (is (null (ptui.term.signals:signals-init)))
      (is (= 1 ensure-calls)))))

(test lower-runtime-signals-init-errors-on-native-failures
  (with-temp-functions
      ((ptui.term.tty::%ensure-native-loaded (lambda () nil))
       (ptui.term.signals::%ptui-signals-init (lambda () 1))
       (ptui.term.signals::%ptui-signals-fd (lambda () 7)))
    (signals error
      (ptui.term.signals:signals-init)))
  (with-temp-functions
      ((ptui.term.tty::%ensure-native-loaded (lambda () nil))
       (ptui.term.signals::%ptui-signals-init (lambda () 0))
       (ptui.term.signals::%ptui-signals-fd (lambda () -1)))
    (signals error
      (ptui.term.signals:signals-init))))

(test lower-runtime-signals-poll-drains-known-and-unknown-signals
  (let ((ensure-calls 0))
    (with-temp-functions
        ((ptui.term.tty::%ensure-native-loaded
          (lambda ()
            (incf ensure-calls)
            nil))
         (ptui.term.signals::%ptui-signals-read
          (%make-signal-read-stub '((1 . 2)
                                    (1 . 28)
                                    (1 . 99)
                                    (0 . nil)))))
      (is (equal '(:int :winch :SIG-99)
                 (ptui.term.signals:signals-poll)))
      (is (= 1 ensure-calls)))))

(test lower-runtime-signals-poll-errors-on-native-failure
  (with-temp-functions
      ((ptui.term.tty::%ensure-native-loaded (lambda () nil))
       (ptui.term.signals::%ptui-signals-read
        (%make-signal-read-stub '((-1 . nil)))))
    (signals error
      (ptui.term.signals:signals-poll))))

(test lower-runtime-event-bus-publish-returns-envelope-and-delivers-subscribers
  (let ((bus (ptui.runtime.event-bus:make-event-bus))
        (seen '()))
    (with-temp-functions
        ((ptui.util.time:monotonic-ms (lambda () 4242)))
      (let* ((subscription-id
               (ptui.runtime.event-bus:subscribe
                bus
                (lambda (event)
                  (push event seen))))
             (event (ptui.runtime.event-bus:publish bus '(:kind :tick))))
        (is (typep event 'ptui.runtime.event-bus:event-envelope))
        (is (= 1 (ptui.runtime.event-bus:event-envelope-sequence event)))
        (is (= 4242 (ptui.runtime.event-bus:event-envelope-timestamp-ms event)))
        (is (equal '(:kind :tick)
                   (ptui.runtime.event-bus:event-envelope-payload event)))
        (is (eq event (first seen)))
        (is-true (ptui.runtime.event-bus:unsubscribe bus subscription-id))))))

(test lower-runtime-event-bus-unsubscribe-removes-handlers-and-missing-ids-return-false
  (let ((bus (ptui.runtime.event-bus:make-event-bus))
        (handler-calls 0))
    (let ((subscription-id
            (ptui.runtime.event-bus:subscribe
             bus
             (lambda (event)
               (declare (ignore event))
               (incf handler-calls)))))
      (is-true (ptui.runtime.event-bus:unsubscribe bus subscription-id))
      (is-false (ptui.runtime.event-bus:unsubscribe bus subscription-id))
      (ptui.runtime.event-bus:publish bus :ignored)
      (is (= 0 handler-calls)))))

(test lower-runtime-backend-protocol-dispatches-through-terminal-backend-subclasses
  (let* ((caps (ptui.term.caps:probe-terminal-caps
                :env (make-env '(("TERM" . "xterm-256color")))))
         (event (ptui.core.events:make-key-event :text :text? "a"))
         (backend (make-contract-backend
                   :caps caps
                   :cols 14
                   :rows 4
                   :events (list event))))
    (is (typep backend 'ptui.backend.protocol:terminal-backend))
    (is (eq caps (ptui.backend.protocol::backend-caps backend)))
    (is (null (ptui.backend.protocol:backend-init backend)))
    (is-true (contract-backend-startedp backend))
    (let ((size (ptui.backend.protocol:backend-size backend)))
      (is (= 14 (ptui.core.types:size-cols size)))
      (is (= 4 (ptui.core.types:size-rows size))))
    (let ((events (ptui.backend.protocol:backend-poll-events backend)))
      (is (= 1 (length events)))
      (is (eq event (first events))))
    (is (null (ptui.backend.protocol:backend-poll-events backend)))
    (is (= 2 (ptui.backend.protocol:backend-commit backend
                                                   '((:draw-text 0 0 "A")
                                                     (:clear)))))
    (is (equal '(((:draw-text 0 0 "A") (:clear)))
               (contract-backend-commit-log backend)))
    (is (null (ptui.backend.protocol:backend-shutdown backend)))
    (is-true (contract-backend-stoppedp backend))))

(defun run-all ()
  (run! 'ptui-lower-runtime-contract-suite))
