(defpackage :ptui.test.engine
  (:use :cl :fiveam)
  (:export #:run-all #:ptui-engine-suite))

(in-package :ptui.test.engine)

(def-suite ptui-engine-suite
  :description "PTUI engine-loop unit coverage (I312).")

(in-suite ptui-engine-suite)

(defclass counting-test-backend (ptui.backend.protocol:terminal-backend)
  ((cols :initarg :cols :accessor counting-test-backend-cols)
   (rows :initarg :rows :accessor counting-test-backend-rows)
   (pending-events :initform '() :accessor counting-test-backend-pending-events)
   (commit-count :initform 0 :accessor counting-test-backend-commit-count)
   (ops-log :initform '() :accessor counting-test-backend-ops-log)))

(defun make-counting-test-backend (&key (cols 80) (rows 24))
  (make-instance 'counting-test-backend :cols cols :rows rows
                 :caps (ptui.term.caps:probe-terminal-caps)))

(defun counting-test-backend-inject-events (backend events)
  (setf (counting-test-backend-pending-events backend)
        (append (counting-test-backend-pending-events backend)
                (if (listp events) events (list events)))))

(defun counting-test-backend-reset-stats (backend)
  (setf (counting-test-backend-commit-count backend) 0
        (counting-test-backend-ops-log backend) '()))

(defun counting-test-backend-pop-commit-ops (backend)
  (nreverse (copy-list (counting-test-backend-ops-log backend))))

(defun quit-key-event ()
  (ptui.core.events:make-key-event :ctrl-c :ctrlp t))

(defun text-key-event (text)
  (ptui.core.events:make-key-event :text :text? text))

(defmethod ptui.backend.protocol:backend-init ((backend counting-test-backend))
  (declare (ignore backend))
  nil)

(defmethod ptui.backend.protocol:backend-shutdown ((backend counting-test-backend))
  (declare (ignore backend))
  nil)

(defmethod ptui.backend.protocol:backend-poll-events ((backend counting-test-backend))
  (let ((events (counting-test-backend-pending-events backend)))
    (setf (counting-test-backend-pending-events backend) '())
    events))

(defmethod ptui.backend.protocol:backend-size ((backend counting-test-backend))
  (ptui.core.types:make-size (counting-test-backend-cols backend)
                             (counting-test-backend-rows backend)))

(defmethod ptui.backend.protocol:backend-commit ((backend counting-test-backend) draw-ops)
  (incf (counting-test-backend-commit-count backend))
  (push (copy-list draw-ops) (counting-test-backend-ops-log backend))
  (length draw-ops))

(test engine-starts-and-stops-cleanly
  (let* ((backend (make-counting-test-backend :cols 20 :rows 5))
         (render-count 0))
    (counting-test-backend-inject-events backend (list (quit-key-event)))
    (ptui.engine.loop:run
     (lambda (state size)
       (declare (ignore state size))
       (incf render-count)
       (ptui.render.buffer:make-buffer 20 5))
     :backend backend
     :fps 120
     :initial-state nil)
    (is (= render-count 1))
    (is (= (counting-test-backend-commit-count backend) 1))
    (is (plusp (length (counting-test-backend-pop-commit-ops backend))))))

(test engine-frame-pacing-stays-near-50ms-at-20fps
  (let ((backend (make-counting-test-backend :cols 16 :rows 3))
        (frame-times '())
        (frame 0))
    (ptui.engine.loop:run
     (lambda (state size)
       (declare (ignore state size))
       (incf frame)
       (push (ptui.util.time:monotonic-ms) frame-times)
       (when (= frame 4)
         (counting-test-backend-inject-events backend (list (quit-key-event))))
       (ptui.render.buffer:make-buffer 16 3))
     :backend backend
     :fps 20
     :initial-state nil)
    (setf frame-times (nreverse frame-times))
    (is (>= (length frame-times) 4))
    (let ((deltas (loop for i from 1 below (length frame-times)
                        collect (- (elt frame-times i)
                                   (elt frame-times (1- i)))))
          (mean 0))
      (setf mean (/ (reduce #'+ deltas) (max 1 (length deltas))))
      (is (<= (- mean 50) 10))
      (is (<= 40 mean 60)))))

(test engine-dispatches-events-to-handler
  (let* ((backend (make-counting-test-backend :cols 20 :rows 5))
         (seen-events '()))
    (counting-test-backend-inject-events backend (list (quit-key-event)))
    (ptui.engine.loop:run
     (lambda (state size)
       (declare (ignore size))
       (setf state (append state '(:frame)))
       (ptui.render.buffer:make-buffer 20 5))
     :backend backend
     :fps 120
     :initial-state '()
     :on-event (lambda (state event)
                 (push event seen-events)
                 (push :called state)))
    (is (= (length seen-events) 1))
    (is (eql (ptui.core.events:key-event-key (first seen-events)) :ctrl-c))))

(test engine-allows-on-event-to-consume-default-quit
  (let* ((backend (make-counting-test-backend :cols 20 :rows 5))
         (render-count 0)
         (seen-dispositions '()))
    (counting-test-backend-inject-events backend (list (quit-key-event)))
    (ptui.engine.loop:run
     (lambda (state size)
       (declare (ignore state size))
       (incf render-count)
       (when (= render-count 2)
         (counting-test-backend-inject-events backend (list (text-key-event "q"))))
       (ptui.render.buffer:make-buffer 20 5))
     :backend backend
     :fps 120
     :initial-state nil
     :on-event (lambda (state event)
                 (declare (ignore state))
                 (let ((key (ptui.core.events:key-event-key event)))
                   (push key seen-dispositions)
                   (if (eql key :ctrl-c)
                       (values nil :consume)
                       nil))))
    (setf seen-dispositions (nreverse seen-dispositions))
    (is (= render-count 2))
    (is (equal seen-dispositions '(:ctrl-c :text)))))

(test engine-updates-layout-after-resize-event
  (let ((backend (make-counting-test-backend :cols 12 :rows 4))
        (frame 0)
        (sizes '()))
    (ptui.engine.loop:run
     (lambda (state size)
       (declare (ignore state))
       (incf frame)
       (push (cons (ptui.core.types:size-cols size) (ptui.core.types:size-rows size))
             sizes)
       (when (= frame 1)
         (counting-test-backend-inject-events
          backend
          (list (ptui.core.events:make-key-event :resize))))
       (when (= frame 2)
         (counting-test-backend-inject-events backend (list (quit-key-event))))
       (ptui.render.buffer:make-buffer (ptui.core.types:size-cols size)
                                      (ptui.core.types:size-rows size)))
     :backend backend
     :fps 20
     :on-event (lambda (state event)
                (declare (ignore state))
                (when (eql (ptui.core.events:key-event-key event) :resize)
                  (setf (counting-test-backend-cols backend) 6
                        (counting-test-backend-rows backend) 2))
                state)
     :initial-state nil)
    (setf sizes (nreverse sizes))
    (is (= (length sizes) 2))
    (is (equal (first sizes) (cons 12 4)))
    (is (equal (second sizes) (cons 6 2)))))

(test engine-commits-draw-ops-on-render
  (let ((backend (make-counting-test-backend :cols 20 :rows 2))
        (frame 0))
    (ptui.engine.loop:run
     (lambda (state size)
       (declare (ignore state size))
       (incf frame)
       (when (= frame 3)
         (counting-test-backend-inject-events backend (list (quit-key-event))))
       (let ((buffer (ptui.render.buffer:make-buffer 20 2)))
         (ptui.render.buffer:buffer-draw-text buffer 0 0 "HELLO")
         buffer))
     :backend backend
     :fps 20
     :initial-state nil)
    (is (= (counting-test-backend-commit-count backend) 3))
    (is (some #'plusp (mapcar #'length
                              (counting-test-backend-pop-commit-ops backend))))))

(test engine-emits-performance-guard-when-diff-ops-spike
  (let ((backend (make-counting-test-backend :cols 80 :rows 30))
        (frame 0))
    (let ((log-output
           (with-output-to-string (err-out)
             (let ((*error-output* err-out))
               (ptui.engine.loop:run
                (lambda (state size)
                  (declare (ignore state size))
                  (incf frame)
                  (when (= frame 1)
                    (counting-test-backend-inject-events
                     backend
                     (list (ptui.core.events:make-key-event :ctrl-c))))
                  (ptui.render.buffer:make-buffer 80 30))
                :backend backend
                :fps 120
                :initial-state nil)))))
      (is (search "render_performance_guard" log-output)))))

(test engine-minimal-ops-on-idle-unchanged-frames
  (let ((backend (make-counting-test-backend :cols 10 :rows 2))
        (frame 0)
        (shared-buffer (ptui.render.buffer:make-buffer 10 2)))
    (ptui.render.buffer:buffer-draw-text shared-buffer 0 0 "A")
    (ptui.engine.loop:run
     (lambda (state size)
       (declare (ignore state size))
       (incf frame)
       (when (= frame 3)
         (counting-test-backend-inject-events backend (list (quit-key-event))))
       shared-buffer)
     :backend backend
     :fps 20
     :initial-state nil)
    (let ((ops-counts (mapcar #'length (counting-test-backend-pop-commit-ops backend))))
      (is (= (length ops-counts) 3))
      (is (plusp (first ops-counts)))
      (is (= (second ops-counts) 0))
      (is (= (third ops-counts) 0)))))

(test engine-step-transition-builds-ordered-effect-plan
  (let* ((snapshot (ptui.engine.loop::make-loop-step-snapshot
                    :quit-requested-p nil
                    :exit-deadline-reached-p nil
                    :needs-redraw-p t
                    :metrics-poll-due-p t))
         (transition (ptui.engine.loop::%evaluate-loop-step-transition snapshot)))
    (is-true (ptui.engine.loop::loop-step-transition-continue-p transition))
    (is (equal (ptui.engine.loop::loop-step-transition-flag-updates transition)
               '((:needs-redraw . nil)
                 (:metrics-poll-due-p . nil))))
    (is (equal (mapcar #'ptui.engine.loop::loop-step-effect-kind
                       (ptui.engine.loop::loop-step-transition-effects transition))
               '(:run-scheduler :render :log-metrics :sleep)))))

(test engine-step-transition-stops-scheduler-and-sleep-after-quit
  (let* ((snapshot (ptui.engine.loop::make-loop-step-snapshot
                    :quit-requested-p t
                    :exit-deadline-reached-p nil
                    :needs-redraw-p t
                    :metrics-poll-due-p t))
         (transition (ptui.engine.loop::%evaluate-loop-step-transition snapshot)))
    (is-false (ptui.engine.loop::loop-step-transition-continue-p transition))
    (is (equal (mapcar #'ptui.engine.loop::loop-step-effect-kind
                       (ptui.engine.loop::loop-step-transition-effects transition))
               '(:render :log-metrics)))))

(defun run-all ()
  (run! 'ptui-engine-suite))
