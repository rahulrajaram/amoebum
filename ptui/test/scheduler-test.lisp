(defpackage :ptui.test.scheduler
  (:use :cl :fiveam)
  (:export #:scheduler-suite))

(in-package :ptui.test.scheduler)

(def-suite scheduler-suite
  :description "PTUI scheduler tests.")

(in-suite scheduler-suite)

(test schedule-once-fires-after-delay
  (let* ((sched (ptui.runtime.scheduler:make-scheduler))
         (fired 0))
    (ptui.runtime.scheduler:schedule-once sched 0 (lambda () (incf fired)))
    (ptui.runtime.scheduler:scheduler-run-due sched)
    (is (= 1 fired))
    ;; Does not repeat
    (ptui.runtime.scheduler:scheduler-run-due sched)
    (is (= 1 fired))))

(test schedule-interval-fires-repeatedly
  (let* ((sched (ptui.runtime.scheduler:make-scheduler))
         (fired 0))
    (ptui.runtime.scheduler:schedule-interval sched 0 (lambda () (incf fired)))
    (ptui.runtime.scheduler:scheduler-run-due sched)
    (is (>= fired 1))
    ;; Run again — interval task should have been re-queued
    (ptui.runtime.scheduler:scheduler-run-due sched)
    (is (>= fired 2))))

(test cancel-task-prevents-future-fires
  (let* ((sched (ptui.runtime.scheduler:make-scheduler))
         (fired 0))
    (let ((task-id (ptui.runtime.scheduler:schedule-interval sched 0 (lambda () (incf fired)))))
      (ptui.runtime.scheduler:cancel-task sched task-id))
    (ptui.runtime.scheduler:scheduler-run-due sched)
    (is (= 0 fired))))

(test scheduler-next-timeout-ms-empty
  (let ((sched (ptui.runtime.scheduler:make-scheduler)))
    (is (null (ptui.runtime.scheduler:scheduler-next-timeout-ms sched)))))

(test scheduler-next-timeout-ms-with-task
  (let ((sched (ptui.runtime.scheduler:make-scheduler)))
    (ptui.runtime.scheduler:schedule-once sched 1000 (lambda () nil))
    (let ((timeout (ptui.runtime.scheduler:scheduler-next-timeout-ms sched)))
      (is (not (null timeout)))
      (is (integerp timeout)))))
