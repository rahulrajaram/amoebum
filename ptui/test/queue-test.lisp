(defpackage :ptui.test.queue
  (:use :cl :fiveam)
  (:export #:queue-suite))

(in-package :ptui.test.queue)

(def-suite queue-suite
  :description "PTUI event-queue tests.")

(in-suite queue-suite)

(test empty-queue-pop-all-returns-nil
  (let ((q (ptui.runtime.queue:make-event-queue)))
    (multiple-value-bind (events count)
        (ptui.runtime.queue:queue-pop-all q)
      (is (null events))
      (is (= 0 count)))))

(test push-then-pop-all-returns-fifo
  (let ((q (ptui.runtime.queue:make-event-queue)))
    (ptui.runtime.queue:queue-push q :a)
    (ptui.runtime.queue:queue-push q :b)
    (ptui.runtime.queue:queue-push q :c)
    (multiple-value-bind (events count)
        (ptui.runtime.queue:queue-pop-all q)
      (is (equal '(:a :b :c) events))
      (is (= 3 count)))))

(test pop-all-drains-queue
  (let ((q (ptui.runtime.queue:make-event-queue)))
    (ptui.runtime.queue:queue-push q :x)
    (ptui.runtime.queue:queue-pop-all q)
    (multiple-value-bind (events count)
        (ptui.runtime.queue:queue-pop-all q)
      (is (null events))
      (is (= 0 count)))))

(test multiple-pushes-single-pop-all
  (let ((q (ptui.runtime.queue:make-event-queue)))
    (dotimes (i 10)
      (ptui.runtime.queue:queue-push q i))
    (multiple-value-bind (events count)
        (ptui.runtime.queue:queue-pop-all q)
      (is (= 10 count))
      (is (equal (loop for i from 0 below 10 collect i)
                 events)))))
