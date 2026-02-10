(defpackage :ptui.util.time
  (:use :cl)
  (:export #:monotonic-ms #:sleep-ms))

(in-package :ptui.util.time)

(defun monotonic-ms ()
  "Return monotonic milliseconds from the process clock."
  (truncate (* 1000
               (/ (get-internal-real-time)
                  internal-time-units-per-second))))

(defun sleep-ms (ms)
  "Sleep for MS milliseconds."
  (sleep (/ (max 0 ms) 1000.0))
  nil)
