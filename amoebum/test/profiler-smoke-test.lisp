(in-package :amoebum/test)

;;; ---------------------------------------------------------------------------
;;; Profiler Smoke Tests (I101)
;;; ---------------------------------------------------------------------------

(def-suite profiler-suite :in amoebum-suite
  :description "Performance profiling smoke tests.")

(in-suite profiler-suite)

(test metrics-entry-creation
  (let ((entry (amoebum::make-metrics-entry :kind :tool-call
                                             :name "test-tool"
                                             :duration-ms 42)))
    (is (amoebum::metrics-entry-p entry))
    (is (eq :tool-call (amoebum::metrics-entry-kind entry)))
    (is (string= "test-tool" (amoebum::metrics-entry-name entry)))
    (is (= 42 (amoebum::metrics-entry-duration-ms entry)))))

(test metrics-store-creation
  (let ((store (amoebum::make-metrics-store :capacity 16)))
    (is (amoebum::metrics-store-p store))
    (is (= 16 (amoebum::metrics-store-capacity store)))
    (is (= 0 (amoebum::metrics-store-count store)))))

(test metrics-store-push-and-recent
  (let ((store (amoebum::make-metrics-store :capacity 16)))
    (amoebum::metrics-store-push store
      (amoebum::make-metrics-entry :kind :test :name "a" :duration-ms 10))
    (amoebum::metrics-store-push store
      (amoebum::make-metrics-entry :kind :test :name "b" :duration-ms 20))
    (is (= 2 (amoebum::metrics-store-count store)))
    (let ((recent (amoebum::metrics-store-recent store)))
      (is (= 2 (length recent))))))

(test metrics-store-ring-buffer-overflow
  (let ((store (amoebum::make-metrics-store :capacity 4)))
    (dotimes (i 10)
      (amoebum::metrics-store-push store
        (amoebum::make-metrics-entry :kind :test
                                      :name (format nil "entry-~A" i)
                                      :duration-ms i)))
    (is (= 4 (amoebum::metrics-store-count store)))
    (let ((recent (amoebum::metrics-store-recent store :limit 10)))
      (is (<= (length recent) 4)))))

(test metrics-store-filter-by-kind
  (let ((store (amoebum::make-metrics-store)))
    (amoebum::metrics-store-push store
      (amoebum::make-metrics-entry :kind :tool-call :name "tool" :duration-ms 10))
    (amoebum::metrics-store-push store
      (amoebum::make-metrics-entry :kind :gc :name "gc" :duration-ms 5))
    (let ((tool-entries (amoebum::metrics-store-recent store :kind :tool-call)))
      (is (= 1 (length tool-entries))))
    (let ((gc-entries (amoebum::metrics-store-recent store :kind :gc)))
      (is (= 1 (length gc-entries))))))

(test metrics-store-clear
  (let ((store (amoebum::make-metrics-store)))
    (amoebum::metrics-store-push store
      (amoebum::make-metrics-entry :kind :test :name "x" :duration-ms 1))
    (amoebum::metrics-store-clear store)
    (is (= 0 (amoebum::metrics-store-count store)))
    (is (null (amoebum::metrics-store-recent store)))))

(test record-metric-global
  (let ((amoebum::*global-metrics-store* (amoebum::make-metrics-store)))
    (amoebum::record-metric :tool-call "test-tool" 42)
    (is (= 1 (amoebum::metrics-store-count amoebum::*global-metrics-store*)))))

(test with-tool-timing-macro
  (let ((amoebum::*global-metrics-store* (amoebum::make-metrics-store)))
    (let ((result (amoebum::with-tool-timing ("test-timing")
                    (+ 1 2))))
      (is (= 3 result))
      (is (= 1 (amoebum::metrics-store-count amoebum::*global-metrics-store*))))))

(test memory-statistics
  (let ((stats (amoebum::memory-statistics)))
    (is (listp stats))
    (is (numberp (getf stats :dynamic-usage)))
    (is (numberp (getf stats :dynamic-usage-mb)))))

(test metrics-summary
  (let ((amoebum::*global-metrics-store* (amoebum::make-metrics-store)))
    (amoebum::record-metric :tool-call "tool-a" 10)
    (amoebum::record-metric :tool-call "tool-b" 20)
    (amoebum::record-metric :gc "gc" 5)
    (let ((summary (amoebum::metrics-summary)))
      (is (stringp summary))
      (is (search "Metrics Summary" summary))
      (is (search "tool-call" summary :test #'char-equal)))))

(test gc-telemetry-enable-disable
  (let ((amoebum::*gc-metrics-enabled-p* nil))
    (amoebum::enable-gc-telemetry)
    (is (eq t amoebum::*gc-metrics-enabled-p*))
    (amoebum::disable-gc-telemetry)
    (is (eq nil amoebum::*gc-metrics-enabled-p*))))

(test profiler-start-stop
  "sb-sprof start/stop should not crash."
  (handler-case
      (progn
        (amoebum::profiler-start)
        (amoebum::profiler-stop)
        (is t))
    (error ()
      (is t "Profiler not available, which is OK."))))

(test perf-sparkline
  (let ((entries (list (amoebum::make-metrics-entry :kind :test :name "a" :duration-ms 10)
                       (amoebum::make-metrics-entry :kind :test :name "b" :duration-ms 50)
                       (amoebum::make-metrics-entry :kind :test :name "c" :duration-ms 0))))
    (let ((sparkline (amoebum::%perf-sparkline entries)))
      (is (stringp sparkline))
      (is (= 3 (length sparkline))))))

(test perf-sparkline-empty
  (is (string= "No data." (amoebum::%perf-sparkline nil))))
