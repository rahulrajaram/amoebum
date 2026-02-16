(in-package :amoebum)

;;; ---------------------------------------------------------------------------
;;; Performance Dashboard Widget (I101)
;;;
;;; defwidget-based sparkline chart for profiling metrics.
;;; ---------------------------------------------------------------------------

(ptui.widgets.defwidget:defwidget perf-dashboard (state)
  (:memoize :equal)
  (let* ((metrics-store (or (getf state :metrics-store) *global-metrics-store*))
         (limit (or (getf state :limit) 20))
         (recent (metrics-store-recent metrics-store :limit limit))
         (mem-stats (memory-statistics)))
    (list
     :type :box
     :direction :vertical
     :children
     (list
      (list :type :text
            :content (format nil "Performance Dashboard — ~A metrics"
                             (length recent)))
      (list :type :text
            :content (format nil "Memory: ~,2F MB dynamic"
                             (getf mem-stats :dynamic-usage-mb 0.0)))
      (list :type :text
            :content (format nil "Profiler: ~A"
                             (if *profiler-running-p* "RUNNING" "stopped")))
      (list :type :text
            :content (%perf-sparkline recent))))))

(defun %perf-sparkline (entries)
  "Generate a simple text sparkline from metrics entries."
  (if (null entries)
      "No data."
      (let* ((durations (mapcar #'metrics-entry-duration-ms entries))
             (max-d (reduce #'max durations))
             (sparkline-chars "▁▂▃▄▅▆▇█"))
        (with-output-to-string (out)
          (dolist (d durations)
            (let ((idx (if (zerop max-d) 0
                           (min 7 (floor (* 7 d) (max 1 max-d))))))
              (write-char (char sparkline-chars idx) out)))))))
