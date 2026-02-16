(in-package :amoebum)

;;; ---------------------------------------------------------------------------
;;; Performance Profiling (I101)
;;;
;;; sb-sprof integration, GC telemetry, tool-call timing, metrics ring buffer.
;;; ---------------------------------------------------------------------------

;;; --- Metrics Store (Ring Buffer) ---

(defstruct (metrics-entry
            (:constructor make-metrics-entry
                (&key kind name
                      (timestamp (get-internal-real-time))
                      duration-ms
                      (metadata nil))))
  (kind :unknown :type keyword)
  (name "" :type string)
  (timestamp (get-internal-real-time) :type integer)
  (duration-ms 0 :type number)
  (metadata nil :type list))

(defstruct (metrics-store
            (:constructor %make-metrics-store))
  (entries (make-array 1024 :initial-element nil) :type simple-vector)
  (capacity 1024 :type integer)
  (write-index 0 :type integer)
  (count 0 :type integer)
  (lock (bt:make-lock "metrics-store-lock")))

(defun make-metrics-store (&key (capacity 1024))
  "Create a ring-buffer metrics store."
  (%make-metrics-store
   :entries (make-array capacity :initial-element nil)
   :capacity capacity))

(defun metrics-store-push (store entry)
  "Push a metrics entry into the ring buffer."
  (bt:with-lock-held ((metrics-store-lock store))
    (setf (svref (metrics-store-entries store) (metrics-store-write-index store))
          entry)
    (setf (metrics-store-write-index store)
          (mod (1+ (metrics-store-write-index store))
               (metrics-store-capacity store)))
    (when (< (metrics-store-count store) (metrics-store-capacity store))
      (incf (metrics-store-count store))))
  entry)

(defun metrics-store-recent (store &key (limit 50) (kind nil))
  "Return recent metrics entries, optionally filtered by KIND."
  (bt:with-lock-held ((metrics-store-lock store))
    (let ((result '())
          (n (min limit (metrics-store-count store)))
          (idx (metrics-store-write-index store))
          (cap (metrics-store-capacity store)))
      (dotimes (i n)
        (let* ((pos (mod (- idx 1 i) cap))
               (entry (svref (metrics-store-entries store) pos)))
          (when entry
            (when (or (null kind) (eq kind (metrics-entry-kind entry)))
              (push entry result)))))
      result)))

(defun metrics-store-clear (store)
  "Clear all entries."
  (bt:with-lock-held ((metrics-store-lock store))
    (dotimes (i (metrics-store-capacity store))
      (setf (svref (metrics-store-entries store) i) nil))
    (setf (metrics-store-write-index store) 0
          (metrics-store-count store) 0)))

;;; --- Global Metrics ---

(defvar *global-metrics-store* (make-metrics-store)
  "Global metrics store for all profiling data.")

(defun record-metric (kind name duration-ms &optional metadata)
  "Record a metric in the global store."
  (metrics-store-push *global-metrics-store*
                      (make-metrics-entry :kind kind
                                          :name name
                                          :duration-ms duration-ms
                                          :metadata metadata)))

;;; --- Tool Call Timing ---

(defmacro with-tool-timing ((tool-name) &body body)
  "Execute BODY and record timing metrics for TOOL-NAME."
  (let ((start-var (gensym "START"))
        (result-var (gensym "RESULT")))
    `(let ((,start-var (get-internal-real-time)))
       (let ((,result-var (progn ,@body)))
         (let ((elapsed-ms (round (* 1000.0
                                      (/ (- (get-internal-real-time) ,start-var)
                                         internal-time-units-per-second)))))
           (record-metric :tool-call ,tool-name elapsed-ms))
         ,result-var))))

;;; --- GC Telemetry ---

(defvar *gc-metrics-enabled-p* nil
  "When T, GC events are recorded to the metrics store.")

(defvar *gc-hook-installed-p* nil
  "Track whether the GC hook has been installed.")

(defun %gc-telemetry-hook ()
  "Hook called after GC to record metrics."
  (when *gc-metrics-enabled-p*
    (let ((usage #+sbcl (sb-kernel:dynamic-usage) #-sbcl 0))
      (record-metric :gc "gc-after"
                     0
                     (list :dynamic-usage usage
                           :dynamic-usage-mb (/ usage 1048576.0))))))

(defun enable-gc-telemetry ()
  "Enable GC telemetry recording."
  (setf *gc-metrics-enabled-p* t)
  #+sbcl
  (unless *gc-hook-installed-p*
    (push #'%gc-telemetry-hook sb-ext:*after-gc-hooks*)
    (setf *gc-hook-installed-p* t))
  t)

(defun disable-gc-telemetry ()
  "Disable GC telemetry recording."
  (setf *gc-metrics-enabled-p* nil)
  #+sbcl
  (when *gc-hook-installed-p*
    (setf sb-ext:*after-gc-hooks*
          (remove #'%gc-telemetry-hook sb-ext:*after-gc-hooks*))
    (setf *gc-hook-installed-p* nil))
  nil)

;;; --- sb-sprof Integration ---

(defvar *profiler-running-p* nil
  "Track whether sb-sprof is currently running.")

(defun profiler-start (&key (mode :cpu) (sample-interval 0.01))
  "Start the statistical profiler."
  #+sbcl
  (progn
    (sb-sprof:start-profiling :mode mode :sample-interval sample-interval)
    (setf *profiler-running-p* t))
  #-sbcl
  (warn "sb-sprof not available on this implementation.")
  t)

(defun profiler-stop ()
  "Stop the statistical profiler."
  #+sbcl
  (when *profiler-running-p*
    (sb-sprof:stop-profiling)
    (setf *profiler-running-p* nil))
  t)

(defun profiler-report (&key (stream *standard-output*) (type :flat))
  "Generate a profiler report."
  #+sbcl
  (sb-sprof:report :stream stream :type type)
  #-sbcl
  (format stream "Profiler not available.~%")
  t)

(defun profiler-reset ()
  "Reset profiler data."
  #+sbcl
  (sb-sprof:reset)
  t)

;;; --- Memory Statistics ---

(defun memory-statistics ()
  "Return current memory statistics as a plist."
  #+sbcl
  (list :dynamic-usage (sb-kernel:dynamic-usage)
        :dynamic-usage-mb (/ (sb-kernel:dynamic-usage) 1048576.0)
        :gc-run-time (/ sb-ext:*gc-run-time* internal-time-units-per-second))
  #-sbcl
  (list :dynamic-usage 0 :dynamic-usage-mb 0.0 :gc-run-time 0))

;;; --- Metrics Summary ---

(defun metrics-summary (&key (limit 100))
  "Return a summary of recent metrics."
  (let* ((entries (metrics-store-recent *global-metrics-store* :limit limit))
         (by-kind (make-hash-table :test #'eq)))
    (dolist (entry entries)
      (push entry (gethash (metrics-entry-kind entry) by-kind)))
    (with-output-to-string (out)
      (format out "Metrics Summary (~A entries):~%" (length entries))
      (maphash
       (lambda (kind entries)
         (let ((count (length entries))
               (total-ms (reduce #'+ entries :key #'metrics-entry-duration-ms))
               (max-ms (reduce #'max entries :key #'metrics-entry-duration-ms)))
           (format out "  ~A: ~A calls, ~,1Fms total, ~,1Fms avg, ~,1Fms max~%"
                   kind count total-ms (/ total-ms (max 1 count)) max-ms)))
       by-kind)
      (let ((mem (memory-statistics)))
        (format out "  Memory: ~,2F MB dynamic~%" (getf mem :dynamic-usage-mb))))))
