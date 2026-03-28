(in-package :amoebum)

;;; ---------------------------------------------------------------------------
;;; eBPF/USDT Performance Introspection (I255)
;;;
;;; Lightweight userland probe emission with optional external eBPF consumers,
;;; prebuilt BPF object metadata, and a text/TUI dashboard surface.
;;; ---------------------------------------------------------------------------

(defparameter +usdt-probe-types+
  '(:tool-enter
    :tool-exit
    :tool-call
    :llm-request-start
    :llm-stream-chunk
    :llm-request-end
    :agent-lifecycle
    :gc-start
    :gc-end
    :render-frame
    :event-dispatch)
  "Supported USDT logical probe types emitted by amoebum.")

(defstruct (usdt-probe-event
            (:constructor make-usdt-probe-event
                (&key type
                      (timestamp-ms 0)
                      (duration-ms 0)
                      payload)))
  (type :tool-enter :type keyword)
  (timestamp-ms 0 :type integer)
  (duration-ms 0 :type integer)
  payload)

(defstruct (usdt-bpf-program
            (:constructor make-usdt-bpf-program
                (&key name
                      path
                      description
                      event-types
                      filter)))
  (name "" :type string)
  path
  (description "" :type string)
  (event-types '() :type list)
  filter)

(defparameter *usdt-probes-enabled-p* nil
  "When T, USDT probe events are recorded.")

(defparameter *usdt-probe-capacity* 4096
  "Ring buffer capacity for USDT probe events.")

(defvar *usdt-probe-lock* (bt:make-lock "amoebum-usdt-probe-lock"))
(defvar *usdt-probe-events* (make-array *usdt-probe-capacity* :initial-element nil))
(defvar *usdt-probe-write-index* 0)
(defvar *usdt-probe-count* 0)

(defvar *usdt-gc-hooks-installed-p* nil)
(defvar *usdt-gc-start-ms* 0)
(defvar *usdt-last-gc-run-time* 0)

(defparameter *usdt-prebuilt-bpf-directory* nil
  "Optional override directory for shipped prebuilt .bpf.o artifacts.")

(defparameter *usdt-prebuilt-bpf-specs*
  '((:name :tool-latency
     :file "tool-latency.bpf.o"
     :description "Tool latency histogram via tool-exit probes."
     :event-types (:tool-exit))
    (:name :llm-latency
     :file "llm-latency.bpf.o"
     :description "LLM request latency tracing via llm-request-end probes."
     :event-types (:llm-request-end))
    (:name :gc-pause
     :file "gc-pause.bpf.o"
     :description "GC pause measurement via gc-end probes."
     :event-types (:gc-end))
    (:name :event-dispatch
     :file "event-dispatch.bpf.o"
     :description "Event bus dispatch timing via event-dispatch probes."
     :event-types (:event-dispatch)))
  "Built-in shipped BPF artifact metadata.")

(defun %usdt-now-ms ()
  (truncate (* 1000
               (/ (coerce (get-internal-real-time) 'double-float)
                  (coerce internal-time-units-per-second 'double-float)))))

(defun usdt-probes-enabled-p ()
  *usdt-probes-enabled-p*)

(defun usdt-probe-count ()
  (bt:with-lock-held (*usdt-probe-lock*)
    *usdt-probe-count*))

(defun %reset-usdt-ring! ()
  (bt:with-lock-held (*usdt-probe-lock*)
    (setf *usdt-probe-events* (make-array *usdt-probe-capacity* :initial-element nil)
          *usdt-probe-write-index* 0
          *usdt-probe-count* 0))
  t)

(defun set-usdt-probe-capacity (capacity)
  (unless (and (integerp capacity) (> capacity 0))
    (error "USDT probe capacity must be a positive integer, got ~S." capacity))
  (setf *usdt-probe-capacity* capacity)
  (%reset-usdt-ring!))

(defun clear-usdt-probe-events ()
  (%reset-usdt-ring!))

(defun %record-usdt-probe-event (type duration-ms payload)
  (let ((event (make-usdt-probe-event
                :type type
                :timestamp-ms (%usdt-now-ms)
                :duration-ms (max 0 (truncate (or duration-ms 0)))
                :payload payload)))
    (bt:with-lock-held (*usdt-probe-lock*)
      (setf (svref *usdt-probe-events* *usdt-probe-write-index*) event
            *usdt-probe-write-index*
            (mod (1+ *usdt-probe-write-index*) *usdt-probe-capacity*))
      (when (< *usdt-probe-count* *usdt-probe-capacity*)
        (incf *usdt-probe-count*)))
    event))

(declaim (inline usdt-probe-tool-enter
                 usdt-probe-tool-exit
                 usdt-probe-tool-call
                 usdt-probe-llm-request-start
                 usdt-probe-llm-stream-chunk
                 usdt-probe-llm-request-end
                 usdt-probe-agent-lifecycle
                 usdt-probe-gc-start
                 usdt-probe-gc-end
                 usdt-probe-render-frame
                 usdt-probe-event-dispatch))

(defun usdt-probe-tool-enter (tool-name request-id)
  (when *usdt-probes-enabled-p*
    (%record-usdt-probe-event :tool-enter
                              0
                              (list :tool-name tool-name
                                    :request-id request-id)))
  nil)

(defun usdt-probe-tool-exit (tool-name request-id elapsed-ms &key (status :ok))
  (when *usdt-probes-enabled-p*
    (%record-usdt-probe-event :tool-exit
                              elapsed-ms
                              (list :tool-name tool-name
                                    :request-id request-id
                                    :status status)))
  nil)

(defun usdt-probe-tool-call (phase tool-name tool-call-id
                             &key request-id index (status :observed))
  (when *usdt-probes-enabled-p*
    (%record-usdt-probe-event :tool-call
                              0
                              (list :phase phase
                                    :tool-name tool-name
                                    :tool-call-id tool-call-id
                                    :request-id request-id
                                    :index index
                                    :status status)))
  nil)

(defun usdt-probe-llm-request-start (model base-url mode request-id)
  (when *usdt-probes-enabled-p*
    (%record-usdt-probe-event :llm-request-start
                              0
                              (list :model model
                                    :base-url base-url
                                    :mode mode
                                    :request-id request-id)))
  nil)

(defun usdt-probe-llm-stream-chunk (model base-url mode request-id chunk-index chunk-text
                                    &key (chunk-kind :content)
                                      total-chunks
                                      total-chars)
  (let ((chunk-char-count (if (stringp chunk-text) (length chunk-text) 0)))
    (when *usdt-probes-enabled-p*
      (%record-usdt-probe-event :llm-stream-chunk
                                0
                                (list :model model
                                      :base-url base-url
                                      :mode mode
                                      :request-id request-id
                                      :chunk-kind chunk-kind
                                      :chunk-index chunk-index
                                      :chunk-char-count chunk-char-count
                                      :total-chunks (or total-chunks chunk-index)
                                      :total-chars (or total-chars chunk-char-count)))))
  nil)

(defun usdt-probe-llm-request-end (model base-url mode request-id elapsed-ms &key (status :ok))
  (when *usdt-probes-enabled-p*
    (%record-usdt-probe-event :llm-request-end
                              elapsed-ms
                              (list :model model
                                    :base-url base-url
                                    :mode mode
                                    :request-id request-id
                                    :status status)))
  nil)

(defun usdt-probe-agent-lifecycle (phase agent-id agent-type status elapsed-ms
                                   &key parent-message-id)
  (when *usdt-probes-enabled-p*
    (%record-usdt-probe-event :agent-lifecycle
                              elapsed-ms
                              (list :phase phase
                                    :agent-id agent-id
                                    :agent-type agent-type
                                    :status status
                                    :parent-message-id parent-message-id)))
  nil)

(defun usdt-probe-gc-start ()
  (when *usdt-probes-enabled-p*
    (%record-usdt-probe-event :gc-start 0 nil))
  nil)

(defun usdt-probe-gc-end (elapsed-ms dynamic-usage)
  (when *usdt-probes-enabled-p*
    (%record-usdt-probe-event :gc-end
                              elapsed-ms
                              (list :dynamic-usage dynamic-usage
                                    :dynamic-usage-mb (/ (or dynamic-usage 0) 1048576.0d0))))
  nil)

(defun usdt-probe-render-frame (frame-index elapsed-ms cols rows)
  (when *usdt-probes-enabled-p*
    (%record-usdt-probe-event :render-frame
                              elapsed-ms
                              (list :frame-index frame-index
                                    :cols cols
                                    :rows rows)))
  nil)

(defun usdt-probe-event-dispatch (event-type subscription-id elapsed-ms)
  (when *usdt-probes-enabled-p*
    (%record-usdt-probe-event :event-dispatch
                              elapsed-ms
                              (list :event-type event-type
                                    :subscription-id subscription-id)))
  nil)

(defun usdt-probe-events (&key limit type)
  (let ((events
          (bt:with-lock-held (*usdt-probe-lock*)
            (let ((items '()))
              (when (plusp *usdt-probe-count*)
                (let ((start-index (if (< *usdt-probe-count* *usdt-probe-capacity*)
                                       0
                                       *usdt-probe-write-index*)))
                  (dotimes (offset *usdt-probe-count*)
                    (let* ((index (mod (+ start-index offset) *usdt-probe-capacity*))
                           (event (svref *usdt-probe-events* index)))
                      (when event
                        (push event items))))))
              (nreverse items)))))
    (let* ((filtered (if type
                         (remove-if-not (lambda (event)
                                          (eq (usdt-probe-event-type event) type))
                                        events)
                         events))
           (count (length filtered))
           (resolved-limit (and (integerp limit) (> limit 0) (min limit count))))
      (if resolved-limit
          (nthcdr (max 0 (- count resolved-limit)) filtered)
          filtered))))

(defun %usdt-after-gc-hook ()
  (when *usdt-probes-enabled-p*
    (let* ((now (%usdt-now-ms))
           (elapsed (if (plusp *usdt-gc-start-ms*)
                        (max 0 (- now *usdt-gc-start-ms*))
                        (let* ((current-run-time #+sbcl sb-ext:*gc-run-time* #-sbcl 0)
                               (delta-run-time (max 0 (- current-run-time *usdt-last-gc-run-time*))))
                          (setf *usdt-last-gc-run-time* current-run-time)
                          (round (* 1000.0d0
                                    (/ delta-run-time
                                       (max 1 internal-time-units-per-second)))))))
           (usage #+sbcl (sb-kernel:dynamic-usage) #-sbcl 0))
      (usdt-probe-gc-start)
      (usdt-probe-gc-end elapsed usage)
      (setf *usdt-gc-start-ms* 0)))
  nil)

(defun install-usdt-gc-hooks ()
  #+sbcl
  (unless *usdt-gc-hooks-installed-p*
    (push #'%usdt-after-gc-hook sb-ext:*after-gc-hooks*)
    (setf *usdt-last-gc-run-time* sb-ext:*gc-run-time*)
    (setf *usdt-gc-hooks-installed-p* t))
  *usdt-gc-hooks-installed-p*)

(defun uninstall-usdt-gc-hooks ()
  #+sbcl
  (when *usdt-gc-hooks-installed-p*
    (setf sb-ext:*after-gc-hooks*
          (remove #'%usdt-after-gc-hook sb-ext:*after-gc-hooks*))
    (setf *usdt-gc-hooks-installed-p* nil
          *usdt-last-gc-run-time* 0))
  *usdt-gc-hooks-installed-p*)

(defun enable-usdt-probes (&key (install-gc-hooks t) (clear-existing nil))
  (when clear-existing
    (clear-usdt-probe-events))
  (setf *usdt-probes-enabled-p* t)
  (when install-gc-hooks
    (install-usdt-gc-hooks))
  t)

(defun disable-usdt-probes (&key (remove-gc-hooks nil))
  (setf *usdt-probes-enabled-p* nil)
  (when remove-gc-hooks
    (uninstall-usdt-gc-hooks))
  nil)

(defun %percentile (values percentile)
  (if (null values)
      0
      (let* ((sorted (sort (copy-list values) #'<))
             (max-index (1- (length sorted)))
             (ratio (min 100 (max 0 percentile)))
             (index (round (* (/ ratio 100.0d0) max-index))))
        (nth (max 0 (min max-index index)) sorted))))

(defun %histogram-labels (buckets)
  (let ((sorted (sort (remove-duplicates (copy-list buckets) :test #'=) #'<)))
    (append (mapcar (lambda (bound)
                      (format nil "<=~Dms" bound))
                    sorted)
            (list (format nil ">~Dms" (if sorted (car (last sorted)) 0))))))

(defun %bucketize-durations (durations buckets)
  (let* ((sorted (sort (remove-duplicates (copy-list buckets) :test #'=) #'<))
         (counts (make-array (1+ (length sorted)) :initial-element 0)))
    (dolist (duration durations)
      (let ((placed nil))
        (loop for bound in sorted
              for index from 0 do
                (when (<= duration bound)
                  (incf (aref counts index))
                  (setf placed t)
                  (return)))
        (unless placed
          (incf (aref counts (length sorted))))))
    counts))

(defun usdt-latency-histogram (probe-type &key (limit 1000)
                                            (buckets '(1 2 5 10 20 50 100 250 500 1000)))
  (let* ((events (usdt-probe-events :limit limit :type probe-type))
         (durations (loop for event in events
                          for duration = (usdt-probe-event-duration-ms event)
                          when (and (numberp duration) (>= duration 0))
                            collect (truncate duration)))
         (labels (%histogram-labels buckets))
         (counts (%bucketize-durations durations buckets)))
    (loop for label in labels
          for index from 0
          collect (list :label label
                        :count (aref counts index)))))

(defun usdt-gc-pause-summary (&key (limit 1000))
  (let* ((events (usdt-probe-events :limit limit :type :gc-end))
         (pauses (loop for event in events
                       for duration = (usdt-probe-event-duration-ms event)
                       when (and (numberp duration) (>= duration 0))
                         collect (truncate duration)))
         (count (length pauses))
         (total (if pauses (reduce #'+ pauses) 0))
         (avg (if (plusp count) (/ total count) 0))
         (maxv (if pauses (reduce #'max pauses) 0))
         (p95 (%percentile pauses 95)))
    (list :count count
          :avg-ms avg
          :p95-ms p95
          :max-ms maxv)))

(defun usdt-dashboard-snapshot (&key (limit 1000))
  (list :enabled-p (usdt-probes-enabled-p)
        :event-count (usdt-probe-count)
        :tool-latency (usdt-latency-histogram :tool-exit :limit limit)
        :llm-latency (usdt-latency-histogram :llm-request-end :limit limit)
        :render-latency (usdt-latency-histogram :render-frame :limit limit)
        :event-dispatch-latency (usdt-latency-histogram :event-dispatch :limit limit)
        :gc-pauses (usdt-gc-pause-summary :limit limit)))

(defun %render-histogram-section (title rows &key (bar-width 24))
  (let* ((max-count (max 1 (loop for row in rows maximize (getf row :count 0))))
         (label-width (max 10 (loop for row in rows maximize (length (getf row :label ""))))))
    (with-output-to-string (out)
      (format out "~A~%" title)
      (dolist (row rows)
        (let* ((label (getf row :label ""))
               (count (getf row :count 0))
               (bar-len (if (<= max-count 0)
                            0
                            (round (* bar-width (/ count (max 1.0d0 max-count))))))
               (bar (make-string (max 0 bar-len) :initial-element #\#)))
          (format out "  ~vA | ~vA | ~D~%"
                  label-width label bar-width bar count))))))

(defun render-usdt-dashboard (&key (limit 1000))
  (let* ((snapshot (usdt-dashboard-snapshot :limit limit))
         (gc (getf snapshot :gc-pauses))
         (enabled (if (getf snapshot :enabled-p) "enabled" "disabled")))
    (with-output-to-string (out)
      (format out "USDT Dashboard (~A)~%" enabled)
      (format out "Events captured: ~D~%~%" (getf snapshot :event-count 0))
      (format out "~A~%"
              (%render-histogram-section "Tool latency"
                                         (getf snapshot :tool-latency)))
      (format out "~A~%"
              (%render-histogram-section "LLM request latency"
                                         (getf snapshot :llm-latency)))
      (format out "~A~%"
              (%render-histogram-section "Render frame latency"
                                         (getf snapshot :render-latency)))
      (format out "~A~%"
              (%render-histogram-section "Event dispatch latency"
                                         (getf snapshot :event-dispatch-latency)))
      (format out "GC pauses: count=~D avg=~Dms p95=~Dms max=~Dms~%"
              (getf gc :count 0)
              (truncate (getf gc :avg-ms 0))
              (getf gc :p95-ms 0)
              (getf gc :max-ms 0)))))

(defun %usdt-dashboard-line-elements (&key (limit 1000))
  (loop for line in (uiop:split-string (render-usdt-dashboard :limit limit)
                                       :separator '(#\Newline))
        for index from 0
        collect (ptui.ui.elements:make-element
                 :text
                 :id (intern (format nil "USDT-DASHBOARD-LINE-~D" index) :keyword)
                 :props (list :text line
                              :role (if (zerop index) :meta :body))
                 :children '())))

(ptui.widgets.defwidget:defwidget usdt-dashboard-widget (state)
  (:memoize :equal)
  (let ((limit (or (getf state :limit) 1000)))
    (box
     (vstack
      (map-widget #'identity
                  (%usdt-dashboard-line-elements :limit limit)))
     :id :usdt-dashboard
     :border t)))

(defun %default-usdt-prebuilt-bpf-directory ()
  (or *usdt-prebuilt-bpf-directory*
      (let ((root (ignore-errors (asdf:system-source-directory "amoebum"))))
        (and root
             (merge-pathnames #P"contrib/bpf/"
                              (uiop:ensure-directory-pathname root))))
      (merge-pathnames #P"amoebum/contrib/bpf/"
                       (uiop:ensure-directory-pathname
                        (ignore-errors (uiop:getcwd))))))

(defun %resolve-bpf-path (spec)
  (let* ((directory (%default-usdt-prebuilt-bpf-directory))
         (file (getf spec :file)))
    (and directory file
         (merge-pathnames (pathname file)
                          (uiop:ensure-directory-pathname directory)))))

(defun list-prebuilt-bpf-programs ()
  (mapcar (lambda (spec)
            (let* ((path (%resolve-bpf-path spec))
                   (resolved (and path (probe-file path))))
              (list :name (getf spec :name)
                    :path (or resolved path)
                    :exists-p (not (null resolved))
                    :description (getf spec :description)
                    :event-types (copy-list (getf spec :event-types)))))
          *usdt-prebuilt-bpf-specs*))

(defun %normalize-bpf-name (name)
  (cond
    ((keywordp name) name)
    ((symbolp name) (intern (string-upcase (symbol-name name)) :keyword))
    ((stringp name) (intern (string-upcase name) :keyword))
    (t (intern (string-upcase (princ-to-string name)) :keyword))))

(defun %find-prebuilt-bpf-spec (name)
  (find (%normalize-bpf-name name)
        *usdt-prebuilt-bpf-specs*
        :test #'eq
        :key (lambda (spec) (getf spec :name))))

(defun load-prebuilt-bpf-program (name &key filter)
  (let* ((spec (%find-prebuilt-bpf-spec name)))
    (unless spec
      (error "Unknown prebuilt BPF program ~S." name))
    (let* ((path (%resolve-bpf-path spec))
           (resolved (and path (probe-file path))))
      (unless resolved
        (error "Prebuilt BPF program file not found: ~A" path))
      (make-usdt-bpf-program
       :name (string-downcase (symbol-name (getf spec :name)))
       :path resolved
       :description (or (getf spec :description) "")
       :event-types (copy-list (getf spec :event-types))
       :filter filter))))

(defun bpf-program-filter-events (program events)
  (check-type program usdt-bpf-program)
  (let ((event-types (usdt-bpf-program-event-types program))
        (filter (usdt-bpf-program-filter program)))
    (remove-if-not
     (lambda (event)
       (and (typep event 'usdt-probe-event)
            (or (null event-types)
                (member (usdt-probe-event-type event) event-types :test #'eq))
            (or (null filter)
                (funcall filter event))))
     events)))

(defun usdt-disabled-overhead-percent (&key (iterations 120000)
                                          (operations-per-iteration 256)
                                          (probe-every 256)
                                          (rounds 5))
  (let ((saved-enabled *usdt-probes-enabled-p*))
    (unwind-protect
        (progn
          (setf *usdt-probes-enabled-p* nil)
          (labels ((%noop-probe (_tool-name _request-id)
                     (declare (ignore _tool-name _request-id))
                     nil)
                   (timed-workload (n probe-fn)
                     (let ((start (get-internal-run-time))
                           (acc 0)
                           (sink 0))
                       (declare (ignorable sink))
                       (dotimes (idx n)
                         (dotimes (step operations-per-iteration)
                           (setf acc (logand #xfffffff
                                             (+ acc
                                                (ldb (byte 12 0)
                                                     (* (+ 1 idx step) 2654435761))))))
                         (when (or (<= probe-every 1)
                                   (zerop (mod idx probe-every)))
                           (funcall probe-fn "bench" nil)))
                       (setf sink acc)
                       (- (get-internal-run-time) start)))
                   (round-overhead-percent (baseline instrumented)
                     (let ((delta (max 0 (- instrumented baseline))))
                       (* 100.0d0 (/ delta (max 1 baseline))))))
            ;; Warm up both branches once so the disabled-probe benchmark is less
            ;; sensitive to first-call effects and transient scheduler noise.
            (timed-workload (max 1024 (truncate iterations 16)) #'%noop-probe)
            (timed-workload (max 1024 (truncate iterations 16)) #'usdt-probe-tool-enter)
            (let ((samples '()))
              (dotimes (round (max 1 rounds))
                (let* ((instrument-first-p (oddp round))
                       (first-run (timed-workload
                                   iterations
                                   (if instrument-first-p
                                       #'usdt-probe-tool-enter
                                       #'%noop-probe)))
                       (second-run (timed-workload
                                    iterations
                                    (if instrument-first-p
                                        #'%noop-probe
                                        #'usdt-probe-tool-enter)))
                       (baseline (if instrument-first-p second-run first-run))
                       (instrumented (if instrument-first-p first-run second-run)))
                  (push (round-overhead-percent baseline instrumented) samples)))
              (reduce #'min samples))))
      (setf *usdt-probes-enabled-p* saved-enabled))))
