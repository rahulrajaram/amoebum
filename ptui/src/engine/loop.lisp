(defpackage :ptui.engine.loop
  (:use :cl)
  (:export #:run #:main #:*reusable-buffer*))

(in-package :ptui.engine.loop)

(defun %ncurses-backend-available-p ()
  (let ((pkg (find-package "PTUI.BACKEND.NCURSES")))
    (and pkg
         (multiple-value-bind (sym status) (find-symbol "MAKE-NCURSES-BACKEND" pkg)
           (declare (ignore status))
           (and sym (fboundp sym))))))

(defun %make-ncurses-backend-or-die ()
  ;; Avoid hard package references so `ptui/engine` is loadable even when
  ;; `ptui-ncurses` is not in *features* (or fasls were compiled under
  ;; different feature sets).
  (let ((pkg (find-package "PTUI.BACKEND.NCURSES")))
    (unless pkg
      (error "The optional ncurses backend is not available (package PTUI.BACKEND.NCURSES not loaded)."))
    (multiple-value-bind (sym status) (find-symbol "MAKE-NCURSES-BACKEND" pkg)
      (declare (ignore status))
      (unless (and sym (fboundp sym))
        (error "The optional ncurses backend is not available (MAKE-NCURSES-BACKEND not found)."))
      (funcall (symbol-function sym)))))

(defun %resolve-backend-keyword (backend)
  (case backend
    (:auto
     (let* ((caps (ptui.term.caps:probe-terminal-caps))
            (truecolorp (ptui.term.caps:terminal-caps-truecolorp caps)))
       (cond
         (truecolorp :ansi)
         ((%ncurses-backend-available-p) :ncurses)
         (t :ansi))))
    (otherwise backend)))

(defun %make-backend (backend)
  (etypecase backend
    (keyword
     (case (%resolve-backend-keyword backend)
       (:ansi (ptui.backend.ansi:make-ansi-backend))
       (:ncurses
        (%make-ncurses-backend-or-die))
       (otherwise
        (error "Unsupported backend keyword: ~S" backend))))
    (ptui.backend.protocol:terminal-backend
     backend)))

(defun %default-render-fn (state size)
  (declare (ignore state))
  (ptui.render.buffer:make-buffer (ptui.core.types:size-cols size)
                                  (ptui.core.types:size-rows size)))

(defstruct (loop-runtime
            (:constructor %make-loop-runtime
                (backend scheduler render-fn on-event event-bus on-handler-error
                 state prev-buffer needs-redraw metrics-poll-due-p exit-after-ms
                 start-ms stats frame-delay-ms max-idle-sleep-ms)))
  backend
  scheduler
  render-fn
  on-event
  event-bus
  on-handler-error
  (state nil)
  (prev-buffer nil)
  (spare-buffer nil)
  (needs-redraw t)
  (metrics-poll-due-p nil)
  (exit-after-ms nil)
  (start-ms 0 :type integer)
  stats
  (frame-delay-ms 50 :type integer)
  (max-idle-sleep-ms 10 :type integer))

(defstruct (loop-step-snapshot
            (:constructor make-loop-step-snapshot
                (&key quit-requested-p exit-deadline-reached-p
                      needs-redraw-p metrics-poll-due-p)))
  (quit-requested-p nil :type boolean)
  (exit-deadline-reached-p nil :type boolean)
  (needs-redraw-p nil :type boolean)
  (metrics-poll-due-p nil :type boolean))

(defstruct (loop-step-effect
            (:constructor make-loop-step-effect (kind)))
  (kind :noop :type keyword))

(defstruct (loop-step-transition
            (:constructor make-loop-step-transition
                (&key continue-p flag-updates effects)))
  (continue-p t :type boolean)
  (flag-updates '() :type list)
  (effects '() :type list))

(defvar *reusable-buffer* nil
  "When non-NIL, a pre-allocated buffer that render functions may clear and reuse
instead of calling MAKE-BUFFER.  Set by the engine loop before calling RENDER-FN.")

(defun %quit-event-p (event)
  (and (typep event 'ptui.core.events:key-event)
       (or (eql (ptui.core.events:key-event-key event) :ctrl-c)
           (and (eql (ptui.core.events:key-event-key event) :text)
                (string-equal (or (ptui.core.events:key-event-text? event) "") "q")))))

(defun %dispatch-event (state event on-event)
  (if on-event
      (multiple-value-bind (next-state event-disposition)
          (funcall on-event state event)
        (values next-state
                (member event-disposition '(:consume :consumed :handled) :test #'eq)
                (eq event-disposition :quit)))
      (values state nil nil)))

(defun %resolve-event-bus-drain-fn ()
  (let ((pkg (find-package "EVENT-BUS")))
    (and pkg
         (multiple-value-bind (sym status) (find-symbol "DRAIN-AND-DISPATCH" pkg)
           (declare (ignore status))
           (and sym
                (fboundp sym)
                (symbol-function sym))))))

(defun %default-on-handler-error (condition)
  (ptui.util.log:log-warn
   "event_bus_handler_error=~S"
   (princ-to-string condition))
  nil)

(defun %resolve-on-handler-error (on-handler-error)
  (cond
    ((or (null on-handler-error)
         (eq on-handler-error :log-and-continue))
     #'%default-on-handler-error)
    ((functionp on-handler-error)
     on-handler-error)
    (t
     (error "ON-HANDLER-ERROR must be NIL, :LOG-AND-CONTINUE, or a function, got ~S."
            on-handler-error))))

(defun %parse-positive-int-env (name &optional default)
  (let ((raw (uiop:getenv name)))
    (or (and raw
             (ignore-errors
               (let ((value (parse-integer raw)))
                 (when (> value 0)
                   value))))
        default)))

(defun %compute-frame-delay-ms (fps)
  (max 1 (truncate (/ 1000 (max 1 fps)))))

(defun %make-run-runtime (backend render-fn fps initial-state on-event event-bus
                          on-handler-error)
  (let* ((scheduler (ptui.runtime.scheduler:make-scheduler))
         (frame-delay-ms (%compute-frame-delay-ms fps))
         (exit-after-ms (%parse-positive-int-env "PTUI_EXIT_AFTER_MS"))
         (max-idle-sleep-ms (%parse-positive-int-env "PTUI_MAX_IDLE_SLEEP_MS" 10)))
    (%make-loop-runtime
     backend
     scheduler
     render-fn
     on-event
     event-bus
     on-handler-error
     initial-state
     nil
     t
     nil
     exit-after-ms
     (ptui.util.time:monotonic-ms)
     (ptui.util.log:make-render-stats)
     frame-delay-ms
     max-idle-sleep-ms)))

(defun %schedule-loop-timers (runtime)
  (ptui.runtime.scheduler:schedule-interval
   (loop-runtime-scheduler runtime)
   (loop-runtime-frame-delay-ms runtime)
   (lambda ()
     (setf (loop-runtime-needs-redraw runtime) t)))
  (ptui.runtime.scheduler:schedule-interval
   (loop-runtime-scheduler runtime)
   1000
   (lambda ()
     (setf (loop-runtime-metrics-poll-due-p runtime) t))))

(defun %handle-backend-event (runtime event)
  (multiple-value-bind (next-state event-consumed-p event-quit-p)
      (%dispatch-event (loop-runtime-state runtime)
                       event
                       (loop-runtime-on-event runtime))
    (setf (loop-runtime-state runtime) next-state)
    (or event-quit-p
        (and (not event-consumed-p)
             (%quit-event-p event)))))

(defun %poll-events-and-check-quit (runtime)
  (loop for event in (ptui.backend.protocol:backend-poll-events
                      (loop-runtime-backend runtime))
        thereis (%handle-backend-event runtime event)))

(defun %drain-runtime-event-bus (runtime)
  (%drain-event-bus (loop-runtime-event-bus runtime)
                    (loop-runtime-on-handler-error runtime)))

(defun %runtime-perf-threshold-ms (runtime)
  (or (%parse-positive-int-env "PTUI_RENDER_FRAME_MS_THRESHOLD")
      (max 120 (* 3 (loop-runtime-frame-delay-ms runtime)))))

(defun %runtime-perf-diff-threshold ()
  (or (%parse-positive-int-env "PTUI_RENDER_DIFF_OPS_THRESHOLD")
      1800))

(defun %render-guard-workload (old-prev diff-op-count cols rows)
  (if old-prev
      diff-op-count
      (max diff-op-count (* cols rows))))

(defun %perf-guard-enabled-p ()
  (let ((raw (uiop:getenv "PTUI_RENDER_PERF_GUARD")))
    (not (and raw
              (member (string-upcase raw)
                      '("OFF" "0" "FALSE" "DISABLE")
                      :test #'string-equal)))))

(defun %update-render-stats (stats frame-ms diff-op-count commit-bytes guarded?)
  (setf (ptui.util.log:render-stats-frame-count stats)
        (the fixnum (1+ (ptui.util.log:render-stats-frame-count stats)))
        (ptui.util.log:render-stats-last-frame-ms stats)
        (the fixnum frame-ms)
        (ptui.util.log:render-stats-last-diff-ops stats)
        (the fixnum diff-op-count)
        (ptui.util.log:render-stats-last-commit-bytes stats)
        (the fixnum commit-bytes)
        (ptui.util.log:render-stats-last-frame-slow-p stats)
        guarded?
        (ptui.util.log:render-stats-last-perf-guard-fired-p stats)
        guarded?))

(defun %log-render-guard (frame-ms diff-op-count guard-workload
                          perf-threshold-ms perf-diff-threshold)
  (ptui.util.log:log-warn
   "~A"
   (string-downcase
    (ptui.util.log:log-kv
     :render_performance_guard t
     :frame_ms frame-ms
     :diff_ops diff-op-count
     :guard_workload guard-workload
     :frame_ms_threshold perf-threshold-ms
     :diff_ops_threshold perf-diff-threshold
     :slow_p t))))

(defun %log-render-debug (frame-ms diff-op-count commit-bytes)
  (ptui.util.log:log-debug
   "~A"
   (ptui.util.log:log-kv
    :frame_ms frame-ms
    :diff_ops diff-op-count
    :commit_bytes commit-bytes)))

(defun %render-runtime-frame (runtime)
  (let* ((frame-start (ptui.util.time:monotonic-ms))
         (backend (loop-runtime-backend runtime))
         (size (ptui.backend.protocol:backend-size backend))
         (cols (ptui.core.types:size-cols size))
         (rows (ptui.core.types:size-rows size))
         ;; Offer the spare buffer for reuse if dimensions match.
         (spare (loop-runtime-spare-buffer runtime))
         (*reusable-buffer* (when (ptui.render.buffer:buffer-dimensions-match-p spare cols rows)
                              spare))
         (next-buffer (funcall (loop-runtime-render-fn runtime)
                               (loop-runtime-state runtime)
                               size)))
    ;; Recycle: old prev-buffer becomes the spare for next frame.
    (let ((old-prev (loop-runtime-prev-buffer runtime)))
      (multiple-value-bind (draw-ops diff-op-count)
          (ptui.render.diff:diff-buffers old-prev
                                         next-buffer
                                         :full-redraw (null old-prev))
        (let* ((commit-bytes (ptui.backend.protocol:backend-commit backend draw-ops))
               (frame-ms (- (ptui.util.time:monotonic-ms) frame-start))
               (perf-threshold-ms (%runtime-perf-threshold-ms runtime))
               (perf-diff-threshold (%runtime-perf-diff-threshold))
               (guard-workload (%render-guard-workload old-prev diff-op-count cols rows))
               (guarded? (and (%perf-guard-enabled-p)
                              (or (> frame-ms perf-threshold-ms)
                                  (> guard-workload perf-diff-threshold)))))
          ;; Stash old prev as spare (unless render-fn already reused it).
          (setf (loop-runtime-spare-buffer runtime)
                (if (eq old-prev next-buffer) nil old-prev))
          (setf (loop-runtime-prev-buffer runtime) next-buffer)
          (%update-render-stats (loop-runtime-stats runtime)
                                frame-ms
                                diff-op-count
                                commit-bytes
                                guarded?)
          (when guarded?
            (%log-render-guard frame-ms diff-op-count guard-workload
                               perf-threshold-ms perf-diff-threshold))
          (%log-render-debug frame-ms diff-op-count commit-bytes))))))

(defun %log-runtime-metrics (runtime)
  (ptui.util.log:log-debug
   "~A"
   (ptui.util.log:log-kv
    :frame_count (ptui.util.log:render-stats-frame-count (loop-runtime-stats runtime))
    :last_frame_ms (ptui.util.log:render-stats-last-frame-ms (loop-runtime-stats runtime)))))

(defun %runtime-exit-deadline-reached-p (runtime)
  (let ((exit-after-ms (loop-runtime-exit-after-ms runtime)))
    (and exit-after-ms
         (>= (- (ptui.util.time:monotonic-ms) (loop-runtime-start-ms runtime))
             exit-after-ms))))

(defun %sleep-until-next-runtime-tick (runtime)
  (let ((timeout-ms
          (ptui.runtime.scheduler:scheduler-next-timeout-ms
           (loop-runtime-scheduler runtime))))
    (ptui.util.time:sleep-ms
     (max 1
          (min (loop-runtime-max-idle-sleep-ms runtime)
               (if timeout-ms
                   timeout-ms
                   (loop-runtime-frame-delay-ms runtime)))))))

(defun %capture-loop-step-snapshot (runtime quit-requested-p)
  (make-loop-step-snapshot
   :quit-requested-p (not (null quit-requested-p))
   :exit-deadline-reached-p (not (null (%runtime-exit-deadline-reached-p runtime)))
   :needs-redraw-p (not (null (loop-runtime-needs-redraw runtime)))
   :metrics-poll-due-p (not (null (loop-runtime-metrics-poll-due-p runtime)))))

(defun %evaluate-loop-step-transition (snapshot)
  (let ((effects '())
        (flag-updates '())
        (continue-p t))
    (cond
      ((loop-step-snapshot-exit-deadline-reached-p snapshot)
       (setf continue-p nil))
      (t
       (unless (loop-step-snapshot-quit-requested-p snapshot)
         (push (make-loop-step-effect :run-scheduler) effects))
       (when (loop-step-snapshot-needs-redraw-p snapshot)
         (push (cons :needs-redraw nil) flag-updates)
         (push (make-loop-step-effect :render) effects))
       (when (loop-step-snapshot-metrics-poll-due-p snapshot)
         (push (cons :metrics-poll-due-p nil) flag-updates)
         (push (make-loop-step-effect :log-metrics) effects))
       (if (loop-step-snapshot-quit-requested-p snapshot)
           (setf continue-p nil)
           (push (make-loop-step-effect :sleep) effects))))
    (make-loop-step-transition
     :continue-p continue-p
     :flag-updates (nreverse flag-updates)
     :effects (nreverse effects))))

(defun %apply-loop-step-flag-updates! (runtime flag-updates)
  (dolist (update flag-updates)
    (case (car update)
      (:needs-redraw
       (setf (loop-runtime-needs-redraw runtime) (cdr update)))
      (:metrics-poll-due-p
       (setf (loop-runtime-metrics-poll-due-p runtime) (cdr update)))
      (otherwise
       (error "Unknown loop step flag update ~S." update))))
  runtime)

(defun %apply-loop-step-effect! (runtime effect)
  (case (loop-step-effect-kind effect)
    (:run-scheduler
     (ptui.runtime.scheduler:scheduler-run-due (loop-runtime-scheduler runtime)))
    (:render
     (%render-runtime-frame runtime)
     ;; A frame rendered in this transition satisfies any redraw request the
     ;; scheduler may have re-armed while we were assembling effects.
     (setf (loop-runtime-needs-redraw runtime) nil))
    (:log-metrics
     (%log-runtime-metrics runtime))
    (:sleep
     (%sleep-until-next-runtime-tick runtime))
    (otherwise
     (error "Unknown loop step effect kind ~S." (loop-step-effect-kind effect))))
  runtime)

(defun %apply-loop-step-transition! (runtime transition)
  (%apply-loop-step-flag-updates! runtime
                                  (loop-step-transition-flag-updates transition))
  (dolist (effect (loop-step-transition-effects transition))
    (%apply-loop-step-effect! runtime effect))
  (loop-step-transition-continue-p transition))

(defun %run-loop-iteration (runtime)
  (let ((quit-requested-p (%poll-events-and-check-quit runtime)))
    (%drain-runtime-event-bus runtime)
    (%apply-loop-step-transition!
     runtime
     (%evaluate-loop-step-transition
      (%capture-loop-step-snapshot runtime quit-requested-p)))))

(defun %drain-event-bus (event-bus on-handler-error)
  (when event-bus
    (let ((drain-fn (%resolve-event-bus-drain-fn)))
      (when drain-fn
        (let ((on-handler-error-fn (%resolve-on-handler-error on-handler-error)))
          (restart-case
              (handler-bind
                  ((error
                     (lambda (condition)
                       (let ((restart (find-restart :on-handler-error condition)))
                         (when restart
                           (invoke-restart restart condition))))))
                (funcall drain-fn event-bus))
            (:on-handler-error (condition)
              (funcall on-handler-error-fn condition))))))))

(defun run (render-fn
            &key
              (backend :ansi)
              (fps 20)
              (initial-state nil)
              (on-event nil)
              (event-bus nil)
              (on-handler-error :log-and-continue))
  "Start the PTUI event loop. RENDER-FN is called with (state size) each frame.
Returns when the user presses Ctrl-C/q or PTUI_EXIT_AFTER_MS elapses.
ON-EVENT may return a second value of :CONSUME to suppress default quit
handling for that event, or :QUIT to request immediate shutdown."
  (check-type render-fn function)
  (setf ptui.util.log:*log-level* (ptui.util.log:resolve-log-level))
  (let* ((backend-obj (%make-backend backend))
         (runtime (%make-run-runtime backend-obj
                                     render-fn
                                     fps
                                     initial-state
                                     on-event
                                     event-bus
                                     on-handler-error))
         (backend-started-p nil))
    (%schedule-loop-timers runtime)
    (unwind-protect
         (progn
           (ptui.backend.protocol:backend-init backend-obj)
           (setf backend-started-p t)
           (loop while (%run-loop-iteration runtime)))
      (when backend-started-p
        (ignore-errors
          (ptui.backend.protocol:backend-shutdown backend-obj))))))

(defun main (&key (backend :ansi) (fps 60))
  (run #'%default-render-fn :backend backend :fps fps))
