(defpackage :ptui.engine.loop
  (:use :cl)
  (:export #:run #:main))

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
         (scheduler (ptui.runtime.scheduler:make-scheduler))
         (running t)
         (backend-started-p nil)
         (state initial-state)
         (prev-buffer nil)
         (needs-redraw t)
         (metrics-poll-due-p nil)
         (exit-after-ms-str (uiop:getenv "PTUI_EXIT_AFTER_MS"))
         (exit-after-ms (and exit-after-ms-str
                             (ignore-errors (parse-integer exit-after-ms-str))))
         (start-ms (ptui.util.time:monotonic-ms))
         (stats (ptui.util.log:make-render-stats))
         (frame-delay-ms (max 1 (truncate (/ 1000 (max 1 fps)))))
         (max-idle-sleep-ms-str (uiop:getenv "PTUI_MAX_IDLE_SLEEP_MS"))
         (max-idle-sleep-ms (max 1
                                (or (and max-idle-sleep-ms-str
                                         (ignore-errors
                                           (parse-integer max-idle-sleep-ms-str)))
                                    10))))
    (ptui.runtime.scheduler:schedule-interval
     scheduler
     frame-delay-ms
     (lambda ()
       (setf needs-redraw t)))
    (ptui.runtime.scheduler:schedule-interval
     scheduler
     1000
     (lambda ()
       (setf metrics-poll-due-p t)))
    (unwind-protect
         (progn
           (ptui.backend.protocol:backend-init backend-obj)
           (setf backend-started-p t)
           (loop while running do
             (dolist (event (ptui.backend.protocol:backend-poll-events backend-obj))
               (multiple-value-bind (next-state event-consumed-p event-quit-p)
                   (%dispatch-event state event on-event)
                 (setf state next-state)
                 (when (or event-quit-p
                           (and (not event-consumed-p)
                                (%quit-event-p event)))
                   (setf running nil))))
             (%drain-event-bus event-bus on-handler-error)
             (ptui.runtime.scheduler:scheduler-run-due scheduler)
             (when needs-redraw
               (setf needs-redraw nil)
               (let* ((frame-start (ptui.util.time:monotonic-ms))
                      (size (ptui.backend.protocol:backend-size backend-obj))
                      (next-buffer (funcall render-fn state size)))
                 (multiple-value-bind (draw-ops diff-op-count)
                     (ptui.render.diff:diff-buffers prev-buffer next-buffer
                                                    :full-redraw (null prev-buffer))
                   (let ((commit-bytes (ptui.backend.protocol:backend-commit backend-obj draw-ops))
                         (frame-ms (- (ptui.util.time:monotonic-ms) frame-start)))
                     (setf prev-buffer next-buffer
                           (ptui.util.log:render-stats-frame-count stats)
                           (the fixnum (1+ (ptui.util.log:render-stats-frame-count stats)))
                           (ptui.util.log:render-stats-last-frame-ms stats)
                           (the fixnum frame-ms)
                           (ptui.util.log:render-stats-last-diff-ops stats)
                           (the fixnum diff-op-count)
                           (ptui.util.log:render-stats-last-commit-bytes stats)
                           (the fixnum commit-bytes))
                     (ptui.util.log:log-debug
                      "~A"
                      (ptui.util.log:log-kv
                       :frame_ms frame-ms
                       :diff_ops diff-op-count
                       :commit_bytes commit-bytes))))))
             (when metrics-poll-due-p
               (setf metrics-poll-due-p nil)
               (ptui.util.log:log-debug
                "~A"
                (ptui.util.log:log-kv
                 :frame_count (ptui.util.log:render-stats-frame-count stats)
                 :last_frame_ms (ptui.util.log:render-stats-last-frame-ms stats))))
             (when exit-after-ms
               (let ((elapsed-ms (- (ptui.util.time:monotonic-ms) start-ms)))
                 (when (>= elapsed-ms exit-after-ms)
                   (setf running nil))))
             (when running
               (let ((timeout-ms (ptui.runtime.scheduler:scheduler-next-timeout-ms scheduler)))
                 (ptui.util.time:sleep-ms
                  (max 1
                       (min max-idle-sleep-ms
                            (if timeout-ms
                                timeout-ms
                              frame-delay-ms))))))))
      (when backend-started-p
        (ignore-errors
          (ptui.backend.protocol:backend-shutdown backend-obj))))))

(defun main (&key (backend :ansi) (fps 20))
  (run #'%default-render-fn :backend backend :fps fps))
