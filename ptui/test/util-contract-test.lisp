(defpackage :ptui.test.util-contract
  (:use :cl :fiveam)
  (:export #:run-all #:ptui-util-contract-suite))

(in-package :ptui.test.util-contract)

(def-suite ptui-util-contract-suite
  :description "PTUI foundational util contracts: logging, time, and engine stats logging.")

(in-suite ptui-util-contract-suite)

(defmacro with-fake-getenv (fn &body body)
  `(let ((original (symbol-function 'uiop:getenv)))
     (unwind-protect
         (progn
           (setf (symbol-function 'uiop:getenv) ,fn)
           ,@body)
       (setf (symbol-function 'uiop:getenv) original))))

(defun %capture-log-output (thunk &key (level :debug))
  (with-output-to-string (out)
    (let ((ptui.util.log:*log-level* level)
          (ptui.util.log:*log-output* out))
      (funcall thunk))))

(defun %make-test-runtime ()
  (let* ((backend (ptui.backend.test:make-test-backend :cols 10 :rows 5))
         (scheduler (ptui.runtime.scheduler:make-scheduler))
         (stats (ptui.util.log:make-render-stats)))
    (ptui.engine.loop::%make-loop-runtime
     backend
     scheduler
     (lambda (state size)
       (declare (ignore state size))
       (ptui.render.buffer:make-buffer 10 5))
     nil
     nil
     nil
     nil
     nil
     t
     nil
     nil
     0
     stats
     50
     10)))

(test util-log-resolve-log-level-defaults-and-normalizes-env
  (with-fake-getenv
      (lambda (name)
        (declare (ignore name))
        nil)
    (is (eq :info (ptui.util.log:resolve-log-level))))
  (with-fake-getenv
      (lambda (name)
        (and (string= name "PTUI_LOG_LEVEL") "DeBuG"))
    (is (eq :debug (ptui.util.log:resolve-log-level))))
  (with-fake-getenv
      (lambda (name)
        (and (string= name "PTUI_LOG_LEVEL") "unexpected"))
    (is (eq :info (ptui.util.log:resolve-log-level)))))

(test util-log-macros-respect-level-gating-and-log-output
  (let ((output
          (%capture-log-output
           (lambda ()
             (ptui.util.log:log-debug "debug")
             (ptui.util.log:log-info "info")
             (ptui.util.log:log-warn "warn")
             (ptui.util.log:log-error "error"))
           :level :warn)))
    (is-false (search "[DEBUG]" output))
    (is-false (search "[INFO]" output))
    (is (search "[WARN] warn" output))
    (is (search "[ERROR] error" output))))

(test util-log-with-log-context-is-scoped-to-body
  (let ((output
          (%capture-log-output
           (lambda ()
             (ptui.util.log:with-log-context
                 ((cons :phase :render) (cons :frame 7))
               (ptui.util.log:log-info "inside"))
             (ptui.util.log:log-info "outside"))
           :level :info)))
    (is (search "[INFO] inside PHASE=:RENDER FRAME=7" output))
    (is (search "[INFO] outside" output))
    (is-false (search "[INFO] outside PHASE=:RENDER FRAME=7" output))))

(test util-log-kv-renders-stable-ordered-pairs
  (is (string= "PHASE=:RENDER MESSAGE=\"hi\" COUNT=3"
               (ptui.util.log:log-kv
                :phase :render
                :message "hi"
                :count 3))))

(test util-log-render-stats-defaults-are-normalized
  (let ((stats (ptui.util.log:make-render-stats)))
    (is (= 0 (ptui.util.log:render-stats-frame-count stats)))
    (is (= 0 (ptui.util.log:render-stats-last-frame-ms stats)))
    (is (= 0 (ptui.util.log:render-stats-last-commit-bytes stats)))
    (is (= 0 (ptui.util.log:render-stats-last-diff-ops stats)))
    (is-false (ptui.util.log:render-stats-last-frame-slow-p stats))
    (is-false (ptui.util.log:render-stats-last-perf-guard-fired-p stats))))

(test util-time-monotonic-ms-returns-monotonic-integers
  (let ((first (ptui.util.time:monotonic-ms))
        (second (ptui.util.time:monotonic-ms)))
    (is (integerp first))
    (is (integerp second))
    (is (<= first second))))

(test util-time-sleep-ms-clamps-negative-values-and-returns-nil
  (is (null (ptui.util.time:sleep-ms -5)))
  (is (null (ptui.util.time:sleep-ms 0))))

(test engine-update-render-stats-populates-all-fields
  (let ((stats (ptui.util.log:make-render-stats)))
    (ptui.engine.loop::%update-render-stats stats 12 34 56 t)
    (is (= 1 (ptui.util.log:render-stats-frame-count stats)))
    (is (= 12 (ptui.util.log:render-stats-last-frame-ms stats)))
    (is (= 34 (ptui.util.log:render-stats-last-diff-ops stats)))
    (is (= 56 (ptui.util.log:render-stats-last-commit-bytes stats)))
    (is-true (ptui.util.log:render-stats-last-frame-slow-p stats))
    (is-true (ptui.util.log:render-stats-last-perf-guard-fired-p stats))
    (ptui.engine.loop::%update-render-stats stats 7 8 9 nil)
    (is (= 2 (ptui.util.log:render-stats-frame-count stats)))
    (is (= 7 (ptui.util.log:render-stats-last-frame-ms stats)))
    (is (= 8 (ptui.util.log:render-stats-last-diff-ops stats)))
    (is (= 9 (ptui.util.log:render-stats-last-commit-bytes stats)))
    (is-false (ptui.util.log:render-stats-last-frame-slow-p stats))
    (is-false (ptui.util.log:render-stats-last-perf-guard-fired-p stats))))

(test engine-render-debug-logging-emits-structured-fields
  (let ((output
          (%capture-log-output
           (lambda ()
             (ptui.engine.loop::%log-render-debug 12 34 56))
           :level :debug)))
    (is (search "[DEBUG]" output))
    (is (search "FRAME_MS=12" output))
    (is (search "DIFF_OPS=34" output))
    (is (search "COMMIT_BYTES=56" output))))

(test engine-render-guard-logging-emits-structured-warning
  (let ((output
          (%capture-log-output
           (lambda ()
             (ptui.engine.loop::%log-render-guard 12 34 80 10 20))
           :level :warn)))
    (is (search "[WARN]" output))
    (is (search "render_performance_guard=t" output))
    (is (search "frame_ms=12" output))
    (is (search "diff_ops=34" output))
    (is (search "guard_workload=80" output))
    (is (search "frame_ms_threshold=10" output))
    (is (search "diff_ops_threshold=20" output))))

(test engine-runtime-metrics-logging-reads-runtime-stats
  (let* ((runtime (%make-test-runtime))
         (stats (ptui.engine.loop::loop-runtime-stats runtime))
         (output nil))
    (ptui.engine.loop::%update-render-stats stats 18 9 7 nil)
    (setf output
          (%capture-log-output
           (lambda ()
             (ptui.engine.loop::%log-runtime-metrics runtime))
           :level :debug))
    (is (search "[DEBUG]" output))
    (is (search "FRAME_COUNT=1" output))
    (is (search "LAST_FRAME_MS=18" output))))

(defun run-all ()
  (run! 'ptui-util-contract-suite))
