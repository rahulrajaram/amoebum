(defpackage :ptui.test.loop-step-dispatch
  (:use :cl :fiveam)
  (:export #:run-all #:loop-step-dispatch-suite))

(in-package :ptui.test.loop-step-dispatch)

(def-suite loop-step-dispatch-suite
  :description "Loop-step effect/flag dispatch table tests (FP-Refine Phase 3, T1).")

(in-suite loop-step-dispatch-suite)

;;; --- Table structure tests ---

(test effect-table-has-four-entries
  (is (= 4 (length ptui.engine.loop::+loop-step-effect-handlers+))))

(test effect-table-entries-are-all-fboundp
  (dolist (entry ptui.engine.loop::+loop-step-effect-handlers+)
    (is (fboundp (cdr entry))
        "Handler ~S is not fboundp." (cdr entry))))

(test effect-table-contains-expected-kinds
  (let ((kinds (mapcar #'car ptui.engine.loop::+loop-step-effect-handlers+)))
    (is (member :run-scheduler kinds :test #'eq))
    (is (member :render kinds :test #'eq))
    (is (member :log-metrics kinds :test #'eq))
    (is (member :sleep kinds :test #'eq))))

(test flag-table-has-two-entries
  (is (= 2 (length ptui.engine.loop::+loop-step-flag-setters+))))

(test flag-table-entries-are-all-fboundp
  (dolist (entry ptui.engine.loop::+loop-step-flag-setters+)
    (is (fboundp (cdr entry))
        "Setter ~S is not fboundp." (cdr entry))))

(test flag-table-contains-expected-flags
  (let ((flags (mapcar #'car ptui.engine.loop::+loop-step-flag-setters+)))
    (is (member :needs-redraw flags :test #'eq))
    (is (member :metrics-poll-due-p flags :test #'eq))))

;;; --- Flag setter behavior tests ---

(defun %make-test-runtime ()
  "Create a minimal loop-runtime for testing flag/effect dispatch."
  (let* ((backend (ptui.backend.test:make-test-backend :cols 10 :rows 5))
         (scheduler (ptui.runtime.scheduler:make-scheduler)))
    (ptui.engine.loop::%make-loop-runtime
     backend scheduler
     (lambda (state size)
       (declare (ignore state size))
       (ptui.render.buffer:make-buffer 10 5))
     nil nil nil nil nil t nil nil
     (ptui.util.time:monotonic-ms)
     (ptui.util.log:make-render-stats)
     50 10)))

(test flag-setter-needs-redraw-sets-field
  (let ((rt (%make-test-runtime)))
    (is-true (ptui.engine.loop::loop-runtime-needs-redraw rt))
    (ptui.engine.loop::%loop-set-needs-redraw! rt nil)
    (is-false (ptui.engine.loop::loop-runtime-needs-redraw rt))
    (ptui.engine.loop::%loop-set-needs-redraw! rt t)
    (is-true (ptui.engine.loop::loop-runtime-needs-redraw rt))))

(test flag-setter-metrics-poll-due-p-sets-field
  (let ((rt (%make-test-runtime)))
    (is-false (ptui.engine.loop::loop-runtime-metrics-poll-due-p rt))
    (ptui.engine.loop::%loop-set-metrics-poll-due-p! rt t)
    (is-true (ptui.engine.loop::loop-runtime-metrics-poll-due-p rt))
    (ptui.engine.loop::%loop-set-metrics-poll-due-p! rt nil)
    (is-false (ptui.engine.loop::loop-runtime-metrics-poll-due-p rt))))

(test apply-flag-updates-applies-both-flags
  (let ((rt (%make-test-runtime)))
    (setf (ptui.engine.loop::loop-runtime-needs-redraw rt) t
          (ptui.engine.loop::loop-runtime-metrics-poll-due-p rt) t)
    (ptui.engine.loop::%apply-loop-step-flag-updates!
     rt '((:needs-redraw . nil) (:metrics-poll-due-p . nil)))
    (is-false (ptui.engine.loop::loop-runtime-needs-redraw rt))
    (is-false (ptui.engine.loop::loop-runtime-metrics-poll-due-p rt))))

(test apply-flag-updates-unknown-flag-signals-error
  (let ((rt (%make-test-runtime)))
    (signals error
      (ptui.engine.loop::%apply-loop-step-flag-updates!
       rt '((:bogus-flag . t))))))

;;; --- Effect handler behavior tests ---

(test effect-handler-run-scheduler-calls-scheduler
  (let* ((rt (%make-test-runtime))
         (scheduler (ptui.engine.loop::loop-runtime-scheduler rt)))
    ;; Schedule a timer that sets a flag when fired
    (let ((fired nil))
      (ptui.runtime.scheduler:schedule-once scheduler 0
                                            (lambda () (setf fired t)))
      (ptui.engine.loop::%loop-effect-run-scheduler! rt)
      (is-true fired))))

(test effect-handler-render-clears-needs-redraw
  (let ((rt (%make-test-runtime)))
    (setf (ptui.engine.loop::loop-runtime-needs-redraw rt) t)
    (ptui.engine.loop::%loop-effect-render! rt)
    (is-false (ptui.engine.loop::loop-runtime-needs-redraw rt))))

(test effect-handler-log-metrics-does-not-error
  (let ((rt (%make-test-runtime)))
    (finishes (ptui.engine.loop::%loop-effect-log-metrics! rt))))

(test effect-handler-sleep-does-not-error
  (let ((rt (%make-test-runtime)))
    (finishes (ptui.engine.loop::%loop-effect-sleep! rt))))

(test apply-effect-unknown-kind-signals-error
  (let ((rt (%make-test-runtime))
        (bad-effect (ptui.engine.loop::make-loop-step-effect :nonexistent)))
    (signals error
      (ptui.engine.loop::%apply-loop-step-effect! rt bad-effect))))

(test apply-effect-dispatches-via-table
  (let ((rt (%make-test-runtime)))
    ;; :log-metrics should not error and should return runtime
    (let ((result (ptui.engine.loop::%apply-loop-step-effect!
                   rt (ptui.engine.loop::make-loop-step-effect :log-metrics))))
      (is (eq rt result)))))

;;; --- Integration: %apply-loop-step-transition! ---

(test apply-transition-integration-full-cycle
  (let* ((rt (%make-test-runtime))
         (transition (ptui.engine.loop::make-loop-step-transition
                      :continue-p t
                      :flag-updates '((:needs-redraw . nil))
                      :effects (list (ptui.engine.loop::make-loop-step-effect :log-metrics)))))
    (setf (ptui.engine.loop::loop-runtime-needs-redraw rt) t)
    (let ((continue-p (ptui.engine.loop::%apply-loop-step-transition! rt transition)))
      (is-true continue-p)
      (is-false (ptui.engine.loop::loop-runtime-needs-redraw rt)))))

(defun run-all ()
  (run! 'loop-step-dispatch-suite))
