(defpackage :ptui.test.loop-step-transition-rules
  (:use :cl :fiveam)
  (:export #:run-all #:loop-step-transition-rules-suite))

(in-package :ptui.test.loop-step-transition-rules)

(def-suite loop-step-transition-rules-suite
  :description "Loop-step transition rule table tests (FP-Refine Phase 3, T3).")

(in-suite loop-step-transition-rules-suite)

(defun %effect-kinds (transition)
  (mapcar #'ptui.engine.loop::loop-step-effect-kind
          (ptui.engine.loop::loop-step-transition-effects transition)))

;;; --- Table structure tests ---

(test exit-rules-has-one-entry
  (is (= 1 (length ptui.engine.loop::+loop-step-exit-rules+))))

(test accumulation-rules-has-three-entries
  (is (= 3 (length ptui.engine.loop::+loop-step-accumulation-rules+))))

(test terminal-rule-has-two-elements
  (is (= 2 (length ptui.engine.loop::+loop-step-terminal-rule+))))

(test exit-rule-predicates-are-fboundp
  (dolist (rule ptui.engine.loop::+loop-step-exit-rules+)
    (is (fboundp (second rule))
        "Predicate ~S should be fboundp." (second rule))))

(test accumulation-rule-predicates-are-fboundp
  (dolist (rule ptui.engine.loop::+loop-step-accumulation-rules+)
    (is (fboundp (second rule))
        "Predicate ~S should be fboundp." (second rule))))

(test terminal-rule-predicate-is-fboundp
  (is (fboundp (second ptui.engine.loop::+loop-step-terminal-rule+))))

;;; --- Exit deadline → immediate stop ---

(test exit-deadline-produces-immediate-stop
  (let* ((snapshot (ptui.engine.loop::make-loop-step-snapshot
                    :quit-requested-p nil
                    :exit-deadline-reached-p t
                    :needs-redraw-p t
                    :metrics-poll-due-p t))
         (transition (ptui.engine.loop::%evaluate-loop-step-transition snapshot)))
    (is-false (ptui.engine.loop::loop-step-transition-continue-p transition))
    (is (null (ptui.engine.loop::loop-step-transition-effects transition)))
    (is (null (ptui.engine.loop::loop-step-transition-flag-updates transition)))))

;;; --- Quit requested → stop, no sleep, but render/metrics still fire ---

(test quit-requested-stops-without-sleep
  (let* ((snapshot (ptui.engine.loop::make-loop-step-snapshot
                    :quit-requested-p t
                    :exit-deadline-reached-p nil
                    :needs-redraw-p t
                    :metrics-poll-due-p t))
         (transition (ptui.engine.loop::%evaluate-loop-step-transition snapshot)))
    (is-false (ptui.engine.loop::loop-step-transition-continue-p transition))
    (let ((kinds (%effect-kinds transition)))
      (is (not (member :run-scheduler kinds :test #'eq)))
      (is (member :render kinds :test #'eq))
      (is (member :log-metrics kinds :test #'eq))
      (is (not (member :sleep kinds :test #'eq))))))

;;; --- Accumulation: individual flag/effect pairs ---

(test redraw-needed-produces-render-effect-and-flag-clear
  (let* ((snapshot (ptui.engine.loop::make-loop-step-snapshot
                    :quit-requested-p nil
                    :exit-deadline-reached-p nil
                    :needs-redraw-p t
                    :metrics-poll-due-p nil))
         (transition (ptui.engine.loop::%evaluate-loop-step-transition snapshot)))
    (is-true (ptui.engine.loop::loop-step-transition-continue-p transition))
    (is (member :render (%effect-kinds transition) :test #'eq))
    (is (assoc :needs-redraw (ptui.engine.loop::loop-step-transition-flag-updates transition)
               :test #'eq))))

(test metrics-due-produces-log-metrics-effect-and-flag-clear
  (let* ((snapshot (ptui.engine.loop::make-loop-step-snapshot
                    :quit-requested-p nil
                    :exit-deadline-reached-p nil
                    :needs-redraw-p nil
                    :metrics-poll-due-p t))
         (transition (ptui.engine.loop::%evaluate-loop-step-transition snapshot)))
    (is-true (ptui.engine.loop::loop-step-transition-continue-p transition))
    (is (member :log-metrics (%effect-kinds transition) :test #'eq))
    (is (assoc :metrics-poll-due-p (ptui.engine.loop::loop-step-transition-flag-updates transition)
               :test #'eq))))

;;; --- Full active cycle: redraw + metrics + not quit ---

(test full-active-cycle-produces-scheduler-render-metrics-sleep
  (let* ((snapshot (ptui.engine.loop::make-loop-step-snapshot
                    :quit-requested-p nil
                    :exit-deadline-reached-p nil
                    :needs-redraw-p t
                    :metrics-poll-due-p t))
         (transition (ptui.engine.loop::%evaluate-loop-step-transition snapshot)))
    (is-true (ptui.engine.loop::loop-step-transition-continue-p transition))
    (is (equal (%effect-kinds transition)
               '(:run-scheduler :render :log-metrics :sleep)))
    (is (equal (ptui.engine.loop::loop-step-transition-flag-updates transition)
               '((:needs-redraw . nil) (:metrics-poll-due-p . nil))))))

;;; --- Idle: not quit, no redraw, no metrics ---

(test idle-produces-scheduler-and-sleep-only
  (let* ((snapshot (ptui.engine.loop::make-loop-step-snapshot
                    :quit-requested-p nil
                    :exit-deadline-reached-p nil
                    :needs-redraw-p nil
                    :metrics-poll-due-p nil))
         (transition (ptui.engine.loop::%evaluate-loop-step-transition snapshot)))
    (is-true (ptui.engine.loop::loop-step-transition-continue-p transition))
    (is (equal (%effect-kinds transition) '(:run-scheduler :sleep)))
    (is (null (ptui.engine.loop::loop-step-transition-flag-updates transition)))))

;;; --- Effects ordering preserved ---

(test effects-ordering-is-stable
  (let* ((snapshot (ptui.engine.loop::make-loop-step-snapshot
                    :quit-requested-p nil
                    :exit-deadline-reached-p nil
                    :needs-redraw-p t
                    :metrics-poll-due-p t))
         (kinds (%effect-kinds
                 (ptui.engine.loop::%evaluate-loop-step-transition snapshot))))
    ;; :run-scheduler < :render < :log-metrics < :sleep
    (is (< (position :run-scheduler kinds :test #'eq)
           (position :render kinds :test #'eq)))
    (is (< (position :render kinds :test #'eq)
           (position :log-metrics kinds :test #'eq)))
    (is (< (position :log-metrics kinds :test #'eq)
           (position :sleep kinds :test #'eq)))))

;;; --- Predicate helper ---

(test snapshot-not-quit-requested-p-works
  (let ((s1 (ptui.engine.loop::make-loop-step-snapshot :quit-requested-p nil))
        (s2 (ptui.engine.loop::make-loop-step-snapshot :quit-requested-p t)))
    (is-true (ptui.engine.loop::%snapshot-not-quit-requested-p s1))
    (is-false (ptui.engine.loop::%snapshot-not-quit-requested-p s2))))

;;; --- Exit deadline takes priority over quit ---

(test exit-deadline-takes-priority-over-quit
  (let* ((snapshot (ptui.engine.loop::make-loop-step-snapshot
                    :quit-requested-p t
                    :exit-deadline-reached-p t
                    :needs-redraw-p t
                    :metrics-poll-due-p t))
         (transition (ptui.engine.loop::%evaluate-loop-step-transition snapshot)))
    (is-false (ptui.engine.loop::loop-step-transition-continue-p transition))
    ;; Exit deadline produces no effects at all (unlike quit which still renders)
    (is (null (ptui.engine.loop::loop-step-transition-effects transition)))))

;;; --- Quit with only redraw ---

(test quit-with-redraw-only
  (let* ((snapshot (ptui.engine.loop::make-loop-step-snapshot
                    :quit-requested-p t
                    :exit-deadline-reached-p nil
                    :needs-redraw-p t
                    :metrics-poll-due-p nil))
         (transition (ptui.engine.loop::%evaluate-loop-step-transition snapshot)))
    (is-false (ptui.engine.loop::loop-step-transition-continue-p transition))
    (is (equal (%effect-kinds transition) '(:render)))))

(defun run-all ()
  (run! 'loop-step-transition-rules-suite))
