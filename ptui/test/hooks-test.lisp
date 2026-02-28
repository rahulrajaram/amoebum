(defpackage :ptui.test.hooks
  (:use :cl :fiveam)
  (:export #:run-all #:hooks-state-suite #:hooks-effect-suite))

(in-package :ptui.test.hooks)

;;; ===================================================================
;;; hooks-state-suite (I269-I271)
;;; ===================================================================

(def-suite hooks-state-suite
  :description "PTUI hooks: use-state, widget context, state cleanup (I269-I271).")

(in-suite hooks-state-suite)

(defun %make-test-runtime ()
  (ptui.ui.runtime:make-runtime))

(defun %with-widget-context (runtime widget-name instance-key thunk)
  "Run THUNK with a bound widget context."
  (let ((ptui.ui.runtime:*current-runtime* runtime)
        (ptui.ui.runtime:*current-widget-context*
          (ptui.ui.runtime::%make-widget-context widget-name instance-key runtime)))
    (funcall thunk)))

(test use-state-outside-context-signals-error
  (signals error
    (ptui.ui.hooks:use-state test-state :initial-value 0)))

(test use-state-returns-initial-value
  (let ((rt (%make-test-runtime)))
    (%with-widget-context rt 'test-widget 'inst-1
      (lambda ()
        (multiple-value-bind (val setter)
            (ptui.ui.hooks:use-state counter :initial-value 42)
          (is (= val 42))
          (is (functionp setter)))))))

(test use-state-setter-updates-value
  (let ((rt (%make-test-runtime)))
    (ptui.widgets.defwidget:register-widget
     'test-widget (lambda ()) :memoize :equal :arity 0)
    (%with-widget-context rt 'test-widget 'inst-1
      (lambda ()
        (multiple-value-bind (val1 setter1)
            (ptui.ui.hooks:use-state counter :initial-value 0)
          (declare (ignore val1))
          (funcall setter1 10))))
    ;; Re-enter context — should get updated value
    (%with-widget-context rt 'test-widget 'inst-1
      (lambda ()
        (multiple-value-bind (val2 setter2)
            (ptui.ui.hooks:use-state counter :initial-value 0)
          (declare (ignore setter2))
          (is (= val2 10)))))))

(test use-state-scoped-by-instance-key
  (let ((rt (%make-test-runtime)))
    (ptui.widgets.defwidget:register-widget
     'test-widget-scoped (lambda ()) :memoize :equal :arity 0)
    ;; Instance A sets counter to 100
    (%with-widget-context rt 'test-widget-scoped 'inst-a
      (lambda ()
        (multiple-value-bind (_val setter)
            (ptui.ui.hooks:use-state counter :initial-value 0)
          (declare (ignore _val))
          (funcall setter 100))))
    ;; Instance B should get fresh initial value
    (%with-widget-context rt 'test-widget-scoped 'inst-b
      (lambda ()
        (multiple-value-bind (val _setter)
            (ptui.ui.hooks:use-state counter :initial-value 0)
          (declare (ignore _setter))
          (is (= val 0)))))))

(test use-state-setter-marks-widget-dirty
  (let ((rt (%make-test-runtime)))
    ;; Register a widget so mark-widget-dirty works
    (ptui.widgets.defwidget:register-widget
     'dirty-test-widget (lambda ()) :memoize :equal :arity 0)
    (ptui.widgets.defwidget:clear-widget-dirty 'dirty-test-widget)
    (%with-widget-context rt 'dirty-test-widget 'inst-1
      (lambda ()
        (multiple-value-bind (_val setter)
            (ptui.ui.hooks:use-state counter :initial-value 0)
          (declare (ignore _val))
          (is-false (ptui.widgets.defwidget:widget-dirty-p 'dirty-test-widget))
          (funcall setter 5)
          (is-true (ptui.widgets.defwidget:widget-dirty-p 'dirty-test-widget)))))))

(test cleanup-widget-state-removes-entries
  (let ((rt (%make-test-runtime)))
    ;; Set up some state
    (%with-widget-context rt 'cleanup-widget 'inst-1
      (lambda ()
        (ptui.ui.hooks:use-state alpha :initial-value 1)
        (ptui.ui.hooks:use-state beta :initial-value 2)))
    ;; Verify state exists
    (is (= (ptui.ui.runtime:runtime-state
             rt (list 'cleanup-widget 'inst-1 'alpha))
            1))
    ;; Cleanup
    (let ((removed (ptui.ui.hooks:cleanup-widget-state rt 'cleanup-widget 'inst-1)))
      (is (>= removed 2)))
    ;; Verify state is gone
    (multiple-value-bind (val found)
        (ptui.ui.runtime:runtime-state rt (list 'cleanup-widget 'inst-1 'alpha))
      (declare (ignore val))
      (is-false found))))

(test state-version-increments-on-set
  (let ((rt (%make-test-runtime)))
    (let ((v0 (ptui.ui.runtime:runtime-state-version rt)))
      (ptui.ui.runtime:set-runtime-state rt :test-key 42)
      (is (> (ptui.ui.runtime:runtime-state-version rt) v0)))))

;;; ===================================================================
;;; hooks-effect-suite (I272-I274)
;;; ===================================================================

(def-suite hooks-effect-suite
  :description "PTUI hooks: use-effect, use-memo, use-callback (I272-I274).")

(in-suite hooks-effect-suite)

(test use-effect-enqueues-on-first-render
  (let ((rt (%make-test-runtime))
        (effect-ran nil))
    (%with-widget-context rt 'effect-widget 'inst-1
      (lambda ()
        (ptui.ui.hooks:use-effect first-effect (:deps (1 2))
          (setf effect-ran t))))
    ;; Effect is enqueued but not yet run
    (is-false effect-ran)
    ;; Run pending effects via update-runtime
    (ptui.ui.runtime:update-runtime rt nil)
    (is-true effect-ran)))

(test use-effect-skips-when-deps-unchanged
  (let ((rt (%make-test-runtime))
        (run-count 0))
    ;; First render — enqueues effect
    (%with-widget-context rt 'effect-widget 'inst-1
      (lambda ()
        (ptui.ui.hooks:use-effect counter-effect (:deps (42))
          (incf run-count))))
    (ptui.ui.runtime:update-runtime rt nil)
    (is (= run-count 1))
    ;; Second render — same deps, should NOT enqueue
    (%with-widget-context rt 'effect-widget 'inst-1
      (lambda ()
        (ptui.ui.hooks:use-effect counter-effect (:deps (42))
          (incf run-count))))
    (ptui.ui.runtime:update-runtime rt nil)
    (is (= run-count 1))))

(test use-effect-reruns-when-deps-change
  (let ((rt (%make-test-runtime))
        (run-count 0))
    (%with-widget-context rt 'effect-widget 'inst-1
      (lambda ()
        (ptui.ui.hooks:use-effect dep-effect (:deps (1))
          (incf run-count))))
    (ptui.ui.runtime:update-runtime rt nil)
    (is (= run-count 1))
    ;; Change deps
    (%with-widget-context rt 'effect-widget 'inst-1
      (lambda ()
        (ptui.ui.hooks:use-effect dep-effect (:deps (2))
          (incf run-count))))
    (ptui.ui.runtime:update-runtime rt nil)
    (is (= run-count 2))))

(test use-effect-cleanup-runs-before-reexecution
  (let ((rt (%make-test-runtime))
        (cleanup-log '()))
    ;; First render — effect returns cleanup fn
    (%with-widget-context rt 'effect-widget 'inst-1
      (lambda ()
        (ptui.ui.hooks:use-effect cleanup-effect (:deps (1))
          (lambda ()
            (push :cleaned-up cleanup-log)))))
    (ptui.ui.runtime:update-runtime rt nil)
    (is (null cleanup-log))
    ;; Change deps — cleanup should run before new effect
    (%with-widget-context rt 'effect-widget 'inst-1
      (lambda ()
        (ptui.ui.hooks:use-effect cleanup-effect (:deps (2))
          (push :new-effect cleanup-log)
          nil)))
    (ptui.ui.runtime:update-runtime rt nil)
    (is (member :cleaned-up cleanup-log))))

(test use-memo-caches-value
  (let ((rt (%make-test-runtime))
        (compute-count 0))
    (let ((val1
            (%with-widget-context rt 'memo-widget 'inst-1
              (lambda ()
                (ptui.ui.hooks:use-memo expensive-calc (:deps (10))
                  (incf compute-count)
                  (* 10 10))))))
      (is (= val1 100))
      (is (= compute-count 1)))
    ;; Same deps — should return cached
    (let ((val2
            (%with-widget-context rt 'memo-widget 'inst-1
              (lambda ()
                (ptui.ui.hooks:use-memo expensive-calc (:deps (10))
                  (incf compute-count)
                  (* 10 10))))))
      (is (= val2 100))
      (is (= compute-count 1)))))

(test use-memo-recomputes-on-dep-change
  (let ((rt (%make-test-runtime))
        (compute-count 0))
    (%with-widget-context rt 'memo-widget 'inst-1
      (lambda ()
        (ptui.ui.hooks:use-memo recompute-calc (:deps (5))
          (incf compute-count)
          25)))
    ;; Different deps
    (let ((val
            (%with-widget-context rt 'memo-widget 'inst-1
              (lambda ()
                (ptui.ui.hooks:use-memo recompute-calc (:deps (6))
                  (incf compute-count)
                  36)))))
      (is (= val 36))
      (is (= compute-count 2)))))

(test use-callback-works-like-use-memo
  (let ((rt (%make-test-runtime)))
    (let ((cb
            (%with-widget-context rt 'cb-widget 'inst-1
              (lambda ()
                (ptui.ui.hooks:use-callback my-handler (:deps (1))
                  (lambda (x) (* x 2)))))))
      (is (functionp cb))
      (is (= (funcall cb 5) 10)))))

(defun run-all ()
  (run! 'hooks-state-suite)
  (run! 'hooks-effect-suite))
