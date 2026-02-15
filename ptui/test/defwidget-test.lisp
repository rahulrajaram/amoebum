(defpackage :ptui.test.defwidget
  (:use :cl :fiveam)
  (:export #:run-all #:defwidget-suite))

(in-package :ptui.test.defwidget)

(def-suite defwidget-suite
  :description "PTUI defwidget macro coverage (I63).")

(in-suite defwidget-suite)

(defparameter *memo-render-count* 0)
(defparameter *dirty-render-count* 0)
(defparameter *dirty-state* (make-hash-table :test #'eq))

(defun %element-text (element)
  (getf (ptui.ui.elements:ui-element-props element) :text))

(defun %defun-with-name-p (form symbol-name)
  (and (consp form)
       (eq (first form) 'defun)
       (symbolp (second form))
       (string= (symbol-name (second form)) symbol-name)))

(ptui.widgets.defwidget:defwidget memo-probe (value)
  (:memoize :equal)
  (incf *memo-render-count*)
  (text (princ-to-string value)))

(ptui.widgets.defwidget:defwidget dirty-probe (state)
  (:memoize :equal)
  (incf *dirty-render-count*)
  (text (gethash :value state "")))

(test defwidget-expands-render-wrapper-and-registration
  (let* ((expanded
           (macroexpand-1
            '(ptui.widgets.defwidget:defwidget expansion-probe (value)
               (:memoize :equal)
               (text value))))
         (forms (rest expanded)))
    (is (and (consp expanded) (eq (first expanded) 'progn)))
    (is (find-if (lambda (form)
                   (%defun-with-name-p form "RENDER-EXPANSION-PROBE"))
                 forms))
    (is (find-if (lambda (form)
                   (%defun-with-name-p form "EXPANSION-PROBE"))
                 forms))
    (is (find-if (lambda (form)
                   (and (consp form)
                        (eq (first form) 'eval-when)
                        (search "REGISTER-WIDGET"
                                (prin1-to-string form)
                                :test #'char-equal)))
                 forms))))

(test memoization-skips-identical-props
  (setf *memo-render-count* 0)
  (ptui.widgets.defwidget:invalidate-widget 'memo-probe)
  (let ((first (memo-probe "same"))
        (second (memo-probe "same")))
    (is (= *memo-render-count* 1))
    (is (eq first second))
    (is (string= (%element-text second) "same"))))

(test dirty-tracking-rerenders-after-state-mutation
  (setf *dirty-render-count* 0
        (gethash :value *dirty-state*) "before")
  (ptui.widgets.defwidget:invalidate-widget 'dirty-probe)
  (let ((first (dirty-probe *dirty-state*)))
    (is (= *dirty-render-count* 1))
    (is (string= (%element-text first) "before")))
  (setf (gethash :value *dirty-state*) "after")
  (let ((still-cached (dirty-probe *dirty-state*)))
    (is (= *dirty-render-count* 1))
    (is (string= (%element-text still-cached) "before")))
  (ptui.widgets.defwidget:mark-widget-dirty 'dirty-probe)
  (let ((rerendered (dirty-probe *dirty-state*)))
    (is (= *dirty-render-count* 2))
    (is (string= (%element-text rerendered) "after"))))

(defun run-all ()
  (run! 'defwidget-suite))
