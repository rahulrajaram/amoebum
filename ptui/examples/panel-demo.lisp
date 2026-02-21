(defpackage :ptui.examples.panel-demo
  (:use :cl)
  (:export #:run-panel-demo))

(in-package :ptui.examples.panel-demo)

;;; ===================================================================
;;; I291: defpanel Example — 3-Region Layout Demo
;;; ===================================================================

(defun %render-counter-item (item index selected-p)
  "Render a single item in the list."
  (declare (ignore index))
  (ptui.widgets.core:make-text-widget
   (if selected-p
       (format nil "> ~A" item)
       (format nil "  ~A" item))))

(ptui.ui.panel:defpanel counter-panel (items title)
  (:state
    (selected-index 0 :type fixnum))
  (:data
    (item-count (length items) :deps (items))
    (status-text (format nil "~A | ~D items" title item-count) :deps (title item-count)))
  (:layout
    (:column
      (header :fixed 1
        (ptui.widgets.core:make-text-widget
         (format nil "=== ~A ===" title)))
      (content :flex 1
        (ptui.views:list-view items #'%render-counter-item 10 nil selected-index nil))
      (footer :fixed 1
        (ptui.views:status-bar
         (list :left status-text :right (format nil "sel:~D" selected-index))
         nil nil))))
  (:keys
    (:up (funcall set-selected-index (max 0 (1- selected-index))))
    (:down (funcall set-selected-index (min (1- item-count) (1+ selected-index))))))

(ptui.ui.app:defapp panel-demo-app (:fps 20)
  (counter-panel
   (list "Alpha" "Beta" "Gamma" "Delta" "Epsilon"
         "Zeta" "Eta" "Theta" "Iota" "Kappa")
   "Panel Demo"))
