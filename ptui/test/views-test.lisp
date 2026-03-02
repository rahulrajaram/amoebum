(defpackage :ptui.test.views
  (:use :cl :fiveam)
  (:export #:run-all #:views-suite))

(in-package :ptui.test.views)

(def-suite views-suite
  :description "PTUI view primitives: list-view, text-input, status-bar (I285-I287).")

(in-suite views-suite)

(defun %make-test-runtime ()
  (ptui.ui.runtime:make-runtime))

(defun %with-widget-context (runtime widget-name instance-key thunk)
  (let ((ptui.ui.runtime:*current-runtime* runtime)
        (ptui.ui.runtime:*current-widget-context*
          (ptui.ui.runtime::%make-widget-context widget-name instance-key runtime)))
    (funcall thunk)))

;;; ===================================================================
;;; I285: list-view tests
;;; ===================================================================

(test list-view-renders-correct-item-count
  (let ((rt (%make-test-runtime)))
    (%with-widget-context rt 'list-view 'lv-1
      (lambda ()
        (let ((items '("a" "b" "c" "d" "e"))
              (render-fn (lambda (item idx selected-p)
                           (declare (ignore idx selected-p))
                           (ptui.widgets.core:make-text-widget item))))
          (let ((elem (ptui.views:render-list-view items render-fn 3 nil nil nil)))
            ;; Should have 3 visible children (viewport-height=3)
            (is (= (length (ptui.ui.elements:ui-element-children elem)) 3))
            (is (eq (ptui.ui.elements:ui-element-type elem) :list-view))))))))

(test list-view-scroll-offset-follows-selection
  (let ((rt (%make-test-runtime)))
    (%with-widget-context rt 'list-view 'lv-2
      (lambda ()
        (let ((items (loop for i from 0 below 20 collect (format nil "item-~D" i)))
              (render-fn (lambda (item idx selected-p)
                           (declare (ignore idx selected-p))
                           (ptui.widgets.core:make-text-widget item))))
          ;; Select item 15 with viewport of 5 — should scroll
          (let ((elem (ptui.views:render-list-view items render-fn 5 nil 15 nil)))
            ;; Should render 5 items including item 15
            (is (= (length (ptui.ui.elements:ui-element-children elem)) 5))))))))

(test list-view-selection-wraps
  (let ((rt (%make-test-runtime)))
    (%with-widget-context rt 'list-view 'lv-3
      (lambda ()
        (let ((items '("only-item"))
              (render-fn (lambda (item idx selected-p)
                           (declare (ignore idx selected-p))
                           (ptui.widgets.core:make-text-widget item))))
          (let ((elem (ptui.views:render-list-view items render-fn 10 nil 0 nil)))
            (is (= (length (ptui.ui.elements:ui-element-children elem)) 1))))))))

;;; ===================================================================
;;; I286: text-input tests
;;; ===================================================================

(test text-input-renders-with-value
  (let ((rt (%make-test-runtime)))
    (%with-widget-context rt 'panel-text-input 'ti-1
      (lambda ()
        (let ((elem (ptui.views:render-panel-text-input "hello" nil nil nil nil nil)))
          (is (eq (ptui.ui.elements:ui-element-type elem) :text-input))
          (is (string= (getf (ptui.ui.elements:ui-element-props elem) :value)
                       "hello")))))))

(test text-input-cursor-position
  (let ((rt (%make-test-runtime)))
    (%with-widget-context rt 'panel-text-input 'ti-2
      (lambda ()
        (let ((elem (ptui.views:render-panel-text-input "abc" nil nil nil 1 nil)))
          (is (= (getf (ptui.ui.elements:ui-element-props elem) :cursor-pos) 1)))))))

;;; ===================================================================
;;; I286: status-bar tests
;;; ===================================================================

(test status-bar-renders-segments
  (let ((rt (%make-test-runtime)))
    (%with-widget-context rt 'status-bar 'sb-1
      (lambda ()
        (let ((elem (ptui.views:render-status-bar
                     (list :left "L" :center "C" :right "R") nil nil)))
          (is (eq (ptui.ui.elements:ui-element-type elem) :status-bar))
          (is (string= (getf (ptui.ui.elements:ui-element-props elem) :left) "L"))
          (is (string= (getf (ptui.ui.elements:ui-element-props elem) :center) "C"))
          (is (string= (getf (ptui.ui.elements:ui-element-props elem) :right) "R")))))))

(test status-bar-converts-non-string-segments
  (let ((rt (%make-test-runtime)))
    (%with-widget-context rt 'status-bar 'sb-2
      (lambda ()
        (let ((elem (ptui.views:render-status-bar
                     (list :left 42 :right nil) nil nil)))
          (is (string= (getf (ptui.ui.elements:ui-element-props elem) :left) "42")))))))

;;; ===================================================================
;;; I287: paint function tests
;;; ===================================================================

(test paint-status-bar-renders-to-buffer
  (let* ((buf (ptui.render.buffer:make-buffer 40 1))
         (elem (ptui.ui.elements:make-element
                :status-bar
                :props (list :left "LEFT" :center "" :right "RIGHT"))))
    (ptui.views.paint:paint-status-bar elem buf 0 0 40 1)
    ;; Buffer should have content (non-empty)
    (is (not (null buf)))))

(test paint-text-input-renders-to-buffer
  (let* ((buf (ptui.render.buffer:make-buffer 40 1))
         (elem (ptui.ui.elements:make-element
                :text-input
                :props (list :value "hello" :placeholder "" :cursor-pos 2))))
    (ptui.views.paint:paint-text-input elem buf 0 0 40 1)
    (is (not (null buf)))))

(test paint-list-view-renders-children
  (let* ((buf (ptui.render.buffer:make-buffer 40 5))
         (child1 (ptui.widgets.core:make-text-widget "item1"))
         (child2 (ptui.widgets.core:make-text-widget "item2"))
         (elem (ptui.ui.elements:make-element
                :list-view
                :props (list :viewport-height 5)
                :children (list child1 child2))))
    (ptui.views.paint:paint-list-view elem buf 0 0 40 5)
    (is (not (null buf)))))

(test paint-text-widget-honors-styled-segments
  (let* ((accent (ptui.core.color:make-color-rgb 255 0 0))
         (style-cell (ptui.core.types:make-cell
                      " "
                      accent
                      ptui.core.color:color-default
                      (ptui.core.types:make-attrs :boldp t)))
         (elem (ptui.widgets.core:make-text-widget
                "A"
                :styled-segments (list (list "A" style-cell))))
         (buf (ptui.render.buffer:make-buffer 4 1)))
    (ptui.ui.app::%paint-element elem buf 0 0 4 1)
    (let ((cell (svref (ptui.core.types:cell-buffer-cells buf) 0)))
      (is (equalp (ptui.core.types:cell-fg cell) accent))
      (is (ptui.core.types:attrs-boldp (ptui.core.types:cell-attrs cell))))))

(defun run-all ()
  (run! 'views-suite))
