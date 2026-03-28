(defpackage :ptui.examples.text-layout-basics
  (:use :cl)
  (:export #:wrapped-lines #:render-text-layout-demo #:main))

(in-package :ptui.examples.text-layout-basics)

(defparameter +sample-text+
  "PTUI text layout keeps grapheme clusters intact while wrapping descriptive text into narrow terminal regions.")

(defun wrapped-lines (width)
  (ptui.text.layout:wrap-by-width +sample-text+ (max 1 width) :engine :fallback))

(defun render-text-layout-demo (cols rows)
  (let* ((buf (ptui.render.buffer:make-buffer cols rows))
         (panel (ptui.core.types:make-rect 1 1
                                           (max 2 (- cols 2))
                                           (max 2 (- rows 2))))
         (title-cell (ptui.core.types:make-cell
                      " "
                      (ptui.core.color:make-color-rgb 140 220 170)
                      :default
                      (ptui.core.types:make-attrs :boldp t)))
         (inner-width (max 1 (- cols 6)))
         (lines (wrapped-lines inner-width)))
    (ptui.render.buffer:buffer-draw-border buf panel :border-style :rounded)
    (ptui.render.buffer:buffer-draw-text
     buf 3 2
     (list (list "PTUI text layout basics" title-cell)))
    (loop for line in lines
          for row from 4
          while (< row (max 4 (- rows 2))) do
            (ptui.render.buffer:buffer-draw-text
             buf 3 row line))
    buf))

(defun %render-text-layout-demo (_state size)
  (declare (ignore _state))
  (render-text-layout-demo (ptui.core.types:size-cols size)
                           (ptui.core.types:size-rows size)))

(defun main (&rest argv)
  (declare (ignore argv))
  (ptui.engine.loop:run #'%render-text-layout-demo :backend :auto :fps 10))
