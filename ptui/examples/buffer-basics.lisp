(defpackage :ptui.examples.buffer-basics
  (:use :cl)
  (:export #:render-buffer-demo #:main))

(in-package :ptui.examples.buffer-basics)

(defun %label-cell ()
  (ptui.core.types:make-cell
   " "
   (ptui.core.color:make-color-rgb 120 210 255)
   :default
   (ptui.core.types:make-attrs :boldp t)))

(defun render-buffer-demo (cols rows)
  (let* ((buf (ptui.render.buffer:make-buffer cols rows))
         (outer (ptui.core.types:make-rect 0 0 cols rows))
         (panel (ptui.core.types:make-rect 2 2
                                           (max 4 (- cols 4))
                                           (max 4 (- rows 4))))
         (fill (ptui.core.types:make-cell
                "."
                (ptui.core.color:make-color-rgb 90 140 210)
                :default
                (ptui.core.types:make-attrs))))
    (ptui.render.buffer:buffer-draw-border buf outer)
    (ptui.render.buffer:buffer-draw-border buf panel :border-style :rounded)
    (ptui.render.buffer:buffer-fill-rect
     buf
     (ptui.core.types:make-rect 4 4
                                (max 0 (- cols 8))
                                (max 0 (- rows 8)))
     fill)
    (ptui.render.buffer:buffer-draw-text
     buf 2 1
     (list (list "PTUI buffer basics" (%label-cell))))
    (ptui.render.buffer:buffer-draw-text
     buf 4 3
     "Draw borders, fill rects, and styled text directly.")
    buf))

(defun %render-buffer-demo (_state size)
  (declare (ignore _state))
  (render-buffer-demo (ptui.core.types:size-cols size)
                      (ptui.core.types:size-rows size)))

(defun main (&rest argv)
  (declare (ignore argv))
  (ptui.engine.loop:run #'%render-buffer-demo :backend :auto :fps 10))
