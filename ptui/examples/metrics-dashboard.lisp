(defpackage :ptui.examples.metrics-dashboard
  (:use :cl)
  (:export #:main))

(in-package :ptui.examples.metrics-dashboard)

(defun %template-cell (&key (fg :default) (bg :default) (boldp nil))
  (ptui.core.types:make-cell
   " "
   fg
   bg
   (ptui.core.types:make-attrs :boldp boldp)))

(defun %safe-inner-rect (cols rows)
  (ptui.core.types:make-rect
   1
   1
   (max 2 (- cols 2))
   (max 2 (- rows 2))))

(defun %fit-line (text)
  ;; Exercise the width-safe text pipeline without changing output semantics.
  (ptui.text.layout:truncate-to-width text (ptui.text.width:string-width text)))

(defun %draw-gradient (buf x y width)
  (loop for i from 0 below (max 0 width) do
    (let* ((ratio (if (> width 1)
                      (/ i (float (1- width)))
                    0.0))
           (r (round (* 255 ratio)))
           (g (round (* 220 (- 1.0 ratio))))
           (b (round (* 255 (- 1.0 ratio))))
           (cell (ptui.core.types:make-cell
                  "*"
                  (ptui.core.color:make-color-rgb r g b)
                  :default
                  (ptui.core.types:make-attrs))))
      (ptui.render.buffer:buffer-draw-text buf (+ x i) y cell))))

(defun %render-dashboard (state size)
  (declare (ignore state))
  (let* ((cols (ptui.core.types:size-cols size))
         (rows (ptui.core.types:size-rows size))
         (buf (ptui.render.buffer:make-buffer cols rows))
         (panel (%safe-inner-rect cols rows))
         (title-cell (%template-cell :fg (ptui.core.color:make-color-rgb 120 200 255) :boldp t))
         (muted-cell (%template-cell :fg (ptui.core.color:make-color-rgb 180 180 180)))
         (info-y 4)
         (bar-y (max 6 (- rows 3)))
         (bar-x 3)
         (bar-w (max 0 (- cols 6))))
    (ptui.render.buffer:buffer-draw-border buf panel)
    (ptui.render.buffer:buffer-draw-text
     buf 3 2
     (list (list (%fit-line "PTUI Metrics Dashboard") title-cell)))
    (ptui.render.buffer:buffer-draw-text
     buf 3 info-y
     (list (list (%fit-line (format nil "Terminal size: ~Dx~D" cols rows)) muted-cell)))
    (ptui.render.buffer:buffer-draw-text
     buf 3 (1+ info-y)
     (list (list (%fit-line "Press q or Ctrl-C to quit") muted-cell)))
    (%draw-gradient buf bar-x bar-y bar-w)
    buf))

(defun main (&rest argv)
  (declare (ignore argv))
  (ptui.engine.loop:run #'%render-dashboard :backend :auto :fps 20))
