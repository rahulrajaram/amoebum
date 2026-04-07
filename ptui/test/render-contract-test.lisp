(defpackage :ptui.test.render-contract
  (:use :cl :fiveam)
  (:export #:run-all #:ptui-render-contract-suite))

(in-package :ptui.test.render-contract)

(def-suite ptui-render-contract-suite
  :description "PTUI foundational render contracts: buffer mutation and diff behavior.")

(in-suite ptui-render-contract-suite)

(defun %buffer-cell (buf x y)
  (svref (ptui.core.types:cell-buffer-cells buf)
         (+ x (* y (ptui.core.types:cell-buffer-cols buf)))))

(defun %default-cell-p (cell)
  (and (string= " " (ptui.core.types:cell-glyph cell))
       (eq ptui.core.color:color-default (ptui.core.types:cell-fg cell))
       (eq ptui.core.color:color-default (ptui.core.types:cell-bg cell))
       (not (ptui.core.types:attrs-boldp (ptui.core.types:cell-attrs cell)))
       (not (ptui.core.types:attrs-italicp (ptui.core.types:cell-attrs cell)))
       (not (ptui.core.types:attrs-underlinep (ptui.core.types:cell-attrs cell)))
       (not (ptui.core.types:attrs-invertp (ptui.core.types:cell-attrs cell)))
       (not (ptui.core.types:attrs-dimp (ptui.core.types:cell-attrs cell)))
       (not (ptui.core.types:attrs-strikep (ptui.core.types:cell-attrs cell)))))

(defun %op-kinds (ops)
  (mapcar #'ptui.render.diff::draw-op-kind ops))

(defun %rgb-channels (color)
  (list (ptui.core.color:color-rgb-r color)
        (ptui.core.color:color-rgb-g color)
        (ptui.core.color:color-rgb-b color)))

(test render-buffer-clear-applies-fill-template-in-place
  (let* ((buf (ptui.render.buffer:make-buffer 2 1))
         (cell-a (%buffer-cell buf 0 0))
         (cell-b (%buffer-cell buf 1 0))
         (fill (ptui.core.types:make-cell
                "Z"
                (ptui.core.color:make-color-rgb 1 2 3)
                :default
                (ptui.core.types:make-attrs :boldp t))))
    (ptui.render.buffer:buffer-draw-text buf 0 0 "HI")
    (ptui.render.buffer:buffer-clear buf :fill fill)
    (is (eq cell-a (%buffer-cell buf 0 0)))
    (is (eq cell-b (%buffer-cell buf 1 0)))
    (dolist (cell (list (%buffer-cell buf 0 0) (%buffer-cell buf 1 0)))
      (is (string= "Z" (ptui.core.types:cell-glyph cell)))
      (is (equal '(1 2 3) (%rgb-channels (ptui.core.types:cell-fg cell))))
      (is (eq ptui.core.color:color-default (ptui.core.types:cell-bg cell)))
      (is-true (ptui.core.types:attrs-boldp (ptui.core.types:cell-attrs cell))))))

(test render-buffer-reset-restores-default-state-in-place
  (let* ((buf (ptui.render.buffer:make-buffer 2 1))
         (cell-a (%buffer-cell buf 0 0))
         (cell-b (%buffer-cell buf 1 0))
         (fill (ptui.core.types:make-cell
                "X"
                (ptui.core.color:make-color-rgb 8 9 10)
                #(11 12 13)
                (ptui.core.types:make-attrs :underlinep t))))
    (ptui.render.buffer:buffer-fill-rect
     buf
     (ptui.core.types:make-rect 0 0 2 1)
     fill)
    (ptui.render.buffer:buffer-reset buf)
    (is (eq cell-a (%buffer-cell buf 0 0)))
    (is (eq cell-b (%buffer-cell buf 1 0)))
    (is-true (%default-cell-p (%buffer-cell buf 0 0)))
    (is-true (%default-cell-p (%buffer-cell buf 1 0)))))

(test render-buffer-draw-styled-segments-handles-wide-glyphs
  (let* ((buf (ptui.render.buffer:make-buffer 4 1))
         (style-a (ptui.core.types:make-cell
                   " "
                   (ptui.core.color:make-color-rgb 255 0 0)
                   :default
                   (ptui.core.types:make-attrs :boldp t)))
         (style-b (ptui.core.types:make-cell
                   " "
                   :default
                   (ptui.core.color:make-color-rgb 0 0 255)
                   (ptui.core.types:make-attrs :underlinep t))))
    (ptui.render.buffer:buffer-draw-styled-segments
     buf
     0
     0
     (list (cons "界" 1)
           (cons "A" 2))
     (lambda (style-id)
       (ecase style-id
         (1 style-a)
         (2 style-b))))
    (is (string= "界" (ptui.core.types:cell-glyph (%buffer-cell buf 0 0))))
    (is (string= "" (ptui.core.types:cell-glyph (%buffer-cell buf 1 0))))
    (is (equal '(255 0 0) (%rgb-channels (ptui.core.types:cell-fg (%buffer-cell buf 0 0)))))
    (is-true (ptui.core.types:attrs-boldp
              (ptui.core.types:cell-attrs (%buffer-cell buf 0 0))))
    (is (string= "A" (ptui.core.types:cell-glyph (%buffer-cell buf 2 0))))
    (is (equal '(0 0 255) (%rgb-channels (ptui.core.types:cell-bg (%buffer-cell buf 2 0)))))
    (is-true (ptui.core.types:attrs-underlinep
              (ptui.core.types:cell-attrs (%buffer-cell buf 2 0))))))

(test render-buffer-draw-text-max-width-counts-display-cells
  (let ((buf (ptui.render.buffer:make-buffer 4 1)))
    (ptui.render.buffer:buffer-draw-text buf 0 0 "界A" :max-width 2)
    (is (string= "界" (ptui.core.types:cell-glyph (%buffer-cell buf 0 0))))
    (is (string= "" (ptui.core.types:cell-glyph (%buffer-cell buf 1 0))))
    (is (string= " " (ptui.core.types:cell-glyph (%buffer-cell buf 2 0))))))

(test render-buffer-draw-border-handles-single-cell-rectangles
  (let ((buf (ptui.render.buffer:make-buffer 3 3)))
    (ptui.render.buffer:buffer-draw-border
     buf
     (ptui.core.types:make-rect 1 1 1 1)
     :border-style :square)
    (is (string= "┌" (ptui.core.types:cell-glyph (%buffer-cell buf 1 1))))
    (is (string= " " (ptui.core.types:cell-glyph (%buffer-cell buf 0 1))))
    (is (string= " " (ptui.core.types:cell-glyph (%buffer-cell buf 1 0))))))

(test render-diff-explicit-full-redraw-forces-clear-screen
  (let ((prev (ptui.render.buffer:make-buffer 3 1))
        (next (ptui.render.buffer:make-buffer 3 1)))
    (multiple-value-bind (ops count)
        (ptui.render.diff:diff-buffers prev next :full-redraw t)
      (is (eq :clear-screen (first (%op-kinds ops))))
      (is (> count 0)))))

(test render-diff-splits-runs-when-style-changes
  (let ((prev (ptui.render.buffer:make-buffer 3 1))
        (next (ptui.render.buffer:make-buffer 3 1))
        (style-a (ptui.core.types:make-cell
                  " "
                  :default
                  :default
                  (ptui.core.types:make-attrs :boldp t)))
        (style-b (ptui.core.types:make-cell
                  " "
                  :default
                  :default
                  (ptui.core.types:make-attrs :underlinep t))))
    (ptui.render.buffer:buffer-draw-text
     next
     0
     0
     (list (list "A" style-a)
           (list "B" style-b)))
    (multiple-value-bind (ops count)
        (ptui.render.diff:diff-buffers prev next :full-redraw nil)
      (declare (ignore count))
      (let ((kinds (%op-kinds ops)))
        (is (>= (count :style kinds) 2))
        (is (= 2 (count :write kinds)))
        (is (>= (count :move kinds) 2))
        (is (equal "A" (ptui.render.diff::draw-op-text
                        (find :write ops :key #'ptui.render.diff::draw-op-kind))))
        (is (equal "B" (ptui.render.diff::draw-op-text
                        (second (remove-if-not
                                 (lambda (op)
                                   (eq :write
                                       (ptui.render.diff::draw-op-kind op)))
                                 ops)))))))))

(test render-diff-skips-unchanged-rows
  (let ((prev (ptui.render.buffer:make-buffer 4 2))
        (next (ptui.render.buffer:make-buffer 4 2)))
    (ptui.render.buffer:buffer-draw-text prev 0 0 "AB")
    (ptui.render.buffer:buffer-draw-text next 0 0 "AB")
    (ptui.render.buffer:buffer-draw-text next 1 1 "Z")
    (multiple-value-bind (ops count)
        (ptui.render.diff:diff-buffers prev next :full-redraw nil)
      (declare (ignore count))
      (let ((moves (remove-if-not (lambda (op)
                                    (eq :move
                                        (ptui.render.diff::draw-op-kind op)))
                                  ops)))
        (is (plusp (length moves)))
        (is (every (lambda (op)
                     (= 1 (ptui.render.diff::draw-op-row op)))
                   moves))))))

(defun run-all ()
  (run! 'ptui-render-contract-suite))
