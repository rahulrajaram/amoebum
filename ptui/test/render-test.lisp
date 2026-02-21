(defpackage :ptui.test.render
  (:use :cl :fiveam)
  (:export #:run-all #:ptui-render-suite))

(in-package :ptui.test.render)

(def-suite ptui-render-suite
  :description "PTUI render buffer and diff coverage (I307).")

(in-suite ptui-render-suite)

(defun %buffer-cell (buf x y)
  (let ((cols (ptui.core.types:cell-buffer-cols buf)))
    (svref (ptui.core.types:cell-buffer-cells buf)
           (+ x (* y cols)))))

(defun %default-cell-equal-p (cell)
  (and (string= (ptui.core.types:cell-glyph cell) " ")
       (equalp (ptui.core.types:cell-fg cell) ptui.core.color:color-default)
       (equalp (ptui.core.types:cell-bg cell) ptui.core.color:color-default)))

(defun %op-kinds (ops)
  (mapcar #'ptui.render.diff::draw-op-kind ops))

(test render-make-buffer-creates-default-cells
  (let ((buf (ptui.render.buffer:make-buffer 4 2)))
    (is (= 4 (ptui.core.types:cell-buffer-cols buf)))
    (is (= 2 (ptui.core.types:cell-buffer-rows buf)))
    (is (every #'%default-cell-equal-p
               (coerce (ptui.core.types:cell-buffer-cells buf) 'list)))))

(test render-buffer-draw-text-populates-cells
  (let ((buf (ptui.render.buffer:make-buffer 6 1)))
    (ptui.render.buffer:buffer-draw-text buf 1 0 "AB")
    (is (string= (ptui.core.types:cell-glyph (%buffer-cell buf 0 0)) " "))
    (is (string= (ptui.core.types:cell-glyph (%buffer-cell buf 1 0)) "A"))
    (is (string= (ptui.core.types:cell-glyph (%buffer-cell buf 2 0)) "B"))
    (is (string= (ptui.core.types:cell-glyph (%buffer-cell buf 3 0)) " "))))

(test render-buffer-fill-rect-respects-bounds
  (let ((buf (ptui.render.buffer:make-buffer 5 4))
        (fill-cell (ptui.core.types:make-cell "X" :default :default
                                             (ptui.core.types:make-attrs :boldp t))))
    (ptui.render.buffer:buffer-fill-rect buf
                                         (ptui.core.types:make-rect 2 1 6 10)
                                         fill-cell)
    (is (string= (ptui.core.types:cell-glyph (%buffer-cell buf 1 1)) " "))
    (is (string= (ptui.core.types:cell-glyph (%buffer-cell buf 2 1)) "X"))
    (is (string= (ptui.core.types:cell-glyph (%buffer-cell buf 4 3)) "X"))
    (is (string= (ptui.core.types:cell-glyph (%buffer-cell buf 2 0)) " "))))

(test render-buffer-draw-border-uses-corners
  (let ((buf (ptui.render.buffer:make-buffer 6 6))
        (border-cell (ptui.core.types:make-cell " " :default :default
                                                (ptui.core.types:make-attrs :underlinep t))))
    (ptui.render.buffer:buffer-draw-border buf
                                           (ptui.core.types:make-rect 1 1 4 3)
                                           :style border-cell
                                           :border-style :square)
    (is (string= (ptui.core.types:cell-glyph (%buffer-cell buf 1 1)) "┌"))
    (is (string= (ptui.core.types:cell-glyph (%buffer-cell buf 4 1)) "┐"))
    (is (string= (ptui.core.types:cell-glyph (%buffer-cell buf 1 3)) "└"))
    (is (string= (ptui.core.types:cell-glyph (%buffer-cell buf 4 3)) "┘"))
    (is (string= (ptui.core.types:cell-glyph (%buffer-cell buf 2 1)) "─"))
    (is (string= (ptui.core.types:cell-glyph (%buffer-cell buf 1 2)) "│"))
    (is (string= (ptui.core.types:cell-glyph (%buffer-cell buf 2 2)) " ")
        )))

(test render-buffer-subrect-clips-with-buffer
  (let ((buf (ptui.render.buffer:make-buffer 4 4)))
    (let ((partial (ptui.render.buffer:buffer-subrect buf
                                                     (ptui.core.types:make-rect -1 -1 2 2))))
      (is (= 0 (ptui.core.types:rect-x partial)))
      (is (= 0 (ptui.core.types:rect-y partial)))
      (is (= 1 (ptui.core.types:rect-w partial)))
      (is (= 1 (ptui.core.types:rect-h partial))))
    (is (null (ptui.render.buffer:buffer-subrect buf (ptui.core.types:make-rect 4 0 1 1))))))

(test render-buffer-write-cell-if-visible-respects-clipping
  (let ((buf (ptui.render.buffer:make-buffer 3 3))
        (cell (ptui.core.types:make-cell "Y" :default :default (ptui.core.types:make-attrs)))
        (clip (ptui.core.types:make-rect 1 1 1 1)))
    (ptui.render.buffer:write-cell-if-visible buf 1 1 cell clip)
    (ptui.render.buffer:write-cell-if-visible buf 2 1 cell clip)
    (is (string= (ptui.core.types:cell-glyph (%buffer-cell buf 1 1)) "Y"))
    (is (string= (ptui.core.types:cell-glyph (%buffer-cell buf 2 1)) " "))))

(test render-buffer-with-clip-culls-drawing
  (let ((buf (ptui.render.buffer:make-buffer 5 5))
        (fill-cell (ptui.core.types:make-cell "Z" :default :default
                                             (ptui.core.types:make-attrs))))
    (ptui.render.buffer:with-clip (buf (ptui.core.types:make-rect 1 1 2 2))
      (ptui.render.buffer:buffer-fill-rect buf (ptui.core.types:make-rect 0 0 5 5) fill-cell))
    (is (string= (ptui.core.types:cell-glyph (%buffer-cell buf 0 0)) " "))
    (is (string= (ptui.core.types:cell-glyph (%buffer-cell buf 1 1)) "Z"))
    (is (string= (ptui.core.types:cell-glyph (%buffer-cell buf 2 2)) "Z"))
    (is (string= (ptui.core.types:cell-glyph (%buffer-cell buf 3 3)) " "))))

(test render-diff-cell-equal-p-compares-glyph-and-style
  (let ((style-a (ptui.core.types:make-attrs :boldp t :italicp t))
        (style-b (ptui.core.types:make-attrs :boldp t :italicp t))
        (style-c (ptui.core.types:make-attrs :boldp nil :italicp t)))
    (let ((cell-a (ptui.core.types:make-cell "A" :default :default style-a))
          (cell-b (ptui.core.types:make-cell "A" :default :default style-b))
          (cell-c (ptui.core.types:make-cell "B" :default :default style-a))
          (cell-d (ptui.core.types:make-cell "A" :default :default style-c)))
    (is (ptui.render.diff::cell-equal-p cell-a cell-b))
    (is (not (ptui.render.diff::cell-equal-p cell-a cell-c)))
    (is (not (ptui.render.diff::cell-equal-p cell-a cell-d))))))

(test render-diff-identical-buffers-yield-no-ops
  (let ((buf (ptui.render.buffer:make-buffer 3 1)))
    (multiple-value-bind (ops count) (ptui.render.diff:diff-buffers buf buf :full-redraw nil)
      (is (null ops))
      (is (= 0 count)))))

(test render-diff-single-change-uses-minimal-ops
  (let ((prev (ptui.render.buffer:make-buffer 5 1))
        (next (ptui.render.buffer:make-buffer 5 1)))
    (ptui.render.buffer:buffer-draw-text next 2 0 "A")
    (multiple-value-bind (ops count) (ptui.render.diff:diff-buffers prev next :full-redraw nil)
      (let ((kinds (%op-kinds ops)))
        (is (not (member :clear-screen kinds)))
        (is (= 3 (length kinds)))
        (is (equal kinds '(:move :style :write)))
        (is (= count 3))))))

(test render-diff-contiguous-style-runs-merge-into-single-write
  (let ((prev (ptui.render.buffer:make-buffer 10 1))
        (next (ptui.render.buffer:make-buffer 10 1)))
    (ptui.render.buffer:buffer-draw-text next 0 0 "HELLO")
    (multiple-value-bind (ops count) (ptui.render.diff:diff-buffers prev next :full-redraw nil)
      (let ((kinds (%op-kinds ops)))
        (is (not (member :clear-screen kinds)))
        (is (= 3 (length kinds)))
        (is (= 1 (count :write kinds)))
        (is (= 1 (count :style kinds)))
        (is (= count 3))))))

(test render-diff-threshold-forced-full-redraw
  (let ((prev (ptui.render.buffer:make-buffer 5 3))
        (next (ptui.render.buffer:make-buffer 5 3)))
    ;; 10 of 15 cells changed => >60% threshold
    (ptui.render.buffer:buffer-fill-rect next
                                         (ptui.core.types:make-rect 0 0 5 2)
                                         (ptui.core.types:make-cell "X" :default :default
                                                                   (ptui.core.types:make-attrs)))
    (multiple-value-bind (ops count) (ptui.render.diff:diff-buffers prev next :full-redraw nil)
      (let ((kinds (%op-kinds ops)))
        (is (eq :clear-screen (first kinds)))
        (is (member :write kinds))
        (is (> count 1))))))

(defun run-all ()
  (run! 'ptui-render-suite))
