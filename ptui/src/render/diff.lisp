(defpackage :ptui.render.diff
  (:use :cl)
  (:export #:draw-op #:make-draw-op
           #:diff-buffers))

(in-package :ptui.render.diff)

(defparameter *full-redraw-threshold* 0.60d0)

(defstruct (draw-op (:constructor make-draw-op (kind &key row col fg bg attrs text)))
  kind
  row
  col
  fg
  bg
  attrs
  text)

(defun attrs-equal-p (a b)
  (and (eql (ptui.core.types:attrs-boldp a) (ptui.core.types:attrs-boldp b))
       (eql (ptui.core.types:attrs-italicp a) (ptui.core.types:attrs-italicp b))
       (eql (ptui.core.types:attrs-underlinep a) (ptui.core.types:attrs-underlinep b))
       (eql (ptui.core.types:attrs-invertp a) (ptui.core.types:attrs-invertp b))
       (eql (ptui.core.types:attrs-dimp a) (ptui.core.types:attrs-dimp b))
       (eql (ptui.core.types:attrs-strikep a) (ptui.core.types:attrs-strikep b))))

(defun cell-style-equal-p (a b)
  (and (equalp (ptui.core.types:cell-fg a) (ptui.core.types:cell-fg b))
       (equalp (ptui.core.types:cell-bg a) (ptui.core.types:cell-bg b))
       (attrs-equal-p (ptui.core.types:cell-attrs a)
                      (ptui.core.types:cell-attrs b))))

(defun cell-equal-p (a b)
  (and (string= (ptui.core.types:cell-glyph a) (ptui.core.types:cell-glyph b))
       (cell-style-equal-p a b)))

(defun blank-glyph-p (glyph)
  (or (string= glyph " ")
      (string= glyph "")))

(defun same-buffer-shape-p (a b)
  (and a b
       (= (ptui.core.types:cell-buffer-cols a)
          (ptui.core.types:cell-buffer-cols b))
       (= (ptui.core.types:cell-buffer-rows a)
          (ptui.core.types:cell-buffer-rows b))))

(defun buffer-cell-at (buf row col)
  (let ((cols (ptui.core.types:cell-buffer-cols buf)))
    (svref (ptui.core.types:cell-buffer-cells buf)
           (+ col (* row cols)))))

(defun count-different-cells (prev next)
  (let* ((next-cells (ptui.core.types:cell-buffer-cells next))
         (count (length next-cells)))
    (if (not (same-buffer-shape-p prev next))
        count
        (loop for i from 0 below count
              count (not (cell-equal-p
                          (svref (ptui.core.types:cell-buffer-cells prev) i)
                          (svref next-cells i)))))))

(defun should-use-full-redraw-p (prev next full-redraw)
  (or full-redraw
      (null prev)
      (not (same-buffer-shape-p prev next))
      (let* ((total (* (ptui.core.types:cell-buffer-cols next)
                       (ptui.core.types:cell-buffer-rows next)))
             (diff-count (count-different-cells prev next)))
        (> (/ (float diff-count 1d0) (max 1 total))
           *full-redraw-threshold*))))

(defstruct (row-diff-state (:constructor %make-row-diff-state (ops cols)))
  ops
  cols
  (col 0))

(defun %row-update-needed-p (prev next row col full-row-p)
  (or full-row-p
      (not (same-buffer-shape-p prev next))
      (not (cell-equal-p (buffer-cell-at prev row col)
                         (buffer-cell-at next row col)))))

(defun %row-trailing-clearable-p (prev next row start next-cell full-row-p cols)
  (and (< start cols)
       (loop for col from start below cols
             for cell = (buffer-cell-at next row col)
             always (and (blank-glyph-p (ptui.core.types:cell-glyph cell))
                         (cell-style-equal-p cell next-cell)))
       (loop for col from start below cols
             thereis (%row-update-needed-p prev next row col full-row-p))))

(defun %row-push-op! (state op)
  (push op (row-diff-state-ops state)))

(defun %row-emit-clear-eol! (state row clear-col)
  (%row-push-op! state (make-draw-op :move :row row :col clear-col))
  (%row-push-op! state (make-draw-op :clear-eol))
  (setf (row-diff-state-col state) (row-diff-state-cols state)))

(defun %row-run-text (prev next row next-cell state full-row-p)
  (let ((clear-col nil))
    (values
     (with-output-to-string (out)
       (loop while (< (row-diff-state-col state) (row-diff-state-cols state)) do
         (let* ((col (row-diff-state-col state))
                (cell (buffer-cell-at next row col)))
           (cond
             ((not (%row-update-needed-p prev next row col full-row-p))
              (return))
             ((not (cell-style-equal-p cell next-cell))
              (return))
             ((and (blank-glyph-p (ptui.core.types:cell-glyph cell))
                   (%row-trailing-clearable-p prev next row col cell full-row-p
                                              (row-diff-state-cols state)))
              (setf clear-col col)
              (return))
             (t
              (write-string (ptui.core.types:cell-glyph cell) out)
              (incf (row-diff-state-col state)))))))
     clear-col)))

(defun %row-emit-text-run! (state prev next row next-cell full-row-p)
  (multiple-value-bind (text clear-col)
      (%row-run-text prev next row next-cell state full-row-p)
    (%row-push-op! state (make-draw-op :write :text text))
    (when clear-col
      (%row-emit-clear-eol! state row clear-col))))

(defun %row-start-run! (state row col next-cell)
  (%row-push-op! state (make-draw-op :move :row row :col col))
  (%row-push-op! state
                 (make-draw-op :style
                               :fg (ptui.core.types:cell-fg next-cell)
                               :bg (ptui.core.types:cell-bg next-cell)
                               :attrs (ptui.core.types:cell-attrs next-cell))))

(defun %row-dirty-p (prev next row force-full)
  (or force-full
      (not (same-buffer-shape-p prev next))
      (loop for col from 0 below (ptui.core.types:cell-buffer-cols next)
            thereis (not (cell-equal-p
                          (buffer-cell-at prev row col)
                          (buffer-cell-at next row col))))))

(defun emit-row-run-diff (ops prev next row full-row-p)
  (let ((state (%make-row-diff-state ops (ptui.core.types:cell-buffer-cols next))))
    (loop while (< (row-diff-state-col state) (row-diff-state-cols state)) do
      (let ((col (row-diff-state-col state)))
        (if (not (%row-update-needed-p prev next row col full-row-p))
            (incf (row-diff-state-col state))
            (let ((next-cell (buffer-cell-at next row col)))
              (%row-start-run! state row col next-cell)
              (if (%row-trailing-clearable-p prev next row col next-cell full-row-p
                                             (row-diff-state-cols state))
                  (%row-emit-clear-eol! state row col)
                  (%row-emit-text-run! state prev next row next-cell full-row-p))))))
    (row-diff-state-ops state)))

(defun diff-buffers (prev next &key (full-redraw nil))
  (when (and (eq prev next) (not full-redraw))
    (return-from diff-buffers (values '() 0)))
  (let* ((force-full (should-use-full-redraw-p prev next full-redraw))
         (rows (ptui.core.types:cell-buffer-rows next))
         (ops '()))
    (when force-full
      (push (make-draw-op :clear-screen) ops))
    (loop for row from 0 below rows do
      (when (%row-dirty-p prev next row force-full)
        (setf ops (emit-row-run-diff ops prev next row force-full))))
    (setf ops (nreverse ops))
    (values ops (length ops))))
