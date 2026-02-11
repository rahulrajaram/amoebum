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

(defun emit-row-run-diff (ops prev next row full-row-p)
  (let ((cols (ptui.core.types:cell-buffer-cols next))
        (col 0))
    (labels ((update-needed-p (c)
               (or full-row-p
                   (not (same-buffer-shape-p prev next))
                   (not (cell-equal-p (buffer-cell-at prev row c)
                                      (buffer-cell-at next row c)))))
             (trailing-clearable-p (start next-cell)
               ;; If the rest of the row is spaces with the same style, prefer :clear-eol
               ;; to avoid emitting long runs of spaces (common when text shrinks).
               (and (< start cols)
                    (loop for c from start below cols
                          for cell = (buffer-cell-at next row c)
                          always (and (blank-glyph-p (ptui.core.types:cell-glyph cell))
                                      (cell-style-equal-p cell next-cell)))
                    (loop for c from start below cols
                          thereis (update-needed-p c))))
             (emit-text-run (next-cell)
               (let* ((clear-col nil)
                      (text
                       (with-output-to-string (out)
                         (loop while (< col cols) do
                           (let ((cell (buffer-cell-at next row col)))
                             (cond
                               ((or (not (update-needed-p col))
                                    (not (cell-style-equal-p cell next-cell)))
                               (return))
                               ;; If we've reached a run of spaces, prefer clear-eol for the tail.
                               ((and (blank-glyph-p (ptui.core.types:cell-glyph cell))
                                     (trailing-clearable-p col cell))
                                (setf clear-col col)
                                (return))
                               (t
                                (write-string (ptui.core.types:cell-glyph cell) out)
                                (incf col))))))))
                 (push (make-draw-op :write :text text) ops)
                 (when clear-col
                   (push (make-draw-op :move :row row :col clear-col) ops)
                   (push (make-draw-op :clear-eol) ops)
                   (setf col cols)))))
      (loop while (< col cols) do
        (if (not (update-needed-p col))
            (incf col)
            (let* ((next-cell (buffer-cell-at next row col))
                   (start col)
                   (fg (ptui.core.types:cell-fg next-cell))
                   (bg (ptui.core.types:cell-bg next-cell))
                   (attrs (ptui.core.types:cell-attrs next-cell)))
              (push (make-draw-op :move :row row :col start) ops)
              (push (make-draw-op :style :fg fg :bg bg :attrs attrs) ops)
              (if (trailing-clearable-p start next-cell)
                  (progn
                    (push (make-draw-op :clear-eol) ops)
                    (setf col cols))
                  (emit-text-run next-cell))))))
    ops))

(defun diff-buffers (prev next &key (full-redraw nil))
  (let* ((force-full (should-use-full-redraw-p prev next full-redraw))
         (rows (ptui.core.types:cell-buffer-rows next))
         (ops '()))
    (when force-full
      (push (make-draw-op :clear-screen) ops))
    (loop for row from 0 below rows do
      (let ((row-dirty-p
              (or force-full
                  (not (same-buffer-shape-p prev next))
                  (loop for col from 0 below (ptui.core.types:cell-buffer-cols next)
                        thereis (not (cell-equal-p
                                      (buffer-cell-at prev row col)
                                      (buffer-cell-at next row col)))))))
        (when row-dirty-p
          (setf ops (emit-row-run-diff ops prev next row force-full)))))
    (setf ops (nreverse ops))
    (values ops (length ops))))
