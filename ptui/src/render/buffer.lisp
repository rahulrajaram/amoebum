(defpackage :ptui.render.buffer
  (:use :cl)
  (:export #:make-buffer #:buffer-clear #:buffer-draw-text
           #:buffer-fill-rect #:buffer-draw-border
           #:buffer-subrect #:with-clip))

(in-package :ptui.render.buffer)

(defvar *clip-buffer* nil)
(defvar *clip-rect* nil)

(defun make-default-cell ()
  (ptui.core.types:make-cell " " nil nil (ptui.core.types:make-attrs)))

(defun clone-attrs (attrs)
  (ptui.core.types:make-attrs
   :boldp (ptui.core.types:attrs-boldp attrs)
   :italicp (ptui.core.types:attrs-italicp attrs)
   :underlinep (ptui.core.types:attrs-underlinep attrs)
   :invertp (ptui.core.types:attrs-invertp attrs)
   :dimp (ptui.core.types:attrs-dimp attrs)
   :strikep (ptui.core.types:attrs-strikep attrs)))

(defun clone-cell (cell)
  (ptui.core.types:make-cell
   (ptui.core.types:cell-glyph cell)
   (ptui.core.types:cell-fg cell)
   (ptui.core.types:cell-bg cell)
   (clone-attrs (ptui.core.types:cell-attrs cell))))

(defun cell-with-glyph (template glyph)
  (ptui.core.types:make-cell
   glyph
   (ptui.core.types:cell-fg template)
   (ptui.core.types:cell-bg template)
   (clone-attrs (ptui.core.types:cell-attrs template))))

(defun buffer-full-rect (buf)
  (ptui.core.types:make-rect
   0
   0
   (ptui.core.types:cell-buffer-cols buf)
   (ptui.core.types:cell-buffer-rows buf)))

(defun rect-intersection (a b)
  (let* ((ax (ptui.core.types:rect-x a))
         (ay (ptui.core.types:rect-y a))
         (bx (ptui.core.types:rect-x b))
         (by (ptui.core.types:rect-y b))
         (ax2 (+ ax (ptui.core.types:rect-w a)))
         (ay2 (+ ay (ptui.core.types:rect-h a)))
         (bx2 (+ bx (ptui.core.types:rect-w b)))
         (by2 (+ by (ptui.core.types:rect-h b)))
         (x1 (max ax bx))
         (y1 (max ay by))
         (x2 (min ax2 bx2))
         (y2 (min ay2 by2)))
    (when (and (> x2 x1) (> y2 y1))
      (ptui.core.types:make-rect x1 y1 (- x2 x1) (- y2 y1)))))

(defun clip-for-buffer (buf)
  (let ((full (buffer-full-rect buf)))
    (if (and *clip-buffer* (eq *clip-buffer* buf) *clip-rect*)
        (or (rect-intersection *clip-rect* full)
            (ptui.core.types:make-rect 0 0 0 0))
        full)))

(defun point-in-rect-p (x y rect)
  (and (>= x (ptui.core.types:rect-x rect))
       (< x (+ (ptui.core.types:rect-x rect) (ptui.core.types:rect-w rect)))
       (>= y (ptui.core.types:rect-y rect))
       (< y (+ (ptui.core.types:rect-y rect) (ptui.core.types:rect-h rect)))))

(defun buffer-index (buf x y)
  (+ x (* y (ptui.core.types:cell-buffer-cols buf))))

(defun write-cell-if-visible (buf x y cell clip)
  (when (point-in-rect-p x y clip)
    (setf (svref (ptui.core.types:cell-buffer-cells buf) (buffer-index buf x y))
          (clone-cell cell))))

(defun make-buffer (cols rows &key (fill (make-default-cell)))
  (let* ((count (* cols rows))
         (cells (make-array count)))
    (loop for i from 0 below count do
      (setf (svref cells i) (clone-cell fill)))
    (ptui.core.types:make-cell-buffer cols rows cells)))

(defun buffer-clear (buf &key (fill (make-default-cell)))
  (let* ((cells (ptui.core.types:cell-buffer-cells buf))
         (count (length cells)))
    (loop for i from 0 below count do
      (setf (svref cells i) (clone-cell fill))))
  nil)

(defun styled-segment->cells (segment)
  (labels ((string->cells (text template)
             (loop for ch across text collect
               (cell-with-glyph template (string ch)))))
    (cond
      ((stringp segment)
       (string->cells segment (make-default-cell)))
      ((typep segment 'ptui.core.types:cell)
       (list (clone-cell segment)))
      ((and (consp segment)
            (stringp (first segment))
            (typep (second segment) 'ptui.core.types:cell))
       (string->cells (first segment) (second segment)))
      (t
       (error "Unsupported styled text segment: ~S" segment)))))

(defun flatten-styled-text (styled-text)
  (cond
    ((null styled-text) '())
    ((or (stringp styled-text)
         (typep styled-text 'ptui.core.types:cell))
     (styled-segment->cells styled-text))
    ((listp styled-text)
     (loop for segment in styled-text append (styled-segment->cells segment)))
    (t
     (error "Unsupported styled-text value: ~S" styled-text))))

(defun buffer-subrect (buf rect)
  (rect-intersection (clip-for-buffer buf) rect))

(defun buffer-draw-text (buf x y styled-text &key (max-width nil))
  (let* ((clip (clip-for-buffer buf))
         (cells (flatten-styled-text styled-text))
         (limit (if max-width (max 0 max-width) most-positive-fixnum)))
    (loop for cell in cells
          for dx from 0
          while (< dx limit)
          do (write-cell-if-visible buf (+ x dx) y cell clip)))
  nil)

(defun buffer-fill-rect (buf rect cell)
  (let ((clip (buffer-subrect buf rect)))
    (when clip
      (loop for row from (ptui.core.types:rect-y clip)
                    below (+ (ptui.core.types:rect-y clip)
                             (ptui.core.types:rect-h clip))
            do (loop for col from (ptui.core.types:rect-x clip)
                          below (+ (ptui.core.types:rect-x clip)
                                   (ptui.core.types:rect-w clip))
                     do (write-cell-if-visible buf col row cell clip)))))
  nil)

(defun buffer-draw-border (buf rect &key style)
  (let* ((template (or style (make-default-cell)))
         (x0 (ptui.core.types:rect-x rect))
         (y0 (ptui.core.types:rect-y rect))
         (w (ptui.core.types:rect-w rect))
         (h (ptui.core.types:rect-h rect))
         (x1 (+ x0 (1- w)))
         (y1 (+ y0 (1- h)))
         (hline "─")
         (vline "│")
         (tl "╭")
         (tr "╮")
         (bl "╰")
         (br "╯"))
    (when (and (> w 0) (> h 0))
      (let ((clip (clip-for-buffer buf)))
        (loop for x from x0 to x1 do
          (write-cell-if-visible
           buf
           x
           y0
           (cell-with-glyph template
                            (cond
                              ((= x x0) tl)
                              ((= x x1) tr)
                              (t hline)))
           clip))
        (when (> h 1)
          (loop for x from x0 to x1 do
            (write-cell-if-visible
             buf
             x
             y1
             (cell-with-glyph template
                              (cond
                                ((= x x0) bl)
                                ((= x x1) br)
                                (t hline)))
             clip)))
        (when (> h 2)
          (loop for y from (1+ y0) below y1 do
            (write-cell-if-visible buf x0 y (cell-with-glyph template vline) clip)
            (when (> w 1)
              (write-cell-if-visible buf x1 y (cell-with-glyph template vline) clip)))))))
  nil)

(defmacro with-clip ((buf rect) &body body)
  `(let* ((target-buf ,buf)
          (target-rect ,rect)
          (parent-clip (if (and *clip-buffer* (eq *clip-buffer* target-buf) *clip-rect*)
                           *clip-rect*
                           (buffer-full-rect target-buf)))
          (effective-clip (or (rect-intersection parent-clip target-rect)
                              (ptui.core.types:make-rect 0 0 0 0))))
     (let ((*clip-buffer* target-buf)
           (*clip-rect* effective-clip))
       ,@body)))
