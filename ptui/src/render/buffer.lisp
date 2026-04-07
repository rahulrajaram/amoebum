(defpackage :ptui.render.buffer
  (:use :cl)
  (:export #:make-buffer #:buffer-clear #:buffer-reset
           #:buffer-dimensions-match-p
           #:buffer-draw-text
           #:buffer-draw-styled-segments
           #:buffer-fill-rect #:buffer-draw-border
           #:buffer-subrect #:with-clip
           #:write-cell-if-visible))

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

(defun copy-cell-into (dst src)
  "Copy SRC cell data into DST cell in-place. Zero allocation."
  (setf (ptui.core.types:cell-glyph dst) (ptui.core.types:cell-glyph src)
        (ptui.core.types:cell-fg dst) (ptui.core.types:cell-fg src)
        (ptui.core.types:cell-bg dst) (ptui.core.types:cell-bg src))
  (let ((da (ptui.core.types:cell-attrs dst))
        (sa (ptui.core.types:cell-attrs src)))
    (setf (ptui.core.types:attrs-boldp da) (ptui.core.types:attrs-boldp sa)
          (ptui.core.types:attrs-italicp da) (ptui.core.types:attrs-italicp sa)
          (ptui.core.types:attrs-underlinep da) (ptui.core.types:attrs-underlinep sa)
          (ptui.core.types:attrs-invertp da) (ptui.core.types:attrs-invertp sa)
          (ptui.core.types:attrs-dimp da) (ptui.core.types:attrs-dimp sa)
          (ptui.core.types:attrs-strikep da) (ptui.core.types:attrs-strikep sa)))
  dst)

(defun write-cell-if-visible (buf x y cell clip)
  (when (point-in-rect-p x y clip)
    (let* ((idx (buffer-index buf x y))
           (existing (svref (ptui.core.types:cell-buffer-cells buf) idx)))
      (if existing
          (copy-cell-into existing cell)
          (setf (svref (ptui.core.types:cell-buffer-cells buf) idx)
                (clone-cell cell))))))

(defun make-buffer (cols rows &key (fill (make-default-cell)))
  (let* ((count (* cols rows))
         (cells (make-array count)))
    (loop for i from 0 below count do
      (setf (svref cells i) (clone-cell fill)))
    (ptui.core.types:make-cell-buffer cols rows cells)))

(defun buffer-clear (buf &key (fill (make-default-cell)))
  "Clear buffer by resetting existing cells in-place where possible."
  (let* ((cells (ptui.core.types:cell-buffer-cells buf))
         (count (length cells)))
    (loop for i from 0 below count do
      (let ((existing (svref cells i)))
        (if existing
            (reset-cell existing fill)
            (setf (svref cells i) (clone-cell fill))))))
  nil)

(defun reset-cell (cell &optional template)
  "Reset CELL in-place from TEMPLATE, defaulting to the standard blank cell."
  (copy-cell-into cell (or template (make-default-cell))))

(defun buffer-reset (buf)
  "Reset all cells in BUF to defaults in-place. Zero allocation."
  (let* ((cells (ptui.core.types:cell-buffer-cells buf))
         (default-cell (make-default-cell))
         (count (length cells)))
    (loop for i from 0 below count do
      (let ((existing (svref cells i)))
        (if existing
            (reset-cell existing default-cell)
            (setf (svref cells i) (clone-cell default-cell))))))
  nil)

(defun buffer-dimensions-match-p (buf cols rows)
  "Return T if BUF has the given dimensions."
  (and buf
       (= (ptui.core.types:cell-buffer-cols buf) cols)
       (= (ptui.core.types:cell-buffer-rows buf) rows)))

(defun styled-segment->cells (segment)
  (labels ((continuation-cell (template)
             (cell-with-glyph template ""))
           (expand-cell-to-width (cell)
             (let* ((glyph (ptui.core.types:cell-glyph cell))
                    (width (ptui.text.width:string-width glyph)))
               (if (> width 1)
                   (append (list (clone-cell cell))
                           (loop repeat (1- width)
                                 collect (continuation-cell cell)))
                   (list (clone-cell cell)))))
           (string->cells (text template)
             ;; P1 FIX: Use cons + nreverse instead of nconc for O(1) per iteration
             ;; instead of O(n) per iteration
             (let ((cells '()))
               (flet ((append-cell (cell)
                        ;; Was: (setf cells (nconc cells (list cell))) - O(n) each!
                        ;; Now: O(1) push, reverse once at end
                        (push cell cells)))
                 (dolist (cluster (ptui.text.grapheme:split-graphemes text))
                   (let ((width (ptui.text.width:grapheme-width cluster)))
                     (cond
                       ;; Zero-width clusters attach to the previous base cluster.
                       ((<= width 0)
                        (when cells
                          (let ((last (car cells)))
                            (setf (ptui.core.types:cell-glyph last)
                                  (concatenate 'string
                                               (ptui.core.types:cell-glyph last)
                                               cluster)))))
                       (t
                        (append-cell (cell-with-glyph template cluster))
                        (loop repeat (1- width) do
                          (append-cell (continuation-cell template)))))))
               ;; Reverse once at end - O(n) total instead of O(n²)
               (nreverse cells)))))
    (cond
      ((stringp segment)
       (string->cells segment (make-default-cell)))
      ((typep segment 'ptui.core.types:cell)
       (expand-cell-to-width segment))
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

(defun %draw-segment-direct (buf x y clip limit dx-start segment)
  "Write a single styled-text segment directly into BUF. Returns new dx offset."
  (let ((dx dx-start))
    (labels ((emit-glyph (glyph template)
               (when (< dx limit)
                 (let ((ex (svref (ptui.core.types:cell-buffer-cells buf)
                                  (buffer-index buf (+ x dx) y))))
                   (when (and ex (point-in-rect-p (+ x dx) y clip))
                     (setf (ptui.core.types:cell-glyph ex) glyph
                           (ptui.core.types:cell-fg ex) (ptui.core.types:cell-fg template)
                           (ptui.core.types:cell-bg ex) (ptui.core.types:cell-bg template))
                     (let ((da (ptui.core.types:cell-attrs ex))
                           (sa (ptui.core.types:cell-attrs template)))
                       (setf (ptui.core.types:attrs-boldp da) (ptui.core.types:attrs-boldp sa)
                             (ptui.core.types:attrs-italicp da) (ptui.core.types:attrs-italicp sa)
                             (ptui.core.types:attrs-underlinep da) (ptui.core.types:attrs-underlinep sa)
                             (ptui.core.types:attrs-invertp da) (ptui.core.types:attrs-invertp sa)
                             (ptui.core.types:attrs-dimp da) (ptui.core.types:attrs-dimp sa)
                             (ptui.core.types:attrs-strikep da) (ptui.core.types:attrs-strikep sa)))))
                 (incf dx)))
             (emit-continuation (template)
               (when (< dx limit)
                 (let ((ex (svref (ptui.core.types:cell-buffer-cells buf)
                                  (buffer-index buf (+ x dx) y))))
                   (when (and ex (point-in-rect-p (+ x dx) y clip))
                     (setf (ptui.core.types:cell-glyph ex) ""
                           (ptui.core.types:cell-fg ex) (ptui.core.types:cell-fg template)
                           (ptui.core.types:cell-bg ex) (ptui.core.types:cell-bg template))
                     (let ((da (ptui.core.types:cell-attrs ex))
                           (sa (ptui.core.types:cell-attrs template)))
                       (setf (ptui.core.types:attrs-boldp da) (ptui.core.types:attrs-boldp sa)
                             (ptui.core.types:attrs-italicp da) (ptui.core.types:attrs-italicp sa)
                             (ptui.core.types:attrs-underlinep da) (ptui.core.types:attrs-underlinep sa)
                             (ptui.core.types:attrs-invertp da) (ptui.core.types:attrs-invertp sa)
                             (ptui.core.types:attrs-dimp da) (ptui.core.types:attrs-dimp sa)
                             (ptui.core.types:attrs-strikep da) (ptui.core.types:attrs-strikep sa)))))
                 (incf dx)))
             (draw-string (text template)
               (dolist (cluster (ptui.text.grapheme:split-graphemes text))
                 (let ((width (ptui.text.width:grapheme-width cluster)))
                   (unless (<= width 0)
                     (emit-glyph cluster template)
                     (loop repeat (1- width) do
                       (emit-continuation template)))))))
      (cond
        ((stringp segment)
         (draw-string segment (make-default-cell)))
        ((typep segment 'ptui.core.types:cell)
         (let ((width (ptui.text.width:string-width
                       (ptui.core.types:cell-glyph segment))))
           (emit-glyph (ptui.core.types:cell-glyph segment) segment)
           (loop repeat (max 0 (1- width)) do
             (emit-continuation segment))))
        ((and (consp segment)
              (stringp (first segment))
              (typep (second segment) 'ptui.core.types:cell))
         (draw-string (first segment) (second segment)))))
    dx))

(defun buffer-draw-text (buf x y styled-text &key (max-width nil))
  (let* ((clip (clip-for-buffer buf))
         (cells (flatten-styled-text styled-text))
         (limit (if max-width (max 0 max-width) most-positive-fixnum)))
    (loop for cell in cells
          for dx from 0
          while (< dx limit)
          do (write-cell-if-visible buf (+ x dx) y cell clip)))
  nil)

(defun buffer-draw-styled-segments (buf x y segments style-resolver &key (max-width nil))
  "Write compact (text . style-id) segments directly into buffer cells.
STYLE-RESOLVER is a function: style-id → ptui cell (used as template).
Zero intermediate cell/attrs allocation — mutates pre-existing buffer cells."
  (let* ((clip (clip-for-buffer buf))
         (limit (if max-width (max 0 max-width) most-positive-fixnum))
         (dx 0)
         (cells-array (ptui.core.types:cell-buffer-cells buf))
         (buf-cols (ptui.core.types:cell-buffer-cols buf)))
    (declare (type fixnum dx limit buf-cols))
    (dolist (segment segments)
      (when (>= dx limit) (return))
      (let* ((text (if (and (consp segment) (stringp (car segment)))
                       (car segment)
                       (if (stringp segment) segment "")))
             (template (cond
                         ;; Compact segment: (text . fixnum-style-id)
                         ((and (consp segment)
                               (stringp (car segment))
                               (typep (cdr segment) 'fixnum))
                          (funcall style-resolver (cdr segment)))
                         ;; Already resolved: (text cell)
                         ((and (consp segment)
                               (stringp (first segment))
                               (typep (second segment) 'ptui.core.types:cell))
                          (second segment))
                         ;; Cell directly
                         ((typep segment 'ptui.core.types:cell)
                          (setf text (ptui.core.types:cell-glyph segment))
                          segment)
                         ;; Plain string
                         ((stringp segment)
                          (make-default-cell))
                         (t (make-default-cell)))))
        (dolist (cluster (ptui.text.grapheme:split-graphemes text))
          (when (>= dx limit) (return))
          (let ((width (ptui.text.width:grapheme-width cluster)))
            (when (> width 0)
              (let ((abs-x (+ x dx)))
                (when (point-in-rect-p abs-x y clip)
                  (let* ((idx (+ abs-x (* y buf-cols)))
                         (ex (svref cells-array idx)))
                    (when ex
                      (setf (ptui.core.types:cell-glyph ex) cluster
                            (ptui.core.types:cell-fg ex) (ptui.core.types:cell-fg template)
                            (ptui.core.types:cell-bg ex) (ptui.core.types:cell-bg template))
                      (let ((da (ptui.core.types:cell-attrs ex))
                            (sa (ptui.core.types:cell-attrs template)))
                        (setf (ptui.core.types:attrs-boldp da) (ptui.core.types:attrs-boldp sa)
                              (ptui.core.types:attrs-italicp da) (ptui.core.types:attrs-italicp sa)
                              (ptui.core.types:attrs-underlinep da) (ptui.core.types:attrs-underlinep sa)
                              (ptui.core.types:attrs-invertp da) (ptui.core.types:attrs-invertp sa)
                              (ptui.core.types:attrs-dimp da) (ptui.core.types:attrs-dimp sa)
                              (ptui.core.types:attrs-strikep da) (ptui.core.types:attrs-strikep sa)))))))
              (incf dx)
              ;; Continuation cells for wide characters
              (loop repeat (1- width) do
                (when (>= dx limit) (return))
                (let ((abs-x (+ x dx)))
                  (when (point-in-rect-p abs-x y clip)
                    (let* ((idx (+ abs-x (* y buf-cols)))
                           (ex (svref cells-array idx)))
                      (when ex
                        (setf (ptui.core.types:cell-glyph ex) ""
                              (ptui.core.types:cell-fg ex) (ptui.core.types:cell-fg template)
                              (ptui.core.types:cell-bg ex) (ptui.core.types:cell-bg template))
                        (let ((da (ptui.core.types:cell-attrs ex))
                              (sa (ptui.core.types:cell-attrs template)))
                          (setf (ptui.core.types:attrs-boldp da) (ptui.core.types:attrs-boldp sa)
                                (ptui.core.types:attrs-italicp da) (ptui.core.types:attrs-italicp sa)
                                (ptui.core.types:attrs-underlinep da) (ptui.core.types:attrs-underlinep sa)
                                (ptui.core.types:attrs-invertp da) (ptui.core.types:attrs-invertp sa)
                                (ptui.core.types:attrs-dimp da) (ptui.core.types:attrs-dimp sa)
                                (ptui.core.types:attrs-strikep da) (ptui.core.types:attrs-strikep sa)))))))
                (incf dx)))))))
    nil))

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

(defun buffer-draw-border (buf rect &key style (border-style :rounded))
  (let* ((template (or style (make-default-cell)))
         (x0 (ptui.core.types:rect-x rect))
         (y0 (ptui.core.types:rect-y rect))
         (w (ptui.core.types:rect-w rect))
         (h (ptui.core.types:rect-h rect))
         (x1 (+ x0 (1- w)))
         (y1 (+ y0 (1- h)))
         (hline "─")
         (vline "│")
         (tl (case border-style
               (:square "┌")
               (otherwise "╭")))
         (tr (case border-style
               (:square "┐")
               (otherwise "╮")))
         (bl (case border-style
               (:square "└")
               (otherwise "╰")))
         (br (case border-style
               (:square "┘")
               (otherwise "╯"))))
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
