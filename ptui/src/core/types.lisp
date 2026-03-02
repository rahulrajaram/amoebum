(defpackage :ptui.core.types
  (:use :cl)
  (:export
   ;; geometry
   #:size #:make-size #:size-cols #:size-rows #:size-width #:size-height
   #:rect #:make-rect #:rect-x #:rect-y #:rect-w #:rect-h
   #:insets #:make-insets #:insets-top #:insets-right #:insets-bottom #:insets-left

   ;; style
   #:attrs #:make-attrs #:attrs-boldp #:attrs-italicp #:attrs-underlinep
   #:attrs-invertp #:attrs-dimp #:attrs-strikep

   ;; cell + buffer
   #:cell #:make-cell #:cell-glyph #:cell-fg #:cell-bg #:cell-attrs
   #:cell-buffer #:make-cell-buffer #:cell-buffer-cols #:cell-buffer-rows
   #:cell-buffer-cells))

(in-package :ptui.core.types)

(deftype index ()
  '(integer 0 #.most-positive-fixnum))

(defun ensure-cell-glyph (glyph)
  "Cell glyph may be a grapheme cluster string.
An empty string is reserved for width-continuation cells."
  (unless (stringp glyph)
    (error "GLYPH must be a string, got: ~S" glyph))
  glyph)

(defstruct (size (:constructor make-size (cols rows)))
  (cols 0 :type index)
  (rows 0 :type index))

(defun size-width (size)
  "Return the width component of SIZE.
This is an alias for `size-cols` to offer consumer-facing symmetry."
  (size-cols size))

(defun size-height (size)
  "Return the height component of SIZE.
This is an alias for `size-rows` to offer consumer-facing symmetry."
  (size-rows size))

(defstruct (rect (:constructor make-rect (x y w h)))
  (x 0 :type fixnum)
  (y 0 :type fixnum)
  (w 0 :type index)
  (h 0 :type index))

(defstruct (insets (:constructor make-insets (top right bottom left)))
  (top 0 :type index)
  (right 0 :type index)
  (bottom 0 :type index)
  (left 0 :type index))

(defstruct (attrs (:constructor make-attrs
                                (&key (boldp nil)
                                      (italicp nil)
                                      (underlinep nil)
                                      (invertp nil)
                                      (dimp nil)
                                      (strikep nil))))
  (boldp nil :type boolean)
  (italicp nil :type boolean)
  (underlinep nil :type boolean)
  (invertp nil :type boolean)
  (dimp nil :type boolean)
  (strikep nil :type boolean))

(defstruct (cell (:constructor %make-cell (glyph fg bg attrs)))
  (glyph " " :type string)
  (fg nil)
  (bg nil)
  (attrs (make-attrs) :type attrs))

(defun normalize-cell-color (value)
  (cond
    ((or (null value) (eq value :default))
     ptui.core.color:color-default)
    ((typep value 'ptui.core.color:color-rgb)
     value)
    ((and (listp value)
          (= (length value) 3)
          (every #'integerp value))
     (ptui.core.color:make-color-rgb
      (first value) (second value) (third value)))
    ((and (vectorp value)
          (= (length value) 3)
          (every #'integerp value))
     (ptui.core.color:make-color-rgb
      (aref value 0) (aref value 1) (aref value 2)))
    (t
     (error "Unsupported color value: ~S" value))))

(defun make-cell (glyph fg bg attrs)
  (%make-cell (ensure-cell-glyph glyph)
              (normalize-cell-color fg)
              (normalize-cell-color bg)
              attrs))

(defstruct (cell-buffer (:constructor %make-cell-buffer (&key cols rows cells)))
  (cols 0 :type index)
  (rows 0 :type index)
  ;; Contract: simple vector length = rows*cols.
  (cells #() :type simple-vector))

(defun make-cell-buffer (cols rows &optional cells)
  (let* ((length (* cols rows))
         (cells* (or cells (make-array length :initial-element nil))))
    (unless (= (length cells*) length)
      (error "CELL-BUFFER cells length mismatch: expected ~D, got ~D"
             length
             (length cells*)))
    (%make-cell-buffer :cols cols :rows rows :cells cells*)))
