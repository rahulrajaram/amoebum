(defpackage :ptui.core.color
  (:use :cl)
  (:export
   #:color #:color-rgb #:make-color-rgb
   #:color-default
   #:resolve-color-mode
   #:color->sgr))

(in-package :ptui.core.color)

(defconstant color-default :default)

(defstruct (color-rgb (:constructor %make-color-rgb (r g b)))
  (r 0 :type (integer 0 255))
  (g 0 :type (integer 0 255))
  (b 0 :type (integer 0 255)))

(deftype color ()
  '(or (eql :default) color-rgb))

(defun make-color-rgb (r g b)
  (labels ((channel (value name)
             (unless (and (integerp value) (<= 0 value 255))
               (error "~A must be an integer in [0,255], got: ~S" name value))
             value))
    (%make-color-rgb (channel r 'r)
                     (channel g 'g)
                     (channel b 'b))))

(defun resolve-color-mode (caps)
  (cond
    ((ptui.term.caps:terminal-caps-truecolorp caps) :truecolor)
    ((ptui.term.caps:terminal-caps-256colorp caps) :x256)
    (t :x16)))

(defun %x256-from-rgb (r g b)
  (+ 16
     (* 36 (round (* (max 0 (min 255 r)) 5) 255))
     (* 6 (round (* (max 0 (min 255 g)) 5) 255))
     (round (* (max 0 (min 255 b)) 5) 255)))

(defun %x16-from-rgb (r g b)
  ;; Project RGB onto ANSI 16-color cube with a simple luminance split.
  (let* ((brightp (>= (+ (* 30 r) (* 59 g) (* 11 b)) 12800))
         (base (+ (if (>= r 128) 1 0)
                  (if (>= g 128) 2 0)
                  (if (>= b 128) 4 0))))
    (if brightp
        (+ 8 base)
      base)))

(defun %normalize-color (value)
  (cond
    ((or (null value) (eq value :default)) color-default)
    ((typep value 'color-rgb) value)
    ((and (listp value)
          (= (length value) 3)
          (every #'integerp value))
     (make-color-rgb (first value) (second value) (third value)))
    ((and (vectorp value)
          (= (length value) 3)
          (every #'integerp value))
     (make-color-rgb (aref value 0) (aref value 1) (aref value 2)))
    (t color-default)))

(defun color->sgr (value &key (mode :x16) (fg-or-bg :fg))
  (let ((color (%normalize-color value))
        (fg-p (eq fg-or-bg :fg)))
    (if (eq color color-default)
        (if fg-p "39" "49")
      (let ((r (color-rgb-r color))
            (g (color-rgb-g color))
            (b (color-rgb-b color)))
        (ecase mode
          (:truecolor
           (format nil "~D;2;~D;~D;~D" (if fg-p 38 48) r g b))
          (:x256
           (format nil "~D;5;~D" (if fg-p 38 48) (%x256-from-rgb r g b)))
          (:x16
           (let* ((idx (%x16-from-rgb r g b))
                  (code (if fg-p
                            (if (< idx 8) (+ 30 idx) (+ 82 idx))
                          (if (< idx 8) (+ 40 idx) (+ 92 idx)))))
             (princ-to-string code))))))))
