(defpackage :ptui.components.ansi-parser
  (:use :cl)
  (:export
   #:ansi-style
   #:make-ansi-style
   #:ansi-style-fg
   #:ansi-style-bg
   #:ansi-style-boldp
   #:ansi-style-italicp
   #:ansi-style-underlinep
   #:ansi-style-invertp
   #:ansi-style-dimp
   #:ansi-style-strikep
   #:clone-style
   #:style->attrs
   #:style->cell
   #:cell-style=
   #:apply-sgr!
   #:parse-sgr-codes
   #:ansi-index->color
   #:consume-ansi-output
   #:+esc-char+
   #:+ansi-palette+))

(in-package :ptui.components.ansi-parser)

(defconstant +esc-char+ (code-char 27))

(defparameter +ansi-palette+
  ;; ANSI 16-color table (regular then bright).
  #(#(0 0 0) #(205 49 49) #(13 188 121) #(229 229 16)
    #(36 114 200) #(188 63 188) #(17 168 205) #(229 229 229)
    #(102 102 102) #(241 76 76) #(35 209 139) #(245 245 67)
    #(59 142 234) #(214 112 214) #(41 184 219) #(255 255 255)))

(defstruct (ansi-style
            (:constructor make-ansi-style
                (&key
                  (fg ptui.core.color:color-default)
                  (bg ptui.core.color:color-default)
                  (boldp nil)
                  (italicp nil)
                  (underlinep nil)
                  (invertp nil)
                  (dimp nil)
                  (strikep nil))))
  (fg ptui.core.color:color-default)
  (bg ptui.core.color:color-default)
  (boldp nil :type boolean)
  (italicp nil :type boolean)
  (underlinep nil :type boolean)
  (invertp nil :type boolean)
  (dimp nil :type boolean)
  (strikep nil :type boolean))

(defun clone-style (style)
  (make-ansi-style
   :fg (ansi-style-fg style)
   :bg (ansi-style-bg style)
   :boldp (ansi-style-boldp style)
   :italicp (ansi-style-italicp style)
   :underlinep (ansi-style-underlinep style)
   :invertp (ansi-style-invertp style)
   :dimp (ansi-style-dimp style)
   :strikep (ansi-style-strikep style)))

(defun style->attrs (style)
  (ptui.core.types:make-attrs
   :boldp (ansi-style-boldp style)
   :italicp (ansi-style-italicp style)
   :underlinep (ansi-style-underlinep style)
   :invertp (ansi-style-invertp style)
   :dimp (ansi-style-dimp style)
   :strikep (ansi-style-strikep style)))

(defun style->cell (style)
  (ptui.core.types:make-cell
   " "
   (ansi-style-fg style)
   (ansi-style-bg style)
   (style->attrs style)))

(defun cell-style= (left right)
  (and (equalp (ptui.core.types:cell-fg left)
               (ptui.core.types:cell-fg right))
       (equalp (ptui.core.types:cell-bg left)
               (ptui.core.types:cell-bg right))
       (equalp (ptui.core.types:attrs-boldp (ptui.core.types:cell-attrs left))
               (ptui.core.types:attrs-boldp (ptui.core.types:cell-attrs right)))
       (equalp (ptui.core.types:attrs-italicp (ptui.core.types:cell-attrs left))
               (ptui.core.types:attrs-italicp (ptui.core.types:cell-attrs right)))
       (equalp (ptui.core.types:attrs-underlinep (ptui.core.types:cell-attrs left))
               (ptui.core.types:attrs-underlinep (ptui.core.types:cell-attrs right)))
       (equalp (ptui.core.types:attrs-invertp (ptui.core.types:cell-attrs left))
               (ptui.core.types:attrs-invertp (ptui.core.types:cell-attrs right)))
       (equalp (ptui.core.types:attrs-dimp (ptui.core.types:cell-attrs left))
               (ptui.core.types:attrs-dimp (ptui.core.types:cell-attrs right)))
       (equalp (ptui.core.types:attrs-strikep (ptui.core.types:cell-attrs left))
               (ptui.core.types:attrs-strikep (ptui.core.types:cell-attrs right)))))

(defun ansi-index->color (index)
  (cond
    ((and (integerp index) (<= 0 index 15))
     (let ((triple (aref +ansi-palette+ index)))
       (ptui.core.color:make-color-rgb
        (aref triple 0) (aref triple 1) (aref triple 2))))
    ((and (integerp index) (<= 16 index 231))
     (let* ((cube (- index 16))
            (r (floor cube 36))
            (g (floor (mod cube 36) 6))
            (b (mod cube 6))
            (levels #(0 95 135 175 215 255)))
       (ptui.core.color:make-color-rgb
        (aref levels r)
        (aref levels g)
        (aref levels b))))
    ((and (integerp index) (<= 232 index 255))
     (let ((level (+ 8 (* 10 (- index 232)))))
       (ptui.core.color:make-color-rgb level level level)))
    (t
     ptui.core.color:color-default)))

(defun %string-split (text delimiter)
  (let ((result '())
        (start 0))
    (loop for index from 0 below (length text) do
      (when (char= (char text index) delimiter)
        (push (subseq text start index) result)
        (setf start (1+ index))))
    (push (subseq text start) result)
    (nreverse result)))

(defun parse-sgr-codes (params)
  (if (zerop (length params))
      '(0)
      (loop for token in (%string-split params #\;)
            collect (if (zerop (length token))
                        0
                        (or (parse-integer token :junk-allowed t)
                            0)))))

(defun apply-sgr! (style params)
  (let ((codes (parse-sgr-codes params)))
    (loop while codes do
      (let ((code (pop codes)))
        (cond
          ((= code 0)
           (setf (ansi-style-fg style) ptui.core.color:color-default
                 (ansi-style-bg style) ptui.core.color:color-default
                 (ansi-style-boldp style) nil
                 (ansi-style-italicp style) nil
                 (ansi-style-underlinep style) nil
                 (ansi-style-invertp style) nil
                 (ansi-style-dimp style) nil
                 (ansi-style-strikep style) nil))
          ((= code 1)
           (setf (ansi-style-boldp style) t))
          ((= code 2)
           (setf (ansi-style-dimp style) t))
          ((= code 3)
           (setf (ansi-style-italicp style) t))
          ((= code 4)
           (setf (ansi-style-underlinep style) t))
          ((= code 7)
           (setf (ansi-style-invertp style) t))
          ((= code 9)
           (setf (ansi-style-strikep style) t))
          ((= code 22)
           (setf (ansi-style-boldp style) nil
                 (ansi-style-dimp style) nil))
          ((= code 23)
           (setf (ansi-style-italicp style) nil))
          ((= code 24)
           (setf (ansi-style-underlinep style) nil))
          ((= code 27)
           (setf (ansi-style-invertp style) nil))
          ((= code 29)
           (setf (ansi-style-strikep style) nil))
          ((<= 30 code 37)
           (setf (ansi-style-fg style) (ansi-index->color (- code 30))))
          ((= code 39)
           (setf (ansi-style-fg style) ptui.core.color:color-default))
          ((<= 40 code 47)
           (setf (ansi-style-bg style) (ansi-index->color (- code 40))))
          ((= code 49)
           (setf (ansi-style-bg style) ptui.core.color:color-default))
          ((<= 90 code 97)
           (setf (ansi-style-fg style) (ansi-index->color (+ 8 (- code 90)))))
          ((<= 100 code 107)
           (setf (ansi-style-bg style) (ansi-index->color (+ 8 (- code 100)))))
          ((or (= code 38) (= code 48))
           (let ((fgp (= code 38)))
             (when codes
               (let ((mode (pop codes)))
                 (cond
                   ((and (= mode 5) codes)
                    (let ((value (pop codes)))
                      (if fgp
                          (setf (ansi-style-fg style) (ansi-index->color value))
                          (setf (ansi-style-bg style) (ansi-index->color value)))))
                   ((and (= mode 2) (>= (length codes) 3))
                    (let ((r (pop codes))
                          (g (pop codes))
                          (b (pop codes)))
                      (when (and (integerp r) (integerp g) (integerp b)
                                 (<= 0 r 255) (<= 0 g 255) (<= 0 b 255))
                        (if fgp
                            (setf (ansi-style-fg style)
                                  (ptui.core.color:make-color-rgb r g b))
                            (setf (ansi-style-bg style)
                                  (ptui.core.color:make-color-rgb r g b))))))))))))))))

(defun consume-ansi-output (output style pending-escape
                            &key on-text on-newline on-incomplete-escape)
  "Parse ANSI output, calling callbacks for text runs, newlines, and incomplete escapes.
Returns (values updated-style updated-pending-escape)."
  (let* ((combined (if (zerop (length pending-escape))
                       output
                       (concatenate 'string pending-escape output)))
         (length (length combined))
         (index 0)
         (new-pending-escape "")
         (run-text ""))
    (labels ((flush-run ()
               (when (> (length run-text) 0)
                 (when on-text
                   (funcall on-text run-text style))
                 (setf run-text ""))))
      (loop while (< index length) do
        (let ((ch (char combined index)))
          (cond
            ((char= ch +esc-char+)
             (if (and (< (1+ index) length)
                      (char= (char combined (1+ index)) #\[))
                 (let ((end (loop for scan from (+ index 2) below length
                                  for c = (char combined scan)
                                  when (and (char<= #\@ c) (char<= c #\~))
                                    do (return scan))))
                   (if end
                       (progn
                         (flush-run)
                         (when (char= (char combined end) #\m)
                           (apply-sgr! style
                                       (subseq combined (+ index 2) end)))
                         (setf index (1+ end)))
                       (progn
                         (flush-run)
                         (setf new-pending-escape (subseq combined index))
                         (setf index length))))
                 (progn
                   (flush-run)
                   (if (= (1+ index) length)
                       (setf new-pending-escape (string +esc-char+))
                       (when on-text
                         (funcall on-text (string ch) style)))
                   (incf index))))
            ((char= ch #\Newline)
             (flush-run)
             (when on-newline
               (funcall on-newline))
             (incf index))
            ((char= ch #\Return)
             (incf index))
            (t
             (setf run-text (concatenate 'string run-text (string ch)))
             (incf index)))))
      (flush-run))
    (when (and on-incomplete-escape
               (> (length new-pending-escape) 0))
      (funcall on-incomplete-escape new-pending-escape))
    (values style new-pending-escape)))
