(defpackage :ptui.test.ansi-parser
  (:use :cl :fiveam)
  (:export #:ansi-parser-suite))

(in-package :ptui.test.ansi-parser)

(def-suite ansi-parser-suite
  :description "PTUI ANSI parser module tests.")

(in-suite ansi-parser-suite)

;;; --- parse-sgr-codes ---

(test parse-sgr-codes-empty-returns-reset
  (is (equal '(0) (ptui.components.ansi-parser:parse-sgr-codes ""))))

(test parse-sgr-codes-single
  (is (equal '(1) (ptui.components.ansi-parser:parse-sgr-codes "1"))))

(test parse-sgr-codes-multiple
  (is (equal '(38 5 196) (ptui.components.ansi-parser:parse-sgr-codes "38;5;196"))))

(test parse-sgr-codes-junk-defaults-to-zero
  (is (equal '(0) (ptui.components.ansi-parser:parse-sgr-codes "abc"))))

;;; --- apply-sgr! ---

(test apply-sgr-reset
  (let ((style (ptui.components.ansi-parser:make-ansi-style :boldp t :fg (ptui.core.color:make-color-rgb 255 0 0))))
    (ptui.components.ansi-parser:apply-sgr! style "0")
    (is (not (ptui.components.ansi-parser:ansi-style-boldp style)))
    (is (equalp ptui.core.color:color-default (ptui.components.ansi-parser:ansi-style-fg style)))))

(test apply-sgr-bold-on-off
  (let ((style (ptui.components.ansi-parser:make-ansi-style)))
    (ptui.components.ansi-parser:apply-sgr! style "1")
    (is (ptui.components.ansi-parser:ansi-style-boldp style))
    (ptui.components.ansi-parser:apply-sgr! style "22")
    (is (not (ptui.components.ansi-parser:ansi-style-boldp style)))))

(test apply-sgr-fg-30-37
  (let ((style (ptui.components.ansi-parser:make-ansi-style)))
    (ptui.components.ansi-parser:apply-sgr! style "31")
    (is (not (equalp ptui.core.color:color-default
                     (ptui.components.ansi-parser:ansi-style-fg style))))))

(test apply-sgr-bg-40-47
  (let ((style (ptui.components.ansi-parser:make-ansi-style)))
    (ptui.components.ansi-parser:apply-sgr! style "42")
    (is (not (equalp ptui.core.color:color-default
                     (ptui.components.ansi-parser:ansi-style-bg style))))))

(test apply-sgr-bright-fg-90-97
  (let ((style (ptui.components.ansi-parser:make-ansi-style)))
    (ptui.components.ansi-parser:apply-sgr! style "91")
    (is (not (equalp ptui.core.color:color-default
                     (ptui.components.ansi-parser:ansi-style-fg style))))))

(test apply-sgr-256-color
  (let ((style (ptui.components.ansi-parser:make-ansi-style)))
    (ptui.components.ansi-parser:apply-sgr! style "38;5;196")
    (is (not (equalp ptui.core.color:color-default
                     (ptui.components.ansi-parser:ansi-style-fg style))))))

(test apply-sgr-truecolor
  (let ((style (ptui.components.ansi-parser:make-ansi-style)))
    (ptui.components.ansi-parser:apply-sgr! style "38;2;100;150;200")
    (is (not (equalp ptui.core.color:color-default
                     (ptui.components.ansi-parser:ansi-style-fg style))))))

;;; --- ansi-index->color ---

(test ansi-index-standard-16
  (let ((c (ptui.components.ansi-parser:ansi-index->color 0)))
    (is (not (equalp ptui.core.color:color-default c)))))

(test ansi-index-cube-16-231
  (let ((c (ptui.components.ansi-parser:ansi-index->color 196)))
    (is (not (equalp ptui.core.color:color-default c)))))

(test ansi-index-grayscale-232-255
  (let ((c (ptui.components.ansi-parser:ansi-index->color 240)))
    (is (not (equalp ptui.core.color:color-default c)))))

(test ansi-index-out-of-range-returns-default
  (is (equalp ptui.core.color:color-default
              (ptui.components.ansi-parser:ansi-index->color 256))))

;;; --- consume-ansi-output ---

(test consume-plain-text
  (let ((collected-text ""))
    (ptui.components.ansi-parser:consume-ansi-output
     "hello world"
     (ptui.components.ansi-parser:make-ansi-style)
     ""
     :on-text (lambda (text style)
                (declare (ignore style))
                (setf collected-text (concatenate 'string collected-text text))))
    (is (string= "hello world" collected-text))))

(test consume-newlines
  (let ((newline-count 0)
        (texts '()))
    (ptui.components.ansi-parser:consume-ansi-output
     (format nil "line1~%line2~%line3")
     (ptui.components.ansi-parser:make-ansi-style)
     ""
     :on-text (lambda (text style)
                (declare (ignore style))
                (push text texts))
     :on-newline (lambda () (incf newline-count)))
    (is (= 2 newline-count))
    (is (= 3 (length texts)))))

(test consume-sgr-sequence
  (let ((bold-seen nil))
    (ptui.components.ansi-parser:consume-ansi-output
     (format nil "~C[1mhello" (code-char 27))
     (ptui.components.ansi-parser:make-ansi-style)
     ""
     :on-text (lambda (text style)
                (declare (ignore text))
                (when (ptui.components.ansi-parser:ansi-style-boldp style)
                  (setf bold-seen t))))
    (is (not (null bold-seen)))))

(test consume-incomplete-escape-at-boundary
  (multiple-value-bind (style pending)
      (ptui.components.ansi-parser:consume-ansi-output
       (format nil "hello~C[3" (code-char 27))
       (ptui.components.ansi-parser:make-ansi-style)
       "")
    (declare (ignore style))
    (is (> (length pending) 0))))

(test consume-cr-stripped
  (let ((collected ""))
    (ptui.components.ansi-parser:consume-ansi-output
     (format nil "hello~Cworld" #\Return)
     (ptui.components.ansi-parser:make-ansi-style)
     ""
     :on-text (lambda (text style)
                (declare (ignore style))
                (setf collected (concatenate 'string collected text))))
    (is (string= "helloworld" collected))))
