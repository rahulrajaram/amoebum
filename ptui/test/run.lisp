(defpackage :ptui.test.run
  (:use :cl))

(in-package :ptui.test.run)

(defvar *tests* '())

(defmacro deftest (name &body body)
  `(progn
     (push (cons ,(string name) (lambda () ,@body)) *tests*)
     ',name))

(defun %fail (fmt &rest args)
  (apply #'format *error-output* (concatenate 'string "~&FAIL: " fmt "~%") args)
  (finish-output *error-output*)
  nil)

(defun %ok (fmt &rest args)
  (apply #'format t (concatenate 'string "~&OK: " fmt "~%") args)
  (finish-output t)
  t)

(defun assert-true (cond fmt &rest args)
  (unless cond
    (apply #'error fmt args)))

(defun draw-op-kinds (ops)
  (mapcar #'ptui.render.diff::draw-op-kind ops))

(defun string-from-codepoints (&rest codepoints)
  (coerce (mapcar #'code-char codepoints) 'string))

(defun run-all-tests ()
  (let ((passed 0)
        (failed 0))
    (dolist (entry (nreverse *tests*))
      (destructuring-bind (name . fn) entry
        (handler-case
            (progn
              (funcall fn)
              (incf passed)
              (%ok "~A" name))
          (error (e)
            (incf failed)
            (%fail "~A => ~A" name e)))))
    (format t "~&TEST_SUMMARY passed=~D failed=~D~%" passed failed)
    (finish-output t)
    (values passed failed)))

;; ---- Tests ----

(deftest diff-minimality-single-cell
  (let* ((buf1 (ptui.render.buffer:make-buffer 5 1))
         (buf2 (ptui.render.buffer:make-buffer 5 1)))
    (ptui.render.buffer:buffer-draw-text buf2 0 0 "X")
    (multiple-value-bind (ops count)
        (ptui.render.diff:diff-buffers buf1 buf2 :full-redraw nil)
      (declare (ignore count))
      (assert-true (not (member :clear-screen (draw-op-kinds ops)))
                   "unexpected :clear-screen for 1-cell diff: ~S" (draw-op-kinds ops))
      (assert-true (= (length ops) 3) "expected 3 ops, got ~D: ~S" (length ops) (draw-op-kinds ops))
      (assert-true (equal (draw-op-kinds ops) '(:move :style :write))
                   "unexpected op sequence: ~S" (draw-op-kinds ops)))))

(deftest diff-shrink-emits-clear-eol
  (let* ((buf1 (ptui.render.buffer:make-buffer 10 1))
         (buf2 (ptui.render.buffer:make-buffer 10 1)))
    (ptui.render.buffer:buffer-draw-text buf1 0 0 "HELLO")
    (ptui.render.buffer:buffer-draw-text buf2 0 0 "HI")
    (multiple-value-bind (ops count)
        (ptui.render.diff:diff-buffers buf1 buf2 :full-redraw nil)
      (declare (ignore count))
      (assert-true (member :clear-eol (draw-op-kinds ops))
                   "expected :clear-eol in ops, got: ~S" (draw-op-kinds ops)))))

(deftest diff-resize-forces-clear-screen
  (let* ((buf1 (ptui.render.buffer:make-buffer 3 1))
         (buf2 (ptui.render.buffer:make-buffer 4 1)))
    (multiple-value-bind (ops count)
        (ptui.render.diff:diff-buffers buf1 buf2 :full-redraw nil)
      (declare (ignore count))
      (assert-true (eql (first (draw-op-kinds ops)) :clear-screen)
                   "expected leading :clear-screen, got: ~S" (draw-op-kinds ops)))))

(deftest input-parser-split-csi-arrow
  (let ((p (ptui.term.input:make-input-parser)))
    (ptui.term.input:input-feed p (make-array 2 :element-type '(unsigned-byte 8)
                                              :initial-contents (list #x1b #x5b)))
    (multiple-value-bind (events1 n1) (ptui.term.input:input-drain-events p)
      (declare (ignore n1))
      (assert-true (null events1) "expected no events for partial CSI, got ~S" events1))
    (ptui.term.input:input-feed p (make-array 1 :element-type '(unsigned-byte 8)
                                              :initial-contents (list #x41)))
    (multiple-value-bind (events2 n2) (ptui.term.input:input-drain-events p)
      (assert-true (= n2 1) "expected 1 event, got ~D / ~S" n2 events2)
      (assert-true (eql (ptui.core.events:key-event-key (first events2)) :up)
                   "expected :up, got ~S" (ptui.core.events:key-event-key (first events2))))))

(deftest color-policy-sgr-modes
  (let* ((caps-true (ptui.term.caps:probe-terminal-caps
                     :env (lambda (k)
                            (cond ((string= k "TERM") "xterm-256color")
                                  ((string= k "COLORTERM") "truecolor")
                                  (t nil)))))
         (caps-256 (ptui.term.caps:probe-terminal-caps
                    :env (lambda (k)
                           (cond ((string= k "TERM") "xterm-256color")
                                 ((string= k "COLORTERM") nil)
                                 (t nil)))))
         (rgb (ptui.core.color:make-color-rgb 1 2 3)))
    (assert-true (eql (ptui.core.color:resolve-color-mode caps-true) :truecolor)
                 "expected truecolor mode")
    (assert-true (search "38;2;" (ptui.core.color:color->sgr rgb :mode :truecolor :fg-or-bg :fg))
                 "expected truecolor sgr fragment")
    (assert-true (eql (ptui.core.color:resolve-color-mode caps-256) :x256)
                 "expected x256 mode")
    (assert-true (search "38;5;" (ptui.core.color:color->sgr rgb :mode :x256 :fg-or-bg :fg))
                 "expected x256 sgr fragment")))

(deftest text-ascii-baseline
  (assert-true (eql (ptui.text.grapheme:grapheme-engine) :fallback)
               "expected fallback grapheme engine")
  (assert-true (equal (ptui.text.grapheme:split-graphemes "abc") '("a" "b" "c"))
               "unexpected grapheme split for ascii")
  (assert-true (= (ptui.text.width:string-width "abc") 3)
               "unexpected ascii width")
  (assert-true (equal (ptui.text.layout:wrap-by-width "abcdef" 3) '("abc" "def"))
               "unexpected wrap result")
  (assert-true (string= (ptui.text.layout:truncate-to-width "abcdef" 4) "abcd")
               "unexpected truncate result"))

(deftest text-native-engine-currently-aliases-fallback
  (let ((text (string-from-codepoints #x0065 #x0301 #x1F468 #x200D #x1F469)))
    (assert-true
     (equal (ptui.text.grapheme:split-graphemes text :engine :native)
            (ptui.text.grapheme:split-graphemes text :engine :fallback))
     "native engine should currently alias fallback behavior")))

(deftest text-symbol-width-not-overclassified-as-emoji
  (let ((scissors (string-from-codepoints #x2702))
        (airplane (string-from-codepoints #x2708)))
    (assert-true (= (ptui.text.width:string-width scissors) 1)
                 "U+2702 should be width=1 in fallback policy")
    (assert-true (= (ptui.text.width:string-width airplane) 1)
                 "U+2708 should be width=1 in fallback policy")))

(deftest text-combining-characters
  (let ((text (string-from-codepoints #x0065 #x0301)))
    (assert-true (= (length (ptui.text.grapheme:split-graphemes text)) 1)
                 "combining cluster should remain one grapheme")
    (assert-true (= (ptui.text.width:string-width text) 1)
                 "combining sequence should be width=1")
    (assert-true (string= (ptui.text.layout:truncate-to-width text 1) text)
                 "combining sequence should survive width=1 truncate")))

(deftest text-emoji-zwj-sequence
  (let* ((family (string-from-codepoints #x1F468 #x200D #x1F469 #x200D #x1F467 #x200D #x1F466))
         (text (concatenate 'string "A" family "B")))
    (assert-true (= (length (ptui.text.grapheme:split-graphemes family)) 1)
                 "emoji ZWJ sequence should be one grapheme")
    (assert-true (= (ptui.text.width:string-width family) 2)
                 "emoji ZWJ sequence should be width=2")
    (assert-true (= (ptui.text.width:string-width text) 4)
                 "A + family + B should be width=4")
    (assert-true (string= (ptui.text.layout:truncate-to-width text 3 :ellipsis t) "A…")
                 "unexpected emoji truncation behavior")))

(deftest text-cjk-wide-characters
  (let ((text (string-from-codepoints #x0041 #x754C #x0042)))
    (assert-true (= (ptui.text.width:string-width text) 4)
                 "A界B should be width=4")
    (assert-true (equal (ptui.text.layout:wrap-by-width text 3)
                        (list (string-from-codepoints #x0041 #x754C)
                              "B"))
                 "unexpected CJK wrap result")
    (assert-true (string= (ptui.text.layout:width-safe-slice text 0 2) "A")
                 "slice should not split wide grapheme")))

;; Script entry
(multiple-value-bind (passed failed) (run-all-tests)
  (declare (ignore passed))
  (uiop:quit (if (zerop failed) 0 1)))
