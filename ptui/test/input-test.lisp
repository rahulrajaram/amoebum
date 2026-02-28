(defpackage :ptui.test.input
  (:use :cl :fiveam)
  (:export #:ptui-input-suite #:run-all))

(in-package :ptui.test.input)

(def-suite ptui-input-suite
  :description "PTUI terminal input parser and terminal capability tests.")

(in-suite ptui-input-suite)

(defun byte-array (bytes)
  (coerce bytes '(simple-array (unsigned-byte 8) (*))))

(defun key-events-for (bytes)
  (let ((parser (ptui.term.input:make-input-parser)))
    (ptui.term.input:input-feed parser (byte-array bytes))
    (multiple-value-list (ptui.term.input:input-drain-events parser))))

(defun assert-single-key-event (bytes expected-key &key expected-text expected-ctrlp)
  (destructuring-bind (events count) (key-events-for bytes)
    (is (= count 1))
    (let ((event (first events)))
      (is (eq (ptui.core.events:key-event-key event) expected-key))
      (if expected-ctrlp
          (is (ptui.core.events:key-event-ctrlp event))
          (is (not (ptui.core.events:key-event-ctrlp event))))
      (when expected-text
        (is (string= expected-text
                     (ptui.core.events:key-event-text? event)))))))

(test ptui-input-single-ascii
  (assert-single-key-event '(97) :text :expected-text "a"))

(test ptui-input-arrow-keys
  (dolist (case '((:up 27 91 65)
                  (:down 27 91 66)
                  (:right 27 91 67)
                  (:left 27 91 68)
                  (:home 27 91 72)
                  (:end 27 91 70)))
    (assert-single-key-event (cdr case) (first case))))

(test ptui-input-ctrl-modifiers
  (assert-single-key-event '(1) :ctrl-a :expected-ctrlp t)
  (assert-single-key-event '(3) :ctrl-c :expected-ctrlp t)
  (assert-single-key-event '(27 91 49 59 53 65) :ctrl-up :expected-ctrlp t)
  (assert-single-key-event '(27 91 49 59 53 68) :ctrl-left :expected-ctrlp t))

(test ptui-input-function-keys-f1-f12
  (dolist (case '((:f1 27 91 49 49 126)
                  (:f2 27 91 49 50 126)
                  (:f3 27 91 49 51 126)
                  (:f4 27 91 49 52 126)
                  (:f5 27 91 49 53 126)
                  (:f6 27 91 49 54 126)
                  (:f7 27 91 49 55 126)
                  (:f8 27 91 49 56 126)
                  (:f9 27 91 49 57 126)
                  (:f10 27 91 50 48 126)
                  (:f11 27 91 50 49 126)
                  (:f12 27 91 50 50 126)))
    (assert-single-key-event (cdr case) (first case))))

(test ptui-input-home-end-pgup-pgdn
  (dolist (case '((:home 27 91 49 126)
                  (:end 27 91 52 126)
                  (:pgup 27 91 53 126)
                  (:pgdown 27 91 54 126)))
    (assert-single-key-event (cdr case) (first case))))

(test ptui-input-escape-alone-timeout
  (let ((parser (ptui.term.input:make-input-parser)))
    (ptui.term.input:input-feed parser (byte-array '(27)))
    (multiple-value-bind (events count)
        (ptui.term.input:input-drain-events parser)
      (is (= count 0))
      (is (null events)))
    (multiple-value-bind (events count)
        (ptui.term.input:input-drain-events parser)
      (is (= count 1))
      (is (eq :escape (ptui.core.events:key-event-key (first events)))))))

(defun make-env (terms)
  (lambda (name)
    (cdr (assoc name terms :test #'string=))))

(test ptui-caps-term-env
  (let ((caps
         (ptui.term.caps:probe-terminal-caps
          :env (make-env '(("TERM" . "xterm-256color")
                          ("COLORTERM" . nil))))))
    (is (string= "xterm-256color" (ptui.term.caps:terminal-caps-term caps)))
    (is (eq t (ptui.term.caps:terminal-caps-256colorp caps)))
    (is (eq nil (ptui.term.caps:terminal-caps-truecolorp caps)))))

(test ptui-caps-truecolor-from-colorterm
  (let ((caps
         (ptui.term.caps:probe-terminal-caps
          :env (make-env '(("TERM" . "xterm")
                          ("COLORTERM" . "24bit"))))))
    (is (eq t (ptui.term.caps:terminal-caps-truecolorp caps)))))

(defun run-all ()
  (run! 'ptui-input-suite))
