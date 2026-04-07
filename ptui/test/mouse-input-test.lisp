(defpackage :ptui.test.mouse-input
  (:use :cl :fiveam)
  (:export #:ptui-mouse-input-suite #:run-all))

(in-package :ptui.test.mouse-input)

(def-suite ptui-mouse-input-suite
  :description "PTUI mouse input protocol parsing tests.")

(in-suite ptui-mouse-input-suite)

;;; ---------------------------------------------------------------------------
;;; Helpers (mirror the pattern from input-test.lisp)
;;; ---------------------------------------------------------------------------

(defun byte-array (bytes)
  (coerce bytes '(simple-array (unsigned-byte 8) (*))))

(defun events-for (bytes)
  "Feed BYTES through a fresh input-parser and drain events.
  Returns (values events count)."
  (let ((parser (ptui.term.input:make-input-parser)))
    (ptui.term.input:input-feed parser (byte-array bytes))
    (multiple-value-list (ptui.term.input:input-drain-events parser))))

(defun single-mouse-event-for (bytes)
  "Feed BYTES, assert exactly one mouse-event is returned, and return it."
  (destructuring-bind (events count) (events-for bytes)
    (is (= count 1)
        "Expected 1 event from ~S, got ~D" bytes count)
    (let ((ev (first events)))
      (is (typep ev 'ptui.core.events:mouse-event)
          "Expected mouse-event, got ~S" ev)
      ev)))

;;; ---------------------------------------------------------------------------
;;; Basic button press / release
;;; ---------------------------------------------------------------------------

(test mouse-left-button-press
  ;; ESC [ < 0 ; 10 ; 5 M
  ;; bytes: 27 91 60 48 59 49 48 59 53 77
  ;; x=10 (1-based) -> 9  y=5 (1-based) -> 4
  (let ((ev (single-mouse-event-for '(27 91 60 48 59 49 48 59 53 77))))
    (is (eq :press  (ptui.core.events:mouse-event-kind ev)))
    (is (eq :left   (ptui.core.events:mouse-event-button ev)))
    (is (= 9        (ptui.core.events:mouse-event-x ev)))
    (is (= 4        (ptui.core.events:mouse-event-y ev)))))

(test mouse-left-button-release
  ;; ESC [ < 0 ; 10 ; 5 m  (lowercase m = release)
  ;; bytes: 27 91 60 48 59 49 48 59 53 109
  (let ((ev (single-mouse-event-for '(27 91 60 48 59 49 48 59 53 109))))
    (is (eq :release (ptui.core.events:mouse-event-kind ev)))
    (is (eq :left    (ptui.core.events:mouse-event-button ev)))))

(test mouse-right-button-press
  ;; ESC [ < 2 ; 1 ; 1 M
  ;; Cb=2 -> right button; x=1->0, y=1->0
  ;; bytes: 27 91 60 50 59 49 59 49 77
  (let ((ev (single-mouse-event-for '(27 91 60 50 59 49 59 49 77))))
    (is (eq :press (ptui.core.events:mouse-event-kind ev)))
    (is (eq :right (ptui.core.events:mouse-event-button ev)))
    (is (= 0       (ptui.core.events:mouse-event-x ev)))
    (is (= 0       (ptui.core.events:mouse-event-y ev)))))

(test mouse-middle-button-press
  ;; ESC [ < 1 ; 20 ; 10 M
  ;; Cb=1 -> middle button; x=20->19, y=10->9
  ;; bytes: 27 91 60 49 59 50 48 59 49 48 77
  (let ((ev (single-mouse-event-for '(27 91 60 49 59 50 48 59 49 48 77))))
    (is (eq :press  (ptui.core.events:mouse-event-kind ev)))
    (is (eq :middle (ptui.core.events:mouse-event-button ev)))
    (is (= 19       (ptui.core.events:mouse-event-x ev)))
    (is (= 9        (ptui.core.events:mouse-event-y ev)))))

;;; ---------------------------------------------------------------------------
;;; Scroll events
;;; ---------------------------------------------------------------------------

(test mouse-scroll-up
  ;; ESC [ < 64 ; 15 ; 8 M
  ;; Cb=64 (bit 6 set, bit 0 clear) -> scroll-up
  ;; bytes: 27 91 60 54 52 59 49 53 59 56 77
  (let ((ev (single-mouse-event-for '(27 91 60 54 52 59 49 53 59 56 77))))
    (is (eq :scroll-up (ptui.core.events:mouse-event-kind ev)))
    (is (null          (ptui.core.events:mouse-event-button ev)))))

(test mouse-scroll-down
  ;; ESC [ < 65 ; 15 ; 8 M
  ;; Cb=65 (bit 6 set, bit 0 set) -> scroll-down
  ;; bytes: 27 91 60 54 53 59 49 53 59 56 77
  (let ((ev (single-mouse-event-for '(27 91 60 54 53 59 49 53 59 56 77))))
    (is (eq :scroll-down (ptui.core.events:mouse-event-kind ev)))
    (is (null            (ptui.core.events:mouse-event-button ev)))))

;;; ---------------------------------------------------------------------------
;;; Motion event
;;; ---------------------------------------------------------------------------

(test mouse-motion-with-left-held
  ;; ESC [ < 32 ; 5 ; 5 M
  ;; Cb=32 = 0x20 (bit 5 set, btn-bits=0) -> motion with left button
  ;; bytes: 27 91 60 51 50 59 53 59 53 77
  (let ((ev (single-mouse-event-for '(27 91 60 51 50 59 53 59 53 77))))
    (is (eq :move (ptui.core.events:mouse-event-kind ev)))
    (is (eq :left (ptui.core.events:mouse-event-button ev)))))

;;; ---------------------------------------------------------------------------
;;; Large coordinates
;;; ---------------------------------------------------------------------------

(test mouse-large-coordinates
  ;; ESC [ < 0 ; 200 ; 100 M
  ;; x=200->199, y=100->99
  ;; "200" = 50 48 48  "100" = 49 48 48
  ;; bytes: 27 91 60 48 59 50 48 48 59 49 48 48 77
  (let ((ev (single-mouse-event-for '(27 91 60 48 59 50 48 48 59 49 48 48 77))))
    (is (eq :press (ptui.core.events:mouse-event-kind ev)))
    (is (= 199     (ptui.core.events:mouse-event-x ev)))
    (is (= 99      (ptui.core.events:mouse-event-y ev)))))

;;; ---------------------------------------------------------------------------
;;; Multiple events in one feed
;;; ---------------------------------------------------------------------------

(test mouse-two-events-in-one-feed
  ;; Feed two complete SGR mouse sequences back-to-back.
  ;; Seq1: ESC [ < 0 ; 1 ; 1 M  (left press at 0,0)
  ;;   bytes: 27 91 60 48 59 49 59 49 77
  ;; Seq2: ESC [ < 0 ; 1 ; 1 m  (left release at 0,0)
  ;;   bytes: 27 91 60 48 59 49 59 49 109
  (destructuring-bind (events count)
      (events-for '(27 91 60 48 59 49 59 49 77
                    27 91 60 48 59 49 59 49 109))
    (is (= 2 count))
    (is (eq :press   (ptui.core.events:mouse-event-kind (first events))))
    (is (eq :release (ptui.core.events:mouse-event-kind (second events))))))

;;; ---------------------------------------------------------------------------
;;; Mouse event mixed with keyboard input
;;; ---------------------------------------------------------------------------

(test mouse-event-followed-by-key
  ;; SGR left-press at (0,0) followed by 'a' (byte 97).
  ;; Mouse bytes: 27 91 60 48 59 49 59 49 77
  ;; Key byte:    97
  (destructuring-bind (events count)
      (events-for '(27 91 60 48 59 49 59 49 77 97))
    (is (= 2 count))
    (let ((mouse-ev (first events))
          (key-ev   (second events)))
      (is (typep mouse-ev 'ptui.core.events:mouse-event))
      (is (eq :press (ptui.core.events:mouse-event-kind mouse-ev)))
      (is (typep key-ev 'ptui.core.events:key-event))
      (is (eq :text  (ptui.core.events:key-event-key key-ev)))
      (is (string= "a" (ptui.core.events:key-event-text? key-ev))))))

;;; ---------------------------------------------------------------------------
;;; Incomplete sequence stays in buffer (no events produced)
;;; ---------------------------------------------------------------------------

(test mouse-incomplete-sequence-no-events
  ;; ESC [ < 0 ; 10  — missing the second semicolon, Cy, and final byte.
  ;; bytes: 27 91 60 48 59 49 48
  (destructuring-bind (events count)
      (events-for '(27 91 60 48 59 49 48))
    (is (= 0 count))
    (is (null events))))

;;; ---------------------------------------------------------------------------
;;; Malformed sequence: ESC [ < M  (no numeric params) -> escape fallback
;;; ---------------------------------------------------------------------------

(test mouse-malformed-no-params-fallback
  ;; ESC [ < M  — the '<' is followed immediately by 'M' (no digit for Cb).
  ;; bytes: 27 91 60 77
  ;; The parser falls back to consuming only the ESC byte (returns 1), then
  ;; the remaining bytes [ < M are each parsed as printable text events.
  ;; So we expect 4 events total: :escape + "[" + "<" + "M".
  (destructuring-bind (events count)
      (events-for '(27 91 60 77))
    (is (= 4 count))
    (let ((first-ev (first events)))
      (is (typep first-ev 'ptui.core.events:key-event))
      (is (eq :escape (ptui.core.events:key-event-key first-ev))))))

(defun run-all ()
  (run! 'ptui-mouse-input-suite))
