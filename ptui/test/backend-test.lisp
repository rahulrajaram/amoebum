(defpackage :ptui.test.backend
  (:use :cl :fiveam)
  (:export #:run-all #:ptui-backend-suite))

(in-package :ptui.test.backend)

(def-suite ptui-backend-suite
  :description "PTUI backend protocol + ANSI backend coverage (I311).")

(in-suite ptui-backend-suite)

(defun %probe-caps (term &optional colorterm)
  (ptui.term.caps:probe-terminal-caps
   :env (lambda (name)
          (cdr (assoc name `(("TERM" . ,term)
                            ("COLORTERM" . ,colorterm)) :test #'string=)))))

(defun %test-backend-buffer-cell (buf x y)
  (svref (ptui.core.types:cell-buffer-cells buf)
         (+ x (* y (ptui.core.types:cell-buffer-cols buf)))))

(defun %make-ansi-backend-with-mode (mode term &optional colorterm)
  (let ((stream (make-string-output-stream)))
    (values
     (let ((backend (ptui.backend.ansi:make-ansi-backend
                     :caps (%probe-caps term colorterm)
                     :stdout stream)))
       (setf (ptui.backend.ansi::backend-color-mode backend) mode)
       backend)
     stream)))

(defun %escape (payload)
  (concatenate 'string (string (code-char 27)) payload))

(defun %expected-ansi-style (mode fg bg attrs)
  (ptui.backend.ansi::%style->escape mode fg bg attrs))

;;; -------------------------------------------------------------------
;;; ptui.backend.protocol contract via ptui.backend.test backend
;;; -------------------------------------------------------------------

(test backend-test-init-shutdown-lifecycle
  (let ((backend (ptui.backend.test:make-test-backend :cols 12 :rows 6)))
    (is (null (ptui.backend.protocol:backend-init backend)))
    (is (null (ptui.backend.protocol:backend-shutdown backend)))))

(test backend-test-size-reflects-configuration
  (let ((backend (ptui.backend.test:make-test-backend :cols 40 :rows 12)))
    (let ((size (ptui.backend.protocol:backend-size backend)))
      (is (= 40 (ptui.core.types:size-cols size)))
      (is (= 12 (ptui.core.types:size-rows size))))))

(test backend-test-poll-events-drains-injected-queue
  (let ((backend (ptui.backend.test:make-test-backend))
        (event-a (ptui.core.events:make-key-event :text :text? "a"))
        (event-b (ptui.core.events:make-key-event :escape :ctrlp nil)))
    (ptui.backend.test:test-backend-inject-events backend (list event-a event-b))
    (let ((events (ptui.backend.protocol:backend-poll-events backend)))
      (is (= 2 (length events)))
      (is (eq (ptui.core.events:key-event-key (first events)) :text))
      (is (eq (ptui.core.events:key-event-key (second events)) :escape)))
    (is (null (ptui.backend.protocol:backend-poll-events backend)))))

(test backend-test-commit-applies-diff-style-bufs
  (let* ((backend (ptui.backend.test:make-test-backend :cols 5 :rows 3))
         (fill-cell (ptui.core.types:make-cell "X" :default :default
                                              (ptui.core.types:make-attrs)))
         (returned (ptui.backend.protocol:backend-commit
                    backend
                    (list (list :draw-text 1 0 "HI")
                          (list :fill-rect (ptui.core.types:make-rect 0 1 2 1) fill-cell)
                          (list :clear))))
         (buf (ptui.backend.test:test-backend-buffer backend)))
    (is (= 3 returned))
    (is (string= " " (ptui.core.types:cell-glyph (%test-backend-buffer-cell buf 0 0))))
    (is (string= " " (ptui.core.types:cell-glyph (%test-backend-buffer-cell buf 1 0))))
    (is (string= " " (ptui.core.types:cell-glyph (%test-backend-buffer-cell buf 2 1))))
    (is (string= " " (ptui.core.types:cell-glyph (%test-backend-buffer-cell buf 3 1))))))

;;; -------------------------------------------------------------------
;;; ptui.backend.ansi draw-op emission coverage
;;; -------------------------------------------------------------------

(test ansi-backend-commit-cell-op-emits-move-style-char
  (multiple-value-bind (backend out)
      (%make-ansi-backend-with-mode :x16 "xterm")
    (ptui.backend.protocol:backend-commit
     backend
     (list (ptui.render.diff:make-draw-op
            :cell :row 1 :col 2 :fg :default :bg :default
            :attrs (ptui.core.types:make-attrs) :text "A")))
    (let ((stdout (get-output-stream-string out))
          (style (%expected-ansi-style :x16 :default :default
                                      (ptui.core.types:make-attrs))))
      (is (string= stdout
                   (concatenate 'string
                                (%escape "[2;3H")
                                style
                                "A"))))))

(test ansi-backend-commit-text-op-emits-move-style-text
  (multiple-value-bind (backend out)
      (%make-ansi-backend-with-mode :x16 "xterm")
    (ptui.backend.protocol:backend-commit
     backend
     (list (ptui.render.diff:make-draw-op
            :text :row 0 :col 1 :fg :default :bg :default
            :attrs (ptui.core.types:make-attrs) :text "Hello")))
    (let ((stdout (get-output-stream-string out))
          (style (%expected-ansi-style :x16 :default :default
                                      (ptui.core.types:make-attrs))))
      (is (string= stdout
                   (concatenate 'string
                                (%escape "[1;2H")
                                style
                                "Hello"))))))

(test ansi-backend-full-redraw-emits-clear-and-repaint
  (multiple-value-bind (backend out)
      (%make-ansi-backend-with-mode :x16 "xterm")
    (ptui.backend.protocol:backend-commit
     backend
     (list (ptui.render.diff:make-draw-op
            :full-redraw :row 0 :col 0 :fg :default :bg :default
            :attrs (ptui.core.types:make-attrs) :text "RST")))
    (let ((stdout (get-output-stream-string out))
          (style (%expected-ansi-style :x16 :default :default
                                      (ptui.core.types:make-attrs))))
      (is (string= stdout
                   (concatenate 'string
                                (%escape "[2J")
                                (%escape "[1;1H")
                                style
                                "RST"))))))

(test ansi-backend-commit-supports-x256-sgr
  (multiple-value-bind (backend out)
      (%make-ansi-backend-with-mode :x256 "xterm-256color")
    (ptui.backend.protocol:backend-commit
     backend
     (list (ptui.render.diff:make-draw-op
            :text :row 0 :col 0
            :fg (ptui.core.color:make-color-rgb 255 0 0)
            :bg (ptui.core.color:make-color-rgb 0 255 0)
            :attrs (ptui.core.types:make-attrs)
            :text "X")))
    (let ((stdout (get-output-stream-string out))
          (style (%expected-ansi-style :x256
                                      (ptui.core.color:make-color-rgb 255 0 0)
                                      (ptui.core.color:make-color-rgb 0 255 0)
                                      (ptui.core.types:make-attrs))))
      (is (search style stdout)))))

(test ansi-backend-commit-supports-truecolor-sgr
  (multiple-value-bind (backend out)
      (%make-ansi-backend-with-mode :truecolor "xterm" "24bit")
    (ptui.backend.protocol:backend-commit
     backend
     (list (ptui.render.diff:make-draw-op
            :text :row 0 :col 0
            :fg (ptui.core.color:make-color-rgb 10 20 30)
            :bg (ptui.core.color:make-color-rgb 40 50 60)
            :attrs (ptui.core.types:make-attrs)
            :text "Y")))
    (let ((stdout (get-output-stream-string out))
          (style (%expected-ansi-style :truecolor
                                      (ptui.core.color:make-color-rgb 10 20 30)
                                      (ptui.core.color:make-color-rgb 40 50 60)
                                      (ptui.core.types:make-attrs))))
      (is (search style stdout)))))

(test ansi-backend-commit-attrs-bold-italic-underline-inverse
  (multiple-value-bind (backend out)
      (%make-ansi-backend-with-mode :x16 "xterm")
    (ptui.backend.protocol:backend-commit
     backend
     (list (ptui.render.diff:make-draw-op
            :text :row 1 :col 1
            :fg :default :bg :default
            :attrs (ptui.core.types:make-attrs :boldp t
                                              :italicp t
                                              :underlinep t
                                              :invertp t)
            :text "B")))
    (let ((stdout (get-output-stream-string out))
          (style (%expected-ansi-style
                  :x16 :default :default
                  (ptui.core.types:make-attrs :boldp t
                                              :italicp t
                                              :underlinep t
                                              :invertp t))))
      (is (search style stdout)))))

(defun run-all ()
  (run! 'ptui-backend-suite))
