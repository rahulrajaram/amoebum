(defpackage :ptui.test.ansi-dispatch
  (:use :cl :fiveam)
  (:export #:run-all #:ansi-dispatch-suite))

(in-package :ptui.test.ansi-dispatch)

(def-suite ansi-dispatch-suite
  :description "ANSI backend draw-op/control-op dispatch table tests (FP-Refine Phase 3, T2).")

(in-suite ansi-dispatch-suite)

(defun %escape (payload)
  (concatenate 'string (string (code-char 27)) payload))

(defun %make-backend-and-stream (&key (mode :x16) (term "xterm"))
  (let ((stream (make-string-output-stream)))
    (values
     (let ((backend (ptui.backend.ansi:make-ansi-backend
                     :caps (ptui.term.caps:probe-terminal-caps
                            :env (lambda (name)
                                   (cdr (assoc name `(("TERM" . ,term)) :test #'string=))))
                     :stdout stream)))
       (setf (ptui.backend.ansi::backend-color-mode backend) mode)
       backend)
     stream)))

;;; --- Control escape table structure ---

(test control-escape-table-has-six-entries
  (is (= 6 (length ptui.backend.ansi::+ansi-control-escapes+))))

(test control-escape-table-keys-are-expected
  (let ((keys (mapcar #'car ptui.backend.ansi::+ansi-control-escapes+)))
    (dolist (k '(:clear-screen :clear-eol :hide-cursor :show-cursor :enter-alt :exit-alt))
      (is (member k keys :test #'eq)
          "Expected ~S in control escape table." k))))

(test control-escape-values-are-strings
  (dolist (entry ptui.backend.ansi::+ansi-control-escapes+)
    (is (stringp (cdr entry))
        "Escape for ~S should be a string." (car entry))))

;;; --- Control escape resolution ---

(test control-escape-clear-screen
  (is (string= (%escape "[2J") (ptui.backend.ansi::%control-escape :clear-screen))))

(test control-escape-clear-eol
  (is (string= (%escape "[K") (ptui.backend.ansi::%control-escape :clear-eol))))

(test control-escape-hide-cursor
  (is (string= (%escape "[?25l") (ptui.backend.ansi::%control-escape :hide-cursor))))

(test control-escape-show-cursor
  (is (string= (%escape "[?25h") (ptui.backend.ansi::%control-escape :show-cursor))))

(test control-escape-enter-alt
  (is (string= (%escape "[?1049h") (ptui.backend.ansi::%control-escape :enter-alt))))

(test control-escape-exit-alt
  (is (string= (%escape "[?1049l") (ptui.backend.ansi::%control-escape :exit-alt))))

(test control-escape-unknown-signals-error
  (signals error (ptui.backend.ansi::%control-escape :bogus)))

;;; --- Draw-op dispatch table structure ---

(test draw-op-table-has-twelve-entries
  (is (= 12 (length ptui.backend.ansi::+ansi-draw-op-handlers+))))

(test draw-op-table-entries-are-all-fboundp
  (dolist (entry ptui.backend.ansi::+ansi-draw-op-handlers+)
    (is (fboundp (cdr entry))
        "Draw-op handler ~S is not fboundp." (cdr entry))))

(test draw-op-table-contains-expected-kinds
  (let ((kinds (mapcar #'car ptui.backend.ansi::+ansi-draw-op-handlers+)))
    (dolist (k '(:move :style :write :cell :text :full-redraw
                 :clear-screen :clear-eol :hide-cursor :show-cursor :enter-alt :exit-alt))
      (is (member k kinds :test #'eq)
          "Expected ~S in draw-op handler table." k))))

;;; --- Draw-op handler behavior via backend-commit ---

(test draw-op-move-emits-cursor-position
  (multiple-value-bind (backend out) (%make-backend-and-stream)
    (ptui.backend.protocol:backend-commit
     backend
     (list (ptui.render.diff:make-draw-op :move :row 3 :col 5)))
    (let ((stdout (get-output-stream-string out)))
      (is (string= (%escape "[4;6H") stdout)))))

(test draw-op-write-emits-text-only
  (multiple-value-bind (backend out) (%make-backend-and-stream)
    (ptui.backend.protocol:backend-commit
     backend
     (list (ptui.render.diff:make-draw-op :write :text "hello")))
    (let ((stdout (get-output-stream-string out)))
      (is (string= "hello" stdout)))))

(test draw-op-cell-emits-move-style-text
  (multiple-value-bind (backend out) (%make-backend-and-stream)
    (ptui.backend.protocol:backend-commit
     backend
     (list (ptui.render.diff:make-draw-op
            :cell :row 0 :col 0
            :fg :default :bg :default
            :attrs (ptui.core.types:make-attrs)
            :text "X")))
    (let ((stdout (get-output-stream-string out)))
      (is (search (%escape "[1;1H") stdout))
      (is (search "X" stdout)))))

(test draw-op-control-ops-use-shared-escape-table
  (dolist (kind '(:clear-screen :clear-eol :hide-cursor :show-cursor :enter-alt :exit-alt))
    (multiple-value-bind (backend out) (%make-backend-and-stream)
      (ptui.backend.protocol:backend-commit
       backend
       (list (ptui.render.diff:make-draw-op kind)))
      (let ((stdout (get-output-stream-string out))
            (expected (ptui.backend.ansi::%control-escape kind)))
        (is (string= expected stdout)
            "Control draw-op ~S should emit ~S." kind expected)))))

(test draw-op-full-redraw-emits-clear-then-paint
  (multiple-value-bind (backend out) (%make-backend-and-stream)
    (ptui.backend.protocol:backend-commit
     backend
     (list (ptui.render.diff:make-draw-op
            :full-redraw :row 0 :col 0
            :fg :default :bg :default
            :attrs (ptui.core.types:make-attrs)
            :text "RST")))
    (let ((stdout (get-output-stream-string out)))
      ;; Should start with clear-screen escape
      (is (eql 0 (search (%escape "[2J") stdout)))
      ;; Should contain the painted text
      (is (search "RST" stdout)))))

(test draw-op-commit-returns-byte-count
  (multiple-value-bind (backend _out) (%make-backend-and-stream)
    (declare (ignore _out))
    (let ((bytes (ptui.backend.protocol:backend-commit
                  backend
                  (list (ptui.render.diff:make-draw-op :write :text "abc")))))
      (is (= 3 bytes)))))

(defun run-all ()
  (run! 'ansi-dispatch-suite))
