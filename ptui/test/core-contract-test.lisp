(defpackage :ptui.test.core-contract
  (:use :cl :fiveam)
  (:export #:run-all #:ptui-core-contract-suite))

(in-package :ptui.test.core-contract)

(def-suite ptui-core-contract-suite
  :description "PTUI foundational core contracts: color, types, and events.")

(in-suite ptui-core-contract-suite)

(defun %caps (&key (term "xterm") colorterm)
  (ptui.term.caps:probe-terminal-caps
   :env (lambda (name)
          (cdr (assoc name `(("TERM" . ,term)
                             ("COLORTERM" . ,colorterm))
                      :test #'string=)))))

(test core-color-make-color-rgb-validates-channels
  (let ((color (ptui.core.color:make-color-rgb 1 2 3)))
    (is (= 1 (ptui.core.color:color-rgb-r color)))
    (is (= 2 (ptui.core.color:color-rgb-g color)))
    (is (= 3 (ptui.core.color:color-rgb-b color))))
  (signals error (ptui.core.color:make-color-rgb -1 0 0))
  (signals error (ptui.core.color:make-color-rgb 0 256 0))
  (signals error (ptui.core.color:make-color-rgb 0 0 "3")))

(test core-color-resolve-color-mode-follows-cap-precedence
  (is (eq :truecolor
          (ptui.core.color:resolve-color-mode
           (%caps :term "xterm-256color" :colorterm "24bit"))))
  (is (eq :x256
          (ptui.core.color:resolve-color-mode
           (%caps :term "xterm-256color"))))
  (is (eq :x16
          (ptui.core.color:resolve-color-mode
           (%caps :term "xterm")))))

(test core-color->sgr-normalizes-defaults-and-rgb-inputs
  (is (string= "39"
               (ptui.core.color:color->sgr nil :mode :x16 :fg-or-bg :fg)))
  (is (string= "49"
               (ptui.core.color:color->sgr :default :mode :x16 :fg-or-bg :bg)))
  (is (string= "38;2;10;20;30"
               (ptui.core.color:color->sgr '(10 20 30)
                                           :mode :truecolor
                                           :fg-or-bg :fg)))
  (is (string= "48;5;196"
               (ptui.core.color:color->sgr #(255 0 0)
                                           :mode :x256
                                           :fg-or-bg :bg)))
  (is (string= "31"
               (ptui.core.color:color->sgr
                (ptui.core.color:make-color-rgb 255 0 0)
                :mode :x16
                :fg-or-bg :fg))))

(test core-types-size-aliases-and-attr-defaults
  (let ((size (ptui.core.types:make-size 12 5))
        (attrs (ptui.core.types:make-attrs)))
    (is (= 12 (ptui.core.types:size-cols size)))
    (is (= 5 (ptui.core.types:size-rows size)))
    (is (= 12 (ptui.core.types:size-width size)))
    (is (= 5 (ptui.core.types:size-height size)))
    (is-false (ptui.core.types:attrs-boldp attrs))
    (is-false (ptui.core.types:attrs-italicp attrs))
    (is-false (ptui.core.types:attrs-underlinep attrs))
    (is-false (ptui.core.types:attrs-invertp attrs))
    (is-false (ptui.core.types:attrs-dimp attrs))
    (is-false (ptui.core.types:attrs-strikep attrs))))

(test core-types-make-cell-normalizes-colors
  (let* ((attrs (ptui.core.types:make-attrs :boldp t))
         (cell (ptui.core.types:make-cell "X" '(1 2 3) nil attrs))
         (bg-cell (ptui.core.types:make-cell "Y" :default #(4 5 6)
                                             (ptui.core.types:make-attrs))))
    (is (typep (ptui.core.types:cell-fg cell) 'ptui.core.color:color-rgb))
    (is (= 1 (ptui.core.color:color-rgb-r (ptui.core.types:cell-fg cell))))
    (is (= 2 (ptui.core.color:color-rgb-g (ptui.core.types:cell-fg cell))))
    (is (= 3 (ptui.core.color:color-rgb-b (ptui.core.types:cell-fg cell))))
    (is (eq ptui.core.color:color-default (ptui.core.types:cell-bg cell)))
    (is (eq ptui.core.color:color-default (ptui.core.types:cell-fg bg-cell)))
    (is (= 4 (ptui.core.color:color-rgb-r (ptui.core.types:cell-bg bg-cell))))
    (is (= 5 (ptui.core.color:color-rgb-g (ptui.core.types:cell-bg bg-cell))))
    (is (= 6 (ptui.core.color:color-rgb-b (ptui.core.types:cell-bg bg-cell))))
    (is (ptui.core.types:attrs-boldp (ptui.core.types:cell-attrs cell)))))

(test core-types-make-cell-rejects-non-string-glyphs
  (signals error
    (ptui.core.types:make-cell #\X :default :default
                               (ptui.core.types:make-attrs))))

(test core-types-make-cell-buffer-validates-cell-count
  (let ((cells (make-array 1 :initial-element nil)))
    (signals error
      (ptui.core.types:make-cell-buffer 2 2 cells))))

(test core-events-key-event-modifiers-preserve-order
  (let ((event (ptui.core.events:make-key-event :text
                                                :ctrlp t
                                                :altp t
                                                :shiftp t
                                                :text? "a")))
    (is (equal '(:ctrl :alt :shift)
               (ptui.core.events:key-event-modifiers event)))
    (is (string= "a" (ptui.core.events:key-event-text? event)))))

(test core-events-resize-detection-and-constructor-defaults
  (let ((resize (ptui.core.events:make-key-event :resize))
        (key (ptui.core.events:make-key-event :escape))
        (mouse (ptui.core.events:make-mouse-event))
        (paste (ptui.core.events:make-paste-event "hello")))
    (is-true (ptui.core.events:resize-event resize))
    (is-false (ptui.core.events:resize-event key))
    (is (eq :move (ptui.core.events:mouse-event-kind mouse)))
    (is (= 0 (ptui.core.events:mouse-event-x mouse)))
    (is (= 0 (ptui.core.events:mouse-event-y mouse)))
    (is (null (ptui.core.events:mouse-event-button mouse)))
    (is (string= "hello" (ptui.core.events:paste-event-text paste)))))

(defun run-all ()
  (run! 'ptui-core-contract-suite))
