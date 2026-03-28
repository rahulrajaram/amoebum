#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

"${ROOT_DIR}/bin/ensure-quicklisp.sh" >/dev/null

sbcl --noinform --non-interactive \
  --eval "(require :asdf)" \
  --eval "(let ((*compile-verbose* nil) (*load-verbose* nil))
             (handler-bind ((warning (lambda (c) (muffle-warning c))))
               (load \"${ROOT_DIR}/.tools/quicklisp/setup.lisp\")
               (setf asdf:*compile-file-warnings-behaviour* :ignore)
               (asdf:load-asd (truename \"${ROOT_DIR}/ptui.asd\"))
               (asdf:load-asd (truename \"${ROOT_DIR}/ptui-examples.asd\"))
               (asdf:load-system :ptui/examples)
               (asdf:load-system :ptui/examples-standalone)))" \
  --eval "(let* ((buf (ptui.examples.buffer-basics:render-buffer-demo 20 8))
                 (lines (ptui.examples.text-layout-basics:wrapped-lines 12))
                 (make-state (symbol-function (find-symbol \"MAKE-EXAMPLE-STATE\" :ptui.examples.event-handling-basics)))
                 (state-count (symbol-function (find-symbol \"EXAMPLE-STATE-COUNT\" :ptui.examples.event-handling-basics)))
                 (state-last-event (symbol-function (find-symbol \"EXAMPLE-STATE-LAST-EVENT\" :ptui.examples.event-handling-basics)))
                 (handle-event (symbol-function (find-symbol \"%HANDLE-EVENT\" :ptui.examples.event-handling-basics)))
                 (up-state (funcall handle-event (funcall make-state)
                                    (ptui.core.events:make-key-event :up)))
                 (down-state (funcall handle-event up-state
                                      (ptui.core.events:make-key-event :down)))
                 (reset-values (multiple-value-list
                                (funcall handle-event down-state
                                         (ptui.core.events:make-key-event :text :text? \"r\"))))
                 (reset-state (first reset-values))
                 (reset-disposition (second reset-values)))
            (assert (= 20 (ptui.core.types:cell-buffer-cols buf)))
            (assert (= 8 (ptui.core.types:cell-buffer-rows buf)))
            (assert (> (length lines) 1))
            (assert (every (lambda (line) (<= (length line) 12)) lines))
            (assert (= 1 (funcall state-count up-state)))
            (assert (= 0 (funcall state-count down-state)))
            (assert (= 0 (funcall state-count reset-state)))
            (assert (eql :consume reset-disposition))
            (assert (string= \"reset\" (funcall state-last-event reset-state)))
            (format t \"EXAMPLE_SMOKE_OK~%\"))"
