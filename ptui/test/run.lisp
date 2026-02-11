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

(defun assert-layout-bound (layout node-id x y w h)
  (let ((bounds (ptui.layout:layout-bound layout node-id)))
    (assert-true bounds "missing layout bounds for ~S" node-id)
    (assert-true (= (ptui.layout:layout-bounds-x bounds) x)
                 "unexpected x for ~S: ~S" node-id bounds)
    (assert-true (= (ptui.layout:layout-bounds-y bounds) y)
                 "unexpected y for ~S: ~S" node-id bounds)
    (assert-true (= (ptui.layout:layout-bounds-width bounds) w)
                 "unexpected width for ~S: ~S" node-id bounds)
    (assert-true (= (ptui.layout:layout-bounds-height bounds) h)
                 "unexpected height for ~S: ~S" node-id bounds)))

(defun draw-op-kinds (ops)
  (mapcar #'ptui.render.diff::draw-op-kind ops))

(defun string-from-codepoints (&rest codepoints)
  (coerce (mapcar #'code-char codepoints) 'string))

(defun ui-op-kinds (ops)
  (mapcar #'ptui.ui.runtime:patch-op-kind ops))

(defun ui-op-ids (ops)
  (mapcar #'ptui.ui.runtime:patch-op-node-id ops))

(defun make-ui-node (type &key id key props children (focusablep nil))
  (ptui.ui.elements:make-element
   type
   :id id
   :key key
   :props props
   :children children
   :focusablep focusablep))

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

(deftest text-engine-adapters-available
  (assert-true (member :fallback (ptui.text.engine:available-text-engines))
               "fallback adapter should be registered")
  (assert-true (member :native (ptui.text.engine:available-text-engines))
               "native adapter stub should be registered")
  (assert-true (not (ptui.text.engine:engine-available-p :native))
               "native adapter should be unavailable until native hooks are wired")
  (assert-true (eql (ptui.text.engine:resolve-text-engine :native) :fallback)
               "native requests should resolve to fallback while unavailable"))

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

(deftest text-flags-and-regional-indicators
  (let* ((flag-us (string-from-codepoints #x1F1FA #x1F1F8))
         (flag-jp (string-from-codepoints #x1F1EF #x1F1F5))
         (both (concatenate 'string flag-us flag-jp)))
    (assert-true (= (length (ptui.text.grapheme:split-graphemes flag-us)) 1)
                 "US flag should be one grapheme")
    (assert-true (= (ptui.text.width:string-width flag-us) 2)
                 "US flag should be width=2")
    (assert-true (= (ptui.text.width:string-width both) 4)
                 "two flags should be width=4")
    (assert-true (equal (ptui.text.layout:wrap-by-width both 2) (list flag-us flag-jp))
                 "flags should wrap by full grapheme width")))

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

(deftest text-variation-selectors-and-keycaps
  (let* ((rainbow-flag (string-from-codepoints #x1F3F3 #xFE0F #x200D #x1F308))
         (kiss (string-from-codepoints #x1F469 #x200D #x2764 #xFE0F #x200D #x1F48B #x200D #x1F468))
         (keycap (string-from-codepoints #x0023 #xFE0F #x20E3)))
    (assert-true (= (length (ptui.text.grapheme:split-graphemes rainbow-flag)) 1)
                 "rainbow flag should remain one grapheme")
    (assert-true (= (ptui.text.width:string-width rainbow-flag) 2)
                 "rainbow flag should be width=2")
    (assert-true (= (length (ptui.text.grapheme:split-graphemes kiss)) 1)
                 "kiss ZWJ sequence should remain one grapheme")
    (assert-true (= (ptui.text.width:string-width kiss) 2)
                 "kiss ZWJ sequence should be width=2")
    (assert-true (= (length (ptui.text.grapheme:split-graphemes keycap)) 1)
                 "keycap sequence should remain one grapheme")
    (assert-true (= (ptui.text.width:string-width keycap) 2)
                 "keycap sequence should be width=2")))

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

(deftest text-cjk-and-ambiguous-width-edges
  (let* ((fullwidth-bang (string-from-codepoints #xFF01))
         (middle-dot (string-from-codepoints #x00B7))
         (em-dash (string-from-codepoints #x2014))
         (mixed (concatenate 'string fullwidth-bang middle-dot em-dash)))
    (assert-true (= (ptui.text.width:string-width fullwidth-bang) 2)
                 "fullwidth punctuation should be width=2")
    (assert-true (= (ptui.text.width:string-width middle-dot) 1)
                 "middle dot should remain ambiguous width=1 in fallback policy")
    (assert-true (= (ptui.text.width:string-width em-dash) 1)
                 "em dash should remain ambiguous width=1 in fallback policy")
    (assert-true (= (ptui.text.width:string-width mixed) 4)
                 "mixed string width should be 4")
    (assert-true (equal (ptui.text.layout:wrap-by-width mixed 3)
                        (list (concatenate 'string fullwidth-bang middle-dot)
                              em-dash))
                 "mixed width wrapping should stay grapheme-safe")))

(deftest layout-golden-column-contract
  (let* ((root (ptui.layout:make-layout-node
                :id :root
                :direction :column
                :width 12
                :gap 1
                :children
                (list
                 (ptui.layout:make-layout-node
                  :id :title
                  :measure (lambda (avail-w avail-h)
                             (declare (ignore avail-w avail-h))
                             (ptui.layout:make-layout-size 8 1)))
                 (ptui.layout:make-layout-node
                  :id :body
                  :height 3)
                 (ptui.layout:make-layout-node
                  :id :footer
                  :measure (lambda (avail-w avail-h)
                             (declare (ignore avail-w avail-h))
                             (ptui.layout:make-layout-size 10 1))))))
         (layout (ptui.layout:compute-layout root :x 2 :y 4)))
    (assert-layout-bound layout :root 2 4 12 7)
    (assert-layout-bound layout :title 2 4 8 1)
    (assert-layout-bound layout :body 2 6 12 3)
    (assert-layout-bound layout :footer 2 10 10 1)))

(deftest layout-golden-row-contract
  (let ((measure-calls '()))
    (flet ((track (id w h)
             (push (list id w h) measure-calls)))
      (let* ((root (ptui.layout:make-layout-node
                    :id :root
                    :direction :row
                    :height 2
                    :gap 1
                    :children
                    (list
                     (ptui.layout:make-layout-node
                      :id :a
                      :measure (lambda (avail-w avail-h)
                                 (track :a avail-w avail-h)
                                 (ptui.layout:make-layout-size 3 1)))
                     (ptui.layout:make-layout-node
                      :id :b
                      :width 4)
                     (ptui.layout:make-layout-node
                      :id :c
                      :measure (lambda (avail-w avail-h)
                                 (track :c avail-w avail-h)
                                 (ptui.layout:make-layout-size 2 2))))))
             (layout (ptui.layout:compute-layout root)))
        (assert-layout-bound layout :root 0 0 11 2)
        (assert-layout-bound layout :a 0 0 3 1)
        (assert-layout-bound layout :b 4 0 4 2)
        (assert-layout-bound layout :c 9 0 2 2)
        (assert-true (member (list :a nil 2) measure-calls :test #'equal)
                     "measure contract should expose available-height for row child A")
        (assert-true (member (list :c nil 2) measure-calls :test #'equal)
                     "measure contract should expose available-height for row child C")))))

(deftest ui-reconcile-deterministic-order
  (let* ((old-tree (make-ui-node
                    :root
                    :id :root
                    :children
                    (list
                     (make-ui-node :label :id :a :props '((:text . "A")))
                     (make-ui-node :label :id :b :props '((:text . "B"))))))
         (new-tree (make-ui-node
                    :root
                    :id :root
                    :children
                    (list
                     (make-ui-node :label :id :a :props '((:text . "A2")))
                     (make-ui-node :label :id :c :props '((:text . "C"))))))
         (ops (ptui.ui.runtime:reconcile-trees old-tree new-tree)))
    (assert-true (equal (ui-op-kinds ops) '(:update :mount :unmount))
                 "unexpected reconcile op kinds: ~S" (ui-op-kinds ops))
    (assert-true (equal (ui-op-ids ops) '(:a :c :b))
                 "unexpected reconcile node ids: ~S" (ui-op-ids ops))))

(deftest ui-runtime-update-ordering
  (let* ((runtime (ptui.ui.runtime:make-runtime))
         (effects-ran '())
         (tree (make-ui-node :root :id :root)))
    (ptui.ui.runtime:enqueue-effect runtime
                                    (lambda () (setf effects-ran (append effects-ran (list :effect-1)))))
    (ptui.ui.runtime:set-runtime-state runtime :counter 1)
    (ptui.ui.runtime:update-runtime runtime tree)
    (assert-true (= (ptui.ui.runtime:runtime-revision runtime) 1)
                 "runtime revision should increment to 1")
    (assert-true (eql (ptui.ui.runtime:runtime-state runtime :counter) 1)
                 "runtime state should persist")
    (assert-true (equal effects-ran '(:effect-1))
                 "effects should run exactly once in order: ~S" effects-ran)
    (assert-true (equal (ptui.ui.runtime:runtime-lifecycle-log runtime)
                        '(:reconcile-begin :reconcile-end :commit (:effect 1)))
                 "unexpected lifecycle ordering: ~S"
                 (ptui.ui.runtime:runtime-lifecycle-log runtime))))

(deftest ui-focus-routing-contract
  (let* ((runtime (ptui.ui.runtime:make-runtime))
         (tree-1 (make-ui-node
                  :root
                  :id :root
                  :children
                  (list
                   (make-ui-node :input :id :a :focusablep t)
                   (make-ui-node :input :id :b :focusablep t)
                   (make-ui-node :input :id :c :focusablep t))))
         (tree-2 (make-ui-node
                  :root
                  :id :root
                  :children
                  (list
                   (make-ui-node :input :id :c :focusablep t)))))
    (ptui.ui.runtime:update-runtime runtime tree-1)
    (assert-true (equal (ptui.ui.runtime:runtime-focus-order runtime) '(:a :b :c))
                 "unexpected initial focus order")
    (assert-true (eql (ptui.ui.runtime:runtime-focus-id runtime) :a)
                 "initial focus should land on first focusable node")
    (let* ((route-1 (ptui.ui.runtime:route-event runtime (ptui.core.events:make-key-event :tab)))
           (route-2 (ptui.ui.runtime:route-event runtime
                                                 (ptui.core.events:make-key-event :tab :shiftp t)))
           (route-3 (ptui.ui.runtime:route-event runtime (ptui.core.events:make-key-event :enter))))
      (assert-true (equal (getf route-1 :target) :b)
                   "tab should advance focus to :b, got ~S" route-1)
      (assert-true (equal (getf route-2 :target) :a)
                   "shift-tab should move focus back to :a, got ~S" route-2)
      (assert-true (equal (getf route-3 :target) :a)
                   "key events should route to current focus, got ~S" route-3))
    (ptui.ui.runtime:update-runtime runtime tree-2)
    (assert-true (equal (ptui.ui.runtime:runtime-focus-order runtime) '(:c))
                 "focus order should update after tree change")
    (assert-true (eql (ptui.ui.runtime:runtime-focus-id runtime) :c)
                 "focus should stabilize onto surviving focusable node")))

;; Script entry
(multiple-value-bind (passed failed) (run-all-tests)
  (declare (ignore passed))
  (uiop:quit (if (zerop failed) 0 1)))
