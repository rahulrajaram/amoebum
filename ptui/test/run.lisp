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

(defun assert-near (actual expected epsilon fmt &rest args)
  (unless (<= (abs (- actual expected)) epsilon)
    (apply #'error
           (concatenate 'string fmt " (actual=~S expected=~S epsilon=~S)")
           (append args (list actual expected epsilon)))))

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

(defun native-differential-corpus ()
  (list
   "abc"
   (string-from-codepoints #x0065 #x0301)
   (string-from-codepoints #x1F1FA #x1F1F8)
   (string-from-codepoints #x1F468 #x200D #x1F469 #x200D #x1F467 #x200D #x1F466)
   (string-from-codepoints #x0041 #x754C #x0042)
   (string-from-codepoints #x0023 #xFE0F #x20E3)))

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

(defun buffer-cell-at (buf x y)
  (svref (ptui.core.types:cell-buffer-cells buf)
         (+ x (* y (ptui.core.types:cell-buffer-cols buf)))))

(defun buffer->flat-text (buf)
  (let ((cols (ptui.core.types:cell-buffer-cols buf))
        (rows (ptui.core.types:cell-buffer-rows buf)))
    (with-output-to-string (out)
      (loop for y from 0 below rows do
        (loop for x from 0 below cols do
          (let ((glyph (ptui.core.types:cell-glyph (buffer-cell-at buf x y))))
            (write-string (if (string= glyph "") " " glyph) out)))
        (when (< y (1- rows))
          (write-char #\Newline out))))))

(defun buffer-max-content-width (buf)
  (let ((cols (ptui.core.types:cell-buffer-cols buf))
        (rows (ptui.core.types:cell-buffer-rows buf))
        (max-used 0))
    (loop for y from 0 below rows do
      (let ((last-used 0))
        (loop for x from 0 below cols do
          (let ((glyph (ptui.core.types:cell-glyph (buffer-cell-at buf x y))))
            (unless (or (string= glyph "") (string= glyph " "))
              (setf last-used (1+ x)))))
        (setf max-used (max max-used last-used))))
    max-used))

(defun make-temp-directory (prefix)
  (uiop:ensure-directory-pathname
   (merge-pathnames
    (make-pathname
     :directory `(:relative
                  ,(format nil "~A-~D-~D"
                           prefix
                           (get-universal-time)
                           (random 1000000))))
    (uiop:ensure-directory-pathname (uiop:temporary-directory)))))

(defun write-text-file (path content)
  (ensure-directories-exist path)
  (with-open-file (stream path
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create
                          :external-format :utf-8)
    (write-string content stream))
  path)

(defun delete-directory-tree-safe (path)
  (when (and path (probe-file path))
    (ignore-errors
      (uiop:delete-directory-tree path
                                  :validate t
                                  :if-does-not-exist :ignore))))

(defun glob-relative-paths (result)
  (mapcar #'ptui.search.glob:glob-entry-relative-path
          (ptui.search.glob:glob-scan-result-matches result)))

(defun make-atop-snapshot-fixture (&key
                                     (timestamp-ms 0)
                                     (cpu-user 0)
                                     (cpu-system 0)
                                     (cpu-idle 100)
                                     (cpu-iowait 0)
                                     (mem-total-kb 1024)
                                     (mem-avail-kb 512)
                                     (swap-total-kb 0)
                                     (swap-free-kb 0)
                                     (mount-count 3)
                                     (rw-mount-count 2)
                                     (root-device "/dev/sda1")
                                     (root-fstype "ext4")
                                     (read-ios 0)
                                     (write-ios 0)
                                     (read-sectors 0)
                                     (write-sectors 0)
                                     (inflight 0)
                                     (io-ms 0)
                                     (rotational 1)
                                     (ssd 0)
                                     (iface-count 1)
                                     (rx-bytes 0)
                                     (tx-bytes 0)
                                     (rx-packets 0)
                                     (tx-packets 0)
                                     (fastest-link-mbps 1000)
                                     (tcp-in-segs 0)
                                     (tcp-out-segs 1)
                                     (tcp-retrans-segs 0)
                                     (tcp-active-opens 0)
                                     (tcp-passive-opens 0)
                                     (tcp-curr-estab 0)
                                     (ip-in-receives 0)
                                     (ip-in-delivers 0)
                                     (ip-out-requests 0)
                                     (processes '()))
  (ptui.examples.atop-dashboard::make-host-snapshot
   :timestamp-ms timestamp-ms
   :cpu (ptui.examples.atop-dashboard::make-cpu-counters
         :user cpu-user
         :system cpu-system
         :idle cpu-idle
         :iowait cpu-iowait)
   :memory (ptui.examples.atop-dashboard::make-memory-counters
            :total-kb mem-total-kb
            :available-kb mem-avail-kb
            :swap-total-kb swap-total-kb
            :swap-free-kb swap-free-kb)
   :filesystem (ptui.examples.atop-dashboard::make-filesystem-counters
                :mount-count mount-count
                :rw-mount-count rw-mount-count
                :root-device root-device
                :root-fstype root-fstype)
   :disk (ptui.examples.atop-dashboard::make-disk-counters
          :read-ios read-ios
          :write-ios write-ios
          :read-sectors read-sectors
          :write-sectors write-sectors
          :inflight inflight
          :io-ms io-ms
          :rotational-devices rotational
          :ssd-devices ssd)
   :network (ptui.examples.atop-dashboard::make-network-counters
             :iface-count iface-count
             :rx-bytes rx-bytes
             :tx-bytes tx-bytes
             :rx-packets rx-packets
             :tx-packets tx-packets
             :fastest-link-mbps fastest-link-mbps)
   :tcpip (ptui.examples.atop-dashboard::make-tcpip-counters
           :tcp-in-segs tcp-in-segs
           :tcp-out-segs tcp-out-segs
           :tcp-retrans-segs tcp-retrans-segs
           :tcp-active-opens tcp-active-opens
           :tcp-passive-opens tcp-passive-opens
           :tcp-curr-estab tcp-curr-estab
           :ip-in-receives ip-in-receives
           :ip-in-delivers ip-in-delivers
           :ip-out-requests ip-out-requests)
   :processes processes))

(defun make-process-counters-fixture (pid &key
                                       (user "root")
                                       (state "R")
                                       (cpu-total-ticks 0)
                                       (rss-kb 0)
                                       (command "cmd"))
  (ptui.examples.atop-dashboard::make-process-counters
   :pid pid
   :user user
   :state state
   :cpu-total-ticks cpu-total-ticks
   :rss-kb rss-kb
   :command command))

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

(deftest diff-prev-nil-safe-without-full-redraw-flag
  (let ((buf (ptui.render.buffer:make-buffer 3 1)))
    (ptui.render.buffer:buffer-draw-text buf 0 0 "A")
    (multiple-value-bind (ops count)
        (ptui.render.diff:diff-buffers nil buf :full-redraw nil)
      (declare (ignore count))
      (assert-true (member :clear-screen (draw-op-kinds ops))
                   "prev=nil should force safe full redraw path"))))

(deftest diff-same-buffer-short-circuits
  (let ((buf (ptui.render.buffer:make-buffer 4 1)))
    (ptui.render.buffer:buffer-draw-text buf 0 0 "AB")
    (multiple-value-bind (ops count)
        (ptui.render.diff:diff-buffers buf buf :full-redraw nil)
      (assert-true (null ops)
                   "same buffer object should produce no draw ops, got: ~S"
                   (draw-op-kinds ops))
      (assert-true (= count 0)
                   "same buffer object should report 0 ops, got: ~D"
                   count))))

(deftest render-wide-grapheme-occupies-multiple-cells
  (let* ((buf (ptui.render.buffer:make-buffer 5 1))
         (text (concatenate 'string "A" (string-from-codepoints #x754C) "B")))
    (ptui.render.buffer:buffer-draw-text buf 0 0 text)
    (assert-true (string= (ptui.core.types:cell-glyph (buffer-cell-at buf 0 0)) "A")
                 "col0 should be A")
    (assert-true (string= (ptui.core.types:cell-glyph (buffer-cell-at buf 1 0))
                          (string-from-codepoints #x754C))
                 "col1 should be wide grapheme")
    (assert-true (string= (ptui.core.types:cell-glyph (buffer-cell-at buf 2 0)) "")
                 "col2 should be continuation cell")
    (assert-true (string= (ptui.core.types:cell-glyph (buffer-cell-at buf 3 0)) "B")
                 "col3 should be B")))

(deftest render-emoji-zwj-occupies-two-cells
  (let* ((buf (ptui.render.buffer:make-buffer 5 1))
         (emoji (string-from-codepoints #x1F469 #x200D #x1F4BB))
         (text (concatenate 'string emoji "X")))
    (ptui.render.buffer:buffer-draw-text buf 0 0 text)
    (assert-true (string= (ptui.core.types:cell-glyph (buffer-cell-at buf 0 0)) emoji)
                 "col0 should hold ZWJ grapheme")
    (assert-true (string= (ptui.core.types:cell-glyph (buffer-cell-at buf 1 0)) "")
                 "col1 should be continuation for ZWJ grapheme")
    (assert-true (string= (ptui.core.types:cell-glyph (buffer-cell-at buf 2 0)) "X")
                 "col2 should hold trailing ASCII")))

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

(deftest input-parser-linefeed-maps-to-ctrl-j
  (let ((p (ptui.term.input:make-input-parser)))
    (ptui.term.input:input-feed p (make-array 1 :element-type '(unsigned-byte 8)
                                              :initial-contents (list 10)))
    (multiple-value-bind (events count) (ptui.term.input:input-drain-events p)
      (assert-true (= count 1) "expected one ctrl-j event, got ~D / ~S" count events)
      (let ((ev (first events)))
        (assert-true (eql (ptui.core.events:key-event-key ev) :ctrl-j)
                     "expected :ctrl-j key, got ~S" (ptui.core.events:key-event-key ev))
        (assert-true (ptui.core.events:key-event-ctrlp ev)
                     "expected ctrl modifier on :ctrl-j event")))))

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

(deftest diff-style-equality-treats-equal-rgb-values-as-equal
  (let* ((buf1 (ptui.render.buffer:make-buffer 2 1))
         (buf2 (ptui.render.buffer:make-buffer 2 1))
         (a1 (ptui.core.types:make-cell "X" (ptui.core.color:make-color-rgb 1 2 3) :default
                                        (ptui.core.types:make-attrs)))
         (a2 (ptui.core.types:make-cell "X" (ptui.core.color:make-color-rgb 1 2 3) :default
                                        (ptui.core.types:make-attrs))))
    (ptui.render.buffer:buffer-draw-text buf1 0 0 a1)
    (ptui.render.buffer:buffer-draw-text buf2 0 0 a2)
    (multiple-value-bind (ops count)
        (ptui.render.diff:diff-buffers buf1 buf2 :full-redraw nil)
      (assert-true (= count 0)
                   "equal rgb values should not produce diff ops, got ~D (~S)" count (draw-op-kinds ops)))))

(deftest cell-glyph-allows-grapheme-clusters
  (let* ((glyph (string-from-codepoints #x0065 #x0301))
         (cell (ptui.core.types:make-cell glyph :default :default (ptui.core.types:make-attrs))))
    (assert-true (string= (ptui.core.types:cell-glyph cell) glyph)
                 "cell glyph should preserve full grapheme cluster")))

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

(deftest text-native-activation-contract
  (let ((env-off (lambda (key)
                   (declare (ignore key))
                   nil))
        (env-on (lambda (key)
                  (if (string= key ptui.text.adapter.native:+native-enable-env-var+)
                      "1"
                      nil)))
        (env-on-with-parity (lambda (key)
                              (cond
                                ((string= key ptui.text.adapter.native:+native-enable-env-var+) "1")
                                ((string= key ptui.text.adapter.native:+native-require-parity-env-var+)
                                 "true")
                                (t nil)))))
    (assert-true (not (ptui.text.adapter.native:native-feature-enabled-p :env env-off))
                 "native feature flag should be off by default")
    (assert-true (ptui.text.adapter.native:native-feature-enabled-p :env env-on)
                 "native feature flag should parse truthy values")
    (assert-true (not (ptui.text.adapter.native:native-runtime-contract-satisfied-p))
                 "runtime contract should fail until native hooks are wired")
    (let ((ptui.text.adapter.native:*native-grapheme-support-p* t)
          (ptui.text.adapter.native:*native-width-support-p* t))
      (assert-true (ptui.text.adapter.native:native-runtime-contract-satisfied-p)
                   "runtime contract should pass when both hooks are wired")
      (assert-true (not (ptui.text.adapter.native:native-engine-available-p :env env-off))
                   "native engine should remain disabled when feature flag is off")
      (assert-true (ptui.text.adapter.native:native-engine-available-p :env env-on)
                   "native engine should activate with flag + hooks")
      (assert-true (ptui.text.adapter.native:native-engine-available-p :env env-on-with-parity)
                   "native engine should pass optional parity gate for current adapter")
      (assert-true (ptui.text.adapter.native:native-parity-check-p
                    :corpus (native-differential-corpus))
                   "native parity check should pass on fixed corpus"))))

(deftest text-native-engine-currently-aliases-fallback
  (let ((text (string-from-codepoints #x0065 #x0301 #x1F468 #x200D #x1F469)))
    (assert-true
     (equal (ptui.text.grapheme:split-graphemes text :engine :native)
            (ptui.text.grapheme:split-graphemes text :engine :fallback))
     "native engine should currently alias fallback behavior")))

(deftest text-native-differential-fixed-corpus
  (let ((ptui.text.adapter.native:*native-grapheme-support-p* t)
        (ptui.text.adapter.native:*native-width-support-p* t)
        (ptui.text.adapter.native::*native-enable-override* t)
        (ptui.text.adapter.native::*native-require-parity-override* nil))
    (assert-true (eql (ptui.text.engine:resolve-text-engine :native) :native)
                 "native requests should resolve to :native when activation contract passes")
    (dolist (text (native-differential-corpus))
      (assert-true (equal (ptui.text.grapheme:split-graphemes text :engine :fallback)
                          (ptui.text.grapheme:split-graphemes text :engine :native))
                   "native split mismatch on corpus text ~S" text)
      (assert-true (= (ptui.text.width:string-width text :engine :fallback)
                      (ptui.text.width:string-width text :engine :native))
                   "native width mismatch on corpus text ~S" text)
      (dolist (max-width '(1 2 3 4))
        (assert-true (equal (ptui.text.layout:wrap-by-width text max-width :engine :fallback)
                            (ptui.text.layout:wrap-by-width text max-width :engine :native))
                     "native wrap mismatch on corpus text ~S (max-width=~D)"
                     text max-width)
        (assert-true
         (string= (ptui.text.layout:truncate-to-width text max-width :engine :fallback)
                  (ptui.text.layout:truncate-to-width text max-width :engine :native))
         "native truncate mismatch on corpus text ~S (max-width=~D)"
         text max-width)))))

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

(deftest ui-runtime-lifecycle-log-bounded
  (let* ((runtime (ptui.ui.runtime:make-runtime))
         (tree (make-ui-node :root :id :root))
         (limit ptui.ui.runtime::*runtime-lifecycle-log-limit*))
    (loop repeat (+ limit 40) do
      (ptui.ui.runtime:update-runtime runtime tree))
    (assert-true (<= (length (ptui.ui.runtime:runtime-lifecycle-log runtime)) limit)
                 "lifecycle log should be bounded by limit")
    (assert-true (member :commit (ptui.ui.runtime:runtime-lifecycle-log runtime))
                 "bounded lifecycle log should still contain commit events")))

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

(deftest ui-duplicate-child-selectors-rejected
  (handler-case
      (progn
        (make-ui-node :root
                      :children (list (make-ui-node :text :id :dup)
                                      (make-ui-node :text :id :dup)))
        (error "expected duplicate selector error"))
    (error ()
      t)))

(deftest widgets-sizing-primitives
  (let* ((text (ptui.widgets.core:make-text-widget "AB"))
         (spacer (ptui.widgets.core:make-spacer-widget 3 2))
         (stack (ptui.widgets.core:make-stack-widget (list text spacer)
                                                     :direction :row
                                                     :gap 1))
         (box (ptui.widgets.core:make-box-widget stack :padding 1 :borderp t))
         (text-size (ptui.widgets.core:widget-measure text))
         (stack-size (ptui.widgets.core:widget-measure stack))
         (box-size (ptui.widgets.core:widget-measure box)))
    (assert-true (= (ptui.layout:layout-size-width text-size) 2)
                 "text width should follow text width policy")
    (assert-true (= (ptui.layout:layout-size-height text-size) 1)
                 "text height should be 1")
    (assert-true (= (ptui.layout:layout-size-width stack-size) 6)
                 "stack row width should include child widths + gap")
    (assert-true (= (ptui.layout:layout-size-height stack-size) 2)
                 "stack row height should match max child height")
    (assert-true (= (ptui.layout:layout-size-width box-size) 10)
                 "box width should include padding + border")
    (assert-true (= (ptui.layout:layout-size-height box-size) 6)
                 "box height should include padding + border")))

(deftest widgets-input-scroll-and-event-dispatch
  (let* ((captured '())
         (input (ptui.widgets.core:make-input-widget
                 "abc"
                 :id :input-1
                 :min-width 5
                 :on-event (lambda (event node)
                             (setf captured
                                   (list :key (ptui.core.events:key-event-key event)
                                         :id (ptui.ui.elements:ui-element-id node))))))
         (scroll (ptui.widgets.core:make-scroll-widget
                  (ptui.widgets.core:make-text-widget "1234567")
                  :viewport-width 4
                  :viewport-height 2))
         (root (ptui.widgets.core:make-stack-widget (list input scroll) :id :root))
         (runtime (ptui.ui.runtime:make-runtime))
         (input-size (ptui.widgets.core:widget-measure input))
         (scroll-size (ptui.widgets.core:widget-measure scroll)))
    (ptui.ui.runtime:update-runtime runtime root)
    (assert-true (= (ptui.layout:layout-size-width input-size) 5)
                 "input width should honor min-width")
    (assert-true (= (ptui.layout:layout-size-height input-size) 1)
                 "input height should be 1")
    (assert-true (= (ptui.layout:layout-size-width scroll-size) 4)
                 "scroll width should honor viewport width")
    (assert-true (= (ptui.layout:layout-size-height scroll-size) 2)
                 "scroll height should honor viewport height")
    (let ((route (ptui.ui.runtime:route-event runtime (ptui.core.events:make-key-event :enter))))
      (ptui.widgets.core:dispatch-widget-event root route))
    (assert-true (equal captured '(:key :enter :id :input-1))
                 "event dispatch should invoke target input handler, got ~S" captured)))

(deftest widgets-components-api-boundary
  (multiple-value-bind (widgets-sym widgets-status)
      (find-symbol "MAKE-PROMPT-BOX-WIDGET" :ptui.widgets.core)
    (assert-true (null widgets-sym)
                 "prompt-box constructor must not exist in ptui.widgets.core, got ~S/~S"
                 widgets-sym widgets-status))
  (multiple-value-bind (components-sym components-status)
      (find-symbol "MAKE-PROMPT-BOX-WIDGET" :ptui.components.prompt-box)
    (assert-true (and components-sym (eql components-status :external))
                 "prompt-box constructor should be exported by ptui.components.prompt-box, got ~S/~S"
                 components-sym components-status)
    (assert-true (fboundp components-sym)
                 "prompt-box constructor symbol should be fboundp: ~S"
                 components-sym)))

(deftest widgets-components-system-boundary
  (labels ((%dep-key (dep) (string-downcase (string dep))))
    (let* ((widgets (asdf:find-system "ptui/widgets"))
           (components (asdf:find-system "ptui/components"))
           (widgets-deps (mapcar #'%dep-key (asdf:system-depends-on widgets)))
           (components-deps (mapcar #'%dep-key (asdf:system-depends-on components))))
      (assert-true (not (member "ptui/components" widgets-deps :test #'string=))
                   "ptui/widgets must not depend on ptui/components, deps=~S"
                   widgets-deps)
      (assert-true (member "ptui/widgets" components-deps :test #'string=)
                   "ptui/components must depend on ptui/widgets, deps=~S"
                   components-deps))))

(deftest widgets-prompt-box-measure-contract
  (let* ((value (concatenate 'string
                             "a" (string #\Newline)
                             "b" (string #\Newline)
                             "c" (string #\Newline)
                             "d"))
         (prompt (ptui.components.prompt-box:make-prompt-box-widget
                  value
                  :id :prompt
                  :min-width 4
                  :max-width 6
                  :min-rows 1
                  :max-rows 2))
         (size (ptui.widgets.core:widget-measure prompt)))
    ;; Content width clamps to [min-width, max-width], plus 2 border cells.
    (assert-true (= (ptui.layout:layout-size-width size) 6)
                 "prompt-box width should be clamped to max-width+border, got ~D"
                 (ptui.layout:layout-size-width size))
    ;; Content rows clamp to max-rows, plus 2 border rows.
    (assert-true (= (ptui.layout:layout-size-height size) 4)
                 "prompt-box height should be max-rows+border, got ~D"
                 (ptui.layout:layout-size-height size))))

(deftest widgets-terminal-pane-api-boundary
  (multiple-value-bind (widgets-sym widgets-status)
      (find-symbol "MAKE-TERMINAL-PANE-WIDGET" :ptui.widgets.core)
    (assert-true (null widgets-sym)
                 "terminal-pane constructor must not exist in ptui.widgets.core, got ~S/~S"
                 widgets-sym widgets-status))
  (multiple-value-bind (components-sym components-status)
      (find-symbol "MAKE-TERMINAL-PANE-WIDGET" :ptui.components.terminal-pane)
    (assert-true (and components-sym (eql components-status :external))
                 "terminal-pane constructor should be exported by ptui.components.terminal-pane, got ~S/~S"
                 components-sym components-status)
    (assert-true (fboundp components-sym)
                 "terminal-pane constructor symbol should be fboundp: ~S"
                 components-sym)))

(deftest widgets-terminal-pane-buffering-and-scroll-contract
  (let* ((state (ptui.components.terminal-pane:make-terminal-pane-state
                 :title "build log"
                 :max-lines 3)))
    (ptui.components.terminal-pane:terminal-pane-append-output
     state
     (concatenate 'string "one" (string #\Newline)
                  "two" (string #\Newline)
                  "partial"))
    (assert-true (equal (ptui.components.terminal-pane:terminal-pane-lines state)
                        '("one" "two"))
                 "expected completed lines after first append, got ~S"
                 (ptui.components.terminal-pane:terminal-pane-lines state))
    (assert-true (string= (ptui.components.terminal-pane:terminal-pane-pending-output state)
                          "partial")
                 "expected trailing partial output to be retained")
    (ptui.components.terminal-pane:terminal-pane-append-output
     state
     (concatenate 'string "-line" (string #\Newline)
                  "three" (string #\Newline)
                  "four" (string #\Newline)))
    (assert-true (equal (ptui.components.terminal-pane:terminal-pane-lines state)
                        '("partial-line" "three" "four"))
                 "expected max-lines trim to keep newest entries, got ~S"
                 (ptui.components.terminal-pane:terminal-pane-lines state))
    (assert-true (string= (ptui.components.terminal-pane:terminal-pane-pending-output state) "")
                 "expected no pending partial output after newline-terminated append")
    (assert-true (equal (ptui.components.terminal-pane:terminal-pane-visible-lines
                         state
                         :viewport-height 2)
                        '("three" "four"))
                 "expected viewport tail when scroll-offset=0")
    (ptui.components.terminal-pane:terminal-pane-handle-event
     state
     (ptui.core.events:make-key-event :up)
     :viewport-height 2)
    (assert-true (= (ptui.components.terminal-pane:terminal-pane-scroll-offset state) 1)
                 "expected :up event to move one row into scrollback")
    (assert-true (equal (ptui.components.terminal-pane:terminal-pane-visible-lines
                         state
                         :viewport-height 2)
                        '("partial-line" "three"))
                 "expected scrollback window after one upward scroll")
    (ptui.components.terminal-pane:terminal-pane-handle-event
     state
     (ptui.core.events:make-key-event :end)
     :viewport-height 2)
    (assert-true (= (ptui.components.terminal-pane:terminal-pane-scroll-offset state) 0)
                 "expected :end event to jump back to live tail")))

(deftest widgets-terminal-pane-widget-composition
  (let* ((state (ptui.components.terminal-pane:make-terminal-pane-state
                 :title "terminal"
                 :lines '("alpha" "beta")
                 :pending-output "gamma"))
         (widget (ptui.components.terminal-pane:make-terminal-pane-widget
                  state
                  :id :terminal-pane
                  :viewport-height 3))
         (size (ptui.widgets.core:widget-measure widget)))
    (assert-true (> (ptui.layout:layout-size-width size) 0)
                 "terminal-pane widget width should be > 0")
    (assert-true (>= (ptui.layout:layout-size-height size) 3)
                 "terminal-pane widget height should include status + viewport rows")))

(deftest widgets-terminal-pane-ansi-search-and-copy
  (let* ((esc (string (code-char 27)))
         (state (ptui.components.terminal-pane:make-terminal-pane-state
                 :title "terminal"
                 :max-lines 16)))
    (ptui.components.terminal-pane:terminal-pane-append-output
     state
     (format nil "~A[31mERR~A[0m ok~%plain~%foo bar foo~%"
             esc
             esc))
    (assert-true (equal (ptui.components.terminal-pane:terminal-pane-lines state)
                        '("ERR ok" "plain" "foo bar foo"))
                 "expected ANSI escapes to render into plain lines, got ~S"
                 (ptui.components.terminal-pane:terminal-pane-lines state))
    (let* ((styled (ptui.components.terminal-pane:terminal-pane-visible-styled-lines
                    state
                    :viewport-height 3))
           (first-line (first styled))
           (first-segment (first first-line))
           (first-cell (second first-segment)))
      (assert-true (= (length first-line) 2)
                   "expected styled split for ANSI reset, got ~S"
                   first-line)
      (assert-true (string= (first first-segment) "ERR")
                   "expected first styled segment text ERR, got ~S"
                   first-segment)
      (assert-true (typep (ptui.core.types:cell-fg first-cell)
                          'ptui.core.color:color-rgb)
                   "expected ANSI fg color to materialize as color-rgb, got ~S"
                   (ptui.core.types:cell-fg first-cell)))
    (ptui.components.terminal-pane:terminal-pane-set-search-query state "foo")
    (assert-true (= (length (ptui.components.terminal-pane:terminal-pane-search-results state)) 2)
                 "expected two search matches for foo")
    (ptui.components.terminal-pane:terminal-pane-search-next state)
    (assert-true (= (ptui.components.terminal-pane:terminal-pane-search-selected-index state) 1)
                 "expected search-next to advance selection index")
    (assert-true (string= (ptui.components.terminal-pane:terminal-pane-copy-visible
                           state
                           :viewport-height 2)
                          "plain
foo bar foo")
                 "copy-visible should copy viewport rows")
    (assert-true (string= (ptui.components.terminal-pane:terminal-pane-copy-search-result state)
                          "foo bar foo")
                 "copy-search-result should copy selected match line")
    (let ((action
            (ptui.components.terminal-pane:terminal-pane-handle-event
             state
             (ptui.core.events:make-key-event :copy-visible)
             :viewport-height 2)))
      (assert-true (eq (getf action :action) :copied-visible)
                   "expected copy event action, got ~S"
                   action))))

(deftest widgets-terminal-pane-widget-exposes-ansi-segments
  (let* ((esc (string (code-char 27)))
         (state (ptui.components.terminal-pane:make-terminal-pane-state
                 :title "terminal"))
         (_ (ptui.components.terminal-pane:terminal-pane-append-output
             state
             (format nil "~A[33mwarn~A[0m~%" esc esc)))
         (widget (ptui.components.terminal-pane:make-terminal-pane-widget
                  state
                  :id :terminal-pane
                  :viewport-height 1))
         (content (first (ptui.ui.elements:ui-element-children widget)))
         (rows (ptui.ui.elements:ui-element-children content))
         (first-output (second rows))
         (segments (getf (ptui.ui.elements:ui-element-props first-output)
                         :styled-segments)))
    (declare (ignore _))
    (assert-true (and (listp segments) segments)
                 "expected terminal-pane output text to include :styled-segments metadata, got ~S"
                 segments)
    (assert-true (string= (first (first segments)) "warn")
                 "expected styled segment text warn, got ~S"
                 segments)))

(deftest widgets-terminal-pane-output-stream-metadata
  (let* ((state (ptui.components.terminal-pane:make-terminal-pane-state
                 :title "terminal"
                 :max-lines 4)))
    (ptui.components.terminal-pane:terminal-pane-append-output
     state
     (concatenate 'string "ok" (string #\Newline))
     :severity :info
     :style :stdout)
    (ptui.components.terminal-pane:terminal-pane-append-output
     state
     "warn-part"
     :severity :warning
     :style :stderr)
    (ptui.components.terminal-pane:terminal-pane-append-output
     state
     (concatenate 'string "-done" (string #\Newline)
                  "error!" (string #\Newline))
     :severity :error
     :style :stderr)
    (assert-true (equal (ptui.components.terminal-pane:terminal-pane-lines state)
                        '("ok" "warn-part-done" "error!"))
                 "expected append-only lines to include merged partial rows, got ~S"
                 (ptui.components.terminal-pane:terminal-pane-lines state))
    (let ((metadata (ptui.components.terminal-pane:terminal-pane-line-metadata state)))
      (assert-true (equal (mapcar (lambda (entry) (getf entry :severity)) metadata)
                          '(:info :error :error))
                   "expected merged severities for completed lines, got ~S"
                   metadata)
      (assert-true (equal (mapcar (lambda (entry) (getf entry :style)) metadata)
                          '(:stdout :stderr :stderr))
                   "expected per-line style metadata, got ~S"
                   metadata))
    (ptui.components.terminal-pane:terminal-pane-append-line
     state
     "debug-note"
     :severity :debug
     :style :system)
    (ptui.components.terminal-pane:terminal-pane-append-line
     state
     "critical-stop"
     :severity :critical
     :style :stderr)
    (assert-true (equal (ptui.components.terminal-pane:terminal-pane-lines state)
                        '("warn-part-done" "error!" "debug-note" "critical-stop"))
                 "expected max-lines trim to keep newest lines, got ~S"
                 (ptui.components.terminal-pane:terminal-pane-lines state))
    (let* ((visible-meta (ptui.components.terminal-pane:terminal-pane-visible-line-metadata
                          state
                          :viewport-height 2))
           (severities (mapcar (lambda (entry) (getf entry :severity)) visible-meta)))
      (assert-true (equal severities '(:debug :critical))
                   "expected visible metadata to align with viewport rows, got ~S"
                   visible-meta))
    (let* ((widget (ptui.components.terminal-pane:make-terminal-pane-widget
                    state
                    :id :terminal-pane
                    :viewport-height 2))
           (content (first (ptui.ui.elements:ui-element-children widget)))
           (rows (ptui.ui.elements:ui-element-children content))
           (first-output (second rows))
           (metadata (getf (ptui.ui.elements:ui-element-props first-output)
                           :metadata)))
      (assert-true (and (listp metadata)
                        (eq (getf metadata :severity) :debug)
                        (eq (getf metadata :style) :system))
                   "expected text widget metadata for first visible output row, got ~S"
                   metadata))))

(deftest widgets-glob-widget-api-boundary
  (multiple-value-bind (widgets-sym widgets-status)
      (find-symbol "MAKE-GLOB-WIDGET" :ptui.widgets.core)
    (assert-true (null widgets-sym)
                 "glob-widget constructor must not exist in ptui.widgets.core, got ~S/~S"
                 widgets-sym widgets-status))
  (multiple-value-bind (components-sym components-status)
      (find-symbol "MAKE-GLOB-WIDGET" :ptui.components.glob-widget)
    (assert-true (and components-sym (eql components-status :external))
                 "glob-widget constructor should be exported by ptui.components.glob-widget, got ~S/~S"
                 components-sym components-status)
    (assert-true (fboundp components-sym)
                 "glob-widget constructor symbol should be fboundp: ~S"
                 components-sym)))

(deftest widgets-glob-widget-streaming-and-cancellation
  (let* ((stream (ptui.components.glob-widget:make-sequence-glob-stream
                  '("src/main.lisp"
                    "README.md"
                    "src/util.lisp")))
         (state (ptui.components.glob-widget:make-glob-widget-state
                 :pattern "src/*.lisp"
                 :stream stream
                 :batch-size 1)))
    (multiple-value-bind (_ consumed matched)
        (ptui.components.glob-widget:glob-widget-step state)
      (declare (ignore _))
      (assert-true (= consumed 1)
                   "expected first stream step to consume one candidate, got ~D"
                   consumed)
      (assert-true (= matched 1)
                   "expected first stream step to match one candidate, got ~D"
                   matched)
      (assert-true (eq (ptui.components.glob-widget:glob-widget-status state) :streaming)
                   "expected status :streaming after first step, got ~S"
                   (ptui.components.glob-widget:glob-widget-status state)))
    (ptui.components.glob-widget:glob-widget-step state :max-items 8)
    (assert-true (equal (ptui.components.glob-widget:glob-widget-matches state)
                        '("src/main.lisp" "src/util.lisp"))
                 "expected deterministic match order from stream, got ~S"
                 (ptui.components.glob-widget:glob-widget-matches state))
    (assert-true (eq (ptui.components.glob-widget:glob-widget-status state) :done)
                 "expected status :done after draining stream, got ~S"
                 (ptui.components.glob-widget:glob-widget-status state))
    (ptui.components.glob-widget:glob-widget-start
     state
     (ptui.components.glob-widget:make-sequence-glob-stream
      '("notes/todo.txt" "notes/next.txt"))
     :pattern "notes/*.txt")
    (assert-true (eq (ptui.components.glob-widget:glob-widget-status state) :streaming)
                 "expected status :streaming after restart, got ~S"
                 (ptui.components.glob-widget:glob-widget-status state))
    (ptui.components.glob-widget:glob-widget-cancel state)
    (assert-true (eq (ptui.components.glob-widget:glob-widget-status state) :cancelled)
                 "expected status :cancelled after cancel, got ~S"
                 (ptui.components.glob-widget:glob-widget-status state))))

(deftest widgets-glob-widget-event-dispatch-and-selection
  (let* ((selected nil)
         (state (ptui.components.glob-widget:make-glob-widget-state
                 :pattern "src/*.lisp"
                 :on-select (lambda (match _state)
                              (declare (ignore _state))
                              (setf selected match))))
         (stream (ptui.components.glob-widget:make-sequence-glob-stream
                  '("src/main.lisp" "src/util.lisp")))
         (runtime (ptui.ui.runtime:make-runtime)))
    (ptui.components.glob-widget:glob-widget-start state stream)
    (ptui.components.glob-widget:glob-widget-step state :max-items 8)
    (let* ((widget (ptui.components.glob-widget:make-glob-widget
                    state
                    :id :glob-root
                    :input-id :glob-input))
           (root (ptui.widgets.core:make-stack-widget (list widget) :id :root))
           (size (ptui.widgets.core:widget-measure widget)))
      (ptui.ui.runtime:update-runtime runtime root)
      (assert-true (> (ptui.layout:layout-size-width size) 0)
                   "glob-widget composed width should be > 0")
      (assert-true (> (ptui.layout:layout-size-height size) 0)
                   "glob-widget composed height should be > 0")
      (ptui.widgets.core:dispatch-widget-event
       root
       (list :target :glob-input
             :event (ptui.core.events:make-key-event :down)))
      (assert-true (= (ptui.components.glob-widget:glob-widget-selected-index state) 1)
                   "expected :down event to move selection to second match")
      (ptui.widgets.core:dispatch-widget-event
       root
       (list :target :glob-input
             :event (ptui.core.events:make-key-event :enter)))
      (assert-true (string= selected "src/util.lisp")
                   "expected enter event to select highlighted match, got ~S"
                   selected)
      (ptui.widgets.core:dispatch-widget-event
       root
       (list :target :glob-input
             :event (ptui.core.events:make-key-event :text :text? "a")))
      (assert-true (string= (ptui.components.glob-widget:glob-widget-pattern state)
                            "src/*.lispa")
                   "expected text event to mutate pattern, got ~S"
                   (ptui.components.glob-widget:glob-widget-pattern state)))))

(deftest dashboard-ui-render-and-grapheme-backspace
  (let* ((state (ptui.examples.metrics-dashboard::make-dashboard-ui-state
                 :runtime (ptui.ui.runtime:make-runtime)))
         (size (ptui.core.types:make-size 50 16))
         (buf (ptui.examples.metrics-dashboard::%render-dashboard-ui state size))
         (text (buffer->flat-text buf))
         (grapheme (string-from-codepoints #x0065 #x0301)))
    (assert-true (typep buf 'ptui.core.types:cell-buffer)
                 "ui dashboard render should return a cell buffer")
    (assert-true (search "PTUI Metrics Dashboard [UI]" text)
                 "ui dashboard should render UI title")
    (assert-true (search "Event:" text)
                 "ui dashboard should render status text")
    (assert-true (>= (count #\╭ text) 2)
                 "ui dashboard should render nested box borders")
    (setf state (ptui.examples.metrics-dashboard::%on-dashboard-ui-event
                 state
                 (ptui.core.events:make-key-event :text :text? grapheme)))
    (assert-true (string= (ptui.examples.metrics-dashboard::dashboard-ui-state-input-text state)
                          grapheme)
                 "input text should contain inserted grapheme")
    (setf state (ptui.examples.metrics-dashboard::%on-dashboard-ui-event
                 state
                 (ptui.core.events:make-key-event :backspace)))
    (assert-true (string= (ptui.examples.metrics-dashboard::dashboard-ui-state-input-text state)
                          "")
                 "backspace should remove one grapheme cluster")))

(deftest dashboard-legacy-ui-parity-objective-signals
  (let* ((size (ptui.core.types:make-size 80 24))
         (legacy-buf (ptui.examples.metrics-dashboard::%render-dashboard-legacy nil size))
         (ui-state (ptui.examples.metrics-dashboard::make-dashboard-ui-state
                    :runtime (ptui.ui.runtime:make-runtime)))
         (ui-buf (ptui.examples.metrics-dashboard::%render-dashboard-ui ui-state size))
         (legacy-text (buffer->flat-text legacy-buf))
         (ui-text (buffer->flat-text ui-buf)))
    (assert-true (search "PTUI Metrics Dashboard" legacy-text)
                 "legacy dashboard should render title")
    (assert-true (search "PTUI Metrics Dashboard" ui-text)
                 "ui dashboard should render title")
    (assert-true (search "Ctrl-C" legacy-text)
                 "legacy dashboard should render quit hint")
    (assert-true (search "Ctrl-C" ui-text)
                 "ui dashboard should render quit hint")
    (assert-true (> (count #\* legacy-text) 10)
                 "legacy dashboard should render gradient row")
    (assert-true (> (count #\* ui-text) 10)
                 "ui dashboard should render gradient row")
    (assert-true (<= (buffer-max-content-width legacy-buf)
                     (ptui.core.types:size-cols size))
                 "legacy rendered content should stay within terminal width")
    (assert-true (<= (buffer-max-content-width ui-buf)
                     (ptui.core.types:size-cols size))
                 "ui rendered content should stay within terminal width")))

(deftest dashboard-ui-input-editing-behavior
  (let* ((state (ptui.examples.metrics-dashboard::make-dashboard-ui-state
                 :runtime (ptui.ui.runtime:make-runtime)))
         (size (ptui.core.types:make-size 50 16)))
    ;; Prime runtime tree/focus before dispatching key events.
    (ptui.examples.metrics-dashboard::%render-dashboard-ui state size)
    (setf state (ptui.examples.metrics-dashboard::%on-dashboard-ui-event
                 state
                 (ptui.core.events:make-key-event :text :text? "abc")))
    (let ((buf-after-input (ptui.examples.metrics-dashboard::%render-dashboard-ui state size)))
      (assert-true (search "abc" (buffer->flat-text buf-after-input))
                   "ui dashboard should display typed input text"))
    (setf state (ptui.examples.metrics-dashboard::%on-dashboard-ui-event
                 state
                 (ptui.core.events:make-key-event :backspace)))
    (let ((buf-after-backspace (ptui.examples.metrics-dashboard::%render-dashboard-ui state size))
          (legacy-buf (ptui.examples.metrics-dashboard::%render-dashboard-legacy nil size)))
      (assert-true (search "ab" (buffer->flat-text buf-after-backspace))
                   "ui dashboard should update input after backspace")
      (assert-true (not (search "Input:" (buffer->flat-text legacy-buf)))
                   "legacy dashboard should remain static and not expose ui input row"))))

(deftest dashboard-ui-ctrl-j-inserts-newline
  (let* ((state (ptui.examples.metrics-dashboard::make-dashboard-ui-state
                 :runtime (ptui.ui.runtime:make-runtime)))
         (size (ptui.core.types:make-size 80 20)))
    ;; Prime runtime tree/focus before dispatching key events.
    (ptui.examples.metrics-dashboard::%render-dashboard-ui state size)
    (setf state (ptui.examples.metrics-dashboard::%on-dashboard-ui-event
                 state
                 (ptui.core.events:make-key-event :text :text? "abc")))
    (setf state (ptui.examples.metrics-dashboard::%on-dashboard-ui-event
                 state
                 (ptui.core.events:make-key-event :ctrl-j :ctrlp t)))
    (setf state (ptui.examples.metrics-dashboard::%on-dashboard-ui-event
                 state
                 (ptui.core.events:make-key-event :text :text? "def")))
    (assert-true (string= (ptui.examples.metrics-dashboard::dashboard-ui-state-input-text state)
                          (concatenate 'string "abc" (string #\Newline) "def"))
                 "ctrl-j should insert newline in prompt input text")
    (let ((flat (buffer->flat-text (ptui.examples.metrics-dashboard::%render-dashboard-ui state size))))
      (assert-true (search "abc" flat)
                   "first prompt line should be rendered")
      (assert-true (search "def" flat)
                   "second prompt line should be rendered"))))

(deftest dashboard-ui-long-input-stays-inside-input-box
  (let* ((marker "LONGINPUTMARKER")
         (payload (with-output-to-string (out)
                    (loop repeat 16 do (write-string marker out))))
         (state (ptui.examples.metrics-dashboard::make-dashboard-ui-state
                 :runtime (ptui.ui.runtime:make-runtime)
                 :input-text payload
                 :last-event "dispatched: :TEXT"))
         (size (ptui.core.types:make-size 166 20))
         (buf (ptui.examples.metrics-dashboard::%render-dashboard-ui state size))
         (flat (buffer->flat-text buf))
         (line (loop with stream = (make-string-input-stream flat)
                     for row = (read-line stream nil nil)
                     while row
                     when (search marker row) do (return row))))
    (assert-true line
                 "expected a rendered row containing long input marker, got none")
    (assert-true (search (concatenate 'string "│" marker) line)
                 "input text should render inside the input row, got: ~S"
                 line)
    (assert-true (not (search (concatenate 'string "╰─" marker) line))
                 "input text must not overwrite bottom border, got: ~S"
                 line)
    (assert-true (<= (buffer-max-content-width buf)
                     (ptui.core.types:size-cols size))
                 "long input render should stay within terminal width")))

(deftest dashboard-ui-render-cache-short-circuit
  (let* ((state (ptui.examples.metrics-dashboard::make-dashboard-ui-state
                 :runtime (ptui.ui.runtime:make-runtime)))
         (size (ptui.core.types:make-size 60 18))
         (buf-1 (ptui.examples.metrics-dashboard::%render-dashboard-ui state size))
         (runtime (ptui.examples.metrics-dashboard::dashboard-ui-state-runtime state))
         (revision-after-first (ptui.ui.runtime:runtime-revision runtime))
         (buf-2 (ptui.examples.metrics-dashboard::%render-dashboard-ui state size)))
    (assert-true (eq buf-1 buf-2)
                 "unchanged UI frame should reuse cached buffer instance")
    (assert-true (= (ptui.ui.runtime:runtime-revision runtime) revision-after-first)
                 "unchanged UI frame should skip runtime update/reconcile")
    (setf state (ptui.examples.metrics-dashboard::%on-dashboard-ui-event
                 state
                 (ptui.core.events:make-key-event :text :text? "x")))
    (let ((buf-3 (ptui.examples.metrics-dashboard::%render-dashboard-ui state size)))
      (assert-true (not (eq buf-2 buf-3))
                   "input change should invalidate cached buffer")
      (assert-true (> (ptui.ui.runtime:runtime-revision runtime) revision-after-first)
                   "input change should trigger runtime update"))))

(deftest atop-collect-host-snapshot-parses-proc-and-sys
  (flet ((fake-read-lines (path)
           (cond
             ((string= path "/proc/stat")
              '("cpu  100 20 40 840 10 5 6 7"
                "cpu0 50 10 20 420 5 2 3 4"))
             ((string= path "/proc/meminfo")
              '("MemTotal:       16384 kB"
                "MemAvailable:    4096 kB"
                "SwapTotal:       2048 kB"
                "SwapFree:        1024 kB"))
             ((string= path "/proc/self/mounts")
              '("/dev/nvme0n1p2 / ext4 rw,relatime 0 0"
                "tmpfs /run tmpfs rw,nosuid,nodev 0 0"
                "proc /proc proc ro,nosuid,nodev,noexec,relatime 0 0"))
             ((string= path "/proc/diskstats")
              '("8 0 sda 100 0 200 0 300 0 400 0 5 600 0 0 0"
                "7 0 loop0 1 0 1 0 1 0 1 0 0 1 0 0 0"))
             ((string= path "/proc/net/dev")
              '("Inter-|   Receive                                                |  Transmit"
                " face |bytes    packets errs drop fifo frame compressed multicast|bytes    packets errs drop fifo colls carrier compressed"
                "  lo: 100 1 0 0 0 0 0 0 100 1 0 0 0 0 0 0"
                "eth0: 2048 22 0 0 0 0 0 0 4096 40 0 0 0 0 0 0"))
             ((string= path "/proc/net/snmp")
              '("Tcp: RtoAlgorithm RtoMin RtoMax MaxConn ActiveOpens PassiveOpens AttemptFails EstabResets CurrEstab InSegs OutSegs RetransSegs"
                "Tcp: 1 200 120000 -1 7 9 0 0 11 300 500 12"
                "Ip: Forwarding DefaultTTL InReceives InHdrErrors InAddrErrors ForwDatagrams InUnknownProtos InDiscards InDelivers OutRequests OutDiscards OutNoRoutes ReasmTimeout ReasmReqds ReasmOKs ReasmFails FragOKs FragFails FragCreates"
                "Ip: 1 64 901 0 0 0 0 0 777 888 0 0 0 0 0 0 0 0 0"))
             ((string= path "/etc/passwd")
              '("root:x:0:0:root:/root:/bin/bash"
                "alice:x:1000:1000:alice:/home/alice:/bin/bash"))
             ((string= path "/proc/111/stat")
              '("111 (python3) S 1 1 1 0 -1 0 0 0 0 0 120 30 0 0 20 0 1 0 100 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0"))
             ((string= path "/proc/111/status")
              '("Name: python3"
                "State: S (sleeping)"
                "Uid: 1000 1000 1000 1000"
                "VmRSS: 20480 kB"))
             ((string= path "/proc/111/cmdline")
              (list (format nil "python3~Cworker.py" #\Null)))
             ((string= path "/proc/222/stat")
              '("222 (sshd) R 1 1 1 0 -1 0 0 0 0 0 240 40 0 0 20 0 1 0 200 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0"))
             ((string= path "/proc/222/status")
              '("Name: sshd"
                "State: R (running)"
                "Uid: 0 0 0 0"
                "VmRSS: 1024 kB"))
             ((string= path "/proc/222/cmdline")
              '("sshd: root@pts/0"))
             ((string= path "/sys/class/net/eth0/speed")
              '("1000"))
             ((string= path "/sys/class/net/wlan0/speed")
              '("300"))
             ((string= path "/sys/block/sda/queue/rotational")
              '("1"))
             ((string= path "/sys/block/nvme0n1/queue/rotational")
              '("0"))
             (t '())))
         (fake-directory (pattern)
           (let ((text (namestring pattern)))
             (cond
               ((string= text "/sys/class/net/*/speed")
                (list #P"/sys/class/net/eth0/speed"
                      #P"/sys/class/net/wlan0/speed"))
               ((string= text "/sys/block/*/queue/rotational")
                (list #P"/sys/block/sda/queue/rotational"
                      #P"/sys/block/nvme0n1/queue/rotational"))
               ((string= text "/proc/[0-9]*/stat")
                (list #P"/proc/111/stat"
                      #P"/proc/222/stat"))
               (t '())))))
    (let* ((snapshot (ptui.examples.atop-dashboard:collect-host-snapshot
                      :read-lines-fn #'fake-read-lines
                      :directory-fn #'fake-directory
                      :now-ms-fn (lambda () 2468)))
           (cpu (ptui.examples.atop-dashboard::host-snapshot-cpu snapshot))
           (mem (ptui.examples.atop-dashboard::host-snapshot-memory snapshot))
           (fs (ptui.examples.atop-dashboard::host-snapshot-filesystem snapshot))
           (disk (ptui.examples.atop-dashboard::host-snapshot-disk snapshot))
           (net (ptui.examples.atop-dashboard::host-snapshot-network snapshot))
           (tcp (ptui.examples.atop-dashboard::host-snapshot-tcpip snapshot))
           (processes (ptui.examples.atop-dashboard::host-snapshot-processes snapshot))
           (proc-a (first processes))
           (proc-b (second processes)))
      (assert-true (= (ptui.examples.atop-dashboard::host-snapshot-timestamp-ms snapshot) 2468)
                   "snapshot timestamp should come from now-ms-fn")
      (assert-true (= (ptui.examples.atop-dashboard::cpu-counters-user cpu) 100)
                   "cpu user parse mismatch")
      (assert-true (= (ptui.examples.atop-dashboard::cpu-counters-system cpu) 40)
                   "cpu system parse mismatch")
      (assert-true (= (ptui.examples.atop-dashboard::memory-counters-total-kb mem) 16384)
                   "mem total parse mismatch")
      (assert-true (= (ptui.examples.atop-dashboard::memory-counters-available-kb mem) 4096)
                   "mem available parse mismatch")
      (assert-true (= (ptui.examples.atop-dashboard::filesystem-counters-mount-count fs) 3)
                   "mount count parse mismatch")
      (assert-true (= (ptui.examples.atop-dashboard::filesystem-counters-rw-mount-count fs) 2)
                   "rw mount count parse mismatch")
      (assert-true (string= (ptui.examples.atop-dashboard::filesystem-counters-root-device fs)
                            "/dev/nvme0n1p2")
                   "root device parse mismatch")
      (assert-true (string= (ptui.examples.atop-dashboard::filesystem-counters-root-fstype fs)
                            "ext4")
                   "root fs parse mismatch")
      (assert-true (= (ptui.examples.atop-dashboard::disk-counters-read-ios disk) 100)
                   "disk read io parse mismatch")
      (assert-true (= (ptui.examples.atop-dashboard::disk-counters-write-ios disk) 300)
                   "disk write io parse mismatch")
      (assert-true (= (ptui.examples.atop-dashboard::disk-counters-rotational-devices disk) 1)
                   "rotational device summary mismatch")
      (assert-true (= (ptui.examples.atop-dashboard::disk-counters-ssd-devices disk) 1)
                   "ssd device summary mismatch")
      (assert-true (= (ptui.examples.atop-dashboard::network-counters-iface-count net) 1)
                   "network interface count mismatch")
      (assert-true (= (ptui.examples.atop-dashboard::network-counters-rx-bytes net) 2048)
                   "network rx bytes parse mismatch")
      (assert-true (= (ptui.examples.atop-dashboard::network-counters-fastest-link-mbps net) 1000)
                   "network max link speed mismatch")
      (assert-true (= (ptui.examples.atop-dashboard::tcpip-counters-tcp-retrans-segs tcp) 12)
                   "tcp retrans parse mismatch")
      (assert-true (= (ptui.examples.atop-dashboard::tcpip-counters-ip-out-requests tcp) 888)
                   "ip out requests parse mismatch")
      (assert-true (= (length processes) 2)
                   "process snapshot count mismatch")
      (assert-true (= (ptui.examples.atop-dashboard::process-counters-pid proc-a) 111)
                   "process rows should be deterministic by pid")
      (assert-true (string= (ptui.examples.atop-dashboard::process-counters-user proc-a) "alice")
                   "process user lookup mismatch")
      (assert-true (string= (ptui.examples.atop-dashboard::process-counters-command proc-a)
                            "python3 worker.py")
                   "process command normalization mismatch")
      (assert-true (string= (ptui.examples.atop-dashboard::process-counters-state proc-b) "R")
                   "process state parse mismatch"))))

(deftest atop-build-model-computes-deltas-and-rates
  (let* ((previous (make-atop-snapshot-fixture
                    :timestamp-ms 1000
                    :cpu-user 10 :cpu-system 20 :cpu-idle 70 :cpu-iowait 0
                    :mem-total-kb 1000 :mem-avail-kb 300
                    :swap-total-kb 500 :swap-free-kb 200
                    :read-ios 100 :write-ios 50
                    :read-sectors 200 :write-sectors 100
                    :inflight 2 :io-ms 1000
                    :rotational 1 :ssd 1
                    :iface-count 2
                    :rx-bytes 10000 :tx-bytes 5000
                    :rx-packets 100 :tx-packets 50
                    :fastest-link-mbps 1000
                    :tcp-in-segs 100 :tcp-out-segs 100 :tcp-retrans-segs 5
                    :processes (list
                                (make-process-counters-fixture 10 :user "root" :state "R"
                                                               :cpu-total-ticks 100 :rss-kb 100
                                                               :command "sshd")
                                (make-process-counters-fixture 20 :user "alice" :state "S"
                                                               :cpu-total-ticks 200 :rss-kb 200
                                                               :command "python app.py"))))
         (current (make-atop-snapshot-fixture
                   :timestamp-ms 3000
                   :cpu-user 50 :cpu-system 40 :cpu-idle 110 :cpu-iowait 0
                   :mem-total-kb 1000 :mem-avail-kb 200
                   :swap-total-kb 500 :swap-free-kb 100
                   :read-ios 160 :write-ios 90
                   :read-sectors 600 :write-sectors 300
                   :inflight 4 :io-ms 1600
                   :rotational 1 :ssd 1
                   :iface-count 2
                   :rx-bytes 20240 :tx-bytes 9100
                   :rx-packets 140 :tx-packets 80
                   :fastest-link-mbps 1000
                   :tcp-in-segs 140 :tcp-out-segs 200 :tcp-retrans-segs 10
                   :processes (list
                               (make-process-counters-fixture 10 :user "root" :state "R"
                                                              :cpu-total-ticks 180 :rss-kb 120
                                                              :command "sshd")
                               (make-process-counters-fixture 20 :user "alice" :state "S"
                                                              :cpu-total-ticks 220 :rss-kb 240
                                                              :command "python app.py")
                               (make-process-counters-fixture 30 :user "bob" :state "D"
                                                              :cpu-total-ticks 50 :rss-kb 300
                                                              :command "postgres"))))
         (model (ptui.examples.atop-dashboard:build-atop-model previous current))
         (cpu (ptui.examples.atop-dashboard::atop-model-cpu model))
         (memory (ptui.examples.atop-dashboard::atop-model-memory model))
         (disk (ptui.examples.atop-dashboard::atop-model-disk model))
         (network (ptui.examples.atop-dashboard::atop-model-network model))
         (tcp (ptui.examples.atop-dashboard::atop-model-tcpip model))
         (processes (ptui.examples.atop-dashboard::atop-model-processes model)))
    (assert-near (ptui.examples.atop-dashboard::cpu-model-usage-pct cpu) 60.0 0.001
                 "cpu usage percent mismatch")
    (assert-near (ptui.examples.atop-dashboard::cpu-model-user-pct cpu) 40.0 0.001
                 "cpu user percent mismatch")
    (assert-near (ptui.examples.atop-dashboard::cpu-model-system-pct cpu) 20.0 0.001
                 "cpu system percent mismatch")
    (assert-true (= (ptui.examples.atop-dashboard::memory-model-used-kb memory) 800)
                 "memory used mismatch")
    (assert-near (ptui.examples.atop-dashboard::memory-model-used-pct memory) 80.0 0.001
                 "memory used percent mismatch")
    (assert-true (= (ptui.examples.atop-dashboard::memory-model-swap-used-kb memory) 400)
                 "swap used mismatch")
    (assert-near (ptui.examples.atop-dashboard::disk-model-read-iops disk) 30.0 0.001
                 "disk read iops mismatch")
    (assert-near (ptui.examples.atop-dashboard::disk-model-write-iops disk) 20.0 0.001
                 "disk write iops mismatch")
    (assert-near (ptui.examples.atop-dashboard::disk-model-read-kib-s disk) 100.0 0.001
                 "disk read throughput mismatch")
    (assert-near (ptui.examples.atop-dashboard::disk-model-write-kib-s disk) 50.0 0.001
                 "disk write throughput mismatch")
    (assert-near (ptui.examples.atop-dashboard::disk-model-busy-pct disk) 30.0 0.001
                 "disk busy percent mismatch")
    (assert-near (ptui.examples.atop-dashboard::network-model-rx-kib-s network) 5.0 0.001
                 "network rx rate mismatch")
    (assert-near (ptui.examples.atop-dashboard::network-model-tx-kib-s network) 2.0019531 0.01
                 "network tx rate mismatch")
    (assert-near (ptui.examples.atop-dashboard::network-model-rx-pps network) 20.0 0.001
                 "network rx pps mismatch")
    (assert-near (ptui.examples.atop-dashboard::network-model-tx-pps network) 15.0 0.001
                 "network tx pps mismatch")
    (assert-near (ptui.examples.atop-dashboard::tcpip-model-tcp-retrans-pct tcp) 5.0 0.001
                 "tcp retrans percent mismatch")
    (assert-true (= (length processes) 3)
                 "process model count mismatch")
    (assert-true (= (ptui.examples.atop-dashboard::process-model-pid (first processes)) 10)
                 "default process sort should be cpu desc then pid")
    (assert-near (ptui.examples.atop-dashboard::process-model-cpu-pct (first processes)) 40.0 0.001
                 "process cpu percent mismatch")
    (assert-near (ptui.examples.atop-dashboard::process-model-mem-pct (first processes)) 12.0 0.001
                 "process mem percent mismatch")
    (assert-true (= (ptui.examples.atop-dashboard::process-model-pid (third processes)) 30)
                 "new process with no prior sample should sort last by cpu")))

(deftest atop-render-displays-dense-panels-and-help-overlay
  (let* ((model (ptui.examples.atop-dashboard:build-atop-model
                 nil
                 (make-atop-snapshot-fixture
                  :timestamp-ms 4000
                  :cpu-user 10 :cpu-system 10 :cpu-idle 80 :cpu-iowait 0
                  :mem-total-kb 8192 :mem-avail-kb 4096
                  :swap-total-kb 2048 :swap-free-kb 1536
                  :mount-count 6 :rw-mount-count 4
                  :iface-count 2
                  :read-ios 100 :write-ios 80
                  :read-sectors 320 :write-sectors 240
                  :tcp-in-segs 123 :tcp-out-segs 456 :tcp-retrans-segs 7
                  :processes (list
                              (make-process-counters-fixture 400 :user "root" :state "S"
                                                             :cpu-total-ticks 500 :rss-kb 128
                                                             :command "dockerd")
                              (make-process-counters-fixture 1010 :user "alice" :state "R"
                                                             :cpu-total-ticks 1500 :rss-kb 512
                                                             :command "python worker.py")))))
         (state (ptui.examples.atop-dashboard:make-atop-dashboard-state
                 :model model
                 :snapshot (make-atop-snapshot-fixture :timestamp-ms 4000)
                 :refresh-ms 1000
                 :last-refresh-ms 5000
                 :show-help-p t
                 :collect-fn (lambda () (error "collect should not run during render test"))
                 :now-ms-fn (lambda () 5500)))
         (size (ptui.core.types:make-size 110 28))
         (buf (ptui.examples.atop-dashboard::%render-atop-dashboard state size))
         (flat (buffer->flat-text buf)))
    (assert-true (search "PTUI Atop Dashboard v1" flat)
                 "render should include atop header")
    (assert-true (search "CPU" flat) "render missing CPU panel")
    (assert-true (search "Memory" flat) "render missing Memory panel")
    (assert-true (search "Filesystem" flat) "render missing Filesystem panel")
    (assert-true (search "Disk I/O" flat) "render missing Disk I/O panel")
    (assert-true (search "Network" flat) "render missing Network panel")
    (assert-true (search "TCP/IP" flat) "render missing TCP/IP panel")
    (assert-true (search "Processes (2)" flat)
                 "render missing process table")
    (assert-true (search "PID    USER" flat)
                 "render missing process table header")
    (assert-true (search "q quit | p pause/resume | ? help" flat)
                 "render missing control hint line")
    (assert-true (not (search "%%" flat))
                 "atop render should show percent values with a single %")
    (assert-true (search "Controls" flat)
                 "help overlay should draw controls title")
    (assert-true (search "pause/resume refresh" flat)
                 "help overlay should include pause/resume control")))

(deftest atop-event-contract-toggle-pause-and-help
  (let* ((state (ptui.examples.atop-dashboard:make-atop-dashboard-state
                 :last-refresh-ms 999))
         (pause-event (ptui.core.events:make-key-event :text :text? "p"))
         (help-event (ptui.core.events:make-key-event :text :text? "?"))
         (hide-help-event (ptui.core.events:make-key-event :text :text? "h")))
    (setf state (ptui.examples.atop-dashboard::%on-atop-event state pause-event))
    (assert-true (ptui.examples.atop-dashboard::atop-dashboard-state-pausedp state)
                 "p should pause refresh")
    (assert-true (string= (ptui.examples.atop-dashboard::atop-dashboard-state-status-line state)
                          "paused by user")
                 "pause status line mismatch")
    (setf (ptui.examples.atop-dashboard::atop-dashboard-state-last-refresh-ms state) 1234)
    (setf state (ptui.examples.atop-dashboard::%on-atop-event state pause-event))
    (assert-true (not (ptui.examples.atop-dashboard::atop-dashboard-state-pausedp state))
                 "second p should resume refresh")
    (assert-true (= (ptui.examples.atop-dashboard::atop-dashboard-state-last-refresh-ms state) 0)
                 "resume should force immediate next refresh")
    (setf state (ptui.examples.atop-dashboard::%on-atop-event state help-event))
    (assert-true (ptui.examples.atop-dashboard::atop-dashboard-state-show-help-p state)
                 "? should enable help overlay")
    (setf state (ptui.examples.atop-dashboard::%on-atop-event state hide-help-event))
    (assert-true (not (ptui.examples.atop-dashboard::atop-dashboard-state-show-help-p state))
                 "h should disable help overlay")))

(deftest atop-render-process-detail-overlay
  (let* ((state (ptui.examples.atop-dashboard:make-atop-dashboard-state
                 :model (ptui.examples.atop-dashboard::make-atop-model
                         :processes (list
                                     (ptui.examples.atop-dashboard::make-process-model
                                      :pid 7 :user "root" :state "S"
                                      :cpu-pct 1.0 :mem-pct 0.2 :rss-kb 128
                                      :command "systemd")
                                     (ptui.examples.atop-dashboard::make-process-model
                                      :pid 42 :user "alice" :state "R"
                                      :cpu-pct 88.0 :mem-pct 12.0 :rss-kb 8192
                                      :command "python pipeline.py")))
                 :process-selected-index 0
                 :show-process-detail-p t
                 :collect-fn (lambda () (error "collect should not run during detail render test"))
                 :now-ms-fn (lambda () 1000)))
         (buf (ptui.examples.atop-dashboard::%render-atop-dashboard
               state
               (ptui.core.types:make-size 100 28)))
         (flat (buffer->flat-text buf)))
    (assert-true (search "Focused Process" flat)
                 "detail overlay title missing")
    (assert-true (search "PID 42" flat)
                 "detail overlay should show selected pid")
    (assert-true (search "python pipeline.py" flat)
                 "detail overlay should show selected command")))

(deftest atop-event-contract-process-table-interactions
  (let* ((state (ptui.examples.atop-dashboard:make-atop-dashboard-state
                 :model (ptui.examples.atop-dashboard::make-atop-model
                         :processes (list
                                     (ptui.examples.atop-dashboard::make-process-model
                                      :pid 100 :user "alice" :state "S"
                                      :cpu-pct 40.0 :mem-pct 2.0 :rss-kb 200
                                      :command "python a.py")
                                     (ptui.examples.atop-dashboard::make-process-model
                                      :pid 200 :user "root" :state "R"
                                      :cpu-pct 20.0 :mem-pct 20.0 :rss-kb 800
                                      :command "db")
                                     (ptui.examples.atop-dashboard::make-process-model
                                      :pid 50 :user "root" :state "S"
                                      :cpu-pct 1.0 :mem-pct 30.0 :rss-kb 900
                                      :command "cache")))))
         (down-event (ptui.core.events:make-key-event :down))
         (j-event (ptui.core.events:make-key-event :text :text? "j"))
         (up-event (ptui.core.events:make-key-event :up))
         (k-event (ptui.core.events:make-key-event :text :text? "k"))
         (sort-mem-event (ptui.core.events:make-key-event :text :text? "m"))
         (sort-pid-event (ptui.core.events:make-key-event :text :text? "n"))
         (detail-event (ptui.core.events:make-key-event :enter))
         (pause-event (ptui.core.events:make-key-event :text :text? "p"))
         (help-event (ptui.core.events:make-key-event :text :text? "?")))
    (setf state (ptui.examples.atop-dashboard::%on-atop-event state down-event))
    (setf state (ptui.examples.atop-dashboard::%on-atop-event state j-event))
    (assert-true (= (ptui.examples.atop-dashboard::atop-dashboard-state-process-selected-index state) 2)
                 "down + j should move selection by two")
    (setf state (ptui.examples.atop-dashboard::%on-atop-event state up-event))
    (setf state (ptui.examples.atop-dashboard::%on-atop-event state k-event))
    (assert-true (= (ptui.examples.atop-dashboard::atop-dashboard-state-process-selected-index state) 0)
                 "up + k should move selection back to zero")
    (setf state (ptui.examples.atop-dashboard::%on-atop-event state sort-mem-event))
    (assert-true (eql (ptui.examples.atop-dashboard::atop-dashboard-state-process-sort-key state) :mem)
                 "m should switch process sort key to :mem")
    (assert-true (= (ptui.examples.atop-dashboard::process-model-pid
                     (first (ptui.examples.atop-dashboard::atop-model-processes
                             (ptui.examples.atop-dashboard::atop-dashboard-state-model state))))
                    50)
                 "mem sort should move highest memory process first")
    (setf state (ptui.examples.atop-dashboard::%on-atop-event state sort-pid-event))
    (assert-true (eql (ptui.examples.atop-dashboard::atop-dashboard-state-process-sort-key state) :pid)
                 "n should switch process sort key to :pid")
    (assert-true (= (ptui.examples.atop-dashboard::process-model-pid
                     (first (ptui.examples.atop-dashboard::atop-model-processes
                             (ptui.examples.atop-dashboard::atop-dashboard-state-model state))))
                    50)
                 "pid sort should be deterministic ascending")
    (setf state (ptui.examples.atop-dashboard::%on-atop-event state detail-event))
    (assert-true (ptui.examples.atop-dashboard::atop-dashboard-state-show-process-detail-p state)
                 "enter should enable process detail overlay")
    (setf state (ptui.examples.atop-dashboard::%on-atop-event state pause-event))
    (setf state (ptui.examples.atop-dashboard::%on-atop-event state help-event))
    (assert-true (ptui.examples.atop-dashboard::atop-dashboard-state-pausedp state)
                 "pause key should still work when process interactions are active")
    (assert-true (ptui.examples.atop-dashboard::atop-dashboard-state-show-help-p state)
                 "help key should coexist with process interactions")
    (assert-true (ptui.examples.atop-dashboard::atop-dashboard-state-show-process-detail-p state)
                 "detail overlay should stay enabled after pause/help toggles")))

(deftest glob-matcher-wildcard-edge-vectors
  (let ((vectors
          '(("*.lisp" "main.lisp" t)
            ("*.lisp" "src/main.lisp" nil)
            ("**/*.lisp" "src/main.lisp" t)
            ("src/?ain.lisp" "src/main.lisp" t)
            ("src/?ain.lisp" "src/chain.lisp" nil)
            ("src/[mw]ain.lisp" "src/main.lisp" t)
            ("src/[mw]ain.lisp" "src/wain.lisp" t)
            ("src/[mw]ain.lisp" "src/xain.lisp" nil)
            ("src/{main,test}.lisp" "src/main.lisp" t)
            ("src/{main,test}.lisp" "src/test.lisp" t)
            ("src/{main,test}.lisp" "src/util.lisp" nil)
            ("docs/**" "docs/guides/setup.md" t)
            ("literal[abc" "literal[abc" t))))
    (dolist (entry vectors)
      (destructuring-bind (pattern path expected) entry
        (let ((actual (ptui.search.glob:glob-match-p pattern path)))
          (assert-true (eql actual expected)
                       "glob vector mismatch pattern=~S path=~S expected=~S actual=~S"
                       pattern path expected actual))))))

(deftest glob-scan-sorts-by-mtime-with-limit
  (let ((root (make-temp-directory "ptui-glob-order")))
    (unwind-protect
         (progn
           (write-text-file (merge-pathnames #P"src/oldest.lisp" root) "oldest")
           (sleep 1)
           (write-text-file (merge-pathnames #P"src/newer.lisp" root) "newer")
           (sleep 1)
           (write-text-file (merge-pathnames #P"src/newest.lisp" root) "newest")
           (let* ((result (ptui.search.glob:scan-glob-files "**/*.lisp"
                                                            :root root
                                                            :limit 2
                                                            :respect-gitignore nil))
                  (paths (glob-relative-paths result)))
             (assert-true (= (length paths) 2)
                          "expected 2 files after limit, got ~D"
                          (length paths))
             (assert-true (equal paths '("src/newest.lisp" "src/newer.lisp"))
                          "expected mtime-desc ordering, got ~S"
                          paths)))
      (delete-directory-tree-safe root))))

(deftest glob-scan-respects-gitignore-and-custom-ignore
  (let ((root (make-temp-directory "ptui-glob-ignore")))
    (unwind-protect
         (progn
           (write-text-file (merge-pathnames #P".gitignore" root)
                            "ignored-dir/
*.tmp
!visible/keep.tmp
")
           (write-text-file (merge-pathnames #P"ignored-dir/secret.lisp" root) "secret")
           (write-text-file (merge-pathnames #P"visible/show.lisp" root) "show")
           (write-text-file (merge-pathnames #P"visible/skip.tmp" root) "skip")
           (write-text-file (merge-pathnames #P"visible/keep.tmp" root) "keep")
           (let* ((result
                    (ptui.search.glob:scan-glob-files
                     "**/*"
                     :root root
                     :respect-gitignore t
                     :ignore-patterns '(".gitignore" "visible/show.lisp")))
                  (paths (sort (copy-list (glob-relative-paths result)) #'string<)))
             (assert-true (equal paths '("visible/keep.tmp"))
                          "ignore handling mismatch, got ~S" paths)))
      (delete-directory-tree-safe root))))

(deftest glob-scan-supports-cancel-and-stream-callback
  (let ((root (make-temp-directory "ptui-glob-cancel")))
    (unwind-protect
         (progn
           (write-text-file (merge-pathnames #P"src/a.lisp" root) "a")
           (write-text-file (merge-pathnames #P"src/b.lisp" root) "b")
           (write-text-file (merge-pathnames #P"src/c.lisp" root) "c")
           (write-text-file (merge-pathnames #P"src/d.lisp" root) "d")
           (let ((seen '())
                 (stop-p nil))
             (let ((result
                     (ptui.search.glob:scan-glob-files
                      "**/*.lisp"
                      :root root
                      :respect-gitignore nil
                      :on-match (lambda (entry)
                                  (push (ptui.search.glob:glob-entry-relative-path entry)
                                        seen)
                                  (when (>= (length seen) 2)
                                    (setf stop-p t)))
                      :cancel-fn (lambda ()
                                   stop-p))))
               (assert-true (ptui.search.glob:glob-scan-result-canceled-p result)
                            "expected scan to report canceled")
               (assert-true (>= (length seen) 1)
                            "expected at least one streamed match")
               (assert-true (< (length (glob-relative-paths result)) 4)
                            "expected cancellation before full traversal; got ~S"
                            (glob-relative-paths result)))))
      (delete-directory-tree-safe root))))

(deftest search-engine-file-ranking-priority
  (let* ((results (ptui.search.engine:rank-file-matches
                   "search"
                   '("search"
                     "search-core.lisp"
                     "src/tool-search-ui.md"
                     "docs/notes.txt")))
         (paths (mapcar #'ptui.search.engine:search-file-match-path results))
         (kinds (mapcar #'ptui.search.engine:search-file-match-kind results))
         (scores (mapcar #'ptui.search.engine:search-file-match-score results)))
    (assert-true (= (length results) 3)
                 "expected three ranked file matches, got ~D (~S)" (length results) paths)
    (assert-true (equal paths
                        '("search" "search-core.lisp" "src/tool-search-ui.md"))
                 "unexpected file ranking order: ~S" paths)
    (assert-true (equal kinds '(:exact :prefix :substring))
                 "unexpected ranking kinds: ~S" kinds)
    (assert-true (>= (first scores) (second scores) (third scores))
                 "expected monotonic descending scores, got ~S" scores)))

(deftest search-engine-file-no-results
  (let ((results (ptui.search.engine:rank-file-matches
                  "does-not-exist"
                  '("src/main.lisp" "README.md"))))
    (assert-true (null results)
                 "expected empty file result set, got ~S" results)))

(deftest search-engine-content-context-and-ranking
  (let* ((documents
           (list
            (ptui.search.engine:make-search-document
             :path "logs/old.txt"
             :content (format nil "alpha~%target needle~%omega~%"))
            (ptui.search.engine:make-search-document
             :path "logs/new.txt"
             :content (format nil "needle needle~%"))))
         (matches (ptui.search.engine:search-content-matches
                   "needle"
                   documents
                   :regex-mode nil
                   :before-context 1
                   :after-context 1))
         (first-match (first matches)))
    (assert-true (= (length matches) 3)
                 "expected three content matches, got ~D" (length matches))
    (assert-true (string= (ptui.search.engine:search-content-match-path first-match)
                          "logs/new.txt")
                 "expected top-ranked match in newest/high-density line, got ~S"
                 (ptui.search.engine:search-content-match-path first-match))
    (let ((context-match
            (find "logs/old.txt"
                  matches
                  :test #'string=
                  :key #'ptui.search.engine:search-content-match-path)))
      (assert-true context-match
                   "expected match entry for logs/old.txt")
      (assert-true (= (ptui.search.engine:search-content-match-line context-match) 2)
                   "expected logs/old.txt match on line 2")
      (assert-true (equal (ptui.search.engine:search-content-match-context-before context-match)
                          '((:line 1 :text "alpha")))
                   "expected one context-before line for logs/old.txt")
      (assert-true (equal (ptui.search.engine:search-content-match-context-after context-match)
                          '((:line 3 :text "omega")))
                   "expected one context-after line for logs/old.txt"))))

(deftest search-engine-content-multiline-toggle
  (let* ((documents
           (list
            (ptui.search.engine:make-search-document
             :path "src/example.txt"
             :content (format nil "alpha~%beta~%"))))
         (line-mode (ptui.search.engine:search-content-matches
                     "alpha\\s+beta"
                     documents
                     :regex-mode t
                     :multiline-mode nil))
         (multiline-mode (ptui.search.engine:search-content-matches
                          "alpha\\s+beta"
                          documents
                          :regex-mode t
                          :multiline-mode t)))
    (assert-true (null line-mode)
                 "expected no line-mode matches for newline-spanning pattern")
    (assert-true (= (length multiline-mode) 1)
                 "expected one multiline match, got ~D" (length multiline-mode))))

(deftest search-engine-content-no-results
  (let* ((documents
           (list
            (ptui.search.engine:make-search-document
             :path "src/example.lisp"
             :content (format nil "(defun hello ())~%"))))
         (matches (ptui.search.engine:search-content-matches
                   "not-found"
                   documents
                   :regex-mode nil)))
    (assert-true (null matches)
                 "expected empty content match list, got ~S" matches)))

(deftest widgets-search-widget-api-boundary
  (multiple-value-bind (widgets-sym widgets-status)
      (find-symbol "MAKE-SEARCH-WIDGET" :ptui.widgets.core)
    (assert-true (null widgets-sym)
                 "search-widget constructor must not exist in ptui.widgets.core, got ~S/~S"
                 widgets-sym widgets-status))
  (multiple-value-bind (components-sym components-status)
      (find-symbol "MAKE-SEARCH-WIDGET" :ptui.components.search-widget)
    (assert-true (and components-sym (eql components-status :external))
                 "search-widget constructor should be exported by ptui.components.search-widget, got ~S/~S"
                 components-sym components-status)
    (assert-true (fboundp components-sym)
                 "search-widget constructor symbol should be fboundp: ~S"
                 components-sym)))

(deftest widgets-search-widget-file-results-and-events
  (let* ((selected nil)
         (state (ptui.components.search-widget:make-search-widget-state
                 :mode :files
                 :query "src/"
                 :file-candidates '("README.md" "src/main.lisp" "src/search.lisp")
                 :visible-count 4
                 :limit 10
                 :on-select (lambda (result _state)
                              (declare (ignore _state))
                              (setf selected
                                    (ptui.search.engine:search-file-match-path result)))))
         (runtime (ptui.ui.runtime:make-runtime)))
    (assert-true (equal (mapcar #'ptui.search.engine:search-file-match-path
                                (ptui.components.search-widget:search-widget-results state))
                        '("src/main.lisp" "src/search.lisp"))
                 "unexpected initial file-search ordering: ~S"
                 (mapcar #'ptui.search.engine:search-file-match-path
                         (ptui.components.search-widget:search-widget-results state)))
    (let* ((widget (ptui.components.search-widget:make-search-widget
                    state
                    :id :search-root
                    :input-id :search-input))
           (root (ptui.widgets.core:make-stack-widget (list widget) :id :root))
           (size (ptui.widgets.core:widget-measure widget)))
      (ptui.ui.runtime:update-runtime runtime root)
      (assert-true (> (ptui.layout:layout-size-width size) 0)
                   "search-widget composed width should be > 0")
      (assert-true (> (ptui.layout:layout-size-height size) 0)
                   "search-widget composed height should be > 0")
      (ptui.widgets.core:dispatch-widget-event
       root
       (list :target :search-input
             :event (ptui.core.events:make-key-event :down)))
      (assert-true (= (ptui.components.search-widget:search-widget-selected-index state) 1)
                   "expected :down event to move selection to second result")
      (ptui.widgets.core:dispatch-widget-event
       root
       (list :target :search-input
             :event (ptui.core.events:make-key-event :enter)))
      (assert-true (string= selected "src/search.lisp")
                   "expected enter event to select highlighted path, got ~S"
                   selected)
      (ptui.widgets.core:dispatch-widget-event
       root
       (list :target :search-input
             :event (ptui.core.events:make-key-event :text :text? "x")))
      (assert-true (string= (ptui.components.search-widget:search-widget-query state)
                            "src/x")
                   "expected text event to mutate query, got ~S"
                   (ptui.components.search-widget:search-widget-query state))
      (assert-true (eq (ptui.components.search-widget:search-widget-status state) :empty)
                   "expected :empty status after no-match query, got ~S"
                   (ptui.components.search-widget:search-widget-status state))
      (ptui.widgets.core:dispatch-widget-event
       root
       (list :target :search-input
             :event (ptui.core.events:make-key-event :backspace)))
      (assert-true (string= (ptui.components.search-widget:search-widget-query state)
                            "src/")
                   "expected backspace to restore query, got ~S"
                   (ptui.components.search-widget:search-widget-query state))
      (assert-true (eq (ptui.components.search-widget:search-widget-status state) :ready)
                   "expected :ready status after restoring query, got ~S"
                   (ptui.components.search-widget:search-widget-status state)))))

(deftest widgets-search-widget-content-results-and-refresh
  (let* ((documents
           (list
            (ptui.search.engine:make-search-document
             :path "logs/old.txt"
             :content (format nil "alpha~%needle one~%omega~%"))
            (ptui.search.engine:make-search-document
             :path "logs/new.txt"
             :content (format nil "needle two~%needle three~%"))))
         (state
           (ptui.components.search-widget:make-search-widget-state
            :mode :content
            :query "needle"
            :documents documents
            :regex-mode nil
            :before-context 1
            :after-context 0
            :visible-count 1
            :limit 10)))
    (assert-true (= (length (ptui.components.search-widget:search-widget-results state)) 3)
                 "expected three content matches, got ~D"
                 (length (ptui.components.search-widget:search-widget-results state)))
    (assert-true (string= (ptui.search.engine:search-content-match-path
                           (first (ptui.components.search-widget:search-widget-results state)))
                          "logs/new.txt")
                 "expected highest ranked content match from logs/new.txt")
    (setf (ptui.components.search-widget:search-widget-selected-index state) 1)
    (let* ((all (ptui.components.search-widget:search-widget-results state))
           (visible (ptui.components.search-widget:search-widget-visible-results state)))
      (assert-true (= (length visible) 1)
                   "expected visible window size 1, got ~D" (length visible))
      (assert-true (eq (first visible) (second all))
                   "expected visible window to track selected index"))
    (ptui.components.search-widget:search-widget-set-documents
     state
     (list (ptui.search.engine:make-search-document
            :path "logs/single.txt"
            :content (format nil "needle once~%"))))
    (assert-true (= (length (ptui.components.search-widget:search-widget-results state)) 1)
                 "expected one match after corpus refresh, got ~D"
                 (length (ptui.components.search-widget:search-widget-results state)))
    (setf (ptui.components.search-widget:search-widget-query state) "missing")
    (assert-true (eq (ptui.components.search-widget:search-widget-status state) :empty)
                 "expected :empty status for missing query, got ~S"
                 (ptui.components.search-widget:search-widget-status state))
    (setf (ptui.components.search-widget:search-widget-query state) "needle")
    (assert-true (eq (ptui.components.search-widget:search-widget-status state) :ready)
                 "expected :ready status after restoring query, got ~S"
                 (ptui.components.search-widget:search-widget-status state))))

;; Script entry
(multiple-value-bind (passed failed) (run-all-tests)
  (declare (ignore passed))
  (uiop:quit (if (zerop failed) 0 1)))
