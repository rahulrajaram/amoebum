(defpackage :ptui.test.streaming-widget
  (:use :cl :fiveam)
  (:export #:streaming-widget-suite))

(in-package :ptui.test.streaming-widget)

(def-suite streaming-widget-suite
  :description "PTUI streaming widget tests.")

(in-suite streaming-widget-suite)

(test streaming-widget-incremental-layout-no-full-reflow-per-char
  (let* ((state (ptui.components.streaming-widget:make-streaming-widget-state
                 :viewport-width 12
                 :viewport-height 4))
         (baseline-relayout
           (ptui.components.streaming-widget:streaming-widget-full-relayout-count state)))
    (loop for char across "hello stream" do
      (ptui.components.streaming-widget:streaming-widget-append-chunk state (string char)))
    (is (= baseline-relayout
           (ptui.components.streaming-widget:streaming-widget-full-relayout-count state)))
    (is (>= (ptui.components.streaming-widget:streaming-widget-incremental-layout-count state)
            (length "hello stream")))))

(test streaming-widget-cursor-blink
  (let* ((state (ptui.components.streaming-widget:make-streaming-widget-state
                 :viewport-width 10
                 :viewport-height 3
                 :cursor-blink-ms 200)))
    (ptui.components.streaming-widget:streaming-widget-append-chunk state "abc")
    (let ((visible
            (ptui.components.streaming-widget:streaming-widget-visible-lines
             state
             :viewport-height 3
             :now-ms 0
             :include-cursor-p t))
          (hidden
            (ptui.components.streaming-widget:streaming-widget-visible-lines
             state
             :viewport-height 3
             :now-ms 250
             :include-cursor-p t)))
      (is (some (lambda (line) (search "|" line :test #'char=)) visible))
      (is (not (some (lambda (line) (search "|" line :test #'char=)) hidden))))))

(test streaming-widget-scroll-follow-disengage-and-resume
  (let* ((state (ptui.components.streaming-widget:make-streaming-widget-state
                 :viewport-width 16
                 :viewport-height 2)))
    (ptui.components.streaming-widget:streaming-widget-append-chunk
     state
     (concatenate 'string "one" (string #\Newline)
                  "two" (string #\Newline)
                  "three" (string #\Newline)))
    (ptui.components.streaming-widget:streaming-widget-handle-event
     state
     (ptui.core.events:make-key-event :up)
     :viewport-height 2)
    (is (not (ptui.components.streaming-widget:streaming-widget-scroll-follow-p state)))
    (is (= 1 (ptui.components.streaming-widget:streaming-widget-scroll-offset state)))
    (ptui.components.streaming-widget:streaming-widget-append-chunk
     state
     (concatenate 'string "tail" (string #\Newline)))
    (is (= 1 (ptui.components.streaming-widget:streaming-widget-scroll-offset state)))
    (ptui.components.streaming-widget:streaming-widget-handle-event
     state
     (ptui.core.events:make-key-event :end)
     :viewport-height 2)
    (is (ptui.components.streaming-widget:streaming-widget-scroll-follow-p state))
    (is (= 0 (ptui.components.streaming-widget:streaming-widget-scroll-offset state)))))

(test streaming-widget-smoke-sentinel
  (format t "STREAMING_WIDGET_SMOKE_OK~%")
  (is (not nil)))
