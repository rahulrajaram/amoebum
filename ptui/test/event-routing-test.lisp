(defpackage :ptui.test.event-routing
  (:use :cl :fiveam)
  (:export #:run-all #:event-routing-suite))

(in-package :ptui.test.event-routing)

(def-suite event-routing-suite
  :description "PTUI event routing: bubbling, capture, use-event-map (I278-I279).")

(in-suite event-routing-suite)

(defun %make-key-event (key &key text shiftp)
  (ptui.core.events:make-key-event
   key
   :text? text
   :shiftp (or shiftp nil)))

(test dispatch-widget-event-basic-non-bubble
  (let* ((handled nil)
         (child (ptui.ui.elements:make-element
                 :input
                 :id "child-1"
                 :props (list :on-event (lambda (ev node)
                                          (declare (ignore ev node))
                                          (setf handled t)))))
         (root (ptui.ui.elements:make-element
                :stack
                :id "root"
                :children (list child)))
         (route (list :kind :key :target "child-1"
                      :event (%make-key-event :enter))))
    (ptui.widgets.core:dispatch-widget-event root route)
    (is-true handled)))

(test dispatch-widget-event-bubble-propagates
  (let* ((log '())
         (child (ptui.ui.elements:make-element
                 :input
                 :id "child-1"
                 :props (list :on-event (lambda (ev node)
                                          (declare (ignore ev node))
                                          (push :child log)
                                          :bubble))))
         (root (ptui.ui.elements:make-element
                :stack
                :id "root"
                :props (list :on-event (lambda (ev node)
                                         (declare (ignore ev node))
                                         (push :root log)
                                         :handled))
                :children (list child)))
         (route (list :kind :key :target "child-1"
                      :event (%make-key-event :enter))))
    (ptui.widgets.core:dispatch-widget-event root route :bubble t)
    (is (equal (reverse log) '(:child :root)))))

(test dispatch-widget-event-bubble-stops-on-consume
  (let* ((log '())
         (child (ptui.ui.elements:make-element
                 :input
                 :id "child-1"
                 :props (list :on-event (lambda (ev node)
                                          (declare (ignore ev node))
                                          (push :child log)
                                          :consumed))))
         (root (ptui.ui.elements:make-element
                :stack
                :id "root"
                :props (list :on-event (lambda (ev node)
                                         (declare (ignore ev node))
                                         (push :root log)
                                         :handled))
                :children (list child)))
         (route (list :kind :key :target "child-1"
                      :event (%make-key-event :enter))))
    (ptui.widgets.core:dispatch-widget-event root route :bubble t)
    ;; Root handler should NOT have run
    (is (equal log '(:child)))))

(test dispatch-widget-event-capture-phase
  (let* ((log '())
         (child (ptui.ui.elements:make-element
                 :input
                 :id "child-1"
                 :props (list :on-event (lambda (ev node)
                                          (declare (ignore ev node))
                                          (push :child log)
                                          :handled)
                              :on-event-capture (lambda (ev node)
                                                  (declare (ignore ev node))
                                                  (push :child-capture log)
                                                  nil))))
         (root (ptui.ui.elements:make-element
                :stack
                :id "root"
                :props (list :on-event-capture (lambda (ev node)
                                                 (declare (ignore ev node))
                                                 (push :root-capture log)
                                                 nil))
                :children (list child)))
         (route (list :kind :key :target "child-1"
                      :event (%make-key-event :enter))))
    (ptui.widgets.core:dispatch-widget-event root route :bubble t)
    ;; Capture runs top-down, then bubble bottom-up
    (is (member :root-capture log))
    (is (member :child log))))

(test capture-phase-can-consume-event
  (let* ((log '())
         (child (ptui.ui.elements:make-element
                 :input
                 :id "child-1"
                 :props (list :on-event (lambda (ev node)
                                          (declare (ignore ev node))
                                          (push :child log)
                                          :handled))))
         (root (ptui.ui.elements:make-element
                :stack
                :id "root"
                :props (list :on-event-capture (lambda (ev node)
                                                 (declare (ignore ev node))
                                                 (push :root-capture log)
                                                 :consumed))
                :children (list child)))
         (route (list :kind :key :target "child-1"
                      :event (%make-key-event :enter))))
    (ptui.widgets.core:dispatch-widget-event root route :bubble t)
    ;; Capture consumed — child handler should NOT run
    (is (equal log '(:root-capture)))))

(test use-event-map-generates-handler
  (let ((handler (ptui.ui.hooks:use-event-map test-map
                   (:enter (values :enter-handled))
                   (:any (values :fallback)))))
    (is (functionp handler))
    ;; Test with enter key
    (let ((result (funcall handler (%make-key-event :enter) nil)))
      (is (eq result :enter-handled)))
    ;; Test with other key
    (let ((result (funcall handler (%make-key-event :backspace) nil)))
      (is (eq result :fallback)))))

(defun run-all ()
  (run! 'event-routing-suite))
