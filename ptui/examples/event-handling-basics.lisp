(defpackage :ptui.examples.event-handling-basics
  (:use :cl)
  (:export #:main))

(in-package :ptui.examples.event-handling-basics)

(defstruct (example-state
            (:constructor make-example-state (&key (count 0) (last-event "none"))))
  (count 0 :type fixnum)
  (last-event "none" :type string))

(defun %event-description (event)
  (let ((key (ptui.core.events:key-event-key event)))
    (cond
      ((eql key :text)
       (format nil "text ~S" (ptui.core.events:key-event-text? event)))
      (t
       (string-downcase (symbol-name key))))))

(defun %handle-event (state event)
  (let ((key (ptui.core.events:key-event-key event)))
    (cond
      ((eql key :up)
       (make-example-state
        :count (1+ (example-state-count state))
        :last-event (%event-description event)))
      ((eql key :down)
       (make-example-state
        :count (1- (example-state-count state))
        :last-event (%event-description event)))
      ((and (eql key :text)
            (string-equal (or (ptui.core.events:key-event-text? event) "") "r"))
       (values (make-example-state :count 0 :last-event "reset") :consume))
      (t
       (make-example-state
        :count (example-state-count state)
        :last-event (%event-description event))))))

(defun %render-event-demo (state size)
  (let* ((current (or state (make-example-state)))
         (cols (ptui.core.types:size-cols size))
         (rows (ptui.core.types:size-rows size))
         (buf (ptui.render.buffer:make-buffer cols rows))
         (panel (ptui.core.types:make-rect 1 1
                                           (max 2 (- cols 2))
                                           (max 2 (- rows 2))))
         (title-cell (ptui.core.types:make-cell
                      " "
                      (ptui.core.color:make-color-rgb 255 210 140)
                      :default
                      (ptui.core.types:make-attrs :boldp t))))
    (ptui.render.buffer:buffer-draw-border buf panel)
    (ptui.render.buffer:buffer-draw-text
     buf 3 2
     (list (list "PTUI event handling basics" title-cell)))
    (ptui.render.buffer:buffer-draw-text
     buf 3 4
     "Up increments, Down decrements, r resets, q quits.")
    (ptui.render.buffer:buffer-draw-text
     buf 3 6
     (format nil "Count: ~D" (example-state-count current)))
    (ptui.render.buffer:buffer-draw-text
     buf 3 7
     (format nil "Last event: ~A" (example-state-last-event current)))
    buf))

(defun main (&rest argv)
  (declare (ignore argv))
  (ptui.engine.loop:run #'%render-event-demo
                        :backend :auto
                        :fps 20
                        :initial-state (make-example-state)
                        :on-event #'%handle-event))
