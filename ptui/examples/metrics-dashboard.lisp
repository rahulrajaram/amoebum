(defpackage :ptui.examples.metrics-dashboard
  (:use :cl)
  (:export #:main #:main-legacy #:main-ui))

(in-package :ptui.examples.metrics-dashboard)

(defun %template-cell (&key (fg :default) (bg :default) (boldp nil))
  (ptui.core.types:make-cell
   " "
   fg
   bg
   (ptui.core.types:make-attrs :boldp boldp)))

(defun %safe-inner-rect (cols rows)
  (ptui.core.types:make-rect
   1
   1
   (max 2 (- cols 2))
   (max 2 (- rows 2))))

(defun %fit-line (text)
  ;; Exercise the width-safe text pipeline without changing output semantics.
  (ptui.text.layout:truncate-to-width text (ptui.text.width:string-width text)))

(defun %fit-line-width (text width)
  (ptui.text.layout:truncate-to-width text (max 0 width)))

(defun %gradient-text (width)
  (make-string (max 0 width) :initial-element #\*))

(defun %draw-gradient-cells (buf x y width)
  (loop for i from 0 below (max 0 width) do
    (let* ((ratio (if (> (max 0 width) 1)
                      (/ i (float (1- width)))
                    0.0))
           (r (round (* 255 ratio)))
           (g (round (* 220 (- 1.0 ratio))))
           (b (round (* 255 (- 1.0 ratio))))
           (cell (ptui.core.types:make-cell
                  "*"
                  (ptui.core.color:make-color-rgb r g b)
                  :default
                  (ptui.core.types:make-attrs))))
      (ptui.render.buffer:buffer-draw-text buf (+ x i) y cell))))

(defun %render-dashboard-legacy (state size)
  (declare (ignore state))
  (let* ((cols (ptui.core.types:size-cols size))
         (rows (ptui.core.types:size-rows size))
         (buf (ptui.render.buffer:make-buffer cols rows))
         (panel (%safe-inner-rect cols rows))
         (title-cell (%template-cell :fg (ptui.core.color:make-color-rgb 120 200 255) :boldp t))
         (muted-cell (%template-cell :fg (ptui.core.color:make-color-rgb 180 180 180)))
         (info-y 4)
         (bar-y (max 6 (- rows 3)))
         (bar-x 3)
         (bar-w (max 0 (- cols 6))))
    (ptui.render.buffer:buffer-draw-border buf panel)
    (ptui.render.buffer:buffer-draw-text
     buf 3 2
     (list (list (%fit-line "PTUI Metrics Dashboard") title-cell)))
    (ptui.render.buffer:buffer-draw-text
     buf 3 info-y
     (list (list (%fit-line (format nil "Terminal size: ~Dx~D" cols rows)) muted-cell)))
    (ptui.render.buffer:buffer-draw-text
     buf 3 (1+ info-y)
     (list (list (%fit-line "Press q or Ctrl-C to quit") muted-cell)))
    (%draw-gradient-cells buf bar-x bar-y bar-w)
    buf))

(defstruct (dashboard-ui-state
            (:constructor make-dashboard-ui-state
                (&key runtime
                      (input-text "")
                      (last-event "none")
                      (frame-count 0))))
  runtime
  (input-text "" :type string)
  (last-event "none" :type string)
  (frame-count 0 :type fixnum))

(defun %ensure-ui-state (state)
  (if (and state (typep state 'dashboard-ui-state))
      state
      (make-dashboard-ui-state :runtime (ptui.ui.runtime:make-runtime))))

(defun %pop-last-grapheme (text)
  (let ((clusters (ptui.text.grapheme:split-graphemes text)))
    (if (null clusters)
        ""
        (with-output-to-string (out)
          (dolist (cluster (butlast clusters))
            (write-string cluster out))))))

(defun %element-prop (element key &optional default)
  (getf (ptui.ui.elements:ui-element-props element) key default))

(defun %element-id (element)
  (or (ptui.ui.elements:ui-element-id element)
      (ptui.ui.elements:ui-element-key element)))

(defun %ui-tree-node (element)
  (let ((id (%element-id element))
        (type (ptui.ui.elements:ui-element-type element))
        (children (mapcar #'%ui-tree-node
                          (ptui.ui.elements:ui-element-children element))))
    (ptui.layout:make-layout-node
     :id id
     :direction (if (eql type :stack)
                    (%element-prop element :direction :column)
                    :column)
     :gap (if (eql type :stack)
              (%element-prop element :gap 0)
              0)
     :measure (lambda (avail-width avail-height)
                (declare (ignore avail-width avail-height))
                (ptui.widgets.core:widget-measure element))
     :children children)))

(defun %ui-line-cell (id focus-id)
  (cond
    ((eql id :ui-title)
     (%template-cell :fg (ptui.core.color:make-color-rgb 120 200 255) :boldp t))
    ((eql id focus-id)
     (%template-cell :fg (ptui.core.color:make-color-rgb 150 230 150) :boldp t))
    ((eql id :ui-status)
     (%template-cell :fg (ptui.core.color:make-color-rgb 180 180 180)))
    (t
     (%template-cell :fg (ptui.core.color:make-color-rgb 200 200 200)))))

(defun %render-ui-element (buf element layout focus-id &key (dx 0) (dy 0))
  (let* ((id (%element-id element))
         (bounds (and id (ptui.layout:layout-bound layout id))))
    (when bounds
      (let* ((kind (ptui.ui.elements:ui-element-type element))
             (x (+ dx (ptui.layout:layout-bounds-x bounds)))
             (y (+ dy (ptui.layout:layout-bounds-y bounds)))
             (w (ptui.layout:layout-bounds-width bounds))
             (h (ptui.layout:layout-bounds-height bounds))
             (rect (ptui.core.types:make-rect x y w h)))
        (labels ((render-children (&key (child-dx dx) (child-dy dy) (clip-rect rect))
                   (ptui.render.buffer:with-clip (buf clip-rect)
                     (dolist (child (ptui.ui.elements:ui-element-children element))
                       (%render-ui-element buf child layout focus-id :dx child-dx :dy child-dy)))))
          (case kind
            (:text
             (let* ((text (%element-prop element :text ""))
                    (line (%fit-line-width text w)))
               (ptui.render.buffer:buffer-draw-text
                buf x y (list (list line (%ui-line-cell id focus-id))) :max-width w)))
            (:input
             (let* ((value (%element-prop element :value ""))
                    (line (%fit-line-width value w)))
               (ptui.render.buffer:buffer-draw-text
                buf x y (list (list line (%ui-line-cell id focus-id))) :max-width w)))
            (:spacer
             nil)
            (:box
             (let* ((padding (%element-prop element :padding 0))
                    (borderp (%element-prop element :borderp nil))
                    (border (if borderp 1 0))
                    (inset (+ border padding))
                    (inner-rect (ptui.core.types:make-rect
                                 (+ x inset)
                                 (+ y inset)
                                 (max 0 (- w (* 2 inset)))
                                 (max 0 (- h (* 2 inset)))))
                    (child (first (ptui.ui.elements:ui-element-children element))))
               (when borderp
                 (ptui.render.buffer:buffer-draw-border buf rect))
               (when child
                 (let* ((child-id (%element-id child))
                        (child-bounds (and child-id (ptui.layout:layout-bound layout child-id))))
                   (when child-bounds
                     (let ((delta-x (- (ptui.core.types:rect-x inner-rect)
                                       (ptui.layout:layout-bounds-x child-bounds)))
                           (delta-y (- (ptui.core.types:rect-y inner-rect)
                                       (ptui.layout:layout-bounds-y child-bounds))))
                       (%render-ui-element buf child layout focus-id
                                           :dx (+ dx delta-x)
                                           :dy (+ dy delta-y))))))))
            (:scroll
             (let* ((offset (%element-prop element :offset 0))
                    (child (first (ptui.ui.elements:ui-element-children element))))
               (when child
                 (ptui.render.buffer:with-clip (buf rect)
                   (%render-ui-element buf child layout focus-id
                                       :dx dx
                                       :dy (- dy offset))))))
            (otherwise
             (render-children))))))))

(defun %build-ui-tree (state cols rows)
  (declare (ignore rows))
  (let* ((inner-width (max 0 (- cols 10)))
         (gradient (%gradient-text inner-width))
         (title (ptui.widgets.core:make-text-widget
                 "PTUI Metrics Dashboard [UI]"
                 :id :ui-title))
         (info (ptui.widgets.core:make-text-widget
                (format nil "Terminal width: ~D" cols)
                :id :ui-info))
         (hint (ptui.widgets.core:make-text-widget
                "Tab focuses input. Type to edit. q/Ctrl-C quits."
                :id :ui-hint))
         (input (ptui.widgets.core:make-input-widget
                 (dashboard-ui-state-input-text state)
                 :id :ui-input
                 :min-width 18
                 :on-event (lambda (event node)
                             (declare (ignore node))
                             (setf (dashboard-ui-state-last-event state)
                                   (format nil "dispatched: ~S"
                                           (and (typep event 'ptui.core.events:key-event)
                                                (ptui.core.events:key-event-key event)))))))
         (input-box (ptui.widgets.core:make-box-widget
                     input
                     :id :ui-input-box
                     :padding 0
                     :borderp t))
         (spacer (ptui.widgets.core:make-spacer-widget 0 0 :id :ui-spacer))
         (bar (ptui.widgets.core:make-text-widget gradient :id :ui-bar))
         (status (ptui.widgets.core:make-text-widget
                  (format nil "Event: ~A" (dashboard-ui-state-last-event state))
                  :id :ui-status))
         (status-scroll (ptui.widgets.core:make-scroll-widget
                         status
                         :id :ui-status-scroll
                         :viewport-width inner-width
                         :viewport-height 1
                         :offset 0))
         (content (ptui.widgets.core:make-stack-widget
                   (list title info hint input-box spacer bar status-scroll)
                   :id :ui-content
                   :direction :column
                   :gap 0)))
    (ptui.widgets.core:make-box-widget
     content
     :id :ui-root
     :padding 0
     :borderp t)))

(defun %render-dashboard-ui (state size)
  (let* ((ui-state (%ensure-ui-state state))
         (runtime (dashboard-ui-state-runtime ui-state))
         (cols (ptui.core.types:size-cols size))
         (rows (ptui.core.types:size-rows size))
         (buf (ptui.render.buffer:make-buffer cols rows))
         (panel (%safe-inner-rect cols rows))
         (tree (%build-ui-tree ui-state cols rows))
         (layout-node (%ui-tree-node tree))
         (layout (ptui.layout:compute-layout
                  layout-node
                  :x 3
                  :y 2
                  :width (max 0 (- cols 6))
                  :height (max 0 (- rows 4))))
         (focus-id nil))
    (incf (dashboard-ui-state-frame-count ui-state))
    (ptui.ui.runtime:update-runtime runtime tree)
    (setf focus-id (ptui.ui.runtime:runtime-focus-id runtime))
    (ptui.render.buffer:buffer-draw-border buf panel)
    (%render-ui-element buf tree layout focus-id)
    buf))

(defun %on-dashboard-ui-event (state event)
  (let ((ui-state (%ensure-ui-state state)))
    (let* ((runtime (dashboard-ui-state-runtime ui-state))
           (route (ptui.ui.runtime:route-event runtime event))
           (target (getf route :target))
           (kind (getf route :kind)))
      (setf (dashboard-ui-state-last-event ui-state)
            (format nil "~A/~A" kind target))
      (when (and (eql kind :key)
                 (typep event 'ptui.core.events:key-event)
                 (eql target :ui-input))
        (let ((key (ptui.core.events:key-event-key event))
              (text (ptui.core.events:key-event-text? event)))
          (cond
            ((and (eql key :text) (stringp text))
             (setf (dashboard-ui-state-input-text ui-state)
                   (concatenate 'string
                                (dashboard-ui-state-input-text ui-state)
                                text)))
            ((eql key :backspace)
             (setf (dashboard-ui-state-input-text ui-state)
                   (%pop-last-grapheme (dashboard-ui-state-input-text ui-state)))))))
      (when (ptui.ui.runtime:runtime-root runtime)
        (ptui.widgets.core:dispatch-widget-event
         (ptui.ui.runtime:runtime-root runtime)
         route)))
    ui-state))

(defun main-legacy (&rest argv)
  (declare (ignore argv))
  (ptui.engine.loop:run #'%render-dashboard-legacy :backend :auto :fps 20))

(defun main-ui (&rest argv)
  (declare (ignore argv))
  (ptui.engine.loop:run #'%render-dashboard-ui
                        :backend :auto
                        :fps 20
                        :initial-state (make-dashboard-ui-state
                                        :runtime (ptui.ui.runtime:make-runtime))
                        :on-event #'%on-dashboard-ui-event))

(defun main (&rest argv)
  (let ((mode (string-downcase (or (uiop:getenv "PTUI_DASHBOARD_MODE") ""))))
    (if (string= mode "legacy")
        (apply #'main-legacy argv)
        (apply #'main-ui argv))))
