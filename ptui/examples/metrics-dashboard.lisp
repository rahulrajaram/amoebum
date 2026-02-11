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

(defun %clamp-layout-size (size avail-width avail-height)
  (let ((w (ptui.layout:layout-size-width size))
        (h (ptui.layout:layout-size-height size)))
    (ptui.layout:make-layout-size
     (if avail-width
         (min w (max 0 avail-width))
         w)
     (if avail-height
         (min h (max 0 avail-height))
         h))))

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
                      (prompt-scroll-offset nil)
                      (frame-count 0))))
  runtime
  (input-text "" :type string)
  (last-event "none" :type string)
  (prompt-scroll-offset nil)
  (frame-count 0 :type fixnum)
  (cached-tree-key nil)
  (cached-tree nil)
  (cached-layout nil)
  (cached-render-key nil)
  (cached-buffer nil))

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
                (%clamp-layout-size
                 (ptui.widgets.core:widget-measure element)
                 avail-width
                 avail-height))
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

(defun %prompt-wrapped-lines (value width)
  (if (<= width 0)
      (list "")
      (ptui.text.layout:wrap-by-width value (max 1 width))))

(defun %prompt-visible-lines (lines visible-rows scroll-offset)
  (let* ((row-count (max 0 visible-rows))
         (total (length lines))
         (max-offset (max 0 (- total row-count)))
         (desired (if (null scroll-offset)
                      max-offset
                      scroll-offset))
         (offset (min max-offset (max 0 desired)))
         (end (min total (+ offset row-count))))
    (values (subseq lines offset end) offset max-offset)))

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
            (:prompt-box
             (let* ((value (%element-prop element :value ""))
                    (border-style (%element-prop element :border-style :rounded))
                    (scroll-offset (%element-prop element :scroll-offset nil))
                    (inner-x (+ x 1))
                    (inner-y (+ y 1))
                    (inner-w (max 0 (- w 2)))
                    (inner-h (max 0 (- h 2)))
                    (line-cell (%ui-line-cell id focus-id))
                    (lines (%prompt-wrapped-lines value inner-w)))
               (ptui.render.buffer:buffer-draw-border
                buf rect :border-style border-style)
               (multiple-value-bind (visible-lines effective-offset max-offset)
                   (%prompt-visible-lines lines inner-h scroll-offset)
                 (declare (ignore effective-offset max-offset))
                 (loop for line in visible-lines
                       for row from 0 do
                         (ptui.render.buffer:buffer-draw-text
                          buf
                          inner-x
                          (+ inner-y row)
                          (list (list (%fit-line-width line inner-w) line-cell))
                          :max-width inner-w)))))
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
                       (ptui.render.buffer:with-clip (buf inner-rect)
                         (%render-ui-element buf child layout focus-id
                                             :dx delta-x
                                             :dy delta-y))))))))
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
                "Tab focuses input. Ctrl-J newline. Up/Down scroll. q/Ctrl-C quits."
                :id :ui-hint))
         (input-box (ptui.components.prompt-box:make-prompt-box-widget
                     (dashboard-ui-state-input-text state)
                     :id :ui-input
                     :min-width 18
                     :max-width inner-width
                     :min-rows 1
                     :max-rows 4
                     :scroll-offset (dashboard-ui-state-prompt-scroll-offset state)
                     :border-style :rounded
                     :on-event (lambda (event node)
                                 (declare (ignore node))
                                 (setf (dashboard-ui-state-last-event state)
                                       (format nil "dispatched: ~S"
                                               (and (typep event 'ptui.core.events:key-event)
                                                    (ptui.core.events:key-event-key event)))))))
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

(defun %dashboard-tree-key (state cols rows)
  (list cols
        rows
        (dashboard-ui-state-input-text state)
        (dashboard-ui-state-prompt-scroll-offset state)
        (dashboard-ui-state-last-event state)))

(defun %render-dashboard-ui (state size)
  (let* ((ui-state (%ensure-ui-state state))
         (runtime (dashboard-ui-state-runtime ui-state))
         (cols (ptui.core.types:size-cols size))
         (rows (ptui.core.types:size-rows size))
         (panel (%safe-inner-rect cols rows))
         (tree-key (%dashboard-tree-key ui-state cols rows))
         (tree (dashboard-ui-state-cached-tree ui-state))
         (layout (dashboard-ui-state-cached-layout ui-state))
         (focus-id nil))
    (incf (dashboard-ui-state-frame-count ui-state))
    (unless (and tree layout
                 (equal tree-key (dashboard-ui-state-cached-tree-key ui-state)))
      (setf tree (%build-ui-tree ui-state cols rows))
      (setf layout (ptui.layout:compute-layout
                    (%ui-tree-node tree)
                    :x 3
                    :y 2
                    :width (max 0 (- cols 6))
                    :height (max 0 (- rows 4))))
      (ptui.ui.runtime:update-runtime runtime tree)
      (setf (dashboard-ui-state-cached-tree-key ui-state) tree-key
            (dashboard-ui-state-cached-tree ui-state) tree
            (dashboard-ui-state-cached-layout ui-state) layout
            (dashboard-ui-state-cached-render-key ui-state) nil
            (dashboard-ui-state-cached-buffer ui-state) nil))
    (setf focus-id (ptui.ui.runtime:runtime-focus-id runtime))
    (let* ((render-key (list tree-key focus-id))
           (cached-render-key (dashboard-ui-state-cached-render-key ui-state))
           (cached-buffer (dashboard-ui-state-cached-buffer ui-state)))
      (if (and cached-buffer (equal render-key cached-render-key))
          cached-buffer
          (let ((buf (ptui.render.buffer:make-buffer cols rows)))
            (ptui.render.buffer:buffer-draw-border buf panel)
            (%render-ui-element buf tree layout focus-id)
            (setf (dashboard-ui-state-cached-render-key ui-state) render-key
                  (dashboard-ui-state-cached-buffer ui-state) buf)
            buf)))))

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
                                text))
             (setf (dashboard-ui-state-prompt-scroll-offset ui-state) nil))
            ((eql key :ctrl-j)
             (setf (dashboard-ui-state-input-text ui-state)
                   (concatenate 'string
                                (dashboard-ui-state-input-text ui-state)
                                (string #\Newline)))
             (setf (dashboard-ui-state-prompt-scroll-offset ui-state) nil))
            ((eql key :backspace)
             (setf (dashboard-ui-state-input-text ui-state)
                   (%pop-last-grapheme
                    (dashboard-ui-state-input-text ui-state)))
             (setf (dashboard-ui-state-prompt-scroll-offset ui-state) nil))
            ((eql key :up)
             (setf (dashboard-ui-state-prompt-scroll-offset ui-state)
                   (1+ (or (dashboard-ui-state-prompt-scroll-offset ui-state) 0))))
            ((eql key :down)
             (setf (dashboard-ui-state-prompt-scroll-offset ui-state)
                   (max 0
                        (1- (or (dashboard-ui-state-prompt-scroll-offset ui-state)
                                0))))))))
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
