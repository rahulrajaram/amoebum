(defpackage :ptui.widgets.core
  (:use :cl)
  (:export
   #:make-text-widget
   #:make-spacer-widget
   #:make-box-widget
   #:make-stack-widget
   #:make-input-widget
   #:make-scroll-widget
   #:widget-measure
   #:dispatch-widget-event))

(in-package :ptui.widgets.core)

(defun %prop (element key &optional default)
  (let ((props (ptui.ui.elements:ui-element-props element)))
    (if (and (listp props) (keywordp key))
        (getf props key default)
        default)))

(defun %node-id (element)
  (or (ptui.ui.elements:ui-element-id element)
      (ptui.ui.elements:ui-element-key element)))

(defun make-text-widget (text &key id key styled-segments role metadata)
  "Create a text display element. Supports optional STYLED-SEGMENTS for ANSI-colored output."
  (ptui.ui.elements:make-element
   :text
   :id id
   :key key
   :props (append (list :text text)
                  (when styled-segments
                    (list :styled-segments styled-segments))
                  (when role
                    (list :role role))
                  (when metadata
                    (list :metadata metadata)))
   :children '()))

(defun make-spacer-widget (width height &key id key)
  (ptui.ui.elements:make-element
   :spacer
   :id id
   :key key
   :props (list :width (max 0 width)
                :height (max 0 height))
   :children '()))

(defun make-box-widget (child &key id key (padding 0) (borderp nil) border fg bg attrs)
  "Create a box container element with optional padding and style.
Accepted BORDER values are :rounded, :single, :double, :none, and nil/false.
For compatibility, BORDERP continues to request default :rounded border when BORDER is NIL."
  (let* ((border-value (cond
                         ((or (not (null borderp)) (and border (not (eq border :none))))
                          (or border :rounded))
                         (t nil)))
         (props (list :padding (max 0 padding)
                      :border border-value
                      :borderp (and border-value (not (eq border-value :none)))
                      :fg fg
                      :bg bg
                      :attrs attrs)))
    (ptui.ui.elements:make-element
     :box
     :id id
     :key key
     :props props
     :children (if child (list child) '()))))

(defun make-stack-widget (children &key id key (direction :column) (gap 0))
  "Create a stack layout element. DIRECTION is :row or :column."
  (unless (member direction '(:row :column))
    (error "STACK direction must be :row or :column. Got: ~S" direction))
  (ptui.ui.elements:make-element
   :stack
   :id id
   :key key
   :props (list :direction direction
                :gap (max 0 gap))
   :children children))

(defun make-input-widget (value &key id key (min-width 0) on-event)
  "Create a focusable text input element. ON-EVENT receives (event node)."
  (when on-event
    (check-type on-event function))
  (ptui.ui.elements:make-element
   :input
   :id id
   :key key
   :props (list :value value
                :min-width (max 0 min-width)
                :on-event on-event)
   :focusablep t
   :children '()))

(defun make-scroll-widget (child &key id key viewport-width viewport-height (offset 0))
  "Create a scrollable viewport element wrapping CHILD."
  (ptui.ui.elements:make-element
   :scroll
   :id id
   :key key
   :props (list :viewport-width viewport-width
                :viewport-height viewport-height
                :offset (max 0 offset))
   :children (if child (list child) '())))

(defun %layout-size (w h)
  (ptui.layout:make-layout-size (max 0 w) (max 0 h)))

(defun %children-measures (element)
  (mapcar #'widget-measure (ptui.ui.elements:ui-element-children element)))

(defun %stack-measure (element)
  (let* ((direction (%prop element :direction :column))
         (gap (%prop element :gap 0))
         (sizes (%children-measures element))
         (count (length sizes)))
    (if (null sizes)
        (%layout-size 0 0)
        (case direction
          (:row
           (%layout-size
            (+ (loop for s in sizes sum (ptui.layout:layout-size-width s))
               (* (max 0 (1- count)) gap))
            (loop for s in sizes maximize (ptui.layout:layout-size-height s))))
          (:column
           (%layout-size
            (loop for s in sizes maximize (ptui.layout:layout-size-width s))
            (+ (loop for s in sizes sum (ptui.layout:layout-size-height s))
               (* (max 0 (1- count)) gap))))
          (t (%layout-size 0 0))))))

(defun widget-measure (element)
  "Compute deterministic intrinsic size for a widget element."
  (check-type element ptui.ui.elements:ui-element)
  (let ((custom-measure (%prop element :measure nil)))
    (when custom-measure
      (check-type custom-measure function)
      (return-from widget-measure (funcall custom-measure element))))
  (let ((type (ptui.ui.elements:ui-element-type element)))
    (case type
      (:text
       (let ((text (%prop element :text "")))
         (%layout-size (ptui.text.width:string-width text) 1)))
      (:spacer
       (%layout-size (%prop element :width 0)
                     (%prop element :height 0)))
      (:input
       (let* ((value (%prop element :value ""))
              (min-width (%prop element :min-width 0))
              (text-width (ptui.text.width:string-width value)))
         (%layout-size (max min-width text-width) 1)))
      (:box
       (let* ((child (first (ptui.ui.elements:ui-element-children element)))
              (child-size (if child (widget-measure child) (%layout-size 0 0)))
              (padding (%prop element :padding 0))
              (border (%prop element :border nil))
              (border-style (if border (or border :rounded) nil))
              (borderp (and border-style
                            (not (eq border-style :none))
                            t))
              (border-extra (if borderp 2 0))
              (pad-extra (* 2 padding)))
         (%layout-size (+ (ptui.layout:layout-size-width child-size) pad-extra border-extra)
                       (+ (ptui.layout:layout-size-height child-size) pad-extra border-extra))))
      (:stack
       (%stack-measure element))
      (:scroll
       (let* ((child (first (ptui.ui.elements:ui-element-children element)))
              (child-size (if child (widget-measure child) (%layout-size 0 0)))
              (viewport-width (%prop element :viewport-width nil))
              (viewport-height (%prop element :viewport-height nil)))
         (%layout-size (or viewport-width (ptui.layout:layout-size-width child-size))
                       (or viewport-height (ptui.layout:layout-size-height child-size))))
       )
      (t
       ;; Generic container fallback: max width, summed height.
       (let ((sizes (%children-measures element)))
         (%layout-size (loop for s in sizes maximize (ptui.layout:layout-size-width s) into maxw
                             finally (return (or maxw 0)))
                       (loop for s in sizes sum (ptui.layout:layout-size-height s))))))))

(defun %find-node-by-id (element target-id)
  (when element
    (if (equal (%node-id element) target-id)
        element
        (loop for child in (ptui.ui.elements:ui-element-children element)
              for hit = (%find-node-by-id child target-id)
              when hit do (return hit)))))

(defun %find-path-to-node (element target-id &optional path)
  "Return list of nodes from ROOT to TARGET-ID (inclusive), or NIL."
  (when element
    (let ((current-path (append path (list element))))
      (if (equal (%node-id element) target-id)
          current-path
          (loop for child in (ptui.ui.elements:ui-element-children element)
                for result = (%find-path-to-node child target-id current-path)
                when result do (return result))))))

(defun dispatch-widget-event (root route &key (bubble nil))
  "Dispatch ROUTE (:target/:event) into ROOT widget tree and invoke handler.
When BUBBLE is T, if the target handler returns :BUBBLE, propagate up to parent.
Supports :on-event-capture prop for capture phase (top-down, before bubble)."
  (check-type root ptui.ui.elements:ui-element)
  (check-type route list)
  (let* ((target (getf route :target))
         (event (getf route :event)))
    (if (not bubble)
        ;; Original non-bubbling dispatch
        (let ((node (and target (%find-node-by-id root target))))
          (when node
            (let ((handler (%prop node :on-event nil)))
              (when handler
                (funcall handler event node)))))
        ;; I279: Bubbling dispatch with capture phase
        (let ((path (and target (%find-path-to-node root target))))
          (when path
            ;; Capture phase: top-down
            (dolist (node path)
              (let ((capture-handler (%prop node :on-event-capture nil)))
                (when capture-handler
                  (let ((result (funcall capture-handler event node)))
                    (when (and result (not (eq result :bubble)))
                      (return-from dispatch-widget-event result))))))
            ;; Bubble phase: bottom-up from target
            (dolist (node (reverse path))
              (let ((handler (%prop node :on-event nil)))
                (when handler
                  (let ((result (funcall handler event node)))
                    (unless (eq result :bubble)
                      (return-from dispatch-widget-event result)))))))
          nil))))
