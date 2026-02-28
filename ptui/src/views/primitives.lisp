(defpackage :ptui.views
  (:use :cl)
  (:export
   ;; I285: list-view
   #:list-view
   #:render-list-view
   ;; I286: text-input, status-bar
   #:panel-text-input
   #:render-panel-text-input
   #:status-bar
   #:render-status-bar))

(in-package :ptui.views)

;;; ===================================================================
;;; I285: list-view Widget
;;; ===================================================================

(ptui.widgets.defwidget:defwidget list-view
    (items render-item viewport-height on-select selected-index id)
  "Virtualized list: only renders visible items. Scrolls to keep selected-index visible."
  (:memoize nil)
  (:focusable t)
  (let ((effective-height (or viewport-height 10)))
    (multiple-value-bind (scroll-offset set-scroll-offset)
        (ptui.ui.hooks:use-state scroll-offset :initial-value 0)
      ;; Auto-scroll to keep selection visible
      (let ((adjusted-offset
              (if selected-index
                  (cond
                    ((< selected-index scroll-offset)
                     selected-index)
                    ((>= selected-index (+ scroll-offset effective-height))
                     (max 0 (- selected-index effective-height -1)))
                    (t scroll-offset))
                  scroll-offset)))
        (unless (= adjusted-offset scroll-offset)
          (funcall set-scroll-offset adjusted-offset)
          (setf scroll-offset adjusted-offset))
        ;; Compute visible window
        (let* ((total (length items))
               (start (min scroll-offset (max 0 (- total effective-height))))
               (start (max 0 start))
               (end (min total (+ start effective-height)))
               (visible-items (subseq items start end))
               (rendered-children
                 (loop for item in visible-items
                       for idx from start
                       for selected-p = (and selected-index (= idx selected-index))
                       collect (funcall render-item item idx selected-p))))
          ;; Event handler for arrow keys and enter
          (let ((handler
                  (ptui.ui.hooks:use-event-map list-view-keys
                    (:up
                     (when selected-index
                       (let ((new-idx (max 0 (1- selected-index))))
                         (when on-select
                           (funcall on-select new-idx)))))
                    (:down
                     (when selected-index
                       (let ((new-idx (min (1- total) (1+ selected-index))))
                         (when on-select
                           (funcall on-select new-idx)))))
                    (:enter
                     (when (and selected-index on-select)
                       (funcall on-select selected-index))))))
            (ptui.ui.elements:make-element
             :list-view
             :id id
             :props (list :viewport-height effective-height
                          :scroll-offset scroll-offset
                          :selected-index selected-index
                          :item-count total
                          :on-event handler)
             :children rendered-children
             :focusablep t)))))))

;;; ===================================================================
;;; I286: text-input Widget
;;; ===================================================================

(ptui.widgets.defwidget:defwidget panel-text-input
    (value placeholder on-change on-submit cursor-pos id)
  "Single-line text input with cursor."
  (:memoize nil)
  (:focusable t)
  (multiple-value-bind (cur-pos set-cur-pos)
      (ptui.ui.hooks:use-state cursor-pos
        :initial-value (or cursor-pos (length (or value ""))))
    (let* ((text (or value ""))
           (effective-pos (min cur-pos (length text)))
           (handler
             (ptui.ui.hooks:use-event-map text-input-keys
               (:left
                (let ((new-pos (max 0 (1- effective-pos))))
                  (funcall set-cur-pos new-pos)))
               (:right
                (let ((new-pos (min (length text) (1+ effective-pos))))
                  (funcall set-cur-pos new-pos)))
               (:home
                (funcall set-cur-pos 0))
               (:end
                (funcall set-cur-pos (length text)))
               (:backspace
                (when (plusp effective-pos)
                  (let ((new-text (concatenate 'string
                                               (subseq text 0 (1- effective-pos))
                                               (subseq text effective-pos))))
                    (funcall set-cur-pos (1- effective-pos))
                    (when on-change
                      (funcall on-change new-text)))))
               (:delete
                (when (< effective-pos (length text))
                  (let ((new-text (concatenate 'string
                                               (subseq text 0 effective-pos)
                                               (subseq text (1+ effective-pos)))))
                    (when on-change
                      (funcall on-change new-text)))))
               (:enter
                (when on-submit
                  (funcall on-submit text)))
               ((:text ch)
                (let ((new-text (concatenate 'string
                                             (subseq text 0 effective-pos)
                                             (string ch)
                                             (subseq text effective-pos))))
                  (funcall set-cur-pos (1+ effective-pos))
                  (when on-change
                    (funcall on-change new-text)))))))
      (ptui.ui.elements:make-element
       :text-input
       :id id
       :props (list :value text
                    :placeholder (or placeholder "")
                    :cursor-pos effective-pos
                    :on-event handler)
       :focusablep t))))

;;; ===================================================================
;;; I286: status-bar Widget
;;; ===================================================================

(ptui.widgets.defwidget:defwidget status-bar (segments style id)
  "Horizontal bar with left/center/right segments."
  (:memoize :equal)
  (:focusable nil)
  (let ((left (or (getf segments :left) ""))
        (center (or (getf segments :center) ""))
        (right (or (getf segments :right) "")))
    (ptui.ui.elements:make-element
     :status-bar
     :id id
     :props (list :left (if (stringp left) left (princ-to-string left))
                  :center (if (stringp center) center (princ-to-string center))
                  :right (if (stringp right) right (princ-to-string right))
                  :style style))))
