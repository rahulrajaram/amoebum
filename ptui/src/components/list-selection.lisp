(defpackage :ptui.components.list-selection
  (:use :cl)
  (:export
   #:clamp-index
   #:move-selection
   #:visible-window))

(in-package :ptui.components.list-selection)

(defun clamp-index (selected-index item-count)
  "Clamp SELECTED-INDEX to valid range [0, ITEM-COUNT-1], or 0 if empty."
  (if (zerop item-count)
      0
      (max 0 (min selected-index (1- item-count)))))

(defun move-selection (current-index item-count direction)
  "Compute new selection index given DIRECTION (:up :down :home :end).
Returns the new index, clamped to valid range."
  (if (zerop item-count)
      0
      (ecase direction
        (:up   (max 0 (1- current-index)))
        (:down (min (1- item-count) (1+ current-index)))
        (:home 0)
        (:end  (1- item-count)))))

(defun visible-window (items selected-index visible-count)
  "Return (values visible-subseq start-index) for a scrolling list view.
ITEMS is a list, SELECTED-INDEX is the current selection, VISIBLE-COUNT
is how many items fit in the viewport."
  (let* ((count (length items))
         (start (if (<= count visible-count)
                    0
                    (min (max 0 (- selected-index (1- visible-count)))
                         (- count visible-count))))
         (end (min count (+ start visible-count))))
    (values (subseq items start end) start)))
