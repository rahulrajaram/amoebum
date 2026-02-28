(defpackage :ptui.views.paint
  (:use :cl)
  (:export
   #:paint-list-view
   #:paint-text-input
   #:paint-status-bar
   #:register-view-painters))

(in-package :ptui.views.paint)

;;; ===================================================================
;;; I287: View Paint Functions
;;; ===================================================================

(defun paint-list-view (element buffer x y max-cols max-rows)
  "Paint visible items of a list-view within bounds."
  (let ((children (ptui.ui.elements:ui-element-children element))
        (offset-y y))
    (dolist (child children)
      (when (>= offset-y max-rows)
        (return))
      (ptui.ui.app::%paint-element child buffer x offset-y max-cols max-rows)
      (incf offset-y))))

(defun paint-text-input (element buffer x y max-cols max-rows)
  "Paint text input with cursor indicator."
  (declare (ignore max-rows))
  (let* ((props (ptui.ui.elements:ui-element-props element))
         (value (or (getf props :value) ""))
         (placeholder (or (getf props :placeholder) ""))
         (cursor-pos (or (getf props :cursor-pos) 0))
         (display-text (if (zerop (length value)) placeholder value))
         (available-width (max 0 (- max-cols x))))
    (when (plusp available-width)
      (ptui.render.buffer:buffer-draw-text
       buffer x y display-text :max-width available-width)
      ;; Draw cursor as inverse character
      (let ((cursor-x (+ x (min cursor-pos available-width))))
        (when (< cursor-x max-cols)
          (let* ((cursor-char (if (< cursor-pos (length value))
                                  (string (char value cursor-pos))
                                  " "))
                 (cursor-cell (ptui.core.types:make-cell
                               cursor-char nil nil
                               (ptui.core.types:make-attrs :invertp t))))
            (ptui.render.buffer:buffer-draw-text
             buffer cursor-x y (list cursor-cell))))))))

(defun paint-status-bar (element buffer x y max-cols max-rows)
  "Paint status bar with left/center/right aligned segments."
  (declare (ignore max-rows))
  (let* ((props (ptui.ui.elements:ui-element-props element))
         (left (or (getf props :left) ""))
         (center (or (getf props :center) ""))
         (right (or (getf props :right) ""))
         (available-width (max 0 (- max-cols x))))
    (when (plusp available-width)
      ;; Left-aligned
      (when (plusp (length left))
        (ptui.render.buffer:buffer-draw-text
         buffer x y left :max-width available-width))
      ;; Center-aligned
      (when (plusp (length center))
        (let ((center-x (+ x (max 0 (floor (- available-width (length center)) 2)))))
          (ptui.render.buffer:buffer-draw-text
           buffer center-x y center :max-width (max 0 (- max-cols center-x)))))
      ;; Right-aligned
      (when (plusp (length right))
        (let ((right-x (+ x (max 0 (- available-width (length right))))))
          (ptui.render.buffer:buffer-draw-text
           buffer right-x y right :max-width (max 0 (- max-cols right-x))))))))

(defun register-view-painters ()
  "Register all view paint functions with the app paint registry."
  (ptui.ui.app:register-view-painter :list-view #'paint-list-view)
  (ptui.ui.app:register-view-painter :text-input #'paint-text-input)
  (ptui.ui.app:register-view-painter :status-bar #'paint-status-bar))

;; Auto-register on load
(eval-when (:load-toplevel :execute)
  (register-view-painters))
