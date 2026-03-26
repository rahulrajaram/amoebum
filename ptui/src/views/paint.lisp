(defpackage :ptui.views.paint
  (:use :cl)
  (:export
   #:paint-list-view
   #:paint-text-input
   #:paint-status-bar
   #:theme-styled-segment
   #:paint-badge
   #:paint-gutter
   #:paint-tree-row
   #:paint-structured-text-line
   #:paint-structured-text-block
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

;;; ===================================================================
;;; I369: Row painters + reusable style primitives
;;; ===================================================================

(defun %active-theme ()
  (or ptui.core.theme:*active-theme*
      (ptui.core.theme:find-theme :dark)))

(defun theme-styled-segment (text role &key (theme nil))
  "Return a styled-text segment for BUFFER-DRAW-TEXT."
  (list text (ptui.core.theme:theme-role-cell
              (or theme (%active-theme))
              role)))

(defun %draw-segments (buffer x y segments &optional max-width)
  (ptui.render.buffer:buffer-draw-text buffer x y segments :max-width max-width))

(defun paint-badge (buffer x y label &key
                   (theme nil)
                   (role :badge)
                   (left "[")
                   (right "]")
                   max-width)
  "Paint a role-styled badge token."
  (let* ((label-text (if (stringp label) label (princ-to-string label)))
         (segments (list (theme-styled-segment left role :theme theme)
                         (theme-styled-segment label-text role :theme theme)
                         (theme-styled-segment right role :theme theme))))
    (%draw-segments buffer x y segments max-width)
    (+ x (ptui.text.width:string-width left)
       (ptui.text.width:string-width label-text)
       (ptui.text.width:string-width right))))

(defun paint-gutter (buffer x y marker &key
                    (theme nil)
                    (selected nil)
                    (default-role :gutter))
  "Paint a gutter token."
  (let* ((role (if selected :gutter-selected default-role))
         (marker-text (if (stringp marker) marker (princ-to-string marker)))
         (segment (theme-styled-segment marker-text role :theme theme)))
    (%draw-segments buffer x y segment)
    (+ x (ptui.text.width:string-width marker-text))))

(defun paint-tree-row (buffer x y label
                      &key
                      (theme nil)
                      (depth 0)
                      (expanded nil)
                      (selected nil))
  "Paint a consistent tree-row prefix with hierarchical depth, marker, and label."
  (let* ((active-theme (or theme (%active-theme)))
         (label-text (if (stringp label) label (princ-to-string label)))
         (indent (make-string (max 0 (* 2 (max 0 depth))) :initial-element #\Space))
         (marker (if expanded "▾" "▸")))
    (%draw-segments
     buffer x y
     (list (theme-styled-segment indent :tree-gutter :theme active-theme)
           (theme-styled-segment marker :tree-connector :theme active-theme)
           (theme-styled-segment " " :tree-gutter :theme active-theme)
           (theme-styled-segment label-text
                                 (if selected :tree-row-selected :tree-row)
                                 :theme active-theme))
     nil)
    (+ x (ptui.text.width:string-width indent)
       (ptui.text.width:string-width marker)
       1
       (ptui.text.width:string-width label-text))))

(defun paint-structured-text-line (buffer x y key value
                                  &key
                                  (theme nil)
                                  (key-role :block-key)
                                  (value-role :block-value)
                                  (meta-role :block-meta)
                                  (show-meta nil)
                                  (meta ""))
  "Paint a single key/value line."
  (let* ((key-text (if (stringp key) key (princ-to-string key)))
         (value-text (if (stringp value) value (princ-to-string value)))
         (active-theme (or theme (%active-theme)))
         (meta-text (if (and show-meta (stringp meta) (> (length meta) 0))
                       (concatenate 'string "  " meta)
                       "")))
    (%draw-segments
     buffer x y
     (remove nil
             (list (theme-styled-segment key-text key-role :theme active-theme)
                   (theme-styled-segment ": " :block-meta :theme active-theme)
                   (theme-styled-segment value-text value-role :theme active-theme)
                   (unless (string= meta-text "")
                     (theme-styled-segment meta-text meta-role :theme active-theme))))
     nil)
    (+ x (ptui.text.width:string-width key-text)
       2
       (ptui.text.width:string-width value-text)
       (ptui.text.width:string-width meta-text))))

(defun paint-structured-text-block (buffer x y title rows
                                   &key
                                   (theme nil)
                                   (title-role :block-title)
                                   (key-role :block-key)
                                   (value-role :block-value)
                                   (meta-role :block-meta))
  "Paint a structured text block from key/value rows."
  (let* ((row y)
         (active-theme (or theme (%active-theme)))
         (title-text (if (stringp title) title (princ-to-string title))))
    (when (plusp (length title-text))
      (%draw-segments buffer x row (theme-styled-segment title-text title-role :theme active-theme))
      (incf row))
    (dolist (entry rows)
      (cond
        ((and (consp entry) (= (length entry) 2))
         (paint-structured-text-line
          buffer x row (first entry) (second entry)
          :theme active-theme
          :key-role key-role
          :value-role value-role
          :meta-role meta-role))
        ((and (consp entry) (eq (first entry) :meta))
         (%draw-segments
          buffer x row
          (theme-styled-segment
           (if (stringp (second entry))
               (second entry)
               (princ-to-string (second entry)))
           meta-role
           :theme active-theme)))
        (t
         (%draw-segments
          buffer x row
          (theme-styled-segment
           (if (stringp entry) entry (princ-to-string entry))
           value-role
           :theme active-theme))))
      (incf row))
    row))

(defun register-view-painters ()
  "Register all view paint functions with the app paint registry."
  (ptui.ui.app:register-view-painter :list-view #'paint-list-view)
  (ptui.ui.app:register-view-painter :text-input #'paint-text-input)
  (ptui.ui.app:register-view-painter :status-bar #'paint-status-bar))

;; Auto-register on load
(eval-when (:load-toplevel :execute)
  (register-view-painters))
