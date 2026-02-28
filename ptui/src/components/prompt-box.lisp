(defpackage :ptui.components.prompt-box
  (:use :cl)
  (:export #:make-prompt-box-widget))

(in-package :ptui.components.prompt-box)

(defun %prop (element key &optional default)
  (let ((props (ptui.ui.elements:ui-element-props element)))
    (if (and (listp props) (keywordp key))
        (getf props key default)
        default)))

(defun %layout-size (w h)
  (ptui.layout:make-layout-size (max 0 w) (max 0 h)))

(defun %prompt-box-measure (element)
  (let* ((value (%prop element :value ""))
         (min-width (%prop element :min-width 0))
         (max-width (%prop element :max-width nil))
         (min-rows (%prop element :min-rows 1))
         (max-rows (%prop element :max-rows nil))
         (lines (ptui.text.layout:wrap-by-width
                 value
                 (if (and max-width (> max-width 0))
                     max-width
                     (max 1 (ptui.text.width:string-width value)))))
         (line-count (max 1 (length lines)))
         (line-width (loop for line in lines
                           maximize (ptui.text.width:string-width line)
                           into maxw
                           finally (return (or maxw 0))))
         (content-width (max min-width line-width))
         (content-width (if (and max-width (> max-width 0))
                            (min content-width max-width)
                            content-width))
         (content-rows (max min-rows line-count))
         (content-rows (if max-rows
                           (min content-rows max-rows)
                           content-rows)))
    ;; Prompt-box includes its own border.
    (%layout-size (+ content-width 2)
                  (+ content-rows 2))))

(defun make-prompt-box-widget (value &key id key (min-width 0) max-width (min-rows 1) max-rows
                                     (scroll-offset nil) (cursor-position nil)
                                     (border-style :rounded) on-event)
  (when on-event
    (check-type on-event function))
  (when (and max-width (< max-width 0))
    (error "MAX-WIDTH must be non-negative or NIL. Got: ~S" max-width))
  (when (< min-rows 1)
    (error "MIN-ROWS must be >= 1. Got: ~S" min-rows))
  (when (and max-rows (< max-rows min-rows))
    (error "MAX-ROWS must be >= MIN-ROWS when provided. Got MIN-ROWS=~S MAX-ROWS=~S"
           min-rows max-rows))
  (unless (member border-style '(:rounded :square))
    (error "BORDER-STYLE must be :rounded or :square. Got: ~S" border-style))
  (ptui.ui.elements:make-element
   :prompt-box
   :id id
   :key key
   :props (list :value value
                :min-width (max 0 min-width)
                :max-width max-width
                :min-rows min-rows
                :max-rows max-rows
                :scroll-offset scroll-offset
                :cursor-position cursor-position
                :border-style border-style
                :measure #'%prompt-box-measure
                :on-event on-event)
   :focusablep t
   :children '()))
