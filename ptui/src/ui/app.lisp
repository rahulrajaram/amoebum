(defpackage :ptui.ui.app
  (:use :cl)
  (:export
   #:defapp
   #:make-app-config
   #:app-config
   #:app-config-name
   #:app-config-backend
   #:app-config-fps
   #:app-config-initial-state
   #:app-config-on-mount
   #:app-config-on-unmount
   #:app-config-interceptors
   ;; Paint registry (I284)
   #:*view-paint-registry*
   #:register-view-painter
   #:%paint-element))

(in-package :ptui.ui.app)

;;; ===================================================================
;;; I275: defapp Macro Foundation
;;; ===================================================================

(defstruct (app-config
            (:constructor %make-app-config
                (name backend fps initial-state on-mount on-unmount interceptors)))
  (name nil :type symbol)
  (backend :auto :type (or keyword ptui.backend.protocol:terminal-backend))
  (fps 20 :type fixnum)
  (initial-state '() :type list)
  (on-mount nil)
  (on-unmount nil)
  (interceptors '() :type list))

(defun make-app-config (name &key (backend :auto) (fps 20) initial-state
                                  on-mount on-unmount interceptors)
  (%make-app-config name backend fps initial-state on-mount on-unmount interceptors))

(defun %populate-initial-state (runtime plist)
  "Pre-populate runtime state-table from an initial-state plist."
  (loop for (key value) on plist by #'cddr
        do (ptui.ui.runtime:set-runtime-state runtime key value)))

(defun %sorted-interceptors (interceptors)
  "Sort interceptors by priority (lower = earlier)."
  (stable-sort (copy-list interceptors) #'< :key #'first))

(defun %run-interceptors (interceptors event)
  "Run interceptors in priority order. Return non-nil if consumed."
  (dolist (triple interceptors)
    (destructuring-bind (priority predicate handler) triple
      (declare (ignore priority))
      (when (funcall predicate event)
        (let ((result (funcall handler event)))
          (when result
            (return-from %run-interceptors result))))))
  nil)

;;; ===================================================================
;;; I276: Auto-Cache and Focus Management
;;; ===================================================================

(defun %make-app-event-handler (runtime root-widget-fn interceptors)
  "Create event handler with auto-cache, focus management, and interceptors.
Returns (values nil disposition) where disposition is :quit, :consume, or NIL."
  (declare (ignore root-widget-fn))
  (let ((sorted-interceptors (%sorted-interceptors interceptors)))
    (lambda (state event)
      (declare (ignore state))
      ;; I280: Run interceptors first
      (let ((interceptor-result (%run-interceptors sorted-interceptors event)))
        (unless interceptor-result
          ;; Tab key → focus management (I276)
          (let ((route (ptui.ui.runtime:route-event runtime event)))
            (when (and (runtime-root-present-p runtime)
                       (eq (getf route :kind) :key))
              (ptui.widgets.core:dispatch-widget-event
               (ptui.ui.runtime:runtime-root runtime) route))))
        ;; Return disposition based on interceptor result
        (cond
          ((eq interceptor-result :quit) (values nil :quit))
          (interceptor-result (values nil :consume))
          (t nil))))))

(defun runtime-root-present-p (runtime)
  (not (null (ptui.ui.runtime:runtime-root runtime))))

(defun %make-app-render-fn (runtime root-widget-fn)
  "Create render function with state-version caching."
  (lambda (state size)
    (declare (ignore state))
    (let* ((ptui.ui.runtime:*current-runtime* runtime)
           (new-tree (funcall root-widget-fn)))
      (ptui.ui.runtime:update-runtime runtime new-tree)
      (%render-tree-to-buffer new-tree size))))

(defun %render-tree-to-buffer (tree size)
  "Render an element tree to a cell-buffer for the engine loop."
  (let* ((cols (ptui.core.types:size-cols size))
         (rows (ptui.core.types:size-rows size))
         (reusable (and (boundp 'ptui.engine.loop:*reusable-buffer*)
                        ptui.engine.loop:*reusable-buffer*))
         (buffer (if (and reusable
                          (ptui.render.buffer:buffer-dimensions-match-p reusable cols rows))
                     (progn (ptui.render.buffer:buffer-reset reusable) reusable)
                     (ptui.render.buffer:make-buffer cols rows))))
    (when tree
      (%paint-element tree buffer 0 0 cols rows))
    buffer))

(defvar *view-paint-registry* (make-hash-table :test #'eq)
  "Registry of paint functions for custom element types.
Key: element-type keyword, Value: (lambda (element buffer x y w h)).")

(defun register-view-painter (element-type paint-fn)
  "Register a paint function for ELEMENT-TYPE.
PAINT-FN signature: (element buffer x y width height)."
  (setf (gethash element-type *view-paint-registry*) paint-fn))

(defun %prompt-wrapped-lines (value width)
  (if (<= width 0)
      (list "")
      (or (ptui.text.layout:wrap-by-width
           (or value "")
           (max 1 width)
           :preserve-spaces t)
          (list ""))))

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

(defun %cursor-to-line-col (cursor-pos lines)
  (let ((remaining cursor-pos))
    (loop for line in lines
          for line-idx from 0
          for line-len = (length line)
          do (if (<= remaining line-len)
                 (if (and (= remaining line-len)
                          (< (1+ line-idx) (length lines)))
                     (return-from %cursor-to-line-col
                       (values (1+ line-idx) 0))
                     (return-from %cursor-to-line-col
                       (values line-idx remaining)))
                 (decf remaining line-len)))
    (values (max 0 (1- (length lines)))
            (if lines (length (car (last lines))) 0))))

(defun %prompt-cursor-pos (value cursor-pos-raw)
  (let ((length* (length (ptui.text.grapheme:split-graphemes (or value "")))))
    (if (and (integerp cursor-pos-raw) (>= cursor-pos-raw 0))
        (min length* cursor-pos-raw)
        length*)))

(defun %normalize-box-border-style (border)
  (case border
    ((:single :double :square) :square)
    ((:rounded :none nil) :rounded)
    (otherwise :rounded)))

(defparameter +built-in-painters+
  '((:text . %paint-text-element)
    (:input . %paint-input-element)
    (:stack . %paint-stack-element)
    (:box . %paint-box-element)
    (:prompt-box . %paint-prompt-box-element)
    (:scroll . %paint-scroll-element)
    (:constraint-layout . %paint-constraint-layout-element)
    (:panel . %paint-panel-element)))

(defun %paint-visible-text (buffer x y max-cols max-rows payload)
  (when (and (< y max-rows) (< x max-cols))
    (ptui.render.buffer:buffer-draw-text
     buffer x y payload :max-width (- max-cols x))))

(defun %text-visible-lines (element max-cols x)
  (let* ((props (ptui.ui.elements:ui-element-props element))
         (text (or (getf props :text) ""))
         (wrapp (getf props :wrap))
         (available-width (max 0 (- max-cols x))))
    (cond
      ((<= available-width 0) '())
      ((not wrapp) (list text))
      (t
       (or (ptui.text.layout:wrap-by-width text (max 1 available-width))
           (list ""))))))

(defun %extract-fill-cell (styled-segments)
  "Extract a background-fill cell from the last styled segment that has a bg color.
Returns a space cell with that background, or NIL."
  (when (listp styled-segments)
    (let ((bg-cell nil))
      (dolist (seg styled-segments)
        (when (and (listp seg) (second seg))
          (let ((cell (second seg)))
            (when (and (typep cell 'ptui.core.types:cell)
                       (ptui.core.types:cell-bg cell))
              (setf bg-cell cell)))))
      (when bg-cell
        (ptui.core.types:make-cell " "
                                    nil
                                    (ptui.core.types:cell-bg bg-cell)
                                    (ptui.core.types:make-attrs))))))

(defun %paint-text-element (element buffer x y max-cols max-rows)
  (let* ((props (ptui.ui.elements:ui-element-props element))
         (styled-segments (getf props :styled-segments))
         (fill-cell (when styled-segments
                      (%extract-fill-cell styled-segments))))
    ;; Fill allocated width with background before drawing text
    (when (and fill-cell (< y max-rows) (< x max-cols))
      (ptui.render.buffer:buffer-fill-rect
       buffer
       (ptui.core.types:make-rect x y (- max-cols x) 1)
       fill-cell))
    ;; Use direct segment rendering when we have a segment list (bypasses flatten-styled-text)
    (when (and (< y max-rows) (< x max-cols))
      (if (and styled-segments (listp styled-segments))
          (ptui.render.buffer:buffer-draw-styled-segments
           buffer x y styled-segments
           #'identity  ; segments already normalized to (text cell) by chat.lisp
           :max-width (- max-cols x))
          (loop for line in (%text-visible-lines element max-cols x)
                for row from 0
                while (< (+ y row) max-rows)
                do (%paint-visible-text buffer x (+ y row) max-cols max-rows line))))))

(defun %paint-input-element (element buffer x y max-cols max-rows)
  (%paint-visible-text buffer
                       x
                       y
                       max-cols
                       max-rows
                       (or (getf (ptui.ui.elements:ui-element-props element) :value) "")))

(defun %advance-stack-offsets (direction size gap offset-x offset-y)
  (case direction
    (:row
     (values (+ offset-x (ptui.layout:layout-size-width size) gap) offset-y))
    (otherwise
     (values offset-x (+ offset-y (ptui.layout:layout-size-height size) gap)))))

(defun %paint-stack-element (element buffer x y max-cols max-rows)
  (let* ((props (ptui.ui.elements:ui-element-props element))
         (direction (getf props :direction :column))
         (gap (or (getf props :gap) 0))
         (offset-x x)
         (offset-y y))
    (dolist (child (ptui.ui.elements:ui-element-children element))
      (when (< offset-y max-rows)
        (let* ((child-available-width (max 0 (- max-cols offset-x)))
               (child-available-height (max 0 (- max-rows offset-y)))
               (child-size (ptui.widgets.core:widget-measure
                            child
                            child-available-width
                            child-available-height))
               (clip-width (case direction
                             (:row (ptui.layout:layout-size-width child-size))
                             (otherwise child-available-width)))
               (clip-height (case direction
                              (:row child-available-height)
                              (otherwise (ptui.layout:layout-size-height child-size)))))
          (ptui.render.buffer:with-clip
              (buffer (ptui.core.types:make-rect offset-x offset-y clip-width clip-height))
            (%paint-element child buffer offset-x offset-y max-cols max-rows))
          (multiple-value-setq (offset-x offset-y)
            (%advance-stack-offsets direction
                                    child-size
                                    gap
                                    offset-x
                                    offset-y)))))))

(defun %box-draw-dimensions (element x y max-cols max-rows)
  (let* ((available-width (max 0 (- max-cols x)))
         (available-height (max 0 (- max-rows y)))
         (desired-width (or (getf (ptui.ui.elements:ui-element-props element) :max-width)
                            available-width))
         (clamped-width (max 0 (min available-width desired-width)))
         (draw-x (+ x (max 0 (floor (- available-width clamped-width) 2))))
         (measured (ptui.widgets.core:widget-measure element clamped-width available-height)))
    (values draw-x
            clamped-width
            (max 0 (min available-height
                        (ptui.layout:layout-size-height measured))))))

(defun %paint-box-border (buffer props border x y draw-width draw-height)
  (let* ((attrs (or (getf props :attrs)
                    (ptui.core.types:make-attrs)))
         (style-cell (ptui.core.types:make-cell
                      " "
                      (getf props :fg)
                      (getf props :bg)
                      attrs)))
    (ptui.render.buffer:buffer-draw-border
     buffer
     (ptui.core.types:make-rect x y draw-width draw-height)
     :style style-cell
     :border-style (%normalize-box-border-style border))))

(defun %paint-box-child (child buffer x y draw-width draw-height
                         inset-top inset-right inset-bottom inset-left)
  (let* ((inner-x (+ x inset-left))
         (inner-y (+ y inset-top))
         (inner-width (max 0 (- draw-width inset-left inset-right)))
         (inner-height (max 0 (- draw-height inset-top inset-bottom))))
    (ptui.render.buffer:with-clip
        (buffer (ptui.core.types:make-rect inner-x inner-y inner-width inner-height))
      (%paint-element child
                      buffer
                      inner-x
                      inner-y
                      (+ inner-x inner-width)
                      (+ inner-y inner-height)))))

(defun %paint-box-element (element buffer x y max-cols max-rows)
  (let* ((props (ptui.ui.elements:ui-element-props element))
         (insets (or (getf props :padding-insets)
                     (let ((p (max 0 (or (getf props :padding) 0))))
                       (list p p p p))))
         (pad-top (first insets))
         (pad-right (second insets))
         (pad-bottom (third insets))
         (pad-left (fourth insets))
         (border (getf props :border nil))
         (borderp (and border (not (eq border :none))))
         (border-width (if borderp 1 0))
         (inset-top (+ pad-top border-width))
         (inset-right (+ pad-right border-width))
         (inset-bottom (+ pad-bottom border-width))
         (inset-left (+ pad-left border-width))
         (child (first (ptui.ui.elements:ui-element-children element))))
    (multiple-value-bind (draw-x draw-width draw-height)
        (%box-draw-dimensions element x y max-cols max-rows)
      (when (and borderp (> draw-width 1) (> draw-height 1))
        (%paint-box-border buffer props border draw-x y draw-width draw-height))
      (when (and child
                 (> draw-width (+ inset-left inset-right))
                 (> draw-height (+ inset-top inset-bottom)))
        (%paint-box-child child buffer draw-x y draw-width draw-height
                          inset-top inset-right inset-bottom inset-left)))))

(defun %prompt-border-style (props)
  (if (eq (getf props :border-style :rounded) :square)
      :square
      :rounded))

(defun %prompt-draw-geometry (element props x y max-cols max-rows)
  (let* ((available-width (max 0 (- max-cols x)))
         (available-height (max 0 (- max-rows y)))
         (desired-width (or (getf props :max-width) available-width))
         (clamped-width (max 2 (min available-width desired-width)))
         ;; Left-align instead of center: use x directly without offset
         (draw-x x)
         (measured-height
           (ptui.layout:layout-size-height
            (ptui.widgets.core:widget-measure element clamped-width available-height)))
         (draw-height (max 0 (min available-height measured-height))))
    (values draw-x clamped-width draw-height)))

(defun %paint-prompt-lines (buffer inner-x inner-y inner-w visible-lines)
  (loop for line in visible-lines
        for row from 0 do
          (ptui.render.buffer:buffer-draw-text
           buffer
           inner-x
           (+ inner-y row)
           line
           :max-width inner-w)))

(defun %paint-prompt-cursor (buffer lines value cursor-pos-raw cursor-visible-p inner-x inner-y inner-w
                             inner-h effective-offset)
  (when cursor-visible-p
    (let* ((cursor-pos (%prompt-cursor-pos value cursor-pos-raw))
           (cursor-cell (ptui.core.types:make-cell
                         " "
                         nil
                         nil
                         (ptui.core.types:make-attrs :boldp t :invertp t))))
      (multiple-value-bind (cursor-line cursor-col)
          (%cursor-to-line-col cursor-pos lines)
        (let ((visible-line (- cursor-line (or effective-offset 0))))
          (when (and (>= visible-line 0) (< visible-line inner-h))
            (let* ((cx (+ inner-x cursor-col))
                   (cy (+ inner-y visible-line))
                   (line (nth cursor-line lines))
                   (glyph (if (and line (< cursor-col (length line)))
                              (string (char line cursor-col))
                              " ")))
              (when (and (>= cx inner-x)
                         (< cx (+ inner-x inner-w))
                         (>= cy inner-y)
                         (< cy (+ inner-y inner-h)))
                (let ((cell (ptui.core.types:make-cell
                             glyph
                             (ptui.core.types:cell-fg cursor-cell)
                             (ptui.core.types:cell-bg cursor-cell)
                             (ptui.core.types:cell-attrs cursor-cell)))
                      (clip-rect (ptui.core.types:make-rect
                                  inner-x inner-y inner-w inner-h)))
                  (ptui.render.buffer:write-cell-if-visible
                   buffer
                   cx
                   cy
                   cell
                   clip-rect))))))))))

(defun %paint-prompt-box-element (element buffer x y max-cols max-rows)
  (let* ((props (ptui.ui.elements:ui-element-props element))
         (value (or (getf props :value) ""))
         (scroll-offset (getf props :scroll-offset nil))
         (cursor-pos-raw (getf props :cursor-position nil))
         (cursor-visible-p (getf props :cursor-visible-p t)))
    (multiple-value-bind (draw-x desired-width draw-height)
        (%prompt-draw-geometry element props x y max-cols max-rows)
      (when (and (> desired-width 1) (> draw-height 1))
        (let* ((rect (ptui.core.types:make-rect draw-x y desired-width draw-height))
               (inner-x (1+ draw-x))
               (inner-y (1+ y))
               (inner-w (max 0 (- desired-width 2)))
               (inner-h (max 0 (- draw-height 2)))
               (lines (%prompt-wrapped-lines value inner-w)))
          (ptui.render.buffer:buffer-draw-border
           buffer rect :border-style (%prompt-border-style props))
          (multiple-value-bind (visible-lines effective-offset max-offset)
              (%prompt-visible-lines lines inner-h scroll-offset)
            (declare (ignore max-offset))
            (%paint-prompt-lines buffer inner-x inner-y inner-w visible-lines)
            (%paint-prompt-cursor buffer
                                  lines
                                  value
                                  cursor-pos-raw
                                  cursor-visible-p
                                  inner-x
                                  inner-y
                                  inner-w
                                  inner-h
                                  effective-offset)))))))

(defun %paint-scroll-element (element buffer x y max-cols max-rows)
  (let* ((props (ptui.ui.elements:ui-element-props element))
         (viewport-width (or (getf props :viewport-width) (- max-cols x)))
         (viewport-height (or (getf props :viewport-height) (- max-rows y)))
         (raw-offset (max 0 (or (getf props :offset) 0)))
         (scroll-bar (getf props :scroll-bar))
         (child (first (ptui.ui.elements:ui-element-children element)))
         (content-width (max 0 (1- viewport-width)))
         (content-height (if child
                             (ptui.layout:layout-size-height
                              (ptui.widgets.core:widget-measure child content-width nil))
                             0))
         ;; Clamp offset so we never scroll past the content
         (max-offset (max 0 (- content-height viewport-height)))
         (offset (min raw-offset max-offset))
         (show-scrollbar (and scroll-bar child
                              (> content-height viewport-height)))
         (bar-col-reserve (if show-scrollbar 1 0))
         (clip-width (max 0 (min (- viewport-width bar-col-reserve) (- max-cols x))))
         (clip-height (max 0 (min viewport-height (- max-rows y)))))
    (when (and child (> clip-width 0) (> clip-height 0))
      (ptui.render.buffer:with-clip
          (buffer (ptui.core.types:make-rect x y clip-width clip-height))
        (%paint-element child
                        buffer
                        x
                        (- y offset)
                        (+ x clip-width)
                        (+ y clip-height))))
    ;; Draw scrollbar in the reserved column
    (when (and show-scrollbar (> clip-height 0))
      (let* ((bar-x (+ x clip-width))
             (max-scroll (max 1 (- content-height clip-height)))
             (thumb-size (max 1 (floor (* clip-height clip-height) content-height)))
             (thumb-top (floor (* offset (- clip-height thumb-size)) max-scroll))
             (track-cell (ptui.core.types:make-cell
                          "│" nil nil
                          (ptui.core.types:make-attrs :dimp t)))
             (thumb-cell (ptui.core.types:make-cell
                          "█" nil nil
                          (ptui.core.types:make-attrs)))
             (clip-rect (ptui.core.types:make-rect bar-x y 1 clip-height)))
        ;; Draw track
        (loop for row from 0 below clip-height
              do (ptui.render.buffer:write-cell-if-visible
                  buffer bar-x (+ y row) track-cell clip-rect))
        ;; Draw thumb over track
        (loop for row from thumb-top below (min (+ thumb-top thumb-size) clip-height)
              do (ptui.render.buffer:write-cell-if-visible
                  buffer bar-x (+ y row) thumb-cell clip-rect))))))

(defun %paint-constraint-layout-element (element buffer x y max-cols max-rows)
  (%paint-constraint-layout element buffer x y max-cols max-rows))

(defun %paint-panel-element (element buffer x y max-cols max-rows)
  (%paint-constraint-layout element buffer x y max-cols max-rows))

(defun %paint-generic-children (element buffer x y max-cols max-rows)
  (let ((offset-y y))
    (dolist (child (ptui.ui.elements:ui-element-children element))
      (when (< offset-y max-rows)
        (%paint-element child buffer x offset-y max-cols max-rows)
        (incf offset-y
              (ptui.layout:layout-size-height
               (ptui.widgets.core:widget-measure child
                                                (max 0 (- max-cols x))
                                                (max 0 (- max-rows offset-y)))))))))

(defun %resolve-paint-handler (type)
  (or (gethash type *view-paint-registry*)
      (let ((handler-symbol (cdr (assoc type +built-in-painters+ :test #'eq))))
        (and handler-symbol
             (symbol-function handler-symbol)))))

(defun %paint-element (element buffer x y max-cols max-rows)
  "Recursively paint element tree into buffer. Simple top-down layout."
  (let ((handler (%resolve-paint-handler
                  (ptui.ui.elements:ui-element-type element))))
    (if handler
        (funcall handler element buffer x y max-cols max-rows)
        (%paint-generic-children element buffer x y max-cols max-rows))))

(defun %paint-constraint-layout (element buffer x y max-cols max-rows)
  "Paint an element with constraint-based layout.
Uses constraint specs from :constraints prop to allocate space to children.
Supports :padding (container-level inset) and :gutters (per-region left inset)."
  (let* ((props (ptui.ui.elements:ui-element-props element))
         (constraints (getf props :constraints))
         (direction (getf props :direction :column))
         (children (ptui.ui.elements:ui-element-children element))
         ;; Padding: (top right bottom left) or nil
         (padding (getf props :padding))
         (pad-top    (if padding (first padding) 0))
         (pad-right  (if padding (second padding) 0))
         (pad-bottom (if padding (third padding) 0))
         (pad-left   (if padding (fourth padding) 0))
         ;; Gutters: alist of (region-id . gutter-width) or nil
         (gutters (getf props :gutters))
         ;; Effective dimensions after padding
         (eff-max-cols (max 0 (- max-cols pad-left pad-right)))
         (eff-max-rows (max 0 (- max-rows pad-top pad-bottom)))
         (main-available (case direction
                           (:column eff-max-rows)
                           (:row eff-max-cols))))
    (if (null constraints)
        ;; Fallback to generic layout
        (let ((offset-y (+ y pad-top)))
          (dolist (child children)
            (when (< offset-y (- max-rows pad-bottom))
              (%paint-element child buffer (+ x pad-left) offset-y
                              (+ x pad-left eff-max-cols) (- max-rows pad-bottom))
              (let ((size (ptui.widgets.core:widget-measure child eff-max-cols nil)))
                (incf offset-y (ptui.layout:layout-size-height size))))))
        ;; Solve constraints and paint
        (let ((solved (ptui.layout.solver:solve-constraints constraints main-available))
              (cursor-x (+ x pad-left))
              (cursor-y (+ y pad-top)))
          (loop for (region-id . allocated) in solved
                for child = (find region-id children
                                  :key (lambda (c)
                                         (or (ptui.ui.elements:ui-element-id c)
                                             (ptui.ui.elements:ui-element-key c))))
                for gutter-w = (or (cdr (assoc region-id gutters :test #'eq)) 0)
                do (when child
                     (let* ((child-x (case direction
                                       (:column (+ cursor-x gutter-w))
                                       (:row (+ cursor-x gutter-w))))
                            (child-w (case direction
                                       (:column (max 0 (- eff-max-cols gutter-w)))
                                       (:row (max 0 (- allocated gutter-w)))))
                            (child-h (case direction
                                       (:column allocated)
                                       (:row eff-max-rows))))
                       (ptui.render.buffer:with-clip
                           (buffer (ptui.core.types:make-rect
                                    child-x cursor-y child-w child-h))
                         (%paint-element child buffer child-x cursor-y
                                         (+ child-x child-w)
                                         (+ cursor-y child-h)))))
                   (case direction
                     (:column (incf cursor-y allocated))
                     (:row (incf cursor-x allocated))))))))

;;; ===================================================================
;;; I275+I277: defapp Macro
;;; ===================================================================

(defmacro defapp (name (&key (backend :auto) (fps 20)
                              initial-state on-mount on-unmount
                              interceptors)
                  &body root-widget-form)
  "Define a PTUI application. Generates RUN-<NAME> function.
ROOT-WIDGET-FORM is evaluated each frame to produce the root element tree."
  (let* ((app-package (or (symbol-package name) *package*))
         (run-name (intern (format nil "RUN-~A" (symbol-name name)) app-package)))
    `(defun ,run-name (&key (override-backend nil override-backend-p))
       (let* ((runtime (ptui.ui.runtime:make-runtime))
              (ptui.ui.runtime:*current-runtime* runtime)
              (effective-backend (if override-backend-p override-backend ,backend))
              (root-fn (lambda ()
                         (let ((ptui.ui.runtime:*current-runtime* runtime))
                           ,@root-widget-form)))
              (event-handler
                (%make-app-event-handler runtime root-fn ,interceptors))
              (render-fn (%make-app-render-fn runtime root-fn)))
         ;; I277: Pre-populate initial state
         ,@(when initial-state
             `((%populate-initial-state runtime ,initial-state)))
         ;; I277: Lifecycle — on-mount runs as first effect, on-unmount in unwind-protect
         ,@(when on-mount
             `((ptui.ui.runtime:enqueue-effect runtime ,on-mount)))
         (unwind-protect
              (ptui.engine.loop:run
               render-fn
               :backend effective-backend
               :fps ,fps
               :on-event event-handler)
           ;; I277: on-unmount cleanup
           ,@(when on-unmount
               `((ignore-errors (funcall ,on-unmount)))))))))
