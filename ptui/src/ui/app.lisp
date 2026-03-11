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
  "Create event handler with auto-cache, focus management, and interceptors."
  (declare (ignore root-widget-fn))
  (let ((sorted-interceptors (%sorted-interceptors interceptors)))
    (lambda (state event)
      (declare (ignore state))
      ;; I280: Run interceptors first
      (unless (%run-interceptors sorted-interceptors event)
        ;; Tab key → focus management (I276)
        (let ((route (ptui.ui.runtime:route-event runtime event)))
          (when (and (runtime-root-present-p runtime)
                     (eq (getf route :kind) :key))
            (ptui.widgets.core:dispatch-widget-event
             (ptui.ui.runtime:runtime-root runtime) route))))
      ;; Return nil — state is managed via runtime state-table
      nil)))

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
         (buffer (ptui.render.buffer:make-buffer cols rows)))
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

(defun %paint-element (element buffer x y max-cols max-rows)
  "Recursively paint element tree into buffer. Simple top-down layout."
  (let ((type (ptui.ui.elements:ui-element-type element))
        (props (ptui.ui.elements:ui-element-props element)))
    ;; Check for registered view painters first
    (let ((view-painter (gethash type *view-paint-registry*)))
      (when view-painter
        (funcall view-painter element buffer x y max-cols max-rows)
        (return-from %paint-element)))
    (case type
      (:text
       (let* ((styled-segments (getf props :styled-segments))
              (text (or (getf props :text) ""))
              (payload (or styled-segments text)))
         (when (and (< y max-rows) (< x max-cols))
           (ptui.render.buffer:buffer-draw-text
            buffer x y payload :max-width (- max-cols x)))))
      (:input
       (let ((text (or (getf props :value) "")))
         (when (and (< y max-rows) (< x max-cols))
           (ptui.render.buffer:buffer-draw-text
            buffer x y text :max-width (- max-cols x)))))
      (:stack
       (let ((direction (getf props :direction :column))
             (gap (or (getf props :gap) 0))
             (offset-x x)
             (offset-y y))
         (dolist (child (ptui.ui.elements:ui-element-children element))
           (when (< offset-y max-rows)
             (%paint-element child buffer offset-x offset-y max-cols max-rows)
             (let ((size (ptui.widgets.core:widget-measure child)))
               (case direction
                 (:column (incf offset-y (+ (ptui.layout:layout-size-height size) gap)))
                 (:row (incf offset-x (+ (ptui.layout:layout-size-width size) gap)))))))))
      (:box
       (let* ((padding (max 0 (or (getf props :padding) 0)))
              (border (getf props :border nil))
              (borderp (and border (not (eq border :none))))
              (border-width (if borderp 1 0))
              (inset (+ padding border-width))
              (available-width (max 0 (- max-cols x)))
              (available-height (max 0 (- max-rows y)))
              (measured (ptui.widgets.core:widget-measure element))
              (draw-width (max 0 (min available-width
                                      (ptui.layout:layout-size-width measured))))
              (draw-height (max 0 (min available-height
                                       (ptui.layout:layout-size-height measured))))
              (child (first (ptui.ui.elements:ui-element-children element))))
        (when (and borderp (> draw-width 1) (> draw-height 1))
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
        (when (and child (> draw-width (* 2 inset)) (> draw-height (* 2 inset)))
          (let* ((inner-x (+ x inset))
                  (inner-y (+ y inset))
                  (inner-width (max 0 (- draw-width (* 2 inset))))
                  (inner-height (max 0 (- draw-height (* 2 inset)))))
             (ptui.render.buffer:with-clip
                 (buffer (ptui.core.types:make-rect inner-x inner-y inner-width inner-height))
               (%paint-element child
                               buffer
                               inner-x
                               inner-y
                               (+ inner-x inner-width)
                               (+ inner-y inner-height)))))))
      (:prompt-box
       (let* ((value (or (getf props :value) ""))
              (scroll-offset (getf props :scroll-offset nil))
              (cursor-pos-raw (getf props :cursor-position nil))
              (border-style (if (eq (getf props :border-style :rounded) :square)
                                :square
                                :rounded))
              (available-width (max 0 (- max-cols x)))
              (available-height (max 0 (- max-rows y)))
              (desired-width (or (getf props :max-width) available-width))
              (desired-width (max 2 (min available-width desired-width)))
              (draw-x (+ x (max 0 (floor (- available-width desired-width) 2))))
              (measured-height
                (ptui.layout:layout-size-height
                 (ptui.widgets.core:widget-measure element)))
              (draw-height (max 0 (min available-height measured-height))))
         (when (and (> desired-width 1) (> draw-height 1))
           (let* ((rect (ptui.core.types:make-rect draw-x y desired-width draw-height))
                  (inner-x (1+ draw-x))
                  (inner-y (1+ y))
                  (inner-w (max 0 (- desired-width 2)))
                  (inner-h (max 0 (- draw-height 2)))
                  (lines (%prompt-wrapped-lines value inner-w)))
             (ptui.render.buffer:buffer-draw-border
              buffer rect :border-style border-style)
             (multiple-value-bind (visible-lines effective-offset max-offset)
                 (%prompt-visible-lines lines inner-h scroll-offset)
               (declare (ignore max-offset))
               (loop for line in visible-lines
                     for row from 0 do
                       (ptui.render.buffer:buffer-draw-text
                        buffer
                        inner-x
                        (+ inner-y row)
                        line
                        :max-width inner-w))
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
                           (ptui.render.buffer:write-cell-if-visible
                            buffer
                            cx
                            cy
                            (ptui.core.types:make-cell
                             glyph
                             (ptui.core.types:cell-fg cursor-cell)
                             (ptui.core.types:cell-bg cursor-cell)
                             (ptui.core.types:cell-attrs cursor-cell))
                            (ptui.core.types:make-rect
                             inner-x inner-y inner-w inner-h)))))))))))))
      (:scroll
       (let* ((viewport-width (or (getf props :viewport-width) (- max-cols x)))
              (viewport-height (or (getf props :viewport-height) (- max-rows y)))
              (offset (max 0 (or (getf props :offset) 0)))
              (clip-width (max 0 (min viewport-width (- max-cols x))))
              (clip-height (max 0 (min viewport-height (- max-rows y))))
              (child (first (ptui.ui.elements:ui-element-children element))))
         (when (and child (> clip-width 0) (> clip-height 0))
           (ptui.render.buffer:with-clip
               (buffer (ptui.core.types:make-rect x y clip-width clip-height))
             (%paint-element child
                             buffer
                             x
                             (- y offset)
                             (+ x clip-width)
                             (+ y clip-height))))))
      (:constraint-layout
       (%paint-constraint-layout element buffer x y max-cols max-rows))
      (:panel
       (%paint-constraint-layout element buffer x y max-cols max-rows))
      (t
       ;; Generic: paint children vertically
       (let ((offset-y y))
         (dolist (child (ptui.ui.elements:ui-element-children element))
           (when (< offset-y max-rows)
             (%paint-element child buffer x offset-y max-cols max-rows)
             (let ((size (ptui.widgets.core:widget-measure child)))
               (incf offset-y (ptui.layout:layout-size-height size))))))))))

(defun %paint-constraint-layout (element buffer x y max-cols max-rows)
  "Paint an element with constraint-based layout.
Uses constraint specs from :constraints prop to allocate space to children."
  (let* ((props (ptui.ui.elements:ui-element-props element))
         (constraints (getf props :constraints))
         (direction (getf props :direction :column))
         (children (ptui.ui.elements:ui-element-children element))
         (main-available (case direction
                           (:column max-rows)
                           (:row max-cols))))
    (if (null constraints)
        ;; Fallback to generic layout
        (let ((offset-y y))
          (dolist (child children)
            (when (< offset-y max-rows)
              (%paint-element child buffer x offset-y max-cols max-rows)
              (let ((size (ptui.widgets.core:widget-measure child)))
                (incf offset-y (ptui.layout:layout-size-height size))))))
        ;; Solve constraints and paint
        (let ((solved (ptui.layout.solver:solve-constraints constraints main-available))
              (cursor-x x)
              (cursor-y y))
          (loop for (region-id . allocated) in solved
                for child = (find region-id children
                                  :key (lambda (c)
                                         (or (ptui.ui.elements:ui-element-id c)
                                             (ptui.ui.elements:ui-element-key c))))
                do (when child
                     (let ((child-w (case direction (:column max-cols) (:row allocated)))
                           (child-h (case direction (:column allocated) (:row max-rows))))
                       (ptui.render.buffer:with-clip
                           (buffer (ptui.core.types:make-rect
                                    cursor-x cursor-y child-w child-h))
                         (%paint-element child buffer cursor-x cursor-y
                                         (+ cursor-x child-w)
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
