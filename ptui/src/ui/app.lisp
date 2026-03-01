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
       (let* ((padding (or (getf props :padding) 0))
              (child (first (ptui.ui.elements:ui-element-children element))))
         (when child
           (%paint-element child buffer (+ x padding) (+ y padding) max-cols max-rows))))
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
