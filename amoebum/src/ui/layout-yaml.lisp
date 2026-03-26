(in-package :amoebum)

;;; ============================================================================
;;; Amoebum YAML Layout Integration — Borrowed from PTUI's layout system
;;; ============================================================================
;;;
;;; This module provides declarative layout configuration via YAML,
;;; supporting constraint-based layouts similar to PTUI's preview system.

;;; ----------------------------------------------------------------------------
;;; Layout Structure Definitions
;;; ----------------------------------------------------------------------------

(defstruct (yaml-layout-child
            (:constructor make-yaml-layout-child
                (&key name height width border focusable focus-order
                      overflow-x overflow-y scroll-follow scroll-bar
                      padding gutter visible fill-weight)))
  "Represents a single child in a YAML layout definition."
  (name nil :type (or null string))
  (height nil :type (or null string integer))  ; "fill", "content", or integer
  (width nil :type (or null string integer))
  (border nil :type (or null string))          ; "none", "single", "double", "rounded"
  (focusable nil :type boolean)
  (focus-order nil :type (or null integer))
  (overflow-x nil :type (or null string))
  (overflow-y nil :type (or null string))
  (scroll-follow nil :type boolean)
  (scroll-bar nil :type (or null string))      ; "auto", "always", "never"
  (padding nil :type (or null integer list))   ; int or [top right bottom left]
  (gutter nil :type (or null integer list))    ; int or [left right]
  (visible t :type boolean)
  (fill-weight 1.0 :type real))

(defstruct (yaml-layout
            (:constructor make-yaml-layout
                (&key direction children padding)))
  "Represents a complete YAML layout definition."
  (direction :column :type (member :column :row))
  (children '() :type list)  ; list of yaml-layout-child
  (padding nil :type (or null integer list)))

;;; ----------------------------------------------------------------------------
;;; YAML Parsing Helpers
;;; ----------------------------------------------------------------------------

(defun %yaml-layout-lookup (node key)
  "Look up KEY in a YAML-parsed node (handles hash-tables and alists)."
  (cond
    ((hash-table-p node)
     (gethash key node))
    ((listp node)
     (let ((pair (assoc key node :test #'equal)))
       (when pair (cdr pair))))
    (t nil)))

(defun %yaml-layout-lookup-any (node &rest keys)
  "Try each key in order, return first non-nil value."
  (dolist (key keys)
    (let ((val (%yaml-layout-lookup node key)))
      (when val (return val)))))

(defun %yaml-hash-table->alist (ht)
  "Convert a hash-table to an alist."
  (let ((alist '()))
    (when (hash-table-p ht)
      (maphash (lambda (k v) (push (cons k v) alist)) ht))
    (nreverse alist)))

;;; ----------------------------------------------------------------------------
;;; Inset/Padding Normalization (adapted from PTUI)
;;; ----------------------------------------------------------------------------

(defun %yaml-inset-to-padding (value)
  "Convert YAML inset shorthand to (top right bottom left).
Handles: integer, [vert horiz], [top right bottom left], nil."
  (cond
    ((null value) '(0 0 0 0))
    ((integerp value) (list value value value value))
    ((and (listp value) (= (length value) 2))
     (list (first value) (second value) (first value) (second value)))
    ((and (listp value) (= (length value) 4))
     value)
    ((and (vectorp value) (= (length value) 2))
     (list (aref value 0) (aref value 1) (aref value 0) (aref value 1)))
    ((and (vectorp value) (= (length value) 4))
     (list (aref value 0) (aref value 1) (aref value 2) (aref value 3)))
    (t '(0 0 0 0))))

(defun %yaml-gutter-value (value)
  "Normalize gutter: integer -> left-only, [left right] -> left value."
  (cond
    ((null value) 0)
    ((integerp value) value)
    ((and (listp value) (>= (length value) 1)) (first value))
    ((and (vectorp value) (>= (length value) 1)) (aref value 0))
    (t 0)))

;;; ----------------------------------------------------------------------------
;;; Layout Parsing
;;; ----------------------------------------------------------------------------

(defun %parse-yaml-layout-child (child-node)
  "Parse a single child node from YAML into yaml-layout-child struct."
  (let ((name (%yaml-layout-lookup-any child-node "name"))
        (height (%yaml-layout-lookup-any child-node "height"))
        (width (%yaml-layout-lookup-any child-node "width"))
        (border (%yaml-layout-lookup-any child-node "border"
                                         "border-style"
                                         "border_style"))
        (focusable (%yaml-layout-lookup-any child-node "focusable"))
        (focus-order (%yaml-layout-lookup-any child-node "focus-order" "focus_order"))
        (overflow-x (%yaml-layout-lookup-any child-node "overflow-x" "overflow_x"))
        (overflow-y (%yaml-layout-lookup-any child-node "overflow-y" "overflow_y"))
        (scroll-follow (%yaml-layout-lookup-any child-node "scroll-follow" "scroll_follow"))
        (scrollable (%yaml-layout-lookup-any child-node "scrollable"))
        (scroll-bar (%yaml-layout-lookup-any child-node "scroll-bar" "scroll_bar" "scrollbar"))
        (padding (%yaml-layout-lookup-any child-node "padding"))
        (gutter (%yaml-layout-lookup-any child-node "gutter"))
        (visible (%yaml-layout-lookup-any child-node "visible"))
        (fill-weight (%yaml-layout-lookup-any child-node "fill-weight" "fill_weight" "weight")))
    (make-yaml-layout-child
     :name (and name (princ-to-string name))
     :height (cond
               ((integerp height) height)
               ((stringp height)
                (let ((normalized (string-downcase height)))
                  (if (string= normalized "flex")
                      "fill"
                      normalized)))
               (t nil))
     :width (cond
              ((integerp width) width)
              ((stringp width) (string-downcase width))
              (t nil))
     :border (and border (string-downcase (princ-to-string border)))
     :focusable (eq focusable t)
     :focus-order (and (integerp focus-order) focus-order)
     :overflow-x (and overflow-x (princ-to-string overflow-x))
     :overflow-y (cond
                   ((eq scrollable t) "scroll")
                   (overflow-y (princ-to-string overflow-y))
                   (t nil))
     :scroll-follow (eq scroll-follow t)
     :scroll-bar (and scroll-bar (princ-to-string scroll-bar))
     :padding (%yaml-inset-to-padding padding)
     :gutter (%yaml-gutter-value gutter)
     :visible (if visible (eq visible t) t)
     :fill-weight (if (realp fill-weight) 
                      (coerce fill-weight 'single-float) 
                      1.0))))

(defun %parse-yaml-layout-children (children-node)
  "Parse a list/vector of child nodes."
  (let ((children (cond
                    ((listp children-node) children-node)
                    ((vectorp children-node) (coerce children-node 'list))
                    (t nil))))
    (mapcar #'%parse-yaml-layout-child children)))

(defun parse-yaml-layout (yaml-data)
  "Parse a YAML layout section into yaml-layout struct.
YAML-DATA can be the full YAML parse result or just the layout section."
  (let* ((layout-section (or (%yaml-layout-lookup yaml-data "layout")
                             yaml-data))
         (direction (%yaml-layout-lookup-any layout-section "direction"))
         (children-node (%yaml-layout-lookup-any layout-section "children" "panels"))
         (padding (%yaml-layout-lookup-any layout-section "padding")))
    (make-yaml-layout
     :direction (cond
                  ((and (stringp direction)
                        (member (string-downcase direction)
                                '("horizontal" "row")
                                :test #'string=))
                   :row)
                  (t :column))
     :children (%parse-yaml-layout-children children-node)
     :padding (%yaml-inset-to-padding padding))))

;;; ----------------------------------------------------------------------------
;;; Layout Application
;;; ----------------------------------------------------------------------------

(defparameter *yaml-layout* nil
  "Currently active YAML layout, or NIL if using default layout.")

(defparameter *yaml-layout-default* nil
  "Default layout used when no YAML layout is configured.")

(defun yaml-layout-find-child (layout name)
  "Find a child by name in the layout."
  (find name (yaml-layout-children layout)
        :key #'yaml-layout-child-name
        :test #'string=))

(defun yaml-layout-child-height-px (child available-pixels)
  "Calculate pixel height for a child given available space."
  (let ((height (yaml-layout-child-height child)))
    (cond
      ((integerp height) height)
      ((and (stringp height)
            (member (string-downcase height) '("fill" "flex") :test #'string=))
       available-pixels)
      ((and (stringp height) (string= height "content")) 1)
      (t 1))))  ; default fallback

(defun %yaml-layout-find-first-child (layout names)
  "Find the first child in LAYOUT matching any of NAMES."
  (loop for name in names
        for child = (yaml-layout-find-child layout name)
        when child do (return child)))

(defun %yaml-layout-has-any-child-p (children names)
  "Return T when CHILDREN contains any child whose name matches NAMES."
  (loop for child in children
        thereis (member (yaml-layout-child-name child) names :test #'string=)))

(defun yaml-layout-apply-to-chat (chat-state &key (layout *yaml-layout*))
  "Apply a YAML layout configuration to chat state.
Updates scroll behavior, focus order, and other layout-related state."
  (when layout
    ;; Find history child and apply scroll settings
    (let ((history-child (%yaml-layout-find-first-child
                          layout
                          '("history" "message-history"))))
      (when history-child
        (setf (chat-ui-state-stream-scroll-follow-p chat-state)
              (yaml-layout-child-scroll-follow history-child))))
    layout))

;;; ----------------------------------------------------------------------------
;;; Layout Validation
;;; ----------------------------------------------------------------------------

(defun yaml-layout-validate (layout)
  "Validate a YAML layout structure.
Returns (values valid-p error-message-or-nil)."
  (cond
    ((null layout)
     (values t nil))
    ((not (yaml-layout-p layout))
     (values nil "Layout is not a valid yaml-layout struct"))
    (t
     ;; Check for required children
     (let ((children (yaml-layout-children layout)))
       (unless (%yaml-layout-has-any-child-p children '("history" "message-history"))
         (return-from yaml-layout-validate
           (values nil "Layout missing required 'history' child")))
       (unless (%yaml-layout-has-any-child-p children '("prompt" "input-prompt"))
         (return-from yaml-layout-validate
           (values nil "Layout missing required 'prompt' child")))
       (unless (%yaml-layout-has-any-child-p children '("status" "status-bar"))
         (return-from yaml-layout-validate
           (values nil "Layout missing required 'status' child"))))
     ;; Check focus orders are unique
     (let ((focus-orders '()))
       (dolist (child (yaml-layout-children layout))
         (when (yaml-layout-child-focus-order child)
           (when (member (yaml-layout-child-focus-order child) focus-orders)
             (return-from yaml-layout-validate
               (values nil (format nil "Duplicate focus-order: ~A"
                                   (yaml-layout-child-focus-order child)))))
           (push (yaml-layout-child-focus-order child) focus-orders))))
     (values t nil))))

;;; ----------------------------------------------------------------------------
;;; Default Layout (matches preview-test.tui-spec.yaml)
;;; ----------------------------------------------------------------------------

(defun make-default-yaml-layout ()
  "Create the default three-panel layout from preview-test.tui-spec.yaml."
  (make-yaml-layout
   :direction :column
   :children (list
              (make-yaml-layout-child
               :name "history"
               :height "fill"
               :overflow-y "scroll"
               :scroll-follow t
               :scroll-bar "auto"
               :gutter 1
               :padding '(0 1 0 1))
              (make-yaml-layout-child
               :name "prompt"
               :height 3
               :border "single"
               :focusable t
               :focus-order 1)
              (make-yaml-layout-child
               :name "status"
               :height 1))))

;;; Initialize default layout
(setf *yaml-layout-default* (make-default-yaml-layout))
