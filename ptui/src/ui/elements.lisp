(defpackage :ptui.ui.elements
  (:use :cl)
  (:export
   #:ui-element
   #:make-element
   #:ui-element-type
   #:ui-element-id
   #:ui-element-key
   #:ui-element-props
   #:ui-element-children
   #:ui-element-focusablep))

(in-package :ptui.ui.elements)

(defstruct (ui-element
            (:constructor %make-element
                (&key type id key props children (focusablep nil))))
  (type :node)
  (id nil)
  (key nil)
  (props '() :type list)
  (children '() :type list)
  (focusablep nil :type boolean))

(defun %reconcile-selector (element)
  (or (ui-element-key element)
      (ui-element-id element)))

(defun make-element (type &key id key (props '()) (children '()) (focusablep nil))
  "Create a UI element node with deterministic children ordering."
  (let ((selectors (make-hash-table :test #'equal)))
    (dolist (child children)
      (unless (typep child 'ui-element)
        (error "Children must be UI-ELEMENT values. Got: ~S" child))
      (let ((selector (%reconcile-selector child)))
        (when selector
          (when (gethash selector selectors)
            (error "Duplicate child selector ~S under parent ~S. Add unique :key/:id values."
                   selector
                   (or id key type)))
          (setf (gethash selector selectors) t)))))
  (%make-element
   :type type
   :id id
   :key key
   :props props
   :children children
   :focusablep (not (null focusablep))))
