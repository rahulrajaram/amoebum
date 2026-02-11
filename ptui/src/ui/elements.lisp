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

(defun make-element (type &key id key (props '()) (children '()) (focusablep nil))
  "Create a UI element node with deterministic children ordering."
  (dolist (child children)
    (unless (typep child 'ui-element)
      (error "Children must be UI-ELEMENT values. Got: ~S" child)))
  (%make-element
   :type type
   :id id
   :key key
   :props props
   :children children
   :focusablep (not (null focusablep))))
