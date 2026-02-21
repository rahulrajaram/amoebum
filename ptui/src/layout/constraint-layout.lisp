(defpackage :ptui.layout.constraint-layout
  (:use :cl)
  (:export
   #:constraint-layout-node
   #:constraint-layout-node-p
   #:make-constraint-layout-node
   #:constraint-layout-node-id
   #:constraint-layout-node-direction
   #:constraint-layout-node-constraints
   #:constraint-layout-node-children
   #:compute-constraint-layout))

(in-package :ptui.layout.constraint-layout)

;;; ===================================================================
;;; I283: Constraint Layout Nodes
;;; ===================================================================

(defstruct (constraint-layout-node
            (:constructor %make-constraint-layout-node
                (&key id direction constraints children)))
  id
  (direction :column :type keyword)
  (constraints '() :type list)
  (children '() :type list))

(defun make-constraint-layout-node (&key id (direction :column) constraints children)
  "Create a layout node whose children are sized by constraint specs rather than flex.
CONSTRAINTS is a list of constraint-specs (one per child, matched by id).
CHILDREN is a list of layout-nodes or constraint-layout-nodes."
  (unless (member direction '(:row :column))
    (error "DIRECTION must be :row or :column. Got: ~S" direction))
  (dolist (child children)
    (unless (or (typep child 'ptui.layout:layout-node)
                (typep child 'constraint-layout-node))
      (error "CHILDREN must contain layout-node or constraint-layout-node values. Got: ~S" child)))
  (dolist (c constraints)
    (unless (typep c 'ptui.layout.constraints:constraint-spec)
      (error "CONSTRAINTS must contain constraint-spec values. Got: ~S" c)))
  (%make-constraint-layout-node
   :id id :direction direction :constraints constraints :children children))

(defun %child-id (child)
  (if (typep child 'constraint-layout-node)
      (constraint-layout-node-id child)
      (ptui.layout:layout-node-id child)))

(defun %constraint-child-by-id (children target-id)
  "Find child by id."
  (find target-id children :key #'%child-id))

(defun %measure-any-node (node available-width available-height)
  "Measure either a layout-node or constraint-layout-node."
  (if (typep node 'constraint-layout-node)
      (%measure-constraint-node node available-width available-height)
      (ptui.layout::%measure-node node available-width available-height)))

(defun %measure-constraint-node (node available-width available-height)
  "Measure a constraint-layout-node by solving constraints for main axis."
  (let* ((direction (constraint-layout-node-direction node))
         (constraints (constraint-layout-node-constraints node))
         (main-available (case direction
                           (:column (or available-height 0))
                           (:row (or available-width 0))))
         (solved (ptui.layout.solver:solve-constraints constraints main-available))
         (total-main 0)
         (max-cross 0))
    (dolist (entry solved)
      (let* ((child-id (car entry))
             (child-main (cdr entry))
             (child (%constraint-child-by-id
                     (constraint-layout-node-children node) child-id)))
        (incf total-main child-main)
        (when child
          (let ((child-size (case direction
                              (:column (%measure-any-node child available-width child-main))
                              (:row (%measure-any-node child child-main available-height)))))
            (setf max-cross (max max-cross
                                 (case direction
                                   (:column (ptui.layout:layout-size-width child-size))
                                   (:row (ptui.layout:layout-size-height child-size)))))))))
    (case direction
      (:column (ptui.layout:make-layout-size (or available-width max-cross) total-main))
      (:row (ptui.layout:make-layout-size total-main (or available-height max-cross))))))

(defun %place-any-node (node x y width height out)
  "Place either a layout-node or constraint-layout-node."
  (if (typep node 'constraint-layout-node)
      (%place-constraint-node node x y width height out)
      (ptui.layout::%place-node node x y width height out)))

(defun %place-constraint-node (node x y width height out)
  "Place a constraint-layout-node using constraint solver."
  (setf (gethash (constraint-layout-node-id node) out)
        (ptui.layout:make-layout-bounds x y width height))
  (let* ((direction (constraint-layout-node-direction node))
         (constraints (constraint-layout-node-constraints node))
         (main-available (case direction
                           (:column height)
                           (:row width)))
         (solved (ptui.layout.solver:solve-constraints constraints main-available))
         (cursor-x x)
         (cursor-y y))
    (dolist (entry solved)
      (let* ((child-id (car entry))
             (child-main (cdr entry))
             (child (%constraint-child-by-id
                     (constraint-layout-node-children node) child-id)))
        (when child
          (let ((child-width (case direction (:column width) (:row child-main)))
                (child-height (case direction (:column child-main) (:row height))))
            (%place-any-node child cursor-x cursor-y child-width child-height out)))
        (case direction
          (:column (incf cursor-y child-main))
          (:row (incf cursor-x child-main))))))
  out)

(defun compute-constraint-layout (root &key width height (x 0) (y 0))
  "Compute layout bounds for a constraint-layout-node tree.
Returns a hash-table keyed by node id with layout-bounds values."
  (unless (or (typep root 'ptui.layout:layout-node)
              (typep root 'constraint-layout-node))
    (error "ROOT must be a layout-node or constraint-layout-node. Got: ~S" root))
  (let* ((root-size (%measure-any-node root width height))
         (layout (make-hash-table :test #'equal)))
    (%place-any-node root
                     x y
                     (ptui.layout:layout-size-width root-size)
                     (ptui.layout:layout-size-height root-size)
                     layout)))
