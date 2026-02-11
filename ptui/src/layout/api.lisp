(defpackage :ptui.layout
  (:use :cl)
  (:export
   #:layout-size
   #:make-layout-size
   #:layout-size-width
   #:layout-size-height
   #:layout-bounds
   #:make-layout-bounds
   #:layout-bounds-x
   #:layout-bounds-y
   #:layout-bounds-width
   #:layout-bounds-height
   #:layout-node
   #:make-layout-node
   #:layout-node-id
   #:layout-node-direction
   #:layout-node-width
   #:layout-node-height
   #:layout-node-gap
   #:layout-node-children
   #:layout-node-measure
   #:compute-layout
   #:layout-bound
   #:layout->alist))

(in-package :ptui.layout)

(deftype non-negative-integer ()
  '(integer 0 *))

(defstruct (layout-size (:constructor make-layout-size (width height)))
  (width 0 :type non-negative-integer)
  (height 0 :type non-negative-integer))

(defstruct (layout-bounds (:constructor make-layout-bounds (x y width height)))
  (x 0 :type integer)
  (y 0 :type integer)
  (width 0 :type non-negative-integer)
  (height 0 :type non-negative-integer))

(defstruct (layout-node
            (:constructor %make-layout-node
                (&key id direction width height gap children measure)))
  id
  (direction :column :type keyword)
  (width nil :type (or null non-negative-integer))
  (height nil :type (or null non-negative-integer))
  (gap 0 :type non-negative-integer)
  (children '() :type list)
  (measure nil :type (or null function)))

(defun make-layout-node (&key id (direction :column) width height (gap 0) (children '()) measure)
  "Create a layout node with strict direction and sizing contracts."
  (unless (member direction '(:row :column))
    (error "DIRECTION must be :row or :column. Got: ~S" direction))
  (when (and width (minusp width))
    (error "WIDTH must be non-negative or NIL. Got: ~S" width))
  (when (and height (minusp height))
    (error "HEIGHT must be non-negative or NIL. Got: ~S" height))
  (when (minusp gap)
    (error "GAP must be non-negative. Got: ~S" gap))
  (dolist (child children)
    (unless (typep child 'layout-node)
      (error "CHILDREN must contain layout-node values. Got: ~S" child)))
  (when measure
    (check-type measure function))
  (%make-layout-node
   :id id
   :direction direction
   :width width
   :height height
   :gap gap
   :children children
   :measure measure))

(defun %call-measure (node available-width available-height)
  (let ((measure (layout-node-measure node)))
    (if (null measure)
        nil
        (let ((value (funcall measure available-width available-height)))
          (unless (typep value 'layout-size)
            (error "Measure for node ~S returned non-layout-size: ~S"
                   (layout-node-id node)
                   value))
          value))))

(defun %measure-node (node available-width available-height)
  (let* ((children (layout-node-children node))
         (direction (layout-node-direction node))
         (gap (layout-node-gap node))
         (measured (%call-measure node available-width available-height))
         (measured-width (and measured (layout-size-width measured)))
         (measured-height (and measured (layout-size-height measured)))
         (content-width 0)
         (content-height 0))
    (when children
      (let ((count 0)
            (main-sum 0)
            (cross-max 0))
        (dolist (child children)
          (incf count)
          (let* ((child-size
                   (case direction
                     (:row (%measure-node child nil available-height))
                     (:column (%measure-node child available-width nil))))
                 (cw (layout-size-width child-size))
                 (ch (layout-size-height child-size)))
            (case direction
              (:row
               (incf main-sum cw)
               (setf cross-max (max cross-max ch)))
              (:column
               (incf main-sum ch)
               (setf cross-max (max cross-max cw))))))
        (let ((gap-total (* (max 0 (1- count)) gap)))
          (case direction
            (:row
             (setf content-width (+ main-sum gap-total)
                   content-height cross-max))
            (:column
             (setf content-width cross-max
                   content-height (+ main-sum gap-total)))))))
    (make-layout-size
     (or (layout-node-width node)
         measured-width
         available-width
         content-width)
     (or (layout-node-height node)
         measured-height
         available-height
         content-height))))

(defun %place-node (node x y width height out)
  (setf (gethash (layout-node-id node) out)
        (make-layout-bounds x y width height))
  (let ((children (layout-node-children node)))
    (when children
      (let ((cursor-x x)
            (cursor-y y)
            (gap (layout-node-gap node))
            (direction (layout-node-direction node)))
        (dolist (child children)
          (let* ((child-size
                   (case direction
                     (:row (%measure-node child nil height))
                     (:column (%measure-node child width nil))))
                 (child-width (layout-size-width child-size))
                 (child-height (layout-size-height child-size)))
            (%place-node child cursor-x cursor-y child-width child-height out)
            (case direction
              (:row
               (incf cursor-x (+ child-width gap)))
              (:column
               (incf cursor-y (+ child-height gap)))))))))
  out)

(defun compute-layout (root &key width height (x 0) (y 0))
  "Compute node bounds in a deterministic pass.
Returns a hash-table keyed by node id with layout-bounds values."
  (check-type root layout-node)
  (let* ((root-size (%measure-node root width height))
         (layout (make-hash-table :test #'equal)))
    (%place-node root
                 x
                 y
                 (layout-size-width root-size)
                 (layout-size-height root-size)
                 layout)))

(defun layout-bound (layout node-id)
  (check-type layout hash-table)
  (gethash node-id layout))

(defun layout->alist (layout)
  "Convert a layout hash-table to sorted (id x y w h) rows for golden tests."
  (check-type layout hash-table)
  (let ((rows '()))
    (maphash (lambda (id bounds)
               (push (list id
                           (layout-bounds-x bounds)
                           (layout-bounds-y bounds)
                           (layout-bounds-width bounds)
                           (layout-bounds-height bounds))
                     rows))
             layout)
    (sort rows #'string<
          :key (lambda (row)
                 (princ-to-string (first row))))))
