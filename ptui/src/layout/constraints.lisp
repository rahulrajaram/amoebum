(defpackage :ptui.layout.constraints
  (:use :cl)
  (:export
   #:constraint-spec
   #:make-constraint-spec
   #:constraint-spec-id
   #:constraint-spec-kind
   #:constraint-spec-value
   #:constraint-spec-min-value
   #:constraint-spec-max-value
   #:constraint-spec-priority
   #:constraint-spec-weight
   #:constraint-spec-metadata
   ;; Builder functions
   #:fixed
   #:percentage
   #:flex
   #:dock))

(in-package :ptui.layout.constraints)

(defstruct (constraint-spec
            (:constructor %make-constraint-spec
                (&key id kind value min-value max-value priority weight metadata)))
  (id nil :type symbol)
  (kind :fixed :type keyword)
  (value nil :type (or null number))
  (min-value nil :type (or null number))
  (max-value nil :type (or null number))
  (priority 10 :type fixnum)
  (weight 1 :type number)
  (metadata nil))

(defun make-constraint-spec (&key id kind value min-value max-value (priority 10) (weight 1)
                               metadata)
  "Construct a `constraint-spec` with explicit field values."
  (%make-constraint-spec :id id :kind kind :value value :min-value min-value
                         :max-value max-value :priority priority :weight weight
                         :metadata metadata))

(defun fixed (id pixels)
  "Create a fixed-size constraint spec. Allocated first (priority 10)."
  (check-type id symbol)
  (check-type pixels (integer 0 *))
  (%make-constraint-spec :id id :kind :fixed :value pixels :priority 10))

(defun percentage (id pct)
  "Create a percentage-based constraint spec. Allocated second (priority 20)."
  (check-type id symbol)
  (check-type pct (real 0 100))
  (%make-constraint-spec :id id :kind :percentage :value pct :priority 20))

(defun flex (id &key min max (weight 1))
  "Create a flex constraint spec. Allocated last (priority 30)."
  (check-type id symbol)
  (when min (check-type min (integer 0 *)))
  (when max (check-type max (integer 0 *)))
  (check-type weight (real (0) *))
  (let ((kind (if (or min max) :min-max :flex)))
    (%make-constraint-spec :id id :kind kind :value nil
                           :min-value min :max-value max
                           :priority 30 :weight weight)))

(defun dock (id side pixels)
  "Create a fixed constraint with dock metadata (shorthand for fixed + dock side)."
  (check-type id symbol)
  (check-type side (member :top :bottom :left :right))
  (check-type pixels (integer 0 *))
  (%make-constraint-spec :id id :kind :fixed :value pixels :priority 10
                         :metadata (list :dock side)))
