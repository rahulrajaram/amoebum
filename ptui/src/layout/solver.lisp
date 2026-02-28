(defpackage :ptui.layout.solver
  (:use :cl)
  (:export
   #:solve-constraints))

(in-package :ptui.layout.solver)

(defun solve-constraints (specs available-pixels)
  "Solve constraint specs against AVAILABLE-PIXELS.
Returns an alist of (id . allocated-pixels) preserving input order.

Algorithm:
1. Sort by priority (lower = allocated first)
2. Allocate :fixed — subtract from remaining
3. Allocate :percentage — compute from original available, clamp to remaining
4. Allocate :min-max — flex with min/max clamping
5. Allocate :flex — distribute remaining by weight
6. Return alist in original input order."
  (check-type available-pixels (integer 0 *))
  (let ((results (make-hash-table :test #'eq))
        (remaining available-pixels)
        (sorted (stable-sort (copy-list specs) #'<
                             :key #'ptui.layout.constraints:constraint-spec-priority)))
    ;; Pass 1: Fixed constraints
    (dolist (spec sorted)
      (when (eq (ptui.layout.constraints:constraint-spec-kind spec) :fixed)
        (let* ((id (ptui.layout.constraints:constraint-spec-id spec))
               (requested (ptui.layout.constraints:constraint-spec-value spec))
               (allocated (max 0 (min requested remaining))))
          (setf (gethash id results) allocated)
          (decf remaining allocated))))
    ;; Pass 2: Percentage constraints
    (dolist (spec sorted)
      (when (eq (ptui.layout.constraints:constraint-spec-kind spec) :percentage)
        (let* ((id (ptui.layout.constraints:constraint-spec-id spec))
               (pct (ptui.layout.constraints:constraint-spec-value spec))
               (computed (floor (* available-pixels pct) 100))
               (allocated (max 0 (min computed remaining))))
          (setf (gethash id results) allocated)
          (decf remaining allocated))))
    ;; Pass 3: Flex and min-max constraints
    (let ((flex-specs (remove-if-not
                       (lambda (s) (member (ptui.layout.constraints:constraint-spec-kind s)
                                           '(:flex :min-max)))
                       sorted)))
      (when flex-specs
        (let ((total-weight (loop for s in flex-specs
                                  sum (ptui.layout.constraints:constraint-spec-weight s))))
          (if (zerop total-weight)
              ;; All weights zero — distribute equally
              (let ((each (floor remaining (length flex-specs))))
                (dolist (spec flex-specs)
                  (let* ((id (ptui.layout.constraints:constraint-spec-id spec))
                         (allocated (%clamp-flex spec each)))
                    (setf (gethash id results) allocated)
                    (decf remaining allocated))))
              ;; Distribute by weight
              (let ((pool remaining))
                (dolist (spec flex-specs)
                  (let* ((id (ptui.layout.constraints:constraint-spec-id spec))
                         (weight (ptui.layout.constraints:constraint-spec-weight spec))
                         (share (floor (* pool weight) total-weight))
                         (allocated (%clamp-flex spec (max 0 (min share remaining)))))
                    (setf (gethash id results) allocated)
                    (decf remaining allocated))))))))
    ;; If there's remaining space and flex specs exist, give it to the last flex
    (when (and (plusp remaining)
               (some (lambda (s) (member (ptui.layout.constraints:constraint-spec-kind s)
                                         '(:flex :min-max)))
                     specs))
      (let ((last-flex (find-if (lambda (s)
                                  (member (ptui.layout.constraints:constraint-spec-kind s)
                                          '(:flex :min-max)))
                                specs :from-end t)))
        (when last-flex
          (let ((id (ptui.layout.constraints:constraint-spec-id last-flex)))
            (incf (gethash id results 0) remaining)))))
    ;; Build result alist in original input order
    (loop for spec in specs
          for id = (ptui.layout.constraints:constraint-spec-id spec)
          collect (cons id (gethash id results 0)))))

(defun %clamp-flex (spec pixels)
  "Clamp PIXELS to min/max bounds if spec is :min-max."
  (if (eq (ptui.layout.constraints:constraint-spec-kind spec) :min-max)
      (let ((min-val (or (ptui.layout.constraints:constraint-spec-min-value spec) 0))
            (max-val (or (ptui.layout.constraints:constraint-spec-max-value spec)
                         most-positive-fixnum)))
        (max min-val (min max-val pixels)))
      pixels))
