(defpackage :ptui.test.constraints
  (:use :cl :fiveam)
  (:export #:run-all
           #:constraint-spec-suite
           #:constraint-solver-suite
           #:constraint-layout-suite))

(in-package :ptui.test.constraints)

;;; ===================================================================
;;; I281: constraint-spec-suite
;;; ===================================================================

(def-suite constraint-spec-suite
  :description "PTUI constraint spec types and builder functions (I281).")

(in-suite constraint-spec-suite)

(test fixed-creates-correct-spec
  (let ((spec (ptui.layout.constraints:fixed 'header 3)))
    (is (eq (ptui.layout.constraints:constraint-spec-id spec) 'header))
    (is (eq (ptui.layout.constraints:constraint-spec-kind spec) :fixed))
    (is (= (ptui.layout.constraints:constraint-spec-value spec) 3))
    (is (= (ptui.layout.constraints:constraint-spec-priority spec) 10))))

(test percentage-creates-correct-spec
  (let ((spec (ptui.layout.constraints:percentage 'sidebar 25)))
    (is (eq (ptui.layout.constraints:constraint-spec-id spec) 'sidebar))
    (is (eq (ptui.layout.constraints:constraint-spec-kind spec) :percentage))
    (is (= (ptui.layout.constraints:constraint-spec-value spec) 25))
    (is (= (ptui.layout.constraints:constraint-spec-priority spec) 20))))

(test flex-creates-correct-spec
  (let ((spec (ptui.layout.constraints:flex 'content)))
    (is (eq (ptui.layout.constraints:constraint-spec-id spec) 'content))
    (is (eq (ptui.layout.constraints:constraint-spec-kind spec) :flex))
    (is (= (ptui.layout.constraints:constraint-spec-priority spec) 30))
    (is (= (ptui.layout.constraints:constraint-spec-weight spec) 1))))

(test flex-with-min-max-creates-min-max-kind
  (let ((spec (ptui.layout.constraints:flex 'content :min 5 :max 20)))
    (is (eq (ptui.layout.constraints:constraint-spec-kind spec) :min-max))
    (is (= (ptui.layout.constraints:constraint-spec-min-value spec) 5))
    (is (= (ptui.layout.constraints:constraint-spec-max-value spec) 20))))

(test flex-with-custom-weight
  (let ((spec (ptui.layout.constraints:flex 'content :weight 2)))
    (is (= (ptui.layout.constraints:constraint-spec-weight spec) 2))))

(test dock-creates-fixed-with-metadata
  (let ((spec (ptui.layout.constraints:dock 'top-bar :top 2)))
    (is (eq (ptui.layout.constraints:constraint-spec-kind spec) :fixed))
    (is (= (ptui.layout.constraints:constraint-spec-value spec) 2))
    (is (equal (ptui.layout.constraints:constraint-spec-metadata spec)
               '(:dock :top)))))

(test fixed-rejects-negative-pixels
  (signals error
    (ptui.layout.constraints:fixed 'bad -1)))

(test percentage-rejects-over-100
  (signals error
    (ptui.layout.constraints:percentage 'bad 101)))

(test flex-rejects-zero-weight
  (signals error
    (ptui.layout.constraints:flex 'bad :weight 0)))

;;; ===================================================================
;;; I282: constraint-solver-suite
;;; ===================================================================

(def-suite constraint-solver-suite
  :description "PTUI constraint solver (I282).")

(in-suite constraint-solver-suite)

(test solve-fixed-only
  (let* ((specs (list (ptui.layout.constraints:fixed 'header 3)
                      (ptui.layout.constraints:fixed 'footer 2)))
         (result (ptui.layout.solver:solve-constraints specs 100)))
    (is (= (cdr (assoc 'header result)) 3))
    (is (= (cdr (assoc 'footer result)) 2))))

(test solve-percentage
  (let* ((specs (list (ptui.layout.constraints:percentage 'sidebar 25)
                      (ptui.layout.constraints:percentage 'main 75)))
         (result (ptui.layout.solver:solve-constraints specs 100)))
    (is (= (cdr (assoc 'sidebar result)) 25))
    (is (= (cdr (assoc 'main result)) 75))))

(test solve-flex-distribution
  (let* ((specs (list (ptui.layout.constraints:flex 'a :weight 1)
                      (ptui.layout.constraints:flex 'b :weight 2)))
         (result (ptui.layout.solver:solve-constraints specs 90)))
    ;; Weight 1:2 of 90 = 30:60, but rounding + remainder absorption
    (is (= (+ (cdr (assoc 'a result))
              (cdr (assoc 'b result)))
            90))))

(test solve-mixed-fixed-flex
  (let* ((specs (list (ptui.layout.constraints:fixed 'header 3)
                      (ptui.layout.constraints:flex 'content)
                      (ptui.layout.constraints:fixed 'footer 1)))
         (result (ptui.layout.solver:solve-constraints specs 24)))
    (is (= (cdr (assoc 'header result)) 3))
    (is (= (cdr (assoc 'footer result)) 1))
    ;; Content gets remaining: 24 - 3 - 1 = 20
    (is (= (cdr (assoc 'content result)) 20))))

(test solve-over-allocation-clamps
  (let* ((specs (list (ptui.layout.constraints:fixed 'a 60)
                      (ptui.layout.constraints:fixed 'b 60)))
         (result (ptui.layout.solver:solve-constraints specs 100)))
    ;; First gets 60, second gets clamped to remaining 40
    (is (= (cdr (assoc 'a result)) 60))
    (is (= (cdr (assoc 'b result)) 40))))

(test solve-min-max-clamping
  (let* ((specs (list (ptui.layout.constraints:flex 'a :min 10 :max 30)
                      (ptui.layout.constraints:flex 'b)))
         (result (ptui.layout.solver:solve-constraints specs 100)))
    ;; 'a should be clamped between 10 and 30
    (is (<= 10 (cdr (assoc 'a result)) 30))))

(test solve-preserves-input-order
  (let* ((specs (list (ptui.layout.constraints:flex 'z)
                      (ptui.layout.constraints:fixed 'a 5)
                      (ptui.layout.constraints:flex 'm)))
         (result (ptui.layout.solver:solve-constraints specs 100)))
    (is (eq (caar result) 'z))
    (is (eq (caadr result) 'a))
    (is (eq (caaddr result) 'm))))

(test solve-zero-available
  (let* ((specs (list (ptui.layout.constraints:fixed 'a 5)))
         (result (ptui.layout.solver:solve-constraints specs 0)))
    (is (= (cdr (assoc 'a result)) 0))))

;;; ===================================================================
;;; I283: constraint-layout-suite
;;; ===================================================================

(def-suite constraint-layout-suite
  :description "PTUI constraint layout nodes (I283).")

(in-suite constraint-layout-suite)

(test constraint-layout-node-creation
  (let ((node (ptui.layout.constraint-layout:make-constraint-layout-node
               :id 'root
               :direction :column
               :constraints (list (ptui.layout.constraints:fixed 'header 3)
                                  (ptui.layout.constraints:flex 'content))
               :children (list (ptui.layout:make-layout-node :id 'header :height 3)
                               (ptui.layout:make-layout-node :id 'content)))))
    (is (ptui.layout.constraint-layout:constraint-layout-node-p node))
    (is (eq (ptui.layout.constraint-layout:constraint-layout-node-id node) 'root))
    (is (eq (ptui.layout.constraint-layout:constraint-layout-node-direction node) :column))))

(test two-region-dock-fill-layout
  (let* ((constraints (list (ptui.layout.constraints:fixed 'header 3)
                            (ptui.layout.constraints:flex 'content)))
         (root (ptui.layout.constraint-layout:make-constraint-layout-node
                :id 'root
                :direction :column
                :constraints constraints
                :children (list (ptui.layout:make-layout-node :id 'header :height 3)
                                (ptui.layout:make-layout-node :id 'content))))
         (layout (ptui.layout.constraint-layout:compute-constraint-layout
                  root :width 80 :height 24)))
    ;; Root should be full size
    (let ((root-bounds (ptui.layout:layout-bound layout 'root)))
      (is (not (null root-bounds)))
      (is (= (ptui.layout:layout-bounds-width root-bounds) 80))
      (is (= (ptui.layout:layout-bounds-height root-bounds) 24)))
    ;; Header: 3 rows at top
    (let ((header-bounds (ptui.layout:layout-bound layout 'header)))
      (is (not (null header-bounds)))
      (is (= (ptui.layout:layout-bounds-y header-bounds) 0))
      (is (= (ptui.layout:layout-bounds-height header-bounds) 3)))
    ;; Content: remaining 21 rows
    (let ((content-bounds (ptui.layout:layout-bound layout 'content)))
      (is (not (null content-bounds)))
      (is (= (ptui.layout:layout-bounds-y content-bounds) 3))
      (is (= (ptui.layout:layout-bounds-height content-bounds) 21)))))

(test three-region-with-percentages
  (let* ((constraints (list (ptui.layout.constraints:fixed 'header 2)
                            (ptui.layout.constraints:percentage 'sidebar 25)
                            (ptui.layout.constraints:flex 'main)))
         (root (ptui.layout.constraint-layout:make-constraint-layout-node
                :id 'root
                :direction :column
                :constraints constraints
                :children (list (ptui.layout:make-layout-node :id 'header :height 2)
                                (ptui.layout:make-layout-node :id 'sidebar)
                                (ptui.layout:make-layout-node :id 'main))))
         (layout (ptui.layout.constraint-layout:compute-constraint-layout
                  root :width 80 :height 100)))
    ;; Header: 2 rows
    (let ((h (ptui.layout:layout-bound layout 'header)))
      (is (= (ptui.layout:layout-bounds-height h) 2)))
    ;; Sidebar: 25% of 100 = 25
    (let ((s (ptui.layout:layout-bound layout 'sidebar)))
      (is (= (ptui.layout:layout-bounds-height s) 25)))
    ;; Main: remaining = 100 - 2 - 25 = 73
    (let ((m (ptui.layout:layout-bound layout 'main)))
      (is (= (ptui.layout:layout-bounds-height m) 73)))))

(test nested-constraint-nodes
  (let* ((inner (ptui.layout.constraint-layout:make-constraint-layout-node
                 :id 'body  ;; Must match constraint id
                 :direction :row
                 :constraints (list (ptui.layout.constraints:fixed 'left 20)
                                    (ptui.layout.constraints:flex 'right))
                 :children (list (ptui.layout:make-layout-node :id 'left :width 20)
                                 (ptui.layout:make-layout-node :id 'right))))
         (root (ptui.layout.constraint-layout:make-constraint-layout-node
                :id 'root
                :direction :column
                :constraints (list (ptui.layout.constraints:fixed 'top 5)
                                   (ptui.layout.constraints:flex 'body))
                :children (list (ptui.layout:make-layout-node :id 'top :height 5)
                                inner)))
         (layout (ptui.layout.constraint-layout:compute-constraint-layout
                  root :width 80 :height 24)))
    (let ((top-b (ptui.layout:layout-bound layout 'top)))
      (is (= (ptui.layout:layout-bounds-height top-b) 5)))
    (let ((body-b (ptui.layout:layout-bound layout 'body)))
      (is (= (ptui.layout:layout-bounds-y body-b) 5))
      (is (= (ptui.layout:layout-bounds-height body-b) 19)))))

;;; ===================================================================
;;; I295: Flex Remainder (fill-remaining) Test
;;; ===================================================================

(in-suite constraint-solver-suite)

(test solve-many-fixed-plus-one-flex-gets-remainder
  "6 fixed regions + 1 flex = flex gets exactly the remainder."
  (let* ((specs (list (ptui.layout.constraints:fixed 'header 3)
                      (ptui.layout.constraints:fixed 'provider 5)
                      (ptui.layout.constraints:fixed 'plan 6)
                      (ptui.layout.constraints:fixed 'approval 4)
                      (ptui.layout.constraints:fixed 'input 4)
                      (ptui.layout.constraints:fixed 'status 1)
                      (ptui.layout.constraints:flex 'history)))
         (total 48)
         (result (ptui.layout.solver:solve-constraints specs total))
         (fixed-sum (+ 3 5 6 4 4 1))  ; = 23
         (expected-flex (- total fixed-sum)))  ; = 25
    ;; Each fixed region gets its exact value
    (is (= (cdr (assoc 'header result)) 3))
    (is (= (cdr (assoc 'provider result)) 5))
    (is (= (cdr (assoc 'plan result)) 6))
    (is (= (cdr (assoc 'approval result)) 4))
    (is (= (cdr (assoc 'input result)) 4))
    (is (= (cdr (assoc 'status result)) 1))
    ;; Flex gets the remainder
    (is (= (cdr (assoc 'history result)) expected-flex))
    ;; Total adds up
    (is (= (loop for (nil . v) in result sum v) total))))

(defun run-all ()
  (run! 'constraint-spec-suite)
  (run! 'constraint-solver-suite)
  (run! 'constraint-layout-suite))
