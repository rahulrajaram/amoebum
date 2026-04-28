(in-package :ptui.ui.panel)

;;; ===================================================================
;;; defpanel layout & element-to-panel contract
;;;
;;; Owns the element-to-panel layout contract: lowering :layout forms
;;; into ptui.ui.elements:make-element trees of :constraint-layout
;;; nodes, child element-id assignment, gutter/padding propagation,
;;; conditional region inclusion via :when, slot binding for
;;; composition, and the embed-panel macro for cross-panel embedding.
;;; ===================================================================

;;; -------------------------------------------------------------------
;;; Slot composition helpers
;;; -------------------------------------------------------------------

(defun %slot-default (slot-spec)
  "Extract :default value from a slot spec."
  (let ((plist (cdr slot-spec)))
    (getf plist :default nil)))

(defun %compile-slot-params (panel-name slot-specs)
  "Compile :slots entries into &key argument specs."
  (loop for slot-spec in slot-specs
        for slot-name = (first slot-spec)
        unless (symbolp slot-name)
          do (%signal-syntax-error panel-name :slots
                                   (format nil "Invalid slot spec ~S." slot-spec))
        collect `(,slot-name ,(%slot-default slot-spec))))

(defun %embed-panel-render-name (panel)
  (intern (format nil "RENDER-~A" (symbol-name panel))
          (or (symbol-package panel) *package*)))

(defmacro embed-panel (child-panel &rest kwargs)
  "Embed a child panel invocation in a layout.
Expands to the child render function call with keyword args."
  (let ((child-render (%embed-panel-render-name child-panel)))
    `(,child-render ,@kwargs)))

;;; -------------------------------------------------------------------
;;; Layout-region symbol scanning (used to validate :when bindings)
;;; -------------------------------------------------------------------

(defun %plist-contains-key-p (plist key)
  (loop for tail on plist by #'cddr
        while (consp tail)
        thereis (eq (first tail) key)))

(defun %collect-name-set (names)
  (let ((seen (make-hash-table :test #'eq)))
    (dolist (name names)
      (setf (gethash name seen) t))
    seen))

(defun %known-symbol-p (symbol known-symbols)
  (or (gethash symbol known-symbols)
      (eq symbol t)
      (eq symbol nil)
      (keywordp symbol)
      (boundp symbol)
      (fboundp symbol)
      (special-operator-p symbol)))

(defun %collect-symbol-refs (form)
  (cond
    ((symbolp form) (list form))
    ((atom form) '())
    ((and (consp form) (eq (car form) 'quote)) '())
    (t
     (let ((tail (cdr form)))
       (reduce #'append
               (loop for part in tail collect (%collect-symbol-refs part))
               :initial-value '())))))

(defun %warn-unbound-when-vars (panel-name when-clause known-symbols)
  (dolist (sym (%collect-symbol-refs when-clause))
    (unless (%known-symbol-p sym known-symbols)
      (%signal-syntax-warning panel-name :layout
                              (format nil "Unbound :when symbol ~S."
                                      sym)))))

(defun %validate-layout-when-clauses (panel-name layout-forms known-symbols)
  (when (and layout-forms (consp layout-forms))
    (let ((container-form (first layout-forms)))
      (when (and (consp container-form) (member (car container-form) '(:column :row)))
        (multiple-value-bind (options regions)
            (%parse-container-options (cdr container-form))
          (declare (ignore options))
          (dolist (region regions)
            (let ((when-clause (%region-when-clause region)))
              (when when-clause
                (%warn-unbound-when-vars panel-name when-clause known-symbols)))))))))

(defun %validate-layout-regions (panel-name layout-forms)
  (when layout-forms
    (let ((seen (make-hash-table :test #'eq)))
      (multiple-value-bind (options regions)
          (%parse-container-options (cdr (first layout-forms)))
        (declare (ignore options))
        (dolist (region regions)
          (let ((region-name (first region)))
            (when (gethash region-name seen)
              (%signal-syntax-error panel-name :layout
                                    (format nil "Duplicate region name ~S in :layout."
                                            region-name)))
            (setf (gethash region-name seen) t)))))))

;;; -------------------------------------------------------------------
;;; Layout tree assembly: lower :layout to constraint-layout elements
;;; -------------------------------------------------------------------

(defun %compile-region-body (region-form)
  "Extract the body expression from a :region form.
(name :fixed N body-expr) -> body-expr
(name :fixed N :when pred body-expr) -> body-expr"
  (multiple-value-bind (name sizing-type sizing-value when-clause gutter min-height max-height body)
      (%parse-region-form region-form)
    (declare (ignore name sizing-type sizing-value when-clause gutter min-height max-height))
    (if (= (length body) 1)
        (first body)
        `(progn ,@body))))

(defun %compile-child-form (name body-form &optional region-style-fn)
  "Compile a region child form that wraps the body in element-id assignment.
When REGION-STYLE-FN is supplied and has a style entry for this region, wrap
the child in a box widget."
  `(let ((%child ,body-form))
     (let ((%child-node (if (typep %child 'ptui.ui.elements:ui-element)
                            (ptui.ui.elements:make-element
                             (ptui.ui.elements:ui-element-type %child)
                             :id ',name
                             :key (ptui.ui.elements:ui-element-key %child)
                             :props (ptui.ui.elements:ui-element-props %child)
                             :children (ptui.ui.elements:ui-element-children %child)
                             :focusablep (ptui.ui.elements:ui-element-focusablep %child))
                            (ptui.widgets.core:make-text-widget
                             (princ-to-string %child) :id ',name))))
       ,(if region-style-fn
            `(let ((%style (funcall ,region-style-fn ',name)))
               (if %style
                   (ptui.widgets.core:make-box-widget
                    %child-node
                    :border (getf %style :border)
                    :fg (getf %style :fg)
                    :bg (getf %style :bg)
                    :attrs (ptui.core.types:make-attrs
                            :boldp (getf %style :bold)
                            :italicp (getf %style :italic)
                            :underlinep (getf %style :underline)
                            :invertp (getf %style :inverse)
                            :dimp (getf %style :dim)))
                   %child-node))
            '%child-node))))

(defun %collect-gutters (regions)
  "Collect an alist of (region-name . gutter-width) for regions with :gutter."
  (loop for region in regions
        for name = (first region)
        for gutter = (%region-gutter region)
        when gutter
          collect (cons name gutter)))

(defun %compile-layout-tree (layout-forms &optional region-style-fn)
  "Compile :layout section into constraint-layout element construction.
Returns form that builds a ui-element of type :constraint-layout.
Supports :when on regions — conditional regions are omitted when predicate is falsy.
Supports :padding on containers and :gutter on regions."
  (when (null layout-forms)
    (return-from %compile-layout-tree
      `(ptui.ui.elements:make-element :node)))
  (let ((container-form (first layout-forms)))
    (unless (and (consp container-form)
                 (member (car container-form) '(:column :row)))
      (error "defpanel :layout must start with (:column ...) or (:row ...). Got: ~S"
             container-form))
    (let ((direction (car container-form)))
      (multiple-value-bind (container-options region-list)
          (%parse-container-options (cdr container-form))
        (let* ((regions region-list)
               (padding-spec (getf container-options :padding))
               (gutters (%collect-gutters regions))
               (has-conditional (some #'%region-when-clause regions))
               (extra-props
                 (append
                  (when padding-spec
                    `(:padding ',(%normalize-padding padding-spec)))
                  (when gutters
                    `(:gutters ',gutters)))))
          (if (not has-conditional)
              ;; Simple case: no conditional regions — static constraints and children
              (let ((constraints (mapcar #'%compile-region-constraint regions))
                    (children (loop for region in regions
                                    for name = (first region)
                                    collect (%compile-child-form name
                                                               (%compile-region-body region)
                                                               region-style-fn))))
                `(ptui.ui.elements:make-element
                  :constraint-layout
                  :props (list :direction ,(if (eq direction :column) :column :row)
                               :constraints (list ,@constraints)
                               ,@extra-props)
                  :children (list ,@children)))
              ;; Conditional case: build constraints and children lists dynamically
              (let ((constraint-entries
                      (loop for region in regions
                            for name = (first region)
                            for when-clause = (%region-when-clause region)
                            for constraint-form = (%compile-region-constraint region)
                            for child-form = (%compile-child-form name
                                                                 (%compile-region-body region)
                                                                 region-style-fn)
                            if when-clause
                              collect `(when ,when-clause
                                        (push ,constraint-form %constraints)
                                        (push ,child-form %children))
                            else
                              collect `(progn
                                        (push ,constraint-form %constraints)
                                        (push ,child-form %children)))))
                `(let ((%constraints '())
                       (%children '()))
                   ,@constraint-entries
                   (ptui.ui.elements:make-element
                    :constraint-layout
                    :props (list :direction ,(if (eq direction :column) :column :row)
                                 :constraints (nreverse %constraints)
                                 ,@extra-props)
                    :children (nreverse %children))))))))))
