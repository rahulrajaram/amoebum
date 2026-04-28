(in-package :ptui.ui.panel)

;;; ===================================================================
;;; defpanel public entry point (residual orchestration)
;;;
;;; Public surface for the panel DSL. Holds %parse-panel-sections
;;; (the section dispatcher) and the defpanel macro itself, which
;;; composes the four modules under panel/:
;;;   - panel/sizing.lisp : defpackage, conditions, signal helpers,
;;;                         padding, container options, region sizing
;;;                         constraints, :style theme evaluation.
;;;   - panel/layout.lisp : element-to-panel layout contract,
;;;                         constraint-layout tree, slots, embed,
;;;                         :when symbol-binding validation.
;;;   - panel/events.lisp : :keys -> handler lambda (flat + modal),
;;;                         focus-root wrap so events route by id.
;;;   - panel/render.lisp : :state / :data / :effects / :context
;;;                         bindings, name validation, m-v-bind nest.
;;; ===================================================================

(defun %parse-panel-sections (body &optional panel-name)
  "Parse defpanel body into (values state data layout keys effects context slots style).
Each section is the cdr of the (:keyword ...) form, or NIL if absent."
  (let (state data layout keys effects context slots style)
    (dolist (form body)
      (when (and (consp form) (keywordp (car form)))
        (case (car form)
          (:state (setf state (cdr form)))
          (:data (setf data (cdr form)))
          (:layout (setf layout (cdr form)))
          (:keys (setf keys (cdr form)))
          (:effects (setf effects (cdr form)))
          (:context (setf context (cdr form)))
          (:slots (setf slots (cdr form)))
          (:style (setf style (%parse-panel-styles (cdr form) panel-name)))
          (t (%signal-syntax-error panel-name (car form)
                                   (format nil "Unknown section keyword ~S."
                                           (car form)))))))
    (values state data layout keys effects context slots style)))

(defmacro defpanel (name (&rest params) &body sections)
  "Define a panel widget with declarative state, data, layout, key bindings, and effects.

Expands to a defwidget whose body:
1. Binds all state vars via use-state
2. Binds all data vars via use-memo
3. Runs :effects (use-effect calls)
4. Evaluates :layout form in scope of all bindings
5. Attaches :keys handler

Example:
  (defpanel my-panel (&key items selected)
    (:state
      (scroll-offset 0 :type fixnum))
    (:data
      (item-count (length items) :deps (items)))
    (:effects
      (log-count (format t \"Items: ~D~%\" item-count)
        :deps (item-count)))
    (:layout
      (:column
        (content :flex 1 (text (format nil \"~D items\" item-count)))
        (footer :fixed 1 (text \"Press q to quit\"))))
    (:keys
      (:up (decf scroll-offset))
      (:down (incf scroll-offset))))"
  (multiple-value-bind
      (state-specs data-specs layout-forms key-specs effects-specs context-specs slot-specs style-specs)
      (%parse-panel-sections sections name)
    (%validate-panel-names name state-specs data-specs slot-specs)
    (multiple-value-bind (state-bindings state-vars setter-names)
        (%compile-panel-state state-specs)
      (declare (ignore setter-names))
      (let ((widget-params
              (loop for p in params
                    unless (member p '(&key &optional &rest &body &allow-other-keys))
                    collect p)))
        (%validate-layout-regions name layout-forms)
        (%validate-style-regions name (%collect-layout-regions name layout-forms) style-specs)
        (multiple-value-bind (data-bindings data-vars)
            (%compile-panel-data name data-specs widget-params state-vars)
          (%validate-layout-when-clauses
           name
           layout-forms
           (%collect-name-set (append widget-params state-vars data-vars)))
          (multiple-value-bind (context-provides context-consumes)
            (%compile-panel-context context-specs)
           (let* ((region-style-fn (%compile-panel-styles style-specs))
                   (layout-form (%compile-layout-tree layout-forms (when region-style-fn '%region-style)))
                   (keys-form (%compile-panel-keys key-specs))
                   (slot-params (%compile-slot-params name slot-specs))
                   (defwidget-params (if slot-params
                                         (append widget-params (list '&key) slot-params)
                                         widget-params))
                   (effects-forms (%compile-panel-effects effects-specs))
                   (layout-style-form (if region-style-fn
                                         `(let ((%region-style ,region-style-fn))
                                            ,layout-form)
                                         layout-form))
                   ;; The inner form: layout wrapped with keys
                   (inner-form (%wrap-with-keys keys-form layout-style-form name))
                   ;; Wrap with effects (before layout, after data)
                   (effects-and-inner
                     (if effects-forms
                         `(progn ,@effects-forms ,inner-form)
                         inner-form))
                   ;; Wrap with data bindings
                   (data-and-rest
                     (if data-bindings
                         `(let* (,@data-bindings)
                            (declare (ignorable ,@(mapcar #'first data-bindings)))
                            ,effects-and-inner)
                         effects-and-inner))
                   ;; Wrap with context consumes (let* bindings from use-context)
                   (context-and-rest
                     (if context-consumes
                         `(let* (,@context-consumes)
                            (declare (ignorable ,@(mapcar #'first context-consumes)))
                            ,data-and-rest)
                         data-and-rest))
                   ;; Wrap with context provides (before everything else)
                   (provides-and-rest
                     (if context-provides
                         `(progn ,@context-provides ,context-and-rest)
                         context-and-rest)))
              ;; Generate defwidget expansion
              `(ptui.widgets.defwidget:defwidget ,name ,defwidget-params
                 (:memoize nil)
                 ,(%nest-state-bindings state-bindings provides-and-rest)))))))))
