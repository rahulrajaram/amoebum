(in-package :ptui.ui.panel)

;;; ===================================================================
;;; defpanel render-time bindings
;;;
;;; State / data / effects / context compilation that produces the
;;; bindings the rendered panel body evaluates at draw time:
;;;   - :state -> use-state binding pairs
;;;   - :data  -> use-memo bindings dependent on state and params
;;;   - :effects -> use-effect calls
;;;   - :context -> provide-context / use-context bindings
;;; Plus name-collision validation and the multiple-value-bind nesting
;;; that wraps the layout form with all bindings in scope.
;;; ===================================================================

;;; -------------------------------------------------------------------
;;; :state compilation (use-state)
;;; -------------------------------------------------------------------

(defun %compile-state-binding (spec)
  "Compile a single (:state (name init &key type)) into use-state binding forms.
Returns (values var-sym setter-sym use-state-form)."
  (destructuring-bind (name initial-value &key type) spec
    (declare (ignore type))
    (let ((setter-name (intern (format nil "SET-~A" (symbol-name name))
                               (symbol-package name))))
      (values name setter-name
              `(multiple-value-bind (,name ,setter-name)
                   (ptui.ui.hooks:use-state ,name :initial-value ,initial-value)
                 (declare (ignorable ,setter-name)))))))

(defun %compile-panel-state (state-specs)
  "Compile all :state specs into a list of let* binding clauses.
Returns (values bindings all-var-names all-setter-names)."
  (let (bindings var-names setter-names)
    (dolist (spec state-specs)
      (multiple-value-bind (var setter form)
          (%compile-state-binding spec)
        (push var var-names)
        (push setter setter-names)
        (push form bindings)))
    (values (nreverse bindings) (nreverse var-names) (nreverse setter-names))))

;;; -------------------------------------------------------------------
;;; :data compilation (use-memo)
;;; -------------------------------------------------------------------

(defun %compile-data-binding (panel-name spec all-params all-state-vars)
  "Compile a single (:data (name expr &key deps)) into use-memo binding form.
Returns (values var-sym use-memo-form)."
  (destructuring-bind (name expr &rest options) spec
    (let ((deps-provided-p (%plist-contains-key-p options :deps))
          (deps (getf options :deps)))
      (unless deps-provided-p
        (%signal-syntax-warning panel-name :data
                                (format nil "Data node ~S has no :deps."
                                        name)))
      (let ((effective-deps (or deps (append all-params all-state-vars))))
        (values name
                `(,name (ptui.ui.hooks:use-memo ,name (:deps ,effective-deps)
                          ,expr)))))))

(defun %compile-panel-data (panel-name data-specs all-params all-state-vars)
  "Compile all :data specs into let* binding clauses.
Returns (values bindings data-var-names)."
  (let (bindings var-names)
    (dolist (spec data-specs)
      (multiple-value-bind (var form)
          (%compile-data-binding panel-name spec all-params all-state-vars)
        (push var var-names)
        (push form bindings)))
    (values (nreverse bindings) (nreverse var-names))))

;;; -------------------------------------------------------------------
;;; :effects compilation (use-effect)
;;; -------------------------------------------------------------------

(defun %compile-effect-binding (spec)
  "Compile a single :effects entry into a use-effect form.
Each spec is (name body &key deps cleanup).
Returns a progn-able form."
  (destructuring-bind (name body &key deps cleanup) spec
    (let ((effect-body
            (if cleanup
                `(progn ,body (lambda () ,cleanup))
                body)))
      `(ptui.ui.hooks:use-effect ,name (:deps ,deps)
         ,effect-body))))

(defun %compile-panel-effects (effects-specs)
  "Compile all :effects specs into a list of use-effect forms.
Returns a list of forms to splice into the panel body."
  (mapcar #'%compile-effect-binding effects-specs))

;;; -------------------------------------------------------------------
;;; :context compilation (provide-context / use-context)
;;; -------------------------------------------------------------------

(defun %context-spec-provide (spec)
  "Extract :provide value from a context spec, or NIL."
  (let ((pos (position :provide (cdr spec))))
    (when pos (nth (1+ pos) (cdr spec)))))

(defun %context-spec-from (spec)
  "Extract :from value from a context spec, or NIL."
  (let ((pos (position :from (cdr spec))))
    (when pos (nth (1+ pos) (cdr spec)))))

(defun %compile-context-provide (spec)
  "Compile a (:context (name :provide expr)) into a provide-context form."
  (let ((name (first spec))
        (provide-expr (%context-spec-provide spec)))
    (when provide-expr
      `(ptui.ui.hooks:provide-context ',name ,provide-expr))))

(defun %compile-context-consume (spec)
  "Compile a (:context (name :from ctx-name)) into a use-context binding."
  (let ((name (first spec))
        (from-name (%context-spec-from spec)))
    (when from-name
      `(,name (ptui.ui.hooks:use-context ',from-name)))))

(defun %compile-panel-context (context-specs)
  "Compile :context specs into (values provide-forms consume-bindings).
Provide-forms are progn-able statements.
Consume-bindings are let* binding clauses."
  (let (provides consumes)
    (dolist (spec context-specs)
      (let ((provide-form (%compile-context-provide spec)))
        (when provide-form (push provide-form provides)))
      (let ((consume-form (%compile-context-consume spec)))
        (when consume-form (push consume-form consumes))))
    (values (nreverse provides) (nreverse consumes))))

;;; -------------------------------------------------------------------
;;; Cross-section validation and binding nesting
;;; -------------------------------------------------------------------

(defun %validate-panel-names (panel-name state-specs data-specs slot-specs)
  "Validate no duplicate names across state, data, and slot sections."
  (let ((all-names (append (mapcar #'first state-specs)
                           (mapcar #'first data-specs)
                           (mapcar #'first slot-specs)))
        (seen (make-hash-table :test #'eq)))
    (dolist (name all-names)
      (when (gethash name seen)
        (%signal-syntax-error panel-name :state
                              (format nil "Duplicate name ~S across :state/:data sections."
                                      name)))
      (setf (gethash name seen) t))))

(defun %nest-state-bindings (bindings inner-form)
  "Nest multiple-value-bind forms from state compilation around INNER-FORM."
  (if (null bindings)
      inner-form
      ;; Each binding is a (multiple-value-bind (var setter) (use-state ...) (declare ...))
      ;; We need to wrap the inner form inside the last binding
      (let ((first-binding (first bindings)))
        ;; first-binding looks like:
        ;; (multiple-value-bind (var setter) (use-state ...) (declare (ignorable setter)))
        ;; We append the rest of the nesting as the body
        (append first-binding
                (list (%nest-state-bindings (rest bindings) inner-form))))))
