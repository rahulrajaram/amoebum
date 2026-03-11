(defpackage :ptui.ui.panel
  (:use :cl)
  (:export
   #:defpanel
   #:embed-panel
   #:defpanel-syntax-error
   #:defpanel-syntax-warning
   #:defpanel-syntax-error-panel-name
   #:defpanel-syntax-error-section
   #:defpanel-syntax-error-detail
   #:defpanel-syntax-warning-panel-name
   #:defpanel-syntax-warning-section
   #:defpanel-syntax-warning-detail
   ;; Compilation helpers (exposed for testing)
   #:%compile-panel-state
   #:%compile-panel-data
   #:%compile-panel-layout
   #:%compile-panel-keys
   #:%compile-panel-effects
   #:%compile-panel-styles
   #:%parse-panel-sections))

(in-package :ptui.ui.panel)

(defparameter +panel-section-keywords+
  '(:state :data :layout :keys :effects :context :slots :style))

(define-condition defpanel-syntax-error (error)
  ((panel-name :initarg :panel-name
               :reader defpanel-syntax-error-panel-name)
   (section :initarg :section
            :reader defpanel-syntax-error-section)
   (detail :initarg :detail
           :reader defpanel-syntax-error-detail))
  (:report (lambda (condition stream)
             (format stream "DEFPANEL ~S section ~S: ~A"
                     (defpanel-syntax-error-panel-name condition)
                     (defpanel-syntax-error-section condition)
                     (defpanel-syntax-error-detail condition)))))

(define-condition defpanel-syntax-warning (style-warning)
  ((panel-name :initarg :panel-name
               :reader defpanel-syntax-warning-panel-name)
   (section :initarg :section
            :reader defpanel-syntax-warning-section)
   (detail :initarg :detail
           :reader defpanel-syntax-warning-detail))
  (:report (lambda (condition stream)
             (format stream "DEFPANEL ~S section ~S warning: ~A"
                     (defpanel-syntax-warning-panel-name condition)
                     (defpanel-syntax-warning-section condition)
                     (defpanel-syntax-warning-detail condition)))))

(defun %signal-syntax-error (panel-name section detail)
  (error 'defpanel-syntax-error
         :panel-name panel-name
         :section section
         :detail detail))

(defun %signal-syntax-warning (panel-name section detail)
  (warn 'defpanel-syntax-warning
        :panel-name panel-name
        :section section
        :detail detail))

;;; ===================================================================
;;; I316: defpanel :style parsing
;;; ===================================================================

(defun %style-key-valid-p (key)
  (member key '(:border :fg :bg :bold :italic :underline :inverse :dim) :test #'eq))

(defun %style-border-valid-p (border)
  (member border '(:rounded :single :double :none) :test #'eq))

(defun %parse-style-spec (spec panel-name)
  (unless (consp spec)
    (%signal-syntax-error panel-name :style
                          (format nil "Invalid style spec ~S." spec)))
  (let ((region-name (first spec))
        (options (cdr spec)))
    (unless (symbolp region-name)
      (%signal-syntax-error panel-name :style
                            (format nil "Invalid style spec ~S." spec)))
    (when (oddp (length options))
      (%signal-syntax-error panel-name :style
                            (format nil "Invalid style options in spec ~S." spec)))
    (loop for (key value) on options by #'cddr
          unless (%style-key-valid-p key)
            do (%signal-syntax-error panel-name :style
                                     (format nil "Invalid style option ~S." key)))
    (when (and (getf options :border)
               (not (%style-border-valid-p (getf options :border))))
      (%signal-syntax-error panel-name :style
                            (format nil "Invalid border style ~S." (getf options :border))))
    (destructuring-bind (region-name &key border fg bg bold italic underline inverse dim &allow-other-keys) spec
      (declare (ignore region-name))
      (let ((style '()))
        (when border
          (setf style (append style (list :border border))))
        (when fg
          (setf style (append style (list :fg fg))))
        (when bg
          (setf style (append style (list :bg bg))))
        (when bold
          (setf style (append style (list :bold bold))))
        (when italic
          (setf style (append style (list :italic italic))))
        (when underline
          (setf style (append style (list :underline underline))))
        (when inverse
          (setf style (append style (list :inverse inverse))))
        (when dim
          (setf style (append style (list :dim dim))))
        (cons (first spec) style)))))

(defun %parse-panel-styles (style-section panel-name)
  (loop for style-spec in style-section
        collect (%parse-style-spec style-spec panel-name)))

;;; ===================================================================
;;; I288: defpanel State and Data Compilation
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
(defun %compile-panel-styles (style-specs)
  "Compile style specs into a region-style lookup function.
Returns NIL when no styles are defined."
  (when style-specs
    `(let ((%styles ',style-specs))
       (lambda (region-name)
         (cdr (assoc region-name %styles :test #'eq))))))

(defun %collect-layout-regions (panel-name layout-forms)
  (when (and layout-forms (consp layout-forms))
    (let ((container-form (first layout-forms))
          (seen (make-hash-table :test #'eq)))
      (unless (and (consp container-form)
                   (member (car container-form) '(:column :row)))
        (%signal-syntax-error panel-name :layout
                              (format nil "Invalid :layout form ~S." container-form)))
      (loop for region in (cdr container-form)
            for region-name = (first region)
            do (when (gethash region-name seen)
                 (%signal-syntax-error panel-name :layout
                                       (format nil "Duplicate region name ~S in :layout."
                                               region-name)))
            do (setf (gethash region-name seen) t)
            collect region-name))))

(defun %validate-style-regions (panel-name layout-regions style-specs)
  (dolist (style-spec style-specs)
    (let ((region-name (car style-spec)))
      (unless (member region-name layout-regions :test #'eq)
        (%signal-syntax-error panel-name :style
                              (format nil "Unknown style region ~S." region-name))))))

;;; ===================================================================
;;; I317: defpanel Slots and Composition Helpers
;;; ===================================================================

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
        (dolist (region (cdr container-form))
          (let ((when-clause (%region-when-clause region)))
            (when when-clause
              (%warn-unbound-when-vars panel-name when-clause known-symbols))))))))

(defun %validate-layout-regions (panel-name layout-forms)
  (when layout-forms
    (let ((seen (make-hash-table :test #'eq)))
      (dolist (region (cdr (first layout-forms)))
        (let ((region-name (first region)))
          (when (gethash region-name seen)
            (%signal-syntax-error panel-name :layout
                                  (format nil "Duplicate region name ~S in :layout."
                                          region-name)))
          (setf (gethash region-name seen) t))))))

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

;;; ===================================================================
;;; I292: defpanel Effects Compilation
;;; ===================================================================

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

;;; ===================================================================
;;; I296: defpanel Context Compilation
;;; ===================================================================

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

;;; ===================================================================
;;; I289: defpanel Layout and Keys Compilation
;;; ===================================================================

(defun %parse-region-form (region-form)
  "Parse a region form into (values name sizing-type sizing-value when-clause body).
Handles optional :when clause: (name :fixed N :when pred body...)."
  (let ((name (first region-form))
        (sizing-type (second region-form))
        (sizing-value (third region-form))
        (rest (cdddr region-form))
        (when-clause nil))
    ;; Check for :when keyword after sizing-value
    (when (and rest (eq (first rest) :when))
      (setf when-clause (second rest))
      (setf rest (cddr rest)))
    (values name sizing-type sizing-value when-clause rest)))

(defun %compile-region-constraint (region-form)
  "Extract constraint spec from a :region form.
(name :fixed N body...) -> (ptui.layout.constraints:fixed 'name N)
(name :flex N body...) -> (ptui.layout.constraints:flex 'name :weight N)
(name :percentage N body...) -> (ptui.layout.constraints:percentage 'name N)"
  (multiple-value-bind (name sizing-type sizing-value when-clause body)
      (%parse-region-form region-form)
    (declare (ignore when-clause body))
    (case sizing-type
      (:fixed `(ptui.layout.constraints:fixed ',name ,sizing-value))
      (:flex `(ptui.layout.constraints:flex ',name :weight ,(or sizing-value 1)))
      (:percentage `(ptui.layout.constraints:percentage ',name ,sizing-value))
      (t (error "defpanel :region sizing must be :fixed, :flex, or :percentage. Got: ~S"
                sizing-type)))))

(defun %region-when-clause (region-form)
  "Extract the :when clause from a region form, or NIL if absent."
  (multiple-value-bind (name sizing-type sizing-value when-clause body)
      (%parse-region-form region-form)
    (declare (ignore name sizing-type sizing-value body))
    when-clause))

(defun %compile-region-body (region-form)
  "Extract the body expression from a :region form.
(name :fixed N body-expr) -> body-expr
(name :fixed N :when pred body-expr) -> body-expr"
  (multiple-value-bind (name sizing-type sizing-value when-clause body)
      (%parse-region-form region-form)
    (declare (ignore name sizing-type sizing-value when-clause))
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

(defun %compile-layout-tree (layout-forms &optional region-style-fn)
  "Compile :layout section into constraint-layout element construction.
Returns form that builds a ui-element of type :constraint-layout.
Supports :when on regions — conditional regions are omitted when predicate is falsy."
  (when (null layout-forms)
    (return-from %compile-layout-tree
      `(ptui.ui.elements:make-element :node)))
  (let ((container-form (first layout-forms)))
    (unless (and (consp container-form)
                 (member (car container-form) '(:column :row)))
      (error "defpanel :layout must start with (:column ...) or (:row ...). Got: ~S"
             container-form))
    (let* ((direction (car container-form))
           (regions (cdr container-form))
           (has-conditional (some #'%region-when-clause regions)))
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
                           :constraints (list ,@constraints))
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
                             :constraints (nreverse %constraints))
                :children (nreverse %children))))))))

(defun %compile-key-pattern (pattern)
  "Compile a defpanel :keys pattern into a condition form.
:ctrl-a -> (eql key :ctrl-a)   ; matches input parser's full key name
:tab -> (eql key :tab)
:enter -> (eql key :enter)
:up/:down -> (eql key :up) etc."
  (cond
    ;; All keyword patterns match the key name directly.
    ;; The input parser emits full key names like :ctrl-a, :ctrl-left, etc.
    ((keywordp pattern)
     `(eql key ,pattern))
    (t
     (error "defpanel :keys invalid pattern: ~S" pattern))))

(defun %key-specs-have-modes-p (key-specs)
  "Return T if any key-spec is a :mode declaration."
  (some (lambda (spec) (eq (car spec) :mode)) key-specs))

(defun %compile-mode-keys (mode-spec)
  "Compile a single (:mode name :when pred key-bindings...) into a cond clause.
Returns (when-form cond-branches) where when-form tests the mode predicate
and cond-branches are the key bindings within that mode."
  (destructuring-bind (mode-name &rest rest) (cdr mode-spec)
    (let ((when-clause nil)
          (key-bindings rest))
      ;; Check for :when
      (when (eq (first key-bindings) :when)
        (setf when-clause (second key-bindings))
        (setf key-bindings (cddr key-bindings)))
      (values mode-name when-clause
              (loop for (pattern . handler-body) in key-bindings
                    unless (or (eq pattern :focus-next)
                               (eq (first handler-body) :focus-next))
                    collect `(,(%compile-key-pattern pattern) ,@handler-body))))))

(defun %compile-modal-keys (key-specs)
  "Compile :keys with :mode sections into a handler lambda.
Modes are checked in order; first active mode wins."
  (let ((mode-forms
          (loop for spec in key-specs
                when (eq (car spec) :mode)
                collect (multiple-value-bind (mode-name when-clause branches)
                            (%compile-mode-keys spec)
                          (declare (ignore mode-name))
                          (if when-clause
                              `(when ,when-clause
                                 (cond ,@branches))
                              `(cond ,@branches))))))
    `(lambda (event node)
       (declare (ignorable node))
       (when (typep event 'ptui.core.events:key-event)
         (let ((key (ptui.core.events:key-event-key event)))
           (declare (ignorable key))
           (or ,@mode-forms))))))

(defun %compile-panel-keys (key-specs)
  "Compile :keys section into a handler lambda.
Supports both flat key bindings and modal (:mode ...) groups.
Returns a form that produces an event handler function, or NIL."
  (when (null key-specs)
    (return-from %compile-panel-keys nil))
  ;; Check for modal key groups
  (when (%key-specs-have-modes-p key-specs)
    (return-from %compile-panel-keys
      (%compile-modal-keys key-specs)))
  ;; Flat key bindings (original behavior)
  (let ((clauses
          (loop for (pattern . handler-body) in key-specs
                collect (cond
                          ((eq pattern :focus-next)
                           nil)
                          ((eq (first handler-body) :focus-next)
                           nil)
                          (t
                           `(,(if (and (keywordp pattern)
                                       (let ((name (symbol-name pattern)))
                                         (and (> (length name) 5)
                                              (string= name "CTRL-" :end1 5))))
                                  :any
                                  pattern)
                             ,@handler-body))))))
    (let ((filtered (remove nil clauses)))
      (when filtered
        `(lambda (event node)
           (declare (ignorable node))
           (when (typep event 'ptui.core.events:key-event)
             (let ((key (ptui.core.events:key-event-key event)))
               (declare (ignorable key))
               (cond
                 ,@(loop for (pattern . handler-body) in key-specs
                         unless (or (eq pattern :focus-next)
                                    (eq (first handler-body) :focus-next))
                         collect `(,(%compile-key-pattern pattern)
                                   ,@handler-body))))))))))

;;; ===================================================================
;;; I290: defpanel Macro Assembly
;;; ===================================================================

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

(defun %wrap-with-keys (keys-form layout-form panel-name)
  "If KEYS-FORM is non-nil, wrap layout-form to attach key handler."
  (if keys-form
      `(let ((%panel-tree ,layout-form)
             (%panel-handler ,keys-form)
             (%widget-context ptui.ui.runtime:*current-widget-context*))
         ;; Attach the key handler to the root element
         ;; Ensure root has a stable id so focused key events can route.
         (ptui.ui.elements:make-element
          (ptui.ui.elements:ui-element-type %panel-tree)
          :id (or (ptui.ui.elements:ui-element-id %panel-tree)
                  (and %widget-context
                       (list :panel-root
                             (ptui.ui.runtime:widget-context-widget-name %widget-context)
                             (ptui.ui.runtime:widget-context-instance-key %widget-context)))
                  (list :panel-root ',panel-name))
          :key (ptui.ui.elements:ui-element-key %panel-tree)
          :props (append (ptui.ui.elements:ui-element-props %panel-tree)
                         (list :on-event %panel-handler))
          :children (ptui.ui.elements:ui-element-children %panel-tree)
          :focusablep t))
      layout-form))
