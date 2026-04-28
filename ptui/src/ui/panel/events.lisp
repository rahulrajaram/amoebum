(in-package :ptui.ui.panel)

;;; ===================================================================
;;; defpanel events & focus traversal
;;;
;;; Event-handling pipeline for defpanel: compiles :keys forms (flat
;;; and modal :mode groups) into key-event handler lambdas, and wires
;;; the resulting handler to the layout root via %wrap-with-keys so
;;; focus traversal can route key events to the panel root by id.
;;; ===================================================================

;;; -------------------------------------------------------------------
;;; :keys compilation (flat + :mode modal groups)
;;; -------------------------------------------------------------------

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

;;; -------------------------------------------------------------------
;;; Focus traversal: attach handler to layout root for routed events
;;; -------------------------------------------------------------------

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
