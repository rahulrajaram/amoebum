(defpackage :ptui.ui.hooks
  (:use :cl)
  (:export
   ;; Scoped state (I269)
   #:use-state
   ;; State cleanup (I271)
   #:cleanup-widget-state
   ;; Effect hooks (I272-I273)
   #:use-effect
   ;; Memo/callback (I274)
   #:use-memo
   #:use-callback
   ;; Event map (I278)
   #:use-event-map))

(in-package :ptui.ui.hooks)

;;; ===================================================================
;;; I269: Scoped State Primitives
;;; Widget context struct lives in ptui.ui.runtime to avoid circular deps.
;;; ===================================================================

(defun %ensure-context (hook-name)
  "Signal error if called outside widget render context."
  (unless ptui.ui.runtime:*current-widget-context*
    (error "~A called outside widget render context. ~
            Hooks may only be called inside a defwidget body." hook-name))
  ptui.ui.runtime:*current-widget-context*)

(defun %state-key (ctx state-name)
  "Construct compound state-table key for scoped state."
  (list (ptui.ui.runtime:widget-context-widget-name ctx)
        (ptui.ui.runtime:widget-context-instance-key ctx)
        state-name))

(defmacro use-state (name &key initial-value)
  "Declare scoped widget state NAME with optional INITIAL-VALUE.
Returns (values current-value setter-fn).
Must be called inside a defwidget body."
  (let ((ctx-var (gensym "CTX"))
        (rt-var (gensym "RT"))
        (key-var (gensym "KEY"))
        (found-var (gensym "FOUND")))
    `(let* ((,ctx-var (ptui.ui.hooks::%ensure-context 'use-state))
            (,rt-var (ptui.ui.runtime:widget-context-runtime ,ctx-var))
            (,key-var (ptui.ui.hooks::%state-key ,ctx-var ',name)))
       (multiple-value-bind (val ,found-var)
           (ptui.ui.runtime:runtime-state ,rt-var ,key-var)
         (unless ,found-var
           (setf val ,initial-value)
           (ptui.ui.runtime:set-runtime-state ,rt-var ,key-var val))
         (values val
                 (lambda (new-val)
                   (ptui.ui.runtime:set-runtime-state ,rt-var ,key-var new-val)
                   (ptui.widgets.defwidget:mark-widget-dirty
                    (ptui.ui.runtime:widget-context-widget-name ,ctx-var))
                   new-val))))))

;;; ===================================================================
;;; I271: State Cleanup and Instance Lifecycle
;;; ===================================================================

(defun cleanup-widget-state (runtime widget-name instance-key)
  "Remove all state-table entries scoped to (WIDGET-NAME INSTANCE-KEY *).
Called on widget unmount to prevent state-table leaks."
  (let ((table (ptui.ui.runtime::runtime-state-table runtime))
        (to-remove '()))
    (maphash (lambda (key _value)
               (declare (ignore _value))
               (when (and (listp key)
                          (>= (length key) 2)
                          (eq (first key) widget-name)
                          (equal (second key) instance-key))
                 (push key to-remove)))
             table)
    (dolist (key to-remove)
      ;; Run effect cleanup before removing state
      (when (and (listp key)
                 (>= (length key) 3)
                 (listp (third key))
                 (eq (first (third key)) :effect-cleanup))
        (let ((cleanup-fn (gethash key table)))
          (when (functionp cleanup-fn)
            (ignore-errors (funcall cleanup-fn)))))
      (remhash key table))
    (length to-remove)))

;;; ===================================================================
;;; I272-I273: use-effect with Dep Tracking and Cleanup
;;; ===================================================================

(defun %deps-changed-p (old-deps new-deps)
  "Return T if deps have changed. Always T if no previous deps."
  (cond
    ((null old-deps) t)
    ((not (= (length (car old-deps)) (length new-deps))) t)
    (t (not (every #'equal (car old-deps) new-deps)))))

(defmacro use-effect (name (&key deps) &body body)
  "Run BODY as post-reconciliation effect when DEPS change.
NAME is a compile-time symbol scoping this effect within the widget instance.
If BODY returns a function, it becomes the cleanup — called before re-execution
and on widget unmount."
  (let ((ctx-var (gensym "CTX"))
        (rt-var (gensym "RT"))
        (deps-key-var (gensym "DEPS-KEY"))
        (cleanup-key-var (gensym "CLEANUP-KEY"))
        (prev-var (gensym "PREV"))
        (prev-found-var (gensym "PREV-FOUND"))
        (new-deps-var (gensym "NEW-DEPS")))
    `(let* ((,ctx-var (ptui.ui.hooks::%ensure-context 'use-effect))
            (,rt-var (ptui.ui.runtime:widget-context-runtime ,ctx-var))
            (,deps-key-var (ptui.ui.hooks::%state-key ,ctx-var '(:effect-deps ,name)))
            (,cleanup-key-var (ptui.ui.hooks::%state-key ,ctx-var '(:effect-cleanup ,name)))
            (,new-deps-var (list ,@deps)))
       (multiple-value-bind (,prev-var ,prev-found-var)
           (ptui.ui.runtime:runtime-state ,rt-var ,deps-key-var)
         (when (or (not ,prev-found-var)
                   (ptui.ui.hooks::%deps-changed-p
                    (when ,prev-found-var (list ,prev-var))
                    ,new-deps-var))
           (ptui.ui.runtime:set-runtime-state ,rt-var ,deps-key-var ,new-deps-var)
           (ptui.ui.runtime:enqueue-effect
            ,rt-var
            (lambda ()
              ;; Run previous cleanup if any
              (let ((prev-cleanup (ptui.ui.runtime:runtime-state ,rt-var ,cleanup-key-var)))
                (when (functionp prev-cleanup)
                  (funcall prev-cleanup)))
              ;; Run effect body and store cleanup if returned
              (let ((result (progn ,@body)))
                (ptui.ui.runtime:set-runtime-state
                 ,rt-var ,cleanup-key-var
                 (if (functionp result) result nil))))))))))

;;; ===================================================================
;;; I274: use-memo and use-callback
;;; ===================================================================

(defmacro use-memo (name (&key deps) &body body)
  "Cache result of BODY, recomputing only when DEPS change.
Returns the cached or freshly computed value."
  (let ((ctx-var (gensym "CTX"))
        (rt-var (gensym "RT"))
        (memo-key-var (gensym "MEMO-KEY"))
        (deps-key-var (gensym "DEPS-KEY"))
        (new-deps-var (gensym "NEW-DEPS"))
        (prev-var (gensym "PREV"))
        (prev-found-var (gensym "PREV-FOUND")))
    `(let* ((,ctx-var (ptui.ui.hooks::%ensure-context 'use-memo))
            (,rt-var (ptui.ui.runtime:widget-context-runtime ,ctx-var))
            (,memo-key-var (ptui.ui.hooks::%state-key ,ctx-var '(:memo ,name)))
            (,deps-key-var (ptui.ui.hooks::%state-key ,ctx-var '(:memo-deps ,name)))
            (,new-deps-var (list ,@deps)))
       (multiple-value-bind (,prev-var ,prev-found-var)
           (ptui.ui.runtime:runtime-state ,rt-var ,deps-key-var)
         (if (and ,prev-found-var
                  (not (ptui.ui.hooks::%deps-changed-p
                        (list ,prev-var)
                        ,new-deps-var)))
             (ptui.ui.runtime:runtime-state ,rt-var ,memo-key-var)
             (let ((value (progn ,@body)))
               (ptui.ui.runtime:set-runtime-state ,rt-var ,deps-key-var ,new-deps-var)
               (ptui.ui.runtime:set-runtime-state ,rt-var ,memo-key-var value)
               value))))))

(defmacro use-callback (name (&key deps) &body body)
  "Cache a function, recomputing only when DEPS change.
Semantically identical to use-memo but communicates intent."
  `(use-memo ,name (:deps ,deps) ,@body))

;;; ===================================================================
;;; I278: use-event-map — Declarative Event Routing
;;; ===================================================================

(defun %compile-event-clause (clause)
  "Compile a single use-event-map clause into a cond branch."
  (destructuring-bind (pattern &body handler-body) clause
    (cond
      ((eq pattern :any)
       `(t ,@handler-body))
      ((keywordp pattern)
       `((eql key ,pattern) ,@handler-body))
      ((and (listp pattern) (eq (first pattern) :text))
       (let ((char-var (second pattern)))
         `((and (eql key :text) (string= text ,(string char-var)))
           ,@handler-body)))
      (t
       (error "use-event-map: invalid pattern ~S. Expected keyword, (:text ch), or :any."
              pattern)))))

(defmacro use-event-map (name &body clauses)
  "Declare key-to-handler mappings. Returns :on-event handler function.
Each clause: (key-pattern &body handler-body)
Patterns: keyword (:enter, :backspace), (:text ch) for char match, :any catch-all."
  (declare (ignore name))
  (let ((event-var (gensym "EVENT"))
        (node-var (gensym "NODE")))
    `(lambda (,event-var ,node-var)
       (declare (ignorable ,node-var))
       (when (typep ,event-var 'ptui.core.events:key-event)
         (let ((key (ptui.core.events:key-event-key ,event-var))
               (text (ptui.core.events:key-event-text? ,event-var)))
           (declare (ignorable key text))
           (cond
             ,@(mapcar #'%compile-event-clause clauses)))))))
