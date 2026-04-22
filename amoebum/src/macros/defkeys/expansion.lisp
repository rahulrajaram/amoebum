(in-package :amoebum)

;;;; ---------------------------------------------------------------------------
;;;; DEFKEYS macroexpansion.
;;;;
;;;; Compile-time helpers for parsing binding option plists, turning a
;;;; `(key-spec handler-form &rest options)` form into a parsed plist,
;;;; warning on duplicate bindings within the same keymap, and the
;;;; `defkeys` macro itself.
;;;;
;;;; Macroexpansion output is defended by
;;;; `amoebum/test/snapshots/macroexpand/defkeys-*.sexp` (NXT-391 goldens).
;;;; Any change here that alters macroexpand-1 output will fail those goldens
;;;; — that is intentional.
;;;;
;;;; Behavior is preserved verbatim from the original
;;;; `amoebum/src/macros/defkeys.lisp`; only file boundaries change.
;;;; ---------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %parse-binding-options (options key-spec)
    (unless (evenp (length options))
      (error "Binding options for key ~S must be key/value pairs." key-spec))
    (let ((guard nil)
          (description nil))
      (loop for (option value) on options by #'cddr do
        (ecase option
          (:when
           (setf guard value))
          (:description
           (setf description (princ-to-string value)))))
      (values guard description)))

  (defun %parse-defkeys-binding (form)
    (unless (and (consp form)
                 (stringp (first form))
                 (>= (length form) 2))
      (error "Invalid DEfKEYS binding form ~S." form))
    (let* ((key-spec (first form))
           (signature (%parse-key-spec key-spec))
           (handler-form (second form))
           (options (cddr form)))
      (multiple-value-bind (guard description)
          (%parse-binding-options options key-spec)
        (list :key-spec key-spec
              :signature signature
              :handler-form handler-form
              :guard-form guard
              :description description))))

  (defun %warn-on-duplicate-bindings (name parsed-bindings)
    (let ((seen (make-hash-table :test #'equal)))
      (dolist (binding parsed-bindings)
        (let ((signature (getf binding :signature))
              (key-spec (getf binding :key-spec)))
          (if (gethash signature seen)
              (warn 'keymap-definition-warning
                    :keymap name
                    :key-spec key-spec
                    :reason "Duplicate key binding in the same keymap.")
              (setf (gethash signature seen) t)))))))

(defmacro defkeys (name &body forms)
  (unless (symbolp name)
    (error "DEFKEYS name must be a symbol, got ~S." name))
  (let* ((docstring (and forms (stringp (first forms)) (first forms)))
         (binding-forms (if docstring (rest forms) forms))
         (parsed-bindings (mapcar #'%parse-defkeys-binding binding-forms))
         (state-symbol (or (find-symbol "STATE" *package*)
                           (intern "STATE" *package*)))
         (key-event-symbol (or (find-symbol "KEY-EVENT" *package*)
                               (intern "KEY-EVENT" *package*)))
         (keymap-symbol
           (intern (format nil "*~A-KEYMAP*" (string-upcase (symbol-name name)))
                   (find-package :amoebum)))
         (source-file (or *compile-file-truename* *load-truename*)))
    (%warn-on-duplicate-bindings name parsed-bindings)
    `(progn
       (defparameter ,keymap-symbol
         (let ((map (make-keymap :name ',name
                                 :description ,(or docstring
                                                   (format nil "Keymap ~A." name)))))
           ,@(mapcar
              (lambda (binding)
                (let ((signature (getf binding :signature))
                      (handler-form (getf binding :handler-form))
                      (guard-form (getf binding :guard-form))
                      (description (getf binding :description))
                      (key-spec (getf binding :key-spec))
                      (state-var (gensym "STATE-"))
                      (event-var (gensym "KEY-EVENT-")))
                  `(register-key-binding
                    map
                    ',signature
                    (lambda (,state-var ,event-var)
                      (let ((,state-symbol ,state-var)
                            (,key-event-symbol ,event-var))
                        (declare (ignorable ,state-symbol ,key-event-symbol))
                        ,handler-form))
                    :guard ,(if guard-form
                                `(lambda (,state-var ,event-var)
                                   (let ((,state-symbol ,state-var)
                                         (,key-event-symbol ,event-var))
                                     (declare (ignorable ,key-event-symbol))
                                     ,guard-form))
                                nil)
                    :description ,(or description (format nil "~A" key-spec))
                    :source-file ,source-file)))
              parsed-bindings)
           (register-keymap map)
           map))
       ',name)))
