(in-package :ptui.ui.definition-loader)

(defun %validate-packet-argument-rules (path index form schema context)
  (dolist (rule (packet-directive-schema-argument-rules schema))
    (let ((value (nth (packet-argument-rule-position rule) form)))
      (unless (funcall (packet-argument-rule-predicate rule) value)
        (%packet-schema-error path index form
                              (packet-argument-rule-detail rule)
                              context)))))

(defun %validate-packet-option-rules (path index form schema context)
  (let* ((options (%packet-directive-options form))
         (option-rules (packet-directive-schema-option-rules schema))
         (allowed-keys (mapcar #'packet-option-rule-key option-rules)))
    (when option-rules
      (%validate-directive-options path index form options allowed-keys context)
      (dolist (rule option-rules)
        (let ((marker (gensym "MISSING"))
              (key (packet-option-rule-key rule)))
          (let ((value (getf options key marker)))
            (unless (eq value marker)
              (unless (funcall (packet-option-rule-predicate rule) value)
                (%packet-schema-error path index form
                                      (packet-option-rule-detail rule)
                                      context)))))))))

(defun %validate-wrapper-directive (form path index allowed-directives context directive-name schema)
  (let ((children (cdr form)))
    (when (< (length children) (packet-directive-schema-min-children schema))
      (%packet-schema-error path index form
                            "Wrapper directive requires at least one child directive."
                            context))
    (loop for child in children
          for child-index from 1
          do (unless (and (consp child) (keywordp (car child)))
               (%packet-schema-error
                path index child
                (or (packet-directive-schema-child-detail schema)
                    "Wrapper children must be keyword directives.")
                (format nil ":~(~A~) child #~D"
                        directive-name child-index)))
             (validate-packet-form child
                                   :path path
                                   :index index
                                   :allowed-directives allowed-directives
                                   :context
                                   (format nil ":~(~A~) child #~D"
                                           directive-name child-index)))))

(defun %validate-directive-with-schema (form path index allowed-directives context directive-name schema)
  (if (packet-directive-schema-wrapper-p schema)
      (%validate-wrapper-directive form path index allowed-directives context directive-name schema)
      (progn
        (%validate-packet-argument-rules path index form schema context)
        (%validate-packet-option-rules path index form schema context))))

(defun validate-packet-form (form &key (path "<memory>") (index 0)
                                  (allowed-directives +default-packet-directives+)
                                  context)
  "Validate one packet FORM against ALLOWED-DIRECTIVES.
Non-keyword top-level forms are accepted for backward compatibility."
  (let ((directive-name (%declarative-directive-name form)))
    (cond
      ((null directive-name) t)
      ((not (member directive-name allowed-directives :test #'string=))
       (%packet-schema-error
        path index form
        (format nil
                "Unknown packet directive :~(~A~). Supported directives: ~A."
                directive-name
                (%packet-supported-directives-string allowed-directives))
        context))
      (t
       (let ((schema (%find-packet-directive-schema directive-name)))
         (unless schema
           (%packet-schema-error
            path index form
            (format nil
                    "No validation schema registered for :~(~A~)."
                    directive-name)
            context))
         (%validate-directive-with-schema form path index allowed-directives context directive-name schema))))
  t))

(defun validate-packet-forms (forms &key (path "<memory>")
                                    (allowed-directives +default-packet-directives+))
  "Validate packet FORMS before declarative expansion."
  (loop for form in forms
        for index from 1
        do (validate-packet-form form
                                 :path path
                                 :index index
                                 :allowed-directives allowed-directives))
  t)

(defun validate-definition-form (form &key (path "<memory>") (index 0)
                                      (allowed-operators +default-allowed-top-level-operators+))
  "Validate a top-level definition FORM against ALLOWED-OPERATORS."
  (unless (consp form)
    (%loader-error path index form
                   "Top-level form must be a non-empty list."))
  (let ((operator (car form))
        (operator-name (%form-operator-name form)))
    (unless (and (symbolp operator) operator-name)
      (%loader-error path index form
                     "Top-level form head must be a symbol."))
    (unless (member operator-name allowed-operators :test #'string=)
      (%loader-error path index form
                     (format nil
                             "Top-level operator ~S is not allowlisted."
                             operator-name)))
    (case (%definition-form-kind form)
      (:panel
       (unless (symbolp (%definition-form-name form))
         (%loader-error path index form
                        "DEFPANEL requires a symbol name as the second argument."))
       (unless (listp (third form))
         (%loader-error path index form
                        "DEFPANEL requires a lambda-list as the third argument.")))
      (:app
       (unless (symbolp (%definition-form-name form))
         (%loader-error path index form
                        "DEFAPP requires a symbol name as the second argument."))
       (unless (listp (third form))
         (%loader-error path index form
                        "DEFAPP requires an option plist list as the third argument.")))
      (:widget
       (unless (symbolp (%definition-form-name form))
         (%loader-error path index form
                        "DEFWIDGET requires a symbol name as the second argument."))
       (unless (listp (third form))
         (%loader-error path index form
                        "DEFWIDGET requires a lambda-list as the third argument.")))))
  t)
