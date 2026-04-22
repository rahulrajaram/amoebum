(in-package :amoebum)

;;;; ---------------------------------------------------------------------------
;;;; Skill macroexpansion layer.
;;;;
;;;; All compile-time helpers (argument-spec normalization, handler/completer
;;;; symbol naming, usage-string defaulting, declaration parsing, expansion
;;;; builder) plus the `defskill` macro itself.
;;;;
;;;; Macroexpansion output is defended by
;;;; `amoebum/test/snapshots/macroexpand/defskill*.sexp` (NXT-391 goldens).
;;;; Any change here that alters macroexpand-1 output by even one character
;;;; will fail the golden suite — that is intentional.
;;;;
;;;; Behavior is preserved verbatim from the original
;;;; `amoebum/src/macros/defskill.lisp`; only file boundaries change.
;;;; ---------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %normalize-skill-type (type-spec)
    (cond
      ((or (eq type-spec :string)
           (eq type-spec 'string))
       :string)
      ((or (eq type-spec :integer)
           (eq type-spec 'integer)
           (eq type-spec 'fixnum))
       :integer)
      ((or (eq type-spec :keyword)
           (eq type-spec 'keyword)
           (eq type-spec 'symbol))
       :keyword)
      ((or (eq type-spec :boolean)
           (eq type-spec 'boolean))
       :boolean)
      ((and (consp type-spec)
            (eq (first type-spec) 'member))
       :keyword)
      (t
       :string)))

  (defun %skill-name->variable (name)
    (etypecase name
      (symbol name)
      (string (intern (string-upcase name) *package*)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %parse-skill-declarations (forms)
    (let ((options (list :category :general
                         :keybinding nil
                         :aliases '()
                         :usage nil
                         :args nil
                         :completer nil))
          (remaining forms))
      (loop while (and remaining
                       (consp (first remaining))
                       (keywordp (first (first remaining))))
            do (let ((declaration (first remaining)))
                 (destructuring-bind (keyword value &rest extra) declaration
                   (declare (ignore extra))
                   (unless (member keyword
                                   '(:category :keybinding :aliases :usage :args :completer)
                                   :test #'eq)
                     (error "Unknown DEFSKILL declaration keyword: ~S" keyword))
                   (setf (getf options keyword) value)))
               (setf remaining (rest remaining)))
      (values options remaining)))

  (defun %normalize-skill-argument-spec (spec)
    (labels ((ensure-options (name options)
               (unless (evenp (length options))
                 (error "Skill argument options must be key/value pairs for ~S." name)))
             (normalize (name type options)
               (ensure-options name options)
               (let* ((default-supplied-p (member :default options :test #'eq))
                      (default (getf options :default))
                      (required-explicit-p (member :required options :test #'eq))
                      (required-p
                        (if required-explicit-p
                            (not (null (getf options :required)))
                            (not default-supplied-p)))
                      (greedy-p (not (null (getf options :greedy))))
                      (prompt (getf options :prompt))
                      (description (getf options :description))
                      (raw-choices (getf options :choices))
                      (choices-value
                        (if (and (consp raw-choices)
                                 (eq (first raw-choices) 'quote))
                            (second raw-choices)
                            raw-choices))
                      (type* (or type :string))
                      (choices (if (and (consp type*)
                                        (eq (first type*) 'member)
                                        (null choices-value))
                                   (rest type*)
                                   choices-value)))
                 (list :name (if (symbolp name)
                                 (string-downcase (symbol-name name))
                                 (string-downcase (princ-to-string name)))
                       :variable (%skill-name->variable
                                  (if (symbolp name)
                                      name
                                      (princ-to-string name)))
                       :type (%normalize-skill-type type*)
                       :required-p required-p
                       :default default
                       :default-supplied-p (not (null default-supplied-p))
                       :choices choices
                       :greedy-p greedy-p
                       :prompt (and prompt (princ-to-string prompt))
                       :description (and description (princ-to-string description))
                       :completer (getf options :completer)))))
      (cond
        ((symbolp spec)
         (normalize spec :string '()))
        ((and (consp spec) (symbolp (first spec)))
         (let* ((name (first spec))
                (tail (rest spec))
                (type (when tail (first tail)))
                (type-keyword-p (member type '(:string :integer :keyword :boolean)
                                        :test #'eq))
                (options (if (and type
                                  (keywordp type)
                                  (not type-keyword-p))
                             tail
                             (rest tail)))
                (type* (if (and type
                                (keywordp type)
                                (not type-keyword-p))
                           :string
                           type)))
           (normalize name type* options)))
        (t
         (error "Invalid skill argument spec: ~S" spec)))))

  (defun %skill-handler-symbol (name)
    (intern (format nil "%SKILL-HANDLER-~A"
                    (string-upcase (symbol-name name)))
            (find-package :amoebum)))

  (defun %skill-completer-symbol (name)
    (intern (format nil "%SKILL-COMPLETER-~A"
                    (string-upcase (symbol-name name)))
            (find-package :amoebum)))

  (defun %skill-arg-plist-constant (argument)
    (list :name (getf argument :name)
          :type (getf argument :type)
          :required-p (not (null (getf argument :required-p)))
          :default (getf argument :default)
          :default-supplied-p (not (null (getf argument :default-supplied-p)))
          :prompt (getf argument :prompt)
          :choices (getf argument :choices)
          :greedy-p (not (null (getf argument :greedy-p)))
          :description (getf argument :description)
          :completer (getf argument :completer)))

  (defun %skill-default-usage (name normalized-args)
    (let ((parts '()))
      (dolist (arg normalized-args)
        (let* ((token
                 (if (getf arg :greedy-p)
                     (format nil "<~A...>" (getf arg :name))
                     (format nil "<~A>" (getf arg :name))))
               (rendered
                 (if (getf arg :required-p)
                     token
                     (format nil "[~A]" token))))
          (push rendered parts)))
      (format nil "/~A~@[ ~{~A~^ ~}~]"
              (%normalize-skill-name name)
              (nreverse parts))))

  (defun %normalize-skill-aliases (raw-aliases)
    (cond
      ((null raw-aliases) '())
      ((listp raw-aliases) raw-aliases)
      (t (list raw-aliases))))

  (defun %skill-binding-form (argument)
    (let ((variable (getf argument :variable))
          (arg-name (getf argument :name))
          (default (getf argument :default))
          (default-supplied-p (not (null (getf argument :default-supplied-p)))))
      `(,variable (%skill-argument-value arguments
                                         ,arg-name
                                         ',default
                                         ,default-supplied-p))))

  (defun %skill-argument-constructor-form (argument)
    `(make-skill-argument
      :name ,(getf argument :name)
      :variable ',(getf argument :variable)
      :type ,(getf argument :type)
      :required-p ,(not (null (getf argument :required-p)))
      :default ',(getf argument :default)
      :default-supplied-p ,(not (null (getf argument :default-supplied-p)))
      :choices ',(getf argument :choices)
      :greedy-p ,(not (null (getf argument :greedy-p)))
      :prompt ,(getf argument :prompt)
      :description ,(getf argument :description)
      :completer ,(getf argument :completer)))

  (defun %build-defskill-expansion (name docstring usage aliases category keybinding
                                    arg-plists argument-forms handler-symbol
                                    completer-symbol custom-completer binding-forms
                                    declare-ignorable body-forms)
    `(progn
       (defun ,handler-symbol (invocation arguments context)
         (declare (ignorable invocation context))
         (let ((missing (%skill-missing-required-arguments ',arg-plists arguments)))
           (when missing
             (return-from ,handler-symbol
               (make-slash-command-result
                :echo-input-p t
                :output (%skill-missing-arguments-output
                         ',name
                         ,usage
                         missing)))))
         (let* (,@binding-forms)
           (declare (ignorable ,@declare-ignorable))
           ,@body-forms))
       (defun ,completer-symbol (command invocation index fragment prefix-tokens)
         (declare (ignore command invocation))
         (or (and ,custom-completer
                  (funcall ,custom-completer
                           index
                           fragment
                           prefix-tokens))
             (%skill-default-completer ',arg-plists index fragment prefix-tokens)))
       (register-skill
        (make-skill-metadata
         :name ,(%normalize-skill-name name)
         :description ,docstring
         :usage ,usage
         :aliases ',aliases
         :category ,category
         :keybinding ,keybinding
         :arguments (list ,@argument-forms)
         :handler #',handler-symbol
         :completer #',completer-symbol
         :source-file ,(or *compile-file-truename*
                           *load-truename*
                           nil)
         :source-line nil
         :defined-at (%skill-now-ms))))))

(defmacro defskill (name argument-specs &body forms)
  (unless (symbolp name)
    (error "DEFSKILL name must be a symbol, got ~S." name))
  (let* ((docstring (and forms (stringp (first forms)) (first forms)))
         (tail (if docstring (rest forms) forms)))
    (multiple-value-bind (declarations body-forms)
        (%parse-skill-declarations tail)
      (when (null body-forms)
        (error "DEFSKILL ~S requires a body." name))
      (let* ((raw-args (or (getf declarations :args) argument-specs '()))
             (normalized-args (mapcar #'%normalize-skill-argument-spec raw-args))
             (arg-plists (mapcar #'%skill-arg-plist-constant normalized-args))
             (usage (or (getf declarations :usage)
                        (%skill-default-usage name normalized-args)))
             (aliases (%normalize-skill-aliases (getf declarations :aliases)))
             (category (or (getf declarations :category) :general))
             (keybinding (getf declarations :keybinding))
             (custom-completer (getf declarations :completer))
             (handler-symbol (%skill-handler-symbol name))
             (completer-symbol (%skill-completer-symbol name))
             (binding-forms (mapcar #'%skill-binding-form normalized-args))
             (declare-ignorable (mapcar (lambda (arg) (getf arg :variable))
                                        normalized-args))
             (argument-forms (mapcar #'%skill-argument-constructor-form normalized-args)))
        (%build-defskill-expansion name docstring usage aliases category keybinding
                                   arg-plists argument-forms handler-symbol
                                   completer-symbol custom-completer binding-forms
                                   declare-ignorable body-forms)))))
