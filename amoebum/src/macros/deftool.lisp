(in-package :amoebum)

(defparameter *toolset* (pseudopod:make-toolset))
(defparameter *tool-metadata* (make-hash-table :test #'equal))

(defparameter *cl-type-schema-type-table*
  (let ((table (make-hash-table :test #'eq)))
    (setf (gethash 'string table) "string")
    (setf (gethash 'integer table) "integer")
    (setf (gethash 'boolean table) "boolean")
    (setf (gethash 'pathname table) "string")
    (setf (gethash 'list table) "array")
    (setf (gethash 'null table) "null")
    table))

(defconstant +missing-tool-argument+ :amoebum/missing-tool-argument)

(defstruct (tool-metadata
            (:constructor make-tool-metadata
                (&key name permission dangerous-p category timeout-seconds
                 source-file source-line parameter-specs defined-at)))
  name
  permission
  dangerous-p
  category
  timeout-seconds
  source-file
  source-line
  parameter-specs
  defined-at)

(defun %ensure-toolset ()
  (unless (and (boundp '*toolset*)
               (pseudopod:toolset-p *toolset*))
    (setf *toolset* (pseudopod:make-toolset)))
  *toolset*)

(defun %tool-name-string (name)
  (string-downcase (symbol-name name)))

(defun %hash-table-keys (table)
  (loop for key being the hash-keys of table collect key))

(defun %getf-boolean (plist key default)
  (if (member key plist :test #'eq)
      (not (null (getf plist key)))
      default))

(defun %normalize-parameter-spec (spec)
  (unless (and (consp spec) (symbolp (first spec)))
    (error "Invalid deftool parameter spec: ~S" spec))
  (destructuring-bind (name type &rest options) spec
    (unless (evenp (length options))
      (error "Parameter options must be key/value pairs for ~S." name))
    (let ((description (getf options :description))
          (required (%getf-boolean options :required nil))
          (default-supplied-p (member :default options :test #'eq)))
      (list :name name
            :type type
            :description (and description (princ-to-string description))
            :required required
            :default (getf options :default)
            :default-supplied-p (not (null default-supplied-p))))))

(defun %json-enum-value (value)
  (typecase value
    (string value)
    (symbol (string-downcase (symbol-name value)))
    (number value)
    (t (princ-to-string value))))

(defun %copy-hash-table-shallow (source)
  (let ((copy (make-hash-table :test #'equal)))
    (maphash (lambda (key value)
               (setf (gethash key copy) value))
             source)
    copy))

(defun %schema-type-value-list (type-spec)
  (cond
    ((and (consp type-spec) (eq (first type-spec) 'or))
     (remove-duplicates
      (mapcan #'%schema-type-value-list (rest type-spec))
      :test #'string=))
    ((and (consp type-spec) (eq (first type-spec) 'integer))
     (list "integer"))
    ((gethash type-spec *cl-type-schema-type-table*)
     (list (gethash type-spec *cl-type-schema-type-table*)))
    (t
     (list "string"))))

(defun cl-type-to-json-schema (type-spec)
  (let ((schema (make-hash-table :test #'equal)))
    (cond
      ((and (consp type-spec) (eq (first type-spec) 'member))
       (setf (gethash "type" schema) "string")
       (setf (gethash "enum" schema)
             (mapcar #'%json-enum-value (rest type-spec))))
      ((and (consp type-spec)
            (eq (first type-spec) 'or)
            (member 'null (rest type-spec) :test #'equal)
            (= 2 (length (rest type-spec))))
       (let* ((non-null-type
                (find-if (lambda (value) (not (equal value 'null)))
                         (rest type-spec)))
              (base (cl-type-to-json-schema non-null-type))
              (base-type (gethash "type" base)))
         (setf schema (%copy-hash-table-shallow base))
         (setf (gethash "type" schema)
               (remove-duplicates
                (append (if (listp base-type)
                            base-type
                            (list base-type))
                        (list "null"))
                :test #'string=))))
      (t
       (let ((type-values (%schema-type-value-list type-spec)))
         (setf (gethash "type" schema)
               (if (= 1 (length type-values))
                   (first type-values)
                   type-values))
         (when (or (eq type-spec 'pathname)
                   (equal type-spec 'pathname))
           (setf (gethash "format" schema) "path"))
         (when (and (consp type-spec) (eq (first type-spec) 'integer))
           (let ((lower (second type-spec))
                 (upper (third type-spec)))
             (when (integerp lower)
               (setf (gethash "minimum" schema) lower))
             (when (integerp upper)
               (setf (gethash "maximum" schema) upper)))))))
    schema))

(defun %tool-schema-from-parameter-specs (parameter-specs)
  (let ((schema (make-hash-table :test #'equal))
        (properties (make-hash-table :test #'equal))
        (required '()))
    (setf (gethash "type" schema) "object")
    (dolist (parameter parameter-specs)
      (let* ((name (getf parameter :name))
             (type (getf parameter :type))
             (property-schema (cl-type-to-json-schema type))
             (description (getf parameter :description))
             (required-p (getf parameter :required))
             (property-key (string-downcase (symbol-name name))))
        (when description
          (setf (gethash "description" property-schema) description))
        (setf (gethash property-key properties) property-schema)
        (when required-p
          (push property-key required))))
    (setf (gethash "properties" schema) properties)
    (when required
      (setf (gethash "required" schema) (nreverse required)))
    schema))

(defun %extract-tool-argument (arguments key)
  (unless (hash-table-p arguments)
    (error "Tool arguments must be a hash-table, got ~S." arguments))
  (multiple-value-bind (value present-p) (gethash key arguments)
    (if present-p
        value
        +missing-tool-argument+)))

(defun %coerce-boolean (value parameter-name)
  (cond
    ((or (eq value t) (eq value nil)) value)
    ((stringp value)
     (let ((normalized (string-downcase (string-trim '(#\Space #\Tab #\Newline #\Return)
                                                     value))))
       (cond
         ((member normalized '("true" "t" "1" "yes") :test #'string=) t)
         ((member normalized '("false" "f" "0" "no" "nil" "") :test #'string=) nil)
         (t (error "Invalid boolean value for ~S: ~S" parameter-name value)))))
    ((numberp value) (not (zerop value)))
    (t (if value t nil))))

(defun %coerce-member-value (value members)
  (cond
    ((member value members :test #'equal)
     value)
    ((stringp value)
     (or (find-if
          (lambda (candidate)
            (string= value (%json-enum-value candidate)))
          members)
         value))
    (t value)))

(defun %coerce-tool-argument (value type-spec parameter-name)
  (cond
    ((eq value +missing-tool-argument+) value)
    ((and (consp type-spec) (eq (first type-spec) 'or))
     (if (null value)
         nil
         (let ((non-null-types (remove 'null (rest type-spec) :test #'equal)))
           (if (= 1 (length non-null-types))
               (%coerce-tool-argument value (first non-null-types) parameter-name)
               value))))
    ((eq type-spec 'pathname)
     (typecase value
       (pathname value)
       (string (pathname value))
       (t value)))
    ((or (eq type-spec 'integer)
         (and (consp type-spec) (eq (first type-spec) 'integer)))
     (typecase value
       (integer value)
       (string (parse-integer value))
       (t value)))
    ((eq type-spec 'boolean)
     (%coerce-boolean value parameter-name))
    ((and (consp type-spec) (eq (first type-spec) 'member))
     (%coerce-member-value value (rest type-spec)))
    (t value)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %parse-tool-declarations (forms)
    (let ((options (list :permission :supervised
                         :dangerous nil
                         :category :general
                         :timeout 30))
          (remaining forms))
      (loop while (and remaining
                       (consp (first remaining))
                       (keywordp (first (first remaining))))
            do (let ((declaration (first remaining)))
                 (destructuring-bind (keyword value &rest extra) declaration
                   (declare (ignore extra))
                   (unless (member keyword '(:permission :dangerous :category :timeout)
                                   :test #'eq)
                     (error "Unknown deftool declaration keyword: ~S" keyword))
                   (setf (getf options keyword)
                         (if (eq keyword :dangerous)
                             (not (null value))
                             value))))
               (setf remaining (rest remaining)))
      (values options remaining)))

  (defun %tool-exec-symbol (name)
    (intern (format nil "%EXEC-~A" (string-upcase (symbol-name name)))
            (find-package :amoebum.tools)))

  (defun %tool-schema-symbol (name)
    (intern (format nil "*TOOL-SCHEMA-~A*" (string-upcase (symbol-name name)))
            (find-package :amoebum.tools)))

  (defun %binding-symbol (prefix parameter)
    (make-symbol (format nil "~A-~A" prefix (string-upcase (symbol-name parameter))))))

(defmacro deftool (name parameter-specs &body forms)
  (unless (symbolp name)
    (error "DEFTTOOL name must be a symbol, got ~S." name))
  (let* ((docstring (and forms (stringp (first forms)) (first forms)))
         (tail (if docstring (rest forms) forms)))
    (multiple-value-bind (declarations body-forms)
        (%parse-tool-declarations tail)
      (when (null body-forms)
        (error "DEFTTOOL ~S requires a body." name))
      (let* ((normalized-parameters
               (mapcar #'%normalize-parameter-spec parameter-specs))
             (tool-name (%tool-name-string name))
             (permission (getf declarations :permission))
             (dangerous-p (getf declarations :dangerous))
             (category (getf declarations :category))
             (timeout (getf declarations :timeout))
             (exec-name (%tool-exec-symbol name))
             (schema-name (%tool-schema-symbol name))
             (source-file (or *compile-file-truename* *load-truename*))
             (source-line nil)
             (bindings '())
             (validation-forms '()))
        (dolist (parameter normalized-parameters)
          (let* ((parameter-name (getf parameter :name))
                 (parameter-type (getf parameter :type))
                 (required-p (getf parameter :required))
                 (default-supplied-p (getf parameter :default-supplied-p))
                 (default (getf parameter :default))
                 (argument-key (string-downcase (symbol-name parameter-name)))
                 (raw-symbol (%binding-symbol "%RAW" parameter-name))
                 (needs-check-symbol (%binding-symbol "%CHECK" parameter-name)))
            (push `(,raw-symbol (%extract-tool-argument arguments ,argument-key))
                  bindings)
            (push `(,parameter-name
                    (if (eq ,raw-symbol +missing-tool-argument+)
                        ,(if default-supplied-p default nil)
                        (%coerce-tool-argument ,raw-symbol ',parameter-type ',parameter-name)))
                  bindings)
            (push `(,needs-check-symbol
                    (or (not (eq ,raw-symbol +missing-tool-argument+))
                        ,(if default-supplied-p t nil)
                        ,(if required-p t nil)))
                  bindings)
            (when required-p
              (push `(when (eq ,raw-symbol +missing-tool-argument+)
                       (error "Missing required tool argument ~S for tool ~A."
                              ',parameter-name
                              ,tool-name))
                    validation-forms))
            (push `(when ,needs-check-symbol
                     (check-type ,parameter-name ,parameter-type))
                  validation-forms)))
        `(progn
           (defun ,exec-name (arguments &optional tool-call)
             (declare (ignore tool-call))
             (let* ,(nreverse bindings)
               ,@(nreverse validation-forms)
               ,(if timeout
                    `#+sbcl
                    (sb-ext:with-timeout ,timeout
                      (progn ,@body-forms))
                    `#+sbcl
                    (progn ,@body-forms))
               #-sbcl
               (progn ,@body-forms)))
           (defparameter ,schema-name
             (%tool-schema-from-parameter-specs ',normalized-parameters))
           (pseudopod:register-tool
            (%ensure-toolset)
            (pseudopod:make-tool-definition
             :name ,tool-name
             :description ,(or docstring "")
             :parameters ,schema-name
             :fn #',exec-name))
           (setf (gethash ,tool-name *tool-metadata*)
                 (make-tool-metadata
                  :name ,tool-name
                  :permission ',permission
                  :dangerous-p ,dangerous-p
                  :category ',category
                  :timeout-seconds ,timeout
                  :source-file ,source-file
                  :source-line ,source-line
                  :parameter-specs ',normalized-parameters
                  :defined-at (get-universal-time)))
           ',name)))))
