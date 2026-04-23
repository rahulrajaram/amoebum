(in-package :amoebum)

;;;; NXT-395: deftool schema helpers.
;;;;
;;;; This module owns the CL type -> JSON schema mapping, round-trip warnings,
;;;; and schema assembly used by `deftool` registrations.

(defparameter *cl-type-schema-type-table*
  (let ((table (make-hash-table :test #'eq)))
    (setf (gethash 'string table) "string")
    (setf (gethash 'integer table) "integer")
    (setf (gethash 'boolean table) "boolean")
    (setf (gethash 'pathname table) "string")
    (setf (gethash 'list table) "array")
    (setf (gethash 'null table) "null")
    table))

(defun %json-enum-value (value)
  (typecase value
    (string value)
    (symbol (string-downcase (symbol-name value)))
    (number value)
    (t (princ-to-string value))))

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

(defun %normalized-schema-types (schema-type-field)
  (remove-duplicates
   (loop for raw in (cond
                      ((null schema-type-field) '())
                      ((listp schema-type-field) schema-type-field)
                      (t (list schema-type-field)))
         for normalized = (string-downcase (princ-to-string raw))
         when (plusp (length normalized))
           collect normalized)
   :test #'string=))

(defun %known-json-schema-type-p (type-name)
  (member type-name
          '("string" "integer" "number" "boolean" "array" "object" "null")
          :test #'string=))

(defun %expected-schema-types-for-type-spec (type-spec)
  (cond
    ((and (consp type-spec) (eq (first type-spec) 'member))
     (list "string"))
    ((and (consp type-spec) (eq (first type-spec) 'or))
     (let ((collected '()))
       (dolist (candidate (rest type-spec))
         (let ((mapped (%expected-schema-types-for-type-spec candidate)))
           (unless mapped
             (return-from %expected-schema-types-for-type-spec nil))
           (setf collected (nconc collected (copy-list mapped)))))
       (remove-duplicates collected :test #'string=)))
    ((and (consp type-spec) (eq (first type-spec) 'integer))
     (list "integer"))
    ((gethash type-spec *cl-type-schema-type-table*)
     (list (gethash type-spec *cl-type-schema-type-table*)))
    (t
     nil)))

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

(defun %validate-type-to-schema-mapping (tool-name parameter-name type-spec)
  (let* ((expected-types (%expected-schema-types-for-type-spec type-spec))
         (schema (cl-type-to-json-schema type-spec))
         (actual-types (%normalized-schema-types (gethash "type" schema)))
         (known-types-p (every #'%known-json-schema-type-p actual-types))
         (round-trip-p (and expected-types
                            known-types-p
                            (null (set-exclusive-or expected-types
                                                    actual-types
                                                    :test #'string=)))))
    (unless round-trip-p
      (warn 'unmapped-type-warning
            :tool-name tool-name
            :parameter parameter-name
            :type-spec type-spec
            :reason "Type does not round-trip cleanly through JSON schema mapping."))
    round-trip-p))

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
