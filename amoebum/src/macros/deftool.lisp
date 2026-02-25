(in-package :amoebum)

(defparameter *toolset* (pseudopod:make-toolset))
(defparameter *tool-metadata* (make-hash-table :test #'equal))
(defparameter *tool-history* (make-hash-table :test #'equal))
(defparameter *tool-history-max-versions* 10)
(defparameter *deftool-compile-time-tool-names* (make-hash-table :test #'equal))

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
(defparameter +allowed-permission-modes+ '(:auto :supervised :full-auto))

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

(defstruct (tool-history-entry
            (:constructor make-tool-history-entry
                (&key tool-definition
                 tool-metadata
                 timestamp
                 source-file
                 source-line)))
  tool-definition
  tool-metadata
  timestamp
  source-file
  source-line)

(defun %ensure-toolset ()
  (unless (and (boundp '*toolset*)
               (pseudopod:toolset-p *toolset*))
    (setf *toolset* (pseudopod:make-toolset)))
  *toolset*)

(defun %tool-name-string (name)
  (let ((value (string-downcase
                (string-trim '(#\Space #\Tab #\Newline #\Return)
                             (cond
                               ((symbolp name) (symbol-name name))
                               ((stringp name) name)
                               (t (princ-to-string name)))))))
    (if (plusp (length value))
        value
        (error "Tool name must not be blank."))))

(defun %hash-table-keys (table)
  (loop for key being the hash-keys of table collect key))

(defun %history-limit ()
  (if (and (integerp *tool-history-max-versions*)
           (> *tool-history-max-versions* 0))
      *tool-history-max-versions*
      10))

(defun %trim-history-entries (entries)
  (let ((limit (%history-limit)))
    (if (> (length entries) limit)
        (subseq entries 0 limit)
        entries)))

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

(defun %deftool-type-spec-valid-p (type-spec)
  (handler-case
      (progn
        (typep nil type-spec)
        t)
    (error ()
      nil)))

(defun %validate-tool-parameter-specs (tool-name normalized-parameters)
  (dolist (parameter normalized-parameters)
    (let ((parameter-name (getf parameter :name))
          (parameter-type (getf parameter :type))
          (required-p (getf parameter :required))
          (description (getf parameter :description)))
      (unless (%deftool-type-spec-valid-p parameter-type)
        (error "DEFTTOOL ~S parameter ~S has invalid type spec ~S."
               tool-name
               parameter-name
               parameter-type))
      (when (and required-p
                 (or (null description)
                     (zerop (length (string-trim '(#\Space #\Tab #\Newline #\Return)
                                                 description)))))
        (warn 'missing-tool-description
              :tool-name tool-name
              :parameter parameter-name
              :reason "Required parameter is missing :description.")))))

(defun %validate-dangerous-permission-combination (tool-name permission dangerous-p)
  (when (and dangerous-p (eq permission :auto))
    (warn 'deftool-definition-warning
          :tool-name tool-name
          :parameter :permission
          :reason "Dangerous tool declared with :permission :auto; use :supervised (the default).")))

(defun reset-deftool-compile-validation-state ()
  (clrhash *deftool-compile-time-tool-names*)
  t)

(defun %record-deftool-name-for-validation (tool-name)
  (let ((normalized (%tool-name-string tool-name)))
    (if (gethash normalized *deftool-compile-time-tool-names*)
        (progn
          (warn 'duplicate-tool-name
                :tool-name normalized)
          t)
        (progn
          (setf (gethash normalized *deftool-compile-time-tool-names*) t)
          nil))))

(defun %blank-string-p (value)
  (or (null value)
      (zerop (length (string-trim '(#\Space #\Tab #\Newline #\Return)
                                  (princ-to-string value))))))

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

(defun %copy-tool-definition (tool-definition)
  (when tool-definition
    (let* ((parameters (pseudopod:tool-definition-parameters tool-definition))
           (copied-parameters
             (if (hash-table-p parameters)
                 (%copy-hash-table-shallow parameters)
                 parameters)))
      (pseudopod:make-tool-definition
       :name (pseudopod:tool-definition-name tool-definition)
       :description (pseudopod:tool-definition-description tool-definition)
       :parameters copied-parameters
       :fn (pseudopod:tool-definition-fn tool-definition)))))

(defun %copy-tool-metadata (metadata)
  (when (and metadata (tool-metadata-p metadata))
    (make-tool-metadata
     :name (tool-metadata-name metadata)
     :permission (tool-metadata-permission metadata)
     :dangerous-p (tool-metadata-dangerous-p metadata)
     :category (tool-metadata-category metadata)
     :timeout-seconds (tool-metadata-timeout-seconds metadata)
     :source-file (tool-metadata-source-file metadata)
     :source-line (tool-metadata-source-line metadata)
     :parameter-specs (copy-tree (tool-metadata-parameter-specs metadata))
     :defined-at (tool-metadata-defined-at metadata))))

(defun %tool-history-entries (tool-name)
  (copy-list (gethash (%tool-name-string tool-name) *tool-history*)))

(defun %push-tool-history-entry (tool-name entry)
  (let* ((key (%tool-name-string tool-name))
         (current (%tool-history-entries key))
         (updated (%trim-history-entries (cons entry current))))
    (setf (gethash key *tool-history*) updated)
    updated))

(defun %push-tool-version-to-history (tool-name tool-definition tool-metadata)
  (when tool-definition
    (%push-tool-history-entry
     tool-name
     (make-tool-history-entry
      :tool-definition (%copy-tool-definition tool-definition)
      :tool-metadata (%copy-tool-metadata tool-metadata)
      :timestamp (get-universal-time)
      :source-file (and (tool-metadata-p tool-metadata)
                        (tool-metadata-source-file tool-metadata))
      :source-line (and (tool-metadata-p tool-metadata)
                        (tool-metadata-source-line tool-metadata))))))

(defun %metadata-value-equal-p (left right)
  (cond
    ((and (pathnamep left) (pathnamep right))
     (string= (namestring left) (namestring right)))
    (t
     (equal left right))))

(defun %tool-metadata->plist (metadata)
  (and (tool-metadata-p metadata)
       (list :name (tool-metadata-name metadata)
             :permission (tool-metadata-permission metadata)
             :dangerous-p (tool-metadata-dangerous-p metadata)
             :category (tool-metadata-category metadata)
             :timeout-seconds (tool-metadata-timeout-seconds metadata)
             :source-file (tool-metadata-source-file metadata)
             :source-line (tool-metadata-source-line metadata)
             :parameter-specs (copy-tree (tool-metadata-parameter-specs metadata))
             :defined-at (tool-metadata-defined-at metadata))))

(defun %tool-metadata-diff (old-metadata new-metadata)
  (let ((diff '()))
    (labels ((push-diff (field old-value new-value)
               (unless (%metadata-value-equal-p old-value new-value)
                 (push (list :field field :old old-value :new new-value) diff))))
      (push-diff :permission
                 (and (tool-metadata-p old-metadata)
                      (tool-metadata-permission old-metadata))
                 (and (tool-metadata-p new-metadata)
                      (tool-metadata-permission new-metadata)))
      (push-diff :dangerous-p
                 (and (tool-metadata-p old-metadata)
                      (tool-metadata-dangerous-p old-metadata))
                 (and (tool-metadata-p new-metadata)
                      (tool-metadata-dangerous-p new-metadata)))
      (push-diff :category
                 (and (tool-metadata-p old-metadata)
                      (tool-metadata-category old-metadata))
                 (and (tool-metadata-p new-metadata)
                      (tool-metadata-category new-metadata)))
      (push-diff :timeout-seconds
                 (and (tool-metadata-p old-metadata)
                      (tool-metadata-timeout-seconds old-metadata))
                 (and (tool-metadata-p new-metadata)
                      (tool-metadata-timeout-seconds new-metadata)))
      (push-diff :source-file
                 (and (tool-metadata-p old-metadata)
                      (tool-metadata-source-file old-metadata))
                 (and (tool-metadata-p new-metadata)
                      (tool-metadata-source-file new-metadata)))
      (push-diff :source-line
                 (and (tool-metadata-p old-metadata)
                      (tool-metadata-source-line old-metadata))
                 (and (tool-metadata-p new-metadata)
                      (tool-metadata-source-line new-metadata)))
      (push-diff :parameter-specs
                 (and (tool-metadata-p old-metadata)
                      (tool-metadata-parameter-specs old-metadata))
                 (and (tool-metadata-p new-metadata)
                      (tool-metadata-parameter-specs new-metadata)))
      (push-diff :defined-at
                 (and (tool-metadata-p old-metadata)
                      (tool-metadata-defined-at old-metadata))
                 (and (tool-metadata-p new-metadata)
                      (tool-metadata-defined-at new-metadata))))
    (nreverse diff)))

(defun %emit-tool-redefined (tool-name old-metadata new-metadata)
  (publish (current-event-bus)
           (make-tool-redefined-event
            :tool-name (%tool-name-string tool-name)
            :old-metadata (%tool-metadata->plist old-metadata)
            :new-metadata (%tool-metadata->plist new-metadata)
            :metadata-diff (%tool-metadata-diff old-metadata new-metadata))))

(defun tool-history (tool-name)
  (let ((entries (%tool-history-entries tool-name)))
    (loop for entry in entries
          for version from 1
          collect (list :version version
                        :timestamp (tool-history-entry-timestamp entry)
                        :source-file (tool-history-entry-source-file entry)
                        :source-line (tool-history-entry-source-line entry)
                        :metadata (tool-history-entry-tool-metadata entry)))))

(defun rollback-tool (tool-name &key (version 1))
  (unless (and (integerp version) (plusp version))
    (error "VERSION must be a positive integer, got ~S." version))
  (let* ((normalized-name (%tool-name-string tool-name))
         (entries (%tool-history-entries normalized-name))
         (target-index (1- version)))
    (when (null entries)
      (error "No tool history exists for ~A." normalized-name))
    (when (>= target-index (length entries))
      (error "Requested version ~D for ~A, but only ~D version~:P available."
             version
             normalized-name
             (length entries)))
    (let* ((target-entry (nth target-index entries))
           (target-definition (tool-history-entry-tool-definition target-entry))
           (target-metadata (tool-history-entry-tool-metadata target-entry))
           (toolset (%ensure-toolset))
           (current-definition (pseudopod:find-tool toolset normalized-name))
           (current-metadata (gethash normalized-name *tool-metadata*))
           (remaining
             (loop for entry in entries
                   for index from 0
                   unless (= index target-index)
                     collect entry))
           (current-entry
             (and current-definition
                  (make-tool-history-entry
                   :tool-definition (%copy-tool-definition current-definition)
                   :tool-metadata (%copy-tool-metadata current-metadata)
                   :timestamp (get-universal-time)
                   :source-file (and (tool-metadata-p current-metadata)
                                     (tool-metadata-source-file current-metadata))
                   :source-line (and (tool-metadata-p current-metadata)
                                     (tool-metadata-source-line current-metadata)))))
           (updated-history
             (%trim-history-entries
              (if current-entry
                  (cons current-entry remaining)
                  remaining))))
      (unless target-definition
        (error "History entry ~D for ~A is missing a tool definition."
               version
               normalized-name))
      (setf (gethash normalized-name *tool-history*) updated-history)
      (pseudopod:register-tool toolset (%copy-tool-definition target-definition))
      (let ((restored-metadata (%copy-tool-metadata target-metadata)))
        (if restored-metadata
            (setf (gethash normalized-name *tool-metadata*) restored-metadata)
            (remhash normalized-name *tool-metadata*))
        (%emit-tool-redefined normalized-name current-metadata restored-metadata)
        restored-metadata))))

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
        (%record-deftool-name-for-validation tool-name)
        (unless (member permission +allowed-permission-modes+ :test #'eq)
          (error 'invalid-permission-mode
                 :tool-name tool-name
                 :permission permission
                 :allowed-values +allowed-permission-modes+))
        (when (and dangerous-p (eq permission :auto))
          (warn 'dangerous-auto-permission
                :tool-name tool-name
                :reason "Dangerous tools should not default to :permission :auto."))
        (when (%blank-string-p docstring)
          (warn 'missing-tool-description
                :tool-name tool-name
                :reason "DEFTTOOL should include a non-empty docstring description."))
        (%validate-tool-parameter-specs tool-name normalized-parameters)
        (%validate-dangerous-permission-combination tool-name permission dangerous-p)
        (dolist (parameter normalized-parameters)
          (let* ((parameter-name (getf parameter :name))
                 (parameter-type (getf parameter :type))
                 (required-p (getf parameter :required))
                 (default-supplied-p (getf parameter :default-supplied-p))
                 (default (getf parameter :default))
                 (argument-key (string-downcase (symbol-name parameter-name)))
                 (raw-symbol (%binding-symbol "%RAW" parameter-name))
                 (needs-check-symbol (%binding-symbol "%CHECK" parameter-name)))
            (%validate-type-to-schema-mapping tool-name parameter-name parameter-type)
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
           (let* ((toolset (%ensure-toolset))
                  (previous-definition (pseudopod:find-tool toolset ,tool-name))
                  (previous-metadata (gethash ,tool-name *tool-metadata*)))
             (%push-tool-version-to-history ,tool-name previous-definition previous-metadata)
             (pseudopod:register-tool
              toolset
              (pseudopod:make-tool-definition
               :name ,tool-name
               :description ,(or docstring "")
               :parameters ,schema-name
               :fn #',exec-name))
             (let ((new-metadata
                     (make-tool-metadata
                      :name ,tool-name
                      :permission ',permission
                      :dangerous-p ,dangerous-p
                      :category ',category
                      :timeout-seconds ,timeout
                      :source-file ,source-file
                      :source-line ,source-line
                      :parameter-specs ',normalized-parameters
                      :defined-at (get-universal-time))))
               (setf (gethash ,tool-name *tool-metadata*) new-metadata)
               (when previous-definition
                 (%emit-tool-redefined ,tool-name previous-metadata new-metadata))))
           ',name)))))
