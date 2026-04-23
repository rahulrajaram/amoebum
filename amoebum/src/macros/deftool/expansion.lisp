(in-package :amoebum)

;;;; NXT-395: deftool macroexpansion helpers and macro entrypoint.
;;;;
;;;; This module now owns the compile-time declaration parser, binding/schema
;;;; builder, and the `deftool` macro itself. Golden coverage in
;;;; `amoebum/test/snapshots/macroexpand/deftool*.sexp` defends the generated
;;;; expansion byte-for-byte.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %parse-tool-declarations (forms)
    (let ((options (list :permission :supervised
                         :dangerous nil
                         :category :general
                         :timeout 30
                         :mcp-server nil))
          (remaining forms))
      (loop while (and remaining
                       (consp (first remaining))
                       (keywordp (first (first remaining))))
            do (let ((declaration (first remaining)))
                 (destructuring-bind (keyword value &rest extra) declaration
                   (declare (ignore extra))
                   (unless (member keyword '(:permission :dangerous :category :timeout
                                             :mcp-server)
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
    (make-symbol (format nil "~A-~A" prefix (string-upcase (symbol-name parameter)))))

  (defun %normalize-tool-mcp-server (name declarations)
    (let ((raw (getf declarations :mcp-server)))
      (and raw
           (let ((text (string-trim '(#\Space #\Tab #\Newline #\Return)
                                    (princ-to-string raw))))
             (unless (plusp (length text))
               (error "DEFTTOOL ~S :MCP-SERVER must not be blank." name))
             (string-downcase text)))))

  (defun %validate-deftool-expansion-options (tool-name permission dangerous-p docstring
                                              normalized-parameters)
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
    (%validate-dangerous-permission-combination tool-name permission dangerous-p))

  (defun %deftool-parameter-forms (parameter tool-name)
    (let* ((parameter-name (getf parameter :name))
           (parameter-type (getf parameter :type))
           (required-p (getf parameter :required))
           (default-supplied-p (getf parameter :default-supplied-p))
           (default (getf parameter :default))
           (argument-key (string-downcase (symbol-name parameter-name)))
           (raw-symbol (%binding-symbol "%RAW" parameter-name))
           (needs-check-symbol (%binding-symbol "%CHECK" parameter-name))
           (bindings
             (list `(,raw-symbol (%extract-tool-argument arguments ,argument-key))
                   `(,parameter-name
                     (if (eq ,raw-symbol +missing-tool-argument+)
                         ,(if default-supplied-p default nil)
                         (%coerce-tool-argument ,raw-symbol ',parameter-type ',parameter-name)))
                   `(,needs-check-symbol
                     (or (not (eq ,raw-symbol +missing-tool-argument+))
                         ,(if default-supplied-p t nil)
                         ,(if required-p t nil)))))
           (validation-forms
             (append
              (when required-p
                (list
                 `(when (eq ,raw-symbol +missing-tool-argument+)
                    (error "Missing required tool argument ~S for tool ~A."
                           ',parameter-name
                           ,tool-name))))
              (list
               `(when ,needs-check-symbol
                  (check-type ,parameter-name ,parameter-type))))))
      (%validate-type-to-schema-mapping tool-name parameter-name parameter-type)
      (values bindings validation-forms)))

  (defun %deftool-runtime-body (timeout body-forms)
    (if timeout
        `#+sbcl
        (sb-ext:with-timeout ,timeout
          (progn ,@body-forms))
        `#+sbcl
        (progn ,@body-forms)))

  (defun %build-deftool-expansion (name tool-name docstring normalized-parameters permission
                                   dangerous-p category timeout mcp-server exec-name
                                   schema-name source-file source-line bindings
                                   validation-forms body-forms)
    `(progn
       (defun ,exec-name (arguments &optional tool-call)
         (declare (ignorable tool-call))
         (let* ,bindings
           ,@validation-forms
           ,(%deftool-runtime-body timeout body-forms)
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
                  :defined-at (get-universal-time)
                  :mcp-server ,mcp-server)))
           (setf (gethash ,tool-name *tool-metadata*) new-metadata)
           (when previous-definition
             (%emit-tool-redefined ,tool-name previous-metadata new-metadata))))
       ',name)))

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
             (mcp-server (%normalize-tool-mcp-server name declarations))
             (exec-name (%tool-exec-symbol name))
             (schema-name (%tool-schema-symbol name))
             (source-file (or *compile-file-truename* *load-truename*))
             (source-line nil)
             (bindings '())
             (validation-forms '()))
        (%validate-deftool-expansion-options tool-name permission dangerous-p docstring
                                             normalized-parameters)
        (dolist (parameter normalized-parameters)
          (multiple-value-bind (parameter-bindings parameter-validations)
              (%deftool-parameter-forms parameter tool-name)
            (setf bindings (append bindings parameter-bindings)
                  validation-forms (append validation-forms parameter-validations))))
        (%build-deftool-expansion name tool-name docstring normalized-parameters permission
                                  dangerous-p category timeout mcp-server exec-name
                                  schema-name source-file source-line bindings
                                  validation-forms body-forms)))))
