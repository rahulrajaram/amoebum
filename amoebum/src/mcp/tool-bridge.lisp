(in-package :amoebum)

(defparameter *mcp-tool-server-registry* (make-hash-table :test #'equal))
(defparameter *mcp-tool-binding-registry* (make-hash-table :test #'equal))
(defparameter *mcp-tool-amoebum->mcp-registry* (make-hash-table :test #'equal))
(defparameter *mcp-tool-mcp->amoebum-registry* (make-hash-table :test #'equal))

(defstruct (mcp-tool-binding
            (:constructor %make-mcp-tool-binding
                (&key server-name
                   tool-name
                   namespaced-name
                   description
                   input-schema
                   server)))
  server-name
  tool-name
  namespaced-name
  description
  input-schema
  server)

(define-condition mcp-tool-bridge-error (tool-execution-error)
  ((server-name :initarg :server-name
                :initform nil
                :reader mcp-tool-bridge-error-server-name)
   (mcp-tool-name :initarg :mcp-tool-name
                  :initform nil
                  :reader mcp-tool-bridge-error-mcp-tool-name)
   (mcp-error-code :initarg :mcp-error-code
                   :initform nil
                   :reader mcp-tool-bridge-error-mcp-error-code)
   (mcp-response-error :initarg :mcp-response-error
                       :initform nil
                       :reader mcp-tool-bridge-error-mcp-response-error))
  (:report (lambda (condition stream)
             (format stream "MCP bridge error server=~A tool=~A code=~A: ~A"
                     (or (mcp-tool-bridge-error-server-name condition) "unknown")
                     (or (mcp-tool-bridge-error-mcp-tool-name condition) "unknown")
                     (or (mcp-tool-bridge-error-mcp-error-code condition) "none")
                     (or (tool-error-reason condition)
                         (amoebum-error-message condition)
                         "bridge failure")))))

(defun %mcp-json-function (name)
  (let* ((jonathan-package (find-package :jonathan))
         (symbol (and jonathan-package
                      (find-symbol name jonathan-package))))
    (unless (and symbol (fboundp symbol))
      (error "Jonathan function ~A is unavailable." name))
    (symbol-function symbol)))

(defun %mcp-json-encode (value)
  (funcall (%mcp-json-function "TO-JSON") value))

(defun %mcp-json-decode (payload)
  (funcall (%mcp-json-function "PARSE") payload :as :hash-table))

(defun %normalize-mcp-string (value label &key (downcase-p t))
  (when (null value)
    (error "~A must not be NIL." label))
  (let* ((raw (string-trim '(#\Space #\Tab #\Newline #\Return)
                           (princ-to-string value)))
         (normalized (if downcase-p
                         (string-downcase raw)
                         raw)))
    (unless (> (length normalized) 0)
      (error "~A must not be empty." label))
    normalized))

(defun %mcp-tools-normalize-server-name (server-name)
  (%normalize-mcp-string server-name "MCP server name"))

(defun %mcp-tools-normalize-tool-name-for-call (tool-name)
  (%normalize-mcp-string tool-name "MCP tool name" :downcase-p nil))

(defun %mcp-tools-normalize-tool-name-for-namespace (tool-name)
  (%normalize-mcp-string tool-name "MCP tool name"))

(defun %mcp-namespaced-tool-name (server-name tool-name)
  (format nil "mcp/~A/~A"
          (%mcp-tools-normalize-server-name server-name)
          (%mcp-tools-normalize-tool-name-for-namespace tool-name)))

(defun %mcp-default-input-schema ()
  (let ((schema (make-hash-table :test #'equal)))
    (setf (gethash "type" schema) "object"
          (gethash "properties" schema) (make-hash-table :test #'equal))
    schema))

(defun %mcp-copy-hash-table-shallow (table)
  (let ((copy (make-hash-table :test #'equal)))
    (maphash (lambda (key value)
               (setf (gethash key copy) value))
             table)
    copy))

(defun %normalize-mcp-input-schema (schema)
  (cond
    ((hash-table-p schema) (%mcp-copy-hash-table-shallow schema))
    ((null schema) (%mcp-default-input-schema))
    (t (%mcp-default-input-schema))))

(defun %normalize-mcp-argument-key (key)
  (cond
    ((stringp key) key)
    ((symbolp key) (string-downcase (symbol-name key)))
    (t (princ-to-string key))))

(defun %coerce-mcp-arguments-hash-table (arguments)
  (let ((table (make-hash-table :test #'equal)))
    (cond
      ((null arguments)
       table)
      ((hash-table-p arguments)
       (maphash (lambda (key value)
                  (setf (gethash (%normalize-mcp-argument-key key) table) value))
                arguments)
       table)
      ((and (listp arguments)
            (every #'consp arguments))
       (dolist (entry arguments table)
         (setf (gethash (%normalize-mcp-argument-key (car entry)) table)
               (cdr entry))))
      ((listp arguments)
       (unless (evenp (length arguments))
         (error "MCP tool argument plist must have even length, got ~S." arguments))
       (loop for (key value) on arguments by #'cddr do
             (setf (gethash (%normalize-mcp-argument-key key) table) value))
       table)
      ((stringp arguments)
       (let ((parsed (%mcp-json-decode arguments)))
         (unless (hash-table-p parsed)
           (error "MCP tool argument string must decode to a JSON object."))
         (%coerce-mcp-arguments-hash-table parsed)))
      (t
       (error "Unsupported MCP argument payload: ~S." arguments)))))

(defun %mcp-prepare-call-arguments (arguments)
  (%mcp-json-decode (%mcp-json-encode (%coerce-mcp-arguments-hash-table arguments))))

(defun %mcp-response-error-object (response)
  (when (hash-table-p response)
    (multiple-value-bind (error-object present-p)
        (gethash "error" response)
      (and present-p error-object))))

(defun %mcp-response-error-text (error-object)
  (cond
    ((hash-table-p error-object)
     (let ((message (gethash "message" error-object))
           (code (gethash "code" error-object)))
       (if message
           (format nil "~A (code ~A)" message code)
           (princ-to-string error-object))))
    (t (princ-to-string error-object))))

(defun %mcp-error-reason-code (code)
  (cond
    ((eql code -32601) :mcp-method-not-found)
    ((eql code -32602) :mcp-invalid-arguments)
    ((eql code -32001) :mcp-tool-not-found)
    (t :mcp-jsonrpc-error)))

(defun %mcp-error-condition-type (code)
  (cond
    ((eql code -32602) 'tool-argument-error)
    ((or (eql code -32601) (eql code -32001)) 'tool-not-found)
    (t 'mcp-tool-bridge-error)))

(defun %signal-mcp-response-error (binding arguments method response)
  (let* ((error-object (%mcp-response-error-object response))
         (code (and (hash-table-p error-object) (gethash "code" error-object)))
         (message (if error-object
                      (%mcp-response-error-text error-object)
                      (format nil "MCP method ~A failed." method)))
         (condition-type (%mcp-error-condition-type code)))
    (if (eq condition-type 'mcp-tool-bridge-error)
        (error condition-type
               :tool-name (mcp-tool-binding-namespaced-name binding)
               :arguments arguments
               :message message
               :reason message
               :reason-code (%mcp-error-reason-code code)
               :cause error-object
               :server-name (mcp-tool-binding-server-name binding)
               :mcp-tool-name (mcp-tool-binding-tool-name binding)
               :mcp-error-code code
               :mcp-response-error error-object)
        (error condition-type
               :tool-name (mcp-tool-binding-namespaced-name binding)
               :arguments arguments
               :message message
               :reason message
               :reason-code (%mcp-error-reason-code code)
               :cause error-object))))

(defun %mcp-response-result-or-error (binding arguments response method)
  (let ((error-object (%mcp-response-error-object response)))
    (when error-object
      (%signal-mcp-response-error binding arguments method response)))
  (multiple-value-bind (result present-p)
      (gethash "result" response)
    (if present-p
        result
        (error 'mcp-tool-bridge-error
               :tool-name (mcp-tool-binding-namespaced-name binding)
               :arguments arguments
               :server-name (mcp-tool-binding-server-name binding)
               :mcp-tool-name (mcp-tool-binding-tool-name binding)
               :message (format nil "MCP method ~A response missing result field." method)
               :reason (format nil "MCP method ~A response missing result field." method)
               :reason-code :mcp-missing-result))))

(defun %mcp-server-from-designator (server-designator)
  (cond
    ((mcp-server-p server-designator) server-designator)
    (t
     (or (gethash (%mcp-tools-normalize-server-name server-designator)
                  *mcp-tool-server-registry*)
         (error "Unknown MCP server designator ~S." server-designator)))))

(defun %mcp-server-client-or-error (server)
  (unless (mcp-server-p server)
    (error "SERVER must be an MCP-SERVER, got ~S." server))
  (unless (mcp-server-running-p server)
    (error "MCP server ~A is not running." (mcp-server-name server)))
  (or (mcp-server-jsonrpc-client server)
      (error "MCP server ~A has no active JSON-RPC client." (mcp-server-name server))))

(defun %mcp-tools-list-request (server cursor)
  (let* ((client (%mcp-server-client-or-error server))
         (params (when cursor
                   (let ((payload (make-hash-table :test #'equal)))
                     (setf (gethash "cursor" payload) cursor)
                     payload)))
         (response (if params
                       (mcp-jsonrpc-send-request client "tools/list" :params params)
                       (mcp-jsonrpc-send-request client "tools/list")))
         (result (%mcp-response-result-or-error
                  (%make-mcp-tool-binding
                   :server-name (mcp-server-name server)
                   :tool-name "tools/list"
                   :namespaced-name "mcp/internal/tools-list"
                   :server server)
                  params
                  response
                  "tools/list"))
         (tools (or (gethash "tools" result) '()))
         (next-cursor (or (gethash "nextCursor" result)
                          (gethash "next_cursor" result))))
    (values (typecase tools
              (list tools)
              (vector (loop for item across tools collect item))
              (t '()))
            (and next-cursor
                 (not (string= (string-trim '(#\Space #\Tab #\Newline #\Return)
                                            (princ-to-string next-cursor))
                               ""))
                 next-cursor))))

(defun %mcp-resolve-tool-binding (tool)
  (cond
    ((mcp-tool-binding-p tool) tool)
    (t
     (or (gethash (%normalize-mcp-string tool "MCP tool reference")
                  *mcp-tool-binding-registry*)
         (error 'tool-not-found
                :tool-name (%normalize-mcp-string tool "MCP tool reference")
                :reason "Unknown MCP tool binding.")))))

(defun %mcp-tool-call-request-id (tool-call)
  (when tool-call
    (let ((id (ignore-errors (pseudopod:tool-call-id tool-call))))
      (when id
        (princ-to-string id)))))

(defun %make-mcp-tool-result (binding arguments result request-id)
  (let ((payload (make-hash-table :test #'equal)))
    (setf (gethash "type" payload) "tool-result"
          (gethash "name" payload) (mcp-tool-binding-namespaced-name binding)
          (gethash "server" payload) (mcp-tool-binding-server-name binding)
          (gethash "tool" payload) (mcp-tool-binding-tool-name binding)
          (gethash "arguments" payload) arguments
          (gethash "result" payload) result)
    (when request-id
      (setf (gethash "requestId" payload) request-id))
    payload))

(defun %mcp-mapping-key (server-name mcp-tool-name)
  (format nil "~A::~A"
          (%mcp-tools-normalize-server-name server-name)
          (%mcp-tools-normalize-tool-name-for-call mcp-tool-name)))

(defun %normalize-schema-type-value (value)
  (string-downcase
   (string-trim '(#\Space #\Tab #\Newline #\Return)
                (princ-to-string value))))

(defun %schema-type-list (type-field)
  (cond
    ((null type-field) '())
    ((listp type-field) (mapcar #'%normalize-schema-type-value type-field))
    (t (list (%normalize-schema-type-value type-field)))))

(defun %mcp-json-schema-type->deftool-type (property-schema)
  (let* ((types (%schema-type-list (and (hash-table-p property-schema)
                                        (gethash "type" property-schema))))
         (nullable-p (member "null" types :test #'string=))
         (base-type
           (cond
             ((and (hash-table-p property-schema)
                   (gethash "enum" property-schema))
              `(member ,@(typecase (gethash "enum" property-schema)
                           (list (gethash "enum" property-schema))
                           (vector (loop for item across (gethash "enum" property-schema)
                                         collect item))
                           (t '()))))
             ((member "integer" types :test #'string=) 'integer)
             ((member "boolean" types :test #'string=) 'boolean)
             ((member "array" types :test #'string=) 'list)
             ((member "null" types :test #'string=) 'null)
             (t 'string))))
    (if (and nullable-p (not (equal base-type 'null)))
        `(or null ,base-type)
        base-type)))

(defun %safe-symbol-name (value &key (fallback "MCP-ARG"))
  (let* ((raw (string-upcase (princ-to-string value)))
         (chars (loop for ch across raw
                      collect (if (or (alphanumericp ch) (char= ch #\-))
                                  ch
                                  #\-)))
         (normalized (string-trim '(#\-) (coerce chars 'string))))
    (if (plusp (length normalized))
        normalized
        fallback)))

(defun %mcp-parameter-symbol (name)
  (intern (%safe-symbol-name name :fallback "MCP-ARG")
          (find-package :amoebum.tools)))

(defun %mcp-schema-required-table (schema)
  (let ((required-table (make-hash-table :test #'equal)))
    (let ((required (and (hash-table-p schema) (gethash "required" schema))))
      (typecase required
        (list
         (dolist (entry required)
           (setf (gethash (string-downcase (princ-to-string entry))
                          required-table)
                 t)))
        (vector
         (loop for entry across required do
               (setf (gethash (string-downcase (princ-to-string entry))
                              required-table)
                     t)))))
    required-table))

(defun %mcp-schema->deftool-parameter-specs (schema)
  (let* ((properties (and (hash-table-p schema) (gethash "properties" schema)))
         (required-table (%mcp-schema-required-table schema))
         (parameter-specs '()))
    (when (hash-table-p properties)
      (let ((keys (loop for key being the hash-keys of properties collect key)))
        (dolist (key (sort keys #'string< :key #'string-downcase))
          (let* ((property-schema (gethash key properties))
                 (parameter (%mcp-parameter-symbol key))
                 (type-spec (%mcp-json-schema-type->deftool-type property-schema))
                 (description (and (hash-table-p property-schema)
                                   (gethash "description" property-schema)))
                 (required-p (gethash (string-downcase (princ-to-string key))
                                      required-table)))
            (push (append (list parameter type-spec)
                          (when description (list :description (princ-to-string description)))
                          (when required-p (list :required t)))
                  parameter-specs)))))
    (nreverse parameter-specs)))

(defun %mcp-wrapper-tool-symbol (namespaced-name)
  (intern (string-upcase namespaced-name)
          (find-package :amoebum.tools)))

(defun mcp-tool-name-for-amoebum-tool (amoebum-tool-name)
  (gethash (%normalize-mcp-string amoebum-tool-name "Amoebum tool name" :downcase-p t)
           *mcp-tool-amoebum->mcp-registry*))

(defun amoebum-tool-name-for-mcp-tool (server-name mcp-tool-name)
  (gethash (%mcp-mapping-key server-name mcp-tool-name)
           *mcp-tool-mcp->amoebum-registry*))

(defun clear-mcp-tool-registries ()
  (clrhash *mcp-tool-server-registry*)
  (clrhash *mcp-tool-binding-registry*)
  (clrhash *mcp-tool-amoebum->mcp-registry*)
  (clrhash *mcp-tool-mcp->amoebum-registry*)
  t)

(defun register-mcp-tool-server (server &key name (discover-tools-p nil)
                                          (toolset *toolset*)
                                          event-bus)
  (unless (mcp-server-p server)
    (error "SERVER must be an MCP-SERVER, got ~S." server))
  (let ((server-name (%mcp-tools-normalize-server-name (or name (mcp-server-name server)))))
    (setf (gethash server-name *mcp-tool-server-registry*) server)
    (if discover-tools-p
        (discover-mcp-server-tools server-name :toolset toolset :event-bus event-bus)
        server)))

(defun unregister-mcp-tool-server (server-name)
  (let ((normalized (%mcp-tools-normalize-server-name server-name))
        (stale-tools '())
        (removed-bindings 0))
    (remhash normalized *mcp-tool-server-registry*)
    (maphash (lambda (namespaced-name binding)
               (when (string= (mcp-tool-binding-server-name binding) normalized)
                 (push namespaced-name stale-tools)
                 (incf removed-bindings)))
             *mcp-tool-binding-registry*)
    (dolist (namespaced-name stale-tools)
      (let ((binding (gethash namespaced-name *mcp-tool-binding-registry*)))
        (remhash namespaced-name *mcp-tool-binding-registry*)
        (remhash namespaced-name *mcp-tool-amoebum->mcp-registry*)
        (when binding
          (remhash (%mcp-mapping-key (mcp-tool-binding-server-name binding)
                                     (mcp-tool-binding-tool-name binding))
                   *mcp-tool-mcp->amoebum-registry*))))
    removed-bindings))

(defun find-mcp-tool-server (server-name)
  (gethash (%mcp-tools-normalize-server-name server-name)
           *mcp-tool-server-registry*))

(defun invoke-mcp-tool-bridge (tool arguments &key tool-call event-bus)
  (let* ((binding (%mcp-resolve-tool-binding tool))
         (server (mcp-tool-binding-server binding))
         (request-id (%mcp-tool-call-request-id tool-call))
         (prepared-arguments (%mcp-prepare-call-arguments arguments))
         (bus (or event-bus (current-event-bus))))
    (handler-case
        (progn
          (publish bus
                   (make-mcp-tool-invoked-event
                    :server-name (mcp-tool-binding-server-name binding)
                    :tool-name (mcp-tool-binding-tool-name binding)
                    :namespaced-name (mcp-tool-binding-namespaced-name binding)
                    :args prepared-arguments
                    :request-id request-id))
          (let* ((params (make-hash-table :test #'equal))
                 (client (%mcp-server-client-or-error server)))
            (setf (gethash "name" params) (mcp-tool-binding-tool-name binding)
                  (gethash "arguments" params) prepared-arguments)
            (%make-mcp-tool-result
             binding
             prepared-arguments
             (%mcp-response-result-or-error
              binding
              prepared-arguments
              (mcp-jsonrpc-send-request client "tools/call" :params params)
              "tools/call")
             request-id)))
      (mcp-timeout (condition)
        (error 'tool-timeout
               :tool-name (mcp-tool-binding-namespaced-name binding)
               :arguments prepared-arguments
               :message (princ-to-string condition)
               :reason (princ-to-string condition)
               :reason-code :mcp-timeout
               :cause condition
               :timeout-seconds (mcp-timeout-timeout-seconds condition)))
      (tool-error (condition)
        (error condition))
      (error (condition)
        (error 'mcp-tool-bridge-error
               :tool-name (mcp-tool-binding-namespaced-name binding)
               :arguments prepared-arguments
               :message (princ-to-string condition)
               :reason (princ-to-string condition)
               :reason-code :mcp-bridge-failure
               :cause condition
               :server-name (mcp-tool-binding-server-name binding)
               :mcp-tool-name (mcp-tool-binding-tool-name binding))))))

(defun invoke-mcp-tool (tool arguments &key tool-call event-bus)
  (invoke-mcp-tool-bridge tool arguments :tool-call tool-call :event-bus event-bus))

(defun %register-mcp-wrapper-via-deftool (binding toolset)
  (let* ((tool-name (mcp-tool-binding-namespaced-name binding))
         (tool-symbol (%mcp-wrapper-tool-symbol tool-name))
         (schema (%normalize-mcp-input-schema (mcp-tool-binding-input-schema binding)))
         (parameter-specs (%mcp-schema->deftool-parameter-specs schema))
         (description (or (mcp-tool-binding-description binding) "MCP tool bridge wrapper."))
         (server-name (mcp-tool-binding-server-name binding)))
    (let ((*toolset* toolset))
      (eval `(deftool ,tool-symbol ,parameter-specs
               ,description
               (:permission :supervised)
               (:dangerous nil)
               (:category :mcp)
               (:timeout ,*mcp-jsonrpc-default-timeout-seconds*)
               (:mcp-server ,server-name)
               (invoke-mcp-tool-bridge ,tool-name arguments :tool-call tool-call))))))

(defun %register-mcp-tool-definition (server tool-entry toolset event-bus)
  (declare (ignore toolset))
  (unless (hash-table-p tool-entry)
    (error "MCP tools/list entry must be a hash-table, got ~S." tool-entry))
  (let* ((server-name (%mcp-tools-normalize-server-name (mcp-server-name server)))
         (tool-name (%mcp-tools-normalize-tool-name-for-call (gethash "name" tool-entry)))
         (namespaced-name (%mcp-namespaced-tool-name server-name tool-name))
         (description (or (gethash "description" tool-entry) ""))
         (input-schema (%normalize-mcp-input-schema
                        (or (gethash "inputSchema" tool-entry)
                            (gethash "input_schema" tool-entry)
                            (gethash "parameters" tool-entry))))
         (binding (%make-mcp-tool-binding
                   :server-name server-name
                   :tool-name tool-name
                   :namespaced-name namespaced-name
                   :description description
                   :input-schema input-schema
                   :server server)))
    (setf (gethash namespaced-name *mcp-tool-binding-registry*) binding)
    (setf (gethash namespaced-name *mcp-tool-amoebum->mcp-registry*)
          (list :server-name server-name :mcp-tool-name tool-name))
    (setf (gethash (%mcp-mapping-key server-name tool-name)
                   *mcp-tool-mcp->amoebum-registry*)
          namespaced-name)
    (%register-mcp-wrapper-via-deftool binding toolset)
    (publish (or event-bus (current-event-bus))
             (make-mcp-tool-discovered-event
              :server-name server-name
              :tool-name tool-name
              :namespaced-name namespaced-name
              :description description))
    namespaced-name))

(defun discover-mcp-server-tools (server-designator &key
                                                    (toolset *toolset*)
                                                    event-bus)
  (let* ((server (%mcp-server-from-designator server-designator))
         (cursor nil)
         (seen-cursors (make-hash-table :test #'equal))
         (namespaced-tools '()))
    (loop
      (multiple-value-bind (tools next-cursor)
          (%mcp-tools-list-request server cursor)
        (dolist (tool-entry tools)
          (push (%register-mcp-tool-definition server
                                               tool-entry
                                               toolset
                                               event-bus)
                namespaced-tools))
        (if (and next-cursor
                 (not (gethash next-cursor seen-cursors)))
            (progn
              (setf (gethash next-cursor seen-cursors) t
                    cursor next-cursor))
            (return))))
    (nreverse namespaced-tools)))

(defun auto-register-mcp-server-tools (server &key
                                                 (toolset *toolset*)
                                                 event-bus)
  (handler-case
      (progn
        (register-mcp-tool-server server
                                  :name (mcp-server-name server)
                                  :discover-tools-p t
                                  :toolset toolset
                                  :event-bus event-bus)
        t)
    (error (condition)
      (warn "MCP auto-registration failed for ~A: ~A"
            (mcp-server-name server)
            condition)
      nil)))
