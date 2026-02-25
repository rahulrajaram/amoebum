(in-package :amoebum)

(defparameter *mcp-tool-server-registry* (make-hash-table :test #'equal))
(defparameter *mcp-tool-binding-registry* (make-hash-table :test #'equal))
(defparameter *mcp-tools-list-request-function* nil)

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

(defun %mcp-response-error-text (error-object)
  (cond
    ((hash-table-p error-object)
     (let ((message (gethash "message" error-object))
           (code (gethash "code" error-object)))
       (if message
           (format nil "~A (code ~A)" message code)
           (princ-to-string error-object))))
    (t (princ-to-string error-object))))

(defun %mcp-response-result-or-error (response method)
  (multiple-value-bind (error-object error-present-p)
      (gethash "error" response)
    (when error-present-p
      (error "MCP method ~A failed: ~A"
             method
             (%mcp-response-error-text error-object))))
  (multiple-value-bind (result present-p)
      (gethash "result" response)
    (if present-p
        result
        (error "MCP method ~A response missing result field." method))))

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
  (when *mcp-tools-list-request-function*
    (return-from %mcp-tools-list-request
      (funcall *mcp-tools-list-request-function* server cursor)))
  (let* ((client (%mcp-server-client-or-error server))
         (params (when cursor
                   (let ((payload (make-hash-table :test #'equal)))
                     (setf (gethash "cursor" payload) cursor)
                     payload)))
         (response (if params
                       (mcp-jsonrpc-send-request client "tools/list" :params params)
                       (mcp-jsonrpc-send-request client "tools/list")))
         (result (%mcp-response-result-or-error response "tools/list"))
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
         (error "Unknown MCP tool ~S." tool)))))

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

(defun clear-mcp-tool-registries ()
  (clrhash *mcp-tool-server-registry*)
  (clrhash *mcp-tool-binding-registry*)
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
      (remhash namespaced-name *mcp-tool-binding-registry*))
    removed-bindings))

(defun find-mcp-tool-server (server-name)
  (gethash (%mcp-tools-normalize-server-name server-name)
           *mcp-tool-server-registry*))

(defun invoke-mcp-tool (tool arguments &key tool-call event-bus)
  (let* ((binding (%mcp-resolve-tool-binding tool))
         (server (mcp-tool-binding-server binding))
         (request-id (%mcp-tool-call-request-id tool-call))
         (prepared-arguments (%mcp-prepare-call-arguments arguments))
         (bus (or event-bus (current-event-bus))))
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
        (mcp-jsonrpc-send-request client "tools/call" :params params)
        "tools/call")
       request-id))))

(defun %register-mcp-tool-definition (server tool-entry toolset event-bus)
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
    (pseudopod:register-tool
     toolset
     (pseudopod:make-tool-definition
      :name namespaced-name
      :description description
      :parameters input-schema
      :fn (lambda (arguments &optional tool-call)
            (invoke-mcp-tool binding arguments
                             :tool-call tool-call
                             :event-bus event-bus))))
    (setf (gethash namespaced-name *tool-metadata*)
          (make-tool-metadata
           :name namespaced-name
           :permission :supervised
           :dangerous-p nil
           :category :mcp
           :timeout-seconds *mcp-jsonrpc-default-timeout-seconds*
           :source-file "mcp/tools.lisp"
           :source-line nil
           :parameter-specs nil
           :defined-at (get-universal-time)))
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
          (let ((tool-name (and (hash-table-p tool-entry)
                                (gethash "name" tool-entry))))
            (when (mcp-server-tool-declared-p server (or tool-name ""))
              (push (%register-mcp-tool-definition server
                                                   tool-entry
                                                   toolset
                                                   event-bus)
                    namespaced-tools))))
        (if (and next-cursor
                 (not (gethash next-cursor seen-cursors)))
            (progn
              (setf (gethash next-cursor seen-cursors) t
                    cursor next-cursor))
            (return))))
    (mcp-update-server-discovered-tool-count server (length namespaced-tools))
    (nreverse namespaced-tools)))
