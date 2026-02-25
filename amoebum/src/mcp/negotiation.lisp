(in-package :amoebum)

(defparameter *mcp-protocol-version* "2024-11-05")
(defparameter *mcp-negotiation-request-function* #'mcp-jsonrpc-send-request)

(defstruct (mcp-server-info
            (:constructor make-mcp-server-info
                (&key protocol-version
                   protocol-version-match-p
                   capabilities
                   tools-capability
                   resources-capability
                   prompts-capability
                   logging-capability
                   declared-tools
                   discovered-tool-count
                   negotiated-at-unix-time)))
  protocol-version
  (protocol-version-match-p t :type boolean)
  capabilities
  tools-capability
  resources-capability
  prompts-capability
  logging-capability
  (declared-tools nil :type list)
  (discovered-tool-count 0 :type integer)
  negotiated-at-unix-time)

(defun %mcp-copy-hash-table-shallow (table)
  (let ((copy (make-hash-table :test #'equal)))
    (when (hash-table-p table)
      (maphash (lambda (key value)
                 (setf (gethash key copy) value))
               table))
    copy))

(defun %mcp-negotiation-table-value (table key)
  (when (hash-table-p table)
    (or (gethash key table)
        (gethash (string-downcase key) table)
        (gethash (string-upcase key) table))))

(defun %mcp-normalize-tool-name (name)
  (let ((raw (if (stringp name) name (princ-to-string name))))
    (string-downcase (string-trim '(#\Space #\Tab #\Newline #\Return) raw))))

(defun %mcp-extract-tool-name (entry)
  (cond
    ((null entry) nil)
    ((stringp entry) (%mcp-normalize-tool-name entry))
    ((symbolp entry) (%mcp-normalize-tool-name entry))
    ((hash-table-p entry)
     (let ((name (%mcp-negotiation-table-value entry "name")))
       (when name
         (%mcp-normalize-tool-name name))))
    (t nil)))

(defun %mcp-normalize-declared-tools (value)
  (let ((raw-tools
          (cond
            ((null value) nil)
            ((vectorp value) (loop for item across value collect item))
            ((listp value) value)
            ((hash-table-p value)
             (or (%mcp-negotiation-table-value value "tools")
                 (%mcp-negotiation-table-value value "list")
                 (%mcp-negotiation-table-value value "names")
                 (%mcp-negotiation-table-value value "allowedTools")
                 (%mcp-negotiation-table-value value "toolNames")))
            (t nil))))
    (let ((names '()))
      (dolist (entry (cond
                       ((vectorp raw-tools) (loop for item across raw-tools collect item))
                       ((listp raw-tools) raw-tools)
                       (t nil)))
        (let ((name (%mcp-extract-tool-name entry)))
          (when (and name (> (length name) 0))
            (pushnew name names :test #'string=))))
      (nreverse names))))

(defun mcp-build-initialize-params ()
  (let ((params (make-hash-table :test #'equal))
        (capabilities (make-hash-table :test #'equal))
        (client-info (make-hash-table :test #'equal)))
    (setf (gethash "tools" capabilities) (make-hash-table :test #'equal)
          (gethash "resources" capabilities) (make-hash-table :test #'equal)
          (gethash "prompts" capabilities) (make-hash-table :test #'equal)
          (gethash "logging" capabilities) (make-hash-table :test #'equal)
          (gethash "protocolVersion" params) *mcp-protocol-version*
          (gethash "capabilities" params) capabilities
          (gethash "name" client-info) "amoebum"
          (gethash "version" client-info) "0.1.0"
          (gethash "clientInfo" params) client-info)
    params))

(defun %mcp-parse-server-info (server result)
  (let* ((protocol-version (or (%mcp-negotiation-table-value result "protocolVersion")
                               "unknown"))
         (capabilities-raw (or (%mcp-negotiation-table-value result "capabilities")
                               (make-hash-table :test #'equal)))
         (capabilities (if (hash-table-p capabilities-raw)
                           (%mcp-copy-hash-table-shallow capabilities-raw)
                           (make-hash-table :test #'equal)))
         (tools-capability (%mcp-negotiation-table-value capabilities "tools"))
         (resources-capability (%mcp-negotiation-table-value capabilities "resources"))
         (prompts-capability (%mcp-negotiation-table-value capabilities "prompts"))
         (logging-capability (%mcp-negotiation-table-value capabilities "logging"))
         (declared-tools (%mcp-normalize-declared-tools tools-capability))
         (version-match-p (string= (princ-to-string protocol-version)
                                   *mcp-protocol-version*)))
    (unless version-match-p
      (warn "MCP protocol version mismatch for server ~A: client ~A, server ~A."
            (mcp-server-name server)
            *mcp-protocol-version*
            protocol-version))
    (make-mcp-server-info
     :protocol-version protocol-version
     :protocol-version-match-p version-match-p
     :capabilities capabilities
     :tools-capability tools-capability
     :resources-capability resources-capability
     :prompts-capability prompts-capability
     :logging-capability logging-capability
     :declared-tools declared-tools
     :negotiated-at-unix-time (get-universal-time))))

(defun mcp-negotiate-server-capabilities (server client &key timeout-seconds)
  (unless (mcp-server-p server)
    (error "SERVER must be an MCP-SERVER, got ~S." server))
  (unless (mcp-jsonrpc-client-p client)
    (error "CLIENT must be an MCP-JSONRPC-CLIENT, got ~S." client))
  (let* ((response (funcall *mcp-negotiation-request-function*
                            client
                            "initialize"
                            :params (mcp-build-initialize-params)
                            :timeout-seconds timeout-seconds))
         (result (%mcp-negotiation-table-value response "result")))
    (unless (hash-table-p result)
      (error "MCP initialize response for server ~A is missing a result object."
             (mcp-server-name server)))
    (let ((server-info (%mcp-parse-server-info server result)))
      (setf (mcp-server-server-info server) server-info)
      server-info)))

(defun mcp-server-tool-declared-p (server tool-name)
  (unless (mcp-server-p server)
    (error "SERVER must be an MCP-SERVER, got ~S." server))
  (let* ((server-info (mcp-server-server-info server))
         (declared-tools (and server-info
                              (mcp-server-info-declared-tools server-info)))
         (normalized (%mcp-normalize-tool-name tool-name)))
    (or (null declared-tools)
        (member normalized declared-tools :test #'string=))))

(defun mcp-update-server-discovered-tool-count (server count)
  (unless (mcp-server-p server)
    (error "SERVER must be an MCP-SERVER, got ~S." server))
  (let ((info (mcp-server-server-info server)))
    (when info
      (setf (mcp-server-info-discovered-tool-count info) (max 0 (or count 0)))))
  server)

(defun mcp-server-capability-summary (server)
  (unless (mcp-server-p server)
    (error "SERVER must be an MCP-SERVER, got ~S." server))
  (let ((info (mcp-server-server-info server)))
    (unless info
      (return-from mcp-server-capability-summary '()))
    (let ((summary '()))
      (when (mcp-server-info-tools-capability info)
        (push "tools" summary))
      (when (mcp-server-info-resources-capability info)
        (push "resources" summary))
      (when (mcp-server-info-prompts-capability info)
        (push "prompts" summary))
      (when (mcp-server-info-logging-capability info)
        (push "logging" summary))
      (nreverse summary))))
