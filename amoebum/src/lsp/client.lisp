(in-package :amoebum)

(defparameter *lsp-server-initialize-timeout-seconds* 10.0d0)
(defparameter *lsp-request-timeout-seconds* 10.0d0)
(defparameter *lsp-server-restart-backoff-base-seconds* 1.0d0)
(defparameter *lsp-server-restart-backoff-max-seconds* 30.0d0)
(defparameter *lsp-server-shutdown-grace-seconds* 5.0d0)
(defparameter *lsp-server-poll-interval-seconds* 0.05d0)

(defparameter *lsp-process-spawner* nil)

(defstruct (lsp-server-spec
            (:constructor %make-lsp-server-spec
                (&key
                   name
                   language-id
                   command
                   (args nil)
                   (file-extensions nil))))
  name
  language-id
  command
  args
  file-extensions)

(defstruct (lsp-document-state
            (:constructor make-lsp-document-state
                (&key path uri language-id text (version 1))))
  path
  uri
  language-id
  text
  (version 1 :type integer))

(defstruct (lsp-server-connection
            (:constructor %make-lsp-server-connection
                (&key spec)))
  spec
  process
  jsonrpc-client
  (open-documents (make-hash-table :test #'equal))
  (restart-count 0 :type integer)
  (consecutive-restart-failures 0 :type integer)
  last-error)

(defstruct (lsp-client
            (:constructor %make-lsp-client
                (&key
                   project-root
                   (server-specs nil)
                   (initialize-timeout-seconds
                    *lsp-server-initialize-timeout-seconds*)
                   (request-timeout-seconds *lsp-request-timeout-seconds*)
                   (restart-backoff-base-seconds
                    *lsp-server-restart-backoff-base-seconds*)
                   (restart-backoff-max-seconds
                    *lsp-server-restart-backoff-max-seconds*)
                   (shutdown-grace-seconds *lsp-server-shutdown-grace-seconds*)
                   (auto-restart-p t))))
  project-root
  server-specs
  (connections (make-hash-table :test #'equal))
  (initialize-timeout-seconds *lsp-server-initialize-timeout-seconds* :type real)
  (request-timeout-seconds *lsp-request-timeout-seconds* :type real)
  (restart-backoff-base-seconds *lsp-server-restart-backoff-base-seconds*
   :type real)
  (restart-backoff-max-seconds *lsp-server-restart-backoff-max-seconds*
   :type real)
  (shutdown-grace-seconds *lsp-server-shutdown-grace-seconds* :type real)
  (auto-restart-p t :type boolean)
  (lock (bordeaux-threads:make-lock "amoebum-lsp-client-lock")))

(defmacro %with-lsp-client-lock ((client) &body body)
  `(bordeaux-threads:with-lock-held ((lsp-client-lock ,client))
     ,@body))

(defun %normalize-lsp-name (value label)
  (let ((normalized (string-downcase (string-trim '(#\Space #\Tab #\Newline #\Return)
                                                   (princ-to-string value)))))
    (unless (> (length normalized) 0)
      (error "~A must not be empty." label))
    normalized))

(defun %normalize-lsp-args (args)
  (unless (listp args)
    (error "LSP server ARGS must be a list of strings, got ~S." args))
  (loop for arg in args
        collect
        (if (stringp arg)
            arg
            (error "LSP server arg must be a string, got ~S." arg))))

(defun %normalize-lsp-extension (extension)
  (let ((normalized (string-downcase (string-trim '(#\Space #\Tab #\Newline #\Return)
                                                   (princ-to-string extension)))))
    (unless (> (length normalized) 0)
      (error "LSP file extension must not be empty."))
    (if (char= (char normalized 0) #\.)
        normalized
        (format nil ".~A" normalized))))

(defun %normalize-lsp-extensions (extensions)
  (unless (listp extensions)
    (error "LSP FILE-EXTENSIONS must be a list, got ~S." extensions))
  (remove-duplicates
   (loop for extension in extensions collect (%normalize-lsp-extension extension))
   :test #'string=))

(defun %normalize-lsp-timeout (value label)
  (unless (and (realp value) (> value 0))
    (error "~A must be a positive real, got ~S." label value))
  (float value 1.0d0))

(defun make-lsp-server-spec (&key name language-id command (args nil) (file-extensions nil))
  (let ((normalized-command (%normalize-lsp-name command "LSP COMMAND")))
    (%make-lsp-server-spec
     :name (%normalize-lsp-name (or name normalized-command) "LSP NAME")
     :language-id (%normalize-lsp-name language-id "LSP LANGUAGE-ID")
     :command normalized-command
     :args (%normalize-lsp-args args)
     :file-extensions (%normalize-lsp-extensions file-extensions))))

(defparameter *lsp-default-server-specs*
  (list
   (make-lsp-server-spec :name "pyright"
                         :language-id "python"
                         :command "pyright-langserver"
                         :args '("--stdio")
                         :file-extensions '(".py" ".pyi"))
   (make-lsp-server-spec :name "rust-analyzer"
                         :language-id "rust"
                         :command "rust-analyzer"
                         :file-extensions '(".rs"))
   (make-lsp-server-spec :name "clangd"
                         :language-id "cpp"
                         :command "clangd"
                         :file-extensions '(".c" ".cc" ".cpp" ".cxx" ".h" ".hpp"))))

(defun %normalize-lsp-project-root (project-root)
  (let ((resolved (or project-root
                      (ignore-errors (config-project-root (current-config)))
                      *default-pathname-defaults*)))
    (handler-case
        (let ((pathname (uiop:ensure-directory-pathname resolved)))
          (or (ignore-errors (truename pathname))
              pathname))
      (error ()
        nil))))

(defun %normalize-server-spec-list (server-specs)
  (unless (listp server-specs)
    (error "SERVER-SPECS must be a list of LSP-SERVER-SPEC values, got ~S."
           server-specs))
  (loop for spec in server-specs collect
        (if (lsp-server-spec-p spec)
            spec
            (error "Expected LSP-SERVER-SPEC, got ~S." spec))))

(defun make-lsp-client (&key
                          project-root
                          (server-specs *lsp-default-server-specs*)
                          (initialize-timeout-seconds
                            *lsp-server-initialize-timeout-seconds*)
                          (request-timeout-seconds *lsp-request-timeout-seconds*)
                          (restart-backoff-base-seconds
                            *lsp-server-restart-backoff-base-seconds*)
                          (restart-backoff-max-seconds
                            *lsp-server-restart-backoff-max-seconds*)
                          (shutdown-grace-seconds *lsp-server-shutdown-grace-seconds*)
                          (auto-restart-p t))
  (%make-lsp-client
   :project-root (%normalize-lsp-project-root project-root)
   :server-specs (%normalize-server-spec-list server-specs)
   :initialize-timeout-seconds (%normalize-lsp-timeout initialize-timeout-seconds
                                                        "INITIALIZE-TIMEOUT-SECONDS")
   :request-timeout-seconds (%normalize-lsp-timeout request-timeout-seconds
                                                     "REQUEST-TIMEOUT-SECONDS")
   :restart-backoff-base-seconds
   (%normalize-lsp-timeout restart-backoff-base-seconds
                           "RESTART-BACKOFF-BASE-SECONDS")
   :restart-backoff-max-seconds
   (%normalize-lsp-timeout restart-backoff-max-seconds
                           "RESTART-BACKOFF-MAX-SECONDS")
   :shutdown-grace-seconds (%normalize-lsp-timeout shutdown-grace-seconds
                                                   "SHUTDOWN-GRACE-SECONDS")
   :auto-restart-p (not (null auto-restart-p))))

(defun %lsp-path-extension (path)
  (let ((type (pathname-type (pathname path))))
    (when type
      (format nil ".~A" (string-downcase (princ-to-string type))))))

(defun %canonical-path-text (path)
  (namestring (or (ignore-errors (truename path))
                  (probe-file path)
                  (pathname path))))

(defun %uri-path-encode (path-text)
  (with-output-to-string (stream)
    (loop for character across path-text do
      (case character
        (#\Space (write-string "%20" stream))
        (#\# (write-string "%23" stream))
        (#\? (write-string "%3F" stream))
        (#\% (write-string "%25" stream))
        (otherwise (write-char character stream))))))

(defun %file-uri (path)
  (let* ((path-text (%canonical-path-text path))
         (normalized (if (and (> (length path-text) 0)
                              (char= (char path-text 0) #\/))
                         path-text
                         (format nil "/~A" path-text))))
    (format nil "file://~A" (%uri-path-encode normalized))))

(defun %read-file-text (path)
  (with-open-file (stream path :direction :input :external-format :utf-8)
    (with-output-to-string (output)
      (loop for line = (read-line stream nil nil)
            while line do
              (write-string line output)
              (write-char #\Newline output)))))

(defun %server-spec-for-extension (extension server-specs)
  (find-if
   (lambda (spec)
     (member extension (lsp-server-spec-file-extensions spec)
             :test #'string=))
   server-specs))

(defun lsp-server-spec-for-path (path &optional (server-specs *lsp-default-server-specs*))
  (let ((extension (%lsp-path-extension path)))
    (and extension
         (%server-spec-for-extension extension server-specs))))

(defun lsp-language-id-for-path (path &optional (server-specs *lsp-default-server-specs*))
  (let ((spec (lsp-server-spec-for-path path server-specs)))
    (and spec (lsp-server-spec-language-id spec))))

(defun %lsp-server-spec-for-language-id (client language-id)
  (let ((normalized (%normalize-lsp-name language-id "LANGUAGE-ID")))
    (find normalized
          (lsp-client-server-specs client)
          :test #'string=
          :key #'lsp-server-spec-language-id)))

(defun %lsp-process-alive-p (process)
  (and process
       #+sbcl
       (ignore-errors (sb-ext:process-alive-p process))
       #-sbcl
       nil))

(defun %lsp-wait-for-process-exit (process timeout-seconds)
  (let* ((seconds (max 0.0d0 (float timeout-seconds 1.0d0)))
         (deadline (+ (get-internal-real-time)
                      (ceiling (* seconds internal-time-units-per-second)))))
    (loop
      do
         (unless (%lsp-process-alive-p process)
           #+sbcl
           (ignore-errors (sb-ext:process-wait process))
           (return t))
         (when (>= (get-internal-real-time) deadline)
           (return nil))
         (sleep *lsp-server-poll-interval-seconds*))))

(defun %lsp-signal-process (process signal)
  (when process
    #+sbcl
    (ignore-errors
      (sb-ext:process-kill process signal))
    #-sbcl
    nil))

(defun %lsp-close-process (process)
  (when process
    #+sbcl
    (ignore-errors
      (sb-ext:process-close process))
    #-sbcl
    nil))

(defun default-lsp-process-spawner (command args &key directory)
  #+sbcl
  (sb-ext:run-program command
                      args
                      :search t
                      :wait nil
                      :directory directory
                      :input :stream
                      :output :stream
                      :error *error-output*)
  #-sbcl
  (declare (ignore command args directory))
  #-sbcl
  (error "LSP client lifecycle requires SBCL run-program support."))

(defun %lsp-initialize-params (client)
  (let ((params (make-hash-table :test #'equal))
        (client-info (make-hash-table :test #'equal))
        (capabilities (make-hash-table :test #'equal)))
    (setf (gethash "processId" params) nil
          (gethash "clientInfo" params) client-info
          (gethash "capabilities" params) capabilities
          (gethash "name" client-info) "amoebum"
          (gethash "version" client-info) "0.1.0")
    (when (lsp-client-project-root client)
      (setf (gethash "rootPath" params) (namestring (lsp-client-project-root client))
            (gethash "rootUri" params) (%file-uri (lsp-client-project-root client))))
    params))

(defun %lsp-did-open-params (document)
  (let ((params (make-hash-table :test #'equal))
        (text-document (make-hash-table :test #'equal)))
    (setf (gethash "uri" text-document) (lsp-document-state-uri document)
          (gethash "languageId" text-document) (lsp-document-state-language-id document)
          (gethash "version" text-document) (lsp-document-state-version document)
          (gethash "text" text-document) (lsp-document-state-text document)
          (gethash "textDocument" params) text-document)
    params))

(defun %lsp-send-shutdown-sequence (client jsonrpc-client)
  (when jsonrpc-client
    (ignore-errors
      (mcp-jsonrpc-send-request
       jsonrpc-client
       "shutdown"
       :timeout-seconds (lsp-client-request-timeout-seconds client)))
    (ignore-errors
      (mcp-jsonrpc-send-notification jsonrpc-client "exit"))
    (ignore-errors
      (mcp-jsonrpc-stop-reader jsonrpc-client))))

(defun %lsp-shutdown-connection (client process jsonrpc-client)
  (%lsp-send-shutdown-sequence client jsonrpc-client)
  (unless (%lsp-wait-for-process-exit process
                                      (lsp-client-shutdown-grace-seconds client))
    (%lsp-signal-process process 15)
    (unless (%lsp-wait-for-process-exit process 1.0d0)
      (%lsp-signal-process process 9)
      (%lsp-wait-for-process-exit process 1.0d0)))
  (%lsp-close-process process)
  (not (%lsp-process-alive-p process)))

(defun %lsp-connection-ready-p (connection)
  (and connection
       (lsp-server-connection-process connection)
       (lsp-server-connection-jsonrpc-client connection)
       (%lsp-process-alive-p (lsp-server-connection-process connection))))

(defun %lsp-detach-connection (connection)
  (prog1
      (values (lsp-server-connection-process connection)
              (lsp-server-connection-jsonrpc-client connection))
    (setf (lsp-server-connection-process connection) nil
          (lsp-server-connection-jsonrpc-client connection) nil)))

(defun %lsp-restore-open-documents (connection)
  (let ((jsonrpc-client (lsp-server-connection-jsonrpc-client connection)))
    (maphash
     (lambda (_path document)
       (declare (ignore _path))
       (mcp-jsonrpc-send-notification
        jsonrpc-client
        "textDocument/didOpen"
        :params (%lsp-did-open-params document)))
     (lsp-server-connection-open-documents connection))))

(defun %lsp-start-connection (client connection &key restart-p)
  (let* ((spec (lsp-server-connection-spec connection))
         (process (funcall (or *lsp-process-spawner* #'default-lsp-process-spawner)
                           (lsp-server-spec-command spec)
                           (lsp-server-spec-args spec)
                           :directory (lsp-client-project-root client)))
         (jsonrpc-client (make-mcp-jsonrpc-client
                          :input-stream #+sbcl (sb-ext:process-output process)
                          :output-stream #+sbcl (sb-ext:process-input process)
                          :start-reader-p t)))
    (handler-case
        (progn
          (mcp-jsonrpc-send-request
           jsonrpc-client
           "initialize"
           :params (%lsp-initialize-params client)
           :timeout-seconds (lsp-client-initialize-timeout-seconds client))
          (mcp-jsonrpc-send-notification jsonrpc-client "initialized")
          (setf (lsp-server-connection-process connection) process
                (lsp-server-connection-jsonrpc-client connection) jsonrpc-client
                (lsp-server-connection-last-error connection) nil
                (lsp-server-connection-consecutive-restart-failures connection) 0)
          (when restart-p
            (incf (lsp-server-connection-restart-count connection)))
          (%lsp-restore-open-documents connection)
          connection)
      (error (condition)
        (%lsp-shutdown-connection client process jsonrpc-client)
        (setf (lsp-server-connection-last-error connection) condition)
        (error condition)))))

(defun %lsp-next-restart-delay (client connection)
  (let* ((attempt (1+ (lsp-server-connection-consecutive-restart-failures connection)))
         (base (lsp-client-restart-backoff-base-seconds client))
         (max-delay (lsp-client-restart-backoff-max-seconds client))
         (delay (min max-delay
                     (* base (expt 2 (1- attempt))))))
    (setf (lsp-server-connection-consecutive-restart-failures connection) attempt)
    (float delay 1.0d0)))

(defun %lsp-restart-connection (client connection)
  (unless (lsp-client-auto-restart-p client)
    (error "LSP server ~A crashed and AUTO-RESTART-P is disabled."
           (lsp-server-spec-name (lsp-server-connection-spec connection))))
  (let ((delay (%lsp-next-restart-delay client connection)))
    (multiple-value-bind (process jsonrpc-client)
        (%lsp-detach-connection connection)
      (%lsp-shutdown-connection client process jsonrpc-client))
    (sleep delay)
    (%lsp-start-connection client connection :restart-p t)))

(defun %lsp-ensure-connection-running (client connection)
  (cond
    ((%lsp-connection-ready-p connection)
     connection)
    ((and (lsp-server-connection-process connection)
          (lsp-client-auto-restart-p client))
     (%lsp-restart-connection client connection))
    ((lsp-server-connection-process connection)
     (error "LSP server ~A is not alive."
            (lsp-server-spec-name (lsp-server-connection-spec connection))))
    (t
     (%lsp-start-connection client connection))))

(defun lsp-client-connection (client language-id)
  (unless (lsp-client-p client)
    (error "CLIENT must be an LSP-CLIENT, got ~S." client))
  (let ((normalized (%normalize-lsp-name language-id "LANGUAGE-ID")))
    (%with-lsp-client-lock (client)
      (gethash normalized (lsp-client-connections client)))))

(defun %lsp-spec-from-client-and-path (client file-path language-id)
  (if language-id
      (or (%lsp-server-spec-for-language-id client language-id)
          (error "No LSP server configured for language-id ~S." language-id))
      (or (lsp-server-spec-for-path file-path (lsp-client-server-specs client))
          (error "No LSP server configured for file path ~A."
                 (%canonical-path-text file-path)))))

(defun lsp-client-ensure-server (client file-path &key language-id)
  (unless (lsp-client-p client)
    (error "CLIENT must be an LSP-CLIENT, got ~S." client))
  (let* ((spec (%lsp-spec-from-client-and-path client file-path language-id))
         (connection nil))
    (%with-lsp-client-lock (client)
      (setf connection
            (or (gethash (lsp-server-spec-language-id spec)
                         (lsp-client-connections client))
                (setf (gethash (lsp-server-spec-language-id spec)
                               (lsp-client-connections client))
                      (%make-lsp-server-connection :spec spec))))
      (%lsp-ensure-connection-running client connection))
    connection))

(defun lsp-open-document (client file-path &key text (version 1) language-id)
  (unless (lsp-client-p client)
    (error "CLIENT must be an LSP-CLIENT, got ~S." client))
  (unless (and (integerp version) (>= version 0))
    (error "VERSION must be an integer >= 0, got ~S." version))
  (let* ((connection (lsp-client-ensure-server client file-path :language-id language-id))
         (spec (lsp-server-connection-spec connection))
         (path-text (%canonical-path-text file-path))
         (document (make-lsp-document-state
                    :path path-text
                    :uri (%file-uri path-text)
                    :language-id (lsp-server-spec-language-id spec)
                    :text (or text (%read-file-text path-text))
                    :version version)))
    (%with-lsp-client-lock (client)
      (%lsp-ensure-connection-running client connection)
      (mcp-jsonrpc-send-notification
       (lsp-server-connection-jsonrpc-client connection)
       "textDocument/didOpen"
       :params (%lsp-did-open-params document))
      (setf (gethash path-text (lsp-server-connection-open-documents connection))
            document))
    (list :language-id (lsp-server-spec-language-id spec)
          :server-name (lsp-server-spec-name spec)
          :path path-text
          :uri (lsp-document-state-uri document))))

(defun lsp-send-notification (client file-path method &key params language-id)
  (unless (lsp-client-p client)
    (error "CLIENT must be an LSP-CLIENT, got ~S." client))
  (let ((connection (lsp-client-ensure-server client file-path :language-id language-id)))
    (%with-lsp-client-lock (client)
      (%lsp-ensure-connection-running client connection)
      (if params
          (mcp-jsonrpc-send-notification
           (lsp-server-connection-jsonrpc-client connection)
           method
           :params params)
          (mcp-jsonrpc-send-notification
           (lsp-server-connection-jsonrpc-client connection)
           method)))))

(defun lsp-send-request (client file-path method
                         &key
                           params
                           language-id
                           timeout-seconds
                           (open-document-p t))
  (unless (lsp-client-p client)
    (error "CLIENT must be an LSP-CLIENT, got ~S." client))
  (let ((connection (lsp-client-ensure-server client file-path :language-id language-id)))
    (when open-document-p
      (let ((document-key (%canonical-path-text file-path)))
        (unless (gethash document-key (lsp-server-connection-open-documents connection))
          (lsp-open-document client file-path :language-id language-id))))
    (%with-lsp-client-lock (client)
      (%lsp-ensure-connection-running client connection)
      (if params
          (mcp-jsonrpc-send-request
           (lsp-server-connection-jsonrpc-client connection)
           method
           :params params
           :timeout-seconds (or timeout-seconds
                                (lsp-client-request-timeout-seconds client)))
          (mcp-jsonrpc-send-request
           (lsp-server-connection-jsonrpc-client connection)
           method
           :timeout-seconds (or timeout-seconds
                                (lsp-client-request-timeout-seconds client)))))))

(defun lsp-drain-notifications (client language-id)
  (unless (lsp-client-p client)
    (error "CLIENT must be an LSP-CLIENT, got ~S." client))
  (let ((connection (lsp-client-connection client language-id)))
    (if (and connection
             (lsp-server-connection-jsonrpc-client connection))
        (mcp-jsonrpc-drain-notifications
         (lsp-server-connection-jsonrpc-client connection))
        nil)))

(defun lsp-client-stop (client)
  (unless (lsp-client-p client)
    (error "CLIENT must be an LSP-CLIENT, got ~S." client))
  (%with-lsp-client-lock (client)
    (maphash
     (lambda (_language-id connection)
       (declare (ignore _language-id))
       (multiple-value-bind (process jsonrpc-client)
           (%lsp-detach-connection connection)
         (%lsp-shutdown-connection client process jsonrpc-client)))
     (lsp-client-connections client)))
  client)
