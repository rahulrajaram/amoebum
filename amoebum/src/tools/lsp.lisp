(in-package :amoebum)

(defparameter *lsp-tool-client* nil)
(defparameter *lsp-tool-client-project-root* nil)
(defparameter *lsp-tool-diagnostics-cache* (make-hash-table :test #'equal))

(defun %lsp-tool-path-text (path)
  (typecase path
    (pathname (namestring path))
    (string path)
    (t (princ-to-string path))))

(defun %lsp-tool-canonical-file (path)
  (let ((resolved (or (ignore-errors (truename path))
                      (probe-file path)
                      (pathname path))))
    (namestring resolved)))

(defun %lsp-tool-root-key (path)
  (and path (namestring (uiop:ensure-directory-pathname path))))

(defun %lsp-tool-resolve-project-root (&optional preferred-path)
  (let ((candidate (or (and preferred-path
                            (make-pathname :name nil
                                           :type nil
                                           :defaults (pathname preferred-path)))
                       (ignore-errors (config-project-root (current-config)))
                       *default-pathname-defaults*)))
    (let ((directory (uiop:ensure-directory-pathname candidate)))
      (or (ignore-errors (truename directory))
          directory))))

(defun reset-lsp-tool-runtime ()
  (when *lsp-tool-client*
    (ignore-errors
      (lsp-client-stop *lsp-tool-client*)))
  (setf *lsp-tool-client* nil
        *lsp-tool-client-project-root* nil)
  (clrhash *lsp-tool-diagnostics-cache*)
  t)

(defun %lsp-tool-ensure-client (&key path)
  (when (and (null path) *lsp-tool-client*)
    (return-from %lsp-tool-ensure-client *lsp-tool-client*))
  (let* ((project-root (%lsp-tool-resolve-project-root path))
         (project-root-key (%lsp-tool-root-key project-root))
         (cached-root-key (%lsp-tool-root-key *lsp-tool-client-project-root*)))
    (when (and *lsp-tool-client*
               cached-root-key
               project-root-key
               (not (string= cached-root-key project-root-key)))
      (reset-lsp-tool-runtime))
    (unless *lsp-tool-client*
      (setf *lsp-tool-client* (make-lsp-client :project-root project-root)
            *lsp-tool-client-project-root* project-root))
    *lsp-tool-client*))

(defun %lsp-tool-ensure-file-path (path)
  (unless path
    (error "PATH is required."))
  (let ((resolved (or (ignore-errors (truename path))
                      (probe-file path))))
    (unless resolved
      (error "File does not exist: ~A." (%lsp-tool-path-text path)))
    (pathname resolved)))

(defun %lsp-tool-position-value (value label)
  (unless (and (integerp value) (>= value 1))
    (error "~A must be an integer >= 1, got ~S." label value))
  (1- value))

(defun %lsp-tool-make-hash (&rest key-values)
  (let ((table (make-hash-table :test #'equal)))
    (loop for (key value) on key-values by #'cddr do
      (setf (gethash key table) value))
    table))

(defun %lsp-tool-text-document-position-params (path line column)
  (let ((text-document (%lsp-tool-make-hash "uri" (%lsp-tool-file-uri path)))
        (position (%lsp-tool-make-hash "line" (%lsp-tool-position-value line "LINE")
                                       "character" (%lsp-tool-position-value column
                                                                            "COLUMN"))))
    (%lsp-tool-make-hash "textDocument" text-document
                         "position" position)))

(defun %lsp-tool-uri-path-encode (path-text)
  (with-output-to-string (stream)
    (loop for character across path-text do
      (case character
        (#\Space (write-string "%20" stream))
        (#\# (write-string "%23" stream))
        (#\? (write-string "%3F" stream))
        (#\% (write-string "%25" stream))
        (otherwise (write-char character stream))))))

(defun %lsp-tool-hex-digit-value (character)
  (cond
    ((and (char>= character #\0) (char<= character #\9))
     (- (char-code character) (char-code #\0)))
    ((and (char>= character #\A) (char<= character #\F))
     (+ 10 (- (char-code character) (char-code #\A))))
    ((and (char>= character #\a) (char<= character #\f))
     (+ 10 (- (char-code character) (char-code #\a))))
    (t nil)))

(defun %lsp-tool-uri-path-decode (value)
  (let ((text (or value "")))
    (with-output-to-string (stream)
      (loop for index from 0 below (length text) do
        (let ((character (char text index)))
          (if (and (char= character #\%)
                   (<= (+ index 2) (1- (length text))))
              (let* ((hi (%lsp-tool-hex-digit-value (char text (1+ index))))
                     (lo (%lsp-tool-hex-digit-value (char text (+ index 2)))))
                (if (and hi lo)
                    (progn
                      (write-char (code-char (+ (* hi 16) lo)) stream)
                      (incf index 2))
                    (write-char character stream)))
              (write-char character stream)))))))

(defun %lsp-tool-file-uri (path)
  (let* ((path-text (%lsp-tool-canonical-file path))
         (normalized (if (and (> (length path-text) 0)
                              (char= (char path-text 0) #\/))
                         path-text
                         (format nil "/~A" path-text))))
    (format nil "file://~A" (%lsp-tool-uri-path-encode normalized))))

(defun %lsp-tool-uri->path (uri)
  (cond
    ((and (stringp uri) (uiop:string-prefix-p "file://" uri))
     (%lsp-tool-uri-path-decode (subseq uri (length "file://"))))
    ((stringp uri) uri)
    (t nil)))

(defun %lsp-tool-sequence-list (value)
  (cond
    ((null value) nil)
    ((listp value) value)
    ((vectorp value) (coerce value 'list))
    (t (list value))))

(defun %lsp-tool-response-result (response method)
  (unless (hash-table-p response)
    (error "LSP method ~A returned non-object response: ~S." method response))
  (multiple-value-bind (error-object error-present-p)
      (gethash "error" response)
    (when error-present-p
      (let ((message (and (hash-table-p error-object)
                          (gethash "message" error-object))))
        (error "LSP method ~A failed: ~A"
               method
               (or message error-object)))))
  (multiple-value-bind (result result-present-p)
      (gethash "result" response)
    (if result-present-p
        result
        nil)))

(defun %lsp-tool-position-plist (position)
  (if (hash-table-p position)
      (let ((line (gethash "line" position))
            (column (gethash "character" position)))
        (list :line (if (integerp line) (1+ line) 1)
              :column (if (integerp column) (1+ column) 1)))
      (list :line 1 :column 1)))

(defun %lsp-tool-range-plist (range)
  (if (hash-table-p range)
      (let* ((start (%lsp-tool-position-plist (gethash "start" range)))
             (end (%lsp-tool-position-plist (gethash "end" range))))
        (list :line (getf start :line)
              :column (getf start :column)
              :end-line (getf end :line)
              :end-column (getf end :column)))
      (list :line 1 :column 1 :end-line 1 :end-column 1)))

(defun %lsp-tool-normalize-location (entry)
  (when (hash-table-p entry)
    (let* ((uri (or (gethash "uri" entry)
                    (gethash "targetUri" entry)))
           (range (or (gethash "range" entry)
                      (gethash "targetSelectionRange" entry)
                      (gethash "targetRange" entry)))
           (path (%lsp-tool-uri->path uri)))
      (append (list :uri uri
                    :path path)
              (%lsp-tool-range-plist range)))))

(defun %lsp-tool-normalize-locations (result)
  (let ((entries (cond
                   ((null result) nil)
                   ((hash-table-p result) (list result))
                   (t (%lsp-tool-sequence-list result)))))
    (remove nil
            (mapcar #'%lsp-tool-normalize-location entries))))

(defun %lsp-tool-hover-content-string (contents)
  (cond
    ((null contents) "")
    ((stringp contents) contents)
    ((hash-table-p contents)
     (or (gethash "value" contents)
         (gethash "contents" contents)
         (princ-to-string contents)))
    ((or (listp contents) (vectorp contents))
     (format nil "~{~A~^~%~}"
             (remove-if (lambda (item)
                          (or (null item)
                              (and (stringp item)
                                   (zerop (length item)))))
                        (mapcar #'%lsp-tool-hover-content-string
                                (%lsp-tool-sequence-list contents)))))
    (t (princ-to-string contents))))

(defun %lsp-tool-normalize-hover (result)
  (if (hash-table-p result)
      (let ((range (gethash "range" result))
            (contents (gethash "contents" result)))
        (append (list :contents (%lsp-tool-hover-content-string contents))
                (when range (%lsp-tool-range-plist range))))
      (list :contents "")))

(defun %lsp-tool-diagnostic-severity (value)
  (case value
    (1 :error)
    (2 :warning)
    (3 :information)
    (4 :hint)
    (otherwise :unknown)))

(defun %lsp-tool-normalize-diagnostic (diagnostic default-uri)
  (when (hash-table-p diagnostic)
    (let* ((uri (or (gethash "uri" diagnostic) default-uri))
           (range (%lsp-tool-range-plist (gethash "range" diagnostic)))
           (severity-code (gethash "severity" diagnostic))
           (severity (%lsp-tool-diagnostic-severity severity-code)))
      (append
       (list :uri uri
             :path (%lsp-tool-uri->path uri)
             :severity severity
             :severity-code severity-code
             :message (or (gethash "message" diagnostic) "")
             :source (gethash "source" diagnostic)
             :code (gethash "code" diagnostic))
       range))))

(defun %lsp-tool-normalize-diagnostics (diagnostics &key default-uri)
  (remove nil
          (mapcar (lambda (entry)
                    (%lsp-tool-normalize-diagnostic entry default-uri))
                  (%lsp-tool-sequence-list diagnostics))))

(defun %lsp-tool-cache-diagnostics (uri diagnostics)
  (when uri
    (setf (gethash uri *lsp-tool-diagnostics-cache*) diagnostics))
  diagnostics)

(defun %lsp-tool-handle-notification (notification)
  (when (hash-table-p notification)
    (let ((method (gethash "method" notification)))
      (when (and (stringp method)
                 (string= method "textDocument/publishDiagnostics"))
        (let* ((params (gethash "params" notification))
               (uri (and (hash-table-p params) (gethash "uri" params)))
               (diagnostics (and (hash-table-p params)
                                 (gethash "diagnostics" params)))
               (normalized (%lsp-tool-normalize-diagnostics diagnostics
                                                            :default-uri uri)))
          (%lsp-tool-cache-diagnostics uri normalized))))))

(defun %lsp-tool-refresh-diagnostics-cache (client)
  (when (and client (lsp-client-p client))
    (maphash
     (lambda (language-id _connection)
       (declare (ignore _connection))
       (dolist (notification (lsp-drain-notifications client language-id))
         (%lsp-tool-handle-notification notification)))
     (lsp-client-connections client)))
  t)

(defun %lsp-tool-pull-diagnostics (client file-path)
  (let* ((path (%lsp-tool-ensure-file-path file-path))
         (uri (%lsp-tool-file-uri path))
         (params (%lsp-tool-make-hash "textDocument"
                                      (%lsp-tool-make-hash "uri" uri))))
    (handler-case
        (let* ((response (lsp-send-request client path "textDocument/diagnostic"
                                           :params params))
               (result (%lsp-tool-response-result response "textDocument/diagnostic"))
               (items (cond
                        ((hash-table-p result)
                         (or (gethash "items" result)
                             (gethash "diagnostics" result)))
                        (t result)))
               (normalized (%lsp-tool-normalize-diagnostics items :default-uri uri)))
          (%lsp-tool-cache-diagnostics uri normalized)
          (values normalized :pull nil))
      (error (condition)
        (values nil :none (princ-to-string condition))))))

(defun %lsp-tool-project-diagnostics ()
  (let ((aggregate '()))
    (maphash (lambda (_uri diagnostics)
               (declare (ignore _uri))
               (setf aggregate (nconc aggregate (copy-list diagnostics))))
             *lsp-tool-diagnostics-cache*)
    aggregate))

(defun %lsp-tool-position-request (client path method line column
                                   &key extra-params)
  (let* ((params (%lsp-tool-text-document-position-params path line column)))
    (when extra-params
      (maphash (lambda (key value)
                 (setf (gethash key params) value))
               extra-params))
    (lsp-send-request client path method :params params)))

(deftool lsp-goto-definition ((path pathname :description "Absolute path to source file." :required t)
                              (line (integer 1) :description "1-based line number." :required t)
                              (column (integer 1) :description "1-based column number." :required t))
  "Resolve definition location for the symbol at PATH:LINE:COLUMN."
  (:permission :auto)
  (:dangerous nil)
  (:category :lsp)
  (:timeout 20)
  (let* ((canonical-path (%lsp-tool-ensure-file-path path))
         (client (%lsp-tool-ensure-client :path canonical-path))
         (response (%lsp-tool-position-request client
                                               canonical-path
                                               "textDocument/definition"
                                               line
                                               column))
         (definitions (%lsp-tool-normalize-locations
                       (%lsp-tool-response-result response
                                                  "textDocument/definition"))))
    (list :path (%lsp-tool-canonical-file canonical-path)
          :line line
          :column column
          :definitions definitions
          :count (length definitions))))

(deftool lsp-find-references ((path pathname :description "Absolute path to source file." :required t)
                              (line (integer 1) :description "1-based line number." :required t)
                              (column (integer 1) :description "1-based column number." :required t)
                              (include-declaration boolean
                               :description "Include symbol declaration in reference list."
                               :default t))
  "Find symbol references at PATH:LINE:COLUMN."
  (:permission :auto)
  (:dangerous nil)
  (:category :lsp)
  (:timeout 20)
  (let* ((canonical-path (%lsp-tool-ensure-file-path path))
         (client (%lsp-tool-ensure-client :path canonical-path))
         (extra (%lsp-tool-make-hash
                 "context" (%lsp-tool-make-hash "includeDeclaration" include-declaration)))
         (response (%lsp-tool-position-request client
                                               canonical-path
                                               "textDocument/references"
                                               line
                                               column
                                               :extra-params extra))
         (references (%lsp-tool-normalize-locations
                      (%lsp-tool-response-result response
                                                 "textDocument/references"))))
    (list :path (%lsp-tool-canonical-file canonical-path)
          :line line
          :column column
          :include-declaration include-declaration
          :references references
          :count (length references))))

(deftool lsp-hover ((path pathname :description "Absolute path to source file." :required t)
                    (line (integer 1) :description "1-based line number." :required t)
                    (column (integer 1) :description "1-based column number." :required t))
  "Fetch hover documentation/type info at PATH:LINE:COLUMN."
  (:permission :auto)
  (:dangerous nil)
  (:category :lsp)
  (:timeout 20)
  (let* ((canonical-path (%lsp-tool-ensure-file-path path))
         (client (%lsp-tool-ensure-client :path canonical-path))
         (response (%lsp-tool-position-request client
                                               canonical-path
                                               "textDocument/hover"
                                               line
                                               column))
         (hover (%lsp-tool-normalize-hover
                 (%lsp-tool-response-result response "textDocument/hover"))))
    (list :path (%lsp-tool-canonical-file canonical-path)
          :line line
          :column column
          :hover hover
          :contents (getf hover :contents))))

(deftool lsp-diagnostics ((path (or null pathname)
                              :description "Absolute path to file. NIL returns cached project diagnostics."
                              :default nil))
  "Return LSP diagnostics for a file or all cached diagnostics across project."
  (:permission :auto)
  (:dangerous nil)
  (:category :lsp)
  (:timeout 20)
  (let* ((canonical-path (and path (%lsp-tool-ensure-file-path path)))
         (client (%lsp-tool-ensure-client :path canonical-path))
         (warning nil))
    (%lsp-tool-refresh-diagnostics-cache client)
    (if canonical-path
        (multiple-value-bind (diagnostics source source-warning)
            (%lsp-tool-pull-diagnostics client canonical-path)
          (setf warning source-warning)
          (unless diagnostics
            (%lsp-tool-refresh-diagnostics-cache client)
            (setf diagnostics (or (gethash (%lsp-tool-file-uri canonical-path)
                                           *lsp-tool-diagnostics-cache*)
                                  '())
                  source :publish))
          (list :scope :file
                :path (%lsp-tool-canonical-file canonical-path)
                :source source
                :diagnostics diagnostics
                :count (length diagnostics)
                :warning warning))
        (let ((diagnostics (%lsp-tool-project-diagnostics)))
          (list :scope :project
                :source :publish
                :diagnostics diagnostics
                :count (length diagnostics))))))
