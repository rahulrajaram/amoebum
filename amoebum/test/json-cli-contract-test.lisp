(in-package :amoebum/test)

(def-suite json-cli-contract-suite
  :in amoebum-suite
  :description "Machine-readable JSON CLI contract tests (I337).")

(in-suite json-cli-contract-suite)

(defun %i337-temp-directory ()
  (uiop:ensure-directory-pathname
   (merge-pathnames
    (make-pathname
     :directory `(:relative
                  ,(format nil "amoebum-i337-~D-~D"
                           (get-universal-time)
                           (random 1000000))))
    (uiop:ensure-directory-pathname (uiop:temporary-directory)))))

(defmacro with-i337-project-root ((var) &body body)
  `(let* ((tmp-root (%i337-temp-directory))
          (old-config (amoebum.config:current-config))
          (old-project-root (amoebum.config:config-project-root old-config)))
     (unwind-protect
          (progn
            (amoebum.config:reload-config :project-root tmp-root)
            (let ((,var tmp-root))
              ,@body))
       (amoebum.config:reload-config :project-root old-project-root)
       (ignore-errors
         (uiop:delete-directory-tree tmp-root
                                     :validate t
                                     :if-does-not-exist :ignore)))))

(defun %i337-parse-json (payload)
  (let* ((jonathan-package (or (find-package :jonathan)
                               (error "Missing package JONATHAN.")))
         (parse-symbol (or (find-symbol "PARSE" jonathan-package)
                           (error "Missing JONATHAN:PARSE."))))
    (funcall (symbol-function parse-symbol) payload :as :hash-table)))

(defun %i337-json-seq->list (value)
  (cond
    ((vectorp value) (loop for item across value collect item))
    ((listp value) value)
    (t '())))

(defun %i337-find-event (events kind phase)
  (find-if (lambda (event)
             (and (hash-table-p event)
                  (string= (or (gethash "kind" event) "") kind)
                  (string= (or (gethash "phase" event) "") phase)))
           events))

(defun %i337-write-sample-png (path)
  (ensure-directories-exist path)
  (with-open-file (stream path
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create
                          :element-type '(unsigned-byte 8))
    (dolist (octet '(137 80 78 71 13 10 26 10 0 0 0 0))
      (write-byte octet stream))))

(defun %i337-invoke-json-cli (&rest args)
  (let ((ok nil))
    (let* ((stdout
             (with-output-to-string (stream)
               (let ((*standard-output* stream))
                 (setf ok (apply #'amoebum::run-cli-json args)))))
           (json-text (string-trim '(#\Space #\Tab #\Newline #\Return) stdout)))
      (values ok (%i337-parse-json json-text)))))

(defun %i337-capture-main-output (&rest args)
  (let ((ok nil))
    (values
     (with-output-to-string (stream)
       (let ((*standard-output* stream))
         (setf ok (apply #'amoebum::main args))))
     ok)))

(test json-cli-contract-prompt-success-shape
  (with-i337-project-root (project-root)
    (let ((image-path (merge-pathnames #P"fixtures/sample.png" project-root)))
      (%i337-write-sample-png image-path)
      (multiple-value-bind (ok payload)
          (%i337-invoke-json-cli "--json"
                                 "--prompt" "triage this image"
                                 "--image" (namestring image-path))
        (let* ((result (gethash "result" payload))
               (events (%i337-json-seq->list (gethash "events" payload))))
          (is-true ok "Expected run-cli-json prompt path to succeed.")
          (is (eq (gethash "ok" payload) t))
          (is (string= (gethash "schema_version" payload) "amoebum.cli.json.v1"))
          (is (string= (gethash "schema_doc" payload) "docs/json-cli-contract.md"))
          (is (string= (gethash "kind" result) "prompt"))
          (is (string= (gethash "status" result) "completed"))
          (is-true (%i337-find-event events "progress" "started"))
          (is-true (%i337-find-event events "progress" "completed")))))))

(test json-cli-contract-command-tool-shape
  (with-i337-project-root (project-root)
    (declare (ignore project-root))
    (multiple-value-bind (ok payload)
        (%i337-invoke-json-cli "--json" "--command" "/config")
      (let* ((result (gethash "result" payload))
             (tool (and (hash-table-p result) (gethash "tool" result)))
             (events (%i337-json-seq->list (gethash "events" payload))))
        (is-true ok "Expected /config command invocation to succeed.")
        (is (eq (gethash "ok" payload) t))
        (is (string= (gethash "action" payload) "command"))
        (is (string= (gethash "kind" result) "tool"))
        (is (string= (gethash "status" result) "completed"))
        (is (string= (gethash "name" tool) "slash-command"))
        (is-true (%i337-find-event events "tool" "completed"))))))

(test json-cli-contract-error-shape
  (with-i337-project-root (project-root)
    (let ((missing-image (merge-pathnames #P"fixtures/missing.png" project-root)))
      (multiple-value-bind (ok payload)
          (%i337-invoke-json-cli "--json"
                                 "--prompt" "missing image should fail"
                                 "--image" (namestring missing-image))
        (let* ((result (gethash "result" payload))
               (events (%i337-json-seq->list (gethash "events" payload)))
               (error-text (or (gethash "error" payload) "")))
          (is (null ok))
          (is (null (gethash "ok" payload)))
          (is (string= (gethash "kind" result) "error"))
          (is (string= (gethash "status" result) "failed"))
          (is-true (search "does not exist" error-text :test #'char-equal))
          (is-true (%i337-find-event events "progress" "failed")))))))

(test cli-help-fast-exit-prints-usage
  (multiple-value-bind (output ok)
      (%i337-capture-main-output "--help")
    (is-true ok "Expected --help to return success.")
    (is-true (search "Usage:" output :test #'char-equal))
    (is-true (search "amoebum --version" output :test #'char-equal))
    (is-false (search "Type below and press Enter" output :test #'char-equal))))

(test cli-version-fast-exit-prints-version
  (multiple-value-bind (output ok)
      (%i337-capture-main-output "--version")
    (is-true ok "Expected --version to return success.")
    (is-true (search "amoebum 0.1.0" output :test #'char-equal))))
