(in-package :amoebum/test)

(def-suite tool-argument-prompting-suite :in amoebum-suite
  :description "I366 missing required tool argument prompting and headless error shape.")

(in-suite tool-argument-prompting-suite)

(defun %i366-parse-json (payload)
  (let* ((jonathan-package (or (find-package :jonathan)
                               (error "Missing package JONATHAN.")))
         (parse-symbol (or (find-symbol "PARSE" jonathan-package)
                           (error "Missing JONATHAN:PARSE."))))
    (funcall (symbol-function parse-symbol) payload :as :hash-table)))

(defun %i366-register-required-tool (toolset tool-name)
  (pseudopod:register-tool-function
   toolset
   :name tool-name
   :description "Requires path and echoes it."
   :parameters (let ((schema (make-hash-table :test #'equal))
                      (properties (make-hash-table :test #'equal))
                      (path (make-hash-table :test #'equal)))
                 (setf (gethash "type" schema) "object")
                 (setf (gethash "type" path) "string")
                 (setf (gethash "path" properties) path)
                 (setf (gethash "properties" schema) properties)
                 (setf (gethash "required" schema) (vector "path"))
                 schema)
   :fn (lambda (arguments _call)
         (declare (ignore _call))
         (format nil "path=~A" (or (gethash "path" arguments) ""))))
  (setf (gethash tool-name amoebum::*tool-metadata*)
        (amoebum::make-tool-metadata
         :name tool-name
         :permission :auto
         :dangerous-p nil
         :category :general
         :timeout-seconds 30
         :parameter-specs
         (list (list :name 'path
                     :type 'string
                     :description "Path argument."
                     :required t
                     :default nil
                     :default-supplied-p nil))
         :defined-at (get-universal-time))))

(defun %i366-copy-hash-table (source)
  (let ((copy (make-hash-table :test (hash-table-test source))))
    (maphash (lambda (key value)
               (setf (gethash key copy) value))
             source)
    copy))

(test i366-interactive-missing-argument-prompts-and-retries
  (let ((original-toolset amoebum:*toolset*)
        (original-metadata (%i366-copy-hash-table amoebum::*tool-metadata*))
        (original-mode amoebum::*missing-tool-argument-recovery-mode*)
        (tool-name "i366-required-interactive")
        (config (amoebum.config:current-config))
        (original-project-root (amoebum.config:config-project-root
                                (amoebum.config:current-config))))
    (unwind-protect
        (let* ((toolset (pseudopod:make-toolset))
               (system-root (or (ignore-errors (truename (%amoebum-system-root)))
                                (%amoebum-system-root)))
               (path (namestring
                      (merge-pathnames #P".tmp-test-work/i366-interactive.txt"
                                       system-root)))
              (input (make-string-input-stream
                      (format nil "~A~%" path)))
              (output (make-string-output-stream)))
          (setf (amoebum.config:config-project-root config) system-root)
          (setf amoebum:*toolset* toolset
                amoebum::*tool-metadata* (make-hash-table :test #'equal)
                amoebum::*missing-tool-argument-recovery-mode* :prompt)
          (%i366-register-required-tool toolset tool-name)
          (let* ((*query-io* (make-two-way-stream input output))
                 (context (amoebum:make-amoebum-context
                           :toolset toolset
                           :permission-mode :full-auto
                           :initialize-notifications-p nil))
                 (tool-call (pseudopod:make-tool-call
                             :id "i366-interactive-call"
                             :name tool-name
                             :arguments "{}"))
                 (result (amoebum:execute-tool tool-call context))
                 (prompt-text (get-output-stream-string output)))
            (is (string= result (format nil "path=~A" path)))
            (is-true (search "missing required argument" prompt-text :test #'char-equal))
            (is-true (search "Enter value for path" prompt-text :test #'char-equal))))
      (setf amoebum:*toolset* original-toolset
            amoebum::*tool-metadata* original-metadata
            amoebum::*missing-tool-argument-recovery-mode* original-mode
            (amoebum.config:config-project-root config) original-project-root))))

(test i366-headless-missing-argument-returns-structured-error
  (let ((original-toolset amoebum:*toolset*)
        (original-metadata (%i366-copy-hash-table amoebum::*tool-metadata*))
        (original-mode amoebum::*missing-tool-argument-recovery-mode*)
        (tool-name "i366-required-headless"))
    (unwind-protect
        (let ((toolset (pseudopod:make-toolset)))
          (setf amoebum:*toolset* toolset
                amoebum::*tool-metadata* (make-hash-table :test #'equal)
                amoebum::*missing-tool-argument-recovery-mode* :structured-error)
          (%i366-register-required-tool toolset tool-name)
          (let* ((empty-input (make-string-input-stream ""))
                 (empty-output (make-string-output-stream))
                 (*query-io* (make-two-way-stream empty-input empty-output))
                 (context (amoebum:make-amoebum-context
                           :toolset toolset
                           :permission-mode :full-auto
                           :initialize-notifications-p nil))
                 (tool-call (pseudopod:make-tool-call
                             :id "i366-headless-call"
                             :name tool-name
                             :arguments "{}"))
                 (result (amoebum:execute-tool tool-call context))
                 (payload (%i366-parse-json result)))
            (is (string= (gethash "kind" payload) "tool_error"))
            (is (string= (gethash "error_type" payload) "missing_tool_argument"))
            (is (string= (gethash "tool" payload) tool-name))
            (is (string= (gethash "argument" payload) "path"))
            (is (string= (gethash "reason_code" payload) "missing_required_argument"))))
      (setf amoebum:*toolset* original-toolset
            amoebum::*tool-metadata* original-metadata
            amoebum::*missing-tool-argument-recovery-mode* original-mode))))
