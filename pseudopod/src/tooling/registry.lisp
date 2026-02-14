(in-package :pseudopod)

#+sbcl (eval-when (:compile-toplevel :load-toplevel :execute)
         (require :sb-introspect))

(defstruct (tool-definition (:constructor %make-tool-definition))
  (name "" :type string)
  (description "" :type string)
  (parameters nil)
  (fn nil))

(defstruct (toolset (:constructor %make-toolset))
  (table (make-hash-table :test #'equal) :type hash-table))

(defun %normalize-tool-name (name)
  (let ((value (string-downcase
                (string-trim '(#\Space #\Tab #\Newline #\Return)
                             (cond
                               ((stringp name) name)
                               ((symbolp name) (symbol-name name))
                               (t (error "Unsupported tool name: ~S" name)))))))
    (if (plusp (length value))
        value
        (error "Tool name must not be empty."))))

(defun %copy-hash-table-shallow (table)
  (let ((copy (make-hash-table :test #'equal)))
    (when (hash-table-p table)
      (maphash (lambda (key value)
                 (setf (gethash key copy) value))
               table))
    copy))

(defun %default-tool-schema ()
  (let ((schema (make-hash-table :test #'equal)))
    (setf (gethash "type" schema) "object")
    (setf (gethash "properties" schema) (make-hash-table :test #'equal))
    schema))

(defun %coerce-tool-schema (parameters)
  (cond
    ((null parameters) (%default-tool-schema))
    ((hash-table-p parameters) (%copy-hash-table-shallow parameters))
    ((stringp parameters)
     (handler-case
         (let ((parsed (jonathan:parse parameters :as :hash-table)))
           (if (hash-table-p parsed)
               parsed
               (error "Tool schema string must decode to a JSON object.")))
       (error (condition)
         (error "Invalid tool schema JSON: ~A" condition))))
    (t
     (error "Unsupported tool schema value: ~S" parameters))))

(defun make-tool-definition (&key name description parameters fn)
  (%make-tool-definition
   :name (%normalize-tool-name name)
   :description (or description "")
   :parameters (%coerce-tool-schema parameters)
   :fn (or fn (error "Tool definition requires :fn for ~S" name))))

(defun tool-definition-to-hash (tool)
  (unless (tool-definition-p tool)
    (error "Expected tool-definition, got ~S" tool))
  (let ((hash (make-hash-table :test #'equal))
        (function-body (make-hash-table :test #'equal)))
    (setf (gethash "type" hash) "function")
    (setf (gethash "name" function-body) (tool-definition-name tool))
    (setf (gethash "description" function-body) (tool-definition-description tool))
    (setf (gethash "parameters" function-body)
          (%copy-hash-table-shallow (tool-definition-parameters tool)))
    (setf (gethash "function" hash) function-body)
    hash))

(defun make-toolset (&key tools)
  (let ((toolset (%make-toolset)))
    (dolist (tool tools toolset)
      (register-tool toolset tool))))

(defun register-tool (toolset tool)
  (unless (toolset-p toolset)
    (error "Expected toolset, got ~S" toolset))
  (unless (tool-definition-p tool)
    (error "Expected tool-definition, got ~S" tool))
  (setf (gethash (tool-definition-name tool) (toolset-table toolset)) tool)
  toolset)

(defun register-tool-function (toolset &key name description parameters fn)
  (register-tool toolset
                 (make-tool-definition :name name
                                       :description description
                                       :parameters parameters
                                       :fn fn)))

(defun find-tool (toolset name)
  (unless (toolset-p toolset)
    (error "Expected toolset, got ~S" toolset))
  (gethash (%normalize-tool-name name) (toolset-table toolset)))

(defun toolset-tools (toolset)
  (unless (toolset-p toolset)
    (error "Expected toolset, got ~S" toolset))
  (let ((result nil))
    (maphash (lambda (_name tool)
               (declare (ignore _name))
               (push tool result))
             (toolset-table toolset))
    (nreverse result)))

(defun %parse-tool-arguments (arguments)
  (cond
    ((null arguments) (make-hash-table :test #'equal))
    ((hash-table-p arguments) arguments)
    ((stringp arguments)
     (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) arguments)))
       (if (zerop (length trimmed))
           (make-hash-table :test #'equal)
           (handler-case
               (let ((parsed (jonathan:parse trimmed :as :hash-table)))
                 (if (hash-table-p parsed)
                     parsed
                     (error 'pseudopod-parse-error
                            :message "Tool call arguments must decode to a JSON object."
                            :payload trimmed)))
             (error (condition)
               (error 'pseudopod-parse-error
                      :message (format nil "Tool call arguments parse failed: ~A"
                                       condition)
                      :payload trimmed
                      :cause condition))))))
    (t
     (error 'pseudopod-parse-error
            :message (format nil "Unsupported tool argument payload: ~S" arguments)
            :payload arguments))))

(defun %normalize-tool-result (result)
  (cond
    ((null result) "")
    ((stringp result) result)
    (t (jonathan:to-json result))))

(defun %tool-fn-accepts-two-args-p (fn)
  "Check whether FN accepts at least two positional arguments.
Uses SBCL introspection when available, falls back to assuming 2-arity."
  #+sbcl
  (handler-case
      (let* ((lambda-list (sb-introspect:function-lambda-list fn))
             (positional-count
               (loop for item in lambda-list
                     while (not (member item lambda-list-keywords))
                     count t))
             (has-rest-or-key
               (intersection lambda-list '(&rest &key &allow-other-keys))))
        (or (>= positional-count 2) (not (null has-rest-or-key))))
    (error () t))
  #-sbcl t)

(defun invoke-tool-call (toolset tool-call)
  "Invoke a tool from TOOLSET matching TOOL-CALL.
Tool functions receive (arguments tool-call) if they accept 2+ args,
or just (arguments) for single-argument tools."
  (unless (toolset-p toolset)
    (error "Expected toolset, got ~S" toolset))
  (let* ((call (if (tool-call-p tool-call)
                   tool-call
                   (hash-to-tool-call tool-call)))
         (tool (find-tool toolset (tool-call-name call))))
    (unless tool
      (error 'pseudopod-error
             :message (format nil "No registered tool named ~S."
                              (tool-call-name call))))
    (let* ((arguments (%parse-tool-arguments (tool-call-arguments call)))
           (fn (tool-definition-fn tool))
           (result (if (%tool-fn-accepts-two-args-p fn)
                       (funcall fn arguments call)
                       (funcall fn arguments))))
      (%normalize-tool-result result))))
