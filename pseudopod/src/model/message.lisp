(in-package :pseudopod)

(defparameter +message-roles+
  '("system" "user" "assistant" "tool"))

(defstruct (content-part (:constructor %make-content-part))
  (type "text" :type string)
  (text nil :type (or null string))
  (think nil :type (or null string))
  (encrypted nil :type (or null string))
  (payload nil))

(defstruct (tool-call (:constructor %make-tool-call))
  (id nil :type (or null string))
  (name "" :type string)
  (arguments nil :type (or null string))
  (extras nil))

(defstruct (message (:constructor %make-message))
  (role "user" :type string)
  (name nil :type (or null string))
  (content nil :type list)
  (tool-calls nil :type list)
  (tool-call-id nil :type (or null string))
  (partial nil))

(defun %copy-hash-table (table)
  (let ((copy (make-hash-table :test #'equal)))
    (when (hash-table-p table)
      (maphash (lambda (key value)
                 (setf (gethash key copy) value))
               table))
    copy))

(defun %sequence->list-safe (sequence)
  (cond
    ((null sequence) nil)
    ((listp sequence) sequence)
    ((vectorp sequence)
     (loop for item across sequence collect item))
    (t
     (error "Expected a list or vector, got ~S" sequence))))

(defun %normalize-role (role)
  (let ((normalized
          (string-downcase
           (cond
             ((stringp role) role)
             ((symbolp role) (symbol-name role))
             (t (error "Unsupported message role: ~S" role))))))
    (unless (member normalized +message-roles+ :test #'string=)
      (error "Unsupported message role: ~S" role))
    normalized))

(defun %normalize-content-part-type (type)
  (string-downcase
   (cond
     ((stringp type) type)
     ((symbolp type) (symbol-name type))
     (t (error "Unsupported content-part type: ~S" type)))))

(defun make-text-part (text)
  (%make-content-part :type "text" :text text))

(defun make-think-part (think &key encrypted)
  (%make-content-part :type "think" :think think :encrypted encrypted))

(defun make-tool-call (&key id name arguments extras)
  (%make-tool-call :id id
                   :name (or name "")
                   :arguments arguments
                   :extras extras))

(defun hash-to-content-part (hash)
  (unless (hash-table-p hash)
    (error "Expected content part hash-table, got ~S" hash))
  (let ((type (%normalize-content-part-type
               (if (gethash "type" hash)
                   (gethash "type" hash)
                   "text"))))
    (cond
      ((string= type "text")
       (%make-content-part :type "text"
                           :text (or (and (stringp (gethash "text" hash))
                                           (gethash "text" hash))
                                     (and (stringp (gethash "content" hash))
                                          (gethash "content" hash)))))
      ((string= type "think")
       (%make-content-part :type "think"
                           :think (and (stringp (gethash "think" hash))
                                       (gethash "think" hash))
                           :encrypted (and (stringp (gethash "encrypted" hash))
                                           (gethash "encrypted" hash))))
      (t
       (%make-content-part :type type
                           :text (and (stringp (gethash "text" hash))
                                      (gethash "text" hash))
                           :think (and (stringp (gethash "think" hash))
                                       (gethash "think" hash))
                           :encrypted (and (stringp (gethash "encrypted" hash))
                                           (gethash "encrypted" hash))
                           :payload (%copy-hash-table hash))))))

(defun %coerce-content-part (value)
  (cond
    ((content-part-p value) value)
    ((stringp value) (make-text-part value))
    ((hash-table-p value) (hash-to-content-part value))
    (t (error "Unsupported content part value: ~S" value))))

(defun content-part-to-hash (part)
  (let* ((content-part (%coerce-content-part part))
         (type (%normalize-content-part-type (content-part-type content-part)))
         (hash (or (and (hash-table-p (content-part-payload content-part))
                        (%copy-hash-table (content-part-payload content-part)))
                   (make-hash-table :test #'equal))))
    (setf (gethash "type" hash) type)
    (when (content-part-text content-part)
      (setf (gethash "text" hash) (content-part-text content-part)))
    (when (content-part-think content-part)
      (setf (gethash "think" hash) (content-part-think content-part)))
    (when (content-part-encrypted content-part)
      (setf (gethash "encrypted" hash) (content-part-encrypted content-part)))
    hash))

(defun hash-to-tool-call (hash)
  (unless (hash-table-p hash)
    (error "Expected tool call hash-table, got ~S" hash))
  (let* ((function-body (and (hash-table-p (gethash "function" hash))
                             (gethash "function" hash)))
         (name (or (and function-body (gethash "name" function-body))
                   (gethash "name" hash)))
         (arguments (or (and function-body (gethash "arguments" function-body))
                        (gethash "arguments" hash))))
    (%make-tool-call
     :id (and (stringp (gethash "id" hash)) (gethash "id" hash))
     :name (if (stringp name) name "")
     :arguments (and (stringp arguments) arguments)
     :extras (let ((extras (gethash "extras" hash)))
               (and (hash-table-p extras) (%copy-hash-table extras))))))

(defun %coerce-tool-call (tool-call)
  (cond
    ((tool-call-p tool-call) tool-call)
    ((hash-table-p tool-call) (hash-to-tool-call tool-call))
    (t (error "Unsupported tool-call value: ~S" tool-call))))

(defun tool-call-to-hash (tool-call)
  (let* ((normalized (%coerce-tool-call tool-call))
         (hash (make-hash-table :test #'equal))
         (function-body (make-hash-table :test #'equal)))
    (setf (gethash "type" hash) "function")
    (when (tool-call-id normalized)
      (setf (gethash "id" hash) (tool-call-id normalized)))
    (setf (gethash "name" function-body) (tool-call-name normalized))
    (when (tool-call-arguments normalized)
      (setf (gethash "arguments" function-body) (tool-call-arguments normalized)))
    (setf (gethash "function" hash) function-body)
    (when (hash-table-p (tool-call-extras normalized))
      (setf (gethash "extras" hash) (%copy-hash-table (tool-call-extras normalized))))
    hash))

(defun %coerce-content (content)
  (cond
    ((null content) nil)
    ((stringp content) (list (make-text-part content)))
    ((content-part-p content) (list content))
    ((hash-table-p content) (list (hash-to-content-part content)))
    (t (mapcar #'%coerce-content-part (%sequence->list-safe content)))))

(defun %coerce-tool-calls (tool-calls)
  (cond
    ((null tool-calls) nil)
    ((tool-call-p tool-calls) (list tool-calls))
    ((hash-table-p tool-calls) (list (hash-to-tool-call tool-calls)))
    (t (mapcar #'%coerce-tool-call (%sequence->list-safe tool-calls)))))

(defun make-message (&key role content name tool-calls tool-call-id partial)
  (%make-message
   :role (%normalize-role (or role "user"))
   :name (and name (princ-to-string name))
   :content (%coerce-content content)
   :tool-calls (%coerce-tool-calls tool-calls)
   :tool-call-id (and tool-call-id (princ-to-string tool-call-id))
   :partial partial))

(defun %serialize-content (content)
  (cond
    ((null content)
     #())
    ((and (= (length content) 1)
          (string= (content-part-type (first content)) "text")
          (stringp (content-part-text (first content))))
     (content-part-text (first content)))
    (t
     (coerce (mapcar #'content-part-to-hash content) 'vector))))

(defun message-to-hash (message)
  (unless (message-p message)
    (error "Expected message struct, got ~S" message))
  (let ((hash (make-hash-table :test #'equal)))
    (setf (gethash "role" hash) (message-role message))
    (setf (gethash "content" hash) (%serialize-content (message-content message)))
    (when (message-name message)
      (setf (gethash "name" hash) (message-name message)))
    (when (message-tool-calls message)
      (setf (gethash "tool_calls" hash)
            (coerce (mapcar #'tool-call-to-hash (message-tool-calls message))
                    'vector)))
    (when (message-tool-call-id message)
      (setf (gethash "tool_call_id" hash) (message-tool-call-id message)))
    (when (message-partial message)
      (setf (gethash "partial" hash) (message-partial message)))
    hash))

(defun %deserialize-content (raw-content)
  (cond
    ((null raw-content) nil)
    ((stringp raw-content) (list (make-text-part raw-content)))
    ((content-part-p raw-content) (list raw-content))
    ((hash-table-p raw-content) (list (hash-to-content-part raw-content)))
    (t (mapcar #'%coerce-content-part (%sequence->list-safe raw-content)))))

(defun %deserialize-tool-calls (raw-tool-calls)
  (cond
    ((null raw-tool-calls) nil)
    ((tool-call-p raw-tool-calls) (list raw-tool-calls))
    ((hash-table-p raw-tool-calls) (list (hash-to-tool-call raw-tool-calls)))
    (t (mapcar #'%coerce-tool-call (%sequence->list-safe raw-tool-calls)))))

(defun hash-to-message (hash)
  (unless (hash-table-p hash)
    (error "Expected message hash-table, got ~S" hash))
  (make-message
   :role (or (gethash "role" hash) "user")
   :name (gethash "name" hash)
   :content (%deserialize-content (gethash "content" hash))
   :tool-calls (%deserialize-tool-calls (gethash "tool_calls" hash))
   :tool-call-id (gethash "tool_call_id" hash)
   :partial (gethash "partial" hash)))
