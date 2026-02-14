(in-package :pseudopod)

(defstruct (model-info (:constructor %make-model-info))
  (id "" :type string)
  (object "model" :type string)
  (created-at 0 :type integer)
  (owned-by nil :type (or null string))
  (permission nil)
  (extras nil))

(defun %model-info-integer (value)
  (cond
    ((integerp value) value)
    ((and (stringp value)
          (plusp (length value))
          (every #'digit-char-p value))
     (parse-integer value))
    (t 0)))

(defun %copy-model-info-extras (hash)
  (let ((extras (make-hash-table :test #'equal)))
    (maphash (lambda (key value)
               (unless (member key
                               '("id" "object" "created" "created_at"
                                 "owned_by" "permission")
                               :test #'string=)
                 (setf (gethash key extras) value)))
             hash)
    (when (> (hash-table-count extras) 0)
      extras)))

(defun hash-to-model-info (hash)
  "Deserialize MODEL object hash from Moonshot/OpenAI-style Models API."
  (unless (hash-table-p hash)
    (error "Expected model info hash-table, got ~S" hash))
  (%make-model-info
   :id (or (and (stringp (gethash "id" hash))
                (gethash "id" hash))
           "")
   :object (or (and (stringp (gethash "object" hash))
                    (gethash "object" hash))
               "model")
   :created-at (%model-info-integer
                (or (gethash "created" hash)
                    (gethash "created_at" hash)))
   :owned-by (and (stringp (gethash "owned_by" hash))
                  (gethash "owned_by" hash))
   :permission (gethash "permission" hash)
   :extras (%copy-model-info-extras hash)))
