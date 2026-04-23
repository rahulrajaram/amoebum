(in-package :amoebum)

;;;; NXT-395: deftool coercion helpers.
;;;;
;;;; This module owns argument extraction and runtime coercion for deftool
;;;; parameter bindings. The `deftool` macro expands against these helpers, so
;;;; they must load before `deftool/expansion.lisp`.

(defun %extract-tool-argument (arguments key)
  (unless (hash-table-p arguments)
    (error "Tool arguments must be a hash-table, got ~S." arguments))
  (multiple-value-bind (value present-p) (gethash key arguments)
    (if present-p
        value
        +missing-tool-argument+)))

(defun %coerce-boolean (value parameter-name)
  (cond
    ((or (eq value t) (eq value nil)) value)
    ((stringp value)
     (let ((normalized (string-downcase (string-trim '(#\Space #\Tab #\Newline #\Return)
                                                     value))))
       (cond
         ((member normalized '("true" "t" "1" "yes") :test #'string=) t)
         ((member normalized '("false" "f" "0" "no" "nil" "") :test #'string=) nil)
         (t (error "Invalid boolean value for ~S: ~S" parameter-name value)))))
    ((numberp value) (not (zerop value)))
    (t (if value t nil))))

(defun %coerce-member-value (value members)
  (cond
    ((member value members :test #'equal)
     value)
    ((stringp value)
     (or (find-if
          (lambda (candidate)
            (string= value (%json-enum-value candidate)))
          members)
         value))
    (t value)))

(defparameter *tool-argument-coercion-dispatch*
  (let ((table (make-hash-table :test #'eq)))
    (setf (gethash 'or table)
          (lambda (value type-spec parameter-name)
            (declare (ignore parameter-name))
            (if (null value)
                nil
                (let ((non-null-types (remove 'null (rest type-spec) :test #'equal)))
                  (if (= 1 (length non-null-types))
                      (%coerce-tool-argument value (first non-null-types) parameter-name)
                      value)))))
    (setf (gethash 'pathname table)
          (lambda (value type-spec parameter-name)
            (declare (ignore type-spec parameter-name))
            (typecase value
              (pathname value)
              (string (pathname value))
              (t value))))
    (setf (gethash 'integer table)
          (lambda (value type-spec parameter-name)
            (declare (ignore type-spec parameter-name))
            (typecase value
              (integer value)
              (string (parse-integer value))
              (t value))))
    (setf (gethash 'boolean table)
          (lambda (value type-spec parameter-name)
            (declare (ignore type-spec))
            (%coerce-boolean value parameter-name)))
    (setf (gethash 'member table)
          (lambda (value type-spec parameter-name)
            (declare (ignore parameter-name))
            (%coerce-member-value value (rest type-spec))))
    table))

(defun %tool-argument-coercion-key (type-spec)
  (if (consp type-spec)
      (first type-spec)
      type-spec))

(defun %coerce-tool-argument (value type-spec parameter-name)
  (if (eq value +missing-tool-argument+) value
      (let ((coercer (gethash (%tool-argument-coercion-key type-spec)
                              *tool-argument-coercion-dispatch*)))
        (if coercer
            (funcall coercer value type-spec parameter-name)
            value))))
