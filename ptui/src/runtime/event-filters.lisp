(defpackage :ptui.runtime.event-filters
  (:use :cl)
  (:export #:event-filter
           #:event-filter-p
           #:filter-matches-p
           #:filter-compile
           #:coerce-filter-function
           #:filter-by-predicate
           #:filter-by-type
           #:filter-by-severity
           #:filter-by-source
           #:filter-and
           #:filter-or
           #:filter-not))

(in-package :ptui.runtime.event-filters)

(defclass event-filter ()
  ((compiled-fn
    :initform nil
    :accessor %event-filter-compiled-fn)))

(defclass constant-filter (event-filter)
  ((value
    :initarg :value
    :reader constant-filter-value
    :type boolean)))

(defclass predicate-filter (event-filter)
  ((fn
    :initarg :fn
    :reader predicate-filter-fn
    :type function)))

(defclass type-filter (event-filter)
  ((types
    :initarg :types
    :reader type-filter-types
    :type list)))

(defclass severity-filter (event-filter)
  ((severities
    :initarg :severities
    :reader severity-filter-severities
    :type list)))

(defclass source-filter (event-filter)
  ((sources
    :initarg :sources
    :reader source-filter-sources
    :type list)))

(defclass and-filter (event-filter)
  ((children
    :initarg :children
    :reader and-filter-children
    :type list)))

(defclass or-filter (event-filter)
  ((children
    :initarg :children
    :reader or-filter-children
    :type list)))

(defclass not-filter (event-filter)
  ((child
    :initarg :child
    :reader not-filter-child
    :type event-filter)))

(defparameter +accept-all-filter+ (make-instance 'constant-filter :value t))
(defparameter +reject-all-filter+ (make-instance 'constant-filter :value nil))

(defun event-filter-p (value)
  (typep value 'event-filter))

(defgeneric filter-matches-p (filter event))
(defgeneric filter-compile (filter))

(defmethod filter-matches-p ((filter event-filter) event)
  (funcall (filter-compile filter) event))

(defmethod filter-compile ((filter event-filter))
  (or (%event-filter-compiled-fn filter)
      (setf (%event-filter-compiled-fn filter)
            (lambda (event)
              (filter-matches-p filter event)))))

(defmethod filter-matches-p ((filter constant-filter) event)
  (declare (ignore event))
  (constant-filter-value filter))

(defmethod filter-compile ((filter constant-filter))
  (if (constant-filter-value filter)
      (lambda (event)
        (declare (ignore event))
        t)
      (lambda (event)
        (declare (ignore event))
        nil)))

(defmethod filter-matches-p ((filter predicate-filter) event)
  (funcall (predicate-filter-fn filter) event))

(defmethod filter-compile ((filter predicate-filter))
  (predicate-filter-fn filter))

(defun %normalize-keyword (value name)
  (cond
    ((keywordp value) value)
    ((symbolp value)
     (intern (symbol-name value) :keyword))
    ((stringp value)
     (intern (string-upcase value) :keyword))
    ((characterp value)
     (intern (string-upcase (string value)) :keyword))
    (t
     (error "~A must be a keyword designator, got ~S." name value))))

(defun %normalize-filter-values (value name)
  (cond
    ((null value) '())
    ((and (listp value) (not (null value)))
     (remove-duplicates (mapcar (lambda (item) (%normalize-keyword item name)) value)
                        :test #'eq))
    (t
     (list (%normalize-keyword value name)))))

(defun %try-event-accessor (event accessor-name)
  (dolist (package-name '("AMOEBUM" "PTUI.RUNTIME.EVENT-BUS") nil)
    (let* ((pkg (find-package package-name))
           (sym (and pkg (find-symbol accessor-name pkg))))
      (when (and sym (fboundp sym))
        (let ((value (ignore-errors (funcall (symbol-function sym) event))))
          (when value
            (return value)))))))

(defun %event-field (event key)
  (labels ((string-key ()
             (string-downcase (symbol-name key))))
    (or (when (hash-table-p event)
          (or (gethash key event)
              (gethash (string-key) event)))
        (when (and (listp event)
                   (or (null event) (keywordp (car event))))
          (or (getf event key)
              (getf event (intern (symbol-name key) :keyword))))
        (when (and (eq key :type)
                   (or (keywordp event) (symbolp event)))
          event)
        (case key
          (:type (%try-event-accessor event "EVENT-TYPE"))
          (:severity (%try-event-accessor event "EVENT-SEVERITY"))
          (:source (%try-event-accessor event "EVENT-SOURCE"))
          (t nil)))))

(defun %event-type (event)
  (let ((value (%event-field event :type)))
    (and value (%normalize-keyword value "EVENT TYPE"))))

(defun %event-severity (event)
  (let ((value (%event-field event :severity)))
    (and value (%normalize-keyword value "EVENT SEVERITY"))))

(defun %event-source (event)
  (let ((value (%event-field event :source)))
    (and value (%normalize-keyword value "EVENT SOURCE"))))

(defmethod filter-matches-p ((filter type-filter) event)
  (member (%event-type event) (type-filter-types filter) :test #'eq))

(defmethod filter-compile ((filter type-filter))
  (let ((types (coerce (type-filter-types filter) 'vector)))
    (lambda (event)
      (let ((event-type (%event-type event)))
        (loop for type across types
              thereis (eq type event-type))))))

(defmethod filter-matches-p ((filter severity-filter) event)
  (member (%event-severity event) (severity-filter-severities filter) :test #'eq))

(defmethod filter-compile ((filter severity-filter))
  (let ((severities (coerce (severity-filter-severities filter) 'vector)))
    (lambda (event)
      (let ((event-severity (%event-severity event)))
        (loop for severity across severities
              thereis (eq severity event-severity))))))

(defmethod filter-matches-p ((filter source-filter) event)
  (member (%event-source event) (source-filter-sources filter) :test #'eq))

(defmethod filter-compile ((filter source-filter))
  (let ((sources (coerce (source-filter-sources filter) 'vector)))
    (lambda (event)
      (let ((event-source (%event-source event)))
        (loop for source across sources
              thereis (eq source event-source))))))

(defmethod filter-matches-p ((filter and-filter) event)
  (every (lambda (child)
           (filter-matches-p child event))
         (and-filter-children filter)))

(defmethod filter-compile ((filter and-filter))
  (let ((compiled-children (mapcar #'filter-compile (and-filter-children filter))))
    (lambda (event)
      (loop for child in compiled-children
            always (funcall child event)))))

(defmethod filter-matches-p ((filter or-filter) event)
  (some (lambda (child)
          (filter-matches-p child event))
        (or-filter-children filter)))

(defmethod filter-compile ((filter or-filter))
  (let ((compiled-children (mapcar #'filter-compile (or-filter-children filter))))
    (lambda (event)
      (loop for child in compiled-children
            thereis (funcall child event)))))

(defmethod filter-matches-p ((filter not-filter) event)
  (not (filter-matches-p (not-filter-child filter) event)))

(defmethod filter-compile ((filter not-filter))
  (let ((compiled-child (filter-compile (not-filter-child filter))))
    (lambda (event)
      (not (funcall compiled-child event)))))

(defun %coerce-filter-object (filter name)
  (cond
    ((null filter) +accept-all-filter+)
    ((event-filter-p filter) filter)
    ((functionp filter) (make-instance 'predicate-filter :fn filter))
    (t (error "~A must be an event filter, function, or NIL, got ~S." name filter))))

(defun coerce-filter-function (filter)
  (cond
    ((null filter) nil)
    ((functionp filter) filter)
    ((event-filter-p filter) (filter-compile filter))
    (t (error "FILTER must be a function, event filter, or NIL, got ~S." filter))))

(defun filter-by-predicate (fn)
  (unless (functionp fn)
    (error "FN must be a function, got ~S." fn))
  (make-instance 'predicate-filter :fn fn))

(defun filter-by-type (event-type)
  (let ((types (%normalize-filter-values event-type "EVENT-TYPE")))
    (cond
      ((null types)
       +reject-all-filter+)
      ((member :* types :test #'eq)
       +accept-all-filter+)
      (t
       (make-instance 'type-filter :types types)))))

(defun filter-by-severity (severity)
  (let ((severities (%normalize-filter-values severity "SEVERITY")))
    (if (null severities)
        +reject-all-filter+
        (make-instance 'severity-filter :severities severities))))

(defun filter-by-source (source)
  (let ((sources (%normalize-filter-values source "SOURCE")))
    (if (null sources)
        +reject-all-filter+
        (make-instance 'source-filter :sources sources))))

(defun filter-and (&rest filters)
  (let ((children '()))
    (dolist (raw filters)
      (let ((filter (%coerce-filter-object raw "FILTER")))
        (cond
          ((eq filter +reject-all-filter+)
           (return-from filter-and +reject-all-filter+))
          ((eq filter +accept-all-filter+)
           nil)
          (t
           (push filter children)))))
    (cond
      ((null children) +accept-all-filter+)
      ((null (cdr children)) (car children))
      (t (make-instance 'and-filter :children (nreverse children))))))

(defun filter-or (&rest filters)
  (let ((children '()))
    (dolist (raw filters)
      (let ((filter (%coerce-filter-object raw "FILTER")))
        (cond
          ((eq filter +accept-all-filter+)
           (return-from filter-or +accept-all-filter+))
          ((eq filter +reject-all-filter+)
           nil)
          (t
           (push filter children)))))
    (cond
      ((null children) +reject-all-filter+)
      ((null (cdr children)) (car children))
      (t (make-instance 'or-filter :children (nreverse children))))))

(defun filter-not (filter)
  (let ((child (%coerce-filter-object filter "FILTER")))
    (cond
      ((eq child +accept-all-filter+) +reject-all-filter+)
      ((eq child +reject-all-filter+) +accept-all-filter+)
      (t (make-instance 'not-filter :child child)))))
