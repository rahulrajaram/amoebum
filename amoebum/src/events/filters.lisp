(in-package :amoebum)

(defparameter +filter-accept-all+
  (lambda (event)
    (declare (ignore event))
    t))

(defparameter +filter-reject-all+
  (lambda (event)
    (declare (ignore event))
    nil))

(defun %accept-all-filter-p (filter)
  (eq filter +filter-accept-all+))

(defun %reject-all-filter-p (filter)
  (eq filter +filter-reject-all+))

(defun %coerce-filter (filter name)
  (cond
    ((null filter) +filter-accept-all+)
    ((functionp filter) filter)
    (t (error "~A must be a function or NIL, got ~S." name filter))))

(defun %normalize-filter-values (value normalizer)
  (cond
    ((null value) '())
    ((and (listp value) (not (null value)))
     (remove-duplicates
      (mapcar normalizer value)
      :test #'equal))
    (t
     (list (funcall normalizer value)))))

(defun %normalize-permission-mode-spec (value)
  (%normalize-keyword value "PERMISSION-MODE"))

(defun %normalize-permission-mode-event (value)
  (and value
       (ignore-errors
        (%normalize-keyword value "PERMISSION-MODE"))))

(defun %normalize-tool-name (value)
  (when value
    (string-downcase
     (typecase value
       (string value)
       (symbol (symbol-name value))
       (character (string value))
       (t (return-from %normalize-tool-name nil))))))

(defun filter-by-type (type)
  (let ((types (%normalize-filter-values type #'%normalize-event-type)))
    (cond
      ((null types)
       +filter-reject-all+)
      ((member :* types :test #'eq)
       +filter-accept-all+)
      ((null (cdr types))
       (let ((expected (car types)))
         (lambda (event)
           (eq (event-type event) expected))))
      (t
       (let ((allowed types))
         (lambda (event)
           (member (event-type event) allowed :test #'eq)))))))

(define-compiler-macro filter-by-type (&whole whole type &environment env)
  (if (constantp type env)
      (let ((types (%normalize-filter-values (eval type) #'%normalize-event-type)))
        (cond
          ((null types)
           '+filter-reject-all+)
          ((member :* types :test #'eq)
           '+filter-accept-all+)
          ((null (cdr types))
           `(lambda (event)
              (eq (event-type event) ',(car types))))
          (t
           `(lambda (event)
              (member (event-type event) ',types :test #'eq)))))
      whole))

(defun filter-by-severity (severity)
  (let ((allowed (%normalize-filter-values
                  severity
                  (lambda (value)
                    (%normalize-keyword value "SEVERITY")))))
    (cond
      ((null allowed)
       +filter-reject-all+)
      ((null (cdr allowed))
       (let ((expected (car allowed)))
         (lambda (event)
           (eq (event-severity event) expected))))
      (t
       (let ((choices allowed))
         (lambda (event)
           (member (event-severity event) choices :test #'eq)))))))

(defun filter-by-source (source)
  (let ((allowed (%normalize-filter-values
                  source
                  (lambda (value)
                    (%normalize-keyword value "SOURCE")))))
    (cond
      ((null allowed)
       +filter-reject-all+)
      ((null (cdr allowed))
       (let ((expected (car allowed)))
         (lambda (event)
           (eq (event-source event) expected))))
      (t
       (let ((choices allowed))
         (lambda (event)
           (member (event-source event) choices :test #'eq)))))))

(defun %payload-tool-name (payload)
  (cond
    ((listp payload)
     (or (getf payload :tool-name)
         (getf payload :tool)))
    ((hash-table-p payload)
     (or (gethash :tool-name payload)
         (gethash "tool-name" payload)
         (gethash :tool payload)
         (gethash "tool" payload)))
    (t
     nil)))

(defun %event-tool-name (event)
  (let ((payload (event-payload event)))
    (or (typecase payload
          (tool-invoked-payload
           (tool-invoked-payload-tool-name payload))
          (tool-completed-payload
           (tool-completed-payload-tool-name payload))
          (tool-error-payload
           (tool-error-payload-tool-name payload))
          (tool-call-started-payload
           (tool-call-started-payload-tool-name payload))
          (tool-call-argument-complete-payload
           (tool-call-argument-complete-payload-tool-name payload))
          (permission-prompted-payload
           (permission-prompted-payload-tool-name payload))
          (t nil))
        (%payload-tool-name payload))))

(defun %payload-permission-mode (payload)
  (cond
    ((listp payload)
     (or (getf payload :permission-mode)
         (getf payload :mode)))
    ((hash-table-p payload)
     (or (gethash :permission-mode payload)
         (gethash "permission-mode" payload)
         (gethash :mode payload)
         (gethash "mode" payload)))
    (t
     nil)))

(defun %event-permission-mode (event)
  (let ((payload (event-payload event)))
    (or (typecase payload
          (tool-invoked-payload
           (tool-invoked-payload-permission-mode payload))
          (permission-prompted-payload
           (permission-prompted-payload-permission-mode payload))
          (t nil))
        (%payload-permission-mode payload))))

(defun filter-by-tool (tool-name)
  (let ((expected (%normalize-tool-name tool-name)))
    (if (null expected)
        +filter-reject-all+
        (lambda (event)
          (let ((candidate (%normalize-tool-name (%event-tool-name event))))
            (and candidate
                 (string= candidate expected)))))))

(defun filter-by-permission-mode (permission-mode)
  (let ((allowed (%normalize-filter-values permission-mode #'%normalize-permission-mode-spec)))
    (cond
      ((null allowed)
       +filter-reject-all+)
      ((null (cdr allowed))
       (let ((expected (car allowed)))
         (lambda (event)
           (eq (%normalize-permission-mode-event (%event-permission-mode event))
               expected))))
      (t
       (let ((choices allowed))
         (lambda (event)
           (member (%normalize-permission-mode-event (%event-permission-mode event))
                   choices
                   :test #'eq)))))))

(defun filter-and (&rest filters)
  (let ((active '()))
    (dolist (raw filters)
      (let ((filter (%coerce-filter raw "FILTER")))
        (cond
          ((%reject-all-filter-p filter)
           (return-from filter-and +filter-reject-all+))
          ((%accept-all-filter-p filter)
           nil)
          (t
           (push filter active)))))
    (cond
      ((null active)
       +filter-accept-all+)
      ((null (cdr active))
       (car active))
      (t
       (let ((predicates (coerce (nreverse active) 'vector)))
         (lambda (event)
           (loop for predicate across predicates
                 always (funcall predicate event))))))))

(defun filter-or (&rest filters)
  (let ((active '()))
    (dolist (raw filters)
      (let ((filter (%coerce-filter raw "FILTER")))
        (cond
          ((%accept-all-filter-p filter)
           (return-from filter-or +filter-accept-all+))
          ((%reject-all-filter-p filter)
           nil)
          (t
           (push filter active)))))
    (cond
      ((null active)
       +filter-reject-all+)
      ((null (cdr active))
       (car active))
      (t
       (let ((predicates (coerce (nreverse active) 'vector)))
         (lambda (event)
           (loop for predicate across predicates
                 thereis (funcall predicate event))))))))

(defun filter-not (filter)
  (let ((predicate (%coerce-filter filter "FILTER")))
    (cond
      ((%accept-all-filter-p predicate)
       +filter-reject-all+)
      ((%reject-all-filter-p predicate)
       +filter-accept-all+)
      (t
       (lambda (event)
         (not (funcall predicate event)))))))

(defun %router-handler-form (handler)
  (if (and (symbolp handler)
           (not (keywordp handler)))
      `#',handler
      handler))

(defun %router-check-form-for-key (key value event-sym env)
  (labels ((constant-check (converter probe &key wildcard-p)
             (when (constantp value env)
               (let ((choices (%normalize-filter-values (eval value) converter)))
                 (cond
                   ((null choices) nil)
                   ((and wildcard-p (member :* choices :test #'eq)) t)
                   ((null (cdr choices))
                    `(eq ,probe ',(car choices)))
                   (t
                    `(member ,probe ',choices :test #'eq)))))))
    (cond
      ((eq key :type)
       (or (constant-check #'%normalize-event-type
                           `(event-type ,event-sym)
                           :wildcard-p t)
           `(funcall (filter-by-type ,value) ,event-sym)))
      ((eq key :severity)
       (or (constant-check (lambda (item)
                             (%normalize-keyword item "SEVERITY"))
                           `(event-severity ,event-sym))
           `(funcall (filter-by-severity ,value) ,event-sym)))
      ((eq key :source)
       (or (constant-check (lambda (item)
                             (%normalize-keyword item "SOURCE"))
                           `(event-source ,event-sym))
           `(funcall (filter-by-source ,value) ,event-sym)))
      ((eq key :tool)
       (if (constantp value env)
           (let ((choices (%normalize-filter-values (eval value) #'%normalize-tool-name)))
             (cond
               ((null choices) nil)
               ((null (cdr choices))
                `(let ((tool-name (%normalize-tool-name (%event-tool-name ,event-sym))))
                   (and tool-name
                        (string= tool-name ,(car choices)))))
               (t
                `(let ((tool-name (%normalize-tool-name (%event-tool-name ,event-sym))))
                   (and tool-name
                        (member tool-name ',choices :test #'string=))))))
           `(funcall (filter-by-tool ,value) ,event-sym)))
      ((eq key :permission-mode)
       (if (constantp value env)
           (let ((choices (%normalize-filter-values (eval value) #'%normalize-permission-mode-spec)))
             (cond
               ((null choices) nil)
               ((null (cdr choices))
                `(eq (%normalize-permission-mode-event (%event-permission-mode ,event-sym))
                     ',(car choices)))
               (t
                `(member (%normalize-permission-mode-event (%event-permission-mode ,event-sym))
                         ',choices
                         :test #'eq))))
           `(funcall (filter-by-permission-mode ,value) ,event-sym)))
      (t
       (error "Unsupported event-router predicate key ~S." key)))))

(defun %router-pattern-check-form (pattern event-sym env)
  (cond
    ((eq pattern t) t)
    ((null pattern) nil)
    ((not (listp pattern))
     (error "Router clause pattern must be a property list or T, got ~S." pattern))
    ((oddp (length pattern))
     (error "Router clause pattern must have an even number of elements, got ~S." pattern))
    (t
     (let ((checks '()))
       (loop for (key value) on pattern by #'cddr
             do (push (%router-check-form-for-key key value event-sym env) checks))
       (cond
         ((null checks) t)
         ((null (cdr checks)) (car checks))
         (t `(and ,@(nreverse checks))))))))

(defun %parse-event-router-args (args)
  (let ((name nil)
        (clauses args))
    (when (and (consp clauses) (eq (car clauses) :name))
      (unless (and (consp (cdr clauses)) (symbolp (cadr clauses)))
        (error "EVENT-ROUTER :NAME must be followed by a symbol."))
      (setf name (cadr clauses)
            clauses (cddr clauses)))
    (unless clauses
      (error "EVENT-ROUTER requires at least one dispatch clause."))
    (values name clauses)))

(defmacro event-router (&rest args &environment env)
  (multiple-value-bind (name clauses)
      (%parse-event-router-args args)
    (let ((event-sym (gensym "EVENT-")))
      (labels ((emit-clause (clause)
                 (unless (and (listp clause) (= (length clause) 2))
                   (error "Router clause must be ((pattern) handler), got ~S." clause))
                 (destructuring-bind (pattern handler) clause
                   (let ((check (%router-pattern-check-form pattern event-sym env))
                         (handler-form (%router-handler-form handler)))
                     `(,check (funcall ,handler-form ,event-sym))))))
        (let ((router-form `(lambda (,event-sym)
                              (cond
                                ,@(mapcar #'emit-clause clauses)
                                (t nil)))))
          (if name
              `(progn
                 (setf (fdefinition ',name) ,router-form)
                 #',name)
              router-form))))))
