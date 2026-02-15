(in-package :amoebum)

(defparameter +hook-point-definitions+
  '((:pre-tool-use
     :params (tool-name args)
     :blocking t
     :description "Runs before tool execution and can block by returning :deny.")
    (:post-tool-use
     :params (tool-name result elapsed-ms)
     :blocking nil
     :description "Runs after tool execution. Return values are informational.")
    (:pre-send
     :params (messages model)
     :blocking t
     :description "Runs before messages are sent to a model.")
    (:post-receive
     :params (response)
     :blocking nil
     :description "Runs after model responses are received.")
    (:on-error
     :params (condition restarts)
     :blocking t
     :description "Runs on conditions and may influence restart behavior.")
    (:on-idle
     :params ()
     :blocking nil
     :description "Runs while the assistant is idle.")
    (:on-commit
     :params (message branch)
     :blocking nil
     :description "Runs after commit operations.")
    (:on-step-complete
     :params (step-result)
     :blocking nil
     :description "Runs when a tranche/step completes.")))

(defparameter *hook-registry* (make-hash-table :test #'equal))
(defparameter *hook-registration-counter* 0)

(defstruct (hook-entry
            (:constructor %make-hook-entry
                (&key hook-point hook-id handler (priority 100)
                 async-p source-file source-line docstring
                 (registered-at 0))))
  hook-point
  hook-id
  handler
  (priority 100 :type integer)
  async-p
  source-file
  source-line
  docstring
  (registered-at 0 :type integer))

(defun %normalize-hook-point (hook-point)
  (cond
    ((keywordp hook-point) hook-point)
    ((symbolp hook-point)
     (intern (string-upcase (symbol-name hook-point)) :keyword))
    ((stringp hook-point)
     (intern (string-upcase hook-point) :keyword))
    (t
     (error "Unknown hook-point designator ~S." hook-point))))

(defun %hook-point-spec (hook-point)
  (assoc (%normalize-hook-point hook-point)
         +hook-point-definitions+
         :test #'eq))

(defun %ensure-hook-point-spec (hook-point)
  (or (%hook-point-spec hook-point)
      (error "Unknown hook-point ~S." hook-point)))

(defun %hook-point-blocking-p (hook-point)
  (let ((spec (%ensure-hook-point-spec hook-point)))
    (not (null (getf (cdr spec) :blocking)))))

(defun %hook-key (hook-point hook-id)
  (cons (%normalize-hook-point hook-point) hook-id))

(defun %next-hook-registration-order ()
  (incf *hook-registration-counter*))

(defun %hook-entries (&optional hook-point)
  (let ((normalized (and hook-point (%normalize-hook-point hook-point)))
        (entries '()))
    (maphash (lambda (key entry)
               (declare (ignore key))
               (when (or (null normalized)
                         (eq normalized (hook-entry-hook-point entry)))
                 (push entry entries)))
             *hook-registry*)
    entries))

(defun %sort-hook-entries (entries)
  (sort entries
        (lambda (left right)
          (if (= (hook-entry-priority left)
                 (hook-entry-priority right))
              (if (= (hook-entry-registered-at left)
                     (hook-entry-registered-at right))
                  (string< (princ-to-string (hook-entry-hook-id left))
                           (princ-to-string (hook-entry-hook-id right)))
                  (< (hook-entry-registered-at left)
                     (hook-entry-registered-at right)))
              (> (hook-entry-priority left)
                 (hook-entry-priority right))))))

(defun list-hooks (&optional hook-point)
  (%sort-hook-entries (%hook-entries hook-point)))

(defun clear-hooks (&optional hook-point)
  (if hook-point
      (let ((target (%normalize-hook-point hook-point))
            (removed 0)
            (keys-to-remove '()))
        (maphash (lambda (key entry)
                   (when (eq target (hook-entry-hook-point entry))
                     (incf removed)
                     (push key keys-to-remove)))
                 *hook-registry*)
        (dolist (key keys-to-remove)
          (remhash key *hook-registry*))
        removed)
      (let ((count (hash-table-count *hook-registry*)))
        (clrhash *hook-registry*)
        count)))

(defun register-hook (hook-point hook-id handler
                      &key (priority 100)
                        (async nil)
                        docstring
                        source-file
                        source-line)
  (unless (symbolp hook-id)
    (error "HOOK-ID must be a symbol, got ~S." hook-id))
  (unless (functionp handler)
    (error "HANDLER must be a function, got ~S." handler))
  (unless (integerp priority)
    (error "PRIORITY must be an integer, got ~S." priority))
  (let* ((normalized (%normalize-hook-point hook-point))
         (spec (%ensure-hook-point-spec normalized))
         (blocking (not (null (getf (cdr spec) :blocking))))
         (key (%hook-key normalized hook-id)))
    (when (and async blocking)
      (warn "Ignoring :async t for blocking hook-point ~S hook-id ~S."
            normalized
            hook-id)
      (setf async nil))
    (setf (gethash key *hook-registry*)
          (%make-hook-entry :hook-point normalized
                            :hook-id hook-id
                            :handler handler
                            :priority priority
                            :async-p (not (null async))
                            :source-file source-file
                            :source-line source-line
                            :docstring docstring
                            :registered-at (%next-hook-registration-order)))
    hook-id))

(defun unregister-hook (hook-point hook-id)
  (let ((key (%hook-key hook-point hook-id)))
    (prog1
        (not (null (gethash key *hook-registry*)))
      (remhash key *hook-registry*))))

(defun describe-hooks (&optional hook-point)
  (with-output-to-string (stream)
    (dolist (entry (list-hooks hook-point))
      (format stream "~S (~S): priority=~D async=~:[no~;yes~]"
              (hook-entry-hook-point entry)
              (hook-entry-hook-id entry)
              (hook-entry-priority entry)
              (hook-entry-async-p entry))
      (when (hook-entry-docstring entry)
        (format stream " doc=~S" (hook-entry-docstring entry)))
      (when (hook-entry-source-file entry)
        (format stream " source=~A" (hook-entry-source-file entry)))
      (when (hook-entry-source-line entry)
        (format stream ":~D" (hook-entry-source-line entry)))
      (terpri stream))))

(defun %arg-value (args key-name)
  (unless (hash-table-p args)
    (return-from %arg-value nil))
  (let* ((key-lower (string-downcase key-name))
         (key-upper (string-upcase key-name))
         (keyword-key (intern key-upper :keyword)))
    (loop for candidate in (list key-name key-lower key-upper keyword-key)
          do (multiple-value-bind (value present-p) (gethash candidate args)
               (when present-p
                 (return value))))))

(defun %dispatch-async (handler args)
  (let* ((thread-package (find-package :sb-thread))
         (make-thread-symbol (and thread-package
                                  (find-symbol "MAKE-THREAD" thread-package))))
    (if (and make-thread-symbol (fboundp make-thread-symbol))
        (funcall (symbol-function make-thread-symbol)
                 (lambda ()
                   (apply handler args))
                 :name "amoebum-hook")
        (apply handler args))))

(defun run-hooks (hook-point &rest args)
  (let* ((normalized (%normalize-hook-point hook-point))
         (blocking (%hook-point-blocking-p normalized))
         (results '()))
    (dolist (entry (list-hooks normalized)
             (values (if blocking :allow :completed)
                     (nreverse results)))
      (let ((hook-id (hook-entry-hook-id entry)))
        (if (hook-entry-async-p entry)
            (progn
              (%dispatch-async (hook-entry-handler entry) args)
              (push (cons hook-id :async-dispatched) results))
            (let ((result (apply (hook-entry-handler entry) args)))
              (push (cons hook-id result) results)
              (when (and blocking (eq result :deny))
                (return (values :deny (nreverse results))))))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %normalize-parameter-names (parameters)
    (mapcar (lambda (parameter)
              (unless (symbolp parameter)
                (error "Hook parameter names must be symbols, got ~S." parameter))
              (string-downcase (symbol-name parameter)))
            parameters))

  (defun %validate-hook-signature (hook-point parameters)
    (let* ((spec (%ensure-hook-point-spec hook-point))
           (expected (getf (cdr spec) :params))
           (actual-names (%normalize-parameter-names parameters))
           (expected-names (%normalize-parameter-names expected)))
      (unless (equal actual-names expected-names)
        (error "Hook-point ~S expects parameters ~S, got ~S."
               (%normalize-hook-point hook-point)
               expected
               parameters))
      t))

  (defun %parse-defhook-options-and-clauses (forms)
    (let ((docstring nil)
          (options (list :priority 100 :async nil))
          (remaining forms)
          (clauses '()))
      (when (and remaining (stringp (first remaining)))
        (setf docstring (first remaining)
              remaining (rest remaining)))
      (loop while (and remaining
                       (consp (first remaining))
                       (keywordp (first (first remaining)))
                       (member (first (first remaining))
                               '(:priority :async)
                               :test #'eq))
            do (destructuring-bind (option value &rest extra) (first remaining)
                 (declare (ignore extra))
                 (ecase option
                   (:priority
                    (unless (integerp value)
                      (error ":priority requires integer value, got ~S." value))
                    (setf (getf options :priority) value))
                   (:async
                    (setf (getf options :async) (not (null value)))))
                 (setf remaining (rest remaining))))
      (dolist (form remaining)
        (unless (and (consp form) (eq (first form) :match))
          (error "DEFHOOK body entries must be :match clauses, got ~S." form))
        (destructuring-bind (keyword pattern &body body) form
          (declare (ignore keyword))
          (when (null body)
            (error "DEFHOOK :match clause requires a body, got ~S." form))
          (push (list pattern body) clauses)))
      (when (null clauses)
        (error "DEFHOOK requires at least one :match clause."))
      (values docstring options (nreverse clauses))))

  (defun %glob-to-regex (glob-pattern)
    (with-output-to-string (stream)
      (write-char #\^ stream)
      (loop for character across glob-pattern
            do (case character
                 (#\* (write-string ".*" stream))
                 (#\? (write-char #\. stream))
                 ((#\. #\+ #\( #\) #\[ #\] #\{ #\} #\^ #\$ #\|)
                  (write-char #\\ stream)
                  (write-char character stream))
                 (t
                  (write-char character stream))))
      (write-char #\$ stream)))

  (defun %compile-scanner-expression (pattern)
    (handler-case
        (progn
          (cl-ppcre:create-scanner pattern)
          `(load-time-value (cl-ppcre:create-scanner ,pattern) t))
      (error (condition)
        (error "Invalid hook regex pattern ~S: ~A" pattern condition))))

  (defun %compile-args-predicate (args-var args-spec)
    (unless (listp args-spec)
      (error ":args matcher must be a plist, got ~S." args-spec))
    (unless (evenp (length args-spec))
      (error ":args matcher plist must have even length, got ~S." args-spec))
    (let ((tests '()))
      (loop for (key value) on args-spec by #'cddr
            do (case key
                 (:pattern
                  (unless (stringp value)
                    (error ":args :pattern must be a string, got ~S." value))
                  (let ((scanner (%compile-scanner-expression value)))
                    (push `(let ((arg (%arg-value ,args-var "command")))
                             (and (stringp arg)
                                  (cl-ppcre:scan ,scanner arg)))
                          tests)))
                 (:path
                  (unless (stringp value)
                    (error ":args :path must be a glob string, got ~S." value))
                  (let ((scanner (%compile-scanner-expression (%glob-to-regex value))))
                    (push `(let ((arg (%arg-value ,args-var "path")))
                             (and (stringp arg)
                                  (cl-ppcre:scan ,scanner arg)))
                          tests)))
                 (otherwise
                  (unless (symbolp key)
                    (error "Unsupported :args matcher key ~S." key))
                  (let ((key-name (string-downcase (symbol-name key))))
                    (push `(equal (%arg-value ,args-var ,key-name) ,value)
                          tests)))))
      (if tests
          `(and ,@(nreverse tests))
          t)))

  (defun %compile-match-predicate (pattern tool-var args-var)
    (cond
      ((eq pattern t)
       t)
      ((and (listp pattern) (evenp (length pattern)))
       (let ((tests '()))
         (loop for (key value) on pattern by #'cddr
               do (case key
                    (:tool
                     (let ((tool-name (string-downcase (princ-to-string value))))
                       (push `(string= (string-downcase (princ-to-string ,tool-var))
                                       ,tool-name)
                             tests)))
                    (:args
                     (push (%compile-args-predicate args-var value) tests))
                    (otherwise
                     (error "Unknown :match predicate key ~S in ~S." key pattern))))
         (if tests
             `(and ,@(nreverse tests))
             t)))
      (t
       (error "Unsupported :match predicate ~S." pattern))))

  (defun %deny-body-p (body)
    (and (= (length body) 1)
         (eq (first body) :deny))))

(defmacro defhook (hook-point parameters &body forms)
  (let* ((normalized-hook-point (%normalize-hook-point hook-point)))
    (%ensure-hook-point-spec normalized-hook-point)
    (%validate-hook-signature normalized-hook-point parameters)
    (multiple-value-bind (docstring options clauses)
        (%parse-defhook-options-and-clauses forms)
      (let* ((priority (getf options :priority))
             (async (getf options :async))
             (blocking (%hook-point-blocking-p normalized-hook-point))
             (handler-symbol (gensym
                              (format nil "HOOK-HANDLER-~A-"
                                      (symbol-name normalized-hook-point))))
             (hook-id (gensym (format nil "HOOK-~A-" (symbol-name normalized-hook-point))))
             (tool-var (or (first parameters) nil))
             (args-var (or (second parameters) nil))
             (compiled-clauses
               (mapcar (lambda (clause)
                         (destructuring-bind (pattern body) clause
                           `(,(%compile-match-predicate pattern tool-var args-var)
                             ,@body)))
                       clauses))
             (source-file (or *compile-file-truename* *load-truename*)))
        (when (and (not blocking)
                   (some (lambda (clause)
                           (%deny-body-p (second clause)))
                         clauses))
          (warn "Hook-point ~S is non-blocking; :deny return values are ignored."
                normalized-hook-point))
        `(progn
           (defun ,handler-symbol ,parameters
             ,@(when docstring (list docstring))
             (cond
               ,@compiled-clauses
               (t nil)))
           (register-hook ',normalized-hook-point
                          ',hook-id
                          #',handler-symbol
                          :priority ,priority
                          :async ,async
                          :docstring ,docstring
                          :source-file ,source-file
                          :source-line nil)
           ',hook-id)))))
