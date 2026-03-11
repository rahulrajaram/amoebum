(in-package :amoebum)

(define-condition defhook-definition-warning (style-warning)
  ((hook-point :initarg :hook-point
               :reader defhook-definition-warning-hook-point)
   (reason :initarg :reason
           :reader defhook-definition-warning-reason))
  (:report (lambda (condition stream)
             (format stream
                     "DEFHOOK ~S: ~A"
                     (defhook-definition-warning-hook-point condition)
                     (defhook-definition-warning-reason condition)))))


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
    (:pre-llm-send
     :params (messages tools model)
     :blocking t
     :description "Runs before LLM calls; may return updated messages or :block.")
    (:post-llm-receive
     :params (response usage model)
     :blocking nil
     :description "Runs after LLM calls for logging and metrics.")
    (:on-error
     :params (condition tool-name)
     :blocking t
     :description "Runs on conditions and may influence restart behavior.")
    (:on-idle
     :params (idle-seconds)
     :blocking nil
     :description "Runs while the assistant is idle.")
    (:on-commit
     :params (commit-hash message files)
     :blocking nil
     :description "Runs after commit operations.")
    (:on-step-complete
     :params (step-number messages-added tool-calls-made)
     :blocking nil
     :description "Runs when a tranche/step completes.")
    (:on-stream-chunk
     :params (chunk-text chunk-index total-tokens)
     :blocking nil
     :description "Runs on each streamed chunk; :block short-circuits hook-chain.")))

(defparameter *hook-registry* (make-hash-table :test #'equal))
(defparameter *hook-registration-counter* 0)
(defparameter *hook-active-stack* '())
(defparameter *hook-trace-limit* 256)
(defparameter *hook-trace-log* '())

(defparameter +default-hook-max-ms+ 1000)
(defparameter +default-hook-failure-threshold+ 3)
(defparameter +known-hook-on-error-policies+
  '(:log-and-continue :signal :disable-hook))

(defstruct (hook-entry
            (:constructor %make-hook-entry
                (&key hook-point hook-id handler (priority 0)
                 async-p source-file source-line docstring
                 (registered-at 0)
                 (max-ms +default-hook-max-ms+)
                 (on-error :log-and-continue)
                 (failure-threshold +default-hook-failure-threshold+)
                 (disabled-p nil)
                 (consecutive-failures 0)
                 (call-count 0)
                 (total-time-ms 0)
                 (failure-count 0)
                 (timeout-count 0)
                 (last-elapsed-ms 0)
                 (last-status :never)
                 (last-error nil))))
  hook-point
  hook-id
  handler
  (priority 0 :type integer)
  async-p
  source-file
  source-line
  docstring
  (registered-at 0 :type integer)
  (max-ms +default-hook-max-ms+ :type integer)
  (on-error :log-and-continue :type keyword)
  (failure-threshold +default-hook-failure-threshold+ :type integer)
  (disabled-p nil :type boolean)
  (consecutive-failures 0 :type integer)
  (call-count 0 :type integer)
  (total-time-ms 0 :type integer)
  (failure-count 0 :type integer)
  (timeout-count 0 :type integer)
  (last-elapsed-ms 0 :type integer)
  (last-status :never :type keyword)
  last-error)

;; Monotonic time delegated to monotonic-ms in util.lisp

(defun %ensure-known-hook-on-error-policy (policy)
  (unless (member policy +known-hook-on-error-policies+ :test #'eq)
    (error "Unknown hook :on-error policy ~S; expected one of ~S."
           policy
           +known-hook-on-error-policies+))
  policy)

(defun %trim-hook-trace-log ()
  (when (and (integerp *hook-trace-limit*)
             (> *hook-trace-limit* 0))
    (let ((current-length (length *hook-trace-log*)))
      (when (> current-length *hook-trace-limit*)
        (setf *hook-trace-log*
              (subseq *hook-trace-log* 0 *hook-trace-limit*))))))

(defun %record-hook-trace (hook-point hook-id status &key elapsed-ms result detail)
  (push (list :timestamp (monotonic-ms)
              :hook-point hook-point
              :hook-id hook-id
              :status status
              :elapsed-ms (or elapsed-ms 0)
              :result result
              :detail (and detail (princ-to-string detail)))
        *hook-trace-log*)
  (%trim-hook-trace-log)
  t)

(defun clear-hook-trace ()
  (setf *hook-trace-log* '())
  t)

(defun hook-trace (&key (limit 20) hook-point)
  (unless (and (integerp limit) (> limit 0))
    (error "LIMIT must be a positive integer, got ~S." limit))
  (let ((normalized (and hook-point (%normalize-hook-point hook-point)))
        (collected '()))
    (dolist (entry *hook-trace-log* (nreverse collected))
      (when (or (null normalized)
                (eq normalized (getf entry :hook-point)))
        (push entry collected)
        (when (>= (length collected) limit)
          (return (nreverse collected)))))))

(defun %entry-snapshot (entry)
  (list :hook-point (hook-entry-hook-point entry)
        :hook-id (hook-entry-hook-id entry)
        :priority (hook-entry-priority entry)
        :async-p (hook-entry-async-p entry)
        :enabled-p (not (hook-entry-disabled-p entry))
        :on-error (hook-entry-on-error entry)
        :max-ms (hook-entry-max-ms entry)
        :failure-threshold (hook-entry-failure-threshold entry)
        :consecutive-failures (hook-entry-consecutive-failures entry)
        :call-count (hook-entry-call-count entry)
        :total-time-ms (hook-entry-total-time-ms entry)
        :failure-count (hook-entry-failure-count entry)
        :timeout-count (hook-entry-timeout-count entry)
        :last-elapsed-ms (hook-entry-last-elapsed-ms entry)
        :last-status (hook-entry-last-status entry)
        :last-error (hook-entry-last-error entry)
        :source-file (hook-entry-source-file entry)
        :source-line (hook-entry-source-line entry)
        :docstring (hook-entry-docstring entry)))

(defun hook-metrics (&optional hook-point hook-id)
  (let ((entries
          (cond
            ((and hook-point hook-id)
             (let ((entry (gethash (%hook-key hook-point hook-id) *hook-registry*)))
               (if entry (list entry) '())))
            (hook-point
             (list-hooks hook-point))
            (t
             (list-hooks)))))
    (mapcar #'%entry-snapshot entries)))

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

(defun %sort-hook-entries-descending (entries)
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

(defun %sort-hook-entries-ascending (entries)
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
              (< (hook-entry-priority left)
                 (hook-entry-priority right))))))

(defun list-hooks (&optional hook-point)
  (%sort-hook-entries-descending (%hook-entries hook-point)))

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
                      &key (priority 0)
                        (async nil)
                        (max-ms +default-hook-max-ms+)
                        (on-error :log-and-continue)
                        (failure-threshold +default-hook-failure-threshold+)
                        docstring
                        source-file
                        source-line)
  (unless (symbolp hook-id)
    (error "HOOK-ID must be a symbol, got ~S." hook-id))
  (unless (functionp handler)
    (error "HANDLER must be a function, got ~S." handler))
  (unless (integerp priority)
    (error "PRIORITY must be an integer, got ~S." priority))
  (unless (and (integerp max-ms) (> max-ms 0))
    (error "MAX-MS must be a positive integer, got ~S." max-ms))
  (unless (and (integerp failure-threshold) (> failure-threshold 0))
    (error "FAILURE-THRESHOLD must be a positive integer, got ~S." failure-threshold))
  (%ensure-known-hook-on-error-policy on-error)
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
                            :max-ms max-ms
                            :on-error on-error
                            :failure-threshold failure-threshold
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
      (format stream "~S (~S): priority=~D async=~:[no~;yes~] enabled=~:[no~;yes~] on-error=~A budget=~Dms threshold=~D calls=~D failures=~D"
              (hook-entry-hook-point entry)
              (hook-entry-hook-id entry)
              (hook-entry-priority entry)
              (hook-entry-async-p entry)
              (not (hook-entry-disabled-p entry))
              (hook-entry-on-error entry)
              (hook-entry-max-ms entry)
              (hook-entry-failure-threshold entry)
              (hook-entry-call-count entry)
              (hook-entry-failure-count entry))
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

(defun %record-hook-success (entry elapsed-ms)
  (incf (hook-entry-call-count entry))
  (incf (hook-entry-total-time-ms entry) elapsed-ms)
  (setf (hook-entry-consecutive-failures entry) 0
        (hook-entry-last-elapsed-ms entry) elapsed-ms
        (hook-entry-last-status entry) :ok
        (hook-entry-last-error entry) nil))

(defun %record-hook-failure (entry status elapsed-ms condition)
  (incf (hook-entry-call-count entry))
  (incf (hook-entry-total-time-ms entry) elapsed-ms)
  (incf (hook-entry-failure-count entry))
  (when (eq status :timeout)
    (incf (hook-entry-timeout-count entry)))
  (incf (hook-entry-consecutive-failures entry))
  (setf (hook-entry-last-elapsed-ms entry) elapsed-ms
        (hook-entry-last-status entry)
        (if (eq status :timeout) :timeout :error)
        (hook-entry-last-error entry)
        (and condition (princ-to-string condition)))
  (when (>= (hook-entry-consecutive-failures entry)
            (hook-entry-failure-threshold entry))
    (setf (hook-entry-disabled-p entry) t))
  entry)

(defun %invoke-hook-handler-with-budget (entry args)
  (let ((start-ms (monotonic-ms))
        (budget-ms (hook-entry-max-ms entry)))
    (flet ((elapsed-ms ()
             (max 0 (- (monotonic-ms) start-ms))))
      (handler-case
          (let ((result
                  #+sbcl
                  (if (and (integerp budget-ms) (> budget-ms 0))
                      (sb-ext:with-timeout
                          (/ (coerce budget-ms 'double-float) 1000d0)
                        (apply (hook-entry-handler entry) args))
                      (apply (hook-entry-handler entry) args))
                  #-sbcl
                  (apply (hook-entry-handler entry) args)))
            (values :ok result (elapsed-ms) nil))
        #+sbcl
        (sb-ext:timeout (condition)
          (values :timeout nil (elapsed-ms) condition))
        (error (condition)
          (values :error nil (elapsed-ms) condition))))))

(defun %handle-hook-failure (hook-point entry status elapsed-ms condition)
  (%record-hook-failure entry status elapsed-ms condition)
  (let* ((hook-id (hook-entry-hook-id entry))
         (policy (hook-entry-on-error entry))
         (hook-condition
           (make-condition 'hook-execution-error
                           :hook-id hook-id
                           :hook-point hook-point
                           :cause condition
                           :message (princ-to-string condition))))
    (when (eq policy :disable-hook)
      (setf (hook-entry-disabled-p entry) t))
    (when (hook-entry-disabled-p entry)
      (setf (hook-entry-last-status entry) :disabled))
    (%record-hook-trace hook-point
                        hook-id
                        (if (eq status :timeout) :timeout :error)
                        :elapsed-ms elapsed-ms
                        :result (if (eq status :timeout) :hook-timeout :hook-error)
                        :detail condition)
    (case policy
      (:signal
       (error hook-condition))
      (:disable-hook
       :hook-disabled)
      (otherwise
       (warn "~A" hook-condition)
       (if (eq status :timeout) :hook-timeout :hook-error)))))

(defun %invoke-hook-entry (hook-point entry args)
  (let* ((hook-id (hook-entry-hook-id entry))
         (hook-key (%hook-key hook-point hook-id)))
    (cond
      ((hook-entry-disabled-p entry)
       (%record-hook-trace hook-point hook-id :disabled :elapsed-ms 0 :result :disabled)
       :disabled)
      ((member hook-key *hook-active-stack* :test #'equal)
       (%record-hook-trace hook-point hook-id :reentrant-skip :elapsed-ms 0 :result :reentrant-skip)
       :reentrant-skip)
      (t
       (let ((*hook-active-stack* (cons hook-key *hook-active-stack*)))
         (multiple-value-bind (status result elapsed-ms condition)
             (%invoke-hook-handler-with-budget entry args)
           (case status
             (:ok
              (%record-hook-success entry elapsed-ms)
              (%record-hook-trace hook-point hook-id :ok :elapsed-ms elapsed-ms :result result)
              result)
             ((:timeout :error)
              (%handle-hook-failure hook-point entry status elapsed-ms condition))
             (otherwise
              (%handle-hook-failure hook-point entry :error elapsed-ms condition)))))))))

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
              (%dispatch-async
               (lambda ()
                 (%invoke-hook-entry normalized entry args))
               '())
              (%record-hook-trace normalized hook-id :async-dispatched :elapsed-ms 0 :result :async-dispatched)
              (push (cons hook-id :async-dispatched) results))
            (let ((result (%invoke-hook-entry normalized entry args)))
              (push (cons hook-id result) results)
              (when (and blocking (or (eq result :deny)
                                      (eq result :block)))
                (return (values result (nreverse results))))))))))

(defun hook-chain (hook-point &rest args)
  (let* ((normalized (%normalize-hook-point hook-point))
         (results '()))
    (dolist (entry (%sort-hook-entries-ascending (%hook-entries normalized))
             (values :continue (nreverse results)))
      (let ((hook-id (hook-entry-hook-id entry)))
        (if (hook-entry-async-p entry)
            (progn
              (%dispatch-async
               (lambda ()
                 (%invoke-hook-entry normalized entry args))
               '())
              (%record-hook-trace normalized hook-id :async-dispatched :elapsed-ms 0 :result :async-dispatched)
              (push (cons hook-id :async-dispatched) results))
            (let ((result (%invoke-hook-entry normalized entry args)))
              (push (cons hook-id result) results)
              (when (eq result :block)
                (return (values :block (nreverse results))))))))))

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
          (options (list :priority 0
                         :async nil
                         :max-ms +default-hook-max-ms+
                         :on-error :log-and-continue
                         :failure-threshold +default-hook-failure-threshold+))
          (remaining forms)
          (clauses '()))
      (when (and remaining (stringp (first remaining)))
        (setf docstring (first remaining)
              remaining (rest remaining)))
      (loop while (and remaining
                       (consp (first remaining))
                       (keywordp (first (first remaining)))
                       (member (first (first remaining))
                               '(:priority :async :max-ms :on-error :failure-threshold)
                               :test #'eq))
            do (destructuring-bind (option value &rest extra) (first remaining)
                 (declare (ignore extra))
                 (ecase option
                   (:priority
                    (unless (integerp value)
                      (error ":priority requires integer value, got ~S." value))
                    (setf (getf options :priority) value))
                   (:async
                    (setf (getf options :async) (not (null value))))
                   (:max-ms
                    (unless (and (integerp value) (> value 0))
                      (error ":max-ms requires a positive integer value, got ~S." value))
                    (setf (getf options :max-ms) value))
                   (:on-error
                    (unless (keywordp value)
                      (error ":on-error requires a keyword value, got ~S." value))
                    (%ensure-known-hook-on-error-policy value)
                    (setf (getf options :on-error) value))
                   (:failure-threshold
                    (unless (and (integerp value) (> value 0))
                      (error ":failure-threshold requires a positive integer value, got ~S."
                             value))
                    (setf (getf options :failure-threshold) value)))
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

  (defun %known-deftool-reference-p (tool-name)
    (and (boundp '*deftool-compile-time-tool-names*)
         (hash-table-p *deftool-compile-time-tool-names*)
         (gethash tool-name *deftool-compile-time-tool-names*)))

  (defun %compile-match-predicate (pattern tool-var args-var &key hook-point)
    (cond
      ((eq pattern t)
       t)
      ((and (listp pattern) (evenp (length pattern)))
       (let ((tests '()))
         (loop for (key value) on pattern by #'cddr
               do (case key
                    (:tool
                     (let ((tool-name (%tool-name-string value)))
                       (unless (%known-deftool-reference-p tool-name)
                         (warn 'unknown-tool-reference
                               :hook-point hook-point
                               :reference tool-name))
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
    (unless (%hook-point-spec normalized-hook-point)
      (warn 'defhook-definition-warning
            :hook-point normalized-hook-point
            :reason "Unknown hook point; definition skipped.")
      (return-from defhook nil))
    (%validate-hook-signature normalized-hook-point parameters)
    (multiple-value-bind (docstring options clauses)
        (%parse-defhook-options-and-clauses forms)
      (let* ((priority (getf options :priority))
             (async (getf options :async))
             (max-ms (getf options :max-ms))
             (on-error (getf options :on-error))
             (failure-threshold (getf options :failure-threshold))
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
                           `(,(%compile-match-predicate pattern
                                                        tool-var
                                                        args-var
                                                        :hook-point normalized-hook-point)
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
                          :max-ms ,max-ms
                          :on-error ,on-error
                          :failure-threshold ,failure-threshold
                          :docstring ,docstring
                          :source-file ,source-file
                          :source-line nil)
           ',hook-id)))))
