;;;; budget.lisp
;;;; Budget envelope primitives and enforcement helpers.

(in-package :sw4rm-sdk)

(define-condition budget-exhausted (sw4rm-error)
  ((budget
    :initarg :budget
    :reader budget-exhausted-budget)
   (required-tokens
    :initarg :required-tokens
    :reader budget-exhausted-required-tokens
    :initform 0)
   (required-wall-time-ms
    :initarg :required-wall-time-ms
    :reader budget-exhausted-required-wall-time-ms
    :initform 0))
  (:default-initargs :error-code +ack-timeout+)
  (:report (lambda (condition stream)
             (format stream "Budget exhausted (tokens=~A, wall=~Ams): ~A"
                     (budget-exhausted-required-tokens condition)
                     (budget-exhausted-required-wall-time-ms condition)
                     (sw4rm-error-message condition)))))

(defstruct (budget-envelope
            (:constructor %make-budget-envelope)
            (:copier nil))
  "Monotonic budget envelope propagated through delegation trees."
  (token-budget-remaining 0 :type (integer 0 *))
  (wall-time-remaining-ms 0 :type (integer 0 *))
  (deadline-epoch-ms 0 :type (integer 0 *))
  (max-delegation-depth 0 :type (integer 0 *))
  (emergency-token-reserve 0 :type (integer 0 *))
  (cancelled-p nil :type boolean)
  (metadata nil :type list))

(defclass budget-interceptor ()
  ((budget
    :initarg :budget
    :accessor budget-interceptor-budget
    :documentation "Mutable BUDGET-ENVELOPE consumed on each request.")
   (estimator
    :initarg :estimator
    :initform (lambda (request)
                (declare (ignore request))
                (values 0 0))
    :accessor budget-interceptor-estimator
    :documentation "Function returning (values estimated-tokens estimated-wall-ms).")
   (warn-threshold
    :initarg :warn-threshold
    :initform 0.2d0
    :accessor budget-interceptor-warn-threshold
    :documentation "Warning threshold ratio for low-budget notifications.")
   (on-warning
    :initarg :on-warning
    :initform nil
    :accessor budget-interceptor-on-warning
    :documentation "Optional callback invoked when budget ratio falls below WARN-THRESHOLD.")
   (on-exhausted
    :initarg :on-exhausted
    :initform nil
    :accessor budget-interceptor-on-exhausted
    :documentation "Optional callback invoked when budget depletion blocks a request.")))

(defun budget-now-ms ()
  "Best-effort wall-clock milliseconds."
  (* (get-universal-time) 1000))

(defun make-budget-envelope (&key
                             (token-budget-remaining 0)
                             (wall-time-remaining-ms 0)
                             (deadline-epoch-ms (+ (budget-now-ms) wall-time-remaining-ms))
                             (max-delegation-depth 0)
                             (emergency-token-reserve 0)
                             (cancelled-p nil)
                             metadata)
  "Construct a budget envelope with defensive validation."
  (dolist (pair (list (cons "token-budget-remaining" token-budget-remaining)
                      (cons "wall-time-remaining-ms" wall-time-remaining-ms)
                      (cons "deadline-epoch-ms" deadline-epoch-ms)
                      (cons "max-delegation-depth" max-delegation-depth)
                      (cons "emergency-token-reserve" emergency-token-reserve)))
    (unless (and (integerp (cdr pair)) (>= (cdr pair) 0))
      (error 'validation-error
             :message (format nil "~A must be a non-negative integer" (car pair))
             :field (car pair)
             :constraint "non-negative integer")))
  (%make-budget-envelope
   :token-budget-remaining token-budget-remaining
   :wall-time-remaining-ms wall-time-remaining-ms
   :deadline-epoch-ms deadline-epoch-ms
   :max-delegation-depth max-delegation-depth
   :emergency-token-reserve emergency-token-reserve
   :cancelled-p (not (null cancelled-p))
   :metadata (copy-list metadata)))

(defun copy-budget-envelope (budget)
  "Create a mutable copy of BUDGET."
  (check-type budget budget-envelope)
  (make-budget-envelope
   :token-budget-remaining (budget-envelope-token-budget-remaining budget)
   :wall-time-remaining-ms (budget-envelope-wall-time-remaining-ms budget)
   :deadline-epoch-ms (budget-envelope-deadline-epoch-ms budget)
   :max-delegation-depth (budget-envelope-max-delegation-depth budget)
   :emergency-token-reserve (budget-envelope-emergency-token-reserve budget)
   :cancelled-p (budget-envelope-cancelled-p budget)
   :metadata (copy-list (budget-envelope-metadata budget))))

(defun budget-exhausted-p (budget &key (now-ms (budget-now-ms)))
  "Return T when BUDGET can no longer fund work."
  (check-type budget budget-envelope)
  (or (budget-envelope-cancelled-p budget)
      (<= (budget-envelope-token-budget-remaining budget) 0)
      (<= (budget-envelope-wall-time-remaining-ms budget) 0)
      (> now-ms (budget-envelope-deadline-epoch-ms budget))))

(defun check-budget (budget &key (estimated-tokens 0) (estimated-wall-time-ms 0)
                            (delegation-depth 0) (now-ms (budget-now-ms)))
  "Predicate that checks whether BUDGET can pay for a pending operation."
  (check-type budget budget-envelope)
  (and (not (budget-envelope-cancelled-p budget))
       (<= delegation-depth (budget-envelope-max-delegation-depth budget))
       (<= estimated-tokens
           (+ (budget-envelope-token-budget-remaining budget)
              (budget-envelope-emergency-token-reserve budget)))
       (<= estimated-wall-time-ms (budget-envelope-wall-time-remaining-ms budget))
       (<= now-ms (budget-envelope-deadline-epoch-ms budget))))

(defun %signal-budget-exhausted (budget required-tokens required-wall-time-ms)
  (restart-case
      (error 'budget-exhausted
             :message "Insufficient budget for requested operation"
             :budget budget
             :required-tokens required-tokens
             :required-wall-time-ms required-wall-time-ms)
    (extend-budget (&optional (extra-tokens 0) (extra-wall-time-ms 0))
      :report "Increase budget envelope and retry."
      (when (and (integerp extra-tokens) (> extra-tokens 0))
        (incf (budget-envelope-token-budget-remaining budget) extra-tokens))
      (when (and (integerp extra-wall-time-ms) (> extra-wall-time-ms 0))
        (incf (budget-envelope-wall-time-remaining-ms budget) extra-wall-time-ms)
        (incf (budget-envelope-deadline-epoch-ms budget) extra-wall-time-ms))
      t)
    (use-emergency-reserve ()
      :report "Transfer emergency reserve into active token budget."
      (let ((reserve (budget-envelope-emergency-token-reserve budget)))
        (when (> reserve 0)
          (incf (budget-envelope-token-budget-remaining budget) reserve)
          (setf (budget-envelope-emergency-token-reserve budget) 0)))
      t)))

(defun decrement-budget (budget &key (tokens 0) (wall-time-ms 0) (now-ms (budget-now-ms)))
  "Atomically decrease BUDGET for a completed operation.

Signals BUDGET-EXHAUSTED with restarts when the request cannot be funded."
  (check-type budget budget-envelope)
  (unless (and (integerp tokens) (>= tokens 0))
    (error 'validation-error
           :message "tokens must be a non-negative integer"
           :field "tokens"
           :constraint "non-negative integer"))
  (unless (and (integerp wall-time-ms) (>= wall-time-ms 0))
    (error 'validation-error
           :message "wall-time-ms must be a non-negative integer"
           :field "wall-time-ms"
           :constraint "non-negative integer"))
  (unless (check-budget budget
                        :estimated-tokens tokens
                        :estimated-wall-time-ms wall-time-ms
                        :now-ms now-ms)
    (%signal-budget-exhausted budget tokens wall-time-ms)
    (unless (check-budget budget
                          :estimated-tokens tokens
                          :estimated-wall-time-ms wall-time-ms
                          :now-ms now-ms)
      (%signal-budget-exhausted budget tokens wall-time-ms)))
  (let* ((regular (budget-envelope-token-budget-remaining budget))
         (reserve (budget-envelope-emergency-token-reserve budget))
         (remaining-after-regular (- regular tokens)))
    (if (>= remaining-after-regular 0)
        (setf (budget-envelope-token-budget-remaining budget) remaining-after-regular)
        (let ((reserve-consumed (abs remaining-after-regular)))
          (setf (budget-envelope-token-budget-remaining budget) 0)
          (setf (budget-envelope-emergency-token-reserve budget)
                (max 0 (- reserve reserve-consumed))))))
  (setf (budget-envelope-wall-time-remaining-ms budget)
        (max 0 (- (budget-envelope-wall-time-remaining-ms budget) wall-time-ms)))
  budget)

(defun split-budget (budget &key token-share wall-time-share tokens wall-time-ms
                           (child-max-delegation-depth nil))
  "Split BUDGET into a child envelope while preserving monotonic constraints.

Returns two values: CHILD-BUDGET and updated parent BUDGET."
  (check-type budget budget-envelope)
  (let* ((parent-tokens (budget-envelope-token-budget-remaining budget))
         (parent-wall (budget-envelope-wall-time-remaining-ms budget))
         (child-tokens (or tokens
                           (if token-share
                               (floor (* parent-tokens token-share))
                               (floor parent-tokens 2))))
         (child-wall (or wall-time-ms
                         (if wall-time-share
                             (floor (* parent-wall wall-time-share))
                             (floor parent-wall 2)))))
    (decrement-budget budget :tokens child-tokens :wall-time-ms child-wall)
    (values
     (make-budget-envelope
      :token-budget-remaining child-tokens
      :wall-time-remaining-ms child-wall
      :deadline-epoch-ms (min (budget-envelope-deadline-epoch-ms budget)
                              (+ (budget-now-ms) child-wall))
      :max-delegation-depth (or child-max-delegation-depth
                                (if (> (budget-envelope-max-delegation-depth budget) 0)
                                    (1- (budget-envelope-max-delegation-depth budget))
                                    0))
      :emergency-token-reserve 0)
     budget)))

(defun tighten-budget (budget &key token-budget-remaining wall-time-remaining-ms
                              deadline-epoch-ms max-delegation-depth)
  "Create a tighter envelope that cannot exceed the parent BUDGET."
  (check-type budget budget-envelope)
  (make-budget-envelope
   :token-budget-remaining
   (min (or token-budget-remaining (budget-envelope-token-budget-remaining budget))
        (budget-envelope-token-budget-remaining budget))
   :wall-time-remaining-ms
   (min (or wall-time-remaining-ms (budget-envelope-wall-time-remaining-ms budget))
        (budget-envelope-wall-time-remaining-ms budget))
   :deadline-epoch-ms
   (min (or deadline-epoch-ms (budget-envelope-deadline-epoch-ms budget))
        (budget-envelope-deadline-epoch-ms budget))
   :max-delegation-depth
   (min (or max-delegation-depth (budget-envelope-max-delegation-depth budget))
        (budget-envelope-max-delegation-depth budget))
   :emergency-token-reserve 0))

(defun cancel-budget (budget)
  "Mark BUDGET as cancelled."
  (check-type budget budget-envelope)
  (setf (budget-envelope-cancelled-p budget) t)
  budget)

(defun cancelled-p (budget)
  "Predicate for cooperative cancellation checks."
  (check-type budget budget-envelope)
  (budget-envelope-cancelled-p budget))

(defun apply-budget-interceptor (interceptor request)
  "Apply INTERCEPTOR accounting to REQUEST and return updated request."
  (check-type interceptor budget-interceptor)
  (multiple-value-bind (estimated-tokens estimated-wall-ms)
      (funcall (budget-interceptor-estimator interceptor) request)
    (handler-case
        (decrement-budget (budget-interceptor-budget interceptor)
                          :tokens (or estimated-tokens 0)
                          :wall-time-ms (or estimated-wall-ms 0))
      (budget-exhausted (condition)
        (when (budget-interceptor-on-exhausted interceptor)
          (funcall (budget-interceptor-on-exhausted interceptor) condition))
        (error condition)))
    (let* ((budget (budget-interceptor-budget interceptor))
           (remaining (budget-envelope-token-budget-remaining budget))
           (total (+ remaining (budget-envelope-emergency-token-reserve budget)))
           (ratio (if (> total 0)
                      (/ (coerce remaining 'double-float)
                         (coerce total 'double-float))
                      0.0d0)))
      (when (and (budget-interceptor-on-warning interceptor)
                 (<= ratio (budget-interceptor-warn-threshold interceptor)))
        (funcall (budget-interceptor-on-warning interceptor) budget ratio)))
    request))
