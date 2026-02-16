;;;; local-hitl.lisp
;;;; Local HITL approval gate with blocking wait + timeout fallback.

(in-package :sw4rm-sdk)

(defstruct hitl-gate-request
  "One HITL request lifecycle record."
  (request-id nil :type string)
  (reason +hitl-reason-unspecified+ :type integer)
  (context nil :type t)
  (status :pending :type keyword)
  (decision nil :type (or null keyword))
  (notes nil :type (or null string))
  (requested-at (current-time-ms) :type integer)
  (deadline-epoch-ms 0 :type (integer 0 *))
  (metadata nil :type list))

(defclass local-hitl-gate ()
  ((requests
    :initform (make-hash-table :test #'equal)
    :accessor local-hitl-gate-requests)
   (lock
    :initform (bt:make-lock "local-hitl-gate-lock")
    :accessor local-hitl-gate-lock)
   (cv
    :initform (bt:make-condition-variable)
    :accessor local-hitl-gate-cv))
  (:documentation "In-process HITL gate for approval workflows."))

(defun make-local-hitl-gate ()
  "Construct a local HITL gate."
  (make-instance 'local-hitl-gate))

(defun %lookup-hitl-request (gate request-id)
  (or (gethash request-id (local-hitl-gate-requests gate))
      (error 'rpc-error
             :message (format nil "HITL request '~A' not found" request-id)
             :status-code "NOT_FOUND"
             :details "request missing")))

(defun request-hitl-approval (gate &key request-id
                                   (reason +hitl-reason-unspecified+)
                                   context
                                   (timeout-ms 30000)
                                   (timeout-action :deny)
                                   metadata)
  "Create a HITL request and block until decision or timeout."
  (check-type gate local-hitl-gate)
  (let* ((id (or request-id (generate-uuid)))
         (deadline (+ (current-time-ms) timeout-ms))
         (request (make-hitl-gate-request
                   :request-id id
                   :reason reason
                   :context context
                   :deadline-epoch-ms deadline
                   :metadata (copy-list metadata))))
    (bt:with-lock-held ((local-hitl-gate-lock gate))
      (setf (gethash id (local-hitl-gate-requests gate)) request))
    (loop
      do
         (bt:with-lock-held ((local-hitl-gate-lock gate))
           (let ((current (%lookup-hitl-request gate id)))
             (unless (eq (hitl-gate-request-status current) :pending)
               (return-from request-hitl-approval
                 (list :request-id id
                       :status (hitl-gate-request-status current)
                       :decision (hitl-gate-request-decision current)
                       :notes (hitl-gate-request-notes current))))
             (when (>= (current-time-ms) (hitl-gate-request-deadline-epoch-ms current))
               (setf (hitl-gate-request-status current)
                     (ecase timeout-action
                       (:deny :denied)
                       (:approve :approved)
                       (:escalate :escalated)))
               (setf (hitl-gate-request-decision current)
                     (ecase timeout-action
                       (:deny :deny)
                       (:approve :approve)
                       (:escalate :escalate)))
               (setf (hitl-gate-request-notes current) "timeout auto-decision")
               (return-from request-hitl-approval
                 (list :request-id id
                       :status (hitl-gate-request-status current)
                       :decision (hitl-gate-request-decision current)
                       :notes (hitl-gate-request-notes current))))
             (bt:condition-wait (local-hitl-gate-cv gate)
                                (local-hitl-gate-lock gate)
                                :timeout 0.1))))))

(defun approve-hitl-request (gate request-id &key notes)
  "Approve a pending HITL request."
  (check-type gate local-hitl-gate)
  (bt:with-lock-held ((local-hitl-gate-lock gate))
    (let ((request (%lookup-hitl-request gate request-id)))
      (setf (hitl-gate-request-status request) :approved)
      (setf (hitl-gate-request-decision request) :approve)
      (setf (hitl-gate-request-notes request) notes)
      (bt:condition-notify (local-hitl-gate-cv gate))
      request)))

(defun deny-hitl-request (gate request-id &key notes)
  "Deny a pending HITL request."
  (check-type gate local-hitl-gate)
  (bt:with-lock-held ((local-hitl-gate-lock gate))
    (let ((request (%lookup-hitl-request gate request-id)))
      (setf (hitl-gate-request-status request) :denied)
      (setf (hitl-gate-request-decision request) :deny)
      (setf (hitl-gate-request-notes request) notes)
      (bt:condition-notify (local-hitl-gate-cv gate))
      request)))

(defun get-hitl-request (gate request-id)
  "Lookup HITL request metadata."
  (check-type gate local-hitl-gate)
  (bt:with-lock-held ((local-hitl-gate-lock gate))
    (let ((request (%lookup-hitl-request gate request-id)))
      (list :request-id (hitl-gate-request-request-id request)
            :reason (hitl-gate-request-reason request)
            :status (hitl-gate-request-status request)
            :decision (hitl-gate-request-decision request)
            :notes (hitl-gate-request-notes request)
            :deadline-epoch-ms (hitl-gate-request-deadline-epoch-ms request)))))

(defun list-pending-hitl-requests (gate)
  "List all pending HITL requests."
  (check-type gate local-hitl-gate)
  (bt:with-lock-held ((local-hitl-gate-lock gate))
    (let ((pending nil))
      (maphash (lambda (_ request)
                 (declare (ignore _))
                 (when (eq (hitl-gate-request-status request) :pending)
                   (push (hitl-gate-request-request-id request) pending)))
               (local-hitl-gate-requests gate))
      pending)))
