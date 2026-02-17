;;;; negotiation-room.lisp - Local negotiation room implementation

(in-package :sw4rm-sdk)

(defclass negotiation-room-client (base-client)
  ((store
    :initarg :store
    :accessor negotiation-room-client-store
    :initform (get-default-store)
    :documentation "Proposal/vote/decision storage backend.")
   (rooms
    :initform (make-hash-table :test #'equal)
    :accessor negotiation-room-client-rooms
    :documentation "Room registry keyed by room-id.")
   (lock
    :initform (bt:make-lock "negotiation-room-client-lock")
    :accessor negotiation-room-client-lock))
  (:documentation "In-process negotiation room client with lifecycle support."))

(defgeneric create-room (client room-id &key description metadata)
  (:documentation "Create a negotiation room and return metadata plist."))

(defgeneric submit-artifact (client artifact-proposal)
  (:documentation "Submit an artifact proposal; returns artifact-id."))

(defgeneric add-critique (client vote)
  (:documentation "Add a critic vote and possibly trigger room decision finalization."))

(defgeneric score-artifact (client artifact-id)
  (:documentation "List votes for artifact-id."))

(defgeneric get-room-status (client room-id)
  (:documentation "Return room status summary."))

(defgeneric wait-for-decision (client artifact-id &key timeout-s poll-interval-s)
  (:documentation "Wait for a decision or signal RPC-TIMEOUT."))

(defun %require-string (value field)
  (unless (and (stringp value)
               (> (length (string-trim '(#\Space #\Tab #\Newline #\Return) value)) 0))
    (error 'validation-error
           :message (format nil "~A must be a non-empty string" field)
           :field field
           :constraint "non-empty string"))
  (string-trim '(#\Space #\Tab #\Newline #\Return) value))

(defun %ensure-room (client room-id)
  (or (gethash room-id (negotiation-room-client-rooms client))
      (error 'rpc-error
             :message (format nil "Negotiation room '~A' not found" room-id)
             :status-code "NOT_FOUND"
             :details "room does not exist")))

(defun %proposal-room-id (proposal)
  (or (getf proposal :negotiation-room-id)
      (getf proposal :room-id)))

(defun %vote-passed-p (vote)
  (let ((passed (getf vote :passed)))
    (if passed t nil)))

(defun %votes-mixed-p (votes)
  (let ((saw-pass nil)
        (saw-fail nil))
    (dolist (vote votes)
      (if (%vote-passed-p vote)
          (setf saw-pass t)
          (setf saw-fail t)))
    (and saw-pass saw-fail)))

(defun %all-requested-critics-voted-p (proposal votes)
  (let ((requested (remove-duplicates
                    (mapcar #'princ-to-string (or (getf proposal :requested-critics) nil))
                    :test #'string=))
        (voters (remove-duplicates
                 (mapcar (lambda (vote) (princ-to-string (getf vote :critic-id))) votes)
                 :test #'string=)))
    (or (null requested)
        (every (lambda (critic-id) (member critic-id voters :test #'string=))
               requested))))

(defun %aggregate-score (votes)
  (let ((scores (remove nil (mapcar (lambda (vote) (getf vote :score)) votes))))
    (if (null scores)
        0.0
        (/ (reduce #'+ scores) (length scores)))))

(defun %decision-outcome-from-votes (proposal votes)
  (let* ((strategy (or (getf proposal :aggregation-strategy) :confidence-weighted))
         (all-pass (every #'%vote-passed-p votes)))
    (cond
      ((eq strategy :unanimous)
       (if all-pass :approved :rejected))
      (t
       (let* ((pass-count (count-if #'%vote-passed-p votes))
              (reject-count (- (length votes) pass-count)))
         (if (>= pass-count reject-count) :approved :rejected))))))

(defun %maybe-finalize-decision (client artifact-id)
  "Create and persist a decision when room state has enough information."
  (let* ((store (negotiation-room-client-store client))
         (proposal (get-proposal store artifact-id)))
    (unless proposal
      (error 'rpc-error
             :message (format nil "Artifact proposal '~A' not found" artifact-id)
             :status-code "NOT_FOUND"
             :details "proposal missing"))
    (or (get-decision store artifact-id)
        (let* ((votes (get-votes store artifact-id))
               (deadline (or (getf proposal :deadline-epoch-ms)
                             (let ((timeout-ms (getf proposal :timeout-ms)))
                               (when timeout-ms (+ (current-time-ms) timeout-ms)))))
               (now-ms (current-time-ms))
               (all-voted-p (%all-requested-critics-voted-p proposal votes))
               (timed-out-p (and deadline (> now-ms deadline)))
               (deadlock-p (%votes-mixed-p votes)))
          (cond
            ((and (null votes) (not timed-out-p))
             nil)
            ((and (not all-voted-p) (not timed-out-p))
             nil)
            (t
             (let* ((outcome
                      (cond
                        (timed-out-p :escalated)
                        (deadlock-p :escalated)
                        (t (%decision-outcome-from-votes proposal votes))))
                    (decision
                      (list :artifact-id artifact-id
                            :negotiation-room-id (%proposal-room-id proposal)
                            :outcome outcome
                            :aggregated-score (%aggregate-score votes)
                            :votes-count (length votes)
                            :state (if (eq outcome :escalated) :escalated :decided)
                            :reason (cond
                                      (timed-out-p :timeout)
                                      (deadlock-p :deadlock)
                                      (t :consensus))
                            :decided-at (get-universal-time))))
               (save-decision store decision)
               (let* ((room-id (%proposal-room-id proposal))
                      (room (gethash room-id (negotiation-room-client-rooms client))))
                 (when room
                   (setf (getf room :state) (getf decision :state))
                   (setf (getf room :updated-at) (get-universal-time))))
               decision)))))))

(defmethod create-room ((client negotiation-room-client) room-id
                        &key description metadata)
  (%require-string room-id "room-id")
  (bt:with-lock-held ((negotiation-room-client-lock client))
    (when (gethash room-id (negotiation-room-client-rooms client))
      (error 'rpc-error
             :message (format nil "Negotiation room '~A' already exists" room-id)
             :status-code "ALREADY_EXISTS"
             :details "duplicate room id"))
    (setf (gethash room-id (negotiation-room-client-rooms client))
          (list :room-id room-id
                :description description
                :metadata metadata
                :state :open
                :created-at (get-universal-time)
                :updated-at (get-universal-time)
                :artifact-ids nil)))
  (list :room-id room-id
        :created-at (get-universal-time)))

(defmethod submit-artifact ((client negotiation-room-client) artifact-proposal)
  (let* ((artifact-id (%require-string (or (getf artifact-proposal :artifact-id) "")
                                       "artifact-id"))
         (room-id (%require-string (or (%proposal-room-id artifact-proposal) "")
                                   "negotiation-room-id"))
         (room (%ensure-room client room-id))
         (store (negotiation-room-client-store client))
         (proposal (copy-list artifact-proposal)))
    (setf (getf proposal :artifact-id) artifact-id)
    (setf (getf proposal :negotiation-room-id) room-id)
    (setf (getf proposal :submitted-at) (get-universal-time))
    (when (has-proposal store artifact-id)
      (error 'rpc-error
             :message (format nil "Artifact '~A' already submitted" artifact-id)
             :status-code "ALREADY_EXISTS"
             :details "duplicate artifact id"))
    (save-proposal store proposal)
    (pushnew artifact-id (getf room :artifact-ids) :test #'string=)
    (setf (getf room :state) :voting)
    (setf (getf room :updated-at) (get-universal-time))
    artifact-id))

(defmethod add-critique ((client negotiation-room-client) vote)
  (let* ((artifact-id (%require-string (or (getf vote :artifact-id) "") "artifact-id"))
         (critic-id (%require-string (or (getf vote :critic-id) "") "critic-id"))
         (store (negotiation-room-client-store client))
         (proposal (get-proposal store artifact-id)))
    (unless proposal
      (error 'rpc-error
             :message (format nil "Artifact proposal '~A' not found" artifact-id)
             :status-code "NOT_FOUND"
             :details "proposal missing"))
    (let ((normalized-vote (copy-list vote)))
      (setf (getf normalized-vote :artifact-id) artifact-id)
      (setf (getf normalized-vote :critic-id) critic-id)
      (setf (getf normalized-vote :submitted-at) (get-universal-time))
      (add-vote store normalized-vote))
    (%maybe-finalize-decision client artifact-id)
    nil))

(defmethod score-artifact ((client negotiation-room-client) artifact-id)
  (let ((store (negotiation-room-client-store client)))
    (unless (has-proposal store artifact-id)
      (error 'rpc-error
             :message (format nil "Artifact proposal '~A' not found" artifact-id)
             :status-code "NOT_FOUND"
             :details "proposal missing"))
    (get-votes store artifact-id)))

(defmethod get-room-status ((client negotiation-room-client) room-id)
  (let* ((room (%ensure-room client room-id))
         (store (negotiation-room-client-store client))
         (pending 0)
         (completed 0)
         (active-critics nil))
    (dolist (artifact-id (getf room :artifact-ids))
      (let* ((proposal (get-proposal store artifact-id))
             (votes (get-votes store artifact-id))
             (decision (or (get-decision store artifact-id)
                           (%maybe-finalize-decision client artifact-id))))
        (if decision
            (incf completed)
            (progn
              (incf pending)
              (let ((requested (or (getf proposal :requested-critics) nil))
                    (already-voted (mapcar (lambda (vote) (getf vote :critic-id)) votes)))
                (dolist (critic requested)
                  (unless (member critic already-voted :test #'string=)
                    (pushnew critic active-critics :test #'string=))))))))
    (list :room-id room-id
          :state (getf room :state)
          :pending-proposals pending
          :completed-decisions completed
          :active-critics (nreverse active-critics)
          :updated-at (getf room :updated-at))))

(defmethod get-decision ((client negotiation-room-client) artifact-id)
  (let ((store (negotiation-room-client-store client)))
    (unless (has-proposal store artifact-id)
      (error 'rpc-error
             :message (format nil "Artifact proposal '~A' not found" artifact-id)
             :status-code "NOT_FOUND"
             :details "proposal missing"))
    (or (get-decision store artifact-id)
        (%maybe-finalize-decision client artifact-id))))

(defmethod wait-for-decision ((client negotiation-room-client) artifact-id
                              &key (timeout-s 30.0) (poll-interval-s 0.1))
  (let ((start (get-internal-real-time))
        (timeout-ms (floor (* timeout-s 1000)))
        (poll-seconds (max 0.01 poll-interval-s)))
    (loop
      for elapsed-ms = (floor (* 1000
                                 (/ (- (get-internal-real-time) start)
                                    internal-time-units-per-second)))
      do
         (let ((decision (handler-case
                             (get-decision client artifact-id)
                           (rpc-error (condition)
                             (error condition)))))
           (when decision
             (return decision)))
         (when (> elapsed-ms timeout-ms)
           (error 'rpc-timeout
                  :message (format nil "Timed out waiting for decision on ~A" artifact-id)
                  :status-code "DEADLINE_EXCEEDED"
                  :details "negotiation room timeout"))
         (sleep poll-seconds))))
