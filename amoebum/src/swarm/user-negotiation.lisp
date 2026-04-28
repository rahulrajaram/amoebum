(in-package :amoebum)

;;; ---------------------------------------------------------------------------
;;; Inter-user negotiation rooms (I253)
;;; ---------------------------------------------------------------------------

(defun create-user-negotiation-room (room-id participant-session-ids
                                     &key description metadata)
  "Create a multi-user negotiation room for code review."
  (let* ((resolved-room-id (%coordination-require-string room-id "room-id"))
         (participants (remove-duplicates
                        (mapcar (lambda (session-id)
                                  (%coordination-require-string session-id "participant session-id"))
                                participant-session-ids)
                        :test #'string=)))
    (dolist (session-id participants)
      (%registered-agent-id-for-session session-id))
    (let ((response (sw4rm-sdk:create-room (%ensure-user-negotiation-client)
                                           resolved-room-id
                                           :description description
                                           :metadata metadata)))
      (bt:with-lock-held (*user-coordination-lock*)
        (setf (gethash resolved-room-id *user-negotiation-room-participants*)
              participants))
      (let ((payload (append response
                             (list :room-id resolved-room-id
                                   :participant-session-ids participants
                                   :description description
                                   :metadata metadata
                                   :agent-id (and participants
                                                  (%registered-agent-id-for-session
                                                   (first participants)))))))
        (%publish-user-coordination-event +event-type-user-negotiation-room-created+
                                          payload)
        payload))))

(defun submit-user-negotiation-artifact (room-id proposer-session-id artifact-id artifact
                                         &key requested-critic-session-ids
                                           (aggregation-strategy :confidence-weighted)
                                           timeout-ms
                                           metadata)
  "Submit ARTIFACT for review in ROOM-ID from PROPOSER-SESSION-ID."
  (let* ((resolved-room-id (%coordination-require-string room-id "room-id"))
         (resolved-artifact-id (%coordination-require-string artifact-id "artifact-id"))
         (proposer-agent-id (%registered-agent-id-for-session proposer-session-id))
         (requested-critic-agent-ids
           (mapcar #'%registered-agent-id-for-session requested-critic-session-ids))
         (proposal (list :artifact-id resolved-artifact-id
                         :negotiation-room-id resolved-room-id
                         :proposer-id proposer-agent-id
                         :artifact artifact
                         :metadata metadata
                         :requested-critics requested-critic-agent-ids
                         :aggregation-strategy aggregation-strategy)))
    (when timeout-ms
      (setf (getf proposal :timeout-ms) timeout-ms))
    (let ((response (sw4rm-sdk:submit-artifact (%ensure-user-negotiation-client) proposal)))
      (%publish-user-coordination-event
       +event-type-user-negotiation-artifact-submitted+
       (list :artifact-id resolved-artifact-id
             :room-id resolved-room-id
             :proposer-session-id proposer-session-id
             :requested-critic-session-ids (copy-list requested-critic-session-ids)
             :agent-id proposer-agent-id
             :metadata metadata
             :timeout-ms timeout-ms
             :response response))
      response)))

(defun add-user-negotiation-critique (room-id artifact-id critic-session-id passed
                                      &key score details)
  "Submit a critique vote in a user negotiation room."
  (let* ((resolved-room-id (%coordination-require-string room-id "room-id"))
         (resolved-artifact-id (%coordination-require-string artifact-id "artifact-id"))
         (critic-agent-id (%registered-agent-id-for-session critic-session-id)))
    (let ((response
            (sw4rm-sdk:add-critique
             (%ensure-user-negotiation-client)
             (list :artifact-id resolved-artifact-id
                   :negotiation-room-id resolved-room-id
                   :critic-id critic-agent-id
                   :passed (if passed t nil)
                   :score score
                   :details details))))
      (%publish-user-coordination-event
       +event-type-user-negotiation-critique-added+
       (list :artifact-id resolved-artifact-id
             :room-id resolved-room-id
             :critic-session-id critic-session-id
             :agent-id critic-agent-id
             :passed (if passed t nil)
             :score score
             :details details
             :response response))
      response)))

(defun get-user-negotiation-room-status (room-id)
  "Return room status enriched with session IDs."
  (let* ((resolved-room-id (%coordination-require-string room-id "room-id"))
         (status (sw4rm-sdk:get-room-status (%ensure-user-negotiation-client)
                                            resolved-room-id))
         (participants (bt:with-lock-held (*user-coordination-lock*)
                         (copy-list
                          (gethash resolved-room-id
                                   *user-negotiation-room-participants*))))
         (active-critic-sessions
           (remove nil
                   (mapcar #'%session-id-for-agent
                           (copy-list (getf status :active-critics))))))
    (append status
            (list :participant-session-ids participants
                  :active-critic-session-ids active-critic-sessions))))

(defun wait-for-user-negotiation-decision (artifact-id &key (timeout-s 30.0) (poll-interval-s 0.1))
  "Wait for a negotiation decision for ARTIFACT-ID."
  (let ((decision (sw4rm-sdk:wait-for-decision (%ensure-user-negotiation-client)
                                               artifact-id
                                               :timeout-s timeout-s
                                               :poll-interval-s poll-interval-s)))
    (%publish-user-coordination-event
     +event-type-user-negotiation-decision+
     (append decision
             (list :artifact-id artifact-id
                   :agent-id (or (getf decision :proposer-id)
                                 (getf decision :agent-id)))))
    decision))
