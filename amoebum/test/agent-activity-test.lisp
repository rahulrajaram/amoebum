(in-package :amoebum/test)

(def-suite agent-activity-suite
  :description "I362 real-time agent activity stream and filtering."
  :in amoebum-suite)

(in-suite agent-activity-suite)

(test agent-activity-record-and-filter
  "Direct activity recording supports agent and type filters."
  (let ((original-bus amoebum:*event-bus*))
    (unwind-protect
         (let ((bus (amoebum:make-event-bus :capacity 128)))
           (setf amoebum:*event-bus* bus)
           (amoebum:clear-agent-activity-stream)
           (amoebum:ensure-agent-activity-stream bus)
           (amoebum:record-agent-activity :inference
                                          :agent-id "agent-a"
                                          :description "planning")
           (amoebum:record-agent-activity :tool-call
                                          :agent-id "agent-b"
                                          :description "run tests")
           (amoebum:record-agent-activity :idle
                                          :agent-id "agent-a"
                                          :description "done")
           (let ((agent-a (amoebum:list-agent-activity :agent-id "agent-a" :limit 10))
                 (tool-only (amoebum:list-agent-activity :activity-type :tool-call :limit 10)))
             (is (= 2 (length agent-a)))
             (is (= 1 (length tool-only)))
             (is (string= "agent-b"
                          (amoebum:agent-activity-entry-agent-id (first tool-only))))
             (is (eq :tool-call
                     (amoebum:agent-activity-entry-activity-type (first tool-only))))))
      (setf amoebum:*event-bus* original-bus))))

(test agent-activity-bridges-event-bus
  "Relevant events are bridged into the activity stream in real time."
  (let ((original-bus amoebum:*event-bus*))
    (unwind-protect
         (let ((bus (amoebum:make-event-bus :capacity 128)))
           (setf amoebum:*event-bus* bus)
           (amoebum:clear-agent-activity-stream)
           (amoebum:ensure-agent-activity-stream bus)
           (amoebum:publish bus
                            amoebum:+event-type-agent-spawn+
                            :source :amoebum
                            :payload (list :id "agent-rt-1"
                                           :task "Investigate flaky test"))
           (amoebum:publish bus
                            amoebum:+event-type-agent-complete+
                            :source :amoebum
                            :payload (list :id "agent-rt-1"
                                           :status :completed))
           (let ((entries (amoebum:list-agent-activity :agent-id "agent-rt-1" :limit 10)))
             (is (>= (length entries) 2))
             (is (member :waiting
                         (mapcar #'amoebum:agent-activity-entry-activity-type entries)
                         :test #'eq))
             (is (member :idle
                         (mapcar #'amoebum:agent-activity-entry-activity-type entries)
                         :test #'eq))))
      (setf amoebum:*event-bus* original-bus))))

(test agent-activity-slash-command-filter
  "/agent-activity supports --type filtering."
  (let ((original-bus amoebum:*event-bus*))
    (unwind-protect
         (let ((bus (amoebum:make-event-bus :capacity 128)))
           (setf amoebum:*event-bus* bus)
           (amoebum:clear-agent-activity-stream)
           (amoebum:ensure-agent-activity-stream bus)
           (amoebum:record-agent-activity :inference
                                          :agent-id "main"
                                          :description "thinking")
           (amoebum:record-agent-activity :tool-call
                                          :agent-id "main"
                                          :description "bash test")
           (multiple-value-bind (handledp result)
               (amoebum:dispatch-slash-command
                "/agent-activity --type tool-call --limit 5"
                :config (amoebum.config:current-config))
             (let ((output (amoebum.commands:slash-command-result-output result)))
               (is-true handledp)
               (is (amoebum.commands:slash-command-result-p result))
               (is (stringp output))
               (is (search "type=tool-call" output :test #'char-equal))
               (is (search "tool-call" output :test #'char-equal)))))
      (setf amoebum:*event-bus* original-bus))))

(test agent-activity-widget-renders
  "PTUI widget renders activity rows."
  (let ((original-bus amoebum:*event-bus*))
    (unwind-protect
         (let ((bus (amoebum:make-event-bus :capacity 128)))
           (setf amoebum:*event-bus* bus)
           (amoebum:clear-agent-activity-stream)
           (amoebum:ensure-agent-activity-stream bus)
           (amoebum:record-agent-activity :waiting
                                          :agent-id "agent-widget"
                                          :description "queued")
           (let ((tree (amoebum:agent-activity-stream
                        '(:agent-id "agent-widget"
                          :activity-type :waiting
                          :limit 5))))
             (is (typep tree 'ptui.ui.elements:ui-element))
             (is (eq :box (ptui.ui.elements:ui-element-type tree)))))
      (setf amoebum:*event-bus* original-bus))))

(test agent-activity-includes-handoff-result-provenance
  "Completed handoff activity should surface structured result metadata."
  (let ((original-bus amoebum:*event-bus*))
    (unwind-protect
         (let ((bus (amoebum:make-event-bus :capacity 128)))
           (setf amoebum:*event-bus* bus)
           (amoebum:clear-agent-activity-stream)
           (amoebum:ensure-agent-activity-stream bus)
           (let ((amoebum::*user-session-registry* (sw4rm-sdk:make-local-registry))
                 (amoebum::*user-session->agent-id* (make-hash-table :test #'equal))
                 (amoebum::*user-session->user-id* (make-hash-table :test #'equal))
                 (amoebum::*user-agent->session-id* (make-hash-table :test #'equal))
                 (amoebum::*user-negotiation-room-participants* (make-hash-table :test #'equal))
                 (amoebum::*user-handoff-client* nil)
                 (amoebum::*user-negotiation-client* nil)
                 (amoebum::*user-handoff-sequence* 0)
                 (amoebum::*user-handoff-details* (make-hash-table :test #'equal))
                 (amoebum::*user-coordination-lock*
                   (bordeaux-threads:make-lock "agent-activity-handoff-lock")))
             (amoebum:register-user-session-peer "session-from" :user-id "alice")
             (amoebum:register-user-session-peer "session-to" :user-id "bob")
             (let* ((response (amoebum:handoff-between-users
                               "session-from"
                               "session-to"
                               "Need a delegated review"))
                    (handoff-id (getf response :handoff-id)))
               (amoebum:accept-user-handoff handoff-id)
               (amoebum:complete-user-handoff
                handoff-id
                :summary "Delegate landed a review plan."
                :result-payload '(:summary "Delegate landed a review plan.")
                :diagnostics '((:severity :warning :message "one flaky check remains"))
                :provenance '(:worker-id "delegate-7")))
             (let* ((entries (amoebum:list-agent-activity :agent-id "user/bob/session/session-to"
                                                          :limit 10))
                   (completed (find amoebum::+event-type-user-handoff-completed+
                                    entries
                                    :key #'amoebum:agent-activity-entry-source-event-type
                                    :test #'eq))
                   (metadata (and completed
                                  (amoebum::agent-activity-entry-metadata completed))))
               (is-true completed)
               (is (search "Delegate landed a review plan."
                           (amoebum::agent-activity-entry-description completed)
                           :test #'char-equal))
               (is (= 1 (length (getf metadata :diagnostics))))
               (is (listp (getf metadata :delegation-trace)))
               (is (equal '(:summary "Delegate landed a review plan.")
                          (getf metadata :result-payload))))))
      (setf amoebum:*event-bus* original-bus))))
