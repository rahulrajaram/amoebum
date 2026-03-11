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
                :config (amoebum:current-config))
             (let ((output (amoebum:slash-command-result-output result)))
               (is-true handledp)
               (is (amoebum:slash-command-result-p result))
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
