(in-package :amoebum/test)

(def-suite agent-tools-suite
  :description "Multi-agent orchestration tool tests."
  :in amoebum-suite)

(in-suite agent-tools-suite)

(test spawn-agent-worker-basic
  "spawn-agent-worker tool returns worker-id in response."
  (let ((original-toolset amoebum:*toolset*)
        (original-metadata amoebum:*tool-metadata*)
        (original-event-bus amoebum:*event-bus*)
        (original-rules amoebum:*permission-rules*)
        (original-supervisor amoebum::*worker-supervisor*))
    (unwind-protect
        (progn
          (setf amoebum:*event-bus* (amoebum:make-event-bus :capacity 64)
                amoebum::*worker-supervisor* (make-instance 'amoebum::in-process-supervisor))
          (let* ((context (amoebum:make-amoebum-context
                           :permission-mode :full-auto
                           :event-bus amoebum:*event-bus*
                           :initialize-notifications-p nil))
                 (call (pseudopod:make-tool-call
                        :id "agent-spawn-test"
                        :name "spawn-agent-worker"
                        :arguments "{\"task\":\"Test agent task\"}"))
                 (result (amoebum:execute-tool call context)))
            (is (stringp result))
            (is (search "worker-id" result :test #'char-equal))
            (is (search "w-agent-" result :test #'char-equal))))
      (setf amoebum:*toolset* original-toolset
            amoebum:*tool-metadata* original-metadata
            amoebum:*event-bus* original-event-bus
            amoebum:*permission-rules* original-rules
            amoebum::*worker-supervisor* original-supervisor))))

(test worker-status-not-found
  "worker-status-tool returns not-found for unknown worker ID."
  (let ((original-toolset amoebum:*toolset*)
        (original-metadata amoebum:*tool-metadata*)
        (original-event-bus amoebum:*event-bus*)
        (original-rules amoebum:*permission-rules*)
        (original-supervisor amoebum::*worker-supervisor*))
    (unwind-protect
        (progn
          (setf amoebum:*event-bus* (amoebum:make-event-bus :capacity 64)
                amoebum::*worker-supervisor* (make-instance 'amoebum::in-process-supervisor))
          (let* ((context (amoebum:make-amoebum-context
                           :permission-mode :full-auto
                           :event-bus amoebum:*event-bus*
                           :initialize-notifications-p nil))
                 (call (pseudopod:make-tool-call
                        :id "status-not-found"
                        :name "worker-status-tool"
                        :arguments "{\"worker-id\":\"w-nonexistent-9999\"}"))
                 (result (amoebum:execute-tool call context)))
            (is (stringp result))
            (is (search "not-found" result :test #'char-equal))))
      (setf amoebum:*toolset* original-toolset
            amoebum:*tool-metadata* original-metadata
            amoebum:*event-bus* original-event-bus
            amoebum:*permission-rules* original-rules
            amoebum::*worker-supervisor* original-supervisor))))

(test list-workers-empty
  "list-workers-tool returns empty list when no workers exist."
  (let ((original-toolset amoebum:*toolset*)
        (original-metadata amoebum:*tool-metadata*)
        (original-event-bus amoebum:*event-bus*)
        (original-rules amoebum:*permission-rules*)
        (original-supervisor amoebum::*worker-supervisor*))
    (unwind-protect
        (progn
          (setf amoebum:*event-bus* (amoebum:make-event-bus :capacity 64)
                amoebum::*worker-supervisor* (make-instance 'amoebum::in-process-supervisor))
          ;; Clear workers
          (amoebum:clear-workers)
          (let* ((context (amoebum:make-amoebum-context
                           :permission-mode :full-auto
                           :event-bus amoebum:*event-bus*
                           :initialize-notifications-p nil))
                 (call (pseudopod:make-tool-call
                        :id "list-empty"
                        :name "list-workers-tool"
                        :arguments "{}"))
                 (result (amoebum:execute-tool call context)))
            (is (stringp result))
            (is (search "\"count\": 0" result :test #'char-equal))))
      (setf amoebum:*toolset* original-toolset
            amoebum:*tool-metadata* original-metadata
            amoebum:*event-bus* original-event-bus
            amoebum:*permission-rules* original-rules
            amoebum::*worker-supervisor* original-supervisor))))

(test fan-out-join-round-trip
  "fan-out + join lifecycle produces results for all workers."
  (let ((original-event-bus amoebum:*event-bus*)
        (original-supervisor amoebum::*worker-supervisor*))
    (unwind-protect
        (progn
          (setf amoebum:*event-bus* (amoebum:make-event-bus :capacity 64)
                amoebum::*worker-supervisor* (make-instance 'amoebum::in-process-supervisor))
          (amoebum:clear-workers)
          (amoebum:clear-worker-groups)
          ;; Fan out 2 shell workers
          (multiple-value-bind (group-id worker-ids)
              (amoebum:fan-out-workers
               (list (list :type :shell :command "echo hello" :label "echo-1")
                     (list :type :shell :command "echo world" :label "echo-2"))
               :timeout-seconds 30)
            (is (stringp group-id))
            (is (= 2 (length worker-ids)))
            ;; Join and collect results
            (let ((results (amoebum:join-worker-group group-id)))
              (is (= 2 (length results)))
              ;; Each result is (worker-id status result)
              (dolist (triple results)
                (is (stringp (first triple)))
                (is (member (second triple)
                            '(:completed :failed :timeout :cancelled)
                            :test #'eq))))))
      (setf amoebum:*event-bus* original-event-bus
            amoebum::*worker-supervisor* original-supervisor))))

(test agent-tools-registered
  "All 8 agent tools should be registered in the toolset."
  (let ((expected-tools '("spawn-agent-worker"
                          "fan-out-workers"
                          "join-workers"
                          "race-workers"
                          "worker-status-tool"
                          "worker-result-tool"
                          "worker-cancel-tool"
                          "list-workers-tool")))
    (dolist (tool-name expected-tools)
      (is-true (pseudopod:find-tool amoebum:*toolset* tool-name)
               "Expected tool ~A to be registered." tool-name))))

(test agent-tools-metadata-category
  "Agent tools should have :agents category in metadata."
  (let ((tool-names '("spawn-agent-worker" "fan-out-workers" "join-workers"
                      "race-workers" "worker-status-tool" "worker-result-tool"
                      "worker-cancel-tool" "list-workers-tool")))
    (dolist (name tool-names)
      (let ((meta (gethash name amoebum:*tool-metadata*)))
        (is-true (amoebum:tool-metadata-p meta)
                 "Expected metadata for ~A." name)
        (when (amoebum:tool-metadata-p meta)
          (is (eq :agents (amoebum:tool-metadata-category meta))
              "Expected :agents category for ~A, got ~A."
              name (amoebum:tool-metadata-category meta)))))))
