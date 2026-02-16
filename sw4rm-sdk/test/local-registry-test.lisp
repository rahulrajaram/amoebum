;;;; local-registry-test.lisp - SW4RM local registry tests

(in-package :sw4rm-test)

(def-suite local-registry-suite
  :description "SW4RM local registry tests"
  :in sw4rm-suite)
(in-suite local-registry-suite)

(defun %agent-config-with-capabilities (agent-id capabilities)
  (sw4rm-sdk:make-agent-config
   :agent-id agent-id
   :name (format nil "agent-~A" agent-id)
   :capabilities capabilities))

(test local-registry-register-and-retrieve
  "Registering an agent should make it retrievable and listable."
  (let ((registry (sw4rm-sdk:make-local-registry)))
    (is (eq 0 (sw4rm-sdk:local-registry-size registry)))
    (let ((cfg (%agent-config-with-capabilities "agent-1" '("analyze" "edit"))))
      (is (eq cfg (sw4rm-sdk:local-registry-register registry cfg)))
      (is (string= "agent-1" (sw4rm-sdk:agent-config-agent-id
                               (sw4rm-sdk:local-registry-get registry "agent-1"))))
      (is (= 1 (length (sw4rm-sdk:local-registry-list registry)))))))

(test local-registry-duplicate-registration
  "Duplicate registration should signal duplicate-agent-registration."
  (let ((registry (sw4rm-sdk:make-local-registry))
        (cfg (%agent-config-with-capabilities "agent-dup" '("review"))))
    (sw4rm-sdk:local-registry-register registry cfg)
    (signals sw4rm-sdk:duplicate-agent-registration
      (sw4rm-sdk:local-registry-register registry cfg))
    (let ((cfg-ignored (%agent-config-with-capabilities "agent-dup" '("plan"))))
      (is (eq cfg
              (sw4rm-sdk:local-registry-register registry cfg-ignored :if-exists :ignore)))
      (is (eq cfg-ignored
              (sw4rm-sdk:local-registry-register registry cfg-ignored :if-exists :replace))))))

(test local-registry-capability-lookup
  "find-agents-by-capability should return matching agents."
  (let ((registry (sw4rm-sdk:make-local-registry)))
    (sw4rm-sdk:local-registry-register
     registry (%agent-config-with-capabilities "agent-a" '("review" "edit")))
    (sw4rm-sdk:local-registry-register
     registry (%agent-config-with-capabilities "agent-b" '("analysis" "review")))
    (sw4rm-sdk:local-registry-register
     registry (%agent-config-with-capabilities "agent-c" '("analysis")))
    (let ((results (sw4rm-sdk:find-agents-by-capability registry "review")))
      (is (= 2 (length results)))
      (is (member "agent-a" (mapcar #'sw4rm-sdk:agent-config-agent-id results) :test #'string=))
      (is (member "agent-b" (mapcar #'sw4rm-sdk:agent-config-agent-id results) :test #'string=))
      (is (null (sw4rm-sdk:find-agents-by-capability registry "nonexistent"))))))

(test local-registry-unregister
  "Unregister should remove the agent from lookup and list."
  (let ((registry (sw4rm-sdk:make-local-registry)))
    (let ((cfg1 (%agent-config-with-capabilities "agent-1" '("edit")))
          (cfg2 (%agent-config-with-capabilities "agent-2" '("plan"))))
      (sw4rm-sdk:local-registry-register registry cfg1)
      (sw4rm-sdk:local-registry-register registry cfg2)
      (is (= 2 (sw4rm-sdk:local-registry-size registry)))
      (is (eq t (sw4rm-sdk:local-registry-unregister registry "agent-1")))
      (is (= 1 (sw4rm-sdk:local-registry-size registry)))
      (is (null (sw4rm-sdk:local-registry-get registry "agent-1")))
      (is (null (sw4rm-sdk:local-registry-unregister registry "agent-missing"))))))

(test local-registry-clear-and-touch
  "Registry can clear all entries and refresh heartbeat timestamp."
  (let ((registry (sw4rm-sdk:make-local-registry)))
    (sw4rm-sdk:local-registry-register
     registry (%agent-config-with-capabilities "agent-1" '("touch")))
    (is (eq t (sw4rm-sdk:local-registry-touch registry "agent-1")))
    (is (= 1 (sw4rm-sdk:local-registry-size registry)))
    (is (eq t (sw4rm-sdk:local-registry-clear registry)))
    (is (= 0 (sw4rm-sdk:local-registry-size registry)))))
