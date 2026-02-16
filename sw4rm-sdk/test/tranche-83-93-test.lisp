;;;; tranche-83-93-test.lisp
;;;; Coverage for SW4RM SDK tranche work I83-I93.

(in-package :sw4rm-test)

(def-suite sw4rm-tranche-83-93-suite
  :description "SW4RM tranche I83-I93 tests"
  :in sw4rm-suite)
(in-suite sw4rm-tranche-83-93-suite)

(defvar *interceptor-events* nil)
(defvar *activity-events* nil)
(defvar *macro-workflow* nil)

;;; ---------------------------------------------------------------------------
;;; I83 + I84
;;; ---------------------------------------------------------------------------

(test state-machine-serialize-deserialize-roundtrip
  (let ((sm (make-instance 'sw4rm-sdk::agent-state-machine)))
    (sw4rm-sdk::transition-to sm :runnable)
    (sw4rm-sdk::transition-to sm :scheduled)
    (let* ((serialized (sw4rm-sdk:serialize-agent-state sm))
           (restored (sw4rm-sdk:deserialize-agent-state serialized)))
      (is (eq :scheduled (sw4rm-sdk::current-state restored)))
      (is (>= (length (sw4rm-sdk::transition-history restored)) 2)))))

(test state-machine-use-force-transition-restart
  (let ((sm (make-instance 'sw4rm-sdk::agent-state-machine)))
    (handler-bind
        ((sw4rm-sdk:state-transition-error
           (lambda (condition)
             (declare (ignore condition))
             (invoke-restart 'sw4rm-sdk::use-force-transition))))
      (sw4rm-sdk::transition-to sm :completed))
    (is (eq :completed (sw4rm-sdk::current-state sm)))))

(test envelope-message-log-ring-buffer
  (let ((log (sw4rm-sdk:make-message-log :capacity 2)))
    (sw4rm-sdk:log-envelope
     log
     (sw4rm-sdk:make-envelope
      :source-agent-id "agent-a"
      :target-agent-id "agent-b"
      :message-type sw4rm-sdk:+data+))
    (sw4rm-sdk:log-envelope
     log
     (sw4rm-sdk:make-envelope
      :source-agent-id "agent-a"
      :target-agent-id "agent-c"
      :message-type sw4rm-sdk:+control+))
    (sw4rm-sdk:log-envelope
     log
     (sw4rm-sdk:make-envelope
      :source-agent-id "agent-d"
      :target-agent-id "agent-e"
      :message-type sw4rm-sdk:+notification+))
    (is (= 2 (sw4rm-sdk:message-log-size log)))
    (let ((from-a (sw4rm-sdk:query-message-log log :source-agent-id "agent-a")))
      (is (= 1 (length from-a))))))

;;; ---------------------------------------------------------------------------
;;; I85 + I86 + I87 + I88
;;; ---------------------------------------------------------------------------

(defclass ordered-test-interceptor ()
  ((name :initarg :name :reader ordered-test-interceptor-name)
   (sink :initarg :sink :reader ordered-test-interceptor-sink)))

(defmethod sw4rm-sdk::interceptor-name ((interceptor ordered-test-interceptor))
  (ordered-test-interceptor-name interceptor))

(defmethod sw4rm-sdk::on-request ((interceptor ordered-test-interceptor) request context)
  (declare (ignore context))
  (push (list :request (ordered-test-interceptor-name interceptor))
        (symbol-value (ordered-test-interceptor-sink interceptor)))
  request)

(defmethod sw4rm-sdk::on-response ((interceptor ordered-test-interceptor) response context)
  (declare (ignore context))
  (push (list :response (ordered-test-interceptor-name interceptor))
        (symbol-value (ordered-test-interceptor-sink interceptor)))
  response)

(test interceptor-ordering-request-forward-response-reverse
  (let* ((*interceptor-events* nil)
         (chain (sw4rm-sdk::make-interceptor-chain))
         (a (make-instance 'ordered-test-interceptor :name "a" :sink '*interceptor-events*))
         (b (make-instance 'ordered-test-interceptor :name "b" :sink '*interceptor-events*)))
    (sw4rm-sdk::add-interceptor chain a)
    (sw4rm-sdk::add-interceptor chain b)
    (sw4rm-sdk::process-request chain '(:x 1))
    (sw4rm-sdk::process-response chain '(:y 2))
    (is (equal (reverse *interceptor-events*)
               '((:request "a")
                 (:request "b")
                 (:response "b")
                 (:response "a"))))))

(test budget-split-and-exhaustion-restart
  (let ((budget (sw4rm-sdk:make-budget-envelope
                 :token-budget-remaining 1
                 :wall-time-remaining-ms 1000
                 :deadline-epoch-ms (+ (sw4rm-sdk::budget-now-ms) 1000)
                 :max-delegation-depth 2
                 :emergency-token-reserve 5)))
    (handler-bind
        ((sw4rm-sdk:budget-exhausted
           (lambda (condition)
             (declare (ignore condition))
             (invoke-restart 'sw4rm-sdk::extend-budget 5 0))))
      (sw4rm-sdk:decrement-budget budget :tokens 10 :wall-time-ms 10))
    (is (= 0 (sw4rm-sdk::budget-envelope-token-budget-remaining budget)))
    (is (= 1 (sw4rm-sdk::budget-envelope-emergency-token-reserve budget)))
    (multiple-value-bind (child parent)
        (sw4rm-sdk:split-budget budget :tokens 1 :wall-time-ms 100)
      (is (= 1 (sw4rm-sdk::budget-envelope-token-budget-remaining child)))
      (is (<= (sw4rm-sdk::budget-envelope-token-budget-remaining parent) 0)))))

(test local-router-fair-scheduling-and-dead-letter
  (let ((router (sw4rm-sdk:make-local-router :queue-capacity 1)))
    (sw4rm-sdk:register-route router "agent-a")
    (sw4rm-sdk:register-route router "agent-b")
    (sw4rm-sdk:route-envelope
     router
     (sw4rm-sdk:make-envelope
      :source-agent-id "source"
      :target-agent-id "agent-a"
      :message-type sw4rm-sdk:+data+))
    (signals sw4rm-sdk:queue-full-error
      (sw4rm-sdk:route-envelope
       router
       (sw4rm-sdk:make-envelope
        :source-agent-id "source"
        :target-agent-id "agent-a"
        :message-type sw4rm-sdk:+data+)))
    (sw4rm-sdk:route-envelope
     router
     (sw4rm-sdk:make-envelope
      :source-agent-id "source"
      :target-agent-id "agent-b"
      :message-type sw4rm-sdk:+data+))
    (multiple-value-bind (first-agent _)
        (sw4rm-sdk:schedule-next-envelope router)
      (declare (ignore _))
      (is (string= "agent-a" first-agent)))
    (multiple-value-bind (second-agent _)
        (sw4rm-sdk:schedule-next-envelope router)
      (declare (ignore _))
      (is (string= "agent-b" second-agent)))
    (handler-case
        (sw4rm-sdk:route-envelope
         router
         (sw4rm-sdk:make-envelope
          :source-agent-id "source"
          :target-agent-id "unknown-agent"
          :message-type sw4rm-sdk:+data+))
      (sw4rm-sdk:sw4rm-error () nil))
    (is (= 2 (length (sw4rm-sdk:dead-letter-entries router))))))

(test handoff-context-roundtrip-and-restartable-rejection
  (let* ((context '(:task "review" :files ("a.lisp" "b.lisp")))
         (serialized (sw4rm-sdk:serialize-handoff-context context))
         (decoded (sw4rm-sdk:deserialize-handoff-context serialized))
         (attempts 0)
         (response
           (handler-bind
               ((sw4rm-sdk:handoff-rejected
                  (lambda (condition)
                    (declare (ignore condition))
                    (invoke-restart 'sw4rm-sdk::try-next-agent "agent-c"))))
             (sw4rm-sdk:delegate-to-swarm
              (lambda (request)
                (incf attempts)
                (if (string= (getf request :to-agent) "agent-c")
                    (list :accepted t :status :accepted)
                    (list :accepted nil
                          :status :rejected
                          :rejection-code sw4rm-sdk:+no-route+
                          :rejection-reason "missing route")))
              :from-agent "agent-a"
              :to-agent "agent-b"
              :reason "capability-gap"
              :budget (list :deadline-epoch-ms (+ 100000 (sw4rm-sdk::%now-ms))
                            :wall-time-remaining-ms 100000)))))
    (is (stringp serialized))
    (is (not (null decoded)))
    (is (= 2 attempts))
    (is (eq t (getf response :accepted)))))

;;; ---------------------------------------------------------------------------
;;; I89 + I90 + I91 + I92 + I93
;;; ---------------------------------------------------------------------------

(test negotiation-room-approves-and-escalates-deadlock
  (let ((client (make-instance 'sw4rm-sdk:negotiation-room-client
                               :address "localhost:50058")))
    (sw4rm-sdk:create-room client "room-1")
    (sw4rm-sdk:submit-artifact
     client
     (list :artifact-id "artifact-approve"
           :artifact-type :code
           :producer-id "producer"
           :requested-critics '("c1" "c2")
           :negotiation-room-id "room-1"))
    (sw4rm-sdk:add-critique client (list :artifact-id "artifact-approve" :critic-id "c1" :passed t :score 8.0))
    (sw4rm-sdk:add-critique client (list :artifact-id "artifact-approve" :critic-id "c2" :passed t :score 7.0))
    (is (eq :approved (getf (sw4rm-sdk:get-decision client "artifact-approve") :outcome)))

    (sw4rm-sdk:submit-artifact
     client
     (list :artifact-id "artifact-deadlock"
           :artifact-type :code
           :producer-id "producer"
           :requested-critics '("c1" "c2")
           :negotiation-room-id "room-1"))
    (sw4rm-sdk:add-critique client (list :artifact-id "artifact-deadlock" :critic-id "c1" :passed t :score 9.0))
    (sw4rm-sdk:add-critique client (list :artifact-id "artifact-deadlock" :critic-id "c2" :passed nil :score 2.0))
    (is (eq :escalated (getf (sw4rm-sdk:get-decision client "artifact-deadlock") :outcome)))))

(test workflow-engine-executes-dag-and-macro-builds-dependencies
  (let* ((execution-order nil)
         (workflow
           (sw4rm-sdk::make-workflow-definition
            :workflow-id "wf-1"
            :nodes (list
                    (sw4rm-sdk::make-workflow-node
                     :node-id "analyze"
                     :action (lambda (ctx _)
                               (declare (ignore _))
                               (push "analyze" execution-order)
                               (setf (gethash :analysis ctx) t)))
                    (sw4rm-sdk::make-workflow-node
                     :node-id "implement"
                     :depends-on '("analyze")
                     :action (lambda (ctx _)
                               (declare (ignore _))
                               (push "implement" execution-order)
                               (setf (gethash :implemented ctx) (gethash :analysis ctx))))
                    (sw4rm-sdk::make-workflow-node
                     :node-id "test"
                     :depends-on '("implement")
                     :action (lambda (ctx _)
                               (declare (ignore _))
                               (push "test" execution-order)
                               (setf (gethash :tested ctx) (gethash :implemented ctx)))))
            :edges (list
                    (sw4rm-sdk::make-workflow-edge :from-node "analyze" :to-node "implement")
                    (sw4rm-sdk::make-workflow-edge :from-node "implement" :to-node "test"))))
         (run (sw4rm-sdk:execute-workflow workflow)))
    (is (eq :completed (sw4rm-sdk::workflow-run-status run)))
    (is (equal (reverse execution-order) '("analyze" "implement" "test")))
    (sw4rm-sdk:defworkflow *macro-workflow*
      (:node "start")
      (:node "finish" :depends-on ("start")))
    (is (equal '("start" "finish")
               (sw4rm-sdk:topological-sort *macro-workflow*)))))

(test local-hitl-approval-and-timeout-deny
  (let ((gate (sw4rm-sdk:make-local-hitl-gate)))
    (bt:make-thread
     (lambda ()
       (sleep 0.05)
       (sw4rm-sdk:approve-hitl-request gate "req-approved" :notes "ok")))
    (let ((approved (sw4rm-sdk:request-hitl-approval
                     gate
                     :request-id "req-approved"
                     :timeout-ms 500)))
      (is (eq :approved (getf approved :status))))
    (let ((timed-out (sw4rm-sdk:request-hitl-approval
                      gate
                      :request-id "req-timeout"
                      :timeout-ms 20
                      :timeout-action :deny)))
      (is (eq :denied (getf timed-out :status))))))

(test activity-buffer-agent-metadata-and-hooks
  (let* ((*activity-events* nil)
         (buf (make-instance 'sw4rm-sdk::activity-buffer
                             :capacity 4
                             :update-hook (lambda (event-type entry)
                                            (push (list event-type
                                                        (sw4rm-sdk::activity-entry-agent-id entry)
                                                        (sw4rm-sdk::activity-entry-activity-type entry))
                                                  *activity-events*)))))
    (sw4rm-sdk::upsert-activity
     buf
     :agent-id "agent-1"
     :activity-type :tool-call
     :task-id "task-1"
     :repo-id "repo-1"
     :worktree-id "wt-1"
     :description "Invoked formatter")
    (let ((entry (sw4rm-sdk::get-activity buf :task-id "task-1" :repo-id "repo-1" :worktree-id "wt-1")))
      (is (string= "agent-1" (sw4rm-sdk::activity-entry-agent-id entry)))
      (is (eq :tool-call (sw4rm-sdk::activity-entry-activity-type entry))))
    (is (= 1 (length (sw4rm-sdk::list-buffer-activities
                      buf
                      :agent-id "agent-1"
                      :activity-type :tool-call))))
    (sw4rm-sdk::remove-activity buf :task-id "task-1" :repo-id "repo-1" :worktree-id "wt-1")
    (is (= 2 (length *activity-events*)))))

(defun %run-program-ok (directory command)
  (multiple-value-bind (_stdout _stderr status)
      (uiop:run-program command
                        :directory directory
                        :output :string
                        :error-output :string
                        :ignore-error-status t)
    (declare (ignore _stdout _stderr))
    (= status 0)))

(test git-worktree-spawn-collect-kill-and-lock
  (let* ((tmp-root (merge-pathnames
                    (format nil "sw4rm-sdk-test-~A/" (get-universal-time))
                    (uiop:temporary-directory)))
         (repo-root (merge-pathnames "repo/" tmp-root))
         (worktree-path (merge-pathnames "worktrees/w1/" tmp-root))
         (lock-path (merge-pathnames "coord.lock" tmp-root)))
    (unwind-protect
         (progn
           (ensure-directories-exist repo-root)
           (ensure-directories-exist (merge-pathnames "worktrees/" tmp-root))
           (is (%run-program-ok repo-root '("git" "init")))
           (is (%run-program-ok repo-root '("git" "config" "user.email" "sw4rm@example.com")))
           (is (%run-program-ok repo-root '("git" "config" "user.name" "SW4RM Test")))
           (with-open-file (stream (merge-pathnames "README.md" repo-root)
                                   :direction :output
                                   :if-exists :supersede
                                   :if-does-not-exist :create)
             (write-line "# test" stream))
           (is (%run-program-ok repo-root '("git" "add" "README.md")))
           (is (%run-program-ok repo-root '("git" "commit" "-m" "init")))

           (let ((coord (sw4rm-sdk:make-git-worktree-coordinator repo-root)))
             (is (not (null (sw4rm-sdk:spawn-worktree
                             coord
                             "w1"
                             (namestring worktree-path)
                             "feature/w1"
                             :base-ref "HEAD"))))
             (let ((collected (sw4rm-sdk:collect-worktree coord "w1")))
               (is (not (null (getf collected :record)))))
             (is (eq t (sw4rm-sdk:kill-worktree coord "w1" :force t))))

           (sw4rm-sdk:with-worktree-lock ((namestring lock-path))
             (signals sw4rm-sdk:worktree-error
               (sw4rm-sdk:with-worktree-lock ((namestring lock-path))
                 t))))
      (ignore-errors (uiop:delete-directory-tree tmp-root :validate t :if-does-not-exist :ignore)))))
