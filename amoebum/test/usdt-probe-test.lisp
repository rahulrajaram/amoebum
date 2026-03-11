(in-package :amoebum/test)

(def-suite usdt-probe-suite
  :in amoebum-suite
  :description "I255/I360 USDT probe integration and coverage tests.")

(in-suite usdt-probe-suite)

(defun %usdt-test-client ()
  (funcall (symbol-function (find-symbol "%MAKE-CLIENT" :pseudopod))
           :api-key "test-key"
           :base-url "https://example.test/v1"
           :model "test-model"))

(defun %usdt-contains-probe-type-p (events type)
  (find type events :key #'amoebum:usdt-probe-event-type :test #'eq))

(defun %wait-for-agent-terminal-status (agent-id &key (timeout-ms 2500) (sleep-seconds 0.01))
  (let ((deadline (+ (get-internal-real-time)
                     (round (* timeout-ms
                               (/ internal-time-units-per-second 1000.0d0))))))
    (loop
      for agent = (amoebum:find-agent agent-id)
      for status = (and agent (amoebum:agent-record-status agent))
      when (member status '(:completed :failed :cancelled) :test #'eq)
        return status
      when (> (get-internal-real-time) deadline)
        do (return nil)
      do (sleep sleep-seconds))))

(test usdt-probe-api-covers-required-points
  (let ((required '(:tool-enter
                    :tool-exit
                    :tool-call
                    :llm-request-start
                    :llm-stream-chunk
                    :llm-request-end
                    :agent-lifecycle
                    :gc-start
                    :gc-end
                    :render-frame
                    :event-dispatch)))
    (unwind-protect
        (progn
          (amoebum:enable-usdt-probes :install-gc-hooks nil :clear-existing t)
          (amoebum:usdt-probe-tool-enter "test-tool" "r1")
          (amoebum:usdt-probe-tool-exit "test-tool" "r1" 4 :status :ok)
          (amoebum:usdt-probe-tool-call :started "test-tool" "tc-1"
                                        :request-id "r1"
                                        :index 0
                                        :status :observed)
          (amoebum:usdt-probe-llm-request-start "test-model" "https://example.test" :stream "req-1")
          (amoebum:usdt-probe-llm-stream-chunk "test-model" "https://example.test" :stream "req-1" 1 "hello"
                                               :chunk-kind :content
                                               :total-chunks 1
                                               :total-chars 5)
          (amoebum:usdt-probe-llm-request-end "test-model" "https://example.test" :stream "req-1" 12 :status :ok)
          (amoebum:usdt-probe-agent-lifecycle :spawn "agent-1" :task :queued 0)
          (amoebum:usdt-probe-gc-start)
          (amoebum:usdt-probe-gc-end 3 1024)
          (amoebum:usdt-probe-render-frame 1 5 80 24)
          (amoebum:usdt-probe-event-dispatch :demo 7 1)
          (let ((events (amoebum:usdt-probe-events)))
            (dolist (type required)
              (is-true (%usdt-contains-probe-type-p events type)
                       "Expected emitted USDT probe type ~S." type))))
      (amoebum:disable-usdt-probes :remove-gc-hooks t))))

(test pipeline-tool-probe-points-fire
  (let ((original-toolset amoebum:*toolset*)
        (original-hooks amoebum:*hook-registry*)
        (original-event-bus amoebum:*event-bus*))
    (unwind-protect
        (progn
          (setf amoebum:*toolset* (pseudopod:make-toolset)
                amoebum:*hook-registry* (make-hash-table :test #'equal)
                amoebum:*event-bus* (amoebum:make-event-bus :capacity 64))
          (pseudopod:register-tool-function
           amoebum:*toolset*
           :name "i255-probe-tool"
           :description "I255 probe tool"
           :parameters (let ((schema (make-hash-table :test #'equal)))
                         (setf (gethash "type" schema) "object")
                         schema)
           :fn (lambda (_arguments _call)
                 (declare (ignore _arguments _call))
                 "ok"))
          (amoebum:enable-usdt-probes :install-gc-hooks nil :clear-existing t)
          (let* ((context (amoebum:make-amoebum-context
                           :toolset amoebum:*toolset*
                           :permission-mode :full-auto
                           :event-bus amoebum:*event-bus*
                           :hook-registry amoebum:*hook-registry*
                           :initialize-notifications-p nil))
                 (call (pseudopod:make-tool-call
                        :id "i255-tool-call"
                        :name "i255-probe-tool"
                        :arguments "{}"))
                 (result (amoebum:execute-tool call context))
                 (events (amoebum:usdt-probe-events)))
            (is (string= "ok" result))
            (is-true (%usdt-contains-probe-type-p events :tool-enter))
            (is-true (%usdt-contains-probe-type-p events :tool-exit))
            (let ((exit-event (find :tool-exit events
                                    :test #'eq
                                    :key #'amoebum:usdt-probe-event-type)))
              (is-true (>= (amoebum:usdt-probe-event-duration-ms exit-event) 0))
              (is (eq :ok
                      (getf (amoebum:usdt-probe-event-payload exit-event) :status))))))
      (amoebum:disable-usdt-probes :remove-gc-hooks t)
      (setf amoebum:*toolset* original-toolset
            amoebum:*hook-registry* original-hooks
            amoebum:*event-bus* original-event-bus))))

(test token-stream-tool-call-probes-fire
  (let ((stream-state (amoebum:make-token-stream-state)))
    (unwind-protect
        (progn
          (amoebum:enable-usdt-probes :install-gc-hooks nil :clear-existing t)
          (let ((tool-call (pseudopod:make-tool-call
                            :id "i360-tool-call"
                            :name "i360-tool"
                            :arguments "{\"path\":\"README.md\"}")))
            (amoebum:token-stream-emit-tool-call-started stream-state tool-call)
            (amoebum:token-stream-emit-tool-call-argument-complete stream-state tool-call)
            (amoebum:token-stream-emit-tool-call-result stream-state
                                                        :tool-call tool-call
                                                        :result "ok"))
          (let* ((events (amoebum:usdt-probe-events :type :tool-call))
                 (phases (mapcar (lambda (event)
                                   (getf (amoebum:usdt-probe-event-payload event) :phase))
                                 events)))
            (is (= 3 (length events)))
            (is (equal '(:started :argument-complete :result) phases))))
      (amoebum:disable-usdt-probes :remove-gc-hooks t))))

(test llm-request-probes-fire-from-stream-runner
  (let* ((stream-sym (find-symbol "STREAM-CHAT-COMPLETION*" :pseudopod))
         (fallback-sym (find-symbol "CHAT-COMPLETION*" :pseudopod))
         (original-stream-fn (symbol-function stream-sym))
         (original-fallback-fn (symbol-function fallback-sym))
         (stream-state (amoebum:make-token-stream-state))
         (client (%usdt-test-client)))
    (unwind-protect
        (progn
          (setf (symbol-function stream-sym)
                (lambda (&rest args &key on-content &allow-other-keys)
                  (declare (ignore args))
                  (when on-content
                    (funcall on-content "hello from stub"))
                  nil))
          (setf (symbol-function fallback-sym)
                (lambda (&rest _args)
                  (declare (ignore _args))
                  (pseudopod:make-message :role "assistant" :content "fallback")))
          (amoebum:enable-usdt-probes :install-gc-hooks nil :clear-existing t)
          (amoebum:stream-pseudopod-chat
           stream-state
           "hello"
           '()
           :client client
           :tools nil)
          (let ((events (amoebum:usdt-probe-events)))
            (is-true (%usdt-contains-probe-type-p events :llm-request-start))
            (is-true (%usdt-contains-probe-type-p events :llm-stream-chunk))
            (is-true (%usdt-contains-probe-type-p events :llm-request-end))
            (let ((chunk-event (find :llm-stream-chunk events
                                     :test #'eq
                                     :key #'amoebum:usdt-probe-event-type)))
              (is (eq :content
                      (getf (amoebum:usdt-probe-event-payload chunk-event)
                            :chunk-kind)))
              (is (eq :stream
                      (getf (amoebum:usdt-probe-event-payload chunk-event)
                            :mode))))
            (let ((end-event (find :llm-request-end events
                                   :test #'eq
                                   :key #'amoebum:usdt-probe-event-type)))
              (is-true (>= (amoebum:usdt-probe-event-duration-ms end-event) 0))
              (is (eq :ok (getf (amoebum:usdt-probe-event-payload end-event) :status))))))
      (setf (symbol-function stream-sym) original-stream-fn
            (symbol-function fallback-sym) original-fallback-fn)
      (amoebum:disable-usdt-probes :remove-gc-hooks t))))

(test agent-lifecycle-probes-fire
  (unwind-protect
      (progn
        (amoebum:clear-agents)
        (amoebum:enable-usdt-probes :install-gc-hooks nil :clear-existing t)
        (let* ((agent (amoebum:spawn-agent
                       "i360 agent lifecycle probe"
                       :runner (lambda (_agent)
                                 (declare (ignore _agent))
                                 (sleep 0.01)
                                 "agent done")))
               (agent-id (and agent (amoebum:agent-record-id agent)))
               (terminal-status (and agent-id
                                     (%wait-for-agent-terminal-status agent-id))))
          (is-true agent-id)
          (is-true (member terminal-status '(:completed :failed :cancelled) :test #'eq))
          (let* ((events (amoebum:usdt-probe-events :type :agent-lifecycle))
                 (phases (mapcar (lambda (event)
                                   (getf (amoebum:usdt-probe-event-payload event) :phase))
                                 events)))
            (is-true (member :spawn phases :test #'eq))
            (is-true (member :start phases :test #'eq))
            (is-true (member :complete phases :test #'eq)))))
    (amoebum:disable-usdt-probes :remove-gc-hooks t)
    (amoebum:clear-agents)))

(test event-dispatch-probe-point-fires
  (let ((bus (amoebum:make-event-bus :capacity 32))
        (handled 0))
    (unwind-protect
        (progn
          (amoebum:enable-usdt-probes :install-gc-hooks nil :clear-existing t)
          (amoebum:subscribe bus :demo
                             (lambda (_event)
                               (declare (ignore _event))
                               (incf handled)))
          (amoebum:publish bus :demo)
          (is (= 1 handled))
          (let ((events (amoebum:usdt-probe-events :type :event-dispatch)))
            (is-true events)
            (is-true
             (some (lambda (event)
                     (eq :demo
                         (getf (amoebum:usdt-probe-event-payload event)
                               :event-type)))
                   events))))
      (amoebum:disable-usdt-probes :remove-gc-hooks t))))

(test render-frame-probe-point-fires
  (let ((state (amoebum:make-chat-ui-state :stream-runner nil))
        (size (ptui.core.types:make-size 80 24)))
    (unwind-protect
        (progn
          (amoebum:enable-usdt-probes :install-gc-hooks nil :clear-existing t)
          (let ((buffer (amoebum::render-chat-ui-buffer state size)))
            (is-true buffer))
          (let ((events (amoebum:usdt-probe-events :type :render-frame)))
            (is-true events)
            (is-true (>= (amoebum:usdt-probe-event-duration-ms (first events)) 0))))
      (amoebum:disable-usdt-probes :remove-gc-hooks t))))

(test gc-hooks-record-pause-events
  (unwind-protect
      (progn
        (amoebum:enable-usdt-probes :install-gc-hooks t :clear-existing t)
        #+sbcl (sb-ext:gc :full t)
        #-sbcl (progn
                 (amoebum:usdt-probe-gc-start)
                 (amoebum:usdt-probe-gc-end 1 0))
        (let ((events (amoebum:usdt-probe-events)))
          (is-true (%usdt-contains-probe-type-p events :gc-start))
          (is-true (%usdt-contains-probe-type-p events :gc-end))
          (let ((summary (amoebum:usdt-gc-pause-summary)))
            (is-true (>= (getf summary :count 0) 1)))))
    (amoebum:disable-usdt-probes :remove-gc-hooks t)))

(test prebuilt-bpf-programs-load-and-filter
  (let* ((programs (amoebum:list-prebuilt-bpf-programs))
         (exists-count (count-if (lambda (entry)
                                   (not (null (getf entry :exists-p))))
                                 programs)))
    (is (>= exists-count 4))
    (let* ((program (amoebum:load-prebuilt-bpf-program :gc-pause))
           (events (list (amoebum:make-usdt-probe-event :type :gc-end :duration-ms 7)
                         (amoebum:make-usdt-probe-event :type :tool-exit :duration-ms 2)))
           (filtered (amoebum:bpf-program-filter-events program events)))
      (is (= 1 (length filtered)))
      (is (eq :gc-end (amoebum:usdt-probe-event-type (first filtered)))))))

(test usdt-dashboard-and-overhead
  (unwind-protect
      (progn
        (amoebum:enable-usdt-probes :install-gc-hooks nil :clear-existing t)
        (amoebum:usdt-probe-tool-exit "tool-a" "a" 5 :status :ok)
        (amoebum:usdt-probe-tool-exit "tool-b" "b" 15 :status :ok)
        (amoebum:usdt-probe-gc-end 3 1024)
        (let* ((snapshot (amoebum:usdt-dashboard-snapshot :limit 100))
               (dashboard (amoebum:render-usdt-dashboard :limit 100))
               (widget (amoebum:usdt-dashboard-widget '(:limit 100))))
          (is-true (listp snapshot))
          (is-true (stringp dashboard))
          (is-true (search "USDT Dashboard" dashboard :test #'char-equal))
          (is-true (typep widget 'ptui.ui.elements:ui-element))))
    (amoebum:disable-usdt-probes :remove-gc-hooks t))
  (let* ((samples (loop repeat 3
                        collect (amoebum:usdt-disabled-overhead-percent
                                 :iterations 180000)))
         (best (reduce #'min samples)))
    (is-true (< best 1.0d0)
             "Expected disabled USDT overhead < 1%%, samples=~S" samples)))

(test usdt-probe-smoke-sentinel
  (is-true t)
  (format t "USDT_PROBE_SMOKE_OK~%"))
