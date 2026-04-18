;;;; package.lisp - Package definition for SW4RM SDK

(defpackage #:sw4rm-sdk
  (:use #:cl #:alexandria)
  (:documentation "SW4RM Protocol SDK for Common Lisp.

This package provides a full peer implementation of the SW4RM protocol for
interruptible, message-driven agent coordination.

The SDK implements:
- Message envelope construction with Three-ID model (message_id, correlation_id, idempotency_token)
- Comprehensive error condition hierarchy with restart support
- Agent lifecycle state machine (INITIALIZING -> RUNNABLE -> SCHEDULED -> RUNNING -> COMPLETED/FAILED)
- Activity buffer for tracking in-flight operations
- Idempotency guarantees via token-based deduplication
- Worktree state management for isolated Git operations
- Negotiation protocol support for multi-agent coordination
- Policy-based access control and audit trails
- gRPC client infrastructure for all SW4RM services

See documentation/protocol/spec.md for the canonical protocol specification.")

  ;; Constants - All protocol enums and defaults
  (:export
   ;; MessageType enum
   #:+message-type-unspecified+
   #:+control+
   #:+data+
   #:+heartbeat+
   #:+notification+
   #:+acknowledgement+
   #:+hitl-invocation+
   #:+worktree-control+
   #:+negotiation+
   #:+tool-call+
   #:+tool-result+
   #:+tool-error+

   ;; AckStage enum
   #:+ack-stage-unspecified+
   #:+received+
   #:+read+
   #:+fulfilled+
   #:+rejected+
   #:+failed+
   #:+timed-out+

   ;; ErrorCode enum
   #:+error-code-unspecified+
   #:+buffer-full+
   #:+no-route+
   #:+ack-timeout+
   #:+agent-unavailable+
   #:+agent-shutdown+
   #:+validation-error+
   #:+permission-denied+
   #:+unsupported-message-type+
   #:+oversize-payload+
   #:+tool-timeout+
   #:+partial-delivery+
   #:+forced-preemption+
   #:+ttl-expired+
   #:+duplicate-detected+
   #:+already-in-progress+
   #:+overloaded+
   #:+redirect+
   #:+internal-error+

   ;; AgentState enum
   #:+agent-state-unspecified+
   #:+initializing+
   #:+runnable+
   #:+scheduled+
   #:+running+
   #:+waiting+
   #:+waiting-resources+
   #:+suspended+
   #:+resumed+
   #:+completed+
   #:+agent-failed+
   #:+shutting-down+
   #:+recovering+

   ;; CommunicationClass enum
   #:+communication-class-unspecified+
   #:+privileged+
   #:+standard+
   #:+bulk+

   ;; DebateIntensity enum
   #:+debate-intensity-unspecified+
   #:+lowest+
   #:+low+
   #:+medium+
   #:+high+
   #:+highest+

   ;; HitlReasonType enum
   #:+hitl-reason-unspecified+
   #:+conflict+
   #:+security-approval+
   #:+task-escalation+
   #:+manual-override+
   #:+worktree-override+
   #:+debate-deadlock+
   #:+tool-privilege-escalation+
   #:+connector-approval+

   ;; EnvelopeState enum
   #:+envelope-state-unspecified+
   #:+sent+
   #:+received-envelope+
   #:+read-envelope+
   #:+fulfilled-envelope+
   #:+rejected-envelope+
   #:+failed-envelope+
   #:+timed-out-envelope+

   ;; WorktreeState enum
   #:+worktree-state-unspecified+
   #:+unbound+
   #:+bound-home+
   #:+switch-pending+
   #:+bound-non-home+
   #:+bind-failed+

   ;; Default ports and addresses
   #:+default-router-port+
   #:+default-registry-port+
   #:+default-scheduler-port+
   #:+default-hitl-port+
   #:+default-worktree-port+
   #:+default-tool-port+
   #:+default-connector-port+
   #:+default-negotiation-port+
   #:+default-reasoning-port+
   #:+default-logging-port+
   #:*default-host*)

  ;; Errors - Condition hierarchy
  (:export
   #:sw4rm-error
   #:sw4rm-error-message
   #:sw4rm-error-error-code

   #:rpc-error
   #:rpc-error-status-code
   #:rpc-error-details

   #:validation-error
   #:validation-error-field
   #:validation-error-constraint

   #:state-transition-error
   #:state-transition-error-from-state
   #:state-transition-error-to-state
   #:state-transition-error-allowed-transitions

   #:timeout-error
   #:timeout-error-operation
   #:timeout-error-timeout-ms

   #:buffer-full-error
   #:buffer-full-error-current-size
   #:buffer-full-error-max-size

   #:negotiation-error
   #:negotiation-error-negotiation-id
   #:negotiation-error-phase

   #:worktree-error
   #:worktree-error-worktree-id
   #:worktree-error-state

   #:duplicate-detected-error
   #:duplicate-detected-error-idempotency-token

   ;; Error handling macros
   #:with-sw4rm-error-handling
   #:invoke-sw4rm-restart)

  ;; Config - Configuration structures
  (:export
   #:endpoints
   #:make-endpoints
   #:endpoints-router
   #:endpoints-registry
   #:endpoints-scheduler
   #:endpoints-hitl
   #:endpoints-worktree
   #:endpoints-tool
   #:endpoints-connector
   #:endpoints-negotiation
   #:endpoints-reasoning
   #:endpoints-logging

   #:agent-config
   #:make-agent-config
   #:agent-config-agent-id
   #:agent-config-name
   #:agent-config-description
   #:agent-config-version
   #:agent-config-capabilities
   #:agent-config-endpoints
   #:agent-config-timeout-ms
   #:agent-config-retry-max-attempts
   #:agent-config-heartbeat-interval-ms
   #:agent-config-model-preference
   #:agent-config-budget-envelope
   #:agent-config-interceptor-chain

   #:make-default-endpoints
   #:load-config-from-env)

  ;; Local registry
  (:export
   #:duplicate-agent-registration
   #:duplicate-agent-registration-agent-id
   #:duplicate-agent-registration-existing-config
   #:duplicate-agent-registration-attempted-config
   #:provider-secret-access-denied
   #:provider-secret-access-denied-requester-agent-id
   #:provider-secret-access-denied-target-agent-id
   #:provider-secret-access-denied-provider-key
   #:make-local-registry
   #:local-registry
   #:local-registry-entry
   #:local-registry-entry-agent-id
   #:local-registry-entry-config
   #:local-registry-entry-capabilities
   #:local-registry-entry-registered-at
   #:local-registry-entry-last-seen-at
   #:local-registry-entry-metadata
   #:local-registry-register
   #:local-registry-unregister
   #:local-registry-get
   #:local-registry-get-entry
   #:local-registry-list
   #:local-registry-size
   #:local-registry-clear
   #:find-agents-by-capability
   #:local-registry-touch
   #:local-registry-set-provider-secret
   #:local-registry-resolve-provider-secret
   #:local-registry-list-provider-secret-keys
   #:local-registry-clear-provider-secrets)

  ;; Envelope - Message construction
  (:export
   #:make-envelope
   #:update-envelope-state
   #:terminal-state-p
   #:compute-deterministic-hash
   #:make-idempotency-token

   #:sequence-tracker
   #:make-sequence-tracker
   #:next-sequence)

  ;; Negotiation events
  (:export
   #:make-event-emitter
   #:on
   #:off
   #:emit
   #:listener-count
   #:get-history
   #:clear-history
   #:clear-listeners
   #:negotiation-event
   #:negotiation-event-event-id
   #:negotiation-event-event-type
   #:negotiation-event-room-id
   #:negotiation-event-agent-id
   #:negotiation-event-timestamp
   #:negotiation-event-payload
   #:make-participant-joined-event
   #:make-participant-left-event
   #:make-proposal-submitted-event
   #:make-critique-added-event
   #:make-vote-cast-event
   #:make-round-complete-event
   #:make-approved-event
   #:make-rejected-event)

  ;; Voting
  (:export
   #:make-vote
   #:vote-agent-id
   #:vote-choice
   #:vote-confidence
   #:vote-timestamp
   #:vote-metadata
   #:vote-to-plist
   #:make-vote-from-plist
   #:aggregate
   #:strategy-name
   #:majority-vote-strategy
   #:confidence-weighted-strategy
   #:simple-average-strategy
   #:borda-count-strategy
   #:voting-aggregator
   #:run-vote
   #:set-strategy
   #:get-round-history
   #:voting-round
   #:voting-round-round-id
   #:voting-round-strategy-name
   #:voting-round-votes)

  ;; Secrets
  (:export
   #:make-file-backend
   #:make-env-backend
   #:make-default-resolver
   #:get-secret
   #:set-secret
   #:delete-secret
   #:list-secrets
   #:add-backend
   #:remove-backend
   #:resolve-secret
   #:store-secret
   #:secret-resolver
   #:secret-not-found
   #:secret-key
   #:secret-backend-error)

  ;; Gateway redirect emitter (SW4-005 local gateway helper)
  (:export
   #:+registration-type-standard-agent+
   #:+registration-type-swarm-gateway+
   #:+default-peer-liveness-threshold-ms+
   #:+non-serving-agent-states+
   #:gateway-peer-descriptor
   #:make-gateway-peer-descriptor
   #:gateway-peer-descriptor-agent-id
   #:gateway-peer-descriptor-registration-type
   #:gateway-peer-descriptor-capabilities
   #:gateway-redirect-emitter
   #:set-peer-descriptors
   #:update-peer-runtime-state
   #:touch-peer-heartbeat
   #:record-peer-overloaded
   #:emit-overloaded-response)

  ;; Handoff client (SW4-004/SW4-005 local surface)
  (:export
   #:handoff-client
   #:handoff-request
   #:make-handoff-request
   #:handoff-request-request-id
   #:handoff-request-from-agent
   #:handoff-request-to-agent
   #:handoff-request-reason
   #:handoff-request-budget
   #:handoff-request-delegation-policy
   #:handoff-request-context-snapshot
   #:handoff-request-capabilities-required
   #:handoff-request-priority
   #:handoff-request-timeout-ms
   #:+default-max-retries-on-overloaded+
   #:+default-initial-backoff-ms+
   #:+default-backoff-multiplier+
   #:+default-max-backoff-ms+
   #:+default-allow-spillover-routing+
   #:+default-max-redirects+
   #:delegate-to-swarm
   #:+min-cancel-grace-period-ms+
   #:handoff-default-delegation-policy
   #:initiate-handoff
   #:accept-handoff
   #:reject-handoff
   #:reject-handoff-with-options
   #:complete-handoff
   #:get-pending-handoffs
   #:get-handoff-status
   #:register-child-delegation
   #:cancel-delegation
   #:cancelled-delegation-p
   #:cancellation-grace-expired-p
   #:forced-preemption-error-code
   #:collect-forced-preemptions
   #:serialize-handoff-context
   #:deserialize-handoff-context
   #:handoff-rejected
   #:handoff-rejected-handoff-id
   #:handoff-rejected-response
   #:handoff-rejected-rejection-code
   #:handoff-rejected-rejection-reason)

  ;; State machine persistence helpers
  (:export
   #:serialize-agent-state
   #:deserialize-agent-state)

  ;; Envelope message-log support
  (:export
   #:message-log
   #:make-message-log
   #:message-log-size
   #:clear-message-log
   #:log-envelope
   #:query-message-log
   #:message-log-entry
   #:message-log-entry-message-id
   #:message-log-entry-correlation-id
   #:message-log-entry-source-agent-id
   #:message-log-entry-target-agent-id
   #:message-log-entry-message-type
   #:message-log-entry-state
   #:message-log-entry-timestamp
   #:message-log-entry-envelope)

  ;; Budget
  (:export
   #:budget-envelope
   #:make-budget-envelope
   #:copy-budget-envelope
   #:tighten-budget
   #:split-budget
   #:check-budget
   #:decrement-budget
   #:budget-exhausted-p
   #:cancel-budget
   #:cancelled-p
   #:budget-exhausted
   #:budget-exhausted-budget
   #:budget-exhausted-required-tokens
   #:budget-exhausted-required-wall-time-ms
   #:budget-interceptor
   #:apply-budget-interceptor)

  ;; Local router
  (:export
   #:local-router
   #:make-local-router
   #:register-route
   #:unregister-route
   #:route-envelope
   #:dequeue-envelope
   #:schedule-next-envelope
   #:router-queue-size
   #:router-snapshot
   #:dead-letter
   #:dead-letter-envelope
   #:dead-letter-reason
   #:dead-letter-timestamp
   #:dead-letter-entries
   #:clear-dead-letters
   #:queue-full-error
   #:queue-full-error-agent-id
   #:queue-full-error-queue-size
   #:queue-full-error-queue-capacity)

  ;; Workflow engine
  (:export
   #:workflow-node
   #:workflow-edge
   #:workflow-definition
   #:workflow-run
   #:workflow-engine
   #:make-workflow-engine
   #:add-node
   #:add-edge
   #:topological-sort
   #:execute-workflow
   #:serialize-workflow-state
   #:restore-workflow-state
   #:register-workflow
   #:run-workflow
   #:get-workflow-run
   #:list-workflow-runs
   #:defworkflow
   #:make-feature-workflow-template
   #:make-bugfix-workflow-template
   #:make-refactor-workflow-template)

  ;; Negotiation room + voting
  (:export
   #:negotiation-room-client
   #:create-room
   #:submit-artifact
   #:add-critique
   #:score-artifact
   #:get-room-status
   #:get-decision
   #:wait-for-decision
   #:unanimous-vote-strategy)

  ;; Local HITL
  (:export
   #:local-hitl-gate
   #:make-local-hitl-gate
   #:request-hitl-approval
   #:approve-hitl-request
   #:deny-hitl-request
   #:get-hitl-request
   #:list-pending-hitl-requests)

  ;; Git worktree coordination
  (:export
   #:with-worktree-lock
   #:git-worktree-add
   #:git-worktree-remove
   #:git-worktree-prune
   #:git-worktree-list
   #:git-worktree-coordinator
   #:make-git-worktree-coordinator
   #:remote-worktree-coordinator
   #:make-remote-worktree-coordinator
   #:spawn-worktree
   #:collect-worktree
   #:inspect-worktree
   #:merge-worktree
   #:kill-worktree))

(defpackage #:sw4rm-sdk.handoff
  (:use #:cl))

(defpackage #:sw4rm-sdk.worktree
  (:use #:cl))

(defpackage #:sw4rm-sdk.workflow
  (:use #:cl))

(defpackage #:sw4rm-sdk.interceptors
  (:use #:cl))

(defpackage #:sw4rm-sdk.internal
  (:use #:cl))
