(asdf:defsystem "amoebum"
  :description "amoebum core application layer"
  :author "amoebum"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("pseudopod" "ptui" "ptui/panel" "ptui/components" "sw4rm-sdk" "uiop" "cl-ppcre" "bordeaux-threads" "cl-yaml")
  :serial t
  :components
  ((:file "src/package")
   (:file "src/package-domains")
   (:file "src/util")
   ;; NXT-264: Functional/immutable kernel (no internal deps).
   (:file "src/fp/package")
   (:file "src/fp/threading")
   (:file "src/fp/result")
   (:file "src/fp/update")
   (:file "src/fp/match")
   (:file "src/fp/plist")
   (:file "src/fp/collections")
   (:file "src/fp/transition")
   (:file "src/fp/frozen")
   (:file "src/runtime-log")
   (:file "src/usdt")
   (:file "src/events")
   (:file "src/events/types")
   (:file "src/events/filters")
   (:file "src/config")
   (:file "src/config/loader")
   (:file "src/worktrees")
   ;; NXT-356: runtime construction, naming, and lifecycle dispatch helpers.
   (:file "src/worktrees/runtime")
   ;; NXT-354: abandonment markers and cleanup policy extracted from worktrees.lisp.
   (:file "src/worktrees/cleanup")
   ;; NXT-355: conflict-handoff registry, room-status, and resolution helpers.
   (:file "src/worktrees/handoffs")
   ;; NXT-357: merge-target resolution plus merge/preflight policy helpers.
   (:file "src/worktrees/merge")
   (:file "src/provider-factory")
   (:file "src/ui/provider-dashboard")
   (:file "src/context")
   (:file "src/conversation/state")
   (:file "src/conversation/codec")
   (:file "src/conversation/forks")
   (:file "src/conversation/load")
   (:file "src/conversation/history")
   (:file "src/conversation")
   ;; NXT-388: memory.lisp decomposed into backend / file-store /
   ;; haake-adapter / haake-transfer / commands behind the residual facade.
   (:file "src/memory/backend")
   (:file "src/memory/file-store")
   (:file "src/memory/haake-adapter")
   (:file "src/memory/haake-transfer")
   (:file "src/memory/commands")
   (:file "src/memory")
   (:file "src/plan-mode")
   (:file "src/policy-types")
   (:file "src/plan-execution")
   (:file "src/plan-execution-output")
   ;; NXT-415: helpers (status normalization, step lookup, git runner)
   ;; extracted from src/plan-execution.lisp. Must load after the structs
   ;; defined in src/plan-execution.lisp and before the modules that use
   ;; %find-plan-execution-step / %safe-plan-execution-string.
   (:file "src/plan-execution-helpers")
   (:file "src/plan-execution-context")
   (:file "src/plan-execution-effects")
   ;; NXT-415: git rollback baseline + restore helpers extracted from
   ;; src/plan-execution.lisp. Loaded after helpers (uses %plan-execution-run-git).
   (:file "src/plan-execution-rollback")
   ;; NXT-415: declarative (status, event) -> transition table extracted
   ;; from src/plan-execution.lisp. Loaded after context/effects which
   ;; provide %build-plan-transition-decision-context and the effect
   ;; constructors used by transition handlers.
   (:file "src/plan-execution-state-machine")
   ;; NXT-415: public lifecycle entry points (start/pause/resume/abort/
   ;; reset/initialize/elapsed/progress) extracted from src/plan-execution.lisp.
   ;; Loaded after the state-machine and effects modules they delegate to.
   (:file "src/plan-execution-lifecycle")
   ;; NXT-415: restart-preserving execution loop + execute-approved-plan-steps
   ;; coordinator extracted from src/plan-execution.lisp. Loaded last so
   ;; lifecycle, state-machine, rollback, helpers, and effects are all
   ;; available when the coordinator delegates into them.
   (:file "src/plan-execution-loop")
   (:file "src/agents")
   (:file "src/agents/personas")
   (:file "src/agent-activity")
   ;; NXT-386: extension subsystem split. Discovery loads first because
   ;; manifest-metadata builders, permissions-prep, and the residual loader
   ;; all consume its path/key helpers. The legacy extensions.lisp file owns
   ;; the EXTENSION-LOAD-RECORD struct that the loader registers and that
   ;; checkpoint.lisp reads.
   (:file "src/extensions")
   (:file "src/extensions/discovery")
   (:file "src/extensions/manifest")
   (:file "src/extensions/permissions-prep")
   (:file "src/extensions/loader")
   ;; NXT-387: hot-reload watch-thread runtime extracted from loader.lisp.
   ;; Must load AFTER loader because the watch loop calls
   ;; reload-user-extensions and reads loader-owned discovery helpers.
   (:file "src/extensions/hot-reload")
   (:file "src/checkpoint")
   ;; NXT-426: checkpoint.lisp split into shared facade + dedicated
   ;; paths/codec/snapshots/session/image/auto modules. Load order keeps
   ;; shared structs/globals first, then path helpers, codecs, snapshot/session
   ;; restore-save flows, image runtime, and finally auto-checkpoint policy.
   (:file "src/checkpoint/paths")
   (:file "src/checkpoint/codec")
   (:file "src/checkpoint/snapshots")
   (:file "src/checkpoint/session")
   (:file "src/checkpoint/image")
   (:file "src/checkpoint/auto")
   (:file "src/sounds")
   (:file "src/sounds/backend")
   (:file "src/voice/tts")
   (:file "src/voice/asr")
   (:file "src/commands-base")
   (:file "src/permissions-command")
   (:file "src/permissions-path")
   (:file "src/permissions")
   (:file "src/permissions-rules")
   (:file "src/permissions-evaluation")
   (:file "src/commands-core-builtins")
   (:file "src/commands/handoffs")
   (:file "src/commands/worktree-handoff")
   (:file "src/commands/agents-runtime")
   (:file "src/commands/swarm-runtime")
   (:file "src/commands-agents")
   (:file "src/commands-phase5")
   (:file "src/sandbox")
   (:file "src/sandbox-os")
   (:file "src/asdf-extensions")
   (:file "src/compile-validation")
   ;; NXT-395: deftool module split. Submodules load in dependency order
   ;; (metadata/history/schema/coercion/expansion) before the residual
   ;; `macros/deftool` facade so the macroexpansion sees every helper
   ;; (%tool-name-string, rollback-tool, cl-type-to-json-schema,
   ;; %coerce-tool-argument, ...) at load time.
   (:file "src/macros/deftool/metadata")
   (:file "src/macros/deftool/history")
   (:file "src/macros/deftool/schema")
   (:file "src/macros/deftool/coercion")
   (:file "src/macros/deftool/expansion")
   (:file "src/macros/deftool")
   (:file "src/macros/defhook")
   ;; NXT-393: defkeys module split. Submodules load in dependency order
   ;; (parser/registry/dispatch/expansion) before the residual
   ;; `macros/defkeys` facade and the builtin keymap definitions, so the
   ;; `defkeys` macroexpansion sees every helper (%parse-key-spec,
   ;; register-key-binding, register-keymap) at load time.
   (:file "src/macros/defkeys/parser")
   (:file "src/macros/defkeys/registry")
   (:file "src/macros/defkeys/dispatch")
   (:file "src/macros/defkeys/expansion")
   (:file "src/macros/defkeys")
   (:file "src/macros/defkeys/builtins")
   ;; NXT-392: defskill module split. Submodules load in dependency order
   ;; (registry/runtime/tool-invocation/review/expansion) before the residual
   ;; `macros/defskill` facade and the builtin skill definitions, so the
   ;; `defskill` macroexpansion sees every helper (register-skill,
   ;; %skill-missing-required-arguments, %skill-default-completer, ...) at
   ;; load time.
   (:file "src/macros/defskill/registry")
   (:file "src/macros/defskill/runtime")
   (:file "src/macros/defskill/tool-invocation")
   (:file "src/macros/defskill/review")
   (:file "src/macros/defskill/expansion")
   (:file "src/macros/defskill")
   (:file "src/macros/defskill/builtins")
   (:file "src/indexer")
   (:file "src/conditions")
   (:file "src/mcp/jsonrpc")
   (:file "src/mcp/server")
   (:file "src/mcp/tool-bridge")
   (:file "src/mcp/negotiation")
   (:file "src/lsp/client")
   (:file "src/mcp/tools")
   (:file "src/events/journal")
   (:file "src/events/replay")
   (:file "src/events/session")
   (:file "src/self-modify")
   (:file "src/notifications")
   (:file "src/notifications/desktop")
   (:file "src/notifications/webhook")
   (:file "src/notifications/audit-log")
   (:file "src/notifications/dispatch")
   (:file "src/commands/plan")
   (:file "src/commands/history")
   (:file "src/commands/index")
   (:file "src/commands/memory")
   (:file "src/commands/self-modify")
   (:file "src/commands/extensions")
   (:file "src/commands/session")
   (:file "src/commands/notifications")
   (:file "src/commands/hooks")
   (:file "src/commands/permissions")
   (:file "src/commands/heap")
   (:file "src/commands")
   (:file "src/commands-registry")
   (:file "src/pipeline/context")
   (:file "src/pipeline/recovery")
   (:file "src/pipeline/events")
   (:file "src/pipeline/execution")
   (:file "src/pipeline")
   (:file "src/tools/files")
   (:file "src/tools/read-orchestration")
   (:file "src/tools/search")
   (:file "src/tools/search-orchestration")
   (:file "src/tools/web")
   ;; NXT-390: shell module split — env/runtime/background submodules load
   ;; before the residual `tools/shell` facade so the deftool form can call
   ;; the extracted helpers (%normalize-*, %prepare-shell-runtime, %persist-
   ;; shell-directory, %run-shell-command, %start-background-shell-task,
   ;; %list/cleanup/fetch-shell-tasks).
   (:file "src/tools/shell/env")
   (:file "src/tools/shell/runtime")
   (:file "src/tools/shell/background")
   (:file "src/tools/shell")
   (:file "src/tools/write-safety")
   (:file "src/tools/edit-validation")
   (:file "src/tools/shell-env")
   (:file "src/tools/shell-safety")
   ;; NXT-436: git module split — shared runtime helpers load before the
   ;; focused status/diff/commit/pr modules, and the residual `tools/git`
   ;; file now only publishes the public deftool facade.
   (:file "src/tools/git/runtime")
   (:file "src/tools/git/status")
   (:file "src/tools/git/diff")
   (:file "src/tools/git/commit")
   (:file "src/tools/git/pr")
   (:file "src/tools/git")
   (:file "src/tools/lsp")
   (:file "src/conversation-export")
   (:file "src/workers")
   (:file "src/tools/agents")
   (:file "src/workers/overwatch")
   (:file "src/workers/retry")
   (:file "src/workers/fanout")
   (:file "src/profiler")
   (:file "src/profiling/dashboard")
   (:file "src/widgets/fuzzy-picker")
   (:file "src/widgets/tree-browser")
   (:file "src/widgets/perf-dashboard")
   (:file "src/widgets/worker-dashboard")
   (:file "src/api-facades/operator-domains")
   (:file "src/api-facades/runtime-domains")
   (:file "src/api-facades/infrastructure-domains")
   (:file "src/api-facades")
   ;; NXT-263: Test isolation fixture. Must load AFTER api-facades because
   ;; %install-facade! moves *checkpoint-directory-override* from :amoebum
   ;; to :amoebum.sessions; the fixture references the post-install location.
   (:file "src/test-support/globals-fixture")
   (:file "src/swarm/agents")
   (:file "src/swarm/handoff-context")
   (:file "src/swarm/user-handoff")
   (:file "src/swarm/user-negotiation")
   (:file "src/swarm")
   (:file "src/widgets/swarm-panel")
   (:file "src/system-prompt")
   (:file "src/ide-context")
   (:file "src/adapters/cultivar")
   (:file "src/adapters/yore")
   ;; NXT-275: cultivar deftool wrappers. Must load AFTER src/adapters/cultivar
   ;; which defines make-cultivar-adapter, %cultivar-usable-p, and the
   ;; cultivar-resolve/expand/preview public API.
   (:file "src/tools/cultivar-tools")
   (:file "src/ui/approval-dialog")
   (:file "src/ui/style-table")
   (:file "src/ui/streaming/token-stream")
   (:file "src/ui/streaming/provider-runtime")
   ;; NXT-383 fallback split: markdown renderer and event journal now own
   ;; the independent ui/streaming clusters that chat-state/chat-stream use.
   (:file "src/ui/streaming/markdown")
   (:file "src/ui/streaming/event-journal")
   (:file "src/ui/streaming")
   (:file "src/ui/theme-amoebum")
   (:file "src/ui/layout-yaml")
   (:file "src/ui/yaml-theme-loader")
   (:file "src/ui/yaml-theme-layout")
   (:file "src/ui/demo")
   (:file "src/ui/status-bar")
   ;; I297-I304: prompt-input must load before chat.lisp
   ;; because chat.lisp calls chat-panel-handle-input-key defined here
   (:file "src/ui/panels/prompt-input")
   ;; NXT-431: chat-state split. Load defaults first, then the dedicated
   ;; conversation-entry coercion, slash-descriptor, and snapshot-metadata
   ;; helpers before the residual coordinator facade.
   (:file "src/ui/chat-state/state-defaults")
   (:file "src/ui/chat-state/slash-command-descriptors")
   (:file "src/ui/chat-state/conversation-entry")
   (:file "src/ui/chat-state/snapshot-metadata")
   (:file "src/ui/chat-state")
   ;; NXT-428: status/output, tool-completion ordering, and event-ingestion
   ;; helpers now load before the residual chat-stream coordinator.
   (:file "src/ui/chat-stream/status-output")
   (:file "src/ui/chat-stream/tool-completion")
   (:file "src/ui/chat-stream/stream-events")
   ;; NXT-279/NXT-428: residual chat streaming coordinator. Must load
   ;; immediately after the chat-stream helper modules and before src/ui/chat.
   (:file "src/ui/chat-stream")
   (:file "src/ui/chat-render/transcript")
   (:file "src/ui/chat-render/stream-overlays")
   ;; NXT-280: chat rendering subsystem extracted from chat.lisp.
   ;; Must load immediately after src/ui/chat-stream and before src/ui/chat.
   (:file "src/ui/chat-render")
   ;; NXT-281: chat input/editing subsystem extracted from chat.lisp.
   ;; Must load immediately after src/ui/chat-render and before src/ui/chat.
   (:file "src/ui/chat-input")
   (:file "src/ui/chat")
   ;; I297-I304: defpanel sub-panels extracted from chat.lisp
   (:file "src/ui/panels/message-history")
   (:file "src/ui/panels/approval-dialog")
   (:file "src/ui/panels/fuzzy-picker")
   (:file "src/ui/panels/tree-browser")
   (:file "src/ui/panels/stream-effects")
   (:file "src/ui/panels/chat-status-bar")
   (:file "src/ui/panels/chat-panel")
   (:file "src/main"))
  :in-order-to ((asdf:test-op (asdf:test-op "amoebum/test"))))

(asdf:defsystem "amoebum/test"
  :description "FiveAM test suite for amoebum core application"
  :depends-on ("amoebum" "fiveam" "ptui/test-support")
  :serial t
  :components
  ((:file "test/suite")
   ;; NXT-263/NXT-264: agent-amenability kernel tests.
   (:file "test/fp-kernel-test")
   (:file "test/fp-match-test")
   (:file "test/fp-plist-test")
   (:file "test/fp-collections-test")
   (:file "test/fp-transition-test")
   (:file "test/fp-frozen-test")
   (:file "test/globals-fixture-test")
   (:file "test/indexer-smoke-test")
   (:file "test/os-sandbox-smoke-test")
   (:file "test/self-modify-smoke-test")
   (:file "test/self-modify-test")
   (:file "test/sandbox-limits-test")
   (:file "test/image-smoke-suite")
   (:file "test/asdf-extensions-smoke-test")
   (:file "test/profiler-smoke-test")
   (:file "test/profiling-dashboard-test")
   (:file "test/compile-validation-conditions-test")
   (:file "test/deftool-type-validation-test")
   (:file "test/provider-factory-test")
   (:file "test/deftool-dangerous-permission-test")
   (:file "test/macroexpand-golden-test")
   (:file "test/pipeline-context-test")
   ;; NXT-287: phase-boundary unit tests for amoebum/src/pipeline.lisp
   (:file "test/pipeline-unit-test")
   (:file "test/tool-argument-prompting-test")
   (:file "test/read-orchestration-test")
   (:file "test/write-safety-test")
   (:file "test/edit-validation-test")
   (:file "test/json-cli-contract-test")
   (:file "test/git-workflow-test")
   (:file "test/entry-spine-test")
   (:file "test/review-workflow-test")
   (:file "test/multimodal-chat-test")
   (:file "test/notebook-edit-toolchain-test")
   (:file "test/permission-path-normalization-test")
   (:file "test/permission-path-memory-test")
   (:file "test/permission-command-matching-test")
   (:file "test/permission-command-canonicalization-test")
   (:file "test/permission-argument-granularity-test")
   (:file "test/permissions-unit-test")
   (:file "test/policy-kernel-test")
   (:file "test/web-search-policy-test")
   (:file "test/shell-env-test")
   (:file "test/shell-background-test")
   (:file "test/shell-runaway-output-test")
   (:file "test/shell-safety-test")
   (:file "test/defhook-cross-reference-test")
   (:file "test/method-combination-dispatch-test")
   (:file "test/restarts-round-trip-test")
   (:file "test/llm-restart-selection-test")
   (:file "test/event-types-test")
   (:file "test/mcp-tool-bridge-test")
   (:file "test/mcp-negotiation-test")
   (:file "test/mcp-transport-test")
   (:file "test/provider-dashboard-test")
   (:file "test/status-bar-mode-test")
   (:file "test/output-style-preset-test")
   (:file "test/status-bar-segments-test")
   (:file "test/notification-dispatch-test")
   (:file "test/runtime-log-test")
   (:file "test/repo-surface-test")
   (:file "test/api-facade-test")
   (:file "test/memory-command-test")
   (:file "test/approval-dialog-guard-test")
   (:file "test/desktop-notification-test")
	   (:file "test/stream-hooks-test")
	   (:file "test/incremental-markdown-test")
   (:file "test/extension-manifest-test")
   (:file "test/extension-loader-test")
   (:file "test/extension-discovery-test")
   (:file "test/asr-test")
   (:file "test/chat-snapshot-test")
   (:file "test/conversation-roundtrip-test")
   (:file "test/conversation-unit-test")
   (:file "test/session-resume-test")
   (:file "test/lifecycle-events-test")
   (:file "test/agent-activity-test")
   (:file "test/worktree-handoff-command-test")
   (:file "test/hailer-adapter-test")
   (:file "test/worker-supervisor-test")
   (:file "test/overwatch-backend-test")
   (:file "test/event-journal-test")
   (:file "test/event-journal-diagnostics-test")
   (:file "test/worker-retry-test")
   (:file "test/worker-fanout-test")
   (:file "test/worktree-runtime-test")
   (:file "test/swarm-execution-semantics-test")
   (:file "test/swarm-unit-test")
   (:file "test/event-replay-test")
   (:file "test/session-recording-test")
   (:file "test/conversation-export-test")
   (:file "test/worker-dashboard-test")
   (:file "test/agentic-loop-integration-test")
   (:file "test/phase11-integration-test")
   ;; Phase 10 gap-fill tests (I237-I251)
   (:file "test/config-loader-test")
   (:file "test/config-validation-test")
   (:file "test/web-fetch-orchestration-test")
   (:file "test/extension-lifecycle-test")
   (:file "test/extension-security-test")
   (:file "test/extension-cli-test")
   (:file "test/model-routing-test")
   (:file "test/streaming-step-test")
   (:file "test/stream-completion-ordering-test")
   (:file "test/token-stream-transition-table-test")
   (:file "test/plan-execution-transition-table-test")
   (:file "test/plan-execution-unit-test")
   (:file "test/budgeting-restart-test")
   (:file "test/codebase-index-test")
   (:file "test/persistence-test")
   (:file "test/checkpoint-rotation-test")
   (:file "test/state-serialization-test")
   (:file "test/user-coordination-test")
   (:file "test/user-handoff-regression-test")
   (:file "test/usdt-probe-test")
   ;; TUI appearance snapshot tests
   (:file "test/input-appearance-test")
   (:file "test/overlay-appearance-test")
   (:file "test/scroll-appearance-test")
   (:file "test/style-appearance-test")
   (:file "test/error-appearance-test")
   (:file "test/focus-appearance-test")
   (:file "test/output-style-appearance-test")
   (:file "test/persona-test")
   (:file "test/agent-tools-test")
   ;; YAML theme tests
   (:file "test/yaml-theme-smoke-test")
   (:file "test/yaml-theme-validation-test")
   ;; NXT-092/NXT-093: IDE context ingestion and prompt wiring
   (:file "test/ide-context-test")
   ;; NXT-094/NXT-095: IDE context integration tests and observability events
   (:file "test/ide-context-integration-test")
   ;; NXT-106/NXT-107: Cultivar and Yore adapter stubs
   (:file "test/cultivar-adapter-test")
   ;; NXT-275: cultivar deftool wrappers — depends on the adapter tests
   ;; above for shared diagnostic helpers and must load after them.
   (:file "test/cultivar-tools-test")
   (:file "test/yore-adapter-test")
   ;; NXT-108/NXT-109: IDE context packet builder and context-pressure metadata
   (:file "test/nxt-108-109-context-pressure-test")
   ;; NXT-110: Cultivar and Yore adapter integration (happy-path)
   (:file "test/adapter-integration-test")
   ;; NXT-111: Status-bar regression (modes, output styles, context pressure, render-key)
   (:file "test/status-bar-regression-test")
   ;; FP-Refine Phase 2: dispatch and decision logic as data
   (:file "test/argument-pattern-dispatch-test")
   (:file "test/permission-decision-dispatch-test")
   ;; NXT-232: Keyboard accessibility / focus navigation tests
   (:file "test/keyboard-nav-test")
   ;; NXT-233: TUI scale / stress tests
   (:file "test/scroll-scale-test")
   ;; NXT-397: Package-import-cycle guardrail mirror in FiveAM.
   (:file "test/import-cycles-test"))
  :perform (asdf:test-op (op c)
             (declare (ignore op c))
             (unless (uiop:symbol-call :amoebum/test :run-all)
               (error "Amoebum FiveAM suite failed."))))

(asdf:defsystem "amoebum/tests"
  :description "Compatibility alias for the Amoebum FiveAM suite."
  :depends-on ("amoebum/test"))
