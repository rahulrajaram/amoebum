# USDT Probe Coverage (I360)

This document records the current USDT probe emission paths for tool execution,
LLM streaming, and agent lifecycle observability.

## Probe Families

1. Tool execution probes (`:tool-enter`, `:tool-exit`)
   - Emitted from `amoebum/src/pipeline.lisp` during `pseudopod:execute-tool`.
   - Carries tool name, request id, status, and elapsed duration.

2. Streamed tool-call probes (`:tool-call`)
   - Emitted from `amoebum/src/ui/streaming.lisp` in:
     - `token-stream-emit-tool-call-started`
     - `token-stream-emit-tool-call-argument-complete`
     - `token-stream-emit-tool-call-result`
   - Carries phase (`:started`, `:argument-complete`, `:result`), tool metadata,
     and result status.

3. LLM request and stream probes (`:llm-request-start`, `:llm-stream-chunk`, `:llm-request-end`)
   - Emitted from `amoebum/src/ui/streaming.lisp` in `stream-pseudopod-chat`.
   - Chunk probes are emitted for content/reasoning/fallback chunk paths and
     include chunk kind, chunk index, and cumulative char counts.

4. Agent lifecycle probes (`:agent-lifecycle`)
   - Emitted from `amoebum/src/agents.lisp` on spawn, start, cancel request,
     and terminal completion/failure/cancel states.
   - Carries phase, agent id/type, terminal status, parent message id, and
     elapsed duration where available.

## Verification Coverage

`amoebum/test/usdt-probe-test.lisp` includes focused tests that assert:

1. All required probe types are emitted by API calls.
2. Token-stream tool-call lifecycle emits probe phases.
3. Streamed chat emits chunk-level LLM probes.
4. Spawned agent execution emits lifecycle probes (`:spawn`, `:start`, `:complete`).
