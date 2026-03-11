# Streamed Turn Lifecycle Contract (I332)

This contract defines the only valid terminal outcomes for one streamed agentic
turn and the invariants used by regression replay tests.

## Source Mapping

- `amoebum/src/ui/chat.lisp`
  - Stream event drain/finalization in `%drain-stream-events`.
  - Tool execution continuation in `%execute-stream-tool-call!` and
    `%maybe-finalize-streaming-assistant-on-complete`.
- `amoebum/src/ui/streaming.lisp`
  - Token-stream lifecycle states and terminal markers
    (`token-stream-start`, `token-stream-drain-events`,
    `token-stream-mark-complete`, `token-stream-mark-failed`,
    `token-stream-mark-cancelled`).
- `pseudopod/src/agent/generate.lisp`
  - Step-loop semantics requiring each model step to resolve to either final
    assistant output or tool-loop continuation.
- `pseudopod/src/agent/conversation.lisp`
  - Streaming chunk contract (`:text-delta`, `:tool-call-delta`, `:usage-delta`,
    terminal `:done`) and completion status handoff.

## Lifecycle States

1. Conversation state machine (`amoebum/src/conversation.lisp`): `:idle`,
   `:user-input`, `:streaming`, `:tool-executing`, `:waiting-approval`,
   `:error-recovery`.
2. Token-stream status (`amoebum/src/ui/streaming.lisp`): `:idle`, `:running`,
   `:completed`, `:cancelled`, `:failed`.
3. Streaming chunk kinds (`pseudopod` + `amoebum`): text deltas, tool-call
   deltas/starts/completions/results, usage deltas, and terminal completion.

## Allowed Streamed-Turn Flow

1. Enter `:streaming` from `:user-input` (or continuation path).
2. Consume streamed text and/or tool-call events.
3. On stream completion:
   - finalize assistant answer, or
   - execute tool continuation, or
   - request retry, or
   - surface explicit failure.
4. Transition conversation to `:idle` on successful completion branches, or
   `:error-recovery` on explicit failures.

## Valid Terminal Outcomes

Only these are contract-valid terminal outcomes for a streamed turn:

- `:answer`
- `:tool-continuation`
- `:retry`
- `:explicit-error`

`:silent-completion` is explicitly invalid and must be treated as a regression
failure signal.

## Invariants

1. A completed stream must map to exactly one valid terminal outcome.
2. Tool-call lifecycle signals imply `:tool-continuation` unless an explicit
   error or retry condition supersedes.
3. Retry outcomes are explicit (for example malformed tool-call replay paths),
   never inferred from missing output.
4. Any surfaced stream/tool/provider failure must classify as `:explicit-error`.
5. Narration/text deltas followed by stream completion with no finalized answer,
   no tool signals, and no retry/error signals classify as `:silent-completion`.

## Regression Fixtures

`amoebum/test/suite.lisp` contains deterministic replay fixtures and classifier
tests for:

- healthy headless tool-using continuation shape, and
- reproduced interactive silent-completion shape.
