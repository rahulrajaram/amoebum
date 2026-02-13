# Kimi SDK to Common Lisp Port Map

This document maps the Python Moonshot ecosystem SDK stack to a Common Lisp implementation plan.

## Source of Truth (Python)

- `kimi-sdk` (thin facade):
  - `sdks/kimi-sdk/src/kimi_sdk/__init__.py`
- `kosong` (real implementation):
  - `packages/kosong/src/kosong/chat_provider/kimi.py`
  - `packages/kosong/src/kosong/message.py`
  - `packages/kosong/src/kosong/_generate.py`
  - `packages/kosong/src/kosong/tooling/*.py`
  - `packages/kosong/src/kosong/utils/*.py`

Monorepo: `https://github.com/MoonshotAI/kimi-cli`

## Current Lisp Status

Implemented in this repo (`moonshot-common-lisp`):

- HTTP/JSON non-stream call (`chat-completion`)
- SSE stream call (`stream-chat-completion`)
- Simple streaming printer (`print-streamed-completion`)
- API key resolution and validation (`MOONSHOT_API_KEY`, `~/.moonshotai`)
- External smoke test script (`smoke-test.lisp`)

Not yet implemented from `kosong` semantics:

- Rich message/content-part model parity
- Tool-call orchestration (`generate`/`step` loop)
- Structured tool protocol and tool result lifecycle
- File upload helpers (`/files`, e.g. video)
- Strongly typed error taxonomy parity
- Snapshot/API compatibility test harness

## Module-by-Module Mapping

1. Facade Layer
- Python: `sdks/kimi-sdk/src/kimi_sdk/__init__.py`
- Lisp target: `src/facade.lisp`
- Goal: single public namespace that re-exports core API.

2. Provider/Core Chat Layer
- Python: `packages/kosong/src/kosong/chat_provider/kimi.py`
- Lisp target: `src/provider/kimi.lisp`
- Goal: model config, request body generation, sync/stream chat, stream usage capture, file API.

3. Message Model
- Python: `packages/kosong/src/kosong/message.py`
- Lisp target: `src/model/message.lisp`
- Goal: role/message/content part structs and serializer/deserializer.

4. Generation Orchestration
- Python: `packages/kosong/src/kosong/_generate.py`
- Lisp target: `src/agent/generate.lisp`
- Goal: high-level `generate` and `step` semantics with tool dispatch loop.

5. Tooling
- Python: `packages/kosong/src/kosong/tooling/*.py`
- Lisp target: `src/tooling/*.lisp`
- Goal: tool definitions, invocation contracts, normalized tool results.

6. Error Model
- Python: `packages/kosong/src/kosong/chat_provider/openai_common.py` + provider errors
- Lisp target: `src/errors.lisp`
- Goal: explicit condition hierarchy for auth, timeout, API status, malformed response.

7. Compatibility Tests
- Python: `packages/kosong/tests/*`
- Lisp target: `test/compat/*`
- Goal: behavior snapshots for non-stream and stream deltas/tool calls.

## Phased Port Plan

1. P0: Stable Core API
- Keep existing client stable.
- Add explicit response extractors and condition classes.

2. P1: Message Model Parity
- Add role/content-part structs and conversion helpers.
- Preserve backward compatibility with current hash-table responses.

3. P2: Tool Call + Step Loop
- Implement `generate` and `step` semantics.
- Add toolset registry and tool-call dispatch contract.

4. P3: Files API
- Add `/files` upload support and typed return objects.

5. P4: Compat Harness
- Add fixture-driven parity tests mirroring `kosong` behavior where practical.

## Immediate Next Steps

1. Create `src/errors.lisp` with first-class condition types.
2. Create `src/model/message.lisp` structs and serializer functions.
3. Add `src/agent/generate.lisp` with minimal `generate` prototype.
4. Add tests for stream chunk parsing parity.
