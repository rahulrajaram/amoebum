# JSON CLI Contract (I337)

This document defines the machine-readable contract for headless CLI runs using
`--json` / `--non-interactive`.

## Versioning

- `schema_version`: `amoebum.cli.json.v1`
- `schema_doc`: `docs/json-cli-contract.md`

Consumers should treat `schema_version` as the compatibility key.

## Top-Level Payload

Every JSON-mode invocation emits exactly one JSON object with these stable keys:

- `schema_version` (string)
- `schema_doc` (string)
- `ok` (boolean)
- `mode` (string, `"json"` for headless JSON mode)
- `action` (string, one of `"prompt"`, `"command"`, `"error"`, `"none"`)
- `session_id` (string or null)
- `request` (object)
- `result` (object)
- `events` (array of event objects)

Backward-compatible keys are still included for existing clients:
`command`, `prompt`, `images`, `output`, `error`.

Command runs may also include:
- `command_payload` (object/array/scalar/null): slash-command payload exported in JSON-safe form.

## Request Object

`request` contains normalized invocation inputs:

- `mode`
- `action`
- `command`
- `prompt`
- `images` (array of paths)
- `session_id`

## Result Object

`result.kind` is the terminal outcome class:

- `prompt`: prompt/attachment path completed successfully.
- `tool`: command path completed successfully (slash-command tool surface).
- `error`: invocation failed or produced a failed terminal state.

`result` fields:

- `kind` (string)
- `status` (`"completed"` or `"failed"`)
- `output` (string or null)
- `error` (string or null)
- `tool` (object or null; currently `{name, command}` for command runs)
- `progress` (object with `status`)

When `result.kind = "tool"`, `result.tool.payload` mirrors `command_payload`.

## Event Stream Envelope

`events` is an ordered array emitted with every run:

1. `progress` event with `phase: "started"`
2. optional `tool` event for command runs
3. terminal `progress` event with `phase: "completed"` or `"failed"`

This provides machine-readable progress outcomes in addition to terminal result
classification.
