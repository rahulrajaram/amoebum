# pseudopod

Common Lisp client for Moonshot's OpenAI-compatible API — amoebum's reach into the Moonshot ecosystem.

Defaults to Kimi K2.5.

## Defaults

- Base URL: `https://api.moonshot.ai/v1`
- Model: `kimi-k2.5`
- API key source (in order):
  1. `MOONSHOT_API_KEY` env var
  2. `~/.moonshotai` file

## Load

```lisp
(load "/path/to/amoebum/ptui/.tools/quicklisp/setup.lisp") ; or your own Quicklisp setup
(require :asdf)
(asdf:load-asd #P"/path/to/amoebum/pseudopod/pseudopod.asd")
(asdf:load-system "pseudopod")
```

## Usage

```lisp
(defparameter *client* (pseudopod:make-client))

;; Streaming output (prints reasoning + content chunks).
(pseudopod:print-streamed-completion
 *client*
 "Write a short haiku about Common Lisp and moonlight.")

;; Parsed non-streaming JSON response (hash table).
(pseudopod:chat-completion
 *client*
 "Say hello in one sentence.")
```

## Backward Compatibility

The package `:pseudopod` has `:moonshot-common-lisp` as a nickname. Existing code using the old package name will continue to work.

## Connectivity Smoke Test

Run:

```bash
sbcl --script ./pseudopod/smoke-test.lisp
```

Expected success output:

```text
PSEUDOPOD_SMOKE_OK
assistant=MOONSHOT_OK
```

## Notes

- This package uses `dexador` (HTTP) and `jonathan` (JSON).
- Quicklisp must be configured in your Lisp environment before loading this system.
- Empty/whitespace API keys are rejected with explicit errors (env var, file, or direct `:api-key`).
