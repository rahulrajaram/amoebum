# moonshot-common-lisp

Thin Common Lisp client for Moonshot's OpenAI-compatible API, with defaults for Kimi K2.5.

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
(asdf:load-asd #P"/path/to/amoebum/moonshot-common-lisp/moonshot-common-lisp.asd")
(asdf:load-system "moonshot-common-lisp")
```

## Usage

```lisp
(defparameter *client* (moonshot-common-lisp:make-client))

;; Streaming output (prints reasoning + content chunks).
(moonshot-common-lisp:print-streamed-completion
 *client*
 "Write a short haiku about Common Lisp and moonlight.")

;; Parsed non-streaming JSON response (hash table).
(moonshot-common-lisp:chat-completion
 *client*
 "Say hello in one sentence.")
```

## Connectivity Smoke Test

Run:

```bash
sbcl --script ./moonshot-common-lisp/smoke-test.lisp
```

Expected success output:

```text
MOONSHOT_SMOKE_OK
assistant=MOONSHOT_OK
```

## Notes

- This package uses `dexador` (HTTP) and `jonathan` (JSON).
- Quicklisp must be configured in your Lisp environment before loading this system.
- Empty/whitespace API keys are rejected with explicit errors (env var, file, or direct `:api-key`).
