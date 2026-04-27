#!/usr/bin/env bash
set -euo pipefail

# Reads the Yarli prompt from stdin, writes it to a temp file, and delegates to
# Codex. Used by yarli with prompt_mode = "stdin" so large tranche prompts do
# not become argv entries.

sanitize_path() {
  local original_path="${PATH:-}"
  local sanitized_path=""
  local entry

  local IFS=:
  for entry in ${original_path}; do
    [[ -z "${entry}" ]] && continue
    case "${entry}" in
      */.codex/tmp/arg0|*/.codex/tmp/arg0/*)
        continue
        ;;
    esac

    if [[ -z "${sanitized_path}" ]]; then
      sanitized_path="${entry}"
    else
      sanitized_path="${sanitized_path}:${entry}"
    fi
  done

  if [[ -z "${sanitized_path}" ]]; then
    sanitized_path="/usr/local/bin:/usr/bin:/bin"
  fi

  export PATH="${sanitized_path}"
}

sanitize_path
unset CODEX_THREAD_ID

tmpdir="${YARLI_PROMPT_TMPDIR:-${PWD}/.yarli/tmp}"
mkdir -p "${tmpdir}"

tmpfile="$(mktemp "${tmpdir%/}/yarli-prompt-XXXXXX.md")"
trap 'rm -f "${tmpfile}"' EXIT

dd of="${tmpfile}" bs=1048576 status=none

exec codex \
  --dangerously-bypass-approvals-and-sandbox \
  exec \
  --ephemeral \
  --json \
  --model gpt-5.4 \
  --config model_reasoning_effort=medium \
  -C "$(pwd)" \
  "$@" \
  - < "${tmpfile}"
