#!/usr/bin/env bash
set -euo pipefail

if [[ $# -ne 1 ]]; then
  echo "usage: $0 <prompt_file>" >&2
  echo "example: $0 PROMPT.md" >&2
  echo "note: reads prompt from file and executes codex with --json in current working dir" >&2
  exit 2
fi

prompt_file="$1"
if [[ ! -f "${prompt_file}" ]]; then
  echo "missing prompt file: ${prompt_file}" >&2
  exit 2
fi

exec codex --dangerously-bypass-approvals-and-sandbox exec --json -C "$(pwd)" - < "${prompt_file}"

