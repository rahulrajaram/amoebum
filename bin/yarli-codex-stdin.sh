#!/usr/bin/env bash
set -euo pipefail

# Reads prompt from stdin, writes to temp file, delegates to codex.
# Used by yarli with prompt_mode = "stdin".

tmpfile="$(mktemp /tmp/yarli-prompt-XXXXXX.md)"
trap 'rm -f "${tmpfile}"' EXIT

cat > "${tmpfile}"

exec codex --dangerously-bypass-approvals-and-sandbox exec --json -C "$(pwd)" - < "${tmpfile}"
