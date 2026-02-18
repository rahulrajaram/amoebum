#!/usr/bin/env bash
set -euo pipefail

# Wrapper to run claude without nested-session detection.
# Unsets CLAUDECODE so that yarli-spawned sessions don't fail.

unset CLAUDECODE
exec claude "$@"
