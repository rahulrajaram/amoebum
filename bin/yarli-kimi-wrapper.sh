#!/usr/bin/env bash
set -euo pipefail

# Wrapper to run kimi without nested-session detection.
# Unsets KIMICODE so that yarli-spawned sessions don't fail.

unset KIMICODE
exec kimi "$@"
