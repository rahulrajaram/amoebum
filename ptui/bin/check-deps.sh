#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
ASD_PATH="${ROOT_DIR}/ptui.asd"

allowed=(
  "uiop"
  "cffi"
  "bordeaux-threads"
  "cl-charms"
  "croatoan"
)

mapfile -t deps < <(
  awk '
    /:depends-on/ { in_deps=1; next }
    in_deps && /:serial/ { in_deps=0 }
    in_deps { print }
  ' "${ASD_PATH}" | rg -No '"([^"]+)"' -r '$1'
)

for dep in "${deps[@]}"; do
  ok=0
  for approved in "${allowed[@]}"; do
    if [[ "${dep}" == "${approved}" ]]; then
      ok=1
      break
    fi
  done
  if [[ ${ok} -ne 1 ]]; then
    printf 'dependency-not-allowed: %s\n' "${dep}" >&2
    exit 1
  fi
done
