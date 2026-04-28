#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
ASD_PATH="${ROOT_DIR}/ptui.asd"

# Allowed external (Quicklisp) dependencies. Internal `ptui/...` systems
# are inferred from defsystem names in the same file and always allowed.
allowed_external=(
  "uiop"
  "cffi"
  "bordeaux-threads"
  "cl-charms"
  "croatoan"
  "cl-ppcre"
  "cl-ppcre-unicode"
  "cl-yaml"
  "yaml"
  "alexandria"
  "fiveam"
  "jonathan"
)

# Collect all defsystem names declared in this asd plus sibling asd
# files in the same directory; treat them as internal cross-system
# deps and allow them.
mapfile -t internal_systems < <(
  rg -INo '^\(asdf:defsystem "([^"]+)"' -r '$1' "${ROOT_DIR}"/*.asd
)

# Extract only top-level system :depends-on quoted strings, ignoring
# per-component `(:file "name" :depends-on (...))` arrows inside
# :components lists.
#
# Strategy: a system :depends-on line appears at indent two and is
# followed by quoted dep names until the closing paren on the same line
# or until a subsequent system-level keyword. Per-component arrows are
# nested deeper inside :components and do not match the indent-two rule.
mapfile -t deps < <(
  awk '
    /^\(asdf:defsystem/ { in_system=1; in_deps=0; next }
    in_system && /^  :depends-on / {
      in_deps=1
      print
      next
    }
    in_deps {
      # End of system-level depends-on: any line starting at indent two
      # with another system field, or the end of the system form.
      if ($0 ~ /^  :[a-z]/ || $0 ~ /^\)/) { in_deps=0; next }
      print
    }
  ' "${ASD_PATH}" | rg -No '"([^"]+)"' -r '$1'
)

is_allowed() {
  local dep="$1"
  local approved
  for approved in "${allowed_external[@]}"; do
    if [[ "${dep}" == "${approved}" ]]; then return 0; fi
  done
  for approved in "${internal_systems[@]}"; do
    if [[ "${dep}" == "${approved}" ]]; then return 0; fi
  done
  return 1
}

for dep in "${deps[@]}"; do
  if ! is_allowed "${dep}"; then
    printf 'dependency-not-allowed: %s\n' "${dep}" >&2
    exit 1
  fi
done
