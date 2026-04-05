#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"
STATE_DIR="${REPO_ROOT}/.yarli"
TRANCHES_FILE="${STATE_DIR}/tranches.toml"
EVIDENCE_DIR="${STATE_DIR}/evidence"

mkdir -p "${STATE_DIR}" "${EVIDENCE_DIR}"

if [[ -f "${TRANCHES_FILE}" ]]; then
  printf 'YARLI_LOCAL_STATE_READY: %s\n' "${TRANCHES_FILE}"
  exit 0
fi

tmp_file="$(mktemp "${STATE_DIR}/tranches.XXXXXX.toml")"
printf '%s\n' 'version = 1' > "${tmp_file}"
mv "${tmp_file}" "${TRANCHES_FILE}"

printf 'YARLI_LOCAL_STATE_BOOTSTRAPPED: %s\n' "${TRANCHES_FILE}"
printf '%s\n' 'Local tranche state is ignored by git; tracked project truth stays in PROMPT.md, IMPLEMENTATION_PLAN.md, and yarli.toml.'
