#!/usr/bin/env bash
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
TARGET="dist/"
GITIGNORE="${REPO_ROOT}/.gitignore"

if [[ ! -f "${GITIGNORE}" ]]; then
  echo "Missing .gitignore at ${GITIGNORE}" >&2
  exit 1
fi

if ! grep -Fq "${TARGET}" "${GITIGNORE}"; then
  echo "Missing ${TARGET} entry in .gitignore" >&2
  exit 1
fi

if ! (cd "${REPO_ROOT}" && git check-ignore -q dist/); then
  echo "Git ignore rules do not currently mark dist/ as ignored" >&2
  exit 1
fi

echo "DIST_IGNORE_OK"
echo "Git ignore check passed"
