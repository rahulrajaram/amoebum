#!/usr/bin/env bash
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
DIST_DIR="${REPO_ROOT}/dist"
QUICKLISP_SETUP="${QUICKLISP_SETUP:-${REPO_ROOT}/ptui/.tools/quicklisp/setup.lisp}"

if [[ ! -f "${QUICKLISP_SETUP}" ]]; then
  QUICKLISP_SETUP="${HOME}/quicklisp/setup.lisp"
fi

if [[ ! -f "${QUICKLISP_SETUP}" ]]; then
  echo "Quicklisp setup not found. Set QUICKLISP_SETUP to a valid path." >&2
  exit 1
fi

mkdir -p "${DIST_DIR}"

echo "Building amoebum binary..."
sbcl --noinform --non-interactive \
  --eval "(load \"${QUICKLISP_SETUP}\")" \
  --eval "(asdf:load-asd \"${REPO_ROOT}/ptui/ptui.asd\")" \
  --eval "(asdf:load-asd \"${REPO_ROOT}/pseudopod/pseudopod.asd\")" \
  --eval "(asdf:load-asd \"${REPO_ROOT}/sw4rm-sdk/sw4rm-sdk.asd\")" \
  --eval "(asdf:load-asd \"${REPO_ROOT}/amoebum/amoebum.asd\")" \
  --eval "(asdf:load-system \"amoebum\")" \
  --eval "(amoebum:save-amoebum-image :path \"${DIST_DIR}/amoebum\")"

echo "Binary saved to ${DIST_DIR}/amoebum"
