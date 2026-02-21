#!/usr/bin/env bash
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
DIST_DIR="${REPO_ROOT}/dist"

mkdir -p "${DIST_DIR}"

echo "Building amoebum binary..."
sbcl --noinform --non-interactive \
  --eval "(load \"${REPO_ROOT}/ptui/.tools/quicklisp/setup.lisp\")" \
  --eval "(asdf:load-asd \"${REPO_ROOT}/ptui/ptui.asd\")" \
  --eval "(asdf:load-asd \"${REPO_ROOT}/pseudopod/pseudopod.asd\")" \
  --eval "(asdf:load-asd \"${REPO_ROOT}/sw4rm-sdk/sw4rm-sdk.asd\")" \
  --eval "(asdf:load-asd \"${REPO_ROOT}/amoebum/amoebum.asd\")" \
  --eval "(asdf:load-system \"amoebum\")" \
  --eval "(amoebum:save-amoebum-image :path \"${DIST_DIR}/amoebum\")"

echo "Binary saved to ${DIST_DIR}/amoebum"
