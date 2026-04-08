#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

"${ROOT_DIR}/bin/ensure-quicklisp.sh" >/dev/null

sbcl --noinform --non-interactive \
  --eval "(load \"${ROOT_DIR}/.tools/quicklisp/setup.lisp\")" \
  --eval "(require :asdf)" \
  --eval "(asdf:load-asd \"${ROOT_DIR}/ptui.asd\")" \
  --eval "(asdf:load-asd \"${ROOT_DIR}/ptui-preview.asd\")" \
  --eval "(asdf:load-asd \"${ROOT_DIR}/ptui-examples.asd\")" \
  --eval "(asdf:load-system \"ptui\")" \
  --eval "(asdf:load-system \"ptui-preview\")" \
  --eval "(asdf:load-system \"ptui-examples\")" \
  --load "${ROOT_DIR}/test/run.lisp"
