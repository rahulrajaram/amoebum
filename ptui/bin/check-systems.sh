#!/usr/bin/env bash
set -euo pipefail

# Smoke-test that each ASDF system loads independently.
# This is the monorepo equivalent of "separately compilable".

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

systems=(
  "ptui/caps"
  "ptui/core"
  "ptui/util"
  "ptui/runtime"
  "ptui/term"
  "ptui/render"
  "ptui/backend"
  "ptui/engine"
  "ptui"
  "ptui/examples"
)

for sys in "${systems[@]}"; do
  echo "==> loading ${sys}"
  sbcl --noinform --non-interactive \
    --eval "(load \"${ROOT_DIR}/.tools/quicklisp/setup.lisp\")" \
    --eval "(require :asdf)" \
    --eval "(asdf:load-asd (merge-pathnames #P\"ptui.asd\" (truename #P\"${ROOT_DIR}/\")))" \
    --eval "(asdf:load-system \"${sys}\")" \
    --eval "(quit)" >/dev/null
  echo "OK: ${sys}"
done
