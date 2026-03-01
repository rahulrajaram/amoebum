#!/usr/bin/env bash
set -euo pipefail

# Smoke-test that each ASDF system loads independently.
# This is the monorepo equivalent of "separately compilable".

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
REPO_ROOT="$(cd "${ROOT_DIR}/.." && pwd)"

PLAN_FILE="${REPO_ROOT}/IMPLEMENTATION_PLAN.md"

# Enforce plan contracts when the plan file is present. In CI, this file may be
# intentionally local-only and absent from the repository.
if [[ -f "${PLAN_FILE}" ]]; then
  "${REPO_ROOT}/bin/yarli-lint-implementation-plan.sh" "${PLAN_FILE}"
  "${REPO_ROOT}/bin/yarli-verify-gate-parity.sh" "${PLAN_FILE}" "${REPO_ROOT}/bin/yarli-run-verification.sh" "${REPO_ROOT}/yarli.toml"
else
  echo "PLAN_LINT_SKIP: ${PLAN_FILE} not found; skipping plan contract checks."
fi

# Ensure Quicklisp exists for external deps (cffi, bordeaux-threads, ...).
"${ROOT_DIR}/bin/ensure-quicklisp.sh" >/dev/null

systems=(
  "ptui/caps"
  "ptui/core"
  "ptui/util"
  "ptui/runtime"
  "ptui/term"
  "ptui/text"
  "ptui/search"
  "ptui/layout"
  "ptui/layout/yoga"
  "ptui/ui"
  "ptui/widgets"
  "ptui/components"
  "ptui/render"
  "ptui/backend"
  "ptui/engine"
  "ptui/standalone"
  "ptui/components-standalone"
  "ptui"
  "ptui/examples"
  "ptui-examples"
  "ptui/examples-standalone"
)

for sys in "${systems[@]}"; do
  echo "==> loading ${sys}"
  sbcl --noinform --non-interactive \
    --eval "(load \"${ROOT_DIR}/.tools/quicklisp/setup.lisp\")" \
    --eval "(require :asdf)" \
    --eval "(asdf:load-asd (merge-pathnames #P\"ptui.asd\" (truename #P\"${ROOT_DIR}/\")))" \
    --eval "(asdf:load-asd (merge-pathnames #P\"ptui-examples.asd\" (truename #P\"${ROOT_DIR}/\")))" \
    --eval "(asdf:load-system \"${sys}\")" \
    --eval "(quit)" >/dev/null
  echo "OK: ${sys}"
done

if [[ "${PTUI_ENABLE_NCURSES:-}" == "1" ]]; then
  echo "==> loading ptui with :ptui-ncurses feature"
  sbcl --noinform --non-interactive \
    --eval "(load \"${ROOT_DIR}/.tools/quicklisp/setup.lisp\")" \
    --eval "(require :asdf)" \
    --eval '(pushnew :ptui-ncurses *features*)' \
    --eval "(asdf:load-asd (merge-pathnames #P\"ptui.asd\" (truename #P\"${ROOT_DIR}/\")))" \
    --eval "(asdf:load-asd (merge-pathnames #P\"ptui-examples.asd\" (truename #P\"${ROOT_DIR}/\")))" \
    --eval "(asdf:load-system \"ptui\")" \
    --eval "(quit)" >/dev/null
  echo "OK: ptui (ptui-ncurses)"
fi

if [[ "${PTUI_ENABLE_LAYOUT_YOGA:-}" == "1" ]]; then
  echo "==> loading ptui/layout/yoga with :ptui-layout-yoga feature"
  sbcl --noinform --non-interactive \
    --eval "(load \"${ROOT_DIR}/.tools/quicklisp/setup.lisp\")" \
    --eval "(require :asdf)" \
    --eval '(pushnew :ptui-layout-yoga *features*)' \
    --eval "(asdf:load-asd (merge-pathnames #P\"ptui.asd\" (truename #P\"${ROOT_DIR}/\")))" \
    --eval "(asdf:load-system \"ptui/layout/yoga\")" \
    --eval "(quit)" >/dev/null
  echo "OK: ptui/layout/yoga (ptui-layout-yoga)"
fi
