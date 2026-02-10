#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
DIST_DIR="${ROOT_DIR}/dist"
TOOLS_DIR="${ROOT_DIR}/.tools"
BUILDAPP_BIN="${TOOLS_DIR}/buildapp/buildapp"
ASD_PATH="${ROOT_DIR}/ptui.asd"
ASD_EXAMPLES_PATH="${ROOT_DIR}/ptui-examples.asd"
NATIVE_DIR="${ROOT_DIR}/native"
NATIVE_LIB="${NATIVE_DIR}/libptui_native.so"

mkdir -p "${DIST_DIR}" "${TOOLS_DIR}"

"${ROOT_DIR}/bin/check-deps.sh"
"${ROOT_DIR}/bin/ensure-quicklisp.sh"

if [[ -f "${NATIVE_DIR}/ptui_native.c" ]]; then
  cc -std=c11 -O2 -fPIC -shared "${NATIVE_DIR}/ptui_native.c" -o "${NATIVE_LIB}"
  cp "${NATIVE_LIB}" "${DIST_DIR}/libptui_native.so"
fi

if ! command -v buildapp >/dev/null 2>&1; then
  if [[ ! -x "${BUILDAPP_BIN}" ]]; then
    rm -rf "${TOOLS_DIR}/buildapp-src" "${TOOLS_DIR}/buildapp"
    git clone --depth 1 https://github.com/xach/buildapp.git "${TOOLS_DIR}/buildapp-src"
    make -C "${TOOLS_DIR}/buildapp-src"
    mkdir -p "${TOOLS_DIR}/buildapp"
    cp "${TOOLS_DIR}/buildapp-src/buildapp" "${BUILDAPP_BIN}"
  fi
  BUILDAPP="${BUILDAPP_BIN}"
else
  BUILDAPP="$(command -v buildapp)"
fi

"${BUILDAPP}" \
  --eval '(require :asdf)' \
  --eval "(load \"${ROOT_DIR}/.tools/quicklisp/setup.lisp\")" \
  --eval "(asdf:load-asd \"${ASD_PATH}\")" \
  --eval "(asdf:load-asd \"${ASD_EXAMPLES_PATH}\")" \
  --load-system ptui-examples \
  --entry ptui.examples.metrics-dashboard:main \
  --output "${DIST_DIR}/metrics-dashboard"

chmod +x "${DIST_DIR}/metrics-dashboard"
