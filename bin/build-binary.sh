#!/usr/bin/env bash
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
DIST_DIR="${REPO_ROOT}/dist"
ENSURE_QUICKLISP="${REPO_ROOT}/ptui/bin/ensure-quicklisp.sh"
REPO_QUICKLISP_SETUP="${REPO_ROOT}/ptui/.tools/quicklisp/setup.lisp"
QUICKLISP_SETUP="${QUICKLISP_SETUP:-${REPO_QUICKLISP_SETUP}}"
QUICKLISP_SETUP_CANDIDATES=("${QUICKLISP_SETUP}" "${REPO_QUICKLISP_SETUP}" "${HOME}/quicklisp/setup.lisp")

if [[ -x "${ENSURE_QUICKLISP}" && ! -f "${QUICKLISP_SETUP}" ]]; then
  "${ENSURE_QUICKLISP}"
fi

if [[ ! -f "${QUICKLISP_SETUP}" ]]; then
  QUICKLISP_SETUP="${REPO_QUICKLISP_SETUP}"
fi

if [[ ! -f "${QUICKLISP_SETUP}" ]]; then
  QUICKLISP_SETUP="${HOME}/quicklisp/setup.lisp"
fi

if [[ ! -f "${QUICKLISP_SETUP}" ]]; then
  echo "Quicklisp setup not found. Tried: ${QUICKLISP_SETUP_CANDIDATES[*]}" >&2
  exit 1
fi

mkdir -p "${DIST_DIR}"

binary_size_bytes() {
  local path="$1"
  wc -c <"${path}" | tr -d '[:space:]'
}

strip_binary() {
  local path="$1"
  # SBCL save-lisp-and-die appends the Lisp heap after the ELF segments.
  # strip and objcopy both remove this trailing data, destroying the image.
  # Do not strip SBCL executables.
  echo "Skipping strip (SBCL images embed heap after ELF — strip destroys it)"
  return 0
}

compress_binary_upx() {
  local path="$1"
  if ! command -v upx >/dev/null 2>&1; then
    echo "WARN: upx not found; skipping binary compression." >&2
    return 0
  fi
  if ! upx --best --lzma "${path}" >/dev/null 2>&1; then
    echo "WARN: upx compression failed for ${path}; leaving binary unmodified." >&2
  fi
}

BINARY_PATH="${DIST_DIR}/amoebum"
ENABLE_STRIP="${AMOEBUM_STRIP_BINARY:-1}"
ENABLE_UPX="${AMOEBUM_UPX:-0}"

echo "Building amoebum binary..."
sbcl --noinform --non-interactive \
  --eval "(require :asdf)" \
  --eval "(let ((*compile-verbose* nil) (*load-verbose* nil)) \
             (handler-bind ((warning (lambda (c) (muffle-warning c)))) \
               (load \"${QUICKLISP_SETUP}\") \
               (setf asdf:*compile-file-warnings-behaviour* :ignore) \
               (asdf:load-asd \"${REPO_ROOT}/ptui/ptui.asd\") \
               (asdf:load-asd \"${REPO_ROOT}/pseudopod/pseudopod.asd\") \
               (asdf:load-asd \"${REPO_ROOT}/sw4rm-sdk/sw4rm-sdk.asd\") \
               (asdf:load-asd \"${REPO_ROOT}/amoebum/amoebum.asd\") \
               (asdf:load-system \"amoebum\") \
               (funcall (symbol-function (find-symbol \"SAVE-AMOEBUM-IMAGE\" \"AMOEBUM\")) \
                        :path \"${BINARY_PATH}\")))"

if [[ ! -f "${BINARY_PATH}" ]]; then
  echo "ERROR: expected binary not found at ${BINARY_PATH}" >&2
  exit 1
fi

before_size="$(binary_size_bytes "${BINARY_PATH}")"
echo "Binary saved to ${BINARY_PATH} (${before_size} bytes)"

if [[ "${ENABLE_STRIP}" == "1" ]]; then
  strip_binary "${BINARY_PATH}"
fi

if [[ "${ENABLE_UPX}" == "1" ]]; then
  compress_binary_upx "${BINARY_PATH}"
fi

after_size="$(binary_size_bytes "${BINARY_PATH}")"
if [[ "${after_size}" != "${before_size}" ]]; then
  echo "Final binary size: ${after_size} bytes (delta: $((after_size - before_size)) bytes)"
else
  echo "Final binary size: ${after_size} bytes"
fi
