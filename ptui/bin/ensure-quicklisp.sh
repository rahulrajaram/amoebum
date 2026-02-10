#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
TOOLS_DIR="${ROOT_DIR}/.tools"
QL_DIR="${TOOLS_DIR}/quicklisp"
QL_SETUP="${QL_DIR}/setup.lisp"
QL_BOOTSTRAP="${TOOLS_DIR}/quicklisp.lisp"
DIST_URL="$(sed -n '1p' "${ROOT_DIR}/deps/quicklisp-dist.txt")"

mkdir -p "${TOOLS_DIR}"

if [[ ! -f "${QL_SETUP}" ]]; then
  curl -fsSL "https://beta.quicklisp.org/quicklisp.lisp" -o "${QL_BOOTSTRAP}"
  sbcl --non-interactive \
    --load "${QL_BOOTSTRAP}" \
    --eval "(quicklisp-quickstart:install :path \"${QL_DIR}\")" \
    --eval "(quit)"
fi

sbcl --non-interactive \
  --load "${QL_SETUP}" \
  --eval "(ql-dist:install-dist \"${DIST_URL}\" :prompt nil :replace t)" \
  --eval "(ql:quickload '(:cffi :bordeaux-threads))" \
  --eval "(quit)"
