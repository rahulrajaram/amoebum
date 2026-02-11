#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
TOOLS_DIR="${ROOT_DIR}/.tools"
QL_DIR="${TOOLS_DIR}/quicklisp"
QL_SETUP="${QL_DIR}/setup.lisp"
QL_BOOTSTRAP="${TOOLS_DIR}/quicklisp.lisp"
DIST_URL="$(sed -n '1p' "${ROOT_DIR}/deps/quicklisp-dist.txt")"

mkdir -p "${TOOLS_DIR}"

LOCKFILE="${TOOLS_DIR}/quicklisp.lock"
if command -v flock >/dev/null 2>&1; then
  exec 9>"${LOCKFILE}"
  flock 9
else
  # Best-effort lock for environments without flock.
  LOCKDIR="${TOOLS_DIR}/quicklisp.lock.d"
  while ! mkdir "${LOCKDIR}" 2>/dev/null; do
    sleep 0.1
  done
  trap 'rmdir "${LOCKDIR}" >/dev/null 2>&1 || true' EXIT
fi

if [[ ! -f "${QL_SETUP}" ]]; then
  curl -fsSL "https://beta.quicklisp.org/quicklisp.lisp" -o "${QL_BOOTSTRAP}"
  sbcl --non-interactive \
    --load "${QL_BOOTSTRAP}" \
    --eval "(quicklisp-quickstart:install :path \"${QL_DIR}\")" \
    --eval "(quit)"
fi

# Avoid re-installing the Quicklisp dist on every run. Replacing the dist rebuilds
# cdb indexes and can race if multiple processes run in parallel.
DIST_STAMP="${TOOLS_DIR}/quicklisp-dist-url.txt"
DIST_INSTALLED=0
if [[ -f "${QL_DIR}/dists/quicklisp/distinfo.txt" && -f "${DIST_STAMP}" ]]; then
  if [[ "$(cat "${DIST_STAMP}")" == "${DIST_URL}" ]]; then
    DIST_INSTALLED=1
  fi
fi

FORCE_REPLACE="${PTUI_FORCE_QL_DIST_REPLACE:-}"

if [[ "${DIST_INSTALLED}" -ne 1 || "${FORCE_REPLACE}" == "1" ]]; then
  echo "${DIST_URL}" >"${DIST_STAMP}"
  sbcl --non-interactive \
    --load "${QL_SETUP}" \
    --eval "(ql-dist:install-dist \"${DIST_URL}\" :prompt nil :replace t)" \
    --eval "(quit)"
fi

sbcl --non-interactive \
  --load "${QL_SETUP}" \
  --eval "(ql:quickload '(:cffi :bordeaux-threads))" \
  --eval "(when (string= (or (sb-ext:posix-getenv \"PTUI_ENABLE_NCURSES\") \"\") \"1\") (ql:quickload '(:cl-charms)))" \
  --eval "(quit)"
