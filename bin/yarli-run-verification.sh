#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"
RUN_SHELL="${YARLI_VERIFICATION_SHELL:-${SHELL:-/bin/bash}}"

if [[ "${RUN_SHELL}" != */* ]]; then
  RUN_SHELL="$(command -v "${RUN_SHELL}" 2>/dev/null || true)"
fi

if [[ -z "${RUN_SHELL}" || ! -x "${RUN_SHELL}" ]]; then
  RUN_SHELL="/bin/bash"
fi

print_commands() {
  cat <<'EOF'
./ptui/bin/check-systems.sh
./ptui/bin/smoke-examples.sh
./ptui/bin/test.sh
./ptui/bin/build.sh
PTUI_EXIT_AFTER_MS=500 ./ptui/dist/metrics-dashboard
PTUI_DASHBOARD_MODE=legacy PTUI_EXIT_AFTER_MS=500 ./ptui/dist/metrics-dashboard
PTUI_EXIT_AFTER_MS=500 ./ptui/dist/atop-dashboard
./ptui/bin/compliance-gate.sh
./ptui/bin/tmux-layout-regression.sh
./ptui/bin/tmux-behavior-regression.sh
./ptui/bin/tmux-ansi-regression.sh
./ptui/bin/capture-screenshots.sh
EOF
}

usage() {
  cat <<'EOF'
Usage:
  bin/yarli-run-verification.sh
  bin/yarli-run-verification.sh --print-commands
EOF
}

if [[ "${1:-}" == "--print-commands" ]]; then
  if [[ $# -ne 1 ]]; then
    usage >&2
    exit 2
  fi
  print_commands
  exit 0
fi

if [[ "${1:-}" == "--help" ]]; then
  usage
  exit 0
fi

if [[ $# -ne 0 ]]; then
  usage >&2
  exit 2
fi

while IFS= read -r cmd; do
  [[ -z "${cmd}" ]] && continue
  echo "==> ${cmd}"
  (
    cd "${REPO_ROOT}"
    "${RUN_SHELL}" -lc "${cmd}" </dev/null
  )
done < <(print_commands)

echo "YARLI_VERIFICATION_OK"
