#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"

PROFILES=(worktrees packages commands facades state policy ui extensions shell macros)
STRICT=0

usage() {
  cat <<'EOF'
Usage:
  bin/line-count-audit.sh [--strict] [profile]
  bin/line-count-audit.sh --list-profiles

Profiles:
  worktrees  Audit split worktree control-plane modules.
  packages   Audit package/load-order hotspot files.
  commands   Audit residual command hotspot files.
  facades    Audit facade hotspot files.
  state      Audit conversation/checkpoint hotspot files.
  policy     Audit plan-execution/permissions hotspot files.
  ui         Audit oversized UI hotspot files.
  extensions Audit extension/runtime hotspot files.
  shell      Audit shell/runtime hotspot files.
  macros     Audit macro hotspot files.
  all        Run every profile.

Options:
  --strict   Fail on missing audit targets.
EOF
}

list_profiles() {
  printf '%s\n' "${PROFILES[@]}" all
}

fail() {
  echo "LINE_COUNT_AUDIT_ERROR: $*" >&2
  exit 1
}

line_count() {
  wc -l <"$1" | tr -d ' '
}

profile_rules() {
  case "$1" in
    worktrees)
      cat <<'EOF'
worktrees|amoebum/src/worktrees.lisp|387
worktrees|amoebum/src/worktrees/runtime.lisp|626
worktrees|amoebum/src/worktrees/merge.lisp|485
worktrees|amoebum/src/worktrees/cleanup.lisp|304
worktrees|amoebum/src/worktrees/handoffs.lisp|279
EOF
      ;;
packages)
      cat <<'EOF'
packages|amoebum/src/package.lisp|1321
packages|amoebum/src/package-domains.lisp|204
EOF
      ;;
    commands)
      cat <<'EOF'
commands|amoebum/src/commands-agents.lisp|314
commands|amoebum/src/commands/agents-runtime.lisp|389
commands|amoebum/src/commands/swarm-runtime.lisp|217
EOF
      ;;
    facades)
      cat <<'EOF'
facades|amoebum/src/api-facades.lisp|197
facades|amoebum/src/api-facades/operator-domains.lisp|546
facades|amoebum/src/api-facades/runtime-domains.lisp|492
facades|amoebum/src/api-facades/infrastructure-domains.lisp|459
EOF
      ;;
    state)
      cat <<'EOF'
state|amoebum/src/conversation.lisp|6
state|amoebum/src/conversation/state.lisp|306
state|amoebum/src/conversation/codec.lisp|170
state|amoebum/src/conversation/forks.lisp|294
state|amoebum/src/conversation/load.lisp|227
state|amoebum/src/conversation/history.lisp|243
state|amoebum/src/checkpoint.lisp|1667
state|amoebum/src/swarm.lisp|11
state|amoebum/src/swarm/agents.lisp|448
state|amoebum/src/swarm/handoff-context.lisp|423
state|amoebum/src/swarm/user-handoff.lisp|566
state|amoebum/src/swarm/user-negotiation.lisp|132
state|amoebum/src/memory.lisp|33
state|amoebum/src/memory/backend.lisp|261
state|amoebum/src/memory/file-store.lisp|429
state|amoebum/src/memory/haake-adapter.lisp|425
state|amoebum/src/memory/haake-transfer.lisp|287
state|amoebum/src/memory/commands.lisp|239
EOF
      ;;
    policy)
      cat <<'EOF'
policy|amoebum/src/plan-execution.lisp|200
policy|amoebum/src/plan-execution-helpers.lisp|220
policy|amoebum/src/plan-execution-rollback.lisp|130
policy|amoebum/src/plan-execution-state-machine.lisp|400
policy|amoebum/src/plan-execution-lifecycle.lisp|180
policy|amoebum/src/plan-execution-loop.lisp|300
policy|amoebum/src/permissions.lisp|860
EOF
      ;;
    ui)
      cat <<'EOF'
ui|amoebum/src/ui/streaming.lisp|1888
ui|amoebum/src/ui/chat-render.lisp|1786
EOF
      ;;
    extensions)
      cat <<'EOF'
extensions|amoebum/src/extensions/loader.lisp|580
extensions|amoebum/src/extensions/discovery.lisp|151
extensions|amoebum/src/extensions/manifest.lisp|511
extensions|amoebum/src/extensions/permissions-prep.lisp|310
extensions|amoebum/src/extensions/hot-reload.lisp|133
EOF
      ;;
    shell)
      cat <<'EOF'
shell|amoebum/src/tools/shell.lisp|300
shell|amoebum/src/tools/shell/env.lisp|260
shell|amoebum/src/tools/shell/runtime.lisp|650
shell|amoebum/src/tools/shell/background.lisp|260
EOF
      ;;
    macros)
      cat <<'EOF'
macros|amoebum/src/macros/defskill.lisp|50
macros|amoebum/src/macros/defskill/registry.lisp|150
macros|amoebum/src/macros/defskill/runtime.lisp|120
macros|amoebum/src/macros/defskill/tool-invocation.lisp|140
macros|amoebum/src/macros/defskill/review.lisp|420
macros|amoebum/src/macros/defskill/expansion.lisp|320
macros|amoebum/src/macros/defskill/builtins.lisp|180
macros|amoebum/src/macros/defkeys.lisp|60
macros|amoebum/src/macros/defkeys/parser.lisp|440
macros|amoebum/src/macros/defkeys/registry.lisp|300
macros|amoebum/src/macros/defkeys/dispatch.lisp|240
macros|amoebum/src/macros/defkeys/expansion.lisp|150
macros|amoebum/src/macros/defkeys/builtins.lisp|70
macros|amoebum/src/macros/deftool.lisp|760
EOF
      ;;
    *)
      fail "unknown profile: $1"
      ;;
  esac
}

selected_profile="${1:-all}"
if [[ $# -gt 0 ]]; then
  case "${1}" in
    --help|-h)
      usage
      exit 0
      ;;
    --list-profiles)
      if [[ $# -ne 1 ]]; then
        usage >&2
        exit 2
      fi
      list_profiles
      exit 0
      ;;
    --strict)
      STRICT=1
      shift
      selected_profile="${1:-all}"
      ;;
  esac
fi

if [[ $# -gt 2 ]]; then
  usage >&2
  exit 2
fi

case "${selected_profile}" in
  all) ;;
  worktrees|packages|commands|facades|state|policy|ui|extensions|shell|macros) ;;
  *)
    usage >&2
    exit 2
    ;;
esac

profiles_to_run=()
if [[ "${selected_profile}" == "all" ]]; then
  profiles_to_run=("${PROFILES[@]}")
else
  profiles_to_run=("${selected_profile}")
fi

total_checked=0
total_failed=0
total_missing=0

for profile in "${profiles_to_run[@]}"; do
  while IFS='|' read -r rule_profile relpath max_lines; do
    [[ -z "${relpath}" ]] && continue
    path="${REPO_ROOT}/${relpath}"
    if [[ ! -f "${path}" ]]; then
      total_missing=$((total_missing + 1))
      status="MISSING"
      echo "LINE_COUNT_AUDIT profile=${rule_profile} path=${relpath} max=${max_lines} status=${status}"
      if [[ "${STRICT}" -eq 1 ]]; then
        total_failed=$((total_failed + 1))
      fi
      continue
    fi

    total_checked=$((total_checked + 1))
    actual_lines="$(line_count "${path}")"
    status="PASS"
    if (( actual_lines > max_lines )); then
      total_failed=$((total_failed + 1))
      status="FAIL"
    fi
    echo "LINE_COUNT_AUDIT profile=${rule_profile} path=${relpath} lines=${actual_lines} max=${max_lines} status=${status}"
  done < <(profile_rules "${profile}")
done

if (( total_failed > 0 )); then
  echo "LINE_COUNT_AUDIT_FAIL profiles=${selected_profile} checked=${total_checked} missing=${total_missing} failed=${total_failed}" >&2
  exit 1
fi

echo "LINE_COUNT_AUDIT_OK profiles=${selected_profile} checked=${total_checked} missing=${total_missing}"
