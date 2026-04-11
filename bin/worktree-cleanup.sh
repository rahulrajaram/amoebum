#!/usr/bin/env bash
# worktree-cleanup.sh — Audit and optionally prune stale git worktrees
#
# Flags worktrees as removable when they are:
#   (a) prunable (git reports them as prunable, or gitdir missing)
#   (b) on a branch with zero commits ahead of master
#   (c) untouched (HEAD mtime) for > 7 days
#   (d) located under /tmp/ or .claude/worktrees/
#
# By default runs in --dry-run mode. Pass --force to actually remove.
# Never touches the main working tree.

set -euo pipefail

MODE="dry-run"
STALE_DAYS=7
MAIN_BRANCH="master"

for arg in "$@"; do
  case "$arg" in
    --dry-run) MODE="dry-run" ;;
    --force)   MODE="force" ;;
    -h|--help)
      cat <<EOF
Usage: worktree-cleanup.sh [--dry-run|--force]

  --dry-run  (default) Report stale worktrees without removing them.
  --force    Actually remove flagged worktrees via 'git worktree remove --force'.

Criteria for flagging a worktree as stale (ANY one is sufficient):
  - git reports the worktree as prunable
  - branch has zero commits ahead of ${MAIN_BRANCH}
  - HEAD file untouched for more than ${STALE_DAYS} days
  - path is under /tmp/ or .claude/worktrees/

The main working tree is always kept.
EOF
      exit 0
      ;;
    *)
      echo "worktree-cleanup: unknown argument: $arg" >&2
      exit 2
      ;;
  esac
done

REPO_ROOT="$(git rev-parse --show-toplevel)"
MAIN_WORKTREE="$(git -C "$REPO_ROOT" rev-parse --show-toplevel)"

now_epoch="$(date +%s)"
stale_cutoff=$(( now_epoch - STALE_DAYS * 86400 ))

kept=0
removed=0
errored=0
flagged=0

parse_worktrees() {
  # Emits one line per worktree: path<TAB>head<TAB>branch<TAB>prunable(0|1)
  git -C "$REPO_ROOT" worktree list --porcelain | awk '
    BEGIN { path=""; head=""; branch=""; prunable=0 }
    /^worktree / { if (path != "") { print path "\t" head "\t" branch "\t" prunable; } path=substr($0,10); head=""; branch=""; prunable=0 }
    /^HEAD /     { head=substr($0,6) }
    /^branch /   { branch=substr($0,8) }
    /^detached/  { branch="(detached)" }
    /^prunable/  { prunable=1 }
    END { if (path != "") print path "\t" head "\t" branch "\t" prunable }
  '
}

is_under_cleanup_dir() {
  local p="$1"
  case "$p" in
    /tmp/*) return 0 ;;
    */.claude/worktrees/*) return 0 ;;
    *) return 1 ;;
  esac
}

head_mtime() {
  local wt="$1"
  local head_file="$wt/.git"
  if [ -f "$head_file" ]; then
    # gitfile — read gitdir and stat HEAD there
    local gitdir
    gitdir=$(sed -n 's/^gitdir: //p' "$head_file" 2>/dev/null || true)
    if [ -n "$gitdir" ]; then
      case "$gitdir" in
        /*) : ;;
        *)  gitdir="$wt/$gitdir" ;;
      esac
      if [ -f "$gitdir/HEAD" ]; then
        stat -c %Y "$gitdir/HEAD" 2>/dev/null || echo 0
        return
      fi
    fi
  elif [ -f "$wt/HEAD" ]; then
    stat -c %Y "$wt/HEAD" 2>/dev/null || echo 0
    return
  fi
  echo 0
}

commits_ahead_of_master() {
  local branch="$1"
  if [ -z "$branch" ] || [ "$branch" = "(detached)" ]; then
    echo "-1"
    return
  fi
  local short="${branch#refs/heads/}"
  git -C "$REPO_ROOT" rev-list --count "${MAIN_BRANCH}..${short}" 2>/dev/null || echo "-1"
}

printf 'worktree-cleanup: mode=%s cutoff_days=%d main=%s\n' "$MODE" "$STALE_DAYS" "$MAIN_WORKTREE"
printf '%s\n' '----------------------------------------------------------------'

while IFS=$'\t' read -r path head branch prunable; do
  [ -z "$path" ] && continue

  if [ "$path" = "$MAIN_WORKTREE" ]; then
    printf 'KEEP   main             %s\n' "$path"
    kept=$((kept + 1))
    continue
  fi

  reasons=()
  [ "$prunable" = "1" ] && reasons+=("prunable")

  ahead="$(commits_ahead_of_master "$branch")"
  if [ "$ahead" = "0" ]; then
    reasons+=("no-commits-ahead")
  fi

  mtime="$(head_mtime "$path")"
  if [ "$mtime" -gt 0 ] && [ "$mtime" -lt "$stale_cutoff" ]; then
    age_days=$(( (now_epoch - mtime) / 86400 ))
    reasons+=("stale-${age_days}d")
  fi

  if is_under_cleanup_dir "$path"; then
    reasons+=("cleanup-path")
  fi

  if [ "${#reasons[@]}" -eq 0 ]; then
    printf 'KEEP   active           %s  (branch=%s ahead=%s)\n' "$path" "$branch" "$ahead"
    kept=$((kept + 1))
    continue
  fi

  flagged=$((flagged + 1))
  reason_str="$(IFS=,; echo "${reasons[*]}")"

  if [ "$MODE" = "dry-run" ]; then
    printf 'FLAG   %-16s %s  (%s)\n' "would-remove" "$path" "$reason_str"
    kept=$((kept + 1))
  else
    printf 'RM     %-16s %s  (%s)\n' "$reason_str" "$path" "$reason_str"
    if git -C "$REPO_ROOT" worktree remove --force "$path" >/dev/null 2>&1; then
      removed=$((removed + 1))
    else
      printf '  ERROR removing %s\n' "$path" >&2
      errored=$((errored + 1))
    fi
  fi
done < <(parse_worktrees)

if [ "$MODE" = "force" ] && [ "$removed" -gt 0 ]; then
  git -C "$REPO_ROOT" worktree prune || true
fi

printf '%s\n' '----------------------------------------------------------------'
printf 'worktree-cleanup summary: mode=%s kept=%d removed=%d errored=%d flagged=%d\n' \
  "$MODE" "$kept" "$removed" "$errored" "$flagged"

if [ "$errored" -gt 0 ]; then
  exit 1
fi
exit 0
