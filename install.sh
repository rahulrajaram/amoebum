#!/usr/bin/env bash
# install.sh — build amoebum binary and install commithooks
set -euo pipefail

REPO_ROOT="$(git rev-parse --show-toplevel 2>/dev/null)" || {
  echo "Error: not inside a git repository." >&2
  exit 1
}

GIT_DIR="$(git rev-parse --git-dir)"
INSTALL_PREFIX="${INSTALL_PREFIX:-$HOME/.local/bin}"
LIBEXEC_DIR="${INSTALL_PREFIX}/../libexec/amoebum"
INSTALL_RUNTIME="$LIBEXEC_DIR/amoebum-sbcl"
INSTALL_WRAPPER="$INSTALL_PREFIX/amoebum"
NATIVE_SO="$REPO_ROOT/ptui/native/libptui_native.so"
AMOEBUM_INSTALL_LOG_DIR="${AMOEBUM_INSTALL_LOG_DIR:-$HOME/.local/var/log/amoebum}"
AMOEBUM_INSTALL_LOG_FILE="${AMOEBUM_INSTALL_LOG_FILE:-}"
AMOEBUM_INSTALL_LOG="${AMOEBUM_INSTALL_LOG:-1}"

# Always capture a fresh per-run install log unless explicitly disabled.
if [ -z "$AMOEBUM_INSTALL_LOG_FILE" ]; then
  LOG_TS="$(date +%Y%m%d-%H%M%S)"
  AMOEBUM_INSTALL_LOG_FILE="$AMOEBUM_INSTALL_LOG_DIR/install-${LOG_TS}.log"
fi
if [ "$AMOEBUM_INSTALL_LOG" != "0" ]; then
  mkdir -p "$AMOEBUM_INSTALL_LOG_DIR"
  touch "$AMOEBUM_INSTALL_LOG_FILE"
fi

log_ok() {
  echo "  [ok]   $1"
}

log_warn() {
  echo "  [warn] $1"
}

log_fail() {
  echo "  [fail] $1" >&2
}

install_atomically() {
  local src="$1"
  local dst="$2"
  local mode="${3:-}"
  local tmp

  tmp="${dst}.tmp.$$.$RANDOM"
  cp "$src" "$tmp"
  if [ -n "$mode" ]; then
    chmod "$mode" "$tmp"
  fi
  mv -f "$tmp" "$dst"
}

run_with_timeout() {
  local secs="$1"
  shift
  local out_file pid rc start now
  local cmd_desc

  cmd_desc="$*"
  out_file="$(mktemp)"
  [ "$AMOEBUM_INSTALL_LOG" != "0" ] && echo "[run_with_timeout] start: timeout=${secs}s cmd: $cmd_desc" >> "$AMOEBUM_INSTALL_LOG_FILE"

  # Prefer system timeout when available.
  if command -v timeout >/dev/null 2>&1; then
    timeout "$secs" "$@" >"$out_file" 2>&1
    rc=$?
    cat "$out_file"
    [ "$AMOEBUM_INSTALL_LOG" != "0" ] && cat "$out_file" >> "$AMOEBUM_INSTALL_LOG_FILE"
    [ "$AMOEBUM_INSTALL_LOG" != "0" ] && echo "[run_with_timeout] done rc=$rc" >> "$AMOEBUM_INSTALL_LOG_FILE"
    rm -f "$out_file"
    return "$rc"
  fi

  ("$@" >"$out_file" 2>&1) &
  pid="$!"
  start="$(date +%s)"

  while kill -0 "$pid" 2>/dev/null; do
    now="$(date +%s)"
    if [ $((now - start)) -ge "$secs" ]; then
      kill "$pid" 2>/dev/null || true
      sleep 1
      kill -9 "$pid" 2>/dev/null || true
      wait "$pid" 2>/dev/null || true
      echo "[timeout] command exceeded ${secs}s: $cmd_desc" >&2
      [ "$AMOEBUM_INSTALL_LOG" != "0" ] && echo "[timeout] command exceeded ${secs}s: $cmd_desc" >> "$AMOEBUM_INSTALL_LOG_FILE"
      cat "$out_file"
      [ "$AMOEBUM_INSTALL_LOG" != "0" ] && cat "$out_file" >> "$AMOEBUM_INSTALL_LOG_FILE"
      rm -f "$out_file"
      return 124
    fi
    sleep 0.1
  done

  wait "$pid"
  rc=$?
  cat "$out_file"
  [ "$AMOEBUM_INSTALL_LOG" != "0" ] && cat "$out_file" >> "$AMOEBUM_INSTALL_LOG_FILE"
  [ "$AMOEBUM_INSTALL_LOG" != "0" ] && echo "[run_with_timeout] done rc=$rc" >> "$AMOEBUM_INSTALL_LOG_FILE"
  rm -f "$out_file"
  return "$rc"
}

if [ "$AMOEBUM_INSTALL_LOG" != "0" ]; then
  # Tee all install output to a persisted log file for post-hoc diagnosis.
  exec > >(tee -a "$AMOEBUM_INSTALL_LOG_FILE") 2> >(tee -a "$AMOEBUM_INSTALL_LOG_FILE" >&2)
fi

echo "╔══════════════════════════════════════╗"
echo "║       amoebum install                ║"
echo "╚══════════════════════════════════════╗"
echo ""

# ── 1. Build binary ──────────────────────────────────────────────────────

echo "1. Building amoebum binary..."
echo ""

if ! command -v sbcl >/dev/null 2>&1; then
  log_fail "sbcl not found. Install SBCL first:"
  echo "         apt install sbcl  OR  brew install sbcl" >&2
  exit 1
fi

bash "$REPO_ROOT/bin/build-binary.sh"

BINARY="$REPO_ROOT/dist/amoebum"
if [ ! -f "$BINARY" ]; then
  log_fail "Binary not found at $BINARY"
  exit 1
fi
BINARY_SIZE="$(du -h "$BINARY" | cut -f1)"
log_ok "Binary built: $BINARY ($BINARY_SIZE)"
echo ""

# ── 2. Install binary ────────────────────────────────────────────────────

echo "2. Installing to $INSTALL_PREFIX..."
echo ""

mkdir -p "$INSTALL_PREFIX" "$LIBEXEC_DIR"

# Install runtime atomically to avoid stale/racy runtime reads.
install_atomically "$BINARY" "$INSTALL_RUNTIME" "+x"
log_ok "$INSTALL_RUNTIME"

# Native shared library must be next to the binary for CFFI to find it.
if [ -f "$NATIVE_SO" ]; then
  install_atomically "$NATIVE_SO" "$LIBEXEC_DIR/libptui_native.so"
  log_ok "libptui_native.so → $LIBEXEC_DIR/"
else
  log_warn "libptui_native.so not found at $NATIVE_SO"
  log_warn "         Build it with: cd ptui/native && make"
fi

# Install wrapper atomically so file replacement never leaves a partial script.
# This installed wrapper intentionally stays repo-independent: it should keep
# working away from the checkout and must not depend on repo-local `.yarli/`
# bootstrap state or the repo wrapper scripts under `./bin/`.
TMP_WRAPPER="$INSTALL_WRAPPER.tmp.$$.$RANDOM"
cat > "$TMP_WRAPPER" <<'WRAPPER'
#!/usr/bin/env bash
# amoebum — installed wrapper that forwards args to the SBCL image
SCRIPT_PATH="${BASH_SOURCE[0]:-$0}"
if command -v readlink >/dev/null 2>&1; then
  if RESOLVED_PATH="$(readlink -f "$SCRIPT_PATH" 2>/dev/null)"; then
    SCRIPT_PATH="$RESOLVED_PATH"
  fi
fi
SELF_DIR="$(cd "$(dirname "$SCRIPT_PATH")" && pwd)"
AMOEBUM_BIN="${SELF_DIR}/../libexec/amoebum/amoebum-sbcl"

if [ ! -x "$AMOEBUM_BIN" ]; then
  echo "amoebum: binary not found at $AMOEBUM_BIN" >&2
  echo "Run install.sh to rebuild." >&2
  exit 1
fi

# SBCL intercepts --help/--version before the Lisp toplevel runs.
# Use end-of-runtime-options to pass all args to the amoebum main function.
: "${AMOEBUM_RUNTIME_LOG_FILE:=$HOME/.amoebum/runtime/runtime.log}"
export AMOEBUM_RUNTIME_LOG_FILE
mkdir -p "$(dirname "$AMOEBUM_RUNTIME_LOG_FILE")"
printf '%s wrapper invoke: %s\n' "$(date -u +%Y-%m-%dT%H:%M:%SZ)" "$*" >> "$AMOEBUM_RUNTIME_LOG_FILE"
if [ "${AMOEBUM_RUNTIME_LOG_TEE:-0}" = "1" ]; then
  exec "$AMOEBUM_BIN" --dynamic-space-size 512 --end-runtime-options "$@" \
    > >(tee -a "$AMOEBUM_RUNTIME_LOG_FILE") \
    2> >(tee -a "$AMOEBUM_RUNTIME_LOG_FILE" >&2)
fi
exec "$AMOEBUM_BIN" --dynamic-space-size 512 --end-runtime-options "$@"
WRAPPER
chmod +x "$TMP_WRAPPER"
mv -f "$TMP_WRAPPER" "$INSTALL_WRAPPER"
log_ok "$INSTALL_WRAPPER (wrapper)"

echo ""

# ── 3. Install commithooks ────────────────────────────────────────────────

echo "3. Installing commithooks..."
echo ""

COMMITHOOKS="${COMMITHOOKS_DIR:-$HOME/Documents/commithooks}"

if [ ! -d "$COMMITHOOKS/lib" ] || [ ! -f "$COMMITHOOKS/lib/common.sh" ]; then
  log_warn "Commithooks source not found at $COMMITHOOKS"
  log_warn "       Clone it to enable git hooks:"
  log_warn "         git clone https://github.com/rahulrajaram/commithooks.git ~/Documents/commithooks"
  log_warn "       Then re-run ./install.sh"
else
  # Copy dispatchers
  for hook in pre-commit commit-msg pre-push post-checkout post-merge; do
    src="$COMMITHOOKS/$hook"
    dst="$GIT_DIR/hooks/$hook"
    [ -f "$src" ] || { log_warn "$hook (not in source)"; continue; }
    cp "$src" "$dst"
    chmod +x "$dst"
    log_ok "$hook → .git/hooks/"
  done

  # Copy library
  rm -rf "${GIT_DIR:?}/lib"
  cp -r "$COMMITHOOKS/lib" "$GIT_DIR/lib"
  log_ok "lib/ → .git/lib/ ($(ls "$GIT_DIR/lib/" | wc -l) modules)"

  # Unset core.hooksPath if set
  if git config --get core.hooksPath >/dev/null 2>&1; then
    git config --unset core.hooksPath
    log_ok "Unset core.hooksPath"
  fi

  mkdir -p "$REPO_ROOT/.githooks"

  scaffold_hook_if_missing() {
    local hook_name="$1"
    local target="$REPO_ROOT/.githooks/$hook_name"
    if [ -e "$target" ] || [ -e "$REPO_ROOT/scripts/git-hooks/$hook_name" ]; then
      log_ok "$hook_name local hook already present"
      return 0
    fi
    cat > "$target"
    chmod +x "$target"
    log_ok "$hook_name → .githooks/"
  }

  scaffold_hook_if_missing pre-push <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
COMMITHOOKS_DIR="$(git rev-parse --git-dir)"
source "$COMMITHOOKS_DIR/lib/common.sh"
source "$COMMITHOOKS_DIR/lib/pre-push.sh"

commithooks_reject_wip_commits "$@"
commithooks_check_branch_name
EOF

  scaffold_hook_if_missing post-checkout <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
COMMITHOOKS_DIR="$(git rev-parse --git-dir)"
source "$COMMITHOOKS_DIR/lib/common.sh"
source "$COMMITHOOKS_DIR/lib/deps.sh"

commithooks_reinstall_if_changed "${1:-}" "${2:-}"
EOF

  scaffold_hook_if_missing post-merge <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
COMMITHOOKS_DIR="$(git rev-parse --git-dir)"
source "$COMMITHOOKS_DIR/lib/common.sh"
source "$COMMITHOOKS_DIR/lib/deps.sh"

commithooks_reinstall_if_changed
EOF

  if rg -n '^\.githooks/?$|^\.githooks/' "$REPO_ROOT/.gitignore" >/dev/null 2>&1; then
    log_warn ".githooks/ is ignored by .gitignore; local hook stubs won't be tracked."
  fi
fi
echo ""

# ── 4. Verify consistency ────────────────────────────────────────────────

echo "4. Verifying consistency..."
echo ""

if ! [ -x "$INSTALL_RUNTIME" ]; then
  log_fail "Installed runtime is not executable: $INSTALL_RUNTIME"
  exit 1
fi

if command -v sha256sum >/dev/null 2>&1; then
  DIST_HASH="$(sha256sum "$BINARY" | awk '{print $1}')"
  INSTALL_HASH="$(sha256sum "$INSTALL_RUNTIME" | awk '{print $1}')"
  if [ "$DIST_HASH" != "$INSTALL_HASH" ]; then
    log_fail "Runtime hash mismatch!"
    log_warn "  dist:    $DIST_HASH"
    log_warn "  install: $INSTALL_HASH"
    exit 1
  fi
  log_ok "Runtime hash matches source-built image: $INSTALL_HASH"
fi

if [ ! -f "$INSTALL_WRAPPER" ]; then
  log_fail "Wrapper missing: $INSTALL_WRAPPER"
  exit 1
fi

VERIFY_OUTPUT=$(run_with_timeout 8 "$INSTALL_PREFIX/amoebum" --json --prompt "ping" 2>&1 || true)
if echo "$VERIFY_OUTPUT" | grep -q '"ok":true'; then
  log_ok "amoebum responds to --json --prompt"
else
  log_warn "Verification returned unexpected output"
  echo "         $VERIFY_OUTPUT" | sed -n '1,2p'
fi

if bash -n "$INSTALL_WRAPPER"; then
  log_ok "Wrapper syntax check passed"
else
  log_warn "Wrapper syntax check failed"
fi

if ! echo "$PATH" | tr ':' '\n' | grep -qx "$INSTALL_PREFIX"; then
  log_warn "$INSTALL_PREFIX is not on your PATH."
  log_warn "Add to your shell profile:"
  log_warn "  export PATH=\"$INSTALL_PREFIX:\\$PATH\""
fi
echo ""

# ── Summary ───────────────────────────────────────────────────────────────

echo "Done."
echo ""
echo "  Binary:   $INSTALL_WRAPPER"
echo "  Image:    $INSTALL_RUNTIME ($(du -h "$INSTALL_RUNTIME" | cut -f1))"
echo "  Hooks:    $(ls "$GIT_DIR/hooks/" 2>/dev/null | grep -cv '\\.sample$' || echo 0) active"
if [ "$AMOEBUM_INSTALL_LOG" != "0" ]; then
  echo "  Log:      $AMOEBUM_INSTALL_LOG_FILE"
fi
echo ""
echo "  Run:      amoebum          # launch TUI"
echo "            amoebum --json   # JSON/headless mode"
