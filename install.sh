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

echo "╔══════════════════════════════════════╗"
echo "║       amoebum install                ║"
echo "╚══════════════════════════════════════╝"
echo ""

# ── 1. Build binary ──────────────────────────────────────────────────────

echo "1. Building amoebum binary..."
echo ""

if ! command -v sbcl >/dev/null 2>&1; then
  echo "  [FAIL] sbcl not found. Install SBCL first:" >&2
  echo "         apt install sbcl  OR  brew install sbcl" >&2
  exit 1
fi

bash "$REPO_ROOT/bin/build-binary.sh"

BINARY="$REPO_ROOT/dist/amoebum"
if [ ! -f "$BINARY" ]; then
  echo "  [FAIL] Binary not found at $BINARY" >&2
  exit 1
fi
echo "  [ok]   Binary built: $BINARY ($(du -h "$BINARY" | cut -f1))"
echo ""

# ── 2. Install binary ────────────────────────────────────────────────────

echo "2. Installing to $INSTALL_PREFIX..."
echo ""

mkdir -p "$INSTALL_PREFIX" "$LIBEXEC_DIR"

# The SBCL image goes into libexec (it's large and not user-facing)
cp "$BINARY" "$LIBEXEC_DIR/amoebum-sbcl"
chmod +x "$LIBEXEC_DIR/amoebum-sbcl"

# Shell wrapper handles --help and forwards args past SBCL's runtime parser
cat > "$INSTALL_PREFIX/amoebum" <<'WRAPPER'
#!/usr/bin/env bash
# amoebum — wrapper that forwards args to the SBCL image
SELF_DIR="$(cd "$(dirname "$0")" && pwd)"
AMOEBUM_BIN="${SELF_DIR}/../libexec/amoebum/amoebum-sbcl"

if [ ! -x "$AMOEBUM_BIN" ]; then
  echo "amoebum: binary not found at $AMOEBUM_BIN" >&2
  echo "Run install.sh to rebuild." >&2
  exit 1
fi

# SBCL intercepts --help/--version before the Lisp toplevel runs.
# Use end-of-runtime-options to pass all args to the amoebum main function.
exec "$AMOEBUM_BIN" --end-runtime-options "$@"
WRAPPER
chmod +x "$INSTALL_PREFIX/amoebum"

echo "  [ok]   $LIBEXEC_DIR/amoebum-sbcl"
echo "  [ok]   $INSTALL_PREFIX/amoebum (wrapper)"

# Check if INSTALL_PREFIX is on PATH
if ! echo "$PATH" | tr ':' '\n' | grep -qx "$INSTALL_PREFIX"; then
  echo ""
  echo "  [warn] $INSTALL_PREFIX is not on your PATH."
  echo "         Add to your shell profile:"
  echo "           export PATH=\"$INSTALL_PREFIX:\$PATH\""
fi
echo ""

# ── 3. Install commithooks ───────────────────────────────────────────────

echo "3. Installing commithooks..."
echo ""

COMMITHOOKS="${COMMITHOOKS_DIR:-$HOME/Documents/commithooks}"

if [ ! -d "$COMMITHOOKS/lib" ] || [ ! -f "$COMMITHOOKS/lib/common.sh" ]; then
  echo "  [skip] Commithooks source not found at $COMMITHOOKS"
  echo "         Clone it to enable git hooks:"
  echo "           git clone https://github.com/rahulrajaram/commithooks.git ~/Documents/commithooks"
  echo "         Then re-run ./install.sh"
else
  # Copy dispatchers
  for hook in pre-commit commit-msg pre-push post-checkout post-merge; do
    src="$COMMITHOOKS/$hook"
    dst="$GIT_DIR/hooks/$hook"
    [ -f "$src" ] || { echo "  [skip] $hook (not in source)"; continue; }
    cp "$src" "$dst"
    chmod +x "$dst"
    echo "  [ok]   $hook → .git/hooks/"
  done

  # Copy library
  rm -rf "${GIT_DIR:?}/lib"
  cp -r "$COMMITHOOKS/lib" "$GIT_DIR/lib"
  echo "  [ok]   lib/ → .git/lib/ ($(ls "$GIT_DIR/lib/" | wc -l) modules)"

  # Unset core.hooksPath if set
  if git config --get core.hooksPath >/dev/null 2>&1; then
    git config --unset core.hooksPath
    echo "  [ok]   Unset core.hooksPath"
  fi
fi
echo ""

# ── 4. Verify ─────────────────────────────────────────────────────────────

echo "4. Verifying..."
echo ""
VERIFY_OUTPUT=$("$INSTALL_PREFIX/amoebum" --json --prompt "ping" 2>&1 || true)
if echo "$VERIFY_OUTPUT" | grep -q '"ok":true'; then
  echo "  [ok]   amoebum responds to --json --prompt"
else
  echo "  [warn] Verification returned unexpected output"
  echo "         $VERIFY_OUTPUT" | head -3
fi
echo ""

# ── Summary ───────────────────────────────────────────────────────────────

echo "Done."
echo ""
echo "  Binary:   $INSTALL_PREFIX/amoebum"
echo "  Image:    $LIBEXEC_DIR/amoebum-sbcl ($(du -h "$LIBEXEC_DIR/amoebum-sbcl" | cut -f1))"
echo "  Hooks:    $(ls "$GIT_DIR/hooks/" 2>/dev/null | grep -cv '\.sample$' || echo 0) active"
echo ""
echo "  Run:      amoebum          # launch TUI"
echo "            amoebum --json   # JSON/headless mode"
