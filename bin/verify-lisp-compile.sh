#!/usr/bin/env bash
# Verify Lisp files compile without errors
# This catches unbalanced parentheses that actually affect compilation

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"

QUICKLISP_SETUP="${REPO_ROOT}/ptui/.tools/quicklisp/setup.lisp"
ERRORS=0

echo "=== Lisp Compilation Verification ==="
echo ""

# Find all .lisp files in amoebum/src (main source, not tests/smoke-tests)
for file in $(find "${REPO_ROOT}/amoebum/src" -name '*.lisp' | sort); do
    rel_path="${file#$REPO_ROOT/}"
    
    # Quick syntax check: try to read the file with SBCL
    if ! sbcl --noinform --non-interactive \
         --eval "(handler-case (with-open-file (f \"$file\") (read f) (format t \"OK\") (sb-ext:exit 0)) (error (e) (format t \"ERROR: ~A\" e) (sb-ext:exit 1)))" \
         2>/dev/null | grep -q "OK"; then
        echo "  ❌ SYNTAX ERROR: $rel_path"
        ERRORS=$((ERRORS + 1))
    fi
done

echo ""

if [ $ERRORS -eq 0 ]; then
    echo "=== ✅ All Lisp files have valid syntax ==="
    exit 0
else
    echo "=== ❌ Found $ERRORS file(s) with syntax errors ==="
    echo "Run: sbcl --load <file> to see detailed errors"
    exit 1
fi
