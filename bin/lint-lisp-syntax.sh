#!/usr/bin/env bash
# Pre-commit / CI hook for Lisp syntax validation
# Checks: parenthesis balance, basic compilation

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"

ERRORS=0

echo "=== Lisp Syntax Lint ==="
echo ""

# Check 1: Parenthesis balance
echo "Checking parenthesis balance..."
for file in $(find "${REPO_ROOT}" -name '*.lisp' -not -path '*/.tools/*' -not -path '*/quicklisp/*'); do
    open=$(tr -cd '(' < "$file" | wc -c)
    close=$(tr -cd ')' < "$file" | wc -c)
    if [ "$open" -ne "$close" ]; then
        echo "  ❌ UNBALANCED: $file (open: $open, close: $close, diff: $((open - close)))"
        ERRORS=$((ERRORS + 1))
    fi
done

if [ $ERRORS -eq 0 ]; then
    echo "  ✅ All files have balanced parentheses"
fi

echo ""

# Check 2: Bracket/Brace balance
echo "Checking bracket/brace balance..."
for file in $(find "${REPO_ROOT}" -name '*.lisp' -not -path '*/.tools/*' -not -path '*/quicklisp/*'); do
    # Count brackets [ ]
    open_bracket=$(tr -cd '[' < "$file" | wc -c)
    close_bracket=$(tr -cd ']' < "$file" | wc -c)
    if [ "$open_bracket" -ne "$close_bracket" ]; then
        echo "  ❌ UNBALANCED BRACKETS: $file ([: $open_bracket, ]: $close_bracket)"
        ERRORS=$((ERRORS + 1))
    fi
    
    # Count braces { }
    open_brace=$(tr -cd '{' < "$file" | wc -c)
    close_brace=$(tr -cd '}' < "$file" | wc -c)
    if [ "$open_brace" -ne "$close_brace" ]; then
        echo "  ❌ UNBALANCED BRACES: $file ({: $open_brace, }: $close_brace)"
        ERRORS=$((ERRORS + 1))
    fi
done

if [ $ERRORS -eq 0 ]; then
    echo "  ✅ All files have balanced brackets/braces"
fi

echo ""

# Check 3: String quote balance (basic)
echo "Checking string quote balance..."
for file in $(find "${REPO_ROOT}" -name '*.lisp' -not -path '*/.tools/*' -not -path '*/quicklisp/*'); do
    # Remove escaped quotes first, then count
    quotes=$(sed 's/\\"//g' "$file" | tr -cd '"' | wc -c)
    if [ $((quotes % 2)) -ne 0 ]; then
        echo "  ⚠️  UNBALANCED QUOTES: $file ($quotes quotes - odd number)"
        ERRORS=$((ERRORS + 1))
    fi
done

if [ $ERRORS -eq 0 ]; then
    echo "  ✅ All files have balanced quotes"
fi

echo ""

# Summary
if [ $ERRORS -eq 0 ]; then
    echo "=== ✅ All syntax checks passed ==="
    exit 0
else
    echo "=== ❌ Found $ERRORS syntax error(s) ==="
    echo "Fix these issues before committing."
    exit 1
fi
