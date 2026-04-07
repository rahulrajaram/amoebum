#!/usr/bin/env bash
# Pre-commit / CI hook for Lisp syntax validation
# Checks: parenthesis balance, basic compilation

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"

ERRORS=0

collect_lisp_files() {
    find "${REPO_ROOT}" -name '*.lisp' -not -path '*/.tools/*' -not -path '*/quicklisp/*'
}

skip_raw_delimiter_checks() {
    local file="$1"
    [[ "$file" == *"/test/snapshots/"* ]]
}

sanitized_source() {
    local file="$1"
    perl - "$file" <<'PERL'
use strict;
use warnings;

my $path = shift @ARGV;
open my $fh, '<', $path or die "open($path): $!";
local $/;
my $text = <$fh>;
close $fh;

my $out = '';
my $i = 0;
my $len = length($text);
my $in_string = 0;
my $escape_next = 0;
my $block_depth = 0;
my $char_lit = 0;

while ($i < $len) {
    my $ch = substr($text, $i, 1);
    my $next = $i + 1 < $len ? substr($text, $i + 1, 1) : '';

    if ($block_depth > 0) {
        if ($ch eq '#' && $next eq '|') {
            $block_depth++;
            $i += 2;
            next;
        }
        if ($ch eq '|' && $next eq '#') {
            $block_depth--;
            $i += 2;
            next;
        }
        $i++;
        next;
    }

    if ($in_string) {
        if ($escape_next) {
            $escape_next = 0;
            $i++;
            next;
        }
        if ($ch eq '\\') {
            $escape_next = 1;
            $i++;
            next;
        }
        if ($ch eq '"') {
            $in_string = 0;
        }
        $i++;
        next;
    }

    if ($char_lit) {
        $char_lit = 0;
        $i++;
        next;
    }

    if ($ch eq ';') {
        $i++;
        $i++ while $i < $len && substr($text, $i, 1) ne "\n";
        next;
    }

    if ($ch eq '#' && $next eq '|') {
        $block_depth++;
        $i += 2;
        next;
    }

    if ($ch eq '#' && $next eq '\\') {
        $char_lit = 1;
        $i += 2;
        next;
    }

    if ($ch eq '"') {
        $in_string = 1;
        $i++;
        next;
    }

    $out .= $ch;
    $i++;
}

print $out;
PERL
}

delimiter_state() {
    local file="$1"
    perl - "$file" <<'PERL'
use strict;
use warnings;

my $path = shift @ARGV;
open my $fh, '<', $path or die "open($path): $!";
local $/;
my $text = <$fh>;
close $fh;

my $i = 0;
my $len = length($text);
my $in_string = 0;
my $escape_next = 0;
my $block_depth = 0;
my $char_lit = 0;

while ($i < $len) {
    my $ch = substr($text, $i, 1);
    my $next = $i + 1 < $len ? substr($text, $i + 1, 1) : '';

    if ($block_depth > 0) {
        if ($ch eq '#' && $next eq '|') {
            $block_depth++;
            $i += 2;
            next;
        }
        if ($ch eq '|' && $next eq '#') {
            $block_depth--;
            $i += 2;
            next;
        }
        $i++;
        next;
    }

    if ($in_string) {
        if ($escape_next) {
            $escape_next = 0;
            $i++;
            next;
        }
        if ($ch eq '\\') {
            $escape_next = 1;
            $i++;
            next;
        }
        if ($ch eq '"') {
            $in_string = 0;
        }
        $i++;
        next;
    }

    if ($char_lit) {
        $char_lit = 0;
        $i++;
        next;
    }

    if ($ch eq ';') {
        $i++;
        $i++ while $i < $len && substr($text, $i, 1) ne "\n";
        next;
    }

    if ($ch eq '#' && $next eq '|') {
        $block_depth++;
        $i += 2;
        next;
    }

    if ($ch eq '#' && $next eq '\\') {
        $char_lit = 1;
        $i += 2;
        next;
    }

    if ($ch eq '"') {
        $in_string = 1;
    }

    $i++;
}

print "in_string=$in_string\n";
print "block_depth=$block_depth\n";
PERL
}

count_char() {
    local text="$1"
    local needle="$2"
    printf '%s' "$text" | tr -cd "$needle" | wc -c | tr -d ' '
}

mapfile -t LISP_FILES < <(collect_lisp_files)

echo "=== Lisp Syntax Lint ==="
echo ""

# Check 1: Parenthesis balance
echo "Checking parenthesis balance..."
if ! sbcl --script "${REPO_ROOT}/bin/lisp-paren-check.lisp" --project; then
    ERRORS=$((ERRORS + 1))
fi

if [ $ERRORS -eq 0 ]; then
    echo "  ✅ All files have balanced parentheses"
fi

echo ""

# Check 2: Bracket/Brace balance
echo "Checking bracket/brace balance..."
for file in "${LISP_FILES[@]}"; do
    if skip_raw_delimiter_checks "$file"; then
        continue
    fi

    sanitized="$(sanitized_source "$file")"

    open_bracket=$(count_char "$sanitized" '[')
    close_bracket=$(count_char "$sanitized" ']')
    if [ "$open_bracket" -ne "$close_bracket" ]; then
        echo "  ❌ UNBALANCED BRACKETS: $file ([: $open_bracket, ]: $close_bracket)"
        ERRORS=$((ERRORS + 1))
    fi

    open_brace=$(count_char "$sanitized" '{')
    close_brace=$(count_char "$sanitized" '}')
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
for file in "${LISP_FILES[@]}"; do
    if skip_raw_delimiter_checks "$file"; then
        continue
    fi

    state="$(delimiter_state "$file")"
    in_string="$(printf '%s\n' "$state" | awk -F= '/^in_string=/{print $2}')"
    block_depth="$(printf '%s\n' "$state" | awk -F= '/^block_depth=/{print $2}')"

    if [ "${in_string}" != "0" ]; then
        echo "  ⚠️  UNTERMINATED STRING: $file"
        ERRORS=$((ERRORS + 1))
    fi
    if [ "${block_depth}" != "0" ]; then
        echo "  ⚠️  UNTERMINATED BLOCK COMMENT: $file (depth: $block_depth)"
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
