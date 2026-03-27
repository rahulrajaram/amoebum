#!/usr/bin/env python3
import sys

def check_parens(filename):
    with open(filename, 'r') as f:
        content = f.read()

    # Count parentheses (simple version - doesn't handle strings/comments)
    open_parens = content.count('(')
    close_parens = content.count(')')

    print(f"{filename}:")
    print(f"  Open parens: {open_parens}")
    print(f"  Close parens: {close_parens}")
    print(f"  Diff: {open_parens - close_parens}")

    # More thorough check using stack
    stack = []
    in_string = False
    string_char = None
    escape_next = False

    for i, char in enumerate(content):
        if escape_next:
            escape_next = False
            continue

        if char == '\\' and in_string:
            escape_next = True
            continue

        if char in '"\'':
            if not in_string:
                in_string = True
                string_char = char
            elif char == string_char:
                in_string = False
                string_char = None
            continue

        if in_string:
            continue

        if char == '(':
            line = content[:i].count('\n') + 1
            stack.append(line)
        elif char == ')':
            if stack:
                stack.pop()
            else:
                print(f"  ERROR: Unbalanced ) at position {i}")

    if stack:
        print(f"  ERROR: Unbalanced ( - {len(stack)} unclosed opens")
        print(f"  Lines of unclosed opens: {stack[-10:]}")  # Show last 10
        return False
    else:
        print(f"  OK: All parentheses balanced!")
        return True

if __name__ == "__main__":
    files = [
        "/home/rahul/Documents/amoebum/amoebum/src/config.lisp",
        "/home/rahul/Documents/amoebum/amoebum/src/config/loader.lisp",
        "/home/rahul/Documents/amoebum/amoebum/src/main.lisp"
    ]

    all_ok = True
    for f in files:
        if not check_parens(f):
            all_ok = False
        print()

    sys.exit(0 if all_ok else 1)
