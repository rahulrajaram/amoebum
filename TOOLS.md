# Amoebum Development Tools

Quick reference for tools and scripts available in this project.

---

## 🚀 Quick Start

```bash
# Before any build or commit - run the linter
bin/amoebum-lint -a                    # Check all files
bin/amoebum-lint src/file.lisp         # Check specific file

# If something fails - diagnose first
bin/amoebum-diagnose build             # Build issues
bin/amoebum-diagnose syntax            # Syntax errors
bin/amoebum-diagnose runtime           # Runtime errors
bin/amoebum-diagnose theme             # Theme loading
bin/amoebum-diagnose all               # Everything
```

---

## Available Scripts (`bin/`)

| Script | Purpose | Usage |
|--------|---------|-------|
| `amoebum-lint` | **Unified lint tool** - syntax + compilation checks | `bin/amoebum-lint [files...]` or `bin/amoebum-lint -a` |
| `amoebum-diagnose` | **Diagnostic helper** - diagnose before retrying | `bin/amoebum-diagnose [build\|syntax\|runtime\|theme\|all]` |
| `lint-lisp-syntax.sh` | Check paren/bracket/brace/quote balance | `bash bin/lint-lisp-syntax.sh <file.lisp>` |
| `verify-lisp-compile.sh` | Verify Lisp files compile without errors | `bash bin/verify-lisp-compile.sh <file.lisp>` |
| `check-parens.sh` | Legacy syntax checker (use amoebum-lint) | `bash bin/check-parens.sh` |

---

## Common Operations

### Loading the System
```lisp
(ql:quickload :amoebum)
```

### Running Tests
```lisp
(asdf:test-system :amoebum)
```

### Building Binary
```bash
./build-binary.sh
```

### Checking Syntax Before Commit
```bash
# Recommended: Use unified lint tool
bin/amoebum-lint -a

# Or check specific files
bin/amoebum-lint src/yaml-theme-loader.lisp

# Legacy method (single file)
bash bin/lint-lisp-syntax.sh src/file.lisp

# All modified files
git diff --name-only | grep '\.lisp$' | xargs -I {} bin/amoebum-lint {}
```

---

## Diagnostic-First Protocol

> **Rule**: Before retrying ANY failed command, run a diagnostic first.

See [DIAGNOSTIC_CHECKLIST.md](DIAGNOSTIC_CHECKLIST.md) for the full protocol.

### Quick Reference

| Failure Type | Diagnostic Command |
|--------------|-------------------|
| Build fails | `bin/amoebum-diagnose build` |
| Syntax error suspected | `bin/amoebum-diagnose syntax` |
| "Undefined function" error | `bin/amoebum-diagnose runtime` |
| Theme won't load | `bin/amoebum-diagnose theme` |
| Unknown issue | `bin/amoebum-diagnose all` |

---

## YAML Theme System

The project uses a YAML-based theme configuration system.

- **Theme loader**: `src/yaml-theme-loader.lisp`
- **Key function**: `load-yaml-theme`
- **Theme files**: `themes/*.yaml`

### Testing Theme Loading
```lisp
(ql:quickload :amoebum)
(amoebum.ui:load-yaml-theme "themes/default.yaml")
```

---

## Troubleshooting

### "Undefined function" errors after build

1. **Check for syntax errors**:
   ```bash
   bin/amoebum-lint src/<file>.lisp
   ```

2. **Verify compilation**:
   ```bash
   bin/amoebum-diagnose runtime
   ```

3. **Check function was actually defined**:
   ```bash
   grep -n "defun.*function-name" src/<file>.lisp
   ```

### Build succeeds but binary fails

This usually indicates a syntax error that compiled but broke functionality:
```bash
bin/amoebum-diagnose all
```

See [RETROSPECTIVE_LOAD_YAML_THEME_FIX.md](RETROSPECTIVE_LOAD_YAML_THEME_FIX.md) for detailed incident analysis.

---

## Adding New Tools

When you create a new tool:
1. Place it in `bin/`
2. Make it executable: `chmod +x bin/<tool>`
3. Document it in this file
4. Update the retrospective if it's commonly used

---

*Last updated: March 23, 2025*
