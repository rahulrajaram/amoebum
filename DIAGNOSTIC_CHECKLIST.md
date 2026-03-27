# Diagnostic-First Protocol

> **Rule**: Before retrying ANY failed command, you MUST run a diagnostic.

This checklist ensures we follow **Rule 2: Diagnose Before Retry** and avoid the "Retry-Without-Diagnosis" anti-pattern.

---

## Quick Reference

```bash
# Command failed? STOP. Run this first:
amoebum-diagnose all

# Or for specific issues:
amoebum-diagnose build     # Build failures
amoebum-diagnose syntax    # Syntax errors  
amoebum-diagnose runtime   # Runtime errors
amoebum-diagnose theme     # Theme loading issues
```

---

## The Diagnostic-First Flow

```
┌─────────────────┐
│ Command fails   │
└────────┬────────┘
         ▼
┌─────────────────────────┐
│ STOP - Do not retry yet │
└────────┬────────────────┘
         ▼
┌─────────────────────────────┐
│ Ask: What diagnostic would  │
│ tell me WHY it failed?      │
└────────┬────────────────────┘
         ▼
┌─────────────────────────────┐
│ Run diagnostic command      │
│ (check logs, files, state)  │
└────────┬────────────────────┘
         ▼
┌─────────────────────────────┐
│ Apply fix based on findings │
└────────┬────────────────────┘
         ▼
┌─────────────────────────────┐
│ Re-run original command     │
└─────────────────────────────┘
```

---

## Pre-Retry Checklist

Before retrying any failed command, answer these questions:

- [ ] **What exactly failed?** (copy the specific error message)
- [ ] **What file/line/resource is involved?** (identify the location)
- [ ] **Has anything changed since last attempt?** (if no, retry won't help)
- [ ] **What diagnostic would confirm root cause?** (choose appropriate check)
- [ ] **What would I check to confirm the fix worked?** (define success criteria)

---

## Common Failure Patterns & Diagnostics

### Pattern 1: `make` or `bash bin/check-parens.sh` fails

**Don't**: Run it again expecting different results

**Do**:
```bash
# Diagnostic
amoebum-lint -a              # Check all files
# OR
bash bin/lint-lisp-syntax.sh <specific-file>

# Fix
# Edit file to fix unbalanced parentheses

# Verify
amoebum-lint <fixed-file>    # Confirm fix before re-running make
```

---

### Pattern 2: Binary fails with "Undefined function"

**Don't**: Rebuild immediately

**Do**:
```bash
# Diagnostic
amoebum-diagnose runtime     # Check for undefined functions
amoebum-diagnose syntax      # Verify file compiled correctly

# Common causes:
# 1. Syntax error prevented function from being defined
# 2. File wasn't loaded during build
# 3. Function name mismatch (typo)

# Fix
# Check src/<file>.lisp for syntax errors
# Verify (defun function-name ...) exists

# Verify
bash bin/verify-lisp-compile.sh <file>
```

---

### Pattern 3: Theme loading fails

**Don't**: Try loading different themes

**Do**:
```bash
# Diagnostic
amoebum-diagnose theme

# Check specific issues:
grep -n "defun.*load-yaml-theme" src/yaml-theme-loader.lisp
ls themes/*.yaml              # Verify theme files exist

# Fix
# Address identified issue

# Verify
sbcl --eval "(ql:quickload :amoebum)" --eval "(amoebum.ui:load-yaml-theme \"themes/default.yaml\")"
```

---

### Pattern 4: Build succeeds but binary doesn't work

**Don't**: Assume the build is fine

**Do**:
```bash
# Diagnostic
ls -la amoebum                # Check binary exists and is executable
file amoebum                  # Verify file type
./amoebum --help 2>&1 | head  # Try running with minimal args

# Check build log
cat build.log | grep -i "error\|warning"

# Fix based on findings

# Verify
./amoebum  # Test actual functionality
```

---

## Diagnostic Template

When documenting a failure and fix, use this format:

```markdown
**Command failed**: `<command>`
**Error**: `<specific error message>`
**Diagnostic**: `<what you checked>`
**Root cause**: `<what you found>`
**Fix applied**: `<what you changed>`
**Verification**: `<command that confirmed fix>`
```

---

## Questions to Ask Before Retrying

1. **What exactly failed?**
   - Copy the error message verbatim
   - Note the file and line number if provided

2. **What file/line/resource is involved?**
   - Identify which component failed
   - Check if it's a file you recently modified

3. **Has anything changed since last attempt?**
   - If nothing changed, the same command will produce the same result
   - You must change something (fix, config, environment) before retrying

4. **What diagnostic would confirm root cause?**
   - Choose from: `amoebum-diagnose build|syntax|runtime|theme`
   - Or use: `amoebum-lint <file>` for syntax issues

5. **What would I check to confirm the fix worked?**
   - Define your success criteria before applying the fix
   - This prevents "fixing" without verifying

---

## Tool Reference

| Tool | Purpose | When to Use |
|------|---------|-------------|
| `amoebum-lint` | Syntax + compilation check | Before any build/retry |
| `amoebum-diagnose build` | Build failure analysis | After failed make/build |
| `amoebum-diagnose syntax` | Detailed syntax check | Syntax errors suspected |
| `amoebum-diagnose runtime` | Runtime error analysis | Binary fails to run |
| `amoebum-diagnose theme` | Theme system check | YAML theme issues |
| `amoebum-diagnose all` | Complete diagnostic | Unknown failure cause |

---

## Compliance Tracking

**Target**: 100% diagnostic-first compliance

**Current Issues**:
- Retry-Without-Diagnosis: 4 instances detected (target: 0)

**Prevention**:
- Run `amoebum-diagnose` before any retry
- Document all failures using the diagnostic template
- Review this checklist at the start of each session

---

*Last updated: March 23, 2025*
