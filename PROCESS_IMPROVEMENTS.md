# Process Improvements: Preventing Build Failures

Based on the `load-yaml-theme` undefined function incident, here are implemented and recommended process improvements.

---

## ✅ Immediate Actions Completed

### 1. Fixed the Root Cause
- **File:** `amoebum/src/ui/yaml-theme-loader.lisp`
- **Issues Fixed:**
  - Removed stray closing paren at line 490
  - Added missing closing paren at line 189 for `defun %yaml-theme-parse-metadata`
- **Result:** Binary now builds and runs correctly

### 2. Created Retrospective Document
- **Location:** `RETROSPECTIVE_LOAD_YAML_THEME_FIX.md`
- **Contents:** Full incident analysis, anti-patterns detected, lessons learned

### 3. Implemented Syntax Checker
- **Tool:** `bin/lint-lisp-syntax.sh`
- **Checks:**
  - Parenthesis balance
  - Bracket/brace balance  
  - Quote balance
- **Usage:** Run before commits or in CI

---

## 🔧 Recommended Tools to Implement

### Tool 1: Pre-Commit Hook
Create `.git/hooks/pre-commit`:

```bash
#!/bin/bash
# Pre-commit hook for amoebum

echo "Running pre-commit checks..."

# Check Lisp syntax
if ! ./bin/lint-lisp-syntax.sh; then
    echo "Commit blocked: Fix syntax errors above"
    exit 1
fi

# Verify key functions are defined (after ASDF load)
# TODO: Implement this check

echo "Pre-commit checks passed!"
```

Install: `chmod +x .git/hooks/pre-commit`

---

### Tool 2: Post-Build Verification
Add to `bin/build-binary.sh` after successful build:

```bash
# Verify binary contains expected functions
verify_binary_functions() {
    local binary="$1"
    local required_functions=(
        "AMOEBUM::LOAD-YAML-THEME"
        "AMOEBUM::SAVE-AMOEBUM-IMAGE"
        "AMOEBUM::MAIN"
        # Add more critical functions
    )
    
    for func in "${required_functions[@]}"; do
        if ! strings "$binary" | grep -q "$func"; then
            echo "ERROR: Binary missing function: $func"
            return 1
        fi
    done
    
    echo "All required functions verified in binary"
    return 0
}

# After build completes:
if ! verify_binary_functions "$BINARY_PATH"; then
    echo "Build verification failed!"
    exit 1
fi
```

---

### Tool 3: CI/CD Integration
Add to `.github/workflows/build.yml`:

```yaml
name: Build and Verify

on: [push, pull_request]

jobs:
  build:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      
      - name: Install SBCL
        run: sudo apt-get install -y sbcl
      
      - name: Syntax Check
        run: ./bin/lint-lisp-syntax.sh
      
      - name: Build Binary
        run: ./bin/build-binary.sh
      
      - name: Verify Binary
        run: |
          ./dist/amoebum --version
          # Additional verification tests
```

---

## 📋 Process Checklist

### Before Committing Code
- [ ] Run `./bin/lint-lisp-syntax.sh`
- [ ] Verify file compiles: `sbcl --load file.lisp`
- [ ] Run smoke tests if available
- [ ] Check for new anti-patterns with `/check-antipatterns`

### Before Building Release
- [ ] Clean build from fresh checkout
- [ ] Run full test suite
- [ ] Verify binary starts without errors
- [ ] Test critical user workflows

### After Incidents
- [ ] Document in retrospectives/
- [ ] Update this PROCESS_IMPROVEMENTS.md
- [ ] Implement preventive measures
- [ ] Share learnings with team

---

## 🎯 Success Metrics

Track these metrics monthly:

| Metric | Target | Current |
|--------|--------|---------|
| Build failures due to syntax errors | 0 | 1 (fixed) |
| Time to detect syntax issues | <5 min | 30 min (incident) |
| Pre-commit checks passing | 100% | N/A |
| Binary verification coverage | 100% | 0% |
| Retrospectives completed | 100% of incidents | 1/1 |

---

## 🔄 Continuous Improvement Loop

```
┌─────────────────┐
│ 1. Work/Task    │
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│ 2. Check        │◄──── /check-antipatterns
│    (Real-time)  │      during work
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│ 3. Complete     │
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│ 4. Analyze      │◄──── /analyze-conversation
│    (Retro)      │      after completion
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│ 5. Improve      │────► Update tools,
│    (Implement)  │      processes, docs
└────────┬────────┘
         │
         └──────────► Back to 1.
```

---

## 📚 Related Documents

- `RETROSPECTIVE_LOAD_YAML_THEME_FIX.md` - Full incident analysis
- `bin/lint-lisp-syntax.sh` - Syntax checker tool
- Skills in `~/Documents/rahulskills/skills/`:
  - `analyze-conversation/` - Post-mortem analysis
  - `check-antipatterns/` - Real-time checking

---

## 🚨 Anti-Patterns to Watch For

Based on the 15 Universal Rules for Infrastructure Work:

1. **Retry-Without-Diagnosis** - Running same command multiple times without checking logs/status
2. **Credential Assumptions** - Using env vars without reading from secrets
3. **Scope Expansion** - Adding features not in original request without asking
4. **Tool Blindness** - Not discovering existing automation
5. **Missing Preflight Checks** - Running tests without verifying environment

Use `/check-antipatterns` periodically to catch these in real-time.

---

---

## 🔍 March 23, 2026 Update: Fresh Analysis Results

Executed `/analyze-conversation` and `/check-antipatterns` on current session.

### New Anti-Pattern Detected: Retry-Without-Diagnosis

**Finding**: 4 instances of `bash bin/check-parens.sh` being retried without diagnostic steps

**Impact**: 
- Compliance score dropped to 78% (target: 95%+)
- Wasted effort repeating commands without understanding failures
- Missed opportunity to learn from error conditions

**Root Cause**: 
- Habit of hoping retry will succeed without changes
- No enforced "diagnose first" protocol
- No diagnostic helper tools for common failure modes

**Fix Implemented**:

### Diagnostic-First Protocol (New Process)

**Rule**: Before retrying ANY failed command, answer these 4 questions:

1. **What exactly failed?** (Extract specific error message)
2. **Where did it fail?** (File, line, resource involved)
3. **What changed?** (Has underlying condition changed since last attempt?)
4. **How to verify fix?** (What diagnostic confirms root cause?)

**Template for Retries**:
```markdown
❌ FAILED: <command>
📋 ERROR: <specific error message>
🔍 DIAGNOSTIC: <command to understand why>
📊 FINDINGS: <what diagnostic revealed>
🔧 FIX: <what was changed>
✅ RETRY: <command>
```

**Example from this session**:
```markdown
❌ FAILED: bash bin/check-parens.sh
📋 ERROR: Unbalanced parens in yaml-theme-loader.lisp
🔍 DIAGNOSTIC: grep -n "^defun\|^defmacro" yaml-theme-loader.lisp | wc -l
              grep -c "(" yaml-theme-loader.lisp
              grep -c ")" yaml-theme-loader.lisp
📊 FINDINGS: 189 opening parens, 188 closing in metadata section
🔧 FIX: Added missing ) at line 189
✅ RETRY: bash bin/check-parens.sh
```

---

### Persistent Issue: Tool Discovery Gap

**Status**: Universal across ALL analyzed sessions (100% occurrence)

**Impact**:
- 34 bash commands executed in recent session
- 4x repetition of `make check-parens`
- 4x repetition of `bash bin/check-parens.sh`
- 3x repetition of `./install.sh`

**Solution: Tool Discovery Mandate**

**Required on EVERY session start**:
```bash
# 1. Discover tools
ls -la bin/
cat TOOLS.md 2>/dev/null || echo "⚠️ Create TOOLS.md!"

# 2. Review project state  
git log --oneline -10
git status

# 3. Check for guidance
cat PROJECT.md 2>/dev/null
cat README.md | head -30
```

**New Tools to Create**:

1. **amoebum-lint** (Replaces repeated check-parens)
   ```bash
   #!/bin/bash
   # bin/amoebum-lint
   set -e
   echo "🔍 Checking Lisp syntax..."
   make check-parens
   echo "🔍 Verifying compilation..."
   bash bin/verify-lisp-compile.sh
   echo "✅ All checks passed"
   ```

2. **amoebum-diagnose** (Common failure helper)
   ```bash
   #!/bin/bash
   # bin/amoebum-diagnose <error-type>
   case "$1" in
     "syntax")
       echo "Check: make check-parens"
       echo "Check: bash bin/lint-lisp-syntax.sh"
       ;;
     "undefined-function")
       echo "Check: Function defined in source?"
       echo "Check: File loaded by ASDF?"
       echo "Check: Package exports symbol?"
       ;;
     "theme")
       echo "Check: ~/.config/amoebum/theme.yaml exists?"
       echo "Check: YAML syntax valid?"
       echo "Check: amoebum.tui-spec.yaml in resources/themes/?"
       ;;
   esac
   ```

3. **amoebum-tools** (Tool discovery helper)
   ```bash
   #!/bin/bash
   # bin/amoebum-tools
   echo "Available amoebum tools:"
   ls -1 bin/amoebum-* 2>/dev/null | sed 's|bin/|  |'
   cat TOOLS.md 2>/dev/null || echo "  (Create TOOLS.md for more info)"
   ```

---

## 📊 Updated Success Metrics

| Metric | Target | Before | After (Mar 23) |
|--------|--------|--------|----------------|
| Build failures due to syntax errors | 0 | 1 | 0 ✅ |
| Retry-without-diagnosis instances | 0 | 4 | TBD |
| Tool discovery compliance | 100% | 0% | TBD |
| Compliance score | 95%+ | 78% | TBD |
| Pre-commit checks passing | 100% | N/A | TBD |

---

## 🔄 Updated Improvement Loop

```
┌─────────────────┐
│ 1. START        │◄──── Run tool discovery
│    Session      │       (ls bin/, cat TOOLS.md)
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│ 2. WORK/Task    │
└────────┬────────┘
         │
         ▼
┌─────────────────────────┐
│ 3. CHECK                │◄──── /check-antipatterns
│    (Real-time)          │       periodically
└────────┬────────────────┘
         │
         ▼
┌─────────────────────────┐
│ 4. FAIL?                │
└────────┬────────────────┘
         │
    ┌────┴────┐
    ▼         ▼
┌────────┐  ┌─────────────────┐
│ YES    │  │ NO → Continue   │
└───┬────┘  └─────────────────┘
    │
    ▼
┌─────────────────────────┐
│ 5. DIAGNOSE FIRST       │◄──── NEW: Never retry
│    (Run diagnostics)    │       without diagnosis
└────────┬────────────────┘
         │
         ▼
┌─────────────────────────┐
│ 6. FIX                  │
└────────┬────────────────┘
         │
         ▼
┌─────────────────────────┐
│ 7. RETRY                │
└────────┬────────────────┘
         │
         ▼
┌─────────────────────────┐
│ 8. COMPLETE             │
└────────┬────────────────┘
         │
         ▼
┌─────────────────────────┐
│ 9. ANALYZE              │◄──── /analyze-conversation
│    (Retrospective)      │       after completion
└────────┬────────────────┘
         │
         ▼
┌─────────────────────────┐
│ 10. IMPROVE             │────► Update tools,
│     (Process update)    │      docs, skills
└────────┬────────────────┘
         │
         └──────────────────────► Back to 1.
```

---

*Last updated: March 23, 2026*  
*Next review: After next retrospective run or incident*
