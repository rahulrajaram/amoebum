# Next Steps for Amoebum

## ANSI Escape Code Sanitization (In Progress)

### [FIXED] Completed

1. **Created sanitization functions** in `amoebum/src/util.lisp`:
   - `sanitize-ansi-escapes` - Removes CSI and OSC ANSI escape sequences using regex patterns
   - `sanitize-string-for-llm` - Converts values to strings and sanitizes them for LLM consumption

2. **Applied sanitization** in `amoebum/src/ui/chat.lisp` (3 locations):
   - Line 1276-1279: Tool results sanitized before storage in streaming context
   - Line 1285: Execution errors sanitized before display
   - Line 1971: Tool results sanitized when appending to conversation messages (with explanatory comment)

### [FIXED] Completed

**Pseudopod library sanitization implemented.** Added ANSI escape sanitization to prevent LLM API errors.

**Changes made:**
1. ✅ Created `pseudopod/src/util.lisp` with `sanitize-ansi-escapes` and `sanitize-string-for-llm` functions
2. ✅ Modified `%execute-tool-calls` in `pseudopod/src/agent/generate.lisp` to sanitize tool output before creating messages
3. ✅ Exported `sanitize-ansi-escapes` and `sanitize-string-for-llm` from `:pseudopod` package
4. ✅ Added `src/util.lisp` to `pseudopod.asd` build components (before `src/errors` to satisfy dependencies)

**Impact:** ANSI escape codes are now sanitized at the pseudopod level, providing defense-in-depth even when amoebum uses `step` or `generate` functions directly.

---

## Tokyo Night Theme Warning [FIXED]

### Problem
Compilation warning: `undefined variable: COMMON-LISP-USER::*THEME/AMOEBUM-TOKYO-NIGHT*`

### Root Cause
The `define-theme` macro in `ptui/src/core/theme.lisp` used `(intern (format nil "*THEME/~A*" name))` without specifying a package, causing the symbol to be interned in the current package at macro expansion time. When the theme was loaded via `eval` from `yaml-theme-loader.lisp`, the variable was created in the wrong package context.

### Fix
Modified `define-theme` macro to explicitly intern the theme variable in `*package*`:
```lisp
;; Before:
(var-name (intern (format nil "*THEME/~A*" name)))

;; After:
(var-name (intern (format nil "*THEME/~A*" name) *package*))
```

**File changed:** `ptui/src/core/theme.lisp` (line 305)

---

## Chat Input Responsiveness Issue [FIXED]

### Problem Description
Amoebum sometimes became unresponsive to chat input - messages typed and Enter pressed, but no LLM calls were made.

### Root Cause
The streaming state could get "stuck" in `:running` status if the worker thread crashed or failed to complete properly. The `token-stream-active-p` function would continue returning true, blocking new input from being processed.

### Solution Implemented

1. **Added stuck stream detection in `token-stream-active-p`** (`amoebum/src/ui/streaming.lisp`)
   - Streams now have a 5-minute timeout (`+stream-stuck-timeout-ms+`)
   - If a stream runs longer than the timeout without progress, it's automatically marked as `:error`

2. **Added `token-stream-force-reset-if-stuck` function** (`amoebum/src/ui/streaming.lisp`)
   - Explicitly checks for stuck streams and force-resets them to `:idle` state
   - Clears all pending events and resets worker thread reference

3. **Added `%recover-stuck-stream-if-needed!` function** (`amoebum/src/ui/chat.lisp`)
   - Called during `%sync-all-state!` before draining stream events
   - Logs recovery events for diagnostics
   - Transitions conversation back to `:idle` to unblock input

4. **Enhanced Escape key handling** (`amoebum/src/ui/panels/chat-panel.lisp`)
   - Pressing Escape during streaming now also calls `token-stream-force-reset-if-stuck`
   - Provides immediate recovery option for users

### Files Modified
- `amoebum/src/ui/streaming.lisp` - Added stuck stream detection and recovery
- `amoebum/src/ui/chat.lisp` - Added recovery check in sync cycle
- `amoebum/src/ui/panels/chat-panel.lisp` - Enhanced Escape key handling
- `amoebum/src/package.lisp` - Exported `token-stream-force-reset-if-stuck`

### Diagnostic Steps (if issue recurs)

1. **Visible overlays?** Any dialogs, pickers, or browsers showing?
2. **Status bar indicators?** Look for "STREAMING", "PLAN", or "APPROVAL" indicators  
3. **Try Escape key** - Now force-resets stuck streams in addition to canceling
4. **Try Ctrl-C** - Cancels active operations
5. **Check logs** - Look for `"stream-stuck-recovery"` events in runtime log

---

## Unicode Display Issue (Known)

**Problem:** Unicode emojis (✅ ❌) cause last character to be cut off in terminal output.

**Root Cause:** Terminal width calculations don't properly account for multi-byte Unicode characters (emojis are 2 columns wide).

**Workaround:** Use ASCII markers instead:
- `[FIXED]` instead of `✅`
- `[TODO]` instead of `❌`
- `[PASS]` instead of `✓`
- `[FAIL]` instead of `✗`

---

## Priority Order

1. ~~**High:** Complete ANSI sanitization in pseudopod (prevents LLM API errors)~~ ✅ **COMPLETED**
2. ~~**High:** Fix Tokyo Night theme warning~~ ✅ **COMPLETED**
3. ~~**Medium:** Investigate and fix chat input responsiveness~~ ✅ **COMPLETED**
4. **Low:** Fix Unicode display width calculations (cosmetic - workaround exists)

---

## Files Modified So Far

### ANSI Sanitization
- `amoebum/src/util.lisp` - Added sanitization functions
- `amoebum/src/ui/chat.lisp` - Applied sanitization to tool results
- `pseudopod/src/util.lisp` - Created pseudopod sanitization utilities (NEW)
- `pseudopod/src/agent/generate.lisp` - Applied sanitization to tool message creation
- `pseudopod/src/package.lisp` - Exported sanitization functions
- `pseudopod/pseudopod.asd` - Added util.lisp to build components

### Tokyo Night Theme Fix
- `ptui/src/core/theme.lisp` - Fixed `define-theme` macro to use explicit package for variable interning

### Chat Input Responsiveness Fix
- `amoebum/src/ui/streaming.lisp` - Added stuck stream detection (`+stream-stuck-timeout-ms+`) and `token-stream-force-reset-if-stuck` function
- `amoebum/src/ui/chat.lisp` - Added `%recover-stuck-stream-if-needed!` function called during state sync
- `amoebum/src/ui/panels/chat-panel.lisp` - Enhanced Escape key handling to force-reset stuck streams
- `amoebum/src/package.lisp` - Exported `token-stream-force-reset-if-stuck`

## Files To Modify

_None remaining for these issues._
