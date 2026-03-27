# Memory Thrashing Fix Plan

## Problem Summary

**Symptom:** Inputs slow down massively over the course of a conversation.  
**Root Cause:** GC thrashing from extreme allocation rate (~200K minor faults per 5 seconds).  
**Key Insight:** RSS stays bounded (~200-230MB) - this is NOT a memory leak. SBCL's GC keeps memory bounded but burns CPU collecting short-lived objects.

**The Hot Path:** Every frame during streaming content:
1. Creates fresh buffers with cloned cells
2. Re-parses entire markdown history
3. Re-wraps every grapheme into styled segments
4. `copy-tree` duplicates entire styled line structures
5. Diff generates fresh draw-op structs

---

## Priority Levels

- **P0 (Critical):** Must fix first - addresses the bulk of allocation
- **P1 (High):** Significant impact - follow P0 fixes
- **P2 (Medium):** Nice-to-have optimizations
- **P3 (Low):** Long-term architectural improvements

---

## P0 (Critical) - Implement First

### 1. Buffer Reuse in Render Loop
**File:** `ptui/src/render/buffer.lisp`, `ptui/src/engine/loop.lisp`  
**Current:** `make-buffer` allocates fresh array + clones N cells every frame  
**Fix:** Add `buffer-clear` reuse path in loop

```lisp
;; In loop.lisp - modify %render-runtime-frame
(let* ((frame-start (ptui.util.time:monotonic-ms))
       (backend (loop-runtime-backend runtime))
       (size (ptui.backend.protocol:backend-size backend))
       ;; REUSE: Clear and reuse prev-buffer instead of fresh allocation
       (next-buffer (or (loop-runtime-prev-buffer runtime)
                        (ptui.render.buffer:make-buffer 
                         (ptui.core.types:size-cols size)
                         (ptui.core.types:size-rows size)))))
  (when (loop-runtime-prev-buffer runtime)
    (ptui.render.buffer:buffer-clear next-buffer))
  ;; ... render into next-buffer ...
)
```

**Expected Impact:** Eliminates ~width×height cell allocations per frame (e.g., 80×24 = 1,920 cells/frame)

---

### 2. Eliminate copy-tree in streaming-markdown-renderer-render-lines
**File:** `amoebum/src/ui/streaming.lisp` (line ~1309)  
**Current:** `(mapcar #'copy-tree (streaming-markdown-renderer-wrapped-lines renderer))`  
**Fix:** Structural sharing with copy-on-write or freeze renderer state

```lisp
;; Option A: Remove copy-tree if consumers are read-only
(let ((styled-lines (streaming-markdown-renderer-wrapped-lines renderer)))
  ;; Trust that downstream won't mutate
  ...)

;; Option B: Add dirty flag, only copy if modified since last render
(defstruct (streaming-markdown-renderer ...)
  ...
  (wrapped-lines-dirty-p t :type boolean))

(defun streaming-markdown-renderer-render-lines (renderer ...)
  (let ((styled-lines (if (streaming-markdown-renderer-wrapped-lines-dirty-p renderer)
                          (progn
                            (setf (streaming-markdown-renderer-wrapped-lines-dirty-p renderer) nil)
                            (mapcar #'copy-tree (streaming-markdown-renderer-wrapped-lines renderer)))
                          (streaming-markdown-renderer-cached-rendered-lines renderer))))
    ...))
```

**Expected Impact:** Eliminates deep copy of entire styled line tree every frame. For 100 lines × 50 segments, this is ~5,000 cons cells/frame.

---

### 3. Stop Re-wrapping All Lines on Every Chunk
**File:** `amoebum/src/ui/streaming.lisp` (lines ~1288-1293, ~1264-1297)  
**Current:** Every `append-chunk` re-wraps ALL accumulated logical lines  
**Fix:** Cache wrapped lines, only wrap new content

```lisp
(defun streaming-markdown-renderer-append-chunk (renderer chunk)
  ;; Track which logical lines are already wrapped
  (let ((prev-line-count (length (streaming-markdown-renderer-logical-lines renderer)))
        (new-logical-lines '()))
    ;; ... process chunk into new-logical-lines ...
    (setf (streaming-markdown-renderer-logical-lines renderer)
          (append (streaming-markdown-renderer-logical-lines renderer)
                  new-logical-lines))
    ;; ONLY wrap the NEW lines, not all lines
    (let ((new-wrapped '()))
      (dolist (segments new-logical-lines)
        (setf new-wrapped 
              (append new-wrapped
                      (%stream-markdown-wrap-segments segments safe-width ...))))
      (setf (streaming-markdown-renderer-wrapped-lines renderer)
            (append (streaming-markdown-renderer-wrapped-lines renderer)
                    new-wrapped))
      (setf (streaming-markdown-renderer-wrapped-lines-dirty-p renderer) t))))
```

**Expected Impact:** O(new content) instead of O(total content) per frame. Critical for long conversations.

---

## P1 (High) - Implement After P0

### 4. Cell Object Pooling
**File:** `ptui/src/render/buffer.lisp`  
**Current:** `clone-cell` allocates fresh cell every time  
**Fix:** Pre-allocate cell pools or use mutable cells with reset

```lisp
;; Option: Mutable cells that are reset rather than cloned
(defun write-cell-raw (buf x y cell clip)
  "Write cell without cloning - caller manages ownership"
  (when (point-in-rect-p x y clip)
    (setf (svref (ptui.core.types:cell-buffer-cells buf) (buffer-index buf x y))
          cell)))

;; In hot paths, reuse cell objects
(let ((temp-cell (ptui.core.types:make-cell " " nil nil (ptui.core.types:make-attrs))))
  (dolist (segment segments)
    (setf (ptui.core.types:cell-glyph temp-cell) (getf segment :text)
          (ptui.core.types:cell-fg temp-cell) (getf segment :fg)
          ...)
    (write-cell-raw buf x y temp-cell clip)))
```

**Expected Impact:** Eliminates per-cell allocation during text rendering.

---

### 5. String Builder for Chunk Concatenation
**File:** `amoebum/src/ui/streaming.lisp` (line ~1268)  
**Current:** `(concatenate 'string pending-line chunk-text)`  
**Fix:** Use adjustable array with fill-pointer as string builder

```lisp
(defstruct (string-builder 
            (:constructor make-string-builder (&optional (initial-size 256)))
  (buffer (make-array initial-size :element-type 'character 
                      :adjustable t :fill-pointer 0))
  (length 0 :type fixnum))

(defun string-builder-append (sb string)
  (let* ((buf (string-builder-buffer sb))
         (len (length string))
         (new-len (+ (string-builder-length sb) len)))
    (when (> new-len (array-dimension buf 0))
      (adjust-array buf (max (* (array-dimension buf 0) 2) new-len)))
    (replace buf string :start1 (string-builder-length sb))
    (setf (fill-pointer buf) new-len)
    (setf (string-builder-length sb) new-len)
    sb))

(defun string-builder-get (sb)
  (subseq (string-builder-buffer sb) 0 (string-builder-length sb)))
```

**Expected Impact:** Reduces string allocation during streaming chunk accumulation.

---

### 6. Avoid nconc in styled-segment->cells
**File:** `ptui/src/render/buffer.lisp` (line ~111)  
**Current:** `(setf cells (nconc cells (list cell)))` in loop  
**Fix:** Use `cons` + `nreverse` or vector builder

```lisp
(defun styled-segment->cells (segment)
  ...
  (let ((cells '()))
    (dolist (cluster (ptui.text.grapheme:split-graphemes text))
      ...
      (push cell cells)  ; Use push (cons)
      (loop repeat (1- width) do
        (push continuation-cell cells)))
    (nreverse cells)))  ; Reverse once at end
```

**Expected Impact:** `nconc` is O(n) per call; `cons` + `nreverse` is O(1) per iteration.

---

## P2 (Medium) - Nice-to-Have Optimizations

### 7. SBCL Nursery Size Tuning (Short-term Mitigation)
**File:** Runtime configuration  
**Change:** Increase generation 0 (nursery) size to reduce GC frequency

```bash
# Set environment variable before starting amoebum
export SBCL_DYNAMIC_SPACE_SIZE=4Gb  # If not already set

# Or in Lisp code at startup:
#+sbcl (setf (sb-ext:bytes-consed-between-gcs) (* 256 1024 1024))  ; 256MB
```

**Note:** This masks the problem but doesn't fix it. Use only as temporary relief.

---

### 8. Batch draw-op Allocation
**File:** `ptui/src/render/diff.lisp`  
**Current:** `(push (make-draw-op ...) ops)` per diff operation  
**Fix:** Pre-allocate vector, fill densely

```lisp
(defun diff-buffers (prev next &key (full-redraw nil))
  ...
  (let ((ops (make-array 128 :fill-pointer 0 :adjustable t))
        (op-count 0))
    ...
    (vector-push-extend (make-draw-op ...) ops)
    (incf op-count)
    ...
    (values ops op-count)))  ; Return vector instead of list
```

**Expected Impact:** Reduces consing for draw operations; better cache locality.

---

### 9. Lazy Grapheme Splitting
**File:** `ptui/src/text/grapheme.lisp`, `ptui/src/render/buffer.lisp`  
**Current:** All text is split into graphemes immediately  
**Fix:** Process ASCII strings as chunks, only split multi-byte

```lisp
(defun fast-string-width (string)
  "Avoid full grapheme split for ASCII strings"
  (if (every (lambda (c) (< (char-code c) 128)) string)
      (length string)  ; ASCII = 1 byte per char
      (reduce #'+ (ptui.text.grapheme:split-graphemes string) 
              :key #'ptui.text.width:grapheme-width)))
```

**Expected Impact:** Most code is ASCII; skip expensive grapheme processing.

---

## P3 (Low) - Long-term Architectural

### 10. Immutable Render Trees with Structural Sharing
**File:** New module  
**Concept:** Use persistent data structures (like Clojure's vectors) for styled lines  
**Benefit:** `copy-tree` becomes cheap pointer manipulation

### 11. GPU-style Dirty Rectangle Tracking
**File:** `ptui/src/render/diff.lisp`  
**Concept:** Track which regions changed, only diff those regions  
**Benefit:** O(changed cells) instead of O(total cells)

### 12. Compile Markdown Stylesheets
**File:** `amoebum/src/ui/streaming.lisp`  
**Concept:** Pre-compile markdown parsing state machine  
**Benefit:** Faster parsing, less intermediate allocation

---

## Implementation Order Recommendation

```
Week 1 (P0 - Critical):
  1. Buffer reuse in loop.lisp
  2. Eliminate copy-tree in streaming.lisp
  3. Incremental wrapping (only wrap new lines)

Week 2 (P1 - High):
  4. Cell pooling in buffer.lisp
  5. String builder for chunks
  6. Fix nconc in styled-segment->cells

Week 3 (P2 - Medium):
  7. SBCL nursery tuning (if still needed)
  8. Batch draw-op allocation
  9. Lazy grapheme splitting

Ongoing (P3 - Low):
  10-12. Architectural improvements as needed
```

---

## Verification Checklist

- [ ] Profile with `sb-sprof` before/after each change
- [ ] Monitor `/proc/<pid>/status` vmrss and min_flt counters
- [ ] Test with 10,000 token streaming response
- [ ] Ensure no functional regressions in rendering
- [ ] Measure frame time consistency (should not spike)

---

## Quick Win (Emergency Patch)

If immediate relief is needed before full implementation:

```lisp
;; Add to amoebum startup
#+sbcl
(when (find-package "SB-EXT")
  ;; Increase nursery to 256MB (default is often 5-10MB)
  (setf (sb-ext:bytes-consed-between-gcs) 
        (* 256 1024 1024))
  ;; Or use a multiplier of default
  (setf (sb-ext:bytes-consed-between-gcs)
        (* 8 (sb-ext:bytes-consed-between-gcs))))
```

This will reduce GC frequency at the cost of slightly longer GC pauses.  
**Warning:** This is a band-aid, not a cure. The allocation rate is still pathological.

---

## Metrics to Track

| Metric | Current (Bad) | Target (Good) |
|--------|--------------|---------------|
| Minor faults / 5 sec | ~200,000 | < 10,000 |
| Frame time (p99) | Spikes to 100ms+ | < 20ms consistent |
| GC time % (SBCL) | > 50% | < 10% |
| RSS growth rate | Stable | Stable |
| Tokens/sec during stream | Degrades over time | Constant |

---

## Implementation Status

### Completed (2024-01-XX)

#### P0 Fixes Implemented:
1. **Buffer Reuse in Render Loop** (`ptui/src/engine/loop.lisp`)
   - Modified `%render-runtime-frame` to reuse `prev-buffer` instead of allocating fresh buffer each frame
   - Buffer is cleared via `buffer-clear` before each render
   - **BREAKING CHANGE**: Render function signature changed from `(state size) -> buffer` to `(state size buffer) -> buffer`

2. **Dirty Flag for Wrapped Lines** (`amoebum/src/ui/streaming.lisp`)
   - Added `wrapped-lines-dirty-p` and `cached-rendered-lines` fields to `streaming-markdown-renderer` struct
   - Modified `streaming-markdown-renderer-render-lines` to only `copy-tree` when dirty
   - Modified `streaming-markdown-renderer-reset` and `%stream-markdown-renderer-rewrap!` to manage dirty flag
   - Modified `streaming-markdown-renderer-append-chunk` to set dirty flag when new lines added

#### P1 Fixes Implemented:
6. **Fixed nconc in styled-segment->cells** (`ptui/src/render/buffer.lisp`)
   - Changed from `(setf cells (nconc cells (list cell)))` to `(push cell cells)` with `nreverse` at end
   - Eliminates O(n²) behavior - now O(n) total for the loop

5. **String Builder for Chunk Concatenation** (`amoebum/src/ui/streaming.lisp`)
   - Added `string-builder` struct with `make-string-builder`, `string-builder-append`, `string-builder-get`
   - Modified `streaming-markdown-renderer-append-chunk` to use string builder for pending line + chunk concatenation
   - Avoids repeated string allocation during streaming

### Known Issues & Follow-up Required

#### Issue 1: Render Function Signature Change
**Problem**: The render function signature change from `(state size)` to `(state size buffer)` is a breaking change.

**Files that may need updating**:
- Any custom render functions passed to `ptui.engine.loop:run`
- The `%make-app-render-fn` in `ptui/src/ui/app.lisp` has been updated
- The `%default-render-fn` in `ptui/src/engine/loop.lisp` has been updated

**Action Required**: Search for any other render function definitions in the codebase:
```bash
grep -r "lambda.*state.*size" --include="*.lisp" amoebum/
grep -r "defun.*render" --include="*.lisp" amoebum/
```

#### Issue 2: Compilation Testing
**Problem**: The changes need to be compiled and tested to ensure they work correctly.

**Action Required**:
```bash
cd /home/rahul/Documents/amoebum
# Load the updated files
sbcl --eval "(load \"ptui/src/engine/loop.lisp\")" \
     --eval "(load \"ptui/src/ui/app.lisp\")" \
     --eval "(load \"ptui/src/render/buffer.lisp\")" \
     --eval "(load \"amoebum/src/ui/streaming.lisp\")" \
     --eval "(quit)"
```

#### Issue 3: Remaining P1/P2 Fixes
**Not yet implemented**:
- P1 #4: Cell object pooling in `buffer.lisp` (mutable cells instead of `clone-cell`)
- P1 #5: (partially done - string builder added but not integrated into renderer struct)
- P2 #7: SBCL nursery size tuning (emergency patch)
- P2 #8: Batch draw-op allocation in `diff.lisp`
- P2 #9: Lazy grapheme splitting
- P3 #10-12: Architectural improvements

### Testing Recommendations

1. **Unit test the string builder**:
```lisp
(let ((sb (make-string-builder)))
  (string-builder-append sb "Hello ")
  (string-builder-append sb "World")
  (assert (string= (string-builder-get sb) "Hello World")))
```

2. **Profile before/after**:
```lisp
;; Before running amoebum
#+sbcl (setf (sb-ext:bytes-consed-between-gcs) (* 256 1024 1024))

;; Monitor GC activity
#+sbcl (sb-sprof:start-profiling :mode :cpu)
;; ... run streaming test ...
#+sbcl (sb-sprof:stop-profiling)
#+sbcl (sb-sprof:report :type :flat)
```

3. **Stress test**:
```lisp
(defun stress-test-streaming (n-tokens)
  (let ((renderer (make-streaming-markdown-renderer)))
    (dotimes (i n-tokens)
      (streaming-markdown-renderer-append-chunk 
        renderer (format nil "token ~a " i)))
    (streaming-markdown-renderer-render-lines renderer 80)))
```

### Emergency Workaround (If Issues Arise)

If the buffer reuse causes any issues, you can quickly revert to fresh allocation by modifying `loop.lisp`:

```lisp
;; In %render-runtime-frame, replace buffer reuse with:
(let* ((...)
       (next-buffer (funcall (loop-runtime-render-fn runtime)
                             (loop-runtime-state runtime)
                             size
                             nil)))  ; Pass nil to force fresh allocation
  ;; Remove the buffer-clear call
  ...)
```

Then update render functions to check for nil buffer:
```lisp
(lambda (state size buffer)
  (let ((buf (or buffer (ptui.render.buffer:make-buffer ...))))
    ...))
```

---

*Generated based on analysis of allocation patterns in buffer.lisp, diff.lisp, and streaming.lisp*
*Root cause: Pathological allocation rate in hot render loop during streaming content*
*Implementation notes: Breaking change to render function signature - see Issue 1 above*
