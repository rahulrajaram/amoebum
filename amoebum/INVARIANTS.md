# Amoebum Invariants
Last verified: 2026-04-08

Authoritative, grep-friendly reference for invariants that agents MUST preserve
when editing the amoebum codebase. Each entry catches a "forgot one of N coupled
mutations" regression. Entries are numbered by category; IDs are stable.

Format:
- **Statement** — what must always hold.
- **Why it matters** — the regression it prevents.
- **Citations** — `file:line` pointers into the tree.
- **Symptom** — what breaks if you violate it.

Many entries are lifted from `.amoebum/MEMORY.md` and
`amoebum/src/ui/CLAUDE.md`; those are attributed inline.

---

## Test Isolation

### INV-001: restore-session tests must save/restore global toolset state
- **Statement**: Any test calling `restore-session` MUST save and restore
  `*toolset*`, `*tool-metadata*`, `*tool-history*`, and `*memory-backend*`
  around the call.
- **Why it matters**: `restore-session` replaces these globals wholesale
  (`amoebum/src/checkpoint.lisp:312-314`, `amoebum/src/checkpoint.lisp:987`);
  downstream tests observing the mutated bindings cross-contaminate.
- **Citations**: `amoebum/src/checkpoint.lisp:9-11`,
  `amoebum/src/checkpoint.lisp:312-314`, `amoebum/src/checkpoint.lisp:987`.
- **Symptom**: Bleed-over test failures where unrelated suites fail after a
  checkpoint test runs first. (Lifted from MEMORY.md "Pre-existing Test
  Failures" and "Test Isolation Pattern".)

### INV-002: tests touching *toolset* use unwind-protect
- **Statement**: Any test that does `(setf *toolset* ...)` MUST wrap the body
  in `unwind-protect` and restore the original value in the cleanup form.
- **Why it matters**: Smoke tests run back-to-back in a single image; a leaked
  toolset wedges every later suite that resolves tools by name.
- **Citations**: `amoebum/src/checkpoint.lisp:9`, pattern enforced in
  `amoebum/test/` suites.
- **Symptom**: Subsequent tool-dispatch tests see an unexpected toolset and
  either fail to find tools or resolve the wrong implementation. (Lifted from
  MEMORY.md "Test Isolation Pattern".)

### INV-003: tests also save/restore event bus and checkpoint override
- **Statement**: Tests that call `restore-session` or touch event wiring MUST
  also save/restore `*event-bus*` and `*checkpoint-directory-override*`.
- **Why it matters**: Notification sinks subscribe through `*event-bus*`;
  `*checkpoint-directory-override*` is a defparameter used by every checkpoint
  path lookup.
- **Citations**: `amoebum/src/checkpoint.lisp:3`,
  `amoebum/src/checkpoint.lisp:37-38`,
  events subsystem documented in `amoebum/src/CLAUDE.md` coupling list.
- **Symptom**: Tests write checkpoints under the previous test's override dir,
  or deliver notifications to a stale bus. (Lifted from MEMORY.md "Test
  Isolation Pattern".)

### INV-004: FiveAM `(is ...)` forms must contain a list
- **Statement**: `(is called)` is an error — FiveAM's `is` macro requires a
  list form. Use `(is (not (null called)))` or `(is (equal expected actual))`.
- **Why it matters**: The non-list form silently passes or raises a compile
  warning that never fails CI.
- **Citations**: pattern documented in MEMORY.md "Common Lisp / SBCL Gotchas".
- **Symptom**: Tests report success while asserting nothing.

---

## State Mutation Coupling (ui/chat.lisp)

### INV-010: clearing input-text must also clear prompt-scroll-offset
- **Statement**: Every site that sets `chat-ui-state-input-text` to `""` in
  the same form also sets `chat-ui-state-prompt-scroll-offset` to `nil`.
- **Why it matters**: A non-nil scroll offset against an empty buffer leaves
  the prompt input visually scrolled past its own content.
- **Citations**: `amoebum/src/ui/chat.lisp:757-758`,
  `amoebum/src/ui/chat.lisp:4147-4148`,
  `amoebum/src/ui/chat.lisp:4240-4241`,
  `amoebum/src/ui/chat.lisp:4326`.
- **Symptom**: After submit / slash-command / plan-mode entry, the prompt
  appears blank-but-scrolled; typing reveals characters at unexpected columns.

### INV-011: history-search triple moves atomically
- **Statement**: `history-search-active-p`, `history-search-original-input`,
  and `history-search-signature` are a three-slot group. Any site that
  activates, deactivates, or reconfigures history search mutates all three in
  the same setf form.
- **Why it matters**: A stale signature against an active picker triggers
  rebuild loops; a missing original-input loses the user's in-flight draft on
  cancel.
- **Citations**: activate at `amoebum/src/ui/chat.lisp:235-239`,
  deactivate at `amoebum/src/ui/chat.lisp:252-254`,
  reset at `amoebum/src/ui/chat.lisp:599-603`.
- **Symptom**: Entering history search loses the typed prompt; escaping it
  restores the wrong text; the picker redraws on every keystroke.
  (Reinforced by `amoebum/src/ui/CLAUDE.md` "Keyboard navigation edge cases".)

### INV-012: stream reset trio — chunks, follow, markdown renderer
- **Statement**: Any site that begins or restarts a streaming assistant
  response clears `stream-response-chunks`, sets `stream-scroll-follow-p` to
  `t`, and calls `streaming-markdown-renderer-reset` on
  `stream-markdown-renderer`. All three, same code path.
- **Why it matters**: Leftover chunks render above the new turn; a stuck
  follow-p hangs the viewport; an un-reset renderer emits the previous turn's
  tail into the new turn's head.
- **Citations**: start at `amoebum/src/ui/chat.lisp:2696-2700`,
  continuation at `amoebum/src/ui/chat.lisp:2421-2424`,
  finalize at `amoebum/src/ui/chat.lisp:2160-2161`.
- **Symptom**: Second and subsequent prompts render corrupt markdown
  (leftover code-fence state), scroll frozen halfway, or duplicate text.

### INV-013: stream tool tracking quintet cleared together
- **Statement**: `%clear-stream-tool-tracking!` clears `stream-tool-calls`,
  `stream-executed-tool-call-keys`, `stream-event-journal`,
  `stream-turn-snapshot`, and `stream-completion-pending-p` as a unit. Every
  new-turn code path calls the helper; do NOT inline partial resets.
- **Why it matters**: Leftover executed keys silently skip tool calls in the
  next turn; a stale journal replays events; a stale snapshot corrupts the
  turn's token accounting.
- **Citations**: helper body at `amoebum/src/ui/chat.lisp:1290-1304`,
  callers at `amoebum/src/ui/chat.lisp:2704`,
  `amoebum/src/ui/chat.lisp:2428`.
- **Symptom**: Tool calls in turn N+1 are silently skipped because their keys
  collided with turn N's executed set.

### INV-014: scroll-up delta disables scroll-follow
- **Statement**: In `chat-ui-scroll-history`, whenever `delta-lines > 0` and
  the resulting scrollback is positive, `stream-scroll-follow-p` is forced to
  `nil`. This fires regardless of streaming state.
- **Why it matters**: Without this, a user who scrolls up during an idle
  period gets yanked back to the tail on the next message append.
- **Citations**: `amoebum/src/ui/chat.lisp:802-810`,
  explanatory comment at `amoebum/src/ui/chat.lisp:797-801`.
- **Symptom**: User cannot read history while messages stream in — the view
  fights them to the tail.

### INV-015: agentic-iteration-count resets on user submit
- **Statement**: `chat-ui-submit-input` resets
  `chat-ui-state-agentic-iteration-count` to `0` before processing the input,
  regardless of whether the input is blank or parseable.
- **Why it matters**: Without this, an agent that exhausted its iteration cap
  in a previous turn cannot start fresh; every new user prompt immediately
  trips the cap.
- **Citations**: `amoebum/src/ui/chat.lisp:746` (before validation at 747).
- **Symptom**: After a long tool loop, the UI refuses to run tools on the
  next prompt with a "max iterations reached" error.

### INV-016: ctrl-c double-tap window uses monotonic ms
- **Statement**: `ctrl-c-quit-armed-at-ms` is set from a monotonic clock and
  compared against `+chat-exit-confirm-window-ms+` (1500 ms). On any other
  key or on successful exit, the slot MUST be cleared to `nil` (never left
  stale).
- **Why it matters**: A stale armed timestamp either double-exits the app on
  a legitimate single Ctrl-C or silently rearms the exit path.
- **Citations**: clear at `amoebum/src/ui/chat.lisp:824`,
  arm at `amoebum/src/ui/chat.lisp:837`,
  window constant referenced in `amoebum/src/ui/CLAUDE.md`.
- **Symptom**: Ctrl-C either exits immediately without confirmation or
  requires three taps.

### INV-017: per-message entry cache invalidates on content mutation
- **Statement**: The `%message-line-entries` per-message cache in `chat.lisp`
  must be invalidated for any message whose content is mutated in place.
  Only the currently-streaming target is allowed to regenerate each frame;
  every other message reuses the cache.
- **Why it matters**: A regression that bypasses the cache restores the
  O(total-conversation-lines) alloc-per-frame pathology (6.10x minor-fault
  growth observed, fixed 2026-03-20).
- **Citations**: see `amoebum/src/ui/chat.lisp` `%message-line-entries`,
  rule detailed in `amoebum/src/ui/CLAUDE.md` "Streaming & render
  performance — DO NOT regress".
- **Symptom**: `bin/tui-perf-test.sh` regresses from ~1.1x to ~6x minor-fault
  growth; TUI feels sluggish after long conversations. (Lifted from MEMORY.md
  "GC Thrashing / TUI Slowdown Fix".)

---

## Conversation Lifecycle

### INV-020: conversation state changes go through conversation-transition!
- **Statement**: `conversation-state-state` is only mutated via
  `conversation-transition!`, which validates the target against
  `+conversation-state-transitions+`. Do NOT `setf` the slot directly from
  call sites.
- **Why it matters**: The transition table is the only place illegal moves
  (e.g. `:idle -> :streaming` without a prior `:user-input`) are caught; a
  raw setf silently bypasses it.
- **Citations**: table at `amoebum/src/conversation.lisp:11-17`,
  guard at `amoebum/src/conversation.lisp:536-551`,
  condition at `amoebum/src/conversation.lisp:19-32`.
- **Symptom**: Streaming starts from an unexpected prior state and the
  invariant that `:streaming` is preceded by a user message silently fails.

### INV-021: conversation-transition! updates updated-at with current time
- **Statement**: Every `conversation-transition!` that succeeds sets
  `conversation-state-updated-at` to `(%conversation-now)` in the same setf
  form as the state change.
- **Why it matters**: Downstream checkpoint/indexer machinery sorts by
  `updated-at`; a stale timestamp causes checkpoints to appear out of order.
- **Citations**: `amoebum/src/conversation.lisp:547-548`.
- **Symptom**: Session manifests list the newest entry as older than its
  parent; resume UI shows sessions out of order.

### INV-022: conversation-reset! clears four slots as a unit
- **Statement**: `conversation-reset!` must clear `entries`, set `state` to
  `:idle`, update `updated-at`, reset `active-fork`, clear
  `fork-branch-point`, and normalize `forks` — all in one setf.
- **Why it matters**: Half-reset state leaves a fork pointing into a
  now-empty entries list; subsequent resume produces an "entry out of bounds"
  error.
- **Citations**: `amoebum/src/conversation.lisp:575-581`.
- **Symptom**: Forked conversations crash on resume with bounds errors.

### INV-023: conversation-state-add-message entry path is check-type guarded
- **Statement**: All public conversation mutators begin with
  `(check-type conversation conversation-state)`. Do not remove these guards
  when refactoring.
- **Why it matters**: The struct is passed across 20+ call sites; a bad
  argument slips silently through `setf` without the guard and corrupts a
  downstream struct that happens to share a slot name.
- **Citations**: `amoebum/src/conversation.lisp:187`,
  `amoebum/src/conversation.lisp:219`,
  `amoebum/src/conversation.lisp:510`,
  `amoebum/src/conversation.lisp:538`,
  `amoebum/src/conversation.lisp:555`,
  `amoebum/src/conversation.lisp:572`,
  `amoebum/src/conversation.lisp:587`,
  `amoebum/src/conversation.lisp:606`.
- **Symptom**: Silent mis-typed arguments land in unrelated structs; errors
  surface far from the root cause.

---

## Checkpoint Round-Trip

### INV-030: vector decode uses `(second value)` not `(rest value)`
- **Statement**: `%checkpoint-decode-value` decodes `:__vector__` by reading
  `(second value)` (i.e. the inner list), then `coerce`ing the
  `mapcar`-decoded result to `'vector`. Do not revert to `(rest value)` —
  that wraps the list in an extra cons and produces a vector-of-one-list.
- **Why it matters**: Fixed in commit 4d4bfc4; the regression silently
  corrupts every checkpointed vector on restore.
- **Citations**: `amoebum/src/checkpoint.lisp:160-162`.
- **Symptom**: Restored sessions have single-element vectors-of-lists where
  the original had multi-element flat vectors; downstream consumers crash or
  misrender. (Lifted from MEMORY.md "Common Lisp / SBCL Gotchas".)

### INV-031: encode/decode sentinel tags are symmetric and exhaustive
- **Statement**: `%checkpoint-encode-value` and `%checkpoint-decode-value`
  together must handle `:__hash-table__`, `:__pathname__`, `:__vector__`,
  plus fall-through `cons` and atoms. Adding a new sentinel on either side
  requires the matching branch on the other side.
- **Why it matters**: An encoder-only sentinel silently leaks the tagged
  list into runtime; a decoder-only sentinel never fires.
- **Citations**: `amoebum/src/checkpoint.lisp:125-143` (encode),
  `amoebum/src/checkpoint.lisp:145-166` (decode).
- **Symptom**: Round-trip test `(equal v (decode (encode v)))` fails for
  the affected type.

### INV-032: checkpoint payload getf keys match the serializer
- **Statement**: Every snapshot decoder reads slot values via
  `(%checkpoint-decode-value (getf snapshot :slot-key))`. The key list on
  decode must exactly match the key list produced by the sibling encoder
  function.
- **Why it matters**: A rename on the encoder side silently produces `nil`
  for the old key on decode — the restored object has a missing field but
  no error.
- **Citations**: encode/decode pairs at
  `amoebum/src/checkpoint.lisp:194-208` (tool params),
  `amoebum/src/checkpoint.lisp:218-235` (tool source),
  `amoebum/src/checkpoint.lisp:246-259`,
  `amoebum/src/checkpoint.lisp:321-329` (memory entry).
- **Symptom**: Restored tool/memory entries are missing fields that were
  present pre-save; smoke tests pass because the defaults are falsy.

### INV-033: checkpoint restore replaces toolset globals
- **Statement**: `restore-session` rebinds `*toolset*`, `*tool-metadata*`,
  and `*tool-history*` via `setf` at `checkpoint.lisp:312-314`. No code path
  may load a checkpoint without going through this function.
- **Why it matters**: Bypassing the helper leaves globals pointing at the
  pre-restore state; callers see a mismatch between the restored
  conversation and the still-old toolset.
- **Citations**: `amoebum/src/checkpoint.lisp:312-314`,
  `amoebum/src/checkpoint.lisp:1049-1095`.
- **Symptom**: Restored session references tools by ID that no longer
  resolve, or resolves to the wrong implementation.

---

## Permissions

### INV-040: adding a permission rule bumps version AND clears cache
- **Statement**: Every call that mutates `*permission-rules*` must
  `(incf *permission-rules-version*)` and call
  `(clear-permission-cache :reason ...)` in the same function. The pattern
  is enforced in `add-permission-rule` and `clear-permission-rules`.
- **Why it matters**: `%permission-cache-key` stamps the version into every
  key (`permissions.lisp:1168-1173`); skipping the bump lets cached
  decisions outlive the rule that justified them.
- **Citations**: `amoebum/src/permissions.lisp:1121-1124` (clear),
  `amoebum/src/permissions.lisp:1144-1157` (add),
  `amoebum/src/permissions.lisp:1167-1173` (cache key),
  version slot at `amoebum/src/permissions.lisp:4`.
- **Symptom**: A freshly denied path still returns `:allow` from cache;
  security-sensitive regression.

### INV-041: path identity must match between decision and use time
- **Statement**: `%record-permission-path-identity-check` captures
  `target-folded`, `target-inode`, `parent-folded`, `parent-inode` at the
  time a decision is made. `%assert-path-identity-stable-at-use-time` MUST
  recompare all four at tool-invocation time and raise
  `tool-permission-denied` with `:reason-code :path-identity-changed` on
  mismatch.
- **Why it matters**: Prevents TOCTOU races where a symlink or rename
  swaps the resolved target between approval and open.
- **Citations**: capture at `amoebum/src/permissions.lisp:316-331`,
  compatibility predicate at `amoebum/src/permissions.lisp:340-352`,
  use-time assert at `amoebum/src/permissions.lisp:393-411`.
- **Symptom**: A writer can swap a symlink after approval and have the
  tool open an unexpected target.

### INV-042: normalize-permission-path is the sole path canonicalizer
- **Statement**: All permission path comparisons go through
  `normalize-permission-path` (which routes through
  `%permission-path-root-and-rest`,
  `%normalize-permission-path-segments`, and
  `%build-normalized-permission-path`). Ad-hoc string comparisons bypass
  `..` collapse, case folding, and trailing-slash handling.
- **Why it matters**: Two strings that represent the same path (e.g.
  `a/./b` vs `a/b`) compare unequal unless both go through the normalizer.
- **Citations**: `amoebum/src/permissions-path.lisp:87-102` (entry point),
  `amoebum/src/permissions-path.lisp:22-50` (root split),
  `amoebum/src/permissions-path.lisp:52-69` (segment collapse),
  `amoebum/src/permissions-path.lisp:71-85` (rebuild).
- **Symptom**: Allow-lists fail to match obviously-equivalent paths;
  users report "I allowed this and it's still denied".

### INV-043: permission rule effect is validated to allow/deny
- **Statement**: `add-permission-rule` signals `error "Permission rule
  EFFECT must be :allow or :deny"` on any other effect keyword. Do not
  remove this guard.
- **Why it matters**: Silently storing an unknown effect keyword causes
  the matcher to treat the rule as inert, leaving the caller convinced
  they added a deny when they didn't.
- **Citations**: `amoebum/src/permissions.lisp:1145-1146`.
- **Symptom**: Pipeline installs a "deny" rule from a user decision
  (`pipeline.lisp:554`) that silently becomes a no-op on a typo.

### INV-044: path-identity cache is bounded and evicted on size
- **Statement**: `%trim-permission-path-identity-cache` runs after every
  write and clears the cache if it grows past
  `*permission-path-identity-check-cache-limit*` (default 512).
- **Why it matters**: Unbounded growth in an approval-heavy session leaks
  memory and slows lookups.
- **Citations**: `amoebum/src/permissions.lisp:24-25`,
  `amoebum/src/permissions.lisp:360-363`,
  `amoebum/src/permissions.lisp:365-374`.
- **Symptom**: Long-running sessions show growing RSS tied to permission
  checks.

---

## Pipeline / Tool Execution

### INV-050: pre-tool chain runs in fixed order
- **Statement**: The `:before` method on `pseudopod:execute-tool` runs,
  in order: `%ensure-tool-registered`, `%validate-tool-arguments`,
  `%check-permission-or-signal`, `sandbox-check-tool-call`,
  `%maybe-log-invocation`, `:pre-tool-use` hook dispatch, then
  `(setf *pipeline-start-time-ms* (monotonic-ms))`. Reordering breaks
  either permission-before-sandbox or hook-before-timer guarantees.
- **Why it matters**: If the hook runs before permission, a hook can't
  see the permission decision; if sandbox runs before permission, a
  permission deny is shadowed by a less-informative sandbox error.
- **Citations**: `amoebum/src/pipeline.lisp:797-819`.
- **Symptom**: Denials surface with the wrong error class; hooks see
  stale pipeline state.

### INV-051: successful tool execution sets *pipeline-current-result*
- **Statement**: The happy path of `execute-tool` applies the sandbox
  output guard, then `setf`s `*pipeline-current-result*` to the guarded
  value before calling `%post-tool-success` and returning.
- **Why it matters**: `%post-tool-success` reads the dynamic variable to
  emit post-tool events; a direct return with the unguarded raw result
  both bypasses the guard and leaves the variable stale.
- **Citations**: `amoebum/src/pipeline.lisp:764-775`.
- **Symptom**: Post-tool events carry the previous tool's result; output
  filters silently skipped.

### INV-052: double budget warning is suppressed
- **Statement**: Only `%emit-stream-budget-warning-if-needed` emits the
  `stream-budget-warning-event`. The legacy
  `%enforce-stream-token-budget-if-needed` must NOT also emit it
  (fixed 864fbed).
- **Why it matters**: Duplicate warnings double-notify the user and
  corrupt the event journal's idempotency guarantees.
- **Citations**: `amoebum/src/ui/chat.lisp:2191-2210`.
- **Symptom**: Users see "context budget approaching" twice per
  threshold crossing. (Lifted from MEMORY.md "Pre-existing Test
  Failures".)

---

## Build / Runtime

### INV-060: SBCL nursery stays at 64 MB
- **Statement**: `main.lisp` configures a 64 MB SBCL nursery at startup.
  Do not shrink it back toward the 8 MB default.
- **Why it matters**: The virtual-scroll + per-message cache fix from
  2026-03-20 relies on the 64 MB nursery to absorb streaming-turn
  allocation without promoting to old-gen; shrinking it restores GC
  thrashing.
- **Citations**: `amoebum/src/main.lisp` nursery setup,
  detailed in `amoebum/src/ui/CLAUDE.md` "Streaming & render
  performance".
- **Symptom**: `bin/tui-perf-test.sh` shows minor-fault rate growth
  back near 6x over 8 prompts. (Lifted from MEMORY.md "GC Thrashing /
  TUI Slowdown Fix".)

### INV-061: tail pointers in streaming-markdown-renderer
- **Statement**: `streaming-markdown-renderer` in `ui/streaming.lisp`
  appends via tail pointers for O(1) append. Do not replace with
  `append` or `(setf last ... )` that walks the list.
- **Why it matters**: Naive append restores O(n) per-chunk cost;
  streaming a long assistant turn stalls the UI.
- **Citations**: `amoebum/src/ui/streaming.lisp`; rule in
  `amoebum/src/ui/CLAUDE.md` "Streaming & render performance".
- **Symptom**: Visible lag between LLM tokens arriving and rendering,
  growing with turn length. (Lifted from MEMORY.md "GC Thrashing / TUI
  Slowdown Fix".)

### INV-062: provider class initarg is :default-model
- **Statement**: Construct providers with
  `(make-instance 'pseudopod::kimi-provider :default-model ...)`.
  The initarg is `:default-model`, never `:model`. `make-kimi-provider`
  does NOT accept `:name`.
- **Why it matters**: `:model` is silently ignored; the provider falls
  back to the hardcoded default and tests that expect a specific model
  fail with unhelpful errors.
- **Citations**: see `amoebum/src/provider-factory.lisp` and the
  pseudopod provider class definitions; rule in
  `amoebum/src/CLAUDE.md` cross-project couplings.
- **Symptom**: Tests targeting a non-default model silently run against
  the default. (Lifted from MEMORY.md "Common Lisp / SBCL Gotchas".)

### INV-063: SBCL --eval takes one expression
- **Statement**: Scripts invoking SBCL non-interactively must put
  multi-form bodies in a temp `.lisp` file loaded with `--load`, not
  chained `--eval` flags. A single `--eval` flag parses exactly one
  expression.
- **Why it matters**: The second form is silently discarded; scripts
  appear to succeed but skip setup.
- **Citations**: documented in MEMORY.md "Common Lisp / SBCL Gotchas".
- **Symptom**: CI scripts run but skip half of their work.

---

Maintenance notes:
- When adding a new invariant, pick the next free ID in its category
  (categories reserve 001-009, 010-019, 020-029, 030-039, 040-049,
  050-059, 060-069, ...). IDs are stable and MUST NOT be recycled.
- When an invariant becomes obsolete, mark it `(RETIRED YYYY-MM-DD)`
  rather than deleting, so git-log greps still find the rationale.
- When updating MEMORY.md with a new gotcha, promote the grep-relevant
  ones here with attribution.
