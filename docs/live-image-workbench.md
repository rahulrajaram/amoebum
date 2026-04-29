# The Live-Image Workbench

> "The image is the runtime." — VISION T2

This document walks an operator from cold start through hot-patching a
tool, defining a new tool from the REPL panel, and snapshotting the
image — exercising every keystroke that makes amoebum a programmable
workbench rather than a one-shot agent runner.

If you finish this walkthrough and the workflow felt natural, the §6
differentiation has landed.

## Prerequisites

- amoebum built (`make build` from repo root produces an SBCL image
  with all systems loaded).
- A terminal that survives `tput`-based redraws — any modern
  `xterm`/`alacritty`/`kitty`/`tmux` works.
- ~50 MB of disk available under `~/.amoebum/images/` if you intend
  to save a snapshot.

## Cold start

```bash
cd /path/to/amoebum
make run-amoebum   # or whatever the project's launch target is
```

You land in the chat TUI with the input prompt focused. You can drive
LLM turns from here — that's the conventional path. Everything below
is the *unconventional* path: programming the running assistant from
inside the same chat.

## Step 1 — Open the REPL panel (NXT-575)

Press **Ctrl-G** ("go to Lisp"). A REPL overlay appears between the
message-history and the prompt input. The overlay has its own input
line and a scrollable history of past evaluations.

Type:

```lisp
(+ 1 2)
```

and press Enter. You see:

```
> (+ 1 2)
=> 3
```

The REPL is bound to the **live image's globals**, not a sandbox copy.
Try:

```lisp
(length amoebum::*toolset*)
```

You get back the actual count of tools loaded into the running
instance. The REPL respects the same denylist as `/deftool` and
`/self-modify` (see "Sandbox boundary" below) — so

```lisp
(delete-file "/etc/passwd")
```

returns a refusal, not destruction.

To close the panel, press **Ctrl-G** again or **Escape**.

### What this proves
The single keystroke between *using* and *programming* the assistant
is real. It is not aspirational text in a vision document — there is
a Ctrl-G keybinding registered at `ptui/src/term/input.lisp` and a
panel module at `amoebum/src/ui/panels/repl-panel.lisp`.

## Step 2 — Define a new tool from the REPL or via slash-command (NXT-577)

Two paths converge on the same endpoint.

### Path A — `/deftool` slash command

In the prompt (not the REPL), type:

```
/deftool greet "Returns a friendly greeting" (lambda (args) (format nil "Hello, ~A!" (getf args :name)))
```

Press Enter. The command parses the name, description, and body;
evaluates the body through `sandboxed-eval`; registers the result via
`pseudopod:register-tool`; and the new tool is immediately available
to the next LLM turn.

Confirm:

```
/deftool --list
```

(or open the REPL with Ctrl-G and `(length amoebum::*toolset*)` —
should be one higher than before.)

### Path B — REPL evaluation directly

In the REPL panel, you can call the same registration API the slash
command calls:

```lisp
(amoebum:register-runtime-tool
  :name "greet"
  :description "Returns a friendly greeting"
  :handler (lambda (args) (format nil "Hello, ~A!" (getf args :name))))
```

(Symbol names may differ — discover via `(apropos "register" :amoebum)`
in the REPL. The point is: the REPL has the same access surface as
slash-command handlers because they both run inside the live image.)

### What this proves
"Macros as language" (VISION §6.2) means the operator can grow the
tool surface without restarting amoebum, without editing source, and
without leaving chat. The runtime is genuinely live.

### Undoing
```
/deftool --undo greet
```

Removes the tool from `*toolset*`. The image is back to where it
started (modulo any state you mutated in the REPL).

## Step 3 — Hot-patch a Lisp file (NXT-576)

Open another terminal window. Edit any `.lisp` file under
`amoebum/src/` or `~/.amoebum/extensions/`. For example:

```bash
cat >> ~/.amoebum/extensions/demo.lisp <<'EOF'
(defparameter *demo-marker* :reload-1)
EOF
```

(If `~/.amoebum/extensions/` does not exist, create it first.)

Watch the chat. Within ~500 ms a system message appears:

```
Hot-patched: ~/.amoebum/extensions/demo.lisp (reloaded; takes effect on next call)
```

In the REPL panel (Ctrl-G), evaluate:

```lisp
*demo-marker*
```

You get back `:reload-1`.

Now in the other terminal, change the value:

```bash
echo '(setf *demo-marker* :reload-2)' >> ~/.amoebum/extensions/demo.lisp
```

Toast appears again. Re-evaluate `*demo-marker*` in the REPL — you get
`:reload-2`. The running image picked up the change without
restarting.

### What this proves
The `(load file)` cycle that Common Lisp programmers have used for
decades is now wired into the chat TUI. Save a file → image redefines
the function → next call uses the new body. No build, no restart, no
state loss.

### Mechanics
A file watcher polls `amoebum/src/**/*.lisp` and
`~/.amoebum/extensions/**/*.lisp` once per render frame at 500 ms
debounce (`amoebum/src/ui/hot-patch-watcher.lisp`). When a path's
mtime advances, it publishes an `extension:reloaded` event on the
event bus, calls `(load path)`, emits the toast. The watcher uses the
shared file-watcher primitive at `amoebum/src/fp/file-watcher.lisp`
(NXT-597), which the YAML theme reload (NXT-587) also uses.

### Errors during reload
If the edited file has a syntax error, the toast becomes:

```
Hot-patch FAILED: ~/.amoebum/extensions/demo.lisp — End of file in #<...>
```

The original image stays intact. Fix the file, save again, get the
success toast.

## Step 4 — Save the image (NXT-582)

You now have a workbench with `*demo-marker*`, the `greet` tool (if
you didn't undo it), any state you mutated in the REPL — all in
memory. Persist it:

```
/save-image my-workbench
```

The chat reports:

```
Image saved to ~/.amoebum/images/my-workbench.core
```

**Important behavior**: SBCL's `save-lisp-and-die` terminates the
running process after writing the core file. The chat will exit. This
is not a bug — it is how `sb-ext:save-lisp-and-die` works at the
runtime level. Treat `/save-image` as "checkpoint and exit".

To list saved images:

```
/list-images
```

### Restoring

```
/load-image my-workbench
```

prints the launch instruction:

```
Run: sbcl --core ~/.amoebum/images/my-workbench.core
(or ./~/.amoebum/images/my-workbench.core directly — the core is :executable t)
```

Run that command in your shell. SBCL launches from the saved core,
recovering everything that was in memory at save time, including the
`greet` tool and `*demo-marker*` value. Pre-save / post-restore
hooks (`amoebum/src/checkpoint/image.lisp`) handle file descriptor
cleanup, network connection drain, MCP reconnect, and API
re-authentication — so the resumed image works against external
services without manual reconfiguration.

### What this proves
"The image is the runtime" (VISION T2) is operational. Your workbench
state survives across machine restarts. The configured tools, the
hot-patched extensions, the conversation context — all checkpointable
as a single `.core` file.

### Caveats
- File handles do not survive serialization; the post-restore hooks
  handle reopening but anything held in transient streams is lost.
- Network sockets must be drained pre-save (the hook does this).
- LLM provider sessions (auth tokens, streaming connections) are
  re-established post-restore; expect a brief reconnect latency on
  the first turn after `--core` resume.

## Step 5 — YAML-driven layout (NXT-585/586/587)

Optional but immediately useful. Edit
`amoebum/resources/themes/amoebum.tui-spec.yaml` (or your own copy).
Change the `input-prompt` height from 3 to 7. Save the file.

Within ~500 ms (the file watcher's debounce window), the chat
re-renders with a 7-row prompt. No restart, no key press needed.

To trigger an immediate reload, press **r** at an empty prompt
(default reload key — see `behavior.keys.reload` in the YAML).

### What this proves
Layout is declarative and live-editable. The chat composition reads
heights, borders, fill-weights, and visibility from
`*yaml-layout-loaded*` at render time
(`amoebum/src/ui/panels/chat-panel.lisp`'s resolver helpers).

### Currently parsed but not consumed
- `padding`, `focus-order`, `focusable` are parsed by
  `amoebum/src/ui/layout-yaml.lisp` but not yet routed to consumers.
  See `.agent/chat-focus-cycle-and-padding-design-2026-04-29.md`
  (NXT-596) for the design memo and NXT-588 for the unblocking work.
- Custom widget children (`:widgets:` section) are not yet supported.
  NXT-589 will add this once the focus-cycle prerequisite lands.

## Sandbox boundary

Any time the operator submits Lisp — REPL panel, `/deftool`,
`/self-modify` — the form is evaluated through `sandboxed-eval`
(defined at `amoebum/src/self-modify.lisp:318`). The sandbox enforces:

- A symbol denylist: `delete-file`, `open`, `eval`, `run-program`,
  `compile-file`, ... (full list in `*self-modify-forbidden-symbol-names*`).
- A package denylist: `uiop`, `asdf`, `sb-ext`, ... (full list in
  the sibling forbidden-package list).

The denylist is single-source-of-truth. Adding a symbol there blocks
it across the REPL, `/deftool`, and any future operator-eval surface.

The sandbox is not a tight VM-level boundary — a determined attacker
who can already write arbitrary Lisp at the operator surface has
already crossed the trust boundary. The sandbox prevents accidents
and casual misuse, not adversarial bypass.

## Recap — the keystroke surface

| Surface | Keystroke / command | What it does |
|---|---|---|
| REPL panel | **Ctrl-G** | Toggle live CL prompt overlay |
| Reload YAML layout | **r** (empty prompt) | Re-read theme/layout from disk |
| Define a tool | `/deftool <name> "<desc>" <body>` | Register a runtime tool |
| Undefine a tool | `/deftool --undo <name>` | Remove from `*toolset*` |
| Save image | `/save-image [name]` | Snapshot to `~/.amoebum/images/` |
| List images | `/list-images` | Show saved cores with timestamps |
| Load image | `/load-image <name>` | Print launch command for resume |
| Auto hot-patch | (none — automatic) | Edit a `.lisp` file → image reloads |
| Manage extensions | `/extensions list/reload/enable/disable` | Per-extension control |

## What does not yet exist

The persistent-session entrypoint (NXT-574) — the variant of `make
run-amoebum` that keeps a single SBCL image alive across multiple
LLM turns instead of treating each turn as a fresh process — is not
yet built. Today, the conventional one-shot launch is what you get;
the inhabited workflow lives within a single process's lifetime.
NXT-574 is the next big lift.

## Cross-references

- VISION §6 — the differentiating UX target this workflow serves.
- `amoebum/src/ui/panels/repl-panel.lisp` — REPL widget (NXT-575).
- `amoebum/src/ui/repl-panel-state.lisp` — REPL state machine.
- `amoebum/src/commands/deftool.lisp` — runtime tool registration
  (NXT-577).
- `amoebum/src/commands/image.lisp` — image-snapshot slash commands
  (NXT-582).
- `amoebum/src/checkpoint/image.lisp` — underlying save/restore
  infrastructure (pre-existing, decomposed by earlier waves).
- `amoebum/src/ui/hot-patch-watcher.lisp` — file watcher for live
  reload (NXT-576).
- `amoebum/src/fp/file-watcher.lisp` — shared file-watcher primitive
  (NXT-597).
- `amoebum/src/ui/yaml-theme-layout.lisp` — YAML reload key + watcher
  (NXT-586, NXT-587).
- `.agent/yaml-layout-parity-investigation.md` — corrected baseline of
  what the YAML DSL controls (NXT-584).
- `.agent/file-watcher-primitive-decision-2026-04-29.md` — adopt-vs-
  build-vs-defer record for the watcher primitive (NXT-593).
- `.agent/chat-focus-cycle-and-padding-design-2026-04-29.md` —
  prerequisite design memo for the next layout-DSL wave (NXT-596).
