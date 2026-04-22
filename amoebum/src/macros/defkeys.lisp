(in-package :amoebum)

;;;; Residual defkeys facade.
;;;;
;;;; The historical 910-line `macros/defkeys.lisp` was decomposed by
;;;; NXT-393 into five sibling modules under `src/macros/defkeys/`:
;;;;
;;;;   * `defkeys/parser.lisp`    — `malformed-key-binding` and
;;;;                                `keymap-definition-warning` conditions,
;;;;                                `key-binding` / `key-chord` / `keymap` /
;;;;                                `keymap-overlay` structs, all parameter
;;;;                                globals (registry, stack, overlay stack,
;;;;                                sequence buffer, timeouts, pending
;;;;                                escape, terminal normalization profile
;;;;                                + table), and the eval-when compile-time
;;;;                                helpers (key-spec parsing, terminal
;;;;                                profile detection, event-to-signature,
;;;;                                keymap designator resolution, chord
;;;;                                suffix/prefix matching).
;;;;   * `defkeys/registry.lisp`  — registry mutators (`make-keymap`,
;;;;                                `register-keymap`, `register-key-binding`,
;;;;                                `define-chord`, ...), keymap / overlay
;;;;                                stack operations, sequence-buffer and
;;;;                                pending-escape state helpers.
;;;;   * `defkeys/dispatch.lisp`  — event dispatch pipeline
;;;;                                (`dispatch-key-event`,
;;;;                                `dispatch-active-keymaps`,
;;;;                                `flush-key-dispatch-timeouts`,
;;;;                                `make-key-dispatch-on-event`) plus
;;;;                                chord matching and alt-modifier
;;;;                                disambiguation.
;;;;   * `defkeys/expansion.lisp` — macroexpansion helpers and the
;;;;                                `defkeys` macro itself. Defended
;;;;                                byte-identically by the NXT-391
;;;;                                `test/snapshots/macroexpand/
;;;;                                defkeys-*.sexp` goldens.
;;;;   * `defkeys/builtins.lisp`  — `chat-mode`, `command-mode`,
;;;;                                `diff-mode` placeholder keymaps plus
;;;;                                `activate-default-keymaps` and the
;;;;                                lazy load-time activation hook.
;;;;
;;;; Public symbols (`defkeys`, `make-keymap`, `register-keymap`,
;;;; `register-key-binding`, `dispatch-active-keymaps`,
;;;; `make-key-dispatch-on-event`, `*keymap-registry*`, ...) continue to
;;;; be reachable from `:amoebum`; the load order in `amoebum.asd` brings
;;;; the submodules in before this file so the facade itself carries no
;;;; behavior — it only documents the seam.
