(in-package :amoebum)

;;;; Residual defskill facade.
;;;;
;;;; The historical 1001-line `macros/defskill.lisp` was decomposed by
;;;; NXT-392 into six sibling modules under `src/macros/defskill/`:
;;;;
;;;;   * `defskill/registry.lisp`        — skill globals, `skill-argument`
;;;;                                       and `skill-metadata` structs,
;;;;                                       `%skill-now-ms`,
;;;;                                       `%normalize-skill-name`,
;;;;                                       `register-skill`, `find-skill`,
;;;;                                       `list-skills`, `describe-skill`.
;;;;   * `defskill/runtime.lisp`         — argument lookup, missing-argument
;;;;                                       reporting, default completer.
;;;;   * `defskill/tool-invocation.lisp` — `%skill-invoke-tool` plus the
;;;;                                       JSON / plist coercion helpers used
;;;;                                       by built-in skills that route
;;;;                                       through the permission pipeline.
;;;;   * `defskill/review.lisp`          — `/review` finding normalization,
;;;;                                       machine-readable report, human
;;;;                                       rendering, default LLM analyzer,
;;;;                                       lazy analyzer wiring.
;;;;   * `defskill/expansion.lisp`       — compile-time helpers and the
;;;;                                       `defskill` macro itself. Defended
;;;;                                       byte-identically by the NXT-391
;;;;                                       `test/snapshots/macroexpand/
;;;;                                       defskill*.sexp` goldens.
;;;;   * `defskill/builtins.lisp`        — `/commit`, `/review`, `/compact`,
;;;;                                       `/status` skill definitions plus
;;;;                                       the `/status`-only
;;;;                                       `%status-token-usage` helper.
;;;;
;;;; Public symbols (`defskill`, `make-skill-argument`, `make-skill-metadata`,
;;;; `register-skill`, `find-skill`, `list-skills`, `describe-skill`,
;;;; `*skill-registry*`, `*skill-review-analyzer*`, ...) continue to be
;;;; reachable from `:amoebum`; the load order in `amoebum.asd` brings the
;;;; submodules in before this file so the facade itself carries no
;;;; behavior — it only documents the seam.
