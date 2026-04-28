(in-package :amoebum)

;;;; Residual deftool facade.
;;;;
;;;; The historical 760-line `macros/deftool.lisp` was decomposed by
;;;; NXT-395 into five sibling modules under `src/macros/deftool/`:
;;;;
;;;;   * `deftool/metadata.lisp`  — tool globals, compile-time validation
;;;;                                registry, metadata structs, name/parameter
;;;;                                normalization, and option validation
;;;;                                helpers.
;;;;   * `deftool/history.lisp`   — tool version snapshots, metadata diffing,
;;;;                                redefinition events, `tool-history`, and
;;;;                                `rollback-tool`.
;;;;   * `deftool/schema.lisp`    — CL type to JSON schema mapping,
;;;;                                round-trip warnings, and per-tool schema
;;;;                                assembly.
;;;;   * `deftool/coercion.lisp`  — argument extraction plus runtime coercion
;;;;                                for booleans, members, integers,
;;;;                                pathnames, and nullable unions.
;;;;   * `deftool/expansion.lisp` — declaration parsing, binding/runtime
;;;;                                builders, and the `deftool` macro itself.
;;;;                                Defended byte-identically by the NXT-391
;;;;                                `test/snapshots/macroexpand/
;;;;                                deftool*.sexp` goldens.
;;;;
;;;; Public symbols (`deftool`, `*toolset*`, `*tool-metadata*`,
;;;; `*tool-history*`, `make-tool-metadata`, `tool-history`,
;;;; `rollback-tool`, `cl-type-to-json-schema`, ...) continue to be reachable
;;;; from `:amoebum`; the load order in `amoebum.asd` brings the submodules in
;;;; before this file so the facade itself carries no behavior — it only
;;;; documents the seam.
