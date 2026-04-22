(in-package :amoebum)

;;;; Residual memory facade.
;;;;
;;;; The historical 1168-line `memory.lisp` was decomposed by NXT-388
;;;; into four sibling modules under `src/memory/`:
;;;;
;;;;   * `memory/backend.lisp`        — backend protocol, memory-entry/
;;;;                                    candidate types, autodetect
;;;;                                    chain (file vs Haake CLI),
;;;;                                    `current-memory-backend`.
;;;;   * `memory/file-store.lisp`     — on-disk MEMORY.md format,
;;;;                                    import resolution, scope-aware
;;;;                                    reads, file-backend
;;;;                                    store/list/query/delete/forget.
;;;;   * `memory/haake-adapter.lisp`  — Haake CLI memory backend
;;;;                                    (already extracted earlier).
;;;;   * `memory/haake-transfer.lisp` — `memory-import-to-haake` /
;;;;                                    `memory-export-from-haake`
;;;;                                    bridges plus dedupe sidecar.
;;;;   * `memory/commands.lisp`       — `/memory ...` slash-command
;;;;                                    parser and natural-language
;;;;                                    candidate extraction.
;;;;
;;;; Public symbols (`store-memory`, `memory-list`, `memory-query`,
;;;; `memory-delete`, `memory-forget`, `memory-command-input-p`,
;;;; `run-memory-command`, `extract-durable-memory-candidate`,
;;;; `apply-memory-candidate`, `*memory-backend*`,
;;;; `current-memory-backend`, `make-file-memory-backend`,
;;;; `make-haake-cli-memory-backend`, ...) continue to be reachable
;;;; from `:amoebum`; the load-order in `amoebum.asd` brings the
;;;; submodules in before this file so the facade itself carries no
;;;; behavior — it only documents the seam.
