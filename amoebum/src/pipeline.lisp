(in-package :amoebum)

;;; NXT-434: pipeline control-plane facade.
;;; Context shaping, recovery hooks, lifecycle event publication, and
;;; execution-loop helpers now live in `pipeline/{context,recovery,events,
;;; execution}.lisp` and continue to share the `:amoebum` package so the
;;; public API and CLOS method combinations remain unchanged.
