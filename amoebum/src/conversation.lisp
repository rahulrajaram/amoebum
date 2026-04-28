(in-package :amoebum)

;; NXT-424: conversation state, codec, fork persistence, load helpers, and
;; history search now live in dedicated src/conversation/*.lisp submodules.
;; This residual facade remains load-order stable for callers that expect the
;; legacy src/conversation path to exist in the system definition.
