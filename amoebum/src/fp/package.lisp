;;;; amoebum/src/fp/package.lisp
;;;;
;;;; Minimal functional/immutable kernel for amoebum. This package is the
;;;; foundation used to reduce imperative `setf` usage in higher-level code
;;;; (notably the UI layer). It has no external dependencies beyond the
;;;; Common Lisp standard.

(defpackage #:amoebum.fp
  (:use #:cl)
  (:export
   ;; threading macros
   #:->
   #:->>
   ;; result type
   #:ok
   #:err
   #:make-ok
   #:make-err
   #:ok-p
   #:err-p
   #:ok-value
   #:err-value
   #:result-map
   #:result-bind
   ;; functional update helpers
   #:assoc-in
   #:update-in
   #:update
   ;; sealed-enum dispatch
   #:match
   #:assert-exhaustive
   #:non-exhaustive-match
   #:non-exhaustive-match-value
   ;; persistent plist helpers
   #:plist-assoc
   #:plist-dissoc
   #:plist-merge
   #:plist-get-in
   #:plist-select-keys
   ;; declarative transition table
   #:make-transition-table
   #:transition-table-lookup
   #:transition
   #:transition-table-events-for
   ;; frozen record macro
   #:defrozen))
