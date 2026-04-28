(in-package :amoebum)

;;; Permission evaluation cache state.
;;;
;;; Extracted mechanically from src/permissions.lisp for NXT-440. Holds the
;;; mutable counters and tables that back the rule-evaluation cache and the
;;; path-identity recheck cache, plus the rule-list version counter that
;;; participates in cache keys. The cache mutators themselves live in
;;; permissions-rules.lisp / permissions-path.lisp; this module only owns
;;; the storage and tuning knobs so cache key shape stays byte-stable.

(defparameter *permission-rules* nil)
(defparameter *permission-rules-version* 0)

(defparameter *permission-evaluation-cache* (make-hash-table :test #'equal))
(defparameter *permission-cache-hits* 0)
(defparameter *permission-cache-misses* 0)
(defparameter *permission-cache-invalidations* 0)
(defparameter *permission-cache-invalidation-events* '())
(defparameter *permission-cache-invalidation-events-limit* 128)

(defparameter *permission-path-identity-check-cache* (make-hash-table :test #'equal))
(defparameter *permission-path-identity-check-cache-limit* 512)
(defparameter *permission-path-identity-recheck-hook* nil)
