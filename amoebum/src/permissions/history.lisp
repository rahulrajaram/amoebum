(in-package :amoebum)

;;; Permission decision history.
;;;
;;; Extracted mechanically from src/permissions.lisp for NXT-440. Owns the
;;; ring of recorded permission-check traces, the monotonic decision-id
;;; sequence, and the cached "last trace" used by replay/inspection paths.
;;; Decision-trace field ordering and limits are preserved byte-for-byte.

(defparameter *permission-decision-history* '())
(defparameter *permission-decision-history-limit* 256)
(defparameter *permission-decision-sequence* 0)
(defparameter *last-permission-decision-trace* nil)

(defun clear-permission-decision-history ()
  (setf *permission-decision-history* '()
        *permission-decision-sequence* 0
        *last-permission-decision-trace* nil)
  t)

(defun permission-decision-history (&key (limit 20))
  (subseq *permission-decision-history*
          0
          (min (max 0 limit) (length *permission-decision-history*))))

(defun %next-permission-decision-id ()
  (incf *permission-decision-sequence*)
  (format nil "perm-~D" *permission-decision-sequence*))

(defun %record-permission-decision (trace)
  (setf *last-permission-decision-trace* trace
        *permission-decision-history* (cons trace *permission-decision-history*))
  (when (> (length *permission-decision-history*) *permission-decision-history-limit*)
    (setf *permission-decision-history*
          (subseq *permission-decision-history* 0 *permission-decision-history-limit*)))
  trace)

(defun last-permission-decision-trace ()
  *last-permission-decision-trace*)
