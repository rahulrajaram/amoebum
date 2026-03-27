(in-package :amoebum)

;;; ---------------------------------------------------------------------------
;;; Compact Style Table — replace 12-element plist segments with fixnum IDs
;;; ---------------------------------------------------------------------------

(defstruct (style-entry
            (:constructor %make-style-entry (role boldp italicp underlinep invertp dimp strikep)))
  (role :default :type keyword)
  (boldp nil :type boolean)
  (italicp nil :type boolean)
  (underlinep nil :type boolean)
  (invertp nil :type boolean)
  (dimp nil :type boolean)
  (strikep nil :type boolean))

(defvar *style-table* (make-array 64 :adjustable t :fill-pointer 0)
  "Vector of style-entry structs, indexed by style-id fixnum.")

(defvar *style-index* (make-hash-table :test #'equalp)
  "Maps style-entry key lists to their fixnum style-id for deduplication.")

(defun intern-style (role &key boldp italicp underlinep invertp dimp strikep)
  "Return a fixnum style-id for the given style combination.
Deduplicates: identical styles always return the same id."
  (let* ((key (list (or role :default)
                    (not (null boldp))
                    (not (null italicp))
                    (not (null underlinep))
                    (not (null invertp))
                    (not (null dimp))
                    (not (null strikep))))
         (existing (gethash key *style-index*)))
    (or existing
        (let ((id (vector-push-extend
                   (%make-style-entry (or role :default)
                                      (not (null boldp))
                                      (not (null italicp))
                                      (not (null underlinep))
                                      (not (null invertp))
                                      (not (null dimp))
                                      (not (null strikep)))
                   *style-table*)))
          (setf (gethash key *style-index*) id)
          id))))

(defun lookup-style (style-id)
  "Return the style-entry for STYLE-ID."
  (aref *style-table* style-id))

;;; --- Compact segment accessors ---
;;; A compact segment is (text . style-id) where style-id is a fixnum.

(declaim (inline compact-segment-p compact-segment-text compact-segment-style-id))

(defun compact-segment-p (seg)
  "Return T if SEG is a compact (text . style-id) segment."
  (and (consp seg)
       (stringp (car seg))
       (typep (cdr seg) 'fixnum)))

(defun compact-segment-text (seg)
  "Return the text string of a compact segment."
  (car seg))

(defun compact-segment-style-id (seg)
  "Return the style-id fixnum of a compact segment."
  (the fixnum (cdr seg)))
