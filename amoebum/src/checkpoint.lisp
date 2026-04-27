(in-package :amoebum)

(defparameter *checkpoint-directory-override* nil)
(defparameter *session-snapshot-directory-override* nil)
(defparameter *checkpoint-last-activity-at* (get-universal-time))
(defparameter *checkpoint-last-auto-checkpoint-at* nil)
(defparameter *checkpoint-default-max-count* 10)
;; Declared in src/macros/deftool.lisp; referenced here for load-order safety.
(defvar *toolset*)
(defvar *tool-metadata*)
(defvar *tool-history*)

(defstruct (session-checkpoint
            (:constructor make-session-checkpoint
                (&key id path created-at (auto-p nil) (trigger :manual))))
  id
  path
  (created-at 0 :type integer)
  (auto-p nil :type boolean)
  (trigger :manual :type keyword))
