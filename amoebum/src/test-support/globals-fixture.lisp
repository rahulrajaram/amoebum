(defpackage #:amoebum.test-support.globals-fixture
  (:use #:cl)
  (:export #:with-clean-amoebum-globals))

(in-package #:amoebum.test-support.globals-fixture)

;;; Test isolation fixture.
;;;
;;; Per amoebum/MEMORY.md, any test that calls `amoebum:restore-session` or
;;; mutates `amoebum:*toolset*` must save and restore the following globals,
;;; otherwise subsequent tests run with a corrupted global environment:
;;;
;;;   amoebum:*toolset*
;;;   amoebum:*tool-metadata*
;;;   amoebum:*tool-history*
;;;   amoebum:*memory-backend*
;;;   amoebum:*event-bus*
;;;   amoebum.sessions:*checkpoint-directory-override*
;;;
;;; All six symbols live in the `:amoebum` package and are exported from it
;;; (see amoebum/src/package.lisp). `with-clean-amoebum-globals` snapshots
;;; their current values, runs BODY inside an `unwind-protect`, and restores
;;; each one on exit whether BODY returns normally or via a non-local exit.

(defmacro with-clean-amoebum-globals (&body body)
  "Snapshot amoebum's test-sensitive globals, run BODY, restore on exit.

The following dynamic variables from the :amoebum package are saved before
BODY runs and unconditionally restored afterwards (including on error):

  *toolset*, *tool-metadata*, *tool-history*, *memory-backend*,
  *event-bus*, *checkpoint-directory-override*

Use this around any test that calls `amoebum:restore-session` or mutates
any of these globals directly."
  (let ((saved-toolset (gensym "SAVED-TOOLSET-"))
        (saved-tool-metadata (gensym "SAVED-TOOL-METADATA-"))
        (saved-tool-history (gensym "SAVED-TOOL-HISTORY-"))
        (saved-memory-backend (gensym "SAVED-MEMORY-BACKEND-"))
        (saved-event-bus (gensym "SAVED-EVENT-BUS-"))
        (saved-checkpoint-dir (gensym "SAVED-CHECKPOINT-DIR-")))
    `(let ((,saved-toolset (and (boundp 'amoebum:*toolset*) amoebum:*toolset*))
           (,saved-tool-metadata (and (boundp 'amoebum:*tool-metadata*)
                                      amoebum:*tool-metadata*))
           (,saved-tool-history (and (boundp 'amoebum:*tool-history*)
                                     amoebum:*tool-history*))
           (,saved-memory-backend (and (boundp 'amoebum:*memory-backend*)
                                       amoebum:*memory-backend*))
           (,saved-event-bus (and (boundp 'amoebum:*event-bus*)
                                  amoebum:*event-bus*))
           (,saved-checkpoint-dir (and (boundp 'amoebum.sessions:*checkpoint-directory-override*)
                                       amoebum.sessions:*checkpoint-directory-override*)))
       (unwind-protect
            (progn ,@body)
         (setf amoebum:*toolset* ,saved-toolset
               amoebum:*tool-metadata* ,saved-tool-metadata
               amoebum:*tool-history* ,saved-tool-history
               amoebum:*memory-backend* ,saved-memory-backend
               amoebum:*event-bus* ,saved-event-bus
               amoebum.sessions:*checkpoint-directory-override* ,saved-checkpoint-dir)))))
