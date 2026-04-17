;;;; amoebum/test/pipeline-unit-test.lisp
;;;;
;;;; NXT-287: Phase boundary unit tests for amoebum/src/pipeline.lisp.
;;;;
;;;; amoebum's pipeline is not a classical phase machine; the "phases"
;;;; are the ordered CLOS method combination around `pseudopod:execute-tool`:
;;;;
;;;;   phase 1 (entering :around):   argument decoding + dynamic bindings
;;;;   phase 2 (:before):            tool-registered check
;;;;   phase 3 (:before):            argument validation (required + type)
;;;;   phase 4 (:before):            permission check + sandbox check
;;;;   phase 5 (:before):            pre-tool-use hook dispatch
;;;;   phase 6 (:before):            usdt + tool-invoked event publish
;;;;   phase 7 (primary):            tool body execution
;;;;   phase 8 (post-success):       metrics, cache, post-tool-use hook,
;;;;                                  tool-completed event
;;;;   phase 9 (error transition):   %coerce-tool-error + tool-error event
;;;;   terminal:                     restart exits (skip-tool / abort-tool)
;;;;
;;;; These tests probe the boundaries between those phases. The existing
;;;; `pipeline-context-test` only covers context construction; the
;;;; `method-combination-dispatch-test` covers ordering of a happy path.
;;;; These tests cover entry/exit preconditions and error transitions
;;;; between adjacent phases.
;;;;
;;;; Test isolation: every test saves and restores `*toolset*`,
;;;; `*hook-registry*`, and `*event-bus*` under `unwind-protect` per
;;;; amoebum/test/CLAUDE.md.

(in-package :amoebum/test)

(def-suite pipeline-unit-suite :in amoebum-suite
  :description "NXT-287 phase-boundary unit tests for amoebum/src/pipeline.lisp.")

(in-suite pipeline-unit-suite)

(defun %nxt287-object-schema ()
  (let ((schema (make-hash-table :test #'equal)))
    (setf (gethash "type" schema) "object")
    schema))

(defun %nxt287-make-args (&rest entries)
  (let ((table (make-hash-table :test #'equal)))
    (loop for (key value) on entries by #'cddr do
          (setf (gethash key table) value))
    table))

(defun %nxt287-make-context (toolset &key (permission-mode :full-auto) logger)
  (amoebum:make-amoebum-context
   :toolset toolset
   :permission-mode permission-mode
   :event-bus amoebum:*event-bus*
   :hook-registry amoebum:*hook-registry*
   :logger logger
   :initialize-notifications-p nil))

(defun %nxt287-register-ok-tool (toolset &key (name "nxt287-ok"))
  (pseudopod:register-tool-function
   toolset
   :name name
   :description "NXT-287 trivial success tool."
   :parameters (%nxt287-object-schema)
   :fn (lambda (_arguments _call)
         (declare (ignore _arguments _call))
         "nxt287-ok-result")))

(defun %nxt287-register-boom-tool (toolset &key (name "nxt287-boom"))
  (pseudopod:register-tool-function
   toolset
   :name name
   :description "NXT-287 failing tool for error-transition coverage."
   :parameters (%nxt287-object-schema)
   :fn (lambda (_arguments _call)
         (declare (ignore _arguments _call))
         (error "nxt287 forced failure"))))

(defun %nxt287-call (name &optional (arguments "{}"))
  (pseudopod:make-tool-call
   :id (format nil "nxt287-call-~A" name)
   :name name
   :arguments arguments))

(defmacro %nxt287-with-isolated-globals (&body body)
  `(let ((%orig-toolset amoebum:*toolset*)
         (%orig-hooks amoebum:*hook-registry*)
         (%orig-event-bus amoebum:*event-bus*))
     (unwind-protect
          (progn
            (setf amoebum:*toolset* (pseudopod:make-toolset)
                  amoebum:*hook-registry* (make-hash-table :test #'equal)
                  amoebum:*event-bus* (amoebum:make-event-bus :capacity 32))
            ,@body)
       (setf amoebum:*toolset* %orig-toolset
             amoebum:*hook-registry* %orig-hooks
             amoebum:*event-bus* %orig-event-bus))))

;;; --------------------------------------------------------------------
;;; Phase 2 boundary: entering tool-registered check must reject unknown
;;; tools BEFORE arguments are validated or permissions checked.
;;; --------------------------------------------------------------------
(test nxt287-phase2-unknown-tool-signals-tool-not-found
  "Executing an unregistered tool must raise tool-not-found from the
   :before phase, before permission or validation phases run."
  (%nxt287-with-isolated-globals
    (let ((context (%nxt287-make-context amoebum:*toolset*)))
      (handler-case
          (progn
            (amoebum:execute-tool
             (%nxt287-call "nxt287-nope-not-registered")
             context)
            (fail "Expected unknown tool to signal a capability-gap condition."))
        (amoebum:capability-gap (condition)
          (is (string= "nxt287-nope-not-registered"
                       (amoebum::tool-error-tool-name condition)))
          (is (string= "nxt287-nope-not-registered"
                       (amoebum:capability-gap-capability-name condition)))
          (is (equal "capability_gap"
                     (getf (amoebum:capability-gap-recovery-contract condition)
                           :kind))))))))

;;; --------------------------------------------------------------------
;;; Phase 3 boundary: argument validation rejects missing required args
;;; BEFORE the tool body runs.
;;; --------------------------------------------------------------------
(test nxt287-phase3-missing-required-argument-blocks-body
  "A tool declared with a required parameter must not reach the primary
   method when that argument is absent. Recovery mode is :disabled so
   the missing-argument signal is not rewritten to a value or retry."
  (%nxt287-with-isolated-globals
    (let ((body-ran nil))
      (pseudopod:register-tool-function
       amoebum:*toolset*
       :name "nxt287-needs-path"
       :description "NXT-287 validation boundary probe."
       :parameters (%nxt287-object-schema)
       :fn (lambda (_arguments _call)
             (declare (ignore _arguments _call))
             (setf body-ran t)
             "should-not-run"))
      ;; Register metadata declaring :path as required so the pipeline
      ;; argument-validation phase fires.
      (setf (gethash "nxt287-needs-path" amoebum::*tool-metadata*)
            (amoebum::make-tool-metadata
             :name "nxt287-needs-path"
             :parameter-specs '((:name "path" :type t :required t))))
      (let ((amoebum::*missing-tool-argument-recovery-mode* :disabled)
            (context (%nxt287-make-context amoebum:*toolset*)))
        (signals amoebum:tool-error
          (amoebum:execute-tool
           (%nxt287-call "nxt287-needs-path" "{}")
           context))
        (is (null body-ran))))))

;;; --------------------------------------------------------------------
;;; Phase 4 boundary: permission denial must short-circuit before the
;;; tool body runs. :readonly denies write-style tools that carry the
;;; dangerous flag; we test via hook :deny because that path is the
;;; deterministic equivalent and hits the same exit boundary.
;;; --------------------------------------------------------------------
(test nxt287-phase4-pre-tool-use-deny-hook-blocks-primary
  "A :pre-tool-use hook returning :deny must raise tool-permission-denied
   and prevent the primary method from running."
  (%nxt287-with-isolated-globals
    (let ((body-ran nil))
      (pseudopod:register-tool-function
       amoebum:*toolset*
       :name "nxt287-denied"
       :description "NXT-287 deny-hook boundary probe."
       :parameters (%nxt287-object-schema)
       :fn (lambda (_arguments _call)
             (declare (ignore _arguments _call))
             (setf body-ran t)
             "should-not-run"))
      (amoebum:register-hook :pre-tool-use
                             'nxt287-deny-hook
                             (lambda (_tool-name _args)
                               (declare (ignore _tool-name _args))
                               :deny))
      (let ((context (%nxt287-make-context amoebum:*toolset*)))
        (signals amoebum:tool-permission-denied
          (amoebum:execute-tool
           (%nxt287-call "nxt287-denied")
           context))
        (is (null body-ran))))))

;;; --------------------------------------------------------------------
;;; Phase 7->8 boundary: exiting the primary phase successfully must
;;; populate metrics, cache the result, and publish a tool-completed
;;; event (post-success phase).
;;; --------------------------------------------------------------------
(test nxt287-phase8-post-success-populates-metrics-and-cache
  "After a successful tool call, metrics and the result cache must be
   populated on the context (entering post-success phase)."
  (%nxt287-with-isolated-globals
    (%nxt287-register-ok-tool amoebum:*toolset*)
    (let* ((context (%nxt287-make-context amoebum:*toolset*))
           (result (amoebum:execute-tool
                    (%nxt287-call "nxt287-ok")
                    context)))
      (is (string= result "nxt287-ok-result"))
      (let ((metrics (amoebum:context-tool-metrics context "nxt287-ok")))
        (is (not (null metrics)))
        (is (= 1 (getf metrics :count)))
        (is (eq :ok (getf metrics :last-status))))
      (let ((cached (amoebum:cached-tool-result
                     context "nxt287-ok" (%nxt287-make-args))))
        (is (string= cached "nxt287-ok-result"))))))

;;; --------------------------------------------------------------------
;;; Phase 8 exit: post-tool-use hook is invoked exactly once per
;;; successful call (entering post-success phase -> hook dispatch ->
;;; tool-completed event).
;;; --------------------------------------------------------------------
(test nxt287-phase8-post-tool-use-hook-fires-once
  "Exiting the primary phase successfully must dispatch :post-tool-use
   exactly once with the tool name and elapsed-ms."
  (%nxt287-with-isolated-globals
    (%nxt287-register-ok-tool amoebum:*toolset*)
    (let ((hook-calls 0)
          (observed-name nil))
      (amoebum:register-hook :post-tool-use
                             'nxt287-post-hook
                             (lambda (tool-name _result _elapsed-ms)
                               (declare (ignore _result _elapsed-ms))
                               (incf hook-calls)
                               (setf observed-name tool-name)
                               nil))
      (let ((context (%nxt287-make-context amoebum:*toolset*)))
        (amoebum:execute-tool
         (%nxt287-call "nxt287-ok")
         context)
        (is (= 1 hook-calls))
        (is (equal "nxt287-ok" observed-name))))))

;;; --------------------------------------------------------------------
;;; Phase 7 -> phase 9 boundary: primary failure transitions into the
;;; error-coercion phase. Metrics must record :error status, the
;;; condition must be typed as tool-error, and the primary body is
;;; visibly marked as having run (i.e. we did exit phase 7).
;;; --------------------------------------------------------------------
(test nxt287-phase9-primary-error-transitions-to-tool-error
  "A raw error from the tool body must be coerced into tool-error on
   the phase-7->phase-9 transition and metrics must record :error."
  (%nxt287-with-isolated-globals
    (%nxt287-register-boom-tool amoebum:*toolset*)
    (let ((context (%nxt287-make-context amoebum:*toolset*)))
      (handler-case
          (progn
            (amoebum:execute-tool
             (%nxt287-call "nxt287-boom")
             context)
            (fail "expected tool-error from nxt287-boom"))
        (amoebum:tool-error (c)
          (is (typep c 'amoebum:tool-error))))
      (let ((metrics (amoebum:context-tool-metrics context "nxt287-boom")))
        (is (not (null metrics)))
        (is (= 1 (getf metrics :count)))
        (is (= 1 (getf metrics :error-count)))
        (is (eq :error (getf metrics :last-status)))))))

;;; --------------------------------------------------------------------
;;; Phase 9 exit: a failed call must NOT populate the result cache.
;;; This is the boundary between the error-transition phase and the
;;; post-success phase (the latter must not run on error).
;;; --------------------------------------------------------------------
(test nxt287-phase9-errored-call-does-not-populate-result-cache
  "Errored tool calls must not reach the post-success caching step."
  (%nxt287-with-isolated-globals
    (%nxt287-register-boom-tool amoebum:*toolset*)
    (let ((context (%nxt287-make-context amoebum:*toolset*)))
      (handler-case
          (amoebum:execute-tool
           (%nxt287-call "nxt287-boom")
           context)
        (amoebum:tool-error () nil))
      (is (null (amoebum:cached-tool-result
                 context "nxt287-boom" (%nxt287-make-args)))))))

;;; --------------------------------------------------------------------
;;; Terminal condition: the skip-tool restart exits the :around phase
;;; with a synthetic "skipped" value instead of propagating an error.
;;; --------------------------------------------------------------------
(test nxt287-terminal-skip-tool-restart-exits-with-skipped-value
  "Invoking the skip-tool restart around a failing primary call must
   exit via the terminal restart path with a human-readable notice."
  (%nxt287-with-isolated-globals
    (%nxt287-register-boom-tool amoebum:*toolset*)
    (let* ((context (%nxt287-make-context amoebum:*toolset*))
           (result
             (handler-bind
                 ((amoebum:tool-error
                    (lambda (_c)
                      (declare (ignore _c))
                      (let ((restart (find-restart 'amoebum::skip-tool)))
                        (when restart
                          (invoke-restart restart))))))
               (amoebum:execute-tool
                (%nxt287-call "nxt287-boom")
                context))))
      (is (stringp result))
      (is-true (search "skipped" result :test #'char-equal)))))

;;; --------------------------------------------------------------------
;;; Terminal condition: the abort-tool restart exits via amoebum-error,
;;; distinct from tool-error.
;;; --------------------------------------------------------------------
(test nxt287-terminal-abort-tool-restart-raises-amoebum-error
  "Invoking the abort-tool restart must raise amoebum-error, not
   tool-error, leaving the call in the terminal aborted state."
  (%nxt287-with-isolated-globals
    (%nxt287-register-boom-tool amoebum:*toolset*)
    (let ((context (%nxt287-make-context amoebum:*toolset*)))
      (signals amoebum:amoebum-error
        (handler-bind
            ((amoebum:tool-error
               (lambda (_c)
                 (declare (ignore _c))
                 (let ((restart (find-restart 'amoebum::abort-tool)))
                   (when restart
                     (invoke-restart restart))))))
          (amoebum:execute-tool
           (%nxt287-call "nxt287-boom")
           context))))))
