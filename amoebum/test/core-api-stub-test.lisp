(in-package :amoebum/test)

;;;; NXT-600: amoebum.core.api stub-layer regression suite.
;;;;
;;;; Guards the new backend/frontend API boundary surface introduced in
;;;; amoebum/src/core/api.lisp and amoebum/src/core/api-events.lisp.
;;;;
;;;; These tests assert:
;;;;   1. Every NXT-600 API symbol is exported from :amoebum.core.api.
;;;;   2. Lifecycle (start/end/info) returns sensible defaults.
;;;;   3. Inbound entry points that publish events do so on the bound
;;;;      *event-bus* with the expected event type and payload shape.
;;;;   4. The 7 new event-type constants are registered in
;;;;      amoebum::+core-event-types+.
;;;;   5. Read-only state queries return without error on a fresh session.
;;;;
;;;; Test isolation: every mutation of *event-bus* runs inside an
;;;; unwind-protect that restores the previous binding (per
;;;; amoebum/test/CLAUDE.md and the NXT-587 yaml-theme-file-watcher pattern).

(def-suite core-api-stub-suite
  :description
  "NXT-600: amoebum.core.api stub layer — exports, lifecycle, event publishing."
  :in amoebum-suite)

(in-suite core-api-stub-suite)

(defparameter +nxt-600-api-symbols+
  '(;; Lifecycle
    #:start-session
    #:end-session
    #:session-info
    #:session-handle-p
    ;; Inbound
    #:submit-user-turn
    #:cancel-current-turn
    #:resolve-tool-approval
    #:register-runtime-tool
    #:unregister-runtime-tool
    #:open-plan-mode
    #:close-plan-mode
    #:approve-plan-step
    #:start-plan-execution
    #:execute-slash-command
    #:load-image
    #:save-image
    ;; Read-only
    #:session-conversation
    #:session-toolset
    #:session-awaiting-approvals
    #:session-plan-state
    #:session-token-budget
    ;; Subscription
    #:subscribe-to
    #:unsubscribe))

(defparameter +nxt-600-new-event-type-constant-names+
  '("+EVENT-TYPE-TURN-SUBMITTED+"
    "+EVENT-TYPE-TURN-ASSISTANT-MESSAGE+"
    "+EVENT-TYPE-APPROVAL-AWAITING+"
    "+EVENT-TYPE-APPROVAL-RESOLVED+"
    "+EVENT-TYPE-CONVERSATION-SNAPSHOT+"
    "+EVENT-TYPE-PLAN-STATE-SNAPSHOT+"
    "+EVENT-TYPE-BACKEND-ERROR+"))

(defmacro %nxt-600-with-clean-event-bus (&body body)
  "Save and restore amoebum::*event-bus* around BODY. Mirrors the
NXT-587 yaml-theme-file-watcher pattern."
  (let ((saved-bus (gensym "SAVED-BUS-")))
    `(let ((,saved-bus amoebum::*event-bus*))
       (unwind-protect
            (progn ,@body)
         (setf amoebum::*event-bus* ,saved-bus)))))

;;; (a) Package exports the surface.
(test core-api-package-exports-the-surface
  "Every NXT-600 API symbol is interned in :amoebum.core.api and external."
  (let ((package (find-package :amoebum.core.api)))
    (is (not (null package))
        "Package :amoebum.core.api must exist after NXT-600 lands.")
    (when package
      (dolist (name +nxt-600-api-symbols+)
        (multiple-value-bind (symbol status)
            (find-symbol (symbol-name name) package)
          (is (not (null symbol))
              "Symbol ~A must be interned in :amoebum.core.api." name)
          (is (eq status :external)
              "Symbol ~A must be EXTERNAL in :amoebum.core.api (status=~S)."
              name status))))))

;;; (b) start-session returns a handle.
(test core-api-start-session-returns-handle
  "start-session returns a non-nil session-handle that satisfies session-handle-p."
  (let ((handle (amoebum.core.api:start-session)))
    (is (not (null handle)))
    (is-true (amoebum.core.api:session-handle-p handle))))

;;; (c) session-info round-trips :config.
(test core-api-session-info-roundtrips-config
  "session-info exposes the :config plist passed to start-session."
  (let* ((handle (amoebum.core.api:start-session :config '(:demo t)))
         (info (amoebum.core.api:session-info handle)))
    (is (listp info))
    (is (equal '(:demo t) (getf info :config)))
    (is (stringp (getf info :id)))
    (is (integerp (getf info :created-at)))))

;;; (d) submit-user-turn publishes the event.
(test core-api-submit-user-turn-publishes-event
  "submit-user-turn fires +event-type-turn-submitted+ exactly once with payload
containing the submitted text."
  (%nxt-600-with-clean-event-bus
    (let* ((bus (amoebum::make-event-bus))
           (received '())
           (handle (amoebum.core.api:start-session)))
      (setf amoebum::*event-bus* bus)
      (amoebum::subscribe bus
                          amoebum::+event-type-turn-submitted+
                          (lambda (event) (push event received)))
      (let ((turn-id (amoebum.core.api:submit-user-turn handle "hello")))
        (is (stringp turn-id))
        (is (= 1 (length received)))
        (let* ((event (first received))
               (payload (amoebum::event-payload event)))
          (is (eq amoebum::+event-type-turn-submitted+
                  (amoebum::event-type event)))
          (is (equal "hello" (getf payload :text)))
          (is (equal turn-id (getf payload :turn-id))))))))

;;; (e) resolve-tool-approval publishes.
(test core-api-resolve-tool-approval-publishes-event
  "resolve-tool-approval fires +event-type-approval-resolved+ with the decision."
  (%nxt-600-with-clean-event-bus
    (let* ((bus (amoebum::make-event-bus))
           (received '())
           (handle (amoebum.core.api:start-session)))
      (setf amoebum::*event-bus* bus)
      (amoebum::subscribe bus
                          amoebum::+event-type-approval-resolved+
                          (lambda (event) (push event received)))
      (amoebum.core.api:resolve-tool-approval handle "approval-1" :approve)
      (is (= 1 (length received)))
      (let* ((event (first received))
             (payload (amoebum::event-payload event)))
        (is (eq amoebum::+event-type-approval-resolved+
                (amoebum::event-type event)))
        (is (equal "approval-1" (getf payload :approval-id)))
        (is (eq :approve (getf payload :decision)))))))

;;; (f) All 7 new event types are in +core-event-types+.
(test core-api-new-event-types-registered-in-core-registry
  "Each of the 7 NXT-600 event-type constants is interned in :amoebum AND
present in amoebum::+core-event-types+."
  (let ((registry amoebum::+core-event-types+))
    (dolist (name +nxt-600-new-event-type-constant-names+)
      (let ((symbol (find-symbol name :amoebum)))
        (is (not (null symbol))
            "Symbol ~A must be interned in :amoebum." name)
        (when (and symbol (boundp symbol))
          (let ((value (symbol-value symbol)))
            (is (find value registry :test #'eq)
                "Constant ~A (= ~S) must appear in amoebum::+core-event-types+."
                name value)))))))

;;; (g) Read-only queries on a fresh session do not error.
(test core-api-readonly-queries-do-not-error
  "session-conversation, session-token-budget, session-toolset,
session-awaiting-approvals, and session-plan-state must not error
on a fresh session-handle (values may be nil for the stub layer)."
  (let ((handle (amoebum.core.api:start-session)))
    (finishes (amoebum.core.api:session-conversation handle))
    (finishes (amoebum.core.api:session-token-budget handle))
    (finishes (amoebum.core.api:session-toolset handle))
    (finishes (amoebum.core.api:session-awaiting-approvals handle))
    (finishes (amoebum.core.api:session-plan-state handle))
    ;; session-token-budget on a real handle returns a plist with :used and :limit
    (let ((budget (amoebum.core.api:session-token-budget handle)))
      (when budget
        (is (integerp (getf budget :used)))
        (is (integerp (getf budget :limit)))))))
