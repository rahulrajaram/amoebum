;;;; amoebum.core.api — Backend/frontend boundary API.
;;;; See .agent/backend-frontend-api-design-2026-04-29.md for the design rationale.
;;;; This file is the NXT-600 stub layer: every function delegates to existing
;;;; internal functions. NXT-602..NXT-606 will migrate call sites to use this
;;;; surface; NXT-607 adds the headless-frontend proof test.

(defpackage :amoebum.core.api
  (:use :common-lisp)
  (:export
   ;; Lifecycle
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
   ;; Read-only state
   #:session-conversation
   #:session-toolset
   #:session-awaiting-approvals
   #:session-plan-state
   #:session-token-budget
   ;; Subscription
   #:subscribe-to
   #:unsubscribe))

(in-package :amoebum.core.api)

;;; Session handle — a thin wrapper. NXT-601 will split chat-ui-state
;;; into backend-state / frontend-state and the session will hold the
;;; backend-state. For now it holds the existing chat-ui-state.
(defstruct (session-handle
            (:constructor %make-session-handle))
  (id "" :type string)
  (chat-ui-state nil)
  (created-at 0 :type integer)
  (config nil))

;;; Lifecycle
(defun start-session (&key config initial-conversation)
  "Create a new session. Returns a SESSION-HANDLE."
  (%make-session-handle
   :id (princ-to-string (get-universal-time))
   :chat-ui-state (amoebum::make-chat-ui-state
                   :conversation initial-conversation)
   :created-at (get-universal-time)
   :config config))

(defun end-session (session)
  "Tear down a session. NXT-601+ will close subscriptions, persist state, etc."
  (declare (ignore session))
  t)

(defun session-info (session)
  "Return a frozen plist of session metadata."
  (list :id (session-handle-id session)
        :created-at (session-handle-created-at session)
        :config (session-handle-config session)))

;;; Inbound — frontend -> backend
(defun submit-user-turn (session text)
  "Submit a user message. Publishes +event-type-turn-submitted+ and returns turn-id."
  (let ((turn-id (princ-to-string (get-universal-time))))
    (when (and amoebum::*event-bus*
               (amoebum::event-bus-p amoebum::*event-bus*))
      (amoebum::publish amoebum::*event-bus*
                        amoebum::+event-type-turn-submitted+
                        :payload (list :turn-id turn-id
                                       :text text
                                       :session-id (session-handle-id session))
                        :source :amoebum.core.api))
    ;; NXT-603 will route this to the actual stream runner.
    turn-id))

(defun cancel-current-turn (session)
  "Cancel the in-flight turn. NXT-603 will wire to token-stream-request-cancel."
  (declare (ignore session))
  nil)

(defun resolve-tool-approval (session approval-id decision &key reason)
  "Resolve an awaiting tool-call approval.
DECISION is one of (:approve :reject :always-allow :never-allow).
NXT-604 will route to the actual approval queue."
  (when (and amoebum::*event-bus*
             (amoebum::event-bus-p amoebum::*event-bus*))
    (amoebum::publish amoebum::*event-bus*
                      amoebum::+event-type-approval-resolved+
                      :payload (list :approval-id approval-id
                                     :decision decision
                                     :reason reason
                                     :session-id (session-handle-id session))
                      :source :amoebum.core.api))
  t)

(defun register-runtime-tool (session tool-spec)
  "Register a tool at runtime. TOOL-SPEC is a plist with :name, :description, :handler.
Returns the tool-id. NXT-602 will route to the existing pseudopod:register-tool path used by /deftool."
  (declare (ignore session))
  (getf tool-spec :name))

(defun unregister-runtime-tool (session tool-id)
  "Remove a runtime-registered tool. NXT-602 will route to pseudopod toolset removal."
  (declare (ignore session))
  tool-id)

(defun open-plan-mode (session)
  "Enter plan mode. NXT-605 will wire to amoebum::enter-plan-mode."
  (declare (ignore session))
  t)

(defun close-plan-mode (session)
  "Exit plan mode. NXT-605 will wire to amoebum::exit-plan-mode."
  (declare (ignore session))
  t)

(defun approve-plan-step (session step-index)
  "Approve a plan step by index. NXT-605 will wire to amoebum::approve-plan-steps."
  (declare (ignore session step-index))
  t)

(defun start-plan-execution (session)
  "Begin executing approved plan steps.
NXT-605 will route to amoebum::start-plan-execution from plan-execution-lifecycle.lisp."
  (declare (ignore session))
  t)

(defun execute-slash-command (session text)
  "Execute a slash command. Returns structured command-result data, NOT a display string.
NXT-602 will route to amoebum::dispatch-slash-command (the existing dispatcher)."
  (declare (ignore session text))
  ;; For now: return a stub indicating not-yet-wired.
  (list :status :stub :message "execute-slash-command stub — NXT-602 will wire it"))

(defun load-image (session path)
  "Load a saved SBCL image. NXT-605/+ may wire to amoebum::%image-load-instructions."
  (declare (ignore session path))
  nil)

(defun save-image (session path)
  "Save the SBCL image. WARNING: terminates the process via sb-ext:save-lisp-and-die.
NXT-605/+ will wire to amoebum::save-amoebum-image."
  (declare (ignore session path))
  nil)

;;; Read-only state queries
(defun session-conversation (session)
  "Return the frozen conversation snapshot."
  (and session
       (session-handle-chat-ui-state session)
       (amoebum::chat-ui-state-conversation
        (session-handle-chat-ui-state session))))

(defun session-toolset (session)
  "Return the live toolset list. NXT-601+ will return a frozen snapshot."
  (declare (ignore session))
  (when (boundp 'amoebum::*toolset*)
    (let ((toolset (symbol-value 'amoebum::*toolset*)))
      (cond
        ((listp toolset) (copy-list toolset))
        (t toolset)))))

(defun session-awaiting-approvals (session)
  "Return the list of approvals awaiting resolution.
NXT-604 will return the actual approval queue."
  (declare (ignore session))
  '())

(defun session-plan-state (session)
  "Return the current plan-execution state snapshot, or NIL when plan mode is off.
NXT-605 will return a frozen plan-state instead of the live struct."
  (declare (ignore session))
  (when (fboundp 'amoebum::current-plan-execution-state)
    (ignore-errors
     (funcall (symbol-function 'amoebum::current-plan-execution-state)))))

(defun session-token-budget (session)
  "Return token budget info as a plist."
  (let ((cs (and session (session-handle-chat-ui-state session))))
    (and cs
         (list :used (amoebum::chat-ui-state-context-used-tokens cs)
               :limit (amoebum::chat-ui-state-context-window-limit cs)))))

;;; Subscription
(defun subscribe-to (session event-type handler)
  "Subscribe HANDLER to EVENT-TYPE on the session's event bus.
Returns a subscription-id usable with UNSUBSCRIBE.
NXT-601+ will scope the subscription to session lifecycle."
  (declare (ignore session))
  (when (and amoebum::*event-bus*
             (amoebum::event-bus-p amoebum::*event-bus*)
             (fboundp 'amoebum::subscribe))
    (funcall (symbol-function 'amoebum::subscribe)
             amoebum::*event-bus*
             event-type
             handler)))

(defun unsubscribe (session subscription-id)
  "Cancel a subscription."
  (declare (ignore session))
  (when (and amoebum::*event-bus*
             (amoebum::event-bus-p amoebum::*event-bus*)
             (fboundp 'amoebum::unsubscribe))
    (funcall (symbol-function 'amoebum::unsubscribe)
             amoebum::*event-bus*
             subscription-id)))
