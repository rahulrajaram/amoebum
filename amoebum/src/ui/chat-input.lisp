;;; NXT-281: input-oriented chat helpers extracted mechanically from chat.lisp.
;;; Keep original behavior and call sites stable while narrowing file ownership.
;;;
;;; NXT-543: this file has been further split into four submodules under
;;; src/ui/chat-input/:
;;;   * state.lisp     -- input-text mutation, grapheme/cluster helpers,
;;;                       parsed input content parts, and string predicates
;;;   * render.lisp    -- wrapped-line geometry, cursor display
;;;                       coordinates, prompt scroll window, blink phase
;;;   * history.lisp   -- fuzzy-picker / history-picker selection apply
;;;   * events.lisp    -- slash-command actions, plan-mode entry detection,
;;;                       key-handler tables, page-scroll routing,
;;;                       approval-dialog interception, vertical cursor
;;;                       move, agent/voice injection, unrouted-key
;;;                       fallback, and the runtime key-event dispatcher
;;;
;;; This residual file owns only the public chat-input facade: the
;;; top-level handle-chat-ui-event entry point that the chat panel and
;;; smoke tests call. Slash-command dispatch, autocomplete, multi-line
;;; input, paste handling, and keybinding semantics remain byte-for-byte
;;; stable at the observable API level.

(in-package :amoebum)

(defun handle-chat-ui-event (state event)
  (let* ((chat-state (ensure-chat-ui-state state))
         (runtime (chat-ui-state-runtime chat-state))
         (route (%chat-ui-route runtime event))
         (outcome nil))
    ;; Sync approval dialog state so key routing can intercept for active dialogs.
    ;; All other sync happens in %sync-all-state! during render.
    (%sync-pending-approval-dialog! chat-state)
    (when (typep event 'ptui.core.events:key-event)
      (setf outcome (%chat-ui-handle-key-event chat-state runtime event route)))
    (unless outcome
      (%chat-ui-dispatch-routed-event chat-state runtime route))
    (when (%chat-ui-text-q-consumed-p event outcome)
      ;; Amoebum owns quit semantics now; a bare "q" should route normally and
      ;; never fall through to PTUI's legacy default quit binding.
      (setf outcome :consume))
    (values chat-state
            outcome)))
