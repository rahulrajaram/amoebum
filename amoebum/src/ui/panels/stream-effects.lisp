;;; I302: stream-effects-panel
;;; Extracts stream-related side effects and the stream-stop-hint widget.
(in-package :amoebum)

(ptui.ui.panel:defpanel stream-effects-panel (chat-state)
  (:effects
    (drain-streams (%drain-stream-events chat-state)
      :deps (chat-state))
    (publish-summary (%publish-status-bar-stream-summary-if-needed chat-state)
      :deps (chat-state))
    (budget-warning (%emit-stream-budget-warning-if-needed chat-state)
      :deps (chat-state)))
  (:layout
    (:column
      (hint :fixed 1 :when (token-stream-active-p
                             (chat-ui-state-stream-state chat-state))
        (%chat-text-widget "Streaming... Press Ctrl-C to stop early."
                           :chat-stream-stop-hint
                           :meta)))))
