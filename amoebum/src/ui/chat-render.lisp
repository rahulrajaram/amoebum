(in-package :amoebum)

;;; Render-oriented chat helpers extracted mechanically from chat.lisp.
;;; Post NXT-545 the residual is a thin facade over five sibling modules
;;; under src/ui/chat-render/:
;;;   - widgets.lisp        (cell/widget primitives, role-cell, style cache,
;;;                          styled-segment normalization, approval dialog)
;;;   - scrollback.lisp     (viewport offset clamp, chat-tree id normalize)
;;;   - transcript.lisp     (transcript cache + message-entry assembly)
;;;   - stream-overlays.lisp (streaming/thinking overlay composition)
;;;   - layout.lisp         (plan-mode workspace data shaping + tree key)
;;;
;;; This file owns only the build/finalize/render glue invoked once per
;;; frame. Scrollback semantics, transcript fan-out, and approval dialog
;;; behavior remain identical to the pre-split implementation.

(defun %compose-chat-ui-tree (chat-state cols rows)
  (let ((runtime (chat-ui-state-runtime chat-state)))
    (let ((ptui.ui.runtime:*current-runtime* runtime))
      (render-chat-panel chat-state cols rows))))

(defun %finalize-chat-ui-tree! (chat-state tree)
  (clrhash *%style-resolve-cache*)
  (%normalize-chat-tree-ids! tree)
  (ptui.ui.runtime:update-runtime (chat-ui-state-runtime chat-state) tree)
  tree)

(defun chat-ui-build-tree (state cols rows)
  (let* ((chat-state (ensure-chat-ui-state state))
         (tree (%compose-chat-ui-tree chat-state cols rows)))
    (%finalize-chat-ui-tree! chat-state tree)))

(defun %render-chat-ui-tree-to-buffer (tree size)
  (%normalize-tree-styled-segments! tree)
  (ptui.ui.app::%render-tree-to-buffer tree size))

(defun render-chat-ui-buffer (state size)
  (labels ((render-once ()
             (let* ((chat-state (ensure-chat-ui-state state))
                    (cols (ptui.core.types:size-cols size))
                    (rows (ptui.core.types:size-rows size))
                    (frame-start-ms (%usdt-now-ms)))
               (%sync-all-state! chat-state)
               (maybe-auto-checkpoint
                :conversation (%ensure-chat-conversation-state chat-state)
                :config (%chat-config)
                :busy-p (token-stream-active-p (chat-ui-state-stream-state chat-state)))
               ;; Fingerprint cache disabled — buffer reuse can mutate cached buffer.

               (incf (chat-ui-state-frame-count chat-state))
               (let* ((frame-index (chat-ui-state-frame-count chat-state))
                      (tree (chat-ui-build-tree chat-state cols rows))
                      (buffer (%render-chat-ui-tree-to-buffer tree size)))
                 (usdt-probe-render-frame frame-index
                                          (max 0 (- (%usdt-now-ms) frame-start-ms))
                                          cols
                                          rows)
                 buffer))))
    (let* ((chat-state (ensure-chat-ui-state state))
           (approval-recovery-p (%approval-recovery-active-p chat-state)))
      (handler-case
          (render-once)
        (error (condition)
          (if approval-recovery-p
              (progn
                (%handle-approval-ui-error! chat-state :render-cycle condition)
                (render-once))
              (error condition)))))))
