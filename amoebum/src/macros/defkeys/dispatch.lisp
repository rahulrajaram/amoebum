(in-package :amoebum)

;;;; ---------------------------------------------------------------------------
;;;; Keymap dispatch pipeline.
;;;;
;;;; This is the runtime path that turns a `ptui.core.events:key-event` into a
;;;; state transition across the currently active keymap stack (or overlay):
;;;; chord matching, per-keymap binding dispatch, pending-escape /
;;;; alt-modifier disambiguation, sequence-buffer timeouts, and the
;;;; `on-event` callback factory consumed by the TUI event loop.
;;;;
;;;; Depends on parser.lisp (signatures + chord match helpers) and
;;;; registry.lisp (stack / overlay / sequence-buffer / pending-escape
;;;; mutators).
;;;;
;;;; Behavior is preserved verbatim from the original
;;;; `amoebum/src/macros/defkeys.lisp`; only file boundaries change.
;;;; ---------------------------------------------------------------------------

(defun %active-keymaps (&optional keymap-stack)
  (if (not (null *keymap-overlay-stack*))
      (mapcar #'keymap-overlay-keymap *keymap-overlay-stack*)
      (let ((stack (or keymap-stack *keymap-stack*)))
        (if (null stack)
            (let ((chat (find-keymap 'chat-mode)))
              (if chat
                  (list chat)
                  '()))
            stack))))

(defun %max-chord-length (&optional keymaps)
  (let ((max-length 1))
    (dolist (keymap keymaps max-length)
      (dolist (chord (keymap-chords keymap))
        (setf max-length
              (max max-length (length (key-chord-sequence chord))))))))

(defun %dispatch-chords (keymap key-event state)
  (let ((pending nil))
    (dolist (chord (keymap-chords keymap))
      (let* ((sequence (key-chord-sequence chord))
             (guard (key-chord-guard chord))
             (guard-passed (or (null guard)
                               (not (null (funcall guard state key-event))))))
        (when guard-passed
          (when (%sequence-suffix-equal-p *key-sequence-buffer* sequence)
            (%reset-key-sequence-buffer)
            (return-from %dispatch-chords
              (values (%invoke-key-handler (key-chord-handler chord)
                                           state
                                           key-event)
                      t
                      :chord)))
          (when (%sequence-prefix-length *key-sequence-buffer* sequence)
            (setf pending t)))))
    (if pending
        (values state t :pending)
        (values state nil :none))))

(defun dispatch-key-event (keymap key-event state &key (append-sequence t))
  (let* ((map (%keymap-from-designator keymap))
         (signature (%key-event-signature key-event)))
    (unless signature
      (return-from dispatch-key-event (values state nil :none)))
    (when append-sequence
      (%append-key-signature signature)
      (%trim-key-sequence-buffer (%max-chord-length (list map))))
    (multiple-value-bind (post-chord-state chord-handledp chord-kind)
        (%dispatch-chords map key-event state)
      (when chord-handledp
        (return-from dispatch-key-event
          (values post-chord-state t chord-kind)))
      (let ((binding (gethash signature (keymap-bindings map))))
        (if (null binding)
            (progn
              (when append-sequence
                (%reset-key-sequence-buffer))
              (values state nil :none))
            (let ((guard (key-binding-guard binding)))
              (if (or (null guard)
                      (not (null (funcall guard state key-event))))
                  (progn
                    (%reset-key-sequence-buffer)
                    (values (%invoke-key-handler (key-binding-handler binding)
                                                 state
                                                 key-event)
                            t
                            :binding))
                  (progn
                    (when append-sequence
                      (%reset-key-sequence-buffer))
                    (values state nil :guard-skipped)))))))))

(defun %make-alt-modified-key-event (event)
  (ptui.core.events:make-key-event (ptui.core.events:key-event-key event)
                                   :ctrlp (ptui.core.events:key-event-ctrlp event)
                                   :altp t
                                   :shiftp (ptui.core.events:key-event-shiftp event)
                                   :text? (ptui.core.events:key-event-text? event)))

(defun %dispatch-key-event-to-active-maps (key-event state &key keymap-stack)
  (let* ((keymaps (%active-keymaps keymap-stack))
         (signature (%key-event-signature key-event)))
    (unless signature
      (return-from %dispatch-key-event-to-active-maps (values state nil nil)))
    (%append-key-signature signature)
    (%trim-key-sequence-buffer (%max-chord-length keymaps))
    (let ((current-state state))
      (dolist (keymap keymaps (values state nil nil))
        (multiple-value-bind (next-state handledp kind)
            (dispatch-key-event keymap key-event current-state :append-sequence nil)
          (setf current-state next-state)
          (when handledp
            (return-from %dispatch-key-event-to-active-maps
              (values current-state t
                      (list :keymap (keymap-name keymap)
                            :kind kind))))))
      (let ((overlay (current-keymap-overlay)))
        (if (and overlay
                 (%plain-escape-signature-p signature)
                 (keymap-overlay-auto-cleanup-on-escape-p overlay))
            (progn
              (pop-keymap-overlay :reason :escape)
              (values state t
                      (list :keymap (keymap-name (keymap-overlay-keymap overlay))
                            :kind :overlay-exit)))
            (progn
              (%reset-key-sequence-buffer)
              (values state nil nil)))))))

(defun flush-key-dispatch-timeouts (state &key keymap-stack (force nil))
  (%expire-key-sequence-buffer)
  (if (and (%pending-escape-active-p)
           (or force (%pending-escape-expired-p)))
      (let ((pending *pending-escape-event*))
        (%clear-pending-escape)
        (%dispatch-key-event-to-active-maps pending state :keymap-stack keymap-stack))
      (values state nil nil)))

(defun dispatch-active-keymaps (key-event state &key keymap-stack)
  (unless (typep key-event 'ptui.core.events:key-event)
    (return-from dispatch-active-keymaps (values state nil nil)))
  (%expire-key-sequence-buffer)
  (multiple-value-bind (post-timeout-state timeout-handledp timeout-metadata)
      (flush-key-dispatch-timeouts state :keymap-stack keymap-stack)
    (let* ((signature (%key-event-signature key-event))
           (current-state post-timeout-state))
      (unless signature
        (return-from dispatch-active-keymaps
          (values current-state timeout-handledp timeout-metadata)))
      (when (%pending-escape-active-p)
        (if (and (not (%pending-escape-expired-p))
                 (not (%plain-escape-signature-p signature)))
            (progn
              (setf key-event (%make-alt-modified-key-event key-event)
                    signature (%key-event-signature key-event))
              (%clear-pending-escape))
            (progn
              (multiple-value-bind (post-escape-state escape-handledp escape-metadata)
                  (%dispatch-key-event-to-active-maps
                   *pending-escape-event*
                   current-state
                   :keymap-stack keymap-stack)
                (setf current-state post-escape-state
                      timeout-handledp escape-handledp
                      timeout-metadata escape-metadata))
              (%clear-pending-escape))))
      (if (%plain-escape-signature-p signature)
          (progn
            (%set-pending-escape key-event)
            (values current-state t
                    (or timeout-metadata
                        (list :kind :pending-escape))))
          (multiple-value-bind (next-state handledp metadata)
              (%dispatch-key-event-to-active-maps key-event current-state :keymap-stack keymap-stack)
            (if handledp
                (values next-state t metadata)
                (values next-state timeout-handledp timeout-metadata)))))))

(defun make-key-dispatch-on-event (&key
                                     (fallback (lambda (state event)
                                                 (declare (ignore event))
                                                 state))
                                     (keymap-stack-fn (lambda () *keymap-stack*)))
  (unless (functionp fallback)
    (error "Fallback must be a function, got ~S." fallback))
  (unless (functionp keymap-stack-fn)
    (error "KEYMAP-STACK-FN must be a function, got ~S." keymap-stack-fn))
  (lambda (state event)
    (let ((stack (funcall keymap-stack-fn)))
      (multiple-value-bind (state-after-timeouts)
          (flush-key-dispatch-timeouts state :keymap-stack stack)
        (if (typep event 'ptui.core.events:key-event)
            (multiple-value-bind (next-state handledp)
                (dispatch-active-keymaps event state-after-timeouts :keymap-stack stack)
              (if handledp
                  next-state
                  (funcall fallback next-state event)))
            (funcall fallback state-after-timeouts event))))))
