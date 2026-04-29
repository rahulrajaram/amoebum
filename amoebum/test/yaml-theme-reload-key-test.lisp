(in-package :amoebum/test)

;;; NXT-586: Wire the YAML reload key into the chat input event dispatcher.
;;;
;;; This test guards `yaml-theme-reload-key` (default "r"),
;;; `%chat-handle-yaml-reload-key!` (the actual reload action +
;;; toast), and the modal-guard predicate
;;; `%chat-ui-yaml-reload-modal-active-p` defined in
;;; `amoebum/src/ui/chat-input/events.lisp`.
;;;
;;; The modal-guarded `:default` mode dispatch path is intrinsic to the
;;; defpanel `(:mode ...)` macro used by chat-panel.lisp. We exercise the
;;; modal predicate directly here rather than asserting macro shape.

(def-suite yaml-theme-reload-key-suite
  :description
  "NXT-586: YAML reload key dispatch — default key, modal guard, and reload action."
  :in amoebum-suite)

(in-suite yaml-theme-reload-key-suite)

(defmacro %nxt-586-with-clean-yaml-globals (&body body)
  "Save and restore every yaml-theme global the reload helper might mutate.
Required by `amoebum/test/CLAUDE.md` test isolation rules — the reload
helper touches `*yaml-theme-source-path*`, `*yaml-theme-last-modified*`,
`*yaml-layout-loaded*`, and `*yaml-behavior-loaded*` indirectly."
  (let ((saved-source (gensym "SAVED-SOURCE-"))
        (saved-mtime (gensym "SAVED-MTIME-"))
        (saved-layout (gensym "SAVED-LAYOUT-"))
        (saved-behavior (gensym "SAVED-BEHAVIOR-")))
    `(let ((,saved-source amoebum::*yaml-theme-source-path*)
           (,saved-mtime amoebum::*yaml-theme-last-modified*)
           (,saved-layout amoebum::*yaml-layout-loaded*)
           (,saved-behavior amoebum::*yaml-behavior-loaded*))
       (unwind-protect
            (progn ,@body)
         (setf amoebum::*yaml-theme-source-path* ,saved-source
               amoebum::*yaml-theme-last-modified* ,saved-mtime
               amoebum::*yaml-layout-loaded* ,saved-layout
               amoebum::*yaml-behavior-loaded* ,saved-behavior)))))

(test yaml-theme-reload-key-default-is-r
  "yaml-theme-reload-key returns \"r\" when no behavior config is loaded."
  (%nxt-586-with-clean-yaml-globals
    (setf amoebum::*yaml-behavior-loaded* nil)
    (is (string= "r" (amoebum::yaml-theme-reload-key)))))

(test yaml-theme-reload-key-honors-behavior-override
  "When behavior config sets :key-reload, yaml-theme-reload-key returns it."
  (%nxt-586-with-clean-yaml-globals
    (setf amoebum::*yaml-behavior-loaded* (list :key-reload "g"))
    (is (string= "g" (amoebum::yaml-theme-reload-key)))))

(test chat-handle-yaml-reload-no-source-path-is-noop
  "%chat-handle-yaml-reload-key! returns NIL and does not error when no
source path is configured (yaml-theme-needs-reload-p returns nil)."
  (%nxt-586-with-clean-yaml-globals
    (setf amoebum::*yaml-theme-source-path* nil
          amoebum::*yaml-theme-last-modified* nil)
    (let ((chat-state (amoebum::make-chat-ui-state)))
      (is (null (amoebum::%chat-handle-yaml-reload-key! chat-state)))
      ;; The no-change toast is appended as a system message.
      (is (plusp (length (amoebum::chat-ui-state-messages chat-state)))))))

(test yaml-reload-modal-guard-blocks-on-approval
  "%chat-ui-yaml-reload-modal-active-p returns T when the approval dialog
is active, so the reload key won't fire."
  (let* ((chat-state (amoebum::make-chat-ui-state))
         (approval (amoebum::chat-ui-state-approval-dialog-state chat-state)))
    ;; Activate the approval dialog modal so the guard trips.
    (setf (amoebum::approval-dialog-state-active-p approval) t)
    (unwind-protect
         (is-true (amoebum::%chat-ui-yaml-reload-modal-active-p chat-state))
      (setf (amoebum::approval-dialog-state-active-p approval) nil))))

(test yaml-reload-modal-guard-blocks-on-history-search
  "%chat-ui-yaml-reload-modal-active-p trips when history search is active."
  (let ((chat-state (amoebum::make-chat-ui-state)))
    (setf (amoebum::chat-ui-state-history-search-active-p chat-state) t)
    (unwind-protect
         (is-true (amoebum::%chat-ui-yaml-reload-modal-active-p chat-state))
      (setf (amoebum::chat-ui-state-history-search-active-p chat-state) nil))))

(test yaml-reload-modal-guard-allows-default-mode
  "%chat-ui-yaml-reload-modal-active-p returns NIL on a vanilla chat-state
(no approval, no picker, no tree, no history search)."
  (let ((chat-state (amoebum::make-chat-ui-state)))
    (is (null (amoebum::%chat-ui-yaml-reload-modal-active-p chat-state)))))

(test yaml-reload-key-active-requires-empty-input
  "%chat-ui-yaml-reload-key-active-p is NIL when the input prompt is non-empty,
even if the key matches yaml-theme-reload-key — pressing 'r' must NOT
hijack typing."
  (%nxt-586-with-clean-yaml-globals
    (setf amoebum::*yaml-behavior-loaded* nil)
    (let ((chat-state (amoebum::make-chat-ui-state)))
      (setf (amoebum::chat-ui-state-input-text chat-state) "hello")
      (is (null (amoebum::%chat-ui-yaml-reload-key-active-p
                 chat-state :text "r"))))))

(test yaml-reload-key-active-on-empty-input
  "%chat-ui-yaml-reload-key-active-p is T when input is empty and no modal
is active and key matches the configured reload key."
  (%nxt-586-with-clean-yaml-globals
    (setf amoebum::*yaml-behavior-loaded* nil)
    (let ((chat-state (amoebum::make-chat-ui-state)))
      (setf (amoebum::chat-ui-state-input-text chat-state) "")
      (is-true (amoebum::%chat-ui-yaml-reload-key-active-p
                chat-state :text "r")))))

(test yaml-reload-key-active-rejects-non-text-key
  "%chat-ui-yaml-reload-key-active-p only fires for :text key events; a
non-text key (e.g., :enter) never triggers reload."
  (%nxt-586-with-clean-yaml-globals
    (setf amoebum::*yaml-behavior-loaded* nil)
    (let ((chat-state (amoebum::make-chat-ui-state)))
      (is (null (amoebum::%chat-ui-yaml-reload-key-active-p
                 chat-state :enter "r"))))))

(test yaml-reload-key-active-rejects-multi-char-text
  "%chat-ui-yaml-reload-key-active-p rejects multi-character text events
(paste of \"r...\") to avoid hijacking pastes that begin with the reload key."
  (%nxt-586-with-clean-yaml-globals
    (setf amoebum::*yaml-behavior-loaded* nil)
    (let ((chat-state (amoebum::make-chat-ui-state)))
      (is (null (amoebum::%chat-ui-yaml-reload-key-active-p
                 chat-state :text "rr"))))))
