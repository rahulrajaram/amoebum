(in-package :amoebum/test)

;;; NXT-585: Lock in the chat-panel YAML resolver behavior.
;;;
;;; This regression test guards `%chat-panel-fixed-height`,
;;; `%chat-panel-flex-weight`, `%chat-panel-layout-visible-p`, and
;;; `%chat-panel-history-viewport-height` defined in
;;; `amoebum/src/ui/panels/chat-panel.lisp`. It builds a synthetic
;;; `yaml-layout` struct directly (no YAML text parsing) and binds it via
;;; `let` to `*yaml-layout-loaded*` for test isolation.

(def-suite yaml-layout-resolver-suite
  :description
  "NXT-585: chat-panel YAML resolver regression — height/visible/fill overrides."
  :in amoebum-suite)

(in-suite yaml-layout-resolver-suite)

(defun %nxt-585-make-synthetic-layout ()
  "Build a `yaml-layout` with: input-prompt height 7, tree-browser hidden,
history fill, status-bar height 1."
  (amoebum::make-yaml-layout
   :direction :column
   :children
   (list
    (amoebum::make-yaml-layout-child
     :name "input-prompt"
     :height 7)
    (amoebum::make-yaml-layout-child
     :name "tree-browser"
     :visible nil)
    (amoebum::make-yaml-layout-child
     :name "history"
     :height "fill"
     :fill-weight 1.0)
    (amoebum::make-yaml-layout-child
     :name "status-bar"
     :height 1))))

(defmacro %with-synthetic-yaml-layout (&body body)
  "Bind `*yaml-layout-loaded*` to a fresh synthetic layout (see
`%nxt-585-make-synthetic-layout`) for the duration of BODY, restoring the
previous value via `unwind-protect` whether BODY exits normally or via a
non-local exit."
  (let ((saved (gensym "SAVED-LAYOUT-")))
    `(let ((,saved (and (boundp 'amoebum::*yaml-layout-loaded*)
                        amoebum::*yaml-layout-loaded*)))
       (unwind-protect
            (progn
              (setf amoebum::*yaml-layout-loaded*
                    (%nxt-585-make-synthetic-layout))
              ,@body)
         (setf amoebum::*yaml-layout-loaded* ,saved)))))

(test yaml-resolver-fixed-height-override-beats-default
  "When the YAML layout sets input-prompt height: 7, the resolver returns 7
even if the caller supplied default 3."
  (%with-synthetic-yaml-layout
    (is (= 7 (amoebum::%chat-panel-fixed-height :input 3)))))

(test yaml-resolver-fixed-height-default-when-no-yaml
  "With *yaml-layout-loaded* nil, the caller-supplied default still wins."
  (let ((saved (and (boundp 'amoebum::*yaml-layout-loaded*)
                    amoebum::*yaml-layout-loaded*)))
    (unwind-protect
         (progn
           (setf amoebum::*yaml-layout-loaded* nil)
           (is (= 3 (amoebum::%chat-panel-fixed-height :input 3))))
      (setf amoebum::*yaml-layout-loaded* saved))))

(test yaml-resolver-visible-false-overrides-active-flag
  "YAML visible: false on tree-browser hides it even when tree-active-p is t."
  (%with-synthetic-yaml-layout
    (is (null (amoebum::%chat-panel-layout-visible-p :tree t)))))

(test yaml-resolver-visible-false-does-not-force-show
  "When the feature flag is nil, the YAML override does not force-show
the region — visibility is the conjunction of feature flag and YAML."
  (%with-synthetic-yaml-layout
    (is (null (amoebum::%chat-panel-layout-visible-p :tree nil)))))

(test yaml-resolver-flex-weight-default-for-fill
  "history height: fill yields the default fill-weight 1.0 for that region."
  (%with-synthetic-yaml-layout
    (is (= 1.0 (amoebum::%chat-panel-flex-weight :history 1)))))

(test yaml-resolver-history-viewport-respects-overrides
  "Composite check: with input-prompt=7, status=1, history=fill, and
tree hidden via visible:false, the history region gets
inner-height (50) - input (7) - status (1) = 42 rows. The tree must
contribute zero rows even when tree-active-p is t, because the YAML
visible:false override hides it."
  (%with-synthetic-yaml-layout
    (let ((history-rows
            (amoebum::%chat-panel-history-viewport-height
             50
             :provider-active-p nil
             :tree-active-p t
             :plan-active-p nil
             :handoff-visible-p nil
             :approval-active-p nil
             :picker-active-p nil)))
      (is (= (- 50 7 1) history-rows)))))
