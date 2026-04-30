(in-package :amoebum/test)

;;; NXT-588: Lock in the chat-panel YAML padding + focus resolver behavior.
;;;
;;; Guards the helpers added to `amoebum/src/ui/panels/chat-panel.lisp`:
;;;   * `%chat-panel-padding-spec`        — read YAML padding shorthand
;;;   * `%chat-panel-region-focusable-p`  — read YAML focusable flag
;;;   * `%chat-panel-region-focus-order`  — read YAML focus-order int
;;;   * `%chat-panel-wrap-padding`        — wrap a widget with PTUI's
;;;                                          make-box-widget when padding
;;;                                          is non-zero, and re-tag it
;;;                                          focusable when YAML asks
;;;
;;; Mirrors the synthetic-yaml-layout pattern from
;;; `yaml-layout-resolver-test.lisp` (NXT-585): build a `yaml-layout`
;;; struct directly, bind via `let` to `*yaml-layout-loaded*`, restore
;;; on exit. No YAML text parsing.

(def-suite yaml-layout-padding-focus-suite
  :description
  "NXT-588: chat-panel padding + focus resolver — wraps via PTUI box widget,
focusable / focus-order pass-through."
  :in amoebum-suite)

(in-suite yaml-layout-padding-focus-suite)

(defun %nxt-588-make-padded-layout ()
  "Build a `yaml-layout` with: input padding [1 2 1 2], focusable t,
focus-order 1; history padding [0 0 0 0], focusable t, focus-order 2;
status with no padding/focusable hints."
  (amoebum::make-yaml-layout
   :direction :column
   :children
   (list
    (amoebum::make-yaml-layout-child
     :name "input-prompt"
     :padding '(1 2 1 2)
     :focusable t
     :focus-order 1)
    (amoebum::make-yaml-layout-child
     :name "history"
     :padding '(0 0 0 0)
     :focusable t
     :focus-order 2)
    (amoebum::make-yaml-layout-child
     :name "status-bar"))))

(defmacro %with-padded-yaml-layout (&body body)
  "Bind `*yaml-layout-loaded*` to a fresh padded synthetic layout for BODY,
restoring the previous binding via `unwind-protect`."
  (let ((saved (gensym "SAVED-LAYOUT-")))
    `(let ((,saved (and (boundp 'amoebum::*yaml-layout-loaded*)
                        amoebum::*yaml-layout-loaded*)))
       (unwind-protect
            (progn
              (setf amoebum::*yaml-layout-loaded*
                    (%nxt-588-make-padded-layout))
              ,@body)
         (setf amoebum::*yaml-layout-loaded* ,saved)))))

(test padding-spec-returns-nil-for-empty-padding
  "history child has padding [0 0 0 0]; resolver treats all-zero as 'no wrap'
and returns NIL so callers skip the wrapper."
  (%with-padded-yaml-layout
    (is (null (amoebum::%chat-panel-padding-spec :history)))))

(test padding-spec-returns-list-for-non-zero-padding
  "input child has padding [1 2 1 2]; resolver returns the normalized
4-tuple so callers can pass it straight to make-box-widget."
  (%with-padded-yaml-layout
    (let ((spec (amoebum::%chat-panel-padding-spec :input)))
      (is (equal '(1 2 1 2) spec)))))

(test padding-spec-returns-nil-when-no-yaml
  "Without *yaml-layout-loaded*, every region resolves to NIL — the
unwrapped pass-through path that preserves the snapshot baseline."
  (let ((saved (and (boundp 'amoebum::*yaml-layout-loaded*)
                    amoebum::*yaml-layout-loaded*)))
    (unwind-protect
         (progn
           (setf amoebum::*yaml-layout-loaded* nil)
           (is (null (amoebum::%chat-panel-padding-spec :input))))
      (setf amoebum::*yaml-layout-loaded* saved))))

(test wrap-padding-passes-through-when-no-padding
  "When YAML padding spec is NIL and focusable is NIL, %chat-panel-wrap-padding
returns its input widget unchanged (EQ identity, no allocation)."
  (let ((saved (and (boundp 'amoebum::*yaml-layout-loaded*)
                    amoebum::*yaml-layout-loaded*))
        (sentinel (ptui.widgets.core:make-text-widget "sentinel")))
    (unwind-protect
         (progn
           (setf amoebum::*yaml-layout-loaded* nil)
           (is (eq sentinel
                   (amoebum::%chat-panel-wrap-padding sentinel :input))))
      (setf amoebum::*yaml-layout-loaded* saved))))

(test wrap-padding-returns-box-widget-when-padding-set
  "When YAML padding is non-zero, %chat-panel-wrap-padding returns a PTUI box
element (type :box) whose only child is the original widget."
  (%with-padded-yaml-layout
    (let* ((inner (ptui.widgets.core:make-text-widget "inner"))
           (wrapped (amoebum::%chat-panel-wrap-padding inner :input)))
      (is (eq :box (ptui.ui.elements:ui-element-type wrapped)))
      (is (eq inner
              (first (ptui.ui.elements:ui-element-children wrapped)))))))

(test focusable-resolver-returns-t-when-yaml-declares-it
  "input child has focusable: t; resolver returns T."
  (%with-padded-yaml-layout
    (is-true (amoebum::%chat-panel-region-focusable-p :input))))

(test focusable-resolver-returns-nil-without-yaml
  "Without YAML, focusable defaults to NIL (regions are not in the focus
cycle unless explicitly opted in)."
  (let ((saved (and (boundp 'amoebum::*yaml-layout-loaded*)
                    amoebum::*yaml-layout-loaded*)))
    (unwind-protect
         (progn
           (setf amoebum::*yaml-layout-loaded* nil)
           (is (null (amoebum::%chat-panel-region-focusable-p :input))))
      (setf amoebum::*yaml-layout-loaded* saved))))

(test focus-order-resolver-returns-integer
  "input child has focus-order: 1; history has focus-order: 2."
  (%with-padded-yaml-layout
    (is (= 1 (amoebum::%chat-panel-region-focus-order :input)))
    (is (= 2 (amoebum::%chat-panel-region-focus-order :history)))))

(test focus-order-resolver-returns-nil-without-hint
  "status-bar declares no focus-order; resolver returns NIL."
  (%with-padded-yaml-layout
    (is (null (amoebum::%chat-panel-region-focus-order :status)))))

(test wrap-padding-marks-focusable-when-yaml-says-so
  "When YAML declares focusable: t and padding is non-zero, the box wrapper
must propagate :focusablep t so PTUI's collect-focus-order picks it up.
make-box-widget does not accept a :focusablep initarg directly
(see ptui/src/widgets/core.lisp:88), so the helper re-tags via
ptui.ui.elements:make-element."
  (%with-padded-yaml-layout
    (let* ((inner (ptui.widgets.core:make-text-widget "inner"))
           (wrapped (amoebum::%chat-panel-wrap-padding inner :input)))
      (is-true (ptui.ui.elements:ui-element-focusablep wrapped)))))

(test wrap-padding-marks-focusable-without-padding
  "When focusable: t but no padding, the helper still marks the underlying
widget focusable (returns a re-tagged copy, not the unwrapped widget)."
  (let ((saved (and (boundp 'amoebum::*yaml-layout-loaded*)
                    amoebum::*yaml-layout-loaded*)))
    (unwind-protect
         (progn
           (setf amoebum::*yaml-layout-loaded*
                 (amoebum::make-yaml-layout
                  :direction :column
                  :children
                  (list (amoebum::make-yaml-layout-child
                         :name "history"
                         :focusable t))))
           (let* ((inner (ptui.widgets.core:make-text-widget "inner"))
                  (wrapped (amoebum::%chat-panel-wrap-padding inner :history)))
             (is-true (ptui.ui.elements:ui-element-focusablep wrapped))))
      (setf amoebum::*yaml-layout-loaded* saved))))

;;; Note (NXT-588): we deliberately do not unit-test the runtime Tab
;;; dispatch here. PTUI's `ptui.ui.runtime:route-event` already cycles
;;; focus on `:tab` (see runtime.lisp:314-318) when collect-focus-order
;;; finds focusable nodes; the unit coverage above locks in that the
;;; right widgets get :focusablep set, which is the integration contract.
;;; Mocking a full runtime + focus collection round-trip would test PTUI,
;;; not amoebum's resolver.
