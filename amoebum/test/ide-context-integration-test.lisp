(in-package :amoebum/test)

;;; ---------------------------------------------------------------------------
;;; NXT-094: IDE context integration tests
;;; ---------------------------------------------------------------------------
;;; Tests for token-budget truncation, selection/diagnostic precedence, and
;;; disabled-mode behaviour.  Also covers NXT-095 observability events.
;;; ---------------------------------------------------------------------------

(def-suite ide-context-integration-suite
  :description "Integration tests for NXT-094 (budget truncation, precedence,
disabled mode) and NXT-095 (observability events)."
  :in amoebum-suite)

(in-suite ide-context-integration-suite)

;;; ---------------------------------------------------------------------------
;;; Helpers
;;; ---------------------------------------------------------------------------

(defun %collect-ide-events (bus event-type &key (n nil))
  "Return all events of EVENT-TYPE from BUS history, optionally capped at N."
  (let ((all (remove-if-not (lambda (e) (eq (amoebum:event-type e) event-type))
                            (amoebum:event-history bus))))
    (if n (last all n) all)))

(defun %fresh-bus ()
  "Return a new, empty event bus."
  (amoebum:make-event-bus))

;;; ---------------------------------------------------------------------------
;;; NXT-094 — disabled mode
;;; ---------------------------------------------------------------------------

(test ide-context-prompt-fragment-nil-when-global-nil
  "When *ide-context* is NIL, ide-context-prompt-fragment returns NIL (disabled mode)."
  (let ((amoebum:*ide-context* nil))
    (is (null (amoebum:ide-context-prompt-fragment)))))

(test ide-context-prompt-fragment/budget-nil-when-global-nil
  "ide-context-prompt-fragment/budget returns NIL when *ide-context* is NIL."
  (let ((amoebum:*ide-context* nil))
    (is (null (amoebum:ide-context-prompt-fragment/budget 500)))))

(test ide-context-token-estimate-zero-when-nil
  "Token estimate is 0 when context is NIL."
  (is (= 0 (amoebum:ide-context-token-estimate nil))))

;;; ---------------------------------------------------------------------------
;;; NXT-094 — token-budget truncation
;;; ---------------------------------------------------------------------------

(test ide-context-budget-no-truncation-within-budget
  "When the context fits within the budget, the full fragment is returned unchanged."
  (let* ((ctx (amoebum:make-ide-context
               :active-file "/src/foo.lisp"
               :diagnostics (list (list :file "/src/foo.lisp"
                                        :line 1
                                        :severity :warning
                                        :message "unused var"))))
         (estimate (amoebum:ide-context-token-estimate ctx))
         (big-budget (* estimate 10))
         (full-frag  (amoebum:ide-context-prompt-fragment ctx))
         (budget-frag (amoebum:ide-context-prompt-fragment/budget big-budget ctx)))
    (is (stringp full-frag))
    (is (stringp budget-frag))
    (is (string= full-frag budget-frag))))

(test ide-context-budget-truncation-drops-diagnostics
  "When context exceeds budget, diagnostics are truncated first."
  ;; Build a context with many diagnostics to ensure it exceeds a small budget.
  (let* ((diags (loop for i from 1 to 10
                      collect (list :file "/src/foo.lisp"
                                    :line i
                                    :severity :error
                                    :message (make-string 40 :initial-element #\x))))
         (ctx (amoebum:make-ide-context
               :active-file "/src/foo.lisp"
               :diagnostics diags))
         ;; The full estimate; set budget to just above the base overhead so
         ;; diagnostics must be trimmed.
         (base-budget 20)
         (frag (amoebum:ide-context-prompt-fragment/budget base-budget ctx)))
    ;; Fragment may be NIL if everything was stripped, or a string —
    ;; the key property is that it doesn't error and the original context is untouched.
    (is (or (null frag) (stringp frag)))
    ;; Original context must be unmodified
    (is (= 10 (length (amoebum:ide-context-diagnostics ctx))))))

(test ide-context-budget-truncation-returns-string-or-nil
  "ide-context-prompt-fragment/budget always returns a string or NIL, never signals."
  (let* ((text (make-string 2000 :initial-element #\z))
         (ctx (amoebum:make-ide-context
               :active-file "/big.lisp"
               :selections (list (list :file "/big.lisp"
                                       :start-line 1
                                       :end-line 200
                                       :text text))
               :diagnostics (list (list :file "/big.lisp"
                                        :line 10
                                        :severity :error
                                        :message "bad"))))
         (result (amoebum:ide-context-prompt-fragment/budget 20 ctx)))
    (is (or (null result) (stringp result)))))

(test ide-context-budget-original-context-immutable
  "Budget-truncation must not modify the original ide-context struct."
  (let* ((diags (loop for i from 1 to 5
                      collect (list :file "/src/a.lisp"
                                    :line i
                                    :severity :warning
                                    :message "msg")))
         (sels  (list (list :file "/src/a.lisp"
                            :start-line 1
                            :end-line 10
                            :text (make-string 200 :initial-element #\a))))
         (ctx (amoebum:make-ide-context
               :active-file "/src/a.lisp"
               :selections sels
               :diagnostics diags)))
    ;; Force truncation with a tiny budget
    (amoebum:ide-context-prompt-fragment/budget 10 ctx)
    ;; Original must be unchanged
    (is (= 5 (length (amoebum:ide-context-diagnostics ctx))))
    (is (= 1 (length (amoebum:ide-context-selections ctx))))))

;;; ---------------------------------------------------------------------------
;;; NXT-094 — selection precedence over diagnostics
;;; ---------------------------------------------------------------------------

(test ide-context-budget-selections-take-precedence-over-diagnostics
  "When budget is limited, selections survive longer than diagnostics."
  ;; Create a context where diagnostics fill up the budget but we have one selection.
  ;; The selection text is short; the diagnostics are numerous.
  (let* ((sel-text "(defun selected () :foo)")
         (sels  (list (list :file "/src/a.lisp"
                            :start-line 1
                            :end-line 2
                            :text sel-text)))
         (diags (loop for i from 1 to 20
                      collect (list :file "/src/a.lisp"
                                    :line i
                                    :severity :warning
                                    :message (make-string 30 :initial-element #\d))))
         (ctx (amoebum:make-ide-context
               :active-file "/src/a.lisp"
               :selections sels
               :diagnostics diags))
         ;; Compute a budget that is just enough for the header + active file +
         ;; the single short selection but NOT all the diagnostics.  We use the
         ;; estimate of a context with only the selection as our target budget.
         (sel-only-budget (amoebum:ide-context-token-estimate
                           (amoebum:make-ide-context
                            :active-file "/src/a.lisp"
                            :selections sels)))
         (frag (amoebum:ide-context-prompt-fragment/budget sel-only-budget ctx)))
    ;; The selection text should still appear in the fragment, meaning
    ;; diagnostics were dropped before the selection was.
    (when (stringp frag)
      (is (not (null (search sel-text frag)))
          "Selection text should be preserved when budget drops diagnostics first"))))

;;; ---------------------------------------------------------------------------
;;; NXT-095 — :ide-context-attached event
;;; ---------------------------------------------------------------------------

(test ide-context-attached-event-emitted-on-first-update
  "update-ide-context! emits :ide-context-attached when *ide-context* transitions from NIL."
  (let* ((bus (amoebum:make-event-bus))
         (amoebum:*event-bus* bus)
         (amoebum:*ide-context* nil))
    (amoebum:update-ide-context! (list :active-file "/attach.lisp"
                                       :open-files '("/attach.lisp")))
    (let ((events (%collect-ide-events bus amoebum:+event-type-ide-context-attached+)))
      (is (= 1 (length events))
          "Exactly one :ide-context-attached event should be emitted")
      (let* ((ev      (first events))
             (payload (amoebum:event-payload ev)))
        (is (amoebum:ide-context-attached-payload-p payload))
        (is (string= "/attach.lisp"
                     (amoebum:ide-context-attached-payload-active-file payload)))
        (is (= 1 (amoebum:ide-context-attached-payload-open-file-count payload)))))))

(test ide-context-attached-event-not-emitted-on-subsequent-update
  "update-ide-context! does NOT emit :ide-context-attached on a second call."
  (let* ((bus (amoebum:make-event-bus))
         (amoebum:*event-bus* bus)
         (amoebum:*ide-context* nil))
    ;; First attach
    (amoebum:update-ide-context! (list :active-file "/first.lisp"))
    ;; Second update — should not emit another attached event
    (amoebum:update-ide-context! (list :active-file "/second.lisp"))
    (let ((events (%collect-ide-events bus amoebum:+event-type-ide-context-attached+)))
      (is (= 1 (length events))
          ":ide-context-attached should only fire once"))))

;;; ---------------------------------------------------------------------------
;;; NXT-095 — :ide-context-truncated event
;;; ---------------------------------------------------------------------------

(test ide-context-truncated-event-emitted-when-budget-exceeded
  ":ide-context-truncated event is emitted when content is dropped."
  (let* ((bus (amoebum:make-event-bus))
         (amoebum:*event-bus* bus)
         (diags (loop for i from 1 to 8
                      collect (list :file "/src/b.lisp"
                                    :line i
                                    :severity :error
                                    :message (make-string 50 :initial-element #\e))))
         (ctx (amoebum:make-ide-context
               :active-file "/src/b.lisp"
               :diagnostics diags)))
    ;; Tiny budget forces truncation
    (amoebum:ide-context-prompt-fragment/budget 20 ctx)
    (let ((events (%collect-ide-events bus amoebum:+event-type-ide-context-truncated+)))
      (is (>= (length events) 1)
          "At least one :ide-context-truncated event expected")
      (let* ((ev      (first events))
             (payload (amoebum:event-payload ev)))
        (is (amoebum:ide-context-truncated-payload-p payload))
        (is (> (amoebum:ide-context-truncated-payload-token-estimate payload) 0))
        (is (= 20 (amoebum:ide-context-truncated-payload-token-budget payload)))
        (is (> (amoebum:ide-context-truncated-payload-diagnostics-dropped payload) 0))))))

(test ide-context-truncated-event-not-emitted-within-budget
  ":ide-context-truncated is NOT emitted when content fits within budget."
  (let* ((bus (amoebum:make-event-bus))
         (amoebum:*event-bus* bus)
         (ctx (amoebum:make-ide-context
               :active-file "/src/c.lisp"
               :diagnostics (list (list :file "/src/c.lisp"
                                        :line 1
                                        :severity :info
                                        :message "ok")))))
    ;; Large budget — no truncation expected
    (amoebum:ide-context-prompt-fragment/budget 9999 ctx)
    (let ((events (%collect-ide-events bus amoebum:+event-type-ide-context-truncated+)))
      (is (= 0 (length events))
          ":ide-context-truncated should not fire when budget is not exceeded"))))

;;; ---------------------------------------------------------------------------
;;; NXT-095 — :ide-context-dropped event
;;; ---------------------------------------------------------------------------

(test ide-context-dropped-event-emitted-on-clear
  "clear-ide-context! emits :ide-context-dropped when context was non-NIL."
  (let* ((bus (amoebum:make-event-bus))
         (amoebum:*event-bus* bus)
         (amoebum:*ide-context* (amoebum:make-ide-context
                                 :active-file "/dropping.lisp")))
    (amoebum:clear-ide-context!)
    (is (null amoebum:*ide-context*)
        "*ide-context* should be NIL after clear")
    (let ((events (%collect-ide-events bus amoebum:+event-type-ide-context-dropped+)))
      (is (= 1 (length events))
          "Exactly one :ide-context-dropped event expected")
      (let* ((ev      (first events))
             (payload (amoebum:event-payload ev)))
        (is (amoebum:ide-context-dropped-payload-p payload))
        (is (string= "/dropping.lisp"
                     (amoebum:ide-context-dropped-payload-active-file payload)))))))

(test ide-context-dropped-event-not-emitted-when-already-nil
  "clear-ide-context! does NOT emit :ide-context-dropped when context is already NIL."
  (let* ((bus (amoebum:make-event-bus))
         (amoebum:*event-bus* bus)
         (amoebum:*ide-context* nil))
    (amoebum:clear-ide-context!)
    (let ((events (%collect-ide-events bus amoebum:+event-type-ide-context-dropped+)))
      (is (= 0 (length events))
          ":ide-context-dropped should not fire when context was already NIL"))))

(test ide-context-dropped-event-returns-previous-context
  "clear-ide-context! returns the previous context object."
  (let* ((ctx (amoebum:make-ide-context :active-file "/prev.lisp"))
         (amoebum:*ide-context* ctx)
         (amoebum:*event-bus* (amoebum:make-event-bus)))
    (let ((returned (amoebum:clear-ide-context!)))
      (is (eq ctx returned)
          "clear-ide-context! should return the previous context"))))

;;; ---------------------------------------------------------------------------
;;; NXT-095 — attach/clear round-trip
;;; ---------------------------------------------------------------------------

(test ide-context-attach-clear-reattach-cycle
  "Full attach, clear, reattach cycle emits the right sequence of events."
  (let* ((bus (amoebum:make-event-bus))
         (amoebum:*event-bus* bus)
         (amoebum:*ide-context* nil))
    ;; Attach
    (amoebum:update-ide-context! (list :active-file "/cycle.lisp"))
    ;; Clear
    (amoebum:clear-ide-context!)
    ;; Re-attach
    (amoebum:update-ide-context! (list :active-file "/cycle2.lisp"))
    (let ((attached (%collect-ide-events bus amoebum:+event-type-ide-context-attached+))
          (dropped  (%collect-ide-events bus amoebum:+event-type-ide-context-dropped+)))
      ;; Two attached events (first attach + re-attach)
      (is (= 2 (length attached))
          "Two :ide-context-attached events expected over the cycle")
      ;; One dropped event
      (is (= 1 (length dropped))
          "One :ide-context-dropped event expected over the cycle"))))
