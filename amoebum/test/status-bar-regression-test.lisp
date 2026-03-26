(in-package :amoebum/test)

;;; ============================================================
;;; NXT-111: Status-Bar Regression Tests
;;;
;;; Regression coverage for:
;;;   - All 4 focus modes (lean/code/docs/arch) producing valid segment lists
;;;   - All 3 output styles (compact/operator/verbose) producing valid segment lists
;;;   - Mode switching round-trips restoring segments correctly
;;;   - Context pressure segment showing correct values
;;;   - Disabled IDE context (nil) not crashing status bar rendering
;;;   - status-bar-render-key changes when any config changes
;;; ============================================================

(def-suite status-bar-regression-suite
  :description "NXT-111: Regression tests for status-bar modes, context pressure,
disabled ingestion, and transcript-budget truncation behavior."
  :in amoebum-suite)

(in-suite status-bar-regression-suite)

;;; ------------------------------------------------------------------
;;; Helpers
;;; ------------------------------------------------------------------

(defun %make-regression-state (&key
                                  (focus-mode :arch)
                                  (output-style :operator)
                                  (context-used 0)
                                  (context-max 128000)
                                  event-bus)
  "Construct a minimal status-bar-state for regression tests."
  (let ((state (amoebum.ui:make-status-bar-state
                :permission-mode :supervised
                :focus-mode focus-mode
                :output-style output-style
                :model-name "moonshot-v1-128k"
                :branch-name "test/nxt-111"
                :event-bus event-bus)))
    (setf (amoebum.ui:status-bar-state-context-used-tokens state) context-used
          (amoebum.ui:status-bar-state-context-max-tokens state)  context-max)
    state))

;;; ------------------------------------------------------------------
;;; NXT-111.1 — All 4 focus modes produce valid segment lists
;;; ------------------------------------------------------------------

(test regression-lean-mode-returns-valid-segments
  "Focus mode :lean produces a non-nil list of string segments."
  (let* ((state (amoebum.ui:make-status-bar-state
                 :permission-mode :supervised
                 :focus-mode :lean
                 :model-name "moonshot-v1-128k"
                 :branch-name "test/nxt-111"))
         (segments (amoebum.ui:status-bar-segments state)))
    (is (listp segments) ":lean mode must return a list")
    (is (plusp (length segments)) ":lean mode must return at least one segment")
    (loop for seg in segments do
      (is (stringp seg) "Every segment must be a string, got ~S" seg))))

(test regression-code-mode-returns-valid-segments
  "Focus mode :code produces a non-nil list of string segments."
  (let* ((state (amoebum.ui:make-status-bar-state
                 :permission-mode :supervised
                 :focus-mode :code
                 :model-name "moonshot-v1-128k"
                 :branch-name "test/nxt-111"))
         (segments (amoebum.ui:status-bar-segments state)))
    (is (listp segments) ":code mode must return a list")
    (is (plusp (length segments)) ":code mode must return at least one segment")
    (loop for seg in segments do
      (is (stringp seg) "Every segment must be a string, got ~S" seg))))

(test regression-docs-mode-returns-valid-segments
  "Focus mode :docs produces a non-nil list of string segments."
  (let* ((state (amoebum.ui:make-status-bar-state
                 :permission-mode :supervised
                 :focus-mode :docs
                 :model-name "moonshot-v1-128k"
                 :branch-name "test/nxt-111"))
         (segments (amoebum.ui:status-bar-segments state)))
    (is (listp segments) ":docs mode must return a list")
    (is (plusp (length segments)) ":docs mode must return at least one segment")
    (loop for seg in segments do
      (is (stringp seg) "Every segment must be a string, got ~S" seg))))

(test regression-arch-mode-returns-valid-segments
  "Focus mode :arch produces a non-nil list of string segments."
  (let* ((state (amoebum.ui:make-status-bar-state
                 :permission-mode :supervised
                 :focus-mode :arch
                 :model-name "moonshot-v1-128k"
                 :branch-name "test/nxt-111"))
         (segments (amoebum.ui:status-bar-segments state)))
    (is (listp segments) ":arch mode must return a list")
    (is (plusp (length segments)) ":arch mode must return at least one segment")
    (loop for seg in segments do
      (is (stringp seg) "Every segment must be a string, got ~S" seg))))

(test regression-all-focus-modes-produce-string-lines
  "status-bar-line produces a non-empty string for all 4 focus modes."
  (dolist (mode '(:lean :code :docs :arch))
    (let* ((state (amoebum.ui:make-status-bar-state
                   :permission-mode :supervised
                   :focus-mode mode
                   :model-name "moonshot-v1-128k"
                   :branch-name "test/nxt-111"))
           (line (amoebum.ui:status-bar-line state)))
      (is (stringp line)
          "status-bar-line for ~S mode must return a string" mode)
      (is (plusp (length line))
          "status-bar-line for ~S mode must not be empty" mode))))

;;; ------------------------------------------------------------------
;;; NXT-111.2 — All 3 output styles produce valid segment lists
;;; ------------------------------------------------------------------

(test regression-compact-style-produces-valid-segments
  "Output style :compact produces a non-nil list of string segments."
  (let* ((state (%make-regression-state :focus-mode :arch :output-style :compact))
         (segments (amoebum.ui:status-bar-segments state)))
    (is (listp segments))
    (is (plusp (length segments)))
    (loop for seg in segments do
      (is (stringp seg)))))

(test regression-operator-style-produces-valid-segments
  "Output style :operator produces a non-nil list of string segments."
  (let* ((state (%make-regression-state :focus-mode :arch :output-style :operator))
         (segments (amoebum.ui:status-bar-segments state)))
    (is (listp segments))
    (is (plusp (length segments)))
    (loop for seg in segments do
      (is (stringp seg)))))

(test regression-verbose-style-produces-valid-segments
  "Output style :verbose produces a non-nil list of string segments."
  (let* ((state (%make-regression-state :focus-mode :arch :output-style :verbose))
         (segments (amoebum.ui:status-bar-segments state)))
    (is (listp segments))
    (is (plusp (length segments)))
    (loop for seg in segments do
      (is (stringp seg)))))

(test regression-all-output-styles-with-all-modes
  "Every (focus-mode x output-style) combination renders without signalling."
  (dolist (mode '(:lean :code :docs :arch))
    (dolist (style '(:compact :operator :verbose))
      (let* ((state (%make-regression-state :focus-mode mode :output-style style))
             (result (handler-case
                         (amoebum.ui:status-bar-line state)
                       (error (c)
                         (list :error (princ-to-string c))))))
        (is (stringp result)
            "status-bar-line for mode=~S style=~S must return a string, got ~S"
            mode style result)))))

;;; ------------------------------------------------------------------
;;; NXT-111.3 — Mode switching round-trips restore segments correctly
;;; ------------------------------------------------------------------

(test regression-arch-to-lean-to-arch-round-trip
  "Switching arch -> lean -> arch via status-bar-set-output-style! restores
the arch line content (including mode arch/ indicator)."
  (let* ((bus   (amoebum:make-event-bus :capacity 32))
         (state (amoebum.ui:make-status-bar-state
                 :event-bus bus
                 :permission-mode :supervised
                 :focus-mode :arch
                 :output-style :operator
                 :model-name "moonshot-v1-128k"
                 :branch-name "test/nxt-111")))
    ;; Baseline: arch mode
    (let ((arch-line-before (amoebum.ui:status-bar-line state)))
      (is (search "mode arch/operator" arch-line-before :test #'char-equal)
          "Baseline arch line must contain mode arch/operator")
      ;; Switch focus to :lean via config event
      (amoebum:publish bus
                       (amoebum:make-config-changed-event
                        :key :status-bar-mode
                        :old-value :arch
                        :new-value :lean))
      (is (eq :lean (amoebum.ui:status-bar-state-focus-mode state))
          "State focus-mode must be :lean after config event")
      (let ((lean-line (amoebum.ui:status-bar-line state)))
        (is-false (search "mode arch/" lean-line :test #'char-equal)
                  "Lean line must not contain arch/ mode indicator")
        ;; Switch back to :arch
        (amoebum:publish bus
                         (amoebum:make-config-changed-event
                          :key :status-bar-mode
                          :old-value :lean
                          :new-value :arch))
        (is (eq :arch (amoebum.ui:status-bar-state-focus-mode state))
            "State focus-mode must be :arch after second config event")
        (let ((arch-line-after (amoebum.ui:status-bar-line state)))
          (is (search "mode arch/operator" arch-line-after :test #'char-equal)
              "Restored arch line must again contain mode arch/operator"))))))

(test regression-mode-switch-does-not-lose-branch-segment
  "Branch segment is present before and after a focus mode switch."
  (let* ((bus   (amoebum:make-event-bus :capacity 32))
         (state (amoebum.ui:make-status-bar-state
                 :event-bus bus
                 :permission-mode :supervised
                 :focus-mode :arch
                 :model-name "moonshot-v1-128k"
                 :branch-name "test/nxt-111")))
    (is (search "branch test/nxt-111"
                (amoebum.ui:status-bar-line state) :test #'char-equal)
        "Branch segment must be present in arch mode")
    ;; Switch to :code
    (amoebum:publish bus
                     (amoebum:make-config-changed-event
                      :key :status-bar-mode
                      :old-value :arch
                      :new-value :code))
    (is (search "branch test/nxt-111"
                (amoebum.ui:status-bar-line state) :test #'char-equal)
        "Branch segment must survive a mode switch to :code")
    ;; Switch back to :arch
    (amoebum:publish bus
                     (amoebum:make-config-changed-event
                      :key :status-bar-mode
                      :old-value :code
                      :new-value :arch))
    (is (search "branch test/nxt-111"
                (amoebum.ui:status-bar-line state) :test #'char-equal)
        "Branch segment must be present after round-tripping back to arch")))

;;; ------------------------------------------------------------------
;;; NXT-111.4 — Context pressure segment shows correct values
;;; ------------------------------------------------------------------

(test regression-context-pressure-zero-percent
  "Context pressure segment shows 0% when zero tokens used."
  (let* ((state (%make-regression-state
                 :focus-mode :arch
                 :context-used 0
                 :context-max 128000))
         (line (amoebum.ui:status-bar-line state)))
    (is (search "0%" line :test #'char-equal)
        "Expected 0% in line: ~S" line)))

(test regression-context-pressure-fifty-percent
  "Context pressure segment shows 50% when half of context is used."
  (let* ((state (%make-regression-state
                 :focus-mode :arch
                 :context-used 64000
                 :context-max 128000))
         (line (amoebum.ui:status-bar-line state)))
    (is (search "50%" line :test #'char-equal)
        "Expected 50% in line: ~S" line)))

(test regression-context-pressure-one-hundred-percent
  "Context pressure segment shows 100% when context is fully used."
  (let* ((state (%make-regression-state
                 :focus-mode :arch
                 :context-used 128000
                 :context-max 128000))
         (line (amoebum.ui:status-bar-line state)))
    (is (search "100%" line :test #'char-equal)
        "Expected 100% in line: ~S" line)))

(test regression-context-pressure-twenty-percent
  "Context pressure segment renders 20% for 25600/128000 tokens."
  (let* ((state (%make-regression-state
                 :focus-mode :arch
                 :context-used 25600
                 :context-max 128000))
         (line (amoebum.ui:status-bar-line state)))
    ;; 25600/128000 = 20%
    (is (search "20%" line :test #'char-equal)
        "Expected 20% in line: ~S" line)))

(test regression-context-pressure-absent-in-lean-mode
  "Context pressure (ctx [...]) must not appear in :lean mode."
  (let* ((state (%make-regression-state
                 :focus-mode :lean
                 :context-used 64000
                 :context-max 128000))
         (line (amoebum.ui:status-bar-line state)))
    (is-false (search "ctx [" line :test #'char-equal)
              "ctx [ must not appear in lean mode, line was: ~S" line)))

(test regression-context-pressure-updates-on-token-progress-event
  "Context pressure percentage updates when a stream-progress event changes token counts."
  (let* ((bus   (amoebum:make-event-bus :capacity 32))
         (state (amoebum.ui:make-status-bar-state
                 :event-bus bus
                 :permission-mode :supervised
                 :focus-mode :arch
                 :model-name "moonshot-v1-128k"
                 :branch-name "test/nxt-111")))
    ;; Initially 0%
    (let ((line-before (amoebum.ui:status-bar-line state)))
      (is (search "0%" line-before :test #'char-equal)
          "Initial context pressure must show 0%"))
    ;; Publish a stream-progress event with 50% usage
    (amoebum:publish-status-bar-stream-summary
     (list :status :running
           :activep t
           :tokens 64000
           :tokens-per-second 50.0d0)
     :event-bus bus)
    (let ((line-after (amoebum.ui:status-bar-line state)))
      (is (search "50%" line-after :test #'char-equal)
          "Context pressure must show 50% after stream-progress event"))))

;;; ------------------------------------------------------------------
;;; NXT-111.5 — Disabled IDE context (nil) doesn't crash status bar
;;; ------------------------------------------------------------------

(test regression-nil-ide-context-does-not-crash-status-bar-lean
  "status-bar-line with *ide-context* = NIL and focus :lean does not signal."
  (let* ((amoebum:*ide-context* nil)
         (state (%make-regression-state :focus-mode :lean))
         (result (handler-case
                     (amoebum.ui:status-bar-line state)
                   (error (c)
                     (list :error (princ-to-string c))))))
    (is (stringp result)
        "status-bar-line must return a string even with NIL ide-context, got ~S" result)))

(test regression-nil-ide-context-does-not-crash-status-bar-arch
  "status-bar-line with *ide-context* = NIL and focus :arch does not signal."
  (let* ((amoebum:*ide-context* nil)
         (state (%make-regression-state :focus-mode :arch))
         (result (handler-case
                     (amoebum.ui:status-bar-line state)
                   (error (c)
                     (list :error (princ-to-string c))))))
    (is (stringp result)
        "status-bar-line must return a string even with NIL ide-context, got ~S" result)))

(test regression-nil-ide-context-does-not-crash-segments
  "status-bar-segments with *ide-context* = NIL does not signal for any focus mode."
  (dolist (mode '(:lean :code :docs :arch))
    (let* ((amoebum:*ide-context* nil)
           (state (%make-regression-state :focus-mode mode))
           (error-caught nil)
           (result (handler-case
                       (amoebum.ui:status-bar-segments state)
                     (error (c)
                       (setf error-caught (princ-to-string c))
                       nil))))
      (is (null error-caught)
          "Must not signal an error for ~S with NIL ide-context, got: ~S" mode error-caught)
      (is (listp result)
          "status-bar-segments for ~S with NIL ide-context must return a list" mode))))

;;; ------------------------------------------------------------------
;;; NXT-111.6 — status-bar-render-key changes when any config changes
;;; ------------------------------------------------------------------

(test regression-render-key-changes-on-focus-mode-change
  "status-bar-render-key changes when focus-mode changes."
  (let* ((state (%make-regression-state :focus-mode :arch))
         (key-before (amoebum.ui:status-bar-render-key state)))
    (setf (amoebum.ui:status-bar-state-focus-mode state) :lean)
    (let ((key-after (amoebum.ui:status-bar-render-key state)))
      (is-false (equal key-before key-after)
                "Render key must change when focus-mode changes"))))

(test regression-render-key-changes-on-output-style-change
  "status-bar-render-key changes when output-style changes."
  (let* ((state (%make-regression-state :output-style :operator))
         (key-before (amoebum.ui:status-bar-render-key state)))
    (amoebum.ui:status-bar-set-output-style! state :verbose)
    (let ((key-after (amoebum.ui:status-bar-render-key state)))
      (is-false (equal key-before key-after)
                "Render key must change when output-style changes"))))

(test regression-render-key-changes-on-delegation-mode-change
  "status-bar-render-key changes when delegation-mode changes."
  (let* ((state (%make-regression-state))
         (key-local (amoebum.ui:status-bar-render-key state)))
    (setf (amoebum.ui:status-bar-state-delegation-mode state) :networked)
    (let ((key-networked (amoebum.ui:status-bar-render-key state)))
      (is-false (equal key-local key-networked)
                "Render key must change when delegation-mode changes"))))

(test regression-render-key-changes-on-context-used-change
  "status-bar-render-key changes when context-used-tokens changes."
  (let* ((state (%make-regression-state :context-used 0 :context-max 128000))
         (key-before (amoebum.ui:status-bar-render-key state)))
    (setf (amoebum.ui:status-bar-state-context-used-tokens state) 64000)
    (let ((key-after (amoebum.ui:status-bar-render-key state)))
      (is-false (equal key-before key-after)
                "Render key must change when context-used-tokens changes"))))

(test regression-render-key-changes-on-model-change
  "status-bar-render-key changes when model-name changes."
  (let* ((state (%make-regression-state))
         (key-before (amoebum.ui:status-bar-render-key state)))
    (setf (amoebum.ui:status-bar-state-model-name state) "gpt-4o")
    (let ((key-after (amoebum.ui:status-bar-render-key state)))
      (is-false (equal key-before key-after)
                "Render key must change when model-name changes"))))

(test regression-render-key-stable-without-changes
  "status-bar-render-key returns the same value when nothing changes."
  (let* ((state (%make-regression-state))
         (key-1 (amoebum.ui:status-bar-render-key state))
         (key-2 (amoebum.ui:status-bar-render-key state)))
    (is (equal key-1 key-2)
        "Render key must be stable when state is unchanged")))

;;; ------------------------------------------------------------------
;;; NXT-111.7 — Transcript-budget truncation: status-bar-line respects width
;;; ------------------------------------------------------------------

(test regression-status-bar-line-truncates-to-width
  "status-bar-line with :width N returns a string of exactly N characters."
  (let* ((state (%make-regression-state :focus-mode :arch))
         (target-width 40)
         (line (amoebum.ui:status-bar-line state :width target-width)))
    (is (stringp line) "Line must be a string")
    (is (= target-width (length line))
        "Line must be exactly ~D characters, got ~D"
        target-width (length line))))

(test regression-status-bar-line-truncates-to-narrow-width
  "status-bar-line :width 20 returns exactly 20 characters."
  (let* ((state (%make-regression-state :focus-mode :arch))
         (line (amoebum.ui:status-bar-line state :width 20)))
    (is (stringp line))
    (is (= 20 (length line))
        "Expected 20 characters, got ~D" (length line))))

(test regression-status-bar-line-no-width-returns-natural-string
  "status-bar-line without :width returns a non-empty string of natural length."
  (let* ((state (%make-regression-state :focus-mode :arch))
         (line (amoebum.ui:status-bar-line state)))
    (is (stringp line))
    (is (plusp (length line)) "Natural-width line must not be empty")))
