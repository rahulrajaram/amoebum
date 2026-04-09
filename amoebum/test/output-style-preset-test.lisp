(in-package :amoebum/test)

(def-suite output-style-preset-suite
  :description "Regression coverage for output-style presets in status-bar-state."
  :in amoebum-suite)

(in-suite output-style-preset-suite)

;;; ---------------------------------------------------------------------------
;;; Helpers
;;; ---------------------------------------------------------------------------

(defun %make-output-style-test-state (&key (output-style nil))
  (amoebum.ui:make-status-bar-state
   :permission-mode :supervised
   :focus-mode :arch
   :model-name "moonshot-v1-128k"
   :branch-name "feature/nxt-089"
   :output-style output-style))

;;; ---------------------------------------------------------------------------
;;; Tests
;;; ---------------------------------------------------------------------------

(test output-style-preset-constant-has-three-entries
  "The +output-style-presets+ alist contains exactly the three documented styles."
  (is (= 3 (length amoebum.ui:+output-style-presets+)))
  (is (assoc :compact  amoebum.ui:+output-style-presets+ :test #'eq))
  (is (assoc :operator amoebum.ui:+output-style-presets+ :test #'eq))
  (is (assoc :verbose  amoebum.ui:+output-style-presets+ :test #'eq)))

(test output-style-preset-constant-descriptions-are-strings
  "Every entry in +output-style-presets+ has a non-empty string description."
  (loop for (key . description) in amoebum.ui:+output-style-presets+ do
    (is (stringp description)
        "Expected string description for ~S, got ~S." key description)
    (is (plusp (length description))
        "Expected non-empty description for ~S." key)))

(test output-style-defaults-to-compact
  "make-status-bar-state defaults output-style to :compact so the package starts quieter."
  (let ((state (%make-output-style-test-state)))
    (is (eq :compact (amoebum.ui:status-bar-current-output-style state)))))

(test output-style-explicit-compact
  "make-status-bar-state honours :compact output-style."
  (let ((state (%make-output-style-test-state :output-style :compact)))
    (is (eq :compact (amoebum.ui:status-bar-current-output-style state)))))

(test output-style-explicit-verbose
  "make-status-bar-state honours :verbose output-style."
  (let ((state (%make-output-style-test-state :output-style :verbose)))
    (is (eq :verbose (amoebum.ui:status-bar-current-output-style state)))))

(test output-style-unknown-coerces-to-default
  "An unknown style keyword is silently coerced to :compact (the default)."
  (let ((state (%make-output-style-test-state :output-style :bogus)))
    (is (eq :compact (amoebum.ui:status-bar-current-output-style state)))))

(test output-style-nil-coerces-to-default
  "A nil style is silently coerced to :compact (the default)."
  (let ((state (%make-output-style-test-state :output-style nil)))
    (is (eq :compact (amoebum.ui:status-bar-current-output-style state)))))

(test status-bar-set-output-style-mutates-state
  "status-bar-set-output-style! replaces the style in place and returns the state."
  (let ((state (%make-output-style-test-state :output-style :operator)))
    (let ((returned (amoebum.ui:status-bar-set-output-style! state :compact)))
      (is (eq state returned))
      (is (eq :compact (amoebum.ui:status-bar-current-output-style state))))
    (amoebum.ui:status-bar-set-output-style! state :verbose)
    (is (eq :verbose (amoebum.ui:status-bar-current-output-style state)))))

(test status-bar-set-output-style-coerces-unknown
  "status-bar-set-output-style! coerces an unrecognised style to :compact."
  (let ((state (%make-output-style-test-state :output-style :compact)))
    (amoebum.ui:status-bar-set-output-style! state :totally-unknown)
    (is (eq :compact (amoebum.ui:status-bar-current-output-style state)))))

(test output-style-updates-from-config-event
  "Publishing a :output-style config event updates the state immediately."
  (let* ((bus (amoebum:make-event-bus :capacity 32))
         (state (amoebum.ui:make-status-bar-state
                 :event-bus bus
                 :permission-mode :supervised
                 :focus-mode :arch
                 :model-name "moonshot-v1-128k"
                 :branch-name "feature/nxt-089"
                 :output-style :compact)))
    ;; Verify initial style.
    (is (eq :compact (amoebum.ui:status-bar-current-output-style state)))
    ;; Fire a config-changed event switching to :verbose.
    (amoebum:publish bus
                     (amoebum:make-config-changed-event
                      :key :output-style
                      :old-value :compact
                      :new-value :verbose))
    (is (eq :verbose (amoebum.ui:status-bar-current-output-style state)))
    ;; Unknown value via event is also coerced.
    (amoebum:publish bus
                     (amoebum:make-config-changed-event
                      :key :output-style
                      :old-value :verbose
                      :new-value :not-a-style))
    (is (eq :compact (amoebum.ui:status-bar-current-output-style state)))))

(test output-style-render-key-includes-style
  "status-bar-render-key includes the output-style so caches invalidate on change."
  (let ((state (%make-output-style-test-state :output-style :compact)))
    (let ((key-compact (amoebum.ui:status-bar-render-key state)))
      (amoebum.ui:status-bar-set-output-style! state :verbose)
      (let ((key-verbose (amoebum.ui:status-bar-render-key state)))
        (is-false (equal key-compact key-verbose))))))

(test output-style-independent-of-focus-mode
  "output-style and focus-mode are independent; changing one does not affect the other."
  (let* ((bus (amoebum:make-event-bus :capacity 32))
         (state (amoebum.ui:make-status-bar-state
                 :event-bus bus
                 :permission-mode :supervised
                 :focus-mode :lean
                 :model-name "moonshot-v1-128k"
                 :branch-name "feature/nxt-089"
                 :output-style :compact)))
    (is (eq :lean    (amoebum.ui:status-bar-state-focus-mode state)))
    (is (eq :compact (amoebum.ui:status-bar-current-output-style state)))
    ;; Change focus-mode only.
    (amoebum:publish bus
                     (amoebum:make-config-changed-event
                      :key :status-bar-mode
                      :old-value :lean
                      :new-value :docs))
    (is (eq :docs    (amoebum.ui:status-bar-state-focus-mode state)))
    (is (eq :compact (amoebum.ui:status-bar-current-output-style state)))
    ;; Change output-style only.
    (amoebum:publish bus
                     (amoebum:make-config-changed-event
                      :key :output-style
                      :old-value :compact
                      :new-value :operator))
    (is (eq :docs     (amoebum.ui:status-bar-state-focus-mode state)))
    (is (eq :operator (amoebum.ui:status-bar-current-output-style state)))))
