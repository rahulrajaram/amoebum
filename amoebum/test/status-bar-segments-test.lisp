(in-package :amoebum/test)

(def-suite status-bar-segments-suite
  :description "Regression coverage for the new status-bar segments: backends, context-pressure, Cultivar daemon, output-mode."
  :in amoebum-suite)

(in-suite status-bar-segments-suite)

;;; ---------------------------------------------------------------------------
;;; Helpers
;;; ---------------------------------------------------------------------------

(defun %make-segments-test-state (&key
                                    (focus-mode :arch)
                                    (output-style :operator)
                                    (delegation-mode nil)
                                    (context-used 0)
                                    (context-max 128000))
  "Create a minimal status-bar-state for segment rendering tests.
DELEGATION-MODE is not a make-status-bar-state keyword; we set it directly."
  (let ((state (amoebum.ui:make-status-bar-state
                :permission-mode :supervised
                :focus-mode focus-mode
                :output-style output-style
                :model-name "moonshot-v1-128k"
                :branch-name "test/nxt-105")))
    (setf (amoebum.ui:status-bar-state-context-used-tokens state) context-used
          (amoebum.ui:status-bar-state-context-max-tokens state) context-max)
    (when delegation-mode
      (setf (amoebum.ui:status-bar-state-delegation-mode state) delegation-mode))
    state))

(defmacro %with-cultivar-status-adapter ((adapter-var &key socket-p mode) &body body)
  `(let* ((root (%make-temp-directory "amoebum-status-cultivar"))
          (index (merge-pathnames "index/" root))
          (,adapter-var (amoebum:make-cultivar-adapter
                         :enabled-p t
                         :binary-path "/bin/sh"
                         :index-path index
                         :daemon-mode ,(or mode :prefer)
                         :daemon-auto-start-p t)))
     (unwind-protect
          (progn
            (ensure-directories-exist (merge-pathnames ".keep" index))
            (when ,socket-p
              (with-open-file (stream (merge-pathnames ".cultivar.sock" index)
                                      :direction :output
                                      :if-exists :supersede
                                      :if-does-not-exist :create)
                (write-string "" stream)))
            ,@body)
       (%delete-directory-tree-safe root))))

;;; ---------------------------------------------------------------------------
;;; :backends segment tests
;;; ---------------------------------------------------------------------------

(test backends-segment-local-mode
  "Local delegation mode renders as 'backend local' in the status line."
  (let* ((state (%make-segments-test-state :focus-mode :lean :delegation-mode :local))
         (line (amoebum.ui:status-bar-line state)))
    (is (search "backend local" line :test #'char-equal))))

(test backends-segment-networked-mode
  "Networked delegation mode renders as 'backend networked' in the status line."
  (let* ((state (%make-segments-test-state :focus-mode :lean :delegation-mode :networked))
         (line (amoebum.ui:status-bar-line state)))
    (is (search "backend networked" line :test #'char-equal))))

(test backends-segment-default-is-local
  "Default state (no explicit delegation-mode) renders 'backend local'."
  (let* ((state (%make-segments-test-state :focus-mode :lean))
         (line (amoebum.ui:status-bar-line state)))
    (is (search "backend local" line :test #'char-equal))))

(test backends-segment-present-in-lean-mode
  "The :lean focus mode includes the :backends segment."
  (let* ((state (%make-segments-test-state :focus-mode :lean))
         (line (amoebum.ui:status-bar-line state)))
    (is (search "backend" line :test #'char-equal))))

(test backends-segment-present-in-code-mode
  "The :code focus mode includes the :backends segment."
  (let* ((state (%make-segments-test-state :focus-mode :code))
         (line (amoebum.ui:status-bar-line state)))
    (is (search "backend" line :test #'char-equal))))

(test backends-segment-present-in-arch-mode
  "The :arch focus mode includes the :backends segment."
  (let* ((state (%make-segments-test-state :focus-mode :arch))
         (line (amoebum.ui:status-bar-line state)))
    (is (search "backend" line :test #'char-equal))))

(test backends-segment-updates-from-config-event
  "Publishing a :swarm-delegation-mode config event updates the backend segment."
  (let* ((bus (amoebum:make-event-bus :capacity 32))
         (state (amoebum.ui:make-status-bar-state
                 :event-bus bus
                 :permission-mode :supervised
                 :focus-mode :lean
                 :model-name "moonshot-v1-128k"
                 :branch-name "test/nxt-105")))
    ;; Default is local.
    (is (eq :local (amoebum.ui:status-bar-state-delegation-mode state)))
    ;; Fire a config-changed event switching to :networked.
    (amoebum:publish bus
                     (amoebum:make-config-changed-event
                      :key :swarm-delegation-mode
                      :old-value :local
                      :new-value :networked))
    (is (eq :networked (amoebum.ui:status-bar-state-delegation-mode state)))
    (let ((line (amoebum.ui:status-bar-line state)))
      (is (search "backend networked" line :test #'char-equal)))))

;;; ---------------------------------------------------------------------------
;;; :cultivar segment tests
;;; ---------------------------------------------------------------------------

(test cultivar-segment-absent-without-configured-adapter
  "No Cultivar adapter means the status line should stay unchanged."
  (let ((amoebum::*cultivar-adapter* nil))
    (let* ((state (%make-segments-test-state :focus-mode :arch))
           (line (amoebum.ui:status-bar-line state)))
      (is-false (search "cult " line :test #'char-equal)))))

(test cultivar-segment-shows-cold-when-daemon-preferred-but-stopped
  "A daemon-preferred adapter without a socket renders a cold Cultivar segment."
  (%with-cultivar-status-adapter (adapter :socket-p nil)
    (let ((amoebum::*cultivar-adapter* adapter))
      (let* ((state (%make-segments-test-state :focus-mode :arch))
             (line (amoebum.ui:status-bar-line state)))
        (is (search "cult cold" line :test #'char-equal))))))

(test cultivar-segment-shows-warm-when-socket-exists
  "A configured adapter with a live socket renders a warm Cultivar segment."
  (%with-cultivar-status-adapter (adapter :socket-p t)
    (let ((amoebum::*cultivar-adapter* adapter))
      (let* ((state (%make-segments-test-state :focus-mode :docs))
             (line (amoebum.ui:status-bar-line state)))
        (is (search "cult warm" line :test #'char-equal))))))

;;; ---------------------------------------------------------------------------
;;; :context-pressure segment tests
;;; ---------------------------------------------------------------------------

(test context-pressure-segment-shows-percentage
  "The :context-pressure segment renders a percentage value."
  (let* ((state (%make-segments-test-state
                 :focus-mode :arch
                 :context-used 25600
                 :context-max 128000))
         (line (amoebum.ui:status-bar-line state)))
    ;; 25600/128000 = 20%
    (is (search "20%" line :test #'char-equal))))

(test context-pressure-segment-shows-ctx-prefix
  "The :context-pressure segment uses the 'ctx' prefix."
  (let* ((state (%make-segments-test-state :focus-mode :code))
         (line (amoebum.ui:status-bar-line state)))
    (is (search "ctx" line :test #'char-equal))))

(test context-pressure-segment-present-in-code-mode
  "The :code focus mode includes the :context-pressure segment."
  (let* ((state (%make-segments-test-state :focus-mode :code))
         (line (amoebum.ui:status-bar-line state)))
    (is (search "ctx" line :test #'char-equal))))

(test context-pressure-segment-present-in-docs-mode
  "The :docs focus mode includes the :context-pressure segment."
  (let* ((state (%make-segments-test-state :focus-mode :docs))
         (line (amoebum.ui:status-bar-line state)))
    (is (search "ctx" line :test #'char-equal))))

(test context-pressure-segment-present-in-arch-mode
  "The :arch focus mode includes the :context-pressure segment."
  (let* ((state (%make-segments-test-state :focus-mode :arch))
         (line (amoebum.ui:status-bar-line state)))
    (is (search "ctx" line :test #'char-equal))))

(test context-pressure-segment-absent-in-lean-mode
  "The :lean focus mode does NOT include the :context-pressure segment."
  (let* ((state (%make-segments-test-state :focus-mode :lean))
         (line (amoebum.ui:status-bar-line state)))
    ;; :lean omits :context-pressure — ctx should not appear
    (is-false (search "ctx [" line :test #'char-equal))))

(test context-pressure-segment-zero-percent
  "Zero context usage renders as 0%."
  (let* ((state (%make-segments-test-state
                 :focus-mode :arch
                 :context-used 0
                 :context-max 128000))
         (line (amoebum.ui:status-bar-line state)))
    (is (search "0%" line :test #'char-equal))))

;;; ---------------------------------------------------------------------------
;;; :output-mode segment tests
;;; ---------------------------------------------------------------------------

(test output-mode-segment-shows-focus-and-style
  "The :output-mode segment shows both focus-mode and output-style."
  (let* ((state (%make-segments-test-state
                 :focus-mode :arch
                 :output-style :operator))
         (line (amoebum.ui:status-bar-line state)))
    (is (search "mode arch/operator" line :test #'char-equal))))

(test output-mode-segment-compact-style
  "The :output-mode segment reflects :compact style."
  (let* ((state (%make-segments-test-state
                 :focus-mode :docs
                 :output-style :compact))
         (line (amoebum.ui:status-bar-line state)))
    (is (search "mode docs/compact" line :test #'char-equal))))

(test output-mode-segment-verbose-style
  "The :output-mode segment reflects :verbose style."
  (let* ((state (%make-segments-test-state
                 :focus-mode :arch
                 :output-style :verbose))
         (line (amoebum.ui:status-bar-line state)))
    (is (search "mode arch/verbose" line :test #'char-equal))))

(test output-mode-segment-present-in-docs-mode
  "The :docs focus mode includes the :output-mode segment."
  (let* ((state (%make-segments-test-state :focus-mode :docs))
         (line (amoebum.ui:status-bar-line state)))
    (is (search "mode docs/" line :test #'char-equal))))

(test output-mode-segment-present-in-arch-mode
  "The :arch focus mode includes the :output-mode segment."
  (let* ((state (%make-segments-test-state :focus-mode :arch))
         (line (amoebum.ui:status-bar-line state)))
    (is (search "mode arch/" line :test #'char-equal))))

(test output-mode-segment-absent-in-lean-mode
  "The :lean focus mode does NOT include the :output-mode segment."
  (let* ((state (%make-segments-test-state :focus-mode :lean))
         (line (amoebum.ui:status-bar-line state)))
    (is-false (search "mode lean/" line :test #'char-equal))))

(test output-mode-segment-absent-in-code-mode
  "The :code focus mode does NOT include the :output-mode segment."
  (let* ((state (%make-segments-test-state :focus-mode :code))
         (line (amoebum.ui:status-bar-line state)))
    (is-false (search "mode code/" line :test #'char-equal))))

(test output-mode-segment-updates-on-style-change
  "Changing output-style via status-bar-set-output-style! updates :output-mode segment text."
  (let* ((state (%make-segments-test-state
                 :focus-mode :arch
                 :output-style :compact)))
    (is (search "mode arch/compact" (amoebum.ui:status-bar-line state) :test #'char-equal))
    (amoebum.ui:status-bar-set-output-style! state :verbose)
    (is (search "mode arch/verbose" (amoebum.ui:status-bar-line state) :test #'char-equal))))

;;; ---------------------------------------------------------------------------
;;; Render key tests
;;; ---------------------------------------------------------------------------

(test render-key-includes-delegation-mode
  "status-bar-render-key includes delegation-mode so caches invalidate on change."
  (let* ((state (%make-segments-test-state :focus-mode :lean)))
    (let ((key-local (amoebum.ui:status-bar-render-key state)))
      (setf (amoebum.ui:status-bar-state-delegation-mode state) :networked)
      (let ((key-networked (amoebum.ui:status-bar-render-key state)))
        (is-false (equal key-local key-networked))))))

(test render-key-includes-cultivar-daemon-state
  "status-bar-render-key changes when Cultivar daemon socket visibility changes."
  (%with-cultivar-status-adapter (adapter :socket-p nil)
    (let ((amoebum::*cultivar-adapter* adapter)
          (state (%make-segments-test-state :focus-mode :arch)))
      (let ((key-cold (amoebum.ui:status-bar-render-key state)))
        (with-open-file (stream (merge-pathnames ".cultivar.sock"
                                                 (amoebum::cultivar-adapter-index-path adapter))
                                :direction :output
                                :if-exists :supersede
                                :if-does-not-exist :create)
          (write-string "" stream))
        (let ((key-warm (amoebum.ui:status-bar-render-key state)))
          (is-false (equal key-cold key-warm)))))))
