(defpackage :ptui.test.panel
  (:use :cl :fiveam)
  (:export #:run-all #:panel-suite))

(in-package :ptui.test.panel)

(def-suite panel-suite
  :description "PTUI defpanel DSL macro (I288-I291).")

(in-suite panel-suite)

(defun %make-test-runtime ()
  (ptui.ui.runtime:make-runtime))

(defun %with-widget-context (runtime widget-name instance-key thunk)
  (let ((ptui.ui.runtime:*current-runtime* runtime)
        (ptui.ui.runtime:*current-widget-context*
          (ptui.ui.runtime::%make-widget-context widget-name instance-key runtime)))
    (funcall thunk)))

;;; ===================================================================
;;; I288: State and Data Compilation Tests
;;; ===================================================================

(test parse-panel-sections-extracts-all
  (multiple-value-bind (state data layout keys)
      (ptui.ui.panel::%parse-panel-sections
       '((:state (x 0) (y 1))
         (:data (sum (+ x y)))
         (:layout (:column (main :flex 1 (text "hi"))))
         (:keys (:up (decf x)))))
    (is (= (length state) 2))
    (is (= (length data) 1))
    (is (not (null layout)))
    (is (= (length keys) 1))))

(test parse-panel-sections-handles-missing
  (multiple-value-bind (state data layout keys)
      (ptui.ui.panel::%parse-panel-sections
       '((:state (x 0))))
    (is (= (length state) 1))
    (is (null data))
    (is (null layout))
    (is (null keys))))

;;; ===================================================================
;;; I289: Layout and Keys Compilation Tests
;;; ===================================================================

(test compile-layout-produces-constraint-element
  ;; Test that a simple column layout compiles
  (let ((form (ptui.ui.panel::%compile-layout-tree
               '((:column
                   (header :fixed 2
                     (ptui.widgets.core:make-text-widget "Title"))
                   (content :flex 1
                     (ptui.widgets.core:make-text-widget "Body")))))))
    ;; Should produce a form that references :constraint-layout
    (is (not (null form)))))

(test compile-keys-produces-handler
  (let ((form (ptui.ui.panel::%compile-panel-keys
               '((:up (decf x))
                 (:down (incf x))
                 (:enter (submit))))))
    (is (not (null form)))))

(test compile-keys-nil-for-empty
  (let ((form (ptui.ui.panel::%compile-panel-keys nil)))
    (is (null form))))

;;; ===================================================================
;;; I290: Full defpanel Expansion Tests
;;; ===================================================================

;; Define a test panel to verify it compiles and loads
(ptui.ui.panel:defpanel test-panel (title count)
  (:state
    (selected 0 :type fixnum))
  (:data
    (display (format nil "~A: ~D (sel: ~D)" title count selected)
      :deps (title count selected)))
  (:layout
    (:column
      (header :fixed 1
        (ptui.widgets.core:make-text-widget title))
      (body :flex 1
        (ptui.widgets.core:make-text-widget display))
      (footer :fixed 1
        (ptui.widgets.core:make-text-widget
         (format nil "Count: ~D" count)))))
  (:keys
    (:up (funcall set-selected (max 0 (1- selected))))
    (:down (funcall set-selected (1+ selected)))))

(test defpanel-compiles-and-loads
  ;; If we got here, test-panel compiled successfully
  (is-true t))

(test defpanel-widget-renders-element
  (let ((rt (%make-test-runtime)))
    (%with-widget-context rt 'test-panel 'tp-1
      (lambda ()
        (let ((elem (render-test-panel "Hello" 42)))
          (is (typep elem 'ptui.ui.elements:ui-element))
          ;; Should be a constraint-layout
          (is (eq (ptui.ui.elements:ui-element-type elem) :constraint-layout))
          ;; Should have 3 children (header, body, footer)
          (is (= (length (ptui.ui.elements:ui-element-children elem)) 3)))))))

(test defpanel-state-vars-accessible
  (let ((rt (%make-test-runtime)))
    (%with-widget-context rt 'test-panel 'tp-2
      (lambda ()
        ;; First render — defwidget rebinds context internally with
        ;; instance-key = widget-name (since no :key/:id param)
        (let ((elem (render-test-panel "Test" 10)))
          (declare (ignore elem))
          ;; State should exist in runtime under the defwidget's key
          ;; defwidget uses widget-name as instance-key when no :key/:id prop
          (let ((key (list 'test-panel 'test-panel 'selected)))
            (multiple-value-bind (val found)
                (ptui.ui.runtime:runtime-state rt key)
              (is (not (null found)))
              (when found
                (is (= val 0))))))))))

(test defpanel-data-recomputes-on-dep-change
  (let ((rt (%make-test-runtime)))
    (%with-widget-context rt 'test-panel 'tp-3
      (lambda ()
        (let ((elem1 (render-test-panel "A" 1)))
          (declare (ignore elem1))
          ;; Re-render with different params
          (let ((elem2 (render-test-panel "B" 2)))
            ;; The data var should reflect the new values
            ;; Check through the rendered children
            (let* ((body-child (second (ptui.ui.elements:ui-element-children elem2)))
                   (text (getf (ptui.ui.elements:ui-element-props body-child) :text)))
              (is (search "B" text)))))))))

;;; ===================================================================
;;; I291: End-to-End Render Tests
;;; ===================================================================

(test defpanel-produces-renderable-buffer
  (let ((rt (%make-test-runtime)))
    (%with-widget-context rt 'test-panel 'tp-4
      (lambda ()
        (let* ((elem (render-test-panel "E2E" 5))
               (buf (ptui.render.buffer:make-buffer 80 24)))
          ;; Paint should not error
          (ptui.ui.app::%paint-element elem buf 0 0 80 24)
          (is (not (null buf))))))))

(test defpanel-key-handler-attached
  (let ((rt (%make-test-runtime)))
    (%with-widget-context rt 'test-panel 'tp-5
      (lambda ()
        (let ((elem (render-test-panel "Keys" 1)))
          ;; Root element should have :on-event handler
          (let ((handler (getf (ptui.ui.elements:ui-element-props elem) :on-event)))
            (is (functionp handler))))))))

(test defpanel-key-routing-updates-state
  (let ((rt (%make-test-runtime)))
    (%with-widget-context rt 'test-panel 'tp-keys-route
      (lambda ()
        ;; Render and commit so runtime focus order/id are established.
        (let ((elem (render-test-panel "Keys" 5)))
          (let ((ptui.ui.runtime:*current-runtime* rt))
            (ptui.ui.runtime:update-runtime rt elem))
          (let* ((route (ptui.ui.runtime:route-event
                         rt
                         (ptui.core.events:make-key-event :down)))
                 (root (ptui.ui.runtime:runtime-root rt)))
            (is (eq (getf route :kind) :key))
            (is (not (null (getf route :target))))
            (ptui.widgets.core:dispatch-widget-event root route)
            (let ((key (list 'test-panel 'test-panel 'selected)))
              (multiple-value-bind (val found)
                  (ptui.ui.runtime:runtime-state rt key)
                (is (not (null found)))
                (when found
                  (is (= val 1)))))))))))

;; Define a minimal panel without keys to test that case
(ptui.ui.panel:defpanel minimal-panel (label)
  (:state
    (counter 0 :type fixnum))
  (:layout
    (:column
      (main :flex 1
        (ptui.widgets.core:make-text-widget
         (format nil "~A: ~D" label counter))))))

(test defpanel-without-keys-works
  (let ((rt (%make-test-runtime)))
    (%with-widget-context rt 'minimal-panel 'mp-1
      (lambda ()
        (let ((elem (render-minimal-panel "no-keys")))
          (is (typep elem 'ptui.ui.elements:ui-element))
          ;; No :on-event since no :keys section
          (is (null (getf (ptui.ui.elements:ui-element-props elem) :on-event))))))))

;;; ===================================================================
;;; I292: Effects Section Tests
;;; ===================================================================

(test parse-panel-sections-extracts-effects
  (multiple-value-bind (state data layout keys effects)
      (ptui.ui.panel::%parse-panel-sections
       '((:state (x 0))
         (:effects
           (my-effect (do-something) :deps (x)))
         (:layout (:column (main :flex 1 (text "hi"))))))
    (is (= (length state) 1))
    (is (null data))
    (is (not (null layout)))
    (is (null keys))
    (is (= (length effects) 1))
    (is (eq (caar effects) 'my-effect))))

(test compile-effects-produces-use-effect-forms
  (let ((forms (ptui.ui.panel::%compile-panel-effects
                '((drain-streams (drain-all) :deps (stream-state))
                  (sync-approval (sync-it) :deps (approval))))))
    (is (= (length forms) 2))
    ;; Each form should be a use-effect call
    (is (eq (caar forms) 'ptui.ui.hooks:use-effect))
    (is (eq (second (first forms)) 'drain-streams))))

;; Define a panel with effects to verify compilation
(defvar *effect-log* nil)

(ptui.ui.panel:defpanel effects-panel (label)
  (:state
    (counter 0 :type fixnum))
  (:effects
    (log-render (push (list 'rendered label counter) *effect-log*)
      :deps (label counter)))
  (:layout
    (:column
      (main :flex 1
        (ptui.widgets.core:make-text-widget
         (format nil "~A: ~D" label counter))))))

(test defpanel-effects-compile-and-load
  ;; If we got here, effects-panel compiled successfully
  (is-true t))

(test defpanel-effects-run-on-render
  (let ((*effect-log* nil)
        (rt (%make-test-runtime)))
    (%with-widget-context rt 'effects-panel 'ep-1
      (lambda ()
        (let ((elem (render-effects-panel "test")))
          (declare (ignore elem))
          ;; Effects are enqueued, not yet run
          ;; Run pending effects via update-runtime
          (let ((ptui.ui.runtime:*current-runtime* rt))
            (ptui.ui.runtime:update-runtime rt
              (render-effects-panel "test")))
          ;; After update-runtime, effects should have fired
          (is (not (null *effect-log*))))))))

;; Panel with effect cleanup
(defvar *cleanup-log* nil)

(ptui.ui.panel:defpanel cleanup-effects-panel (label)
  (:effects
    (tracked (progn
               (push (list 'effect label) *cleanup-log*)
               (lambda () (push (list 'cleanup label) *cleanup-log*)))
      :deps (label)
      :cleanup nil))
  (:layout
    (:column
      (main :flex 1
        (ptui.widgets.core:make-text-widget label)))))

(test defpanel-effects-cleanup-compiles
  ;; Cleanup panel compiles
  (is-true t))

;;; ===================================================================
;;; I293: Conditional Regions Tests
;;; ===================================================================

;; Panel with conditional region
(ptui.ui.panel:defpanel conditional-panel (show-header)
  (:layout
    (:column
      (header :fixed 2 :when show-header
        (ptui.widgets.core:make-text-widget "Header"))
      (body :flex 1
        (ptui.widgets.core:make-text-widget "Body")))))

(test defpanel-conditional-region-present-when-truthy
  (let ((rt (%make-test-runtime)))
    (%with-widget-context rt 'conditional-panel 'cp-1
      (lambda ()
        (let ((elem (render-conditional-panel t)))
          (is (typep elem 'ptui.ui.elements:ui-element))
          ;; Should have 2 children when show-header is t
          (is (= (length (ptui.ui.elements:ui-element-children elem)) 2)))))))

(test defpanel-conditional-region-absent-when-nil
  (let ((rt (%make-test-runtime)))
    (%with-widget-context rt 'conditional-panel 'cp-2
      (lambda ()
        (let ((elem (render-conditional-panel nil)))
          (is (typep elem 'ptui.ui.elements:ui-element))
          ;; Should have 1 child when show-header is nil
          (is (= (length (ptui.ui.elements:ui-element-children elem)) 1))
          ;; The remaining child should be the body
          (let* ((child (first (ptui.ui.elements:ui-element-children elem)))
                 (text (getf (ptui.ui.elements:ui-element-props child) :text)))
            (is (string= text "Body"))))))))

;; Panel with multiple conditional regions
(ptui.ui.panel:defpanel multi-conditional-panel (show-a show-b)
  (:layout
    (:column
      (region-a :fixed 3 :when show-a
        (ptui.widgets.core:make-text-widget "A"))
      (region-b :fixed 3 :when show-b
        (ptui.widgets.core:make-text-widget "B"))
      (main :flex 1
        (ptui.widgets.core:make-text-widget "Main")))))

(test defpanel-multiple-conditional-regions
  (let ((rt (%make-test-runtime)))
    (%with-widget-context rt 'multi-conditional-panel 'mcp-1
      (lambda ()
        ;; Both shown
        (let ((elem (render-multi-conditional-panel t t)))
          (is (= (length (ptui.ui.elements:ui-element-children elem)) 3)))
        ;; Only A shown
        (let ((elem (render-multi-conditional-panel t nil)))
          (is (= (length (ptui.ui.elements:ui-element-children elem)) 2)))
        ;; Neither shown
        (let ((elem (render-multi-conditional-panel nil nil)))
          (is (= (length (ptui.ui.elements:ui-element-children elem)) 1)))))))

(test defpanel-conditional-constraints-match-children
  (let ((rt (%make-test-runtime)))
    (%with-widget-context rt 'conditional-panel 'cp-3
      (lambda ()
        ;; When condition is nil, constraints list should also shrink
        (let* ((elem (render-conditional-panel nil))
               (constraints (getf (ptui.ui.elements:ui-element-props elem) :constraints)))
          ;; Only 1 constraint (body :flex) since header is hidden
          (is (= (length constraints) 1)))))))

;;; ===================================================================
;;; I294: Modal Key Groups Tests
;;; ===================================================================

(defvar *modal-key-log* nil)

(ptui.ui.panel:defpanel modal-keys-panel (mode-a-active mode-b-active)
  (:layout
    (:column
      (main :flex 1
        (ptui.widgets.core:make-text-widget "modal"))))
  (:keys
    (:mode :mode-a :when mode-a-active
      (:up (push :a-up *modal-key-log*))
      (:down (push :a-down *modal-key-log*)))
    (:mode :mode-b :when mode-b-active
      (:up (push :b-up *modal-key-log*))
      (:down (push :b-down *modal-key-log*)))
    (:mode :default
      (:up (push :default-up *modal-key-log*))
      (:down (push :default-down *modal-key-log*)))))

(test defpanel-modal-keys-compile
  ;; If we got here, modal keys compiled
  (is-true t))

(test defpanel-modal-keys-handler-attached
  (let ((rt (%make-test-runtime)))
    (%with-widget-context rt 'modal-keys-panel 'mk-1
      (lambda ()
        (let ((elem (render-modal-keys-panel nil nil)))
          (let ((handler (getf (ptui.ui.elements:ui-element-props elem) :on-event)))
            (is (functionp handler))))))))

(test defpanel-modal-keys-first-active-wins
  (let ((*modal-key-log* nil)
        (rt (%make-test-runtime)))
    (%with-widget-context rt 'modal-keys-panel 'mk-2
      (lambda ()
        (let* ((elem (render-modal-keys-panel t nil))
               (handler (getf (ptui.ui.elements:ui-element-props elem) :on-event))
               (event (ptui.core.events:make-key-event :up)))
          ;; Mode A is active, so :a-up should fire
          (funcall handler event nil)
          (is (eq (first *modal-key-log*) :a-up)))))))

(test defpanel-modal-keys-fallthrough-to-default
  (let ((*modal-key-log* nil)
        (rt (%make-test-runtime)))
    (%with-widget-context rt 'modal-keys-panel 'mk-3
      (lambda ()
        (let* ((elem (render-modal-keys-panel nil nil))
               (handler (getf (ptui.ui.elements:ui-element-props elem) :on-event))
               (event (ptui.core.events:make-key-event :up)))
          ;; No mode active, should fall through to default
          (funcall handler event nil)
          (is (eq (first *modal-key-log*) :default-up)))))))

;;; ===================================================================
;;; I296: Context Section Tests
;;; ===================================================================

(test provide-context-and-use-context-roundtrip
  (let ((rt (%make-test-runtime)))
    (%with-widget-context rt 'test-ctx 'ctx-1
      (lambda ()
        (ptui.ui.hooks:provide-context 'theme :dark)
        (is (eq (ptui.ui.hooks:use-context 'theme) :dark))))))

(test use-context-returns-nil-when-not-provided
  (let ((rt (%make-test-runtime)))
    (%with-widget-context rt 'test-ctx 'ctx-2
      (lambda ()
        (is (null (ptui.ui.hooks:use-context 'nonexistent)))))))

;; Provider panel
(ptui.ui.panel:defpanel context-provider-panel (theme-value)
  (:context
    (theme :provide theme-value))
  (:layout
    (:column
      (main :flex 1
        (ptui.widgets.core:make-text-widget "provider")))))

;; Consumer panel (reads context directly via use-context)
(ptui.ui.panel:defpanel context-consumer-panel ()
  (:context
    (current-theme :from theme))
  (:layout
    (:column
      (main :flex 1
        (ptui.widgets.core:make-text-widget
         (format nil "theme: ~A" current-theme))))))

(test defpanel-context-provide-compiles
  (is-true t))

(test defpanel-context-consume-compiles
  (is-true t))

(test defpanel-context-parent-to-child-propagation
  (let ((rt (%make-test-runtime)))
    (%with-widget-context rt 'context-provider-panel 'prov-1
      (lambda ()
        ;; Provider renders and sets context
        (render-context-provider-panel :dark)
        ;; Now consumer should be able to read it
        (%with-widget-context rt 'context-consumer-panel 'cons-1
          (lambda ()
            (let ((elem (render-context-consumer-panel)))
              ;; Check that the body text contains "dark"
              (let* ((child (first (ptui.ui.elements:ui-element-children elem)))
                 (text (getf (ptui.ui.elements:ui-element-props child) :text)))
                (is (search "DARK" (string-upcase text)))))))))))

;;; ===================================================================
;;; I315: defpanel Validation Tests
;;; ===================================================================

(test defpanel-duplicate-state-vars-signals-error
  (signals ptui.ui.panel:defpanel-syntax-error
    (macroexpand-1
     '(ptui.ui.panel:defpanel duplicate-state-panel (title)
        (:state
          (count 0)
          (count 1))
        (:layout
          (:column
            (main :flex 1
              (ptui.widgets.core:make-text-widget title))))))))

(test defpanel-unknown-section-signals-error
  (signals ptui.ui.panel:defpanel-syntax-error
    (macroexpand-1
     '(ptui.ui.panel:defpanel unknown-section-panel (title)
        (:mystery (value 1))
        (:layout
          (:column
            (main :flex 1
              (ptui.widgets.core:make-text-widget title))))))))

(test defpanel-duplicate-layout-region-names-signals-error
  (signals ptui.ui.panel:defpanel-syntax-error
    (macroexpand-1
     '(ptui.ui.panel:defpanel duplicate-region-panel (title)
        (:layout
          (:column
            (panel :fixed 1
              (ptui.widgets.core:make-text-widget title))
            (panel :flex 1
              (ptui.widgets.core:make-text-widget title))))))))

(test defpanel-missing-deps-warning
  (let ((warned nil))
    (handler-bind ((ptui.ui.panel:defpanel-syntax-warning
                    (lambda (warning)
                      (when (eq (ptui.ui.panel:defpanel-syntax-warning-section warning) :data)
                        (setf warned t))
                      (muffle-warning warning))))
      (macroexpand-1
       '(ptui.ui.panel:defpanel missing-deps-panel (title)
          (:data
            (display (concatenate 'string title "-value")))
          (:layout
            (:column
              (main :flex 1
                (ptui.widgets.core:make-text-widget title)))))))
    (is-true warned)))

(test defpanel-when-unbound-warning
  (let ((warned nil))
    (handler-bind ((ptui.ui.panel:defpanel-syntax-warning
                    (lambda (warning)
                      (when (eq (ptui.ui.panel:defpanel-syntax-warning-section warning) :layout)
                        (setf warned t))
                      (muffle-warning warning))))
      (macroexpand-1
       '(ptui.ui.panel:defpanel unbound-when-panel (title)
          (:layout
            (:column
              (main :fixed 1 :when unbound-switch
                (ptui.widgets.core:make-text-widget title)))))))
    (is-true warned)))

(test defpanel-valid-panel-does-not-signal
  (let ((warned nil)
        (errored nil))
    (handler-bind ((ptui.ui.panel:defpanel-syntax-error
                     (lambda (condition)
                       (declare (ignore condition))
                       (setf errored t))))
      (handler-bind ((ptui.ui.panel:defpanel-syntax-warning
                      (lambda (warning)
                        (setf warned t)
                        (muffle-warning warning))))
        (macroexpand-1
         '(ptui.ui.panel:defpanel valid-compile-panel (title count)
            (:state
              (selected 0 :type fixnum))
            (:data
              (display (format nil "~A: ~D" title count)
                :deps (title count selected)))
            (:layout
              (:column
                (main :flex 1
                  (ptui.widgets.core:make-text-widget display)))))))
      (is-false errored))
        (is-false warned)))

;;; ===================================================================
;;; I316: defpanel Style Section Tests
;;; ===================================================================

(ptui.ui.panel:defpanel styled-region-panel (title)
  (:style
    (header
      :border :rounded
      :fg :green
      :bg :black
      :bold t))
  (:layout
    (:column
      (header :fixed 1
        (ptui.widgets.core:make-text-widget (format nil "title: ~A" title)))
      (body :flex 1
        (ptui.widgets.core:make-text-widget "body")))))

(test defpanel-style-section-wraps-regions-in-box-widget
  (let ((rt (%make-test-runtime)))
    (%with-widget-context rt 'styled-region-panel 'sp-1
      (lambda ()
        (let* ((elem (render-styled-region-panel "Style"))
               (children (ptui.ui.elements:ui-element-children elem))
               (header-child (first children))
               (props (ptui.ui.elements:ui-element-props header-child))
               (attrs (getf props :attrs)))
          (is (eq (ptui.ui.elements:ui-element-type header-child) :box))
          (is (eq (getf props :border) :rounded))
          (is (eq (getf props :fg) :green))
          (is (eq (getf props :bg) :black))
          (is (typep attrs 'ptui.core.types:attrs))
          (is (eq (ptui.core.types:attrs-boldp attrs) t)))))))

(test defpanel-style-no-region-style-backward-compat
  (signals error
    (macroexpand-1
     '(ptui.ui.panel:defpanel legacy-region-style-panel (title)
        (:layout
            (:column
             (header :style :rounded
               (ptui.widgets.core:make-text-widget title))))))))

(test defpanel-style-unknown-region-signals-error
  (signals ptui.ui.panel:defpanel-syntax-error
    (macroexpand-1
     '(ptui.ui.panel:defpanel style-unknown-region-panel (title)
        (:style
          (ghost :border :single :fg :red))
        (:layout
          (:column
            (main :flex 1
               (ptui.widgets.core:make-text-widget title))))))))

(defun %find-child-by-id (element id)
  (find id (ptui.ui.elements:ui-element-children element)
        :key #'ptui.ui.elements:ui-element-id
        :test #'eq))

;;; ===================================================================
;;; I317: Panel Composition Helpers
;;; ===================================================================

(test embed-panel-expands-to-child-render-call
  (let* ((child 'slot-child-panel)
         (expected (intern (format nil "RENDER-~A" (symbol-name child))
                          (symbol-package child)))
         (expanded (macroexpand-1 '(ptui.ui.panel:embed-panel slot-child-panel :title "Hello"))))
    (is (equal expanded (list expected :title "Hello")))))

(ptui.ui.panel:defpanel slot-host-panel (label)
  (:slots
   (sidebar :default (ptui.widgets.core:make-text-widget "Default Sidebar")))
  (:layout
    (:column
      (header :fixed 1 (ptui.widgets.core:make-text-widget label))
      (sidebar-region :flex 1 sidebar))))

(test panel-slot-uses-default-when-not-provided
  (let ((rt (%make-test-runtime)))
    (%with-widget-context rt 'slot-host-panel 'slot-host-1
      (lambda ()
        (let* ((elem (render-slot-host-panel "Main"))
               (sidebar (%find-child-by-id elem 'sidebar-region))
               (text (getf (ptui.ui.elements:ui-element-props sidebar) :text)))
          (is (string= text "Default Sidebar")))))))

(test panel-slot-override-overrides-default
  (let ((rt (%make-test-runtime)))
    (%with-widget-context rt 'slot-host-panel 'slot-host-2
      (lambda ()
        (let* ((elem (render-slot-host-panel
                      "Main"
                      :sidebar (ptui.widgets.core:make-text-widget "Custom Sidebar")))
               (sidebar (%find-child-by-id elem 'sidebar-region))
               (text (getf (ptui.ui.elements:ui-element-props sidebar) :text)))
          (is (string= text "Custom Sidebar")))))))

(defvar *slot-key-value* nil)

(ptui.ui.panel:defpanel slot-key-panel (label)
  (:slots
   (action :default :default-action))
  (:layout
    (:column
      (main :flex 1 (ptui.widgets.core:make-text-widget label))))
  (:keys
    (:enter (setf *slot-key-value* action))))

(test panel-slot-values-available-in-keys
  (let ((*slot-key-value* nil)
        (rt (%make-test-runtime)))
    (%with-widget-context rt 'slot-key-panel 'slot-key-1
      (lambda ()
        (let* ((elem (render-slot-key-panel "Main"))
               (handler (getf (ptui.ui.elements:ui-element-props elem) :on-event)))
          (funcall handler (ptui.core.events:make-key-event :enter) nil)
          (is (eq *slot-key-value* :default-action)))))))

;;; ===================================================================
;;; Padding and Gutter DSL Tests
;;; ===================================================================

(test normalize-padding-nil
  (is (equal (ptui.ui.panel::%normalize-padding nil) '(0 0 0 0))))

(test normalize-padding-shorthand-1
  (is (equal (ptui.ui.panel::%normalize-padding 1) '(1 1 1 1))))

(test normalize-padding-shorthand-2
  (is (equal (ptui.ui.panel::%normalize-padding '(1 2)) '(1 2 1 2))))

(test normalize-padding-shorthand-4
  (is (equal (ptui.ui.panel::%normalize-padding '(1 2 3 4)) '(1 2 3 4))))

(test container-padding-parsed
  "(:column :padding (1 2) ...) emits :padding in element props."
  (let ((form (ptui.ui.panel::%compile-layout-tree
               '((:column :padding (1 2)
                   (header :fixed 2
                     (ptui.widgets.core:make-text-widget "Title"))
                   (content :flex 1
                     (ptui.widgets.core:make-text-widget "Body")))))))
    (is (not (null form)))
    ;; The form should contain :padding in the props list
    (let ((form-str (format nil "~S" form)))
      (is (search ":PADDING" form-str)))))

(test region-gutter-parsed
  "(history :flex 1 :gutter 2 ...) emits gutter in :gutters prop."
  (let ((form (ptui.ui.panel::%compile-layout-tree
               '((:column
                   (history :flex 1 :gutter 2
                     (ptui.widgets.core:make-text-widget "Messages"))
                   (status :fixed 1
                     (ptui.widgets.core:make-text-widget "OK")))))))
    (is (not (null form)))
    (let ((form-str (format nil "~S" form)))
      (is (search ":GUTTERS" form-str)))))

(test region-min-max-height-parsed
  "(history :flex 1 :min-height 5 :max-height 20 ...) emits :MIN and :MAX in constraint."
  (let ((form (ptui.ui.panel::%compile-layout-tree
               '((:column
                   (history :flex 1 :min-height 5 :max-height 20
                     (ptui.widgets.core:make-text-widget "Messages"))
                   (status :fixed 1
                     (ptui.widgets.core:make-text-widget "OK")))))))
    (is (not (null form)))
    (let ((form-str (format nil "~S" form)))
      (is (search ":MIN" form-str))
      (is (search ":MAX" form-str)))))

(test container-padding-absent
  "No :padding keyword -> no :padding prop (backward compat)."
  (let ((form (ptui.ui.panel::%compile-layout-tree
               '((:column
                   (main :flex 1
                     (ptui.widgets.core:make-text-widget "Body")))))))
    (is (not (null form)))
    (let ((form-str (format nil "~S" form)))
      (is (null (search ":PADDING" form-str))))))

;; Panel with padding and gutter to verify compilation
(ptui.ui.panel:defpanel padded-panel (label)
  (:layout
    (:column :padding (1 2)
      (header :fixed 1
        (ptui.widgets.core:make-text-widget label))
      (body :flex 1 :gutter 2
        (ptui.widgets.core:make-text-widget "Content")))))

(test defpanel-padding-gutter-compiles-and-renders
  (let ((rt (%make-test-runtime)))
    (%with-widget-context rt 'padded-panel 'pp-1
      (lambda ()
        (let ((elem (render-padded-panel "Padded")))
          (is (typep elem 'ptui.ui.elements:ui-element))
          (is (eq (ptui.ui.elements:ui-element-type elem) :constraint-layout))
          ;; Should have padding in props
          (let ((padding (getf (ptui.ui.elements:ui-element-props elem) :padding)))
            (is (equal padding '(1 2 1 2))))
          ;; Should have gutters in props
          (let ((gutters (getf (ptui.ui.elements:ui-element-props elem) :gutters)))
            (is (not (null gutters)))
            (is (= (cdr (assoc 'body gutters)) 2)))
          ;; Should have 2 children
          (is (= (length (ptui.ui.elements:ui-element-children elem)) 2)))))))

(test defpanel-padding-gutter-paints-without-error
  (let ((rt (%make-test-runtime)))
    (%with-widget-context rt 'padded-panel 'pp-2
      (lambda ()
        (let* ((elem (render-padded-panel "Paint"))
               (buf (ptui.render.buffer:make-buffer 80 24)))
          (ptui.ui.app::%paint-element elem buf 0 0 80 24)
          (is (not (null buf))))))))

(defun run-all ()
  (run! 'panel-suite))
