# PTUI defpanel Consumer Guide (I319)

This tutorial is for `defpanel` consumers and uses concrete snippets from:

- `ptui/examples/panel-demo.lisp`
- `ptui/examples/focus-console.lisp`
- `ptui/examples/ops-wallboard.lisp`
- `ptui/examples/release-tracker.lisp`
- `amoebum/src/ui/panels/chat-panel.lisp`
- `amoebum/src/ui/panels/prompt-input.lisp`
- `amoebum/src/ui/panels/stream-effects.lisp`

For `:context` and composition (`embed-panel`, `panel-slot`), current canonical examples live in `ptui/test/panel-test.lisp`.

## 1) Quick Start (counter-panel from panel-demo.lisp)

Start with the minimal end-to-end example in `ptui/examples/panel-demo.lisp`:

```lisp
(ptui.ui.panel:defpanel counter-panel (items title)
  (:state
    (selected-index 0 :type fixnum))
  (:data
    (item-count (length items) :deps (items))
    (status-text (format nil "~A | ~D items" title item-count) :deps (title item-count)))
  (:layout
    (:column
      (header :fixed 1
        (ptui.widgets.core:make-text-widget
         (format nil "=== ~A ===" title)))
      (content :flex 1
        (ptui.views:list-view items #'%render-counter-item 10 nil selected-index nil))
      (footer :fixed 1
        (ptui.views:status-bar
         (list :left status-text :right (format nil "sel:~D" selected-index))
         nil nil))))
  (:keys
    (:up (funcall set-selected-index (max 0 (1- selected-index))))
    (:down (funcall set-selected-index (min (1- item-count) (1+ selected-index))))))

(ptui.ui.app:defapp panel-demo-app (:fps 20)
  (counter-panel
   (list "Alpha" "Beta" "Gamma" "Delta" "Epsilon"
         "Zeta" "Eta" "Theta" "Iota" "Kappa")
   "Panel Demo"))
```

Use this as the baseline pattern: `:state` + `:data` + `:layout` + `:keys`, then mount via `defapp`.

## 2) State and Data (`:state` for mutable, `:data` for derived, `:deps`)

`ptui/examples/focus-console.lisp` shows local mutable state:

```lisp
(:state
  (selected-task 0 :type fixnum)
  (minutes-elapsed 17 :type fixnum)
  (break-mode nil :type boolean))
```

`defpanel` generates setters like `set-selected-task` and `set-break-mode`, used directly in key handlers:

```lisp
(:enter (funcall set-break-mode (not break-mode)))
```

Derived values go in `:data`; recomputation is controlled by `:deps`:

```lisp
(:data
  (session-label (if break-mode
                     (format nil "~A | break window" session-name)
                     (format nil "~A | focus sprint" session-name))
    :deps (session-name break-mode))
  (task-count (length tasks) :deps (tasks)))
```

`ptui/examples/ops-wallboard.lisp` demonstrates chained derived values:

```lisp
(:data
  (filtered-services
   (remove-if-not (lambda (row) (%matches-filter-p row filter-mode))
                  services)
   :deps (services filter-mode))
  (service-count (length filtered-services) :deps (filtered-services)))
```

## 3) Effects (`:effects`, `:deps`, `:cleanup`)

`amoebum/src/ui/panels/stream-effects.lisp` is a clean production example:

```lisp
(:effects
  (drain-streams (%drain-stream-events chat-state)
    :deps (chat-state))
  (publish-summary (%publish-status-bar-stream-summary-if-needed chat-state)
    :deps (chat-state))
  (budget-warning (%emit-stream-budget-warning-if-needed chat-state)
    :deps (chat-state)))
```

`ptui/examples/ops-wallboard.lisp` shows effects for state clamping:

```lisp
(:effects
  (clamp-selection
    (when (and (plusp service-count)
               (> selected-index (1- service-count)))
      (funcall set-selected-index (1- service-count)))
    :deps (selected-index service-count))
  (reset-empty-selection
    (when (zerop service-count)
      (funcall set-selected-index 0))
    :deps (service-count)))
```

Cleanup is optional and supported by the DSL contract:

```lisp
;; from ptui/test/panel-test.lisp
(:effects
  (tracked (progn
             (push (list 'effect label) *cleanup-log*)
             (lambda () (push (list 'cleanup label) *cleanup-log*)))
    :deps (label)
    :cleanup nil))
```

## 4) Layout (`:column`, `:fixed`/`:flex`/`:percentage`, `:when`)

Column layout with fixed and flex regions (from `panel-demo`):

```lisp
(:layout
  (:column
    (header :fixed 1 ...)
    (content :flex 1 ...)
    (footer :fixed 1 ...)))
```

Row layout (from `ptui/examples/release-tracker.lisp`):

```lisp
(:layout
  (:row
    (queued-col :flex 1 ...)
    (active-col :flex 1 ...)
    (done-col :flex 1 ...)))
```

Conditional regions with `:when` are used heavily in chat UI composition (`amoebum/src/ui/panels/chat-panel.lisp`):

```lisp
(:layout
  (:column
    (provider :fixed 5 :when provider-visible-p ...)
    (tree :fixed 10 :when tree-active-p ...)
    (plan :fixed 12 :when plan-active-p ...)
    (history :flex 1 ...)
    (approval :fixed 4 :when approval-active-p ...)
    (picker :fixed 8 :when picker-active-p ...)
    (stream-hint :fixed 1 :when stream-active-p ...)
    (input :fixed 3 ...)
    (status :fixed 1 ...)))
```

`defpanel` also supports percentage sizing in region specs:

```lisp
(:layout
  (:row
    (left :percentage 30 ...)
    (right :percentage 70 ...)))
```

## 5) Key Handling (flat keys, modal `:mode` groups)

Flat key map style from `amoebum/src/ui/panels/prompt-input.lisp`:

```lisp
(:keys
  (:text (chat-panel-handle-input-key
           chat-state :text
           (ptui.core.events:key-event-text? ptui.ui.panel::event)
           inner-width))
  (:enter (chat-panel-handle-input-key chat-state :enter nil inner-width))
  (:backspace (chat-panel-handle-input-key chat-state :backspace nil inner-width))
  (:up (chat-panel-handle-input-key chat-state :up nil inner-width))
  (:down (chat-panel-handle-input-key chat-state :down nil inner-width))
  (:ctrl-k (chat-panel-handle-input-key chat-state :ctrl-k nil inner-width)))
```

Modal key groups from `amoebum/src/ui/panels/chat-panel.lisp`:

```lisp
(:keys
  (:mode :approval :when approval-active-p
    (:left (approval-dialog-handle-key! approval-state :left))
    (:right (approval-dialog-handle-key! approval-state :right))
    (:enter (approval-dialog-handle-key! approval-state :enter)))
  (:mode :picker :when picker-active-p
    (:up (chat-panel-handle-fuzzy-picker-key chat-state :up))
    (:down (chat-panel-handle-fuzzy-picker-key chat-state :down)))
  (:mode :default
    (:up (let ((has-text (plusp (length (chat-ui-state-input-text chat-state)))))
           (if has-text
               (chat-panel-handle-input-key chat-state :up nil inner-width)
               (chat-ui-scroll-history chat-state 1))))
    (:down (let ((has-text (plusp (length (chat-ui-state-input-text chat-state)))))
             (if has-text
                 (chat-panel-handle-input-key chat-state :down nil inner-width)
                 (chat-ui-scroll-history chat-state -1))))))
```

Modes are evaluated in order; first active mode wins.

## 6) Context (`provide-context` / `use-context`)

`defpanel` supports context wiring with `:context` entries:

```lisp
;; from ptui/test/panel-test.lisp
(ptui.ui.panel:defpanel context-provider-panel (theme-value)
  (:context
    (theme :provide theme-value))
  (:layout
    (:column
      (main :flex 1
        (ptui.widgets.core:make-text-widget "provider")))))

(ptui.ui.panel:defpanel context-consumer-panel ()
  (:context
    (current-theme :from theme))
  (:layout
    (:column
      (main :flex 1
        (ptui.widgets.core:make-text-widget
         (format nil "theme: ~A" current-theme))))))
```

Equivalent direct hook API:

```lisp
(ptui.ui.hooks:provide-context 'theme :dark)
(ptui.ui.hooks:use-context 'theme) ; => :dark
```

## 7) Composition (`embed-panel`, `panel-slot`)

Child panel composition via `embed-panel`:

```lisp
;; from ptui/test/panel-test.lisp
(ptui.ui.panel:embed-panel slot-child-panel :title "Hello")
```

Slot defaults and overrides:

```lisp
;; host panel
(:slots
 (sidebar :default (ptui.widgets.core:make-text-widget "Default Sidebar")))

(:layout
  (:column
    (header :fixed 1 (ptui.widgets.core:make-text-widget label))
    (sidebar-region :flex 1 sidebar)))

;; render-time override
(render-slot-host-panel
 "Main"
 :sidebar (ptui.widgets.core:make-text-widget "Custom Sidebar"))
```

If a slot argument is omitted, its `:default` value is used.

## 8) Testing Patterns

Use `ptui/test/panel-test.lisp` as the main regression map for `defpanel`.
High-signal tests by feature:

- State/data: `defpanel-state-vars-accessible`, `defpanel-data-recomputes-on-dep-change`
- Effects: `defpanel-effects-run-on-render`, `defpanel-effects-cleanup-compiles`
- Layout/conditionals: `compile-layout-produces-constraint-element`, `defpanel-conditional-region-present-when-truthy`
- Keys: `defpanel-modal-keys-first-active-wins`, `defpanel-modal-keys-fallthrough-to-default`
- Context: `defpanel-context-parent-to-child-propagation`
- Composition: `embed-panel-expands-to-child-render-call`, `panel-slot-override-overrides`

For tranche verification, run the PTUI gate sequence:

```bash
./ptui/bin/check-systems.sh
./ptui/bin/test.sh
./ptui/bin/build.sh
PTUI_EXIT_AFTER_MS=500 ./ptui/dist/metrics-dashboard
PTUI_DASHBOARD_MODE=legacy PTUI_EXIT_AFTER_MS=500 ./ptui/dist/metrics-dashboard
PTUI_EXIT_AFTER_MS=500 ./ptui/dist/atop-dashboard
./ptui/bin/compliance-gate.sh
```
