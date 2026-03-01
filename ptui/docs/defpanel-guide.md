# PTUI defpanel Consumer Guide (I319)

This guide is written for consumers of PTUI `defpanel`, with practical examples from:
- `ptui/examples/`
- `amoebum/src/ui/panels/`

## 1) Quick Start

Start from `ptui/examples/panel-demo.lisp` and wire the panel into an app:

```lisp
(ptui.ui.panel:defpanel counter-panel (items title)
  (:state
    (selected-index 0 :type fixnum))
  (:data
    (item-count (length items) :deps (items))
    (status-text (format nil "~A | ~D items" title item-count)
                 :deps (title item-count)))
  (:layout
    (:column
      (header :fixed 1
        (ptui.widgets.core:make-text-widget
         (format nil "=== ~A ===" title)))
      (content :flex 1
        (ptui.views:list-view items #'%render-counter-item 10 nil selected-index nil))
      (footer :fixed 1
        (ptui.views:status-bar
         (list :left status-text
               :right (format nil "sel:~D" selected-index))
         nil nil))))
  (:keys
    (:up (funcall set-selected-index (max 0 (1- selected-index))))
    (:down (funcall set-selected-index (min (1- item-count) (1+ selected-index)))))

(ptui.ui.app:defapp panel-demo-app (:fps 20)
  (counter-panel
   (list "Alpha" "Beta" "Gamma" "Delta" "Epsilon"
         "Zeta" "Eta" "Theta" "Iota" "Kappa")
   "Panel Demo"))
```

## 2) State and Data: `:state`, `:data`, `:deps`

`:`state` stores mutable panel-local bindings and expands to `use-state`.

```lisp
(:state
  (selected-index 0 :type fixnum)
  (filter-mode :all :type keyword))
```

You can call the generated setter inside handlers:

```lisp
(:down (funcall set-selected-index (min (1- service-count) (1+ selected-index))))
```

`:data` is derived and memoized. Provide dependency vectors with `:deps` to control
when it recomputes.

```lisp
(:data
  (session-label (if break-mode
                     (format nil "~A | break window" session-name)
                     (format nil "~A | focus sprint" session-name))
                :deps (session-name break-mode))
  (task-count (length tasks) :deps (tasks)))

(:data
  (service-count (length filtered-services) :deps (filtered-services)))
```

If a data binding omits `:deps`, PTUI warns because it cannot infer the minimal
invalidation set safely.

## 3) Effects (`:effects`, `:deps`, `:cleanup`)

`:`effects` entries compile into `ptui.ui.hooks:use-effect`.

```lisp
(:effects
  (sync-approval (%sync-pending-approval-dialog! chat-state)
    :deps (chat-state))
  (drain-streams (%drain-stream-events chat-state)
    :deps (chat-state))
  (budget-warning (%emit-stream-budget-warning-if-needed chat-state)
    :deps (chat-state)))
```

You can also bind effects to local data lifecycle, including clamping behavior:

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

The effect compiler supports optional `:cleanup` forms for teardown.

```lisp
;; shape accepted by the compiler
(name body-expression :deps (...) :cleanup cleanup-expression)
```

## 4) Layout (`:column`, `:row`, `:fixed`, `:flex`, `:percentage`, `:when`)

Use `:column` or `:row` as the root and size each region with one of:
`:`fixed`, `:flex`, or `:percentage`.

```lisp
(:column
  (hero :fixed 1 ...)
  (tasks-region :flex 1 ...)
  (footer :fixed 1 ...))
```

`:percentage` is available from the DSL implementation:

```lisp
(panel-left :percentage 30 ...)
(panel-right :percentage 70 ...)
```

Conditionals are expressed with `:when` on a region:

```lisp
(:column
  (tree :fixed 10 :when tree-active-p
    (make-tree-browser-widget tree-state))
  (input :fixed 4 ...))
```

When a predicate is false, that region is dropped and the resulting constraints are
recomputed each render.

## 5) Key handling (flat keys, modal `:mode` groups)

Flat key maps: direct key-handler pairs.

```lisp
(:keys
  (:up (funcall set-selected-index (max 0 (1- selected-task))))
  (:down (funcall set-selected-index (min (1- task-count) (1+ selected-task))))
  (:enter (funcall set-break-mode (not break-mode))))
```

Modal handling groups with `:mode`, optional `:when`, plus `:mode :default` fallback.

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
           (unless has-text (chat-ui-scroll-history chat-state 1))))
    (:down (let ((has-text (plusp (length (chat-ui-state-input-text chat-state)))))
             (unless has-text (chat-ui-scroll-history chat-state -1))))))
```

Mode clauses are tested in order and the first active mode handles the key.

## 6) Context (`provide-context`, `use-context`)

`:`context` is ambient data plumbing and supports `:provide` / `:from`.

```lisp
(:context
  (theme :provide theme-value)
  (current-theme :from theme))
```

Provide-first, consume-later semantics are enforced by the compiler while wiring
panel invocation.

## 7) Composition (`embed-panel`, `panel-slot`)

Use `embed-panel` to invoke another panel from layout:

```lisp
(ptui.ui.panel:embed-panel slot-child-panel :title "Hello")
```

`panel-slot` in this tranche refers to the slot contract driven by `:slots` and
`&key` arguments.

```lisp
(:slots
  (sidebar :default (ptui.widgets.core:make-text-widget "Default Sidebar")))

(sidebar-region :flex 1 sidebar)
```

Render-time override:

```lisp
(render-slot-host-panel
  "Main"
  :sidebar (ptui.widgets.core:make-text-widget "Custom Sidebar"))
```

If omitted, the `:default` value is used.

## 8) Testing patterns

Use panel-suite regression tests in `ptui/test/panel-test.lisp` and PTUI test
commands.

```bash
./ptui/bin/test.sh
./ptui/bin/compliance-gate.sh
```

Useful targeted checks:

- `defpanel-state-vars-accessible`
- `defpanel-data-recomputes-on-dep-change`
- `defpanel-effects-run-on-render`
- `compile-layout-produces-constraint-element`
- `defpanel-conditional-region-present-when-truthy`
- `defpanel-modal-keys-first-active-wins`
- `defpanel-context-parent-to-child-propagation`
- `panel-slot-override-overrides`
