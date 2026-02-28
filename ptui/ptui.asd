(asdf:defsystem "ptui/caps"
  :description "PTUI terminal capability probe (pure env parsing)"
  :author "Ralph"
  :license "MIT"
  :depends-on ()
  :serial t
  :components
  ((:file "src/term/caps")))

(asdf:defsystem "ptui/core"
  :description "PTUI core types/events/color"
  :author "Ralph"
  :license "MIT"
  :depends-on ("ptui/caps")
  :serial t
  :components
  ((:file "src/core/color")
   (:file "src/core/types")
   (:file "src/core/events")))

(asdf:defsystem "ptui/util"
  :description "PTUI utilities (log/time)"
  :author "Ralph"
  :license "MIT"
  :depends-on ()
  :serial t
  :components
  ((:file "src/util/log")
   (:file "src/util/time")))

(asdf:defsystem "ptui/search"
  :description "PTUI file-set search primitives (glob matcher/scan engine)"
  :author "Ralph"
  :license "MIT"
  :depends-on ("cl-ppcre")
  :serial t
  :components
  ((:file "src/search/glob")
   (:file "src/search/engine")))

(asdf:defsystem "ptui/runtime"
  :description "PTUI runtime primitives (queue/scheduler)"
  :author "Ralph"
  :license "MIT"
  :depends-on ("bordeaux-threads" "ptui/util")
  :serial t
  :components
  ((:file "src/runtime/queue")
   (:file "src/runtime/event-bus")
   (:file "src/runtime/event-filters")
   (:file "src/runtime/scheduler")))

(asdf:defsystem "ptui/term"
  :description "PTUI terminal integration (caps/tty/signals/input)"
  :author "Ralph"
  :license "MIT"
  :depends-on ("cffi" "ptui/caps" "ptui/core" "ptui/util")
  :serial t
  :components
  ((:file "src/term/tty")
   (:file "src/term/signals")
   (:file "src/term/input")))

(asdf:defsystem "ptui/render"
  :description "PTUI renderer (cell buffer + diff)"
  :author "Ralph"
  :license "MIT"
  :depends-on ("ptui/core" "ptui/text")
  :serial t
  :components
  ((:file "src/render/buffer")
   (:file "src/render/diff")))

(asdf:defsystem "ptui/text"
  :description "PTUI text pipeline (grapheme/width/layout)"
  :author "Ralph"
  :license "MIT"
  :depends-on ()
  :serial t
  :components
  ((:file "src/text/engine")
   (:file "src/text/adapters/fallback")
   (:file "src/text/adapters/native")
   (:file "src/text/grapheme")
   (:file "src/text/width")
   (:file "src/text/layout")))


(asdf:defsystem "ptui/layout"
  :description "PTUI layout foundation (node contracts + deterministic solver)"
  :author "Ralph"
  :license "MIT"
  :depends-on ("ptui/text")
  :serial t
  :components
  ((:file "src/layout/api")))

(asdf:defsystem "ptui/constraints"
  :description "PTUI constraint-based layout (specs + solver + constraint nodes)"
  :author "Ralph"
  :license "MIT"
  :depends-on ("ptui/layout")
  :serial t
  :components
  ((:file "src/layout/constraints")
   (:file "src/layout/solver")
   (:file "src/layout/constraint-layout")))

(asdf:defsystem "ptui/layout/yoga"
  :description "PTUI optional Yoga adapter boundary (feature-gated)"
  :author "Ralph"
  :license "MIT"
  :depends-on ("ptui/layout")
  :serial t
  :components
  (#+ptui-layout-yoga (:file "src/layout/yoga")))

(asdf:defsystem "ptui/ui"
  :description "PTUI component runtime core (tree/reconcile/lifecycle/focus)"
  :author "Ralph"
  :license "MIT"
  :depends-on ("ptui/core" "ptui/layout")
  :serial t
  :components
  ((:file "src/ui/elements")
   (:file "src/ui/runtime")))

(asdf:defsystem "ptui/hooks"
  :description "PTUI React-like hooks (use-state/use-effect/use-memo)"
  :author "Ralph"
  :license "MIT"
  :depends-on ("ptui/widgets")
  :serial t
  :components
  ((:file "src/ui/hooks")))

(asdf:defsystem "ptui/app"
  :description "PTUI app shell (defapp macro with lifecycle/interceptors)"
  :author "Ralph"
  :license "MIT"
  :depends-on ("ptui/hooks" "ptui/widgets" "ptui/engine" "ptui/render")
  :serial t
  :components
  ((:file "src/ui/app")))

(asdf:defsystem "ptui/widgets"
  :description "PTUI reusable widget primitives"
  :author "Ralph"
  :license "MIT"
  :depends-on ("ptui/ui" "ptui/layout" "ptui/text")
  :serial t
  :components
  ((:file "src/widgets/core")
   (:file "src/widgets/defwidget")))

(asdf:defsystem "ptui/views"
  :description "PTUI view primitives (list-view, text-input, status-bar)"
  :author "Ralph"
  :license "MIT"
  :depends-on ("ptui/widgets" "ptui/hooks" "ptui/constraints")
  :serial t
  :components
  ((:file "src/views/primitives")
   (:file "src/views/paint")))

(asdf:defsystem "ptui/panel"
  :description "PTUI defpanel DSL macro"
  :author "Ralph"
  :license "MIT"
  :depends-on ("ptui/views" "ptui/hooks" "ptui/constraints" "ptui/app")
  :serial t
  :components
  ((:file "src/ui/panel")))

(asdf:defsystem "ptui/components"
  :description "PTUI higher-level composable widgets"
  :author "Ralph"
  :license "MIT"
  :depends-on ("ptui/widgets" "ptui/search")
  :serial t
  :components
  ((:file "src/components/prompt-box")
   (:file "src/components/ansi-parser")
   (:file "src/components/terminal-pane")
   (:file "src/components/plan-presentation")
   (:file "src/components/list-selection")
   (:file "src/components/glob-widget")
   (:file "src/components/search-widget")))

(asdf:defsystem "ptui/backend"
  :description "PTUI backend boundary and implementations"
  :author "Ralph"
  :license "MIT"
  :depends-on ("ptui/core" "ptui/term" "ptui/render" "ptui/util"
               #+ptui-ncurses "cl-charms")
  :serial t
  :components
  ((:file "src/backend/protocol")
   (:file "src/backend/ansi")
   #+ptui-ncurses (:file "src/backend/ncurses")))

(asdf:defsystem "ptui/engine"
  :description "PTUI engine loop"
  :author "Ralph"
  :license "MIT"
  :depends-on ("ptui/backend" "ptui/runtime" "ptui/util" "ptui/render")
  :serial t
  :components
  ((:file "src/engine/loop")))

(asdf:defsystem "ptui/standalone"
  :description "PTUI standalone system (monolithic; no internal ptui/* dependencies)"
  :author "Ralph"
  :license "MIT"
  :depends-on ("cffi" "bordeaux-threads" "cl-ppcre"
               #+ptui-ncurses "cl-charms")
  :serial t
  :components
  ((:file "src/term/caps")
   (:file "src/core/color")
   (:file "src/core/types")
   (:file "src/core/events")
   (:file "src/util/log")
   (:file "src/util/time")
   (:file "src/search/engine")
   (:file "src/runtime/queue")
   (:file "src/runtime/event-bus")
   (:file "src/runtime/event-filters")
   (:file "src/runtime/scheduler")
   (:file "src/term/tty")
   (:file "src/term/signals")
   (:file "src/term/input")
   (:file "src/text/engine")
   (:file "src/text/adapters/fallback")
   (:file "src/text/adapters/native")
   (:file "src/text/grapheme")
   (:file "src/text/width")
   (:file "src/text/layout")
   (:file "src/search/glob")
   (:file "src/layout/api")
   #+ptui-layout-yoga (:file "src/layout/yoga")
   (:file "src/ui/elements")
   (:file "src/ui/runtime")
   (:file "src/widgets/core")
   (:file "src/widgets/defwidget")
   (:file "src/render/buffer")
   (:file "src/render/diff")
   (:file "src/backend/protocol")
   (:file "src/backend/ansi")
   #+ptui-ncurses (:file "src/backend/ncurses")
   (:file "src/engine/loop")
   (:file "src/layout/constraints")
   (:file "src/layout/solver")
   (:file "src/layout/constraint-layout")
   (:file "src/ui/hooks")
   (:file "src/ui/app")
   (:file "src/views/primitives")
   (:file "src/views/paint")
   (:file "src/ui/panel")))

(asdf:defsystem "ptui/components-standalone"
  :description "PTUI component library on top of ptui/standalone"
  :author "Ralph"
  :license "MIT"
  :depends-on ("ptui/standalone")
  :serial t
  :components
  ((:file "src/components/prompt-box")
   (:file "src/components/ansi-parser")
   (:file "src/components/terminal-pane")
   (:file "src/components/plan-presentation")
   (:file "src/components/list-selection")
   (:file "src/components/glob-widget")
   (:file "src/components/search-widget")))

(asdf:defsystem "ptui"
  :description "PTUI umbrella system"
  :author "Ralph"
  :license "MIT"
  :depends-on ("ptui/core" "ptui/util" "ptui/search" "ptui/runtime" "ptui/term" "ptui/text" "ptui/layout" "ptui/constraints" "ptui/ui" "ptui/widgets" "ptui/render" "ptui/backend" "ptui/engine" "ptui/hooks" "ptui/app" "ptui/views" "ptui/panel")
  :serial t
  :components ())

(asdf:defsystem "ptui/examples"
  :description "PTUI examples"
  :author "Ralph"
  :license "MIT"
  :depends-on ("ptui" "ptui/components")
  :serial t
  :components
  ((:file "examples/metrics-dashboard")
   (:file "examples/atop-dashboard")
   (:file "examples/panel-demo")))

(asdf:defsystem "ptui/examples-standalone"
  :description "PTUI examples (standalone dependency)"
  :author "Ralph"
  :license "MIT"
  :depends-on ("ptui/components-standalone")
  :serial t
  :components
  ((:file "examples/metrics-dashboard")
   (:file "examples/atop-dashboard")))

(asdf:defsystem "ptui/test-support"
  :description "PTUI snapshot testing framework"
  :author "Ralph"
  :license "MIT"
  :depends-on ("ptui/backend" "ptui/render" "ptui/core")
  :serial t
  :components
  ((:file "src/backend/test")
   (:file "src/test-support/snapshot")
   (:file "src/test-support/harness")))

(asdf:defsystem "ptui/tests"
  :description "PTUI FiveAM suites"
  :author "Ralph"
  :license "MIT"
  :depends-on ("ptui" "ptui/components" "ptui/test-support" "fiveam")
  :serial t
  :components
  ((:file "test/defwidget-test")
   (:file "test/snapshot-test")
   (:file "test/event-filters-test")
   (:file "test/ansi-parser-test")
   (:file "test/queue-test")
   (:file "test/search-test")
   (:file "test/scheduler-test")
   (:file "test/list-selection-test")
   (:file "test/hooks-test")
   (:file "test/app-test")
   (:file "test/event-routing-test")
   (:file "test/constraints-test")
   (:file "test/views-test")
   (:file "test/render-test")
   (:file "test/text-test")
   (:file "test/backend-test")
   (:file "test/input-test")
   (:file "test/panel-test")
   (:file "test/engine-test")))
