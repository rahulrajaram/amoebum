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

(asdf:defsystem "ptui/runtime"
  :description "PTUI runtime primitives (queue/scheduler)"
  :author "Ralph"
  :license "MIT"
  :depends-on ("bordeaux-threads" "ptui/util")
  :serial t
  :components
  ((:file "src/runtime/queue")
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
  :depends-on ("ptui/core")
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

(asdf:defsystem "ptui/widgets"
  :description "PTUI reusable widget primitives"
  :author "Ralph"
  :license "MIT"
  :depends-on ("ptui/ui" "ptui/layout" "ptui/text")
  :serial t
  :components
  ((:file "src/widgets/core")))

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
  :depends-on ("cffi" "bordeaux-threads"
               #+ptui-ncurses "cl-charms")
  :serial t
  :components
  ((:file "src/term/caps")
   (:file "src/core/color")
   (:file "src/core/types")
   (:file "src/core/events")
   (:file "src/util/log")
   (:file "src/util/time")
   (:file "src/runtime/queue")
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
   (:file "src/layout/api")
   #+ptui-layout-yoga (:file "src/layout/yoga")
   (:file "src/ui/elements")
   (:file "src/ui/runtime")
   (:file "src/widgets/core")
   (:file "src/render/buffer")
   (:file "src/render/diff")
   (:file "src/backend/protocol")
   (:file "src/backend/ansi")
   #+ptui-ncurses (:file "src/backend/ncurses")
   (:file "src/engine/loop")))

(asdf:defsystem "ptui"
  :description "PTUI umbrella system"
  :author "Ralph"
  :license "MIT"
  :depends-on ("ptui/core" "ptui/util" "ptui/runtime" "ptui/term" "ptui/text" "ptui/layout" "ptui/ui" "ptui/widgets" "ptui/render" "ptui/backend" "ptui/engine")
  :serial t
  :components ())

(asdf:defsystem "ptui/examples"
  :description "PTUI examples"
  :author "Ralph"
  :license "MIT"
  :depends-on ("ptui")
  :serial t
  :components
  ((:file "examples/metrics-dashboard")))

(asdf:defsystem "ptui/examples-standalone"
  :description "PTUI examples (standalone dependency)"
  :author "Ralph"
  :license "MIT"
  :depends-on ("ptui/standalone")
  :serial t
  :components
  ((:file "examples/metrics-dashboard")))
