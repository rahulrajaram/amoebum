(asdf:defsystem "ptui"
  :description "Portable terminal UI kernel"
  :author "Ralph"
  :license "MIT"
  :depends-on
  ("cffi"
   "bordeaux-threads"
   #+ptui-ncurses "cl-charms")
  :serial t
  :components
  ((:file "src/term/caps")
   (:file "src/core/color")
   (:file "src/core/types")
   (:file "src/core/events")
   (:file "src/render/buffer")
   (:file "src/render/diff")
   (:file "src/util/log")
   (:file "src/util/time")
   (:file "src/runtime/queue")
   (:file "src/runtime/scheduler")
   (:file "src/term/tty")
   (:file "src/term/signals")
   (:file "src/term/input")
   (:file "src/backend/protocol")
   (:file "src/backend/ansi")
   #+ptui-ncurses (:file "src/backend/ncurses")
   (:file "src/engine/loop")))

(asdf:defsystem "ptui-examples"
  :description "PTUI examples"
  :depends-on ("ptui")
  :serial t
  :components
  ((:file "examples/metrics-dashboard")))
