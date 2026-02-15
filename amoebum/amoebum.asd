(asdf:defsystem "amoebum"
  :description "amoebum core application layer"
  :author "amoebum"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("pseudopod" "ptui" "ptui/components" "uiop" "cl-ppcre" "bordeaux-threads")
  :serial t
  :components
  ((:file "src/package")
   (:file "src/events")
   (:file "src/config")
   (:file "src/memory")
   (:file "src/plan-mode")
   (:file "src/agents")
   (:file "src/commands")
   (:file "src/permissions")
   (:file "src/macros/deftool")
   (:file "src/macros/defhook")
   (:file "src/macros/defkeys")
   (:file "src/conditions")
   (:file "src/pipeline")
   (:file "src/tools/files")
   (:file "src/tools/search")
   (:file "src/tools/shell")
   (:file "src/ui/streaming")
   (:file "src/ui/status-bar")
   (:file "src/ui/chat")
   (:file "src/main"))
  :in-order-to ((asdf:test-op (asdf:test-op "amoebum/test"))))

(asdf:defsystem "amoebum/test"
  :description "FiveAM test suite for amoebum core application"
  :depends-on ("amoebum" "fiveam")
  :serial t
  :components
  ((:file "test/suite"))
  :perform (asdf:test-op (op c)
             (declare (ignore op c))
             (unless (uiop:symbol-call :amoebum/test :run-all)
               (error "Amoebum FiveAM suite failed."))))
