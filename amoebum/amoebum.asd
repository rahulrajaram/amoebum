(asdf:defsystem "amoebum"
  :description "amoebum core application layer"
  :author "amoebum"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("pseudopod" "ptui" "ptui/components" "uiop" "cl-ppcre" "bordeaux-threads" "named-readtables")
  :serial t
  :components
  ((:file "src/package")
   (:file "src/events")
   (:file "src/events/filters")
   (:file "src/config")
   (:file "src/context")
   (:file "src/conversation")
   (:file "src/memory")
   (:file "src/memory/haake-adapter")
   (:file "src/plan-mode")
   (:file "src/agents")
   (:file "src/extensions")
   (:file "src/checkpoint")
   (:file "src/sounds")
   (:file "src/commands")
   (:file "src/permissions")
   (:file "src/sandbox")
   (:file "src/reader-macros")
   (:file "src/macros/deftool")
   (:file "src/macros/defhook")
   (:file "src/macros/defkeys")
   (:file "src/macros/defskill")
   (:file "src/compile-validation")
   (:file "src/conditions")
   (:file "src/mcp/jsonrpc")
   (:file "src/mcp/server")
   (:file "src/lsp/client")
   (:file "src/mcp/tools")
   (:file "src/notifications")
   (:file "src/pipeline")
   (:file "src/tools/files")
   (:file "src/tools/search")
   (:file "src/tools/web")
   (:file "src/tools/shell")
   (:file "src/tools/git")
   (:file "src/tools/lsp")
   (:file "src/widgets/fuzzy-picker")
   (:file "src/widgets/tree-browser")
   (:file "src/system-prompt")
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
