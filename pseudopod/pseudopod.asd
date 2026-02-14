(asdf:defsystem "pseudopod"
  :description "Common Lisp client for Moonshot/Kimi API — amoebum's reach into the Moonshot ecosystem."
  :author "amoebum"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("uiop" "dexador" "jonathan")
  :serial t
  :components
  ((:file "src/package")
   (:file "src/errors")
   (:file "src/model/message")
   (:file "src/model/model-info")
   (:file "src/model/file-object")
   (:file "src/tooling/registry")
   (:file "src/client")
   (:file "src/agent/generate")
   (:file "src/agent/conversation")))
