(asdf:defsystem "pseudopod"
  :description "Common Lisp client for Moonshot/Kimi API — amoebum's reach into the Moonshot ecosystem."
  :author "amoebum"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("uiop" "dexador" "jonathan")
  :serial t
  :components
  ((:file "src/package")
   (:file "src/client")))
