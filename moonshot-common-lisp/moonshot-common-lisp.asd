(asdf:defsystem "moonshot-common-lisp"
  :description "Thin Common Lisp client for Moonshot/Kimi OpenAI-compatible chat completions API."
  :author "amoebum"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("uiop" "dexador" "jonathan")
  :serial t
  :components
  ((:file "src/package")
   (:file "src/client")))
