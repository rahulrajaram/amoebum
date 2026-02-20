(asdf:defsystem "pseudopod"
  :description "Common Lisp client for Moonshot/Kimi API — amoebum's reach into the Moonshot ecosystem."
  :author "amoebum"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("uiop" "dexador" "jonathan" "usocket" "babel" "cl-ppcre")
  :serial t
  :components
  ((:file "src/package")
   (:file "src/errors")
   (:file "src/model/message")
   (:file "src/model/model-info")
   (:file "src/model/file-object")
   (:file "src/tooling/registry")
   (:file "src/tooling/file-read")
   (:file "src/tooling/structured-read")
   (:file "src/tooling/write-file")
   (:file "src/tooling/string-replace")
   (:file "src/tooling/run-command")
   (:file "src/tooling/search-backend")
   (:file "src/client")
   (:file "src/providers/protocol")
   (:file "src/providers/kimi")
   (:file "src/providers/anthropic")
   (:file "src/providers/openai-compat")
   (:file "src/providers/router")
   (:file "src/agent/generate")
   (:file "src/agent/conversation"))
  :in-order-to ((asdf:test-op (asdf:test-op "pseudopod/test"))))

(asdf:defsystem "pseudopod/test"
  :description "Test suite for pseudopod SDK"
  :depends-on ("pseudopod" "fiveam" "jonathan")
  :serial t
  :components
  ((:file "test/suite")
   (:file "test/provider-test")
   (:file "test/router-test")
   (:file "test/file-read-test")
   (:file "test/structured-read-test")
   (:file "test/write-file-test")
   (:file "test/string-replace-test")
   (:file "test/run-command-test")
   (:file "test/search-backend-test"))
  :perform (asdf:test-op (op c)
             (declare (ignore op c))
             (uiop:symbol-call :pseudopod/test :run-all)))
