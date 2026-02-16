;;;; sw4rm-sdk.asd - ASDF system for SW4RM SDK (amoebum-adapted, no gRPC)

(asdf:defsystem #:sw4rm-sdk
  :description "SW4RM Protocol SDK for Common Lisp — local-mode for amoebum"
  :version "0.6.0"
  :author "SW4RM Team + amoebum"
  :license "Apache-2.0"
  :depends-on (#:alexandria
               #:bordeaux-threads
               #:local-time
               #:ironclad
               #:cl-json
               #:uuid
               #:jonathan
               #:cl-ppcre
               #:split-sequence)
  :serial t
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "constants")
                             (:file "errors")
                             (:file "config")
                             (:file "local-registry")
                             (:file "envelope")
                             (:file "state-machine")
                             (:file "activity-buffer")
                             (:file "worktree-state")
                             (:file "voting")
                             (:file "audit")
                             (:file "secrets")
                             (:file "persistence")
                             (:file "ack-manager")
                             (:file "control")
                             (:file "interceptors")
                             (:file "negotiation-events")
                             (:file "policy-store")))
               (:module "clients"
                :pathname "src/clients"
                :depends-on ("src")
                :serial t
                :components ((:file "base")
                             (:file "handoff")
                             (:file "gateway")
                             (:file "workflow")
                             (:file "negotiation-room")
                             (:file "negotiation-room-store"))))
  :in-order-to ((asdf:test-op (asdf:test-op #:sw4rm-sdk/tests))))

(asdf:defsystem #:sw4rm-sdk/tests
  :description "Test suite for SW4RM SDK (amoebum-adapted)"
  :depends-on (#:sw4rm-sdk
               #:fiveam)
  :components ((:module "test"
                :serial t
                :components ((:file "suite")
                             (:file "integration-test")
                             (:file "local-registry-test"))))
  :perform (asdf:test-op (o c)
             (let* ((suite-package (or (find-package :sw4rm-test)
                                       (error "SW4RM test package missing")))
                    (suite-symbol (or (find-symbol "SW4RM-SUITE" suite-package)
                                      (error "SW4RM suite symbol missing"))))
               (uiop:symbol-call :fiveam '#:run! suite-symbol))))
