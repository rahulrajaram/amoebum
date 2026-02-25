(in-package :amoebum/test)

(def-suite method-combination-dispatch-suite :in amoebum-suite
  :description "Execute-tool method combination dispatch ordering tests (I208).")

(in-suite method-combination-dispatch-suite)

(test execute-tool-method-combination-order
  (let ((original-toolset amoebum:*toolset*)
        (original-hooks amoebum:*hook-registry*)
        (original-event-bus amoebum:*event-bus*)
        (order nil)
        (saw-retry nil)
        (saw-skip nil))
    (unwind-protect
        (progn
          (setf amoebum:*toolset* (pseudopod:make-toolset)
                amoebum:*hook-registry* (make-hash-table :test #'equal)
                amoebum:*event-bus* (amoebum:make-event-bus :capacity 32))
          (pseudopod:register-tool-function
           amoebum:*toolset*
           :name "i208-dispatch-order"
           :description "I208 method combination order probe."
           :parameters (let ((schema (make-hash-table :test #'equal)))
                         (setf (gethash "type" schema) "object")
                         schema)
           :fn (lambda (_arguments _call)
                 (declare (ignore _arguments _call))
                 (let ((restart-names (mapcar #'restart-name (compute-restarts))))
                   (setf saw-retry (member 'retry-tool restart-names)
                         saw-skip (member 'skip-tool restart-names)))
                 (push :primary order)
                 "primary-ok"))
          (amoebum:register-hook :pre-tool-use
                                 'i208-dispatch-pre
                                 (lambda (_tool-name _args)
                                   (declare (ignore _tool-name _args))
                                   (push :before order)
                                   :allow))
          (amoebum:register-hook :post-tool-use
                                 'i208-dispatch-post
                                 (lambda (_tool-name _result _elapsed-ms)
                                   (declare (ignore _tool-name _result _elapsed-ms))
                                   (push :after order)
                                   nil))
          (let* ((context (amoebum:make-amoebum-context
                           :toolset amoebum:*toolset*
                           :permission-mode :full-auto
                           :event-bus amoebum:*event-bus*
                           :hook-registry amoebum:*hook-registry*
                           :logger (lambda (_fmt &rest _args)
                                     (declare (ignore _fmt _args))
                                     (push :log order))
                           :initialize-notifications-p nil))
                 (call (pseudopod:make-tool-call
                        :id "i208-call"
                        :name "i208-dispatch-order"
                        :arguments "{}"))
                 (result (amoebum:execute-tool call context)))
            (is (string= result "primary-ok"))
            (is (equal (nreverse order) '(:log :before :primary :after)))
            (is-true saw-retry)
            (is-true saw-skip)))
      (setf amoebum:*toolset* original-toolset
            amoebum:*hook-registry* original-hooks
            amoebum:*event-bus* original-event-bus))))
