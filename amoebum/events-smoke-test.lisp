(let* ((smoke-file (or *load-truename* *compile-file-truename*))
       (amoebum-dir (and smoke-file (make-pathname :name nil :type nil :defaults smoke-file)))
       (repo-root (and amoebum-dir (truename (merge-pathnames #P"../" amoebum-dir)))))
  (unless repo-root
    (error "Unable to resolve repository root from ~S" smoke-file))

  (load (merge-pathnames #P"ptui/.tools/quicklisp/setup.lisp" repo-root))
  (require :asdf)

  (let* ((asdf-pkg (or (find-package "ASDF")
                       (error "Missing package ASDF")))
         (load-asd-sym (or (find-symbol "LOAD-ASD" asdf-pkg)
                           (error "Missing symbol LOAD-ASD in ASDF package")))
         (load-system-sym (or (find-symbol "LOAD-SYSTEM" asdf-pkg)
                              (error "Missing symbol LOAD-SYSTEM in ASDF package")))
         (load-asd-fn (symbol-function load-asd-sym))
         (load-system-fn (symbol-function load-system-sym)))
    (funcall load-asd-fn (merge-pathnames #P"pseudopod/pseudopod.asd" repo-root))
    (funcall load-asd-fn (merge-pathnames #P"sw4rm-sdk/sw4rm-sdk.asd" repo-root))
    (funcall load-asd-fn (merge-pathnames #P"ptui/ptui.asd" repo-root))
    (funcall load-asd-fn (merge-pathnames #P"amoebum/amoebum.asd" repo-root))
    (funcall load-system-fn "amoebum"))

  (let* ((amoebum-pkg (or (find-package "AMOEBUM")
                          (error "Missing package AMOEBUM after load.")))
         (symbol-in
           (lambda (name package)
             (or (find-symbol name package)
                 (error "Missing symbol ~A in package ~A." name (package-name package)))))
         (fn
           (lambda (name)
             (symbol-function (funcall symbol-in name amoebum-pkg))))
         (make-event-bus-fn (funcall fn "MAKE-EVENT-BUS"))
         (subscribe-fn (funcall fn "SUBSCRIBE"))
         (unsubscribe-fn (funcall fn "UNSUBSCRIBE"))
         (publish-fn (funcall fn "PUBLISH"))
         (make-tool-invoked-event-fn (funcall fn "MAKE-TOOL-INVOKED-EVENT"))
         (make-mcp-tool-discovered-event-fn
          (funcall fn "MAKE-MCP-TOOL-DISCOVERED-EVENT"))
         (make-mcp-tool-invoked-event-fn
          (funcall fn "MAKE-MCP-TOOL-INVOKED-EVENT"))
         (make-memory-backend-selected-event-fn
          (funcall fn "MAKE-MEMORY-BACKEND-SELECTED-EVENT"))
         (make-config-changed-event-fn (funcall fn "MAKE-CONFIG-CHANGED-EVENT"))
         (event-history-fn (funcall fn "EVENT-HISTORY"))
         (event-payload-fn (funcall fn "EVENT-PAYLOAD"))
         (event-type-fn (funcall fn "EVENT-TYPE"))
         (event-seq-fn (funcall fn "EVENT-SEQ"))
         (setconfig-fn (funcall fn "SETCONFIG"))
         (config-changed-key-fn (funcall fn "CONFIG-CHANGED-PAYLOAD-KEY"))
         (config-changed-old-fn (funcall fn "CONFIG-CHANGED-PAYLOAD-OLD-VALUE"))
         (config-changed-new-fn (funcall fn "CONFIG-CHANGED-PAYLOAD-NEW-VALUE"))
         (config-changed-payload-p-fn (funcall fn "CONFIG-CHANGED-PAYLOAD-P"))
         (event-bus-sym (funcall symbol-in "*EVENT-BUS*" amoebum-pkg))
         (event-type-tool-invoked (symbol-value (funcall symbol-in "+EVENT-TYPE-TOOL-INVOKED+" amoebum-pkg)))
         (event-type-mcp-tool-discovered
          (symbol-value (funcall symbol-in "+EVENT-TYPE-MCP-TOOL-DISCOVERED+" amoebum-pkg)))
         (event-type-mcp-tool-invoked
          (symbol-value (funcall symbol-in "+EVENT-TYPE-MCP-TOOL-INVOKED+" amoebum-pkg)))
         (event-type-memory-backend-selected
          (symbol-value (funcall symbol-in "+EVENT-TYPE-MEMORY-BACKEND-SELECTED+" amoebum-pkg)))
         (event-type-config-changed (symbol-value (funcall symbol-in "+EVENT-TYPE-CONFIG-CHANGED+" amoebum-pkg)))
         (core-event-types (symbol-value (funcall symbol-in "+CORE-EVENT-TYPES+" amoebum-pkg))))
    (labels ((assert-true (condition format-string &rest format-args)
               (unless condition
                 (error (apply #'format nil format-string format-args)))))
      (assert-true (member event-type-tool-invoked core-event-types :test #'eq)
                   "Expected +CORE-EVENT-TYPES+ to include tool:invoked.")
      (assert-true (member event-type-config-changed core-event-types :test #'eq)
                   "Expected +CORE-EVENT-TYPES+ to include config:changed.")
      (assert-true (member event-type-mcp-tool-discovered core-event-types :test #'eq)
                   "Expected +CORE-EVENT-TYPES+ to include mcp:tool-discovered.")
      (assert-true (member event-type-mcp-tool-invoked core-event-types :test #'eq)
                   "Expected +CORE-EVENT-TYPES+ to include mcp:tool-invoked.")
      (assert-true (member event-type-memory-backend-selected core-event-types :test #'eq)
                   "Expected +CORE-EVENT-TYPES+ to include memory:backend-selected.")

      (let ((bus (funcall make-event-bus-fn :capacity 16))
            (dispatch-order '())
            (wildcard-events '())
            (secondary-events '()))
        (let ((secondary-id
                (funcall subscribe-fn
                         bus
                         event-type-tool-invoked
                         (lambda (event)
                           (setf secondary-events (append secondary-events (list event)))
                           (setf dispatch-order (append dispatch-order (list :secondary))))
                         :priority 50))
              (primary-id
                (funcall subscribe-fn
                         bus
                         event-type-tool-invoked
                         (lambda (event)
                           (declare (ignore event))
                           (setf dispatch-order (append dispatch-order (list :primary))))
                         :priority 10))
              (wildcard-id
                (funcall subscribe-fn
                         bus
                         :*
                         (lambda (event)
                           (setf wildcard-events (append wildcard-events (list event)))
                           (setf dispatch-order (append dispatch-order (list :wildcard))))
                         :priority 20)))
          (declare (ignore primary-id))
          (let ((seq (funcall publish-fn
                              bus
                              (funcall make-tool-invoked-event-fn
                                       :tool-name "read-file"
                                       :args '(:path "/tmp/example.lisp")
                                       :permission-mode :auto-edit
                                       :request-id "req-1"))))
            (assert-true (= seq 1)
                         "Expected first publish sequence number to be 1."))

          (assert-true (equal dispatch-order '(:primary :wildcard :secondary))
                       "Expected synchronous priority ordering to be respected.")
          (assert-true (= (length wildcard-events) 1)
                       "Expected wildcard subscriber to receive the published event.")
          (assert-true (= (length secondary-events) 1)
                       "Expected typed subscriber to receive the published event.")

          (assert-true (funcall unsubscribe-fn bus secondary-id)
                       "Expected unsubscribe to return true for active subscription.")
          (assert-true (not (funcall unsubscribe-fn bus secondary-id))
                       "Expected unsubscribe to return nil for unknown subscription.")

          (setf dispatch-order '())
          (funcall publish-fn bus event-type-tool-invoked :payload '(:ok t))
          (assert-true (equal dispatch-order '(:primary :wildcard))
                       "Expected unsubscribed handler to stop receiving events.")

          (funcall publish-fn
                   bus
                   (funcall make-mcp-tool-discovered-event-fn
                            :server-name "github"
                            :tool-name "create-issue"
                            :namespaced-name "mcp/github/create-issue"
                            :description "Create an issue."))
          (funcall publish-fn
                   bus
                   (funcall make-mcp-tool-invoked-event-fn
                            :server-name "github"
                            :tool-name "create-issue"
                            :namespaced-name "mcp/github/create-issue"
                            :args '(:title "Issue")
                            :request-id "mcp-evt-1"))
          (funcall publish-fn
                   bus
                   (funcall make-memory-backend-selected-event-fn
                            :backend :haake-cli
                            :reason :auto-detected-haake-cli
                            :requested-backend :auto))

          (let* ((history (funcall event-history-fn bus))
                 (latest (car (last history))))
            (assert-true (= (length history) 5)
                         "Expected event history to include typed + MCP + memory backend events.")
            (assert-true (eq (funcall event-type-fn latest) event-type-memory-backend-selected)
                         "Expected latest history event type to be memory:backend-selected.")
            (assert-true (= (funcall event-seq-fn latest) 5)
                         "Expected sequence number to increment monotonically.")))

        (setf (symbol-value event-bus-sym) bus)
        (let ((captured-payload nil))
          (funcall subscribe-fn
                   bus
                   event-type-config-changed
                   (lambda (event)
                     (setf captured-payload (funcall event-payload-fn event)))
                   :priority 5)
          (funcall setconfig-fn :model "i30-model")
          (assert-true captured-payload
                       "Expected setconfig to emit config:changed onto event bus.")
          (assert-true (funcall config-changed-payload-p-fn captured-payload)
                       "Expected config:changed payload to be strongly typed.")
          (assert-true (eq (funcall config-changed-key-fn captured-payload) :model)
                       "Expected config:changed payload key to match setconfig key.")
          (assert-true (string= (funcall config-changed-new-fn captured-payload) "i30-model")
                       "Expected config:changed payload new value to match setconfig.")
          (assert-true (stringp (funcall config-changed-old-fn captured-payload))
                       "Expected config:changed payload old value to be present as prior model."))))

  (format t "AMOEBUM_EVENTS_SMOKE_OK~%")))
