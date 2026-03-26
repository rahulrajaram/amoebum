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
         (uiop-pkg (or (find-package "UIOP")
                       (find-package "ASDF/UTILITY")
                       (error "Missing UIOP package after requiring ASDF.")))
         (symbol-in
           (lambda (name package)
             (or (find-symbol name package)
                 (error "Missing symbol ~A in package ~A." name (package-name package)))))
         (fn
           (lambda (name)
             (symbol-function (funcall symbol-in name amoebum-pkg))))
         (ensure-directory-pathname-fn
           (symbol-function (funcall symbol-in "ENSURE-DIRECTORY-PATHNAME" uiop-pkg)))
         (temporary-directory-fn
           (symbol-function (funcall symbol-in "TEMPORARY-DIRECTORY" uiop-pkg)))
         (load-config-fn (funcall fn "LOAD-CONFIG"))
         (setconfig-fn (funcall fn "SETCONFIG"))
         (make-event-bus-fn (funcall fn "MAKE-EVENT-BUS"))
         (publish-fn (funcall fn "PUBLISH"))
         (make-notification-manager-fn (funcall fn "MAKE-NOTIFICATION-MANAGER"))
         (stop-notification-manager-fn (funcall fn "STOP-NOTIFICATION-MANAGER"))
         (stop-notification-dispatcher-fn (funcall fn "STOP-NOTIFICATION-DISPATCHER"))
         (make-tool-completed-event-fn (funcall fn "MAKE-TOOL-COMPLETED-EVENT"))
         (make-tool-error-event-fn (funcall fn "MAKE-TOOL-ERROR-EVENT"))
         (make-permission-prompted-event-fn (funcall fn "MAKE-PERMISSION-PROMPTED-EVENT"))
         (current-config-sym (funcall symbol-in "*CURRENT-CONFIG*" amoebum-pkg))
         (event-bus-sym (funcall symbol-in "*EVENT-BUS*" amoebum-pkg))
         (command-prober-sym (funcall symbol-in "*NOTIFICATION-COMMAND-PROBER*" amoebum-pkg))
         (command-runner-sym (funcall symbol-in "*NOTIFICATION-COMMAND-RUNNER*" amoebum-pkg))
         (async-dispatch-sym (funcall symbol-in "*NOTIFICATION-ASYNC-DISPATCH-P*" amoebum-pkg))
         (manager-registry-sym (funcall symbol-in "*NOTIFICATION-MANAGER-REGISTRY*" amoebum-pkg))
         (tmp-root
           (funcall ensure-directory-pathname-fn
                    (merge-pathnames
                     (make-pathname :directory
                                    `(:relative ,(format nil "amoebum-i61-~A" (get-universal-time))))
                     (funcall temporary-directory-fn))))
         (project-root (merge-pathnames #P"project/" tmp-root))
         (sound-path (merge-pathnames #P"sounds/test.wav" tmp-root))
         (log-path (merge-pathnames #P"logs/notifications.jsonl" tmp-root))
         (original-config (symbol-value current-config-sym))
         (original-event-bus (symbol-value event-bus-sym))
         (original-command-prober (symbol-value command-prober-sym))
         (original-command-runner (symbol-value command-runner-sym))
         (original-async-dispatch (symbol-value async-dispatch-sym))
         (dispatcher-sym (funcall symbol-in "*NOTIFICATION-DISPATCHER*" amoebum-pkg))
         (original-dispatcher (symbol-value dispatcher-sym))
         (original-manager-registry (symbol-value manager-registry-sym))
         (desktop-run-fn-sym (funcall symbol-in "*DESKTOP-NOTIFICATION-RUN-COMMAND-FUNCTION*" amoebum-pkg))
         (original-desktop-run-fn (symbol-value desktop-run-fn-sym))
         (desktop-suppressed-sym (funcall symbol-in "*DESKTOP-NOTIFICATIONS-SUPPRESSED*" amoebum-pkg))
         (original-desktop-suppressed (symbol-value desktop-suppressed-sym)))
    (labels ((assert-true (condition format-string &rest format-args)
               (unless condition
                 (error (apply #'format nil format-string format-args))))
             (set-notification-config (cfg)
               (setf (symbol-value current-config-sym) cfg)
               (funcall setconfig-fn :notifications-enabled t)
               (funcall setconfig-fn :notification-events '(:task-complete :error :approval-needed))
               (funcall setconfig-fn :notification-sound-enabled t)
               (funcall setconfig-fn :notification-desktop-enabled t)
               (funcall setconfig-fn :notification-log-enabled t)
               (funcall setconfig-fn :notification-sound-player "fake-player")
               (funcall setconfig-fn :notification-desktop-command "fake-notify-send")
               (funcall setconfig-fn :notification-log-path log-path)
               (funcall setconfig-fn :notification-sound-task-complete sound-path)
               (funcall setconfig-fn :notification-sound-error sound-path)
               (funcall setconfig-fn :notification-sound-approval-needed sound-path))
             (read-lines (path)
               (with-open-file (stream path :direction :input)
                 (loop for line = (read-line stream nil nil)
                       while line
                       collect line))))
      (unwind-protect
          (progn
            ;; Unsuppress desktop notifications for this test — we
            ;; capture commands via *desktop-notification-run-command-function*.
            (setf (symbol-value desktop-suppressed-sym) nil)
            (ensure-directories-exist sound-path)
            (with-open-file (stream sound-path
                                    :direction :output
                                    :if-does-not-exist :create
                                    :if-exists :supersede)
              (write-line "RIFF" stream))

            (let* ((cfg (funcall load-config-fn :project-root project-root))
                   (invocations '())
                   (bus (funcall make-event-bus-fn :capacity 64)))
              (setf (symbol-value manager-registry-sym) (make-hash-table :test #'eq))
              (setf (symbol-value dispatcher-sym) nil)
              (setf (symbol-value event-bus-sym) bus)
              (setf (symbol-value async-dispatch-sym) nil)
              (setf (symbol-value command-prober-sym)
                    (lambda (command)
                      (member command '("fake-player" "fake-notify-send" "notify-send")
                              :test #'string=)))
              (setf (symbol-value command-runner-sym)
                    (lambda (arguments)
                      (push arguments invocations)
                      (list :exit-code 0 :stdout "" :stderr "")))
              ;; Desktop backend uses its own run-command function, not
              ;; *notification-command-runner*.  Override it to record invocations.
              (setf (symbol-value desktop-run-fn-sym)
                    (lambda (command-string)
                      (push (list "fake-notify-send" command-string) invocations)
                      (list :exit-code 0 :stdout "" :stderr "")))
              (set-notification-config cfg)

              (let ((manager (funcall make-notification-manager-fn
                                      :config cfg
                                      :event-bus bus)))
                ;; Stop the wildcard dispatcher subscription created by
                ;; ensure-notification-dispatcher inside make-notification-manager,
                ;; so only the manager's direct event subscriptions are active.
                (funcall stop-notification-dispatcher-fn :event-bus bus)
                (funcall publish-fn bus
                         (funcall make-tool-completed-event-fn
                                  :tool-name "read-file"
                                  :args '(:path "a")
                                  :result "ok"
                                  :elapsed-ms 11
                                  :request-id "i61-complete"))
                (funcall publish-fn bus
                         (funcall make-tool-error-event-fn
                                  :tool-name "edit-file"
                                  :args '(:path "b")
                                  :condition "boom"
                                  :elapsed-ms 7
                                  :request-id "i61-error"))
                (funcall publish-fn bus
                         (funcall make-permission-prompted-event-fn
                                  :tool-name "shell"
                                  :command "rm -rf /tmp"
                                  :reason "approval required"
                                  :permission-mode :supervised))

                (let* ((sound-count
                         (count-if (lambda (args)
                                     (and (consp args)
                                          (string= (first args) "fake-player")))
                                   invocations))
                       (desktop-count
                         (count-if (lambda (args)
                                     (and (consp args)
                                          (string= (first args) "fake-notify-send")))
                                   invocations))
                       (log-lines (read-lines log-path)))
                  (assert-true (= sound-count 3)
                               "Expected sound backend to run for all configured events, got ~S invocations=~S."
                               sound-count
                               invocations)
                  (assert-true (= desktop-count 3)
                               "Expected desktop backend to run for all configured events, got ~S invocations=~S."
                               desktop-count
                               invocations)
                  (assert-true (= (length log-lines) 3)
                               "Expected structured log backend to append 3 lines, got ~S."
                               (length log-lines)))
                (funcall stop-notification-manager-fn manager)))

            (let* ((cfg (funcall load-config-fn :project-root project-root))
                   (invocations '())
                   (bus (funcall make-event-bus-fn :capacity 32)))
              (setf (symbol-value manager-registry-sym) (make-hash-table :test #'eq))
              (setf (symbol-value dispatcher-sym) nil)
              (setf (symbol-value event-bus-sym) bus)
              (setf (symbol-value async-dispatch-sym) nil)
              (setf (symbol-value command-prober-sym) (lambda (_command) nil))
              (setf (symbol-value command-runner-sym)
                    (lambda (arguments)
                      (push arguments invocations)
                      (list :exit-code 0 :stdout "" :stderr "")))
              (set-notification-config cfg)
              (let ((manager (funcall make-notification-manager-fn
                                      :config cfg
                                      :event-bus bus)))
                (funcall stop-notification-dispatcher-fn :event-bus bus)
                (assert-true (integerp
                              (funcall publish-fn bus
                                       (funcall make-tool-completed-event-fn
                                                :tool-name "noop"
                                                :args nil
                                                :result "ok"
                                                :elapsed-ms 1
                                                :request-id "i61-fallback-1")))
                             "Expected publish to succeed when command backends are unavailable.")
                (assert-true (= (length invocations) 0)
                             "Expected unavailable command backends to skip execution.")
                (funcall stop-notification-manager-fn manager)))

            (let* ((cfg (funcall load-config-fn :project-root project-root))
                   (bus (funcall make-event-bus-fn :capacity 32)))
              (setf (symbol-value manager-registry-sym) (make-hash-table :test #'eq))
              (setf (symbol-value dispatcher-sym) nil)
              (setf (symbol-value event-bus-sym) bus)
              (setf (symbol-value async-dispatch-sym) nil)
              (setf (symbol-value command-prober-sym)
                    (lambda (_command) t))
              (setf (symbol-value command-runner-sym)
                    (lambda (arguments)
                      (if (and (consp arguments)
                               (string= (first arguments) "fake-notify-send"))
                          (list :exit-code 1 :stdout "" :stderr "desktop backend failed")
                          (list :exit-code 0 :stdout "" :stderr ""))))
              (set-notification-config cfg)
              (let ((manager (funcall make-notification-manager-fn
                                      :config cfg
                                      :event-bus bus)))
                (funcall stop-notification-dispatcher-fn :event-bus bus)
                (assert-true (integerp
                              (funcall publish-fn bus
                                       (funcall make-tool-error-event-fn
                                                :tool-name "failing-tool"
                                                :args nil
                                                :condition "bad"
                                                :elapsed-ms 3
                                                :request-id "i61-fallback-2")))
                             "Expected publish to remain successful when desktop backend send fails.")
                (funcall stop-notification-manager-fn manager))))
        (setf (symbol-value current-config-sym) original-config
              (symbol-value event-bus-sym) original-event-bus
              (symbol-value command-prober-sym) original-command-prober
              (symbol-value command-runner-sym) original-command-runner
              (symbol-value async-dispatch-sym) original-async-dispatch
              (symbol-value dispatcher-sym) original-dispatcher
              (symbol-value manager-registry-sym) original-manager-registry
              (symbol-value desktop-run-fn-sym) original-desktop-run-fn
              (symbol-value desktop-suppressed-sym) original-desktop-suppressed))))

  (format t "AMOEBUM_NOTIFICATIONS_SMOKE_OK~%"))
