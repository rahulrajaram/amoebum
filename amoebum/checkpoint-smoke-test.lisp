(let* ((smoke-file (or *load-truename* *compile-file-truename*))
       (amoebum-dir (and smoke-file (make-pathname :name nil :type nil :defaults smoke-file)))
       (repo-root (and amoebum-dir (truename (merge-pathnames #P"../" amoebum-dir)))))
  (unless repo-root
    (error "Unable to resolve repository root from ~S" smoke-file))

  #+sbcl
  (let ((home-root (merge-pathnames #P".tmp/home/" repo-root))
        (cache-root (merge-pathnames #P".tmp/xdg-cache/" repo-root))
        (tmp-root (merge-pathnames #P".tmp/" repo-root)))
    (ensure-directories-exist home-root)
    (ensure-directories-exist cache-root)
    (ensure-directories-exist tmp-root)
    (ignore-errors
      (require :sb-posix)
      (let ((setenv-sym (find-symbol "SETENV" "SB-POSIX")))
        (when setenv-sym
          (let ((setenv (symbol-function setenv-sym)))
            (funcall setenv "HOME" (namestring home-root) 1)
            (funcall setenv "XDG_CACHE_HOME" (namestring cache-root) 1)
            (funcall setenv "TMPDIR" (namestring tmp-root) 1))))))

  (let* ((local-quicklisp (merge-pathnames #P"ptui/.tools/quicklisp/setup.lisp" repo-root))
         (fallback-root #P"/home/rahul/Documents/amoebum/")
         (fallback-quicklisp (merge-pathnames #P"ptui/.tools/quicklisp/setup.lisp" fallback-root))
         (quicklisp-setup
           (cond
             ((probe-file fallback-quicklisp) fallback-quicklisp)
             ((probe-file local-quicklisp) local-quicklisp)
             (t (error "Unable to locate quicklisp setup at ~A or ~A."
                       local-quicklisp
                       fallback-quicklisp)))))
    (load quicklisp-setup))
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
         (pseudopod-pkg (or (find-package "PSEUDOPOD")
                            (error "Missing package PSEUDOPOD after load.")))
         (uiop-pkg (or (find-package "UIOP")
                       (find-package "ASDF/UTILITY")
                       (error "Missing UIOP package after requiring ASDF.")))
         (symbol-in
           (lambda (name package)
             (or (find-symbol name package)
                 (error "Missing symbol ~A in package ~A." name (package-name package)))))
         (fn-in
           (lambda (name package)
             (symbol-function (funcall symbol-in name package))))
         (temporary-directory-fn (funcall fn-in "TEMPORARY-DIRECTORY" uiop-pkg))
         (ensure-directory-pathname-fn (funcall fn-in "ENSURE-DIRECTORY-PATHNAME" uiop-pkg))
         (delete-directory-tree-fn (ignore-errors (funcall fn-in "DELETE-DIRECTORY-TREE" uiop-pkg)))
         (reload-config-fn (funcall fn-in "RELOAD-CONFIG" amoebum-pkg))
         (current-config-fn (funcall fn-in "CURRENT-CONFIG" amoebum-pkg))
         (setconfig-fn (funcall fn-in "SETCONFIG" amoebum-pkg))
         (config-model-fn (funcall fn-in "CONFIG-MODEL" amoebum-pkg))
         (checkpoint-session-fn (funcall fn-in "CHECKPOINT-SESSION" amoebum-pkg))
         (restore-session-fn (funcall fn-in "RESTORE-SESSION" amoebum-pkg))
         (list-checkpoints-fn (funcall fn-in "LIST-SESSION-CHECKPOINTS" amoebum-pkg))
         (auto-checkpoint-fn (funcall fn-in "MAYBE-AUTO-CHECKPOINT" amoebum-pkg))
         (checkpoint-mark-activity-fn (funcall fn-in "CHECKPOINT-MARK-ACTIVITY" amoebum-pkg))
         (checkpoint-id-fn (funcall fn-in "SESSION-CHECKPOINT-ID" amoebum-pkg))
         (checkpoint-path-fn (funcall fn-in "SESSION-CHECKPOINT-PATH" amoebum-pkg))
         (checkpoint-auto-p-fn (funcall fn-in "SESSION-CHECKPOINT-AUTO-P" amoebum-pkg))
         (make-conversation-state-fn (funcall fn-in "MAKE-CONVERSATION-STATE" amoebum-pkg))
         (conversation-add-message-fn (funcall fn-in "CONVERSATION-STATE-ADD-MESSAGE" amoebum-pkg))
         (conversation-entries-fn (funcall fn-in "CONVERSATION-STATE-ENTRIES" amoebum-pkg))
         (memory-store-fn (funcall fn-in "MEMORY-STORE" amoebum-pkg))
         (memory-list-fn (funcall fn-in "MEMORY-LIST" amoebum-pkg))
         (memory-entry-key-fn (funcall fn-in "MEMORY-ENTRY-KEY" amoebum-pkg))
         (memory-entry-value-fn (funcall fn-in "MEMORY-ENTRY-VALUE" amoebum-pkg))
         (make-file-memory-backend-fn (funcall fn-in "MAKE-FILE-MEMORY-BACKEND" amoebum-pkg))
         (reset-memory-backend-fn (funcall fn-in "RESET-MEMORY-BACKEND" amoebum-pkg))
         (current-memory-backend-fn (funcall fn-in "CURRENT-MEMORY-BACKEND" amoebum-pkg))
         (load-extensions-fn (funcall fn-in "LOAD-USER-EXTENSIONS" amoebum-pkg))
         (dispatch-command-fn (funcall fn-in "DISPATCH-SLASH-COMMAND" amoebum-pkg))
         (result-output-fn (funcall fn-in "SLASH-COMMAND-RESULT-OUTPUT" amoebum-pkg))
         (make-event-bus-fn (funcall fn-in "MAKE-EVENT-BUS" amoebum-pkg))
         (event-history-fn (funcall fn-in "EVENT-HISTORY" amoebum-pkg))
         (event-type-fn (funcall fn-in "EVENT-TYPE" amoebum-pkg))
         (make-message-fn (funcall fn-in "MAKE-MESSAGE" pseudopod-pkg))
         (find-tool-fn (funcall fn-in "FIND-TOOL" pseudopod-pkg))
         (checkpoint-event-type
           (symbol-value (funcall symbol-in "+EVENT-TYPE-SESSION-CHECKPOINTED+" amoebum-pkg)))
         (restored-event-type
           (symbol-value (funcall symbol-in "+EVENT-TYPE-SESSION-RESTORED+" amoebum-pkg)))
         (deftool-sym (funcall symbol-in "DEFTOOL" amoebum-pkg))
         (event-bus-sym (funcall symbol-in "*EVENT-BUS*" amoebum-pkg))
         (toolset-sym (funcall symbol-in "*TOOLSET*" amoebum-pkg))
         (tool-metadata-sym (funcall symbol-in "*TOOL-METADATA*" amoebum-pkg))
         (tool-history-sym (funcall symbol-in "*TOOL-HISTORY*" amoebum-pkg))
         (checkpoint-dir-override-sym
           (funcall symbol-in "*CHECKPOINT-DIRECTORY-OVERRIDE*" amoebum-pkg))
         (extensions-global-override-sym
           (funcall symbol-in "*EXTENSIONS-GLOBAL-DIRECTORY-OVERRIDE*" amoebum-pkg))
         (extensions-project-override-sym
           (funcall symbol-in "*EXTENSIONS-PROJECT-DIRECTORY-OVERRIDE*" amoebum-pkg))
         (session-memory-sym (funcall symbol-in "*SESSION-MEMORY-ENTRIES*" amoebum-pkg)))
    (labels ((assert-true (condition format-string &rest format-args)
               (unless condition
                 (error (apply #'format nil format-string format-args))))
             (contains-text-p (haystack needle)
               (and (stringp haystack)
                    (search needle haystack :test #'char-equal)))
             (write-file (path content)
               (ensure-directories-exist path)
               (with-open-file (stream path
                                       :direction :output
                                       :if-does-not-exist :create
                                       :if-exists :supersede
                                       :external-format :utf-8)
                 (write-string content stream))))
      (let* ((temp-root
               (funcall ensure-directory-pathname-fn
                        (merge-pathnames
                         (make-pathname :directory
                                        `(:relative
                                          ,(format nil "amoebum-i81-~A"
                                                   (get-universal-time))))
                         (funcall temporary-directory-fn))))
             (project-root (merge-pathnames #P"project/" temp-root))
             (checkpoint-dir (merge-pathnames #P"checkpoints/" temp-root))
             (global-memory-path (merge-pathnames #P"global-memory.md" temp-root))
             (project-memory-path (merge-pathnames #P"project-memory.md" temp-root))
             (global-ext-dir (merge-pathnames #P"global-ext/" temp-root))
             (project-ext-dir (merge-pathnames #P"project-ext/" temp-root))
             (extension-file (merge-pathnames #P"10-checkpoint-extension.lisp" project-ext-dir))
             (old-event-bus (symbol-value event-bus-sym))
             (old-toolset (symbol-value toolset-sym))
             (old-tool-metadata (symbol-value tool-metadata-sym))
             (old-tool-history (symbol-value tool-history-sym))
             (old-session-memory (symbol-value session-memory-sym))
             (old-checkpoint-override (symbol-value checkpoint-dir-override-sym))
             (old-global-ext-override (symbol-value extensions-global-override-sym))
             (old-project-ext-override (symbol-value extensions-project-override-sym))
             (old-memory-backend (funcall current-memory-backend-fn))
             (bus (funcall make-event-bus-fn :capacity 128)))
        (unwind-protect
            (progn
              (setf (symbol-value event-bus-sym) bus
                    (symbol-value checkpoint-dir-override-sym) checkpoint-dir
                    (symbol-value extensions-global-override-sym) global-ext-dir
                    (symbol-value extensions-project-override-sym) project-ext-dir)
              (funcall reload-config-fn :project-root project-root)
              (funcall setconfig-fn :model "checkpoint-model")
              (funcall setconfig-fn :permission-mode :full-auto)
              (funcall setconfig-fn :auto-checkpoint-idle-seconds 300)

              (funcall reset-memory-backend-fn
                       (funcall make-file-memory-backend-fn
                                :global-path global-memory-path
                                :project-path project-memory-path
                                :project-root project-root))
              (let ((memory-backend (funcall current-memory-backend-fn)))
                (funcall memory-store-fn memory-backend "project_pref" "use checkpoint tests"
                         :scope :project
                         :source :manual)
                (setf (symbol-value session-memory-sym)
                      (list (funcall memory-store-fn memory-backend "session_pref" "ephemeral checkpoint value"
                                     :scope :project
                                     :source :manual))))

              (write-file extension-file
                          "(in-package :amoebum)
(deftool checkpoint-ext-tool ((text string :required t :description \"input\"))
  \"Checkpoint extension tool.\"
  (format nil \"ext:~A\" text))
")
              (funcall load-extensions-fn :project-root project-root)

              (eval
               `(,deftool-sym checkpoint-smoke-tool ((text string :required t :description "input"))
                  "Checkpoint smoke tool."
                  (format nil "ok:~A" text)))

              (let* ((conversation (funcall make-conversation-state-fn :project-root project-root))
                     (user-message (funcall make-message-fn :role "user" :content "checkpoint user"))
                     (assistant-message (funcall make-message-fn :role "assistant" :content "checkpoint assistant")))
                (funcall conversation-add-message-fn conversation user-message :save-p nil)
                (funcall conversation-add-message-fn conversation assistant-message :save-p nil)

                (let* ((checkpoint
                         (funcall checkpoint-session-fn
                                  :conversation conversation
                                  :config (funcall current-config-fn)
                                  :event-bus bus
                                  :trigger :manual
                                  :auto-p nil))
                       (checkpoint-id (funcall checkpoint-id-fn checkpoint))
                       (checkpoint-path (funcall checkpoint-path-fn checkpoint))
                       (original-model (funcall config-model-fn (funcall current-config-fn)))
                       (original-entry-count (length (funcall conversation-entries-fn conversation))))
                  (assert-true (and checkpoint-path (probe-file checkpoint-path))
                               "Expected checkpoint file to be written, got ~S." checkpoint-path)
                  (assert-true (plusp (length checkpoint-id))
                               "Expected non-empty checkpoint id.")

                  (funcall setconfig-fn :model "mutated-after-checkpoint")
                  (setf (symbol-value toolset-sym) (funcall (funcall fn-in "MAKE-TOOLSET" pseudopod-pkg))
                        (symbol-value tool-metadata-sym) (make-hash-table :test #'equal)
                        (symbol-value tool-history-sym) (make-hash-table :test #'equal)
                        (symbol-value session-memory-sym) '())

                  (let* ((restored (funcall restore-session-fn
                                            :checkpoint-id checkpoint-id
                                            :config (funcall current-config-fn)
                                            :event-bus bus))
                         (restored-conversation (getf restored :conversation))
                         (restored-backend (getf restored :memory-backend))
                         (restored-message-count
                           (length (funcall conversation-entries-fn restored-conversation)))
                         (restored-model (funcall config-model-fn (funcall current-config-fn)))
                         (restored-toolset (symbol-value toolset-sym))
                         (checkpoint-tool (funcall find-tool-fn restored-toolset "checkpoint-smoke-tool"))
                         (extension-tool (funcall find-tool-fn restored-toolset "checkpoint-ext-tool"))
                         (memory-entries (funcall memory-list-fn restored-backend :scope :project)))
                    (assert-true (= restored-message-count original-entry-count)
                                 "Expected restored conversation entry count ~D, got ~D."
                                 original-entry-count
                                 restored-message-count)
                    (assert-true (string= restored-model original-model)
                                 "Expected restored config model ~S, got ~S."
                                 original-model
                                 restored-model)
                    (assert-true checkpoint-tool
                                 "Expected checkpoint-smoke-tool to be restored in tool registry.")
                    (assert-true extension-tool
                                 "Expected extension tool checkpoint-ext-tool to be restored.")
                    (assert-true (some (lambda (entry)
                                         (and (string= (funcall memory-entry-key-fn entry)
                                                       "project_pref")
                                              (contains-text-p (funcall memory-entry-value-fn entry)
                                                               "checkpoint")))
                                       memory-entries)
                                 "Expected restored project memory entry to include project_pref.")

                    (funcall setconfig-fn :auto-checkpoint-idle-seconds 1)
                    (funcall checkpoint-mark-activity-fn 1000)
                    (let ((auto-checkpoint
                            (funcall auto-checkpoint-fn
                                     :conversation restored-conversation
                                     :config (funcall current-config-fn)
                                     :event-bus bus
                                     :timestamp 1002
                                     :busy-p nil)))
                      (assert-true auto-checkpoint
                                   "Expected maybe-auto-checkpoint to create a checkpoint after idle.")
                      (assert-true (funcall checkpoint-auto-p-fn auto-checkpoint)
                                   "Expected auto checkpoint marker to be true."))

                    (let ((checkpoints (funcall list-checkpoints-fn)))
                      (assert-true (>= (length checkpoints) 2)
                                   "Expected at least two checkpoints after manual+auto saves."))

                    (multiple-value-bind (handledp result)
                        (funcall dispatch-command-fn
                                 "/checkpoint list"
                                 :config (funcall current-config-fn)
                                 :memory-backend restored-backend)
                      (let ((output (and result (funcall result-output-fn result))))
                        (assert-true handledp "Expected /checkpoint list to be handled.")
                        (assert-true (contains-text-p output checkpoint-id)
                                     "Expected /checkpoint list output to include checkpoint id ~A, got ~S."
                                     checkpoint-id
                                     output)))

                    (multiple-value-bind (handledp result)
                        (funcall dispatch-command-fn
                                 "/checkpoint save"
                                 :config (funcall current-config-fn)
                                 :memory-backend restored-backend)
                      (let ((output (and result (funcall result-output-fn result))))
                        (assert-true handledp "Expected /checkpoint save to be handled.")
                        (assert-true (contains-text-p output "Saved checkpoint")
                                     "Expected /checkpoint save output confirmation, got ~S."
                                     output)))

                    (multiple-value-bind (handledp result)
                        (funcall dispatch-command-fn
                                 (format nil "/checkpoint restore ~A" checkpoint-id)
                                 :config (funcall current-config-fn)
                                 :memory-backend restored-backend)
                      (let ((output (and result (funcall result-output-fn result))))
                        (assert-true handledp "Expected /checkpoint restore to be handled.")
                        (assert-true (contains-text-p output "Restored checkpoint")
                                     "Expected /checkpoint restore output confirmation, got ~S."
                                     output)))))

                  (let ((history (funcall event-history-fn bus)))
                    (assert-true (some (lambda (event)
                                         (eq (funcall event-type-fn event)
                                             checkpoint-event-type))
                                       history)
                                 "Expected event history to include session:checkpointed event.")
                    (assert-true (some (lambda (event)
                                         (eq (funcall event-type-fn event)
                                             restored-event-type))
                                       history)
                                 "Expected event history to include session:restored event.")))))
          (setf (symbol-value event-bus-sym) old-event-bus
                (symbol-value toolset-sym) old-toolset
                (symbol-value tool-metadata-sym) old-tool-metadata
                (symbol-value tool-history-sym) old-tool-history
                (symbol-value session-memory-sym) old-session-memory
                (symbol-value checkpoint-dir-override-sym) old-checkpoint-override
                (symbol-value extensions-global-override-sym) old-global-ext-override
                (symbol-value extensions-project-override-sym) old-project-ext-override)
          (funcall reset-memory-backend-fn old-memory-backend)
          (when (and delete-directory-tree-fn (probe-file temp-root))
            (ignore-errors
              (funcall delete-directory-tree-fn temp-root
                       :validate t
                       :if-does-not-exist :ignore)))))))

  (format t "AMOEBUM_CHECKPOINT_SMOKE_OK~%")
