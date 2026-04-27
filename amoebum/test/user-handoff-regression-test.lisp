(in-package :amoebum/test)

(def-suite user-handoff-regression-suite :in amoebum-suite)
(in-suite user-handoff-regression-suite)

(test handoff-context-packet-captures-structured-coding-state
  "Delegated coding handoffs should ship a structured context packet."
  (with-fresh-user-coordination ()
    (let* ((tmp-root (%make-temp-directory "handoff-context-packet"))
           (project-root (uiop:ensure-directory-pathname
                          (merge-pathnames #P"project/" tmp-root)))
           (global-memory (merge-pathnames #P"home/.amoebum/memory/MEMORY.md" tmp-root))
           (project-memory (merge-pathnames #P".amoebum/MEMORY.md" project-root))
           (backend (amoebum:make-file-memory-backend
                     :project-root project-root
                     :global-path global-memory
                     :project-path project-memory))
           (conversation (amoebum::make-conversation-state
                          :project-root project-root
                          :session-id "session-author"))
           (worktree (amoebum:make-worktree-metadata
                      :id "wt-378"
                      :branch "sw4rm/nxt-378/context-packet"
                      :path (namestring project-root)))
           (amoebum:*ide-context*
             (amoebum:make-ide-context
              :active-file "amoebum/src/swarm.lisp"
              :open-files '("amoebum/src/swarm.lisp" "amoebum/test/user-coordination-test.lisp")
              :selections '((:file "amoebum/src/swarm.lisp"
                             :start 740
                             :end 810
                             :text "handoff-between-users"))
              :diagnostics '((:severity "warning" :message "context packet missing")))))
      (unwind-protect
           (progn
             (amoebum::conversation-state-add-message
              conversation
              (pseudopod:make-message :role "user"
                                      :content "Please review the handoff seam.")
              :save-p nil)
             (amoebum::conversation-state-add-message
              conversation
              (pseudopod:make-message :role "assistant"
                                      :content "I am collecting git, memory, and worktree context.")
              :save-p nil)
             (amoebum:memory-store backend "handoff-policy" "Ship structured packets." :scope :project :source :test)
             (amoebum:memory-store backend "verification" "Run focused suites first." :scope :session :source :test)
             (amoebum:register-user-session-peer "session-author" :user-id "author")
             (amoebum:register-user-session-peer "session-reviewer" :user-id "reviewer")
             (amoebum:handoff-between-users
              "session-author"
              "session-reviewer"
              "Review the delegation packet"
              :context (list :conversation conversation
                             :memory-backend backend
                             :worktree worktree
                             :project-root project-root
                             :notes "keep context structured")
              :budget (list :deadline-epoch-ms (+ (sw4rm-sdk::current-time-ms) 10000)
                            :context-max-bytes 8192))
             (let* ((pending (amoebum:get-user-pending-handoffs "session-reviewer"))
                    (request (first pending))
                    (snapshot (getf request :context-snapshot))
                    (packet (getf request :context-packet))
                    (conversation-packet (getf packet :conversation))
                    (files-packet (getf packet :files))
                    (git-packet (getf packet :git))
                    (memory-packet (getf packet :memory))
                    (worktree-packet (getf packet :worktree)))
               (is (= 1 (length pending)))
               (is (stringp snapshot))
               (is (listp packet))
               (is (= 1 (getf packet :schema-version)))
               (is (string= "coding-task-context" (getf packet :packet-kind)))
               (is (listp conversation-packet))
               (is (= 2 (getf conversation-packet :entry-count)))
               (is (= 2 (length (getf conversation-packet :entries))))
               (is (string= "amoebum/src/swarm.lisp"
                            (getf files-packet :active-file)))
               (is (listp git-packet))
               (is (or (null (getf git-packet :branch))
                       (stringp (getf git-packet :branch))))
               (is (listp memory-packet))
               (is (>= (getf memory-packet :project-count) 1))
               (is (>= (getf memory-packet :session-count) 1))
               (is (string= "wt-378" (getf worktree-packet :id)))
               (is (string= "keep context structured"
                            (getf (getf packet :extras) :notes)))))
        (%delete-directory-tree-safe tmp-root)))))

(test handoff-context-packet-stays-deserializable-under-tight-budget
  "Budget-aware compression should keep the handoff packet structured and parseable."
  (with-fresh-user-coordination ()
    (let* ((tmp-root (%make-temp-directory "handoff-context-tight"))
           (project-root (uiop:ensure-directory-pathname
                          (merge-pathnames #P"project/" tmp-root)))
           (global-memory (merge-pathnames #P"home/.amoebum/memory/MEMORY.md" tmp-root))
           (project-memory (merge-pathnames #P".amoebum/MEMORY.md" project-root))
           (backend (amoebum:make-file-memory-backend
                     :project-root project-root
                     :global-path global-memory
                     :project-path project-memory))
           (conversation (amoebum::make-conversation-state
                          :project-root project-root
                          :session-id "session-author"))
           (amoebum:*ide-context*
             (amoebum:make-ide-context
              :active-file "amoebum/src/swarm.lisp"
              :open-files '("amoebum/src/swarm.lisp")
              :selections (loop repeat 8
                                collect (list :file "amoebum/src/swarm.lisp"
                                              :start 1
                                              :end 20
                                              :text (make-string 160 :initial-element #\S)))
              :diagnostics (loop repeat 8
                                 collect (list :severity "warning"
                                               :message (make-string 160 :initial-element #\D))))))
      (unwind-protect
           (progn
             (loop repeat 10
                   for index from 1 do
                     (amoebum::conversation-state-add-message
                      conversation
                      (pseudopod:make-message
                       :role (if (oddp index) "user" "assistant")
                       :content (make-string 220 :initial-element
                                             (if (oddp index) #\U #\A)))
                      :save-p nil))
             (loop repeat 8
                   for index from 1 do
                     (amoebum:memory-store backend
                                           (format nil "entry-~D" index)
                                           (make-string 120 :initial-element #\M)
                                           :scope :project
                                           :source :test))
             (amoebum:register-user-session-peer "session-author" :user-id "author")
             (amoebum:register-user-session-peer "session-reviewer" :user-id "reviewer")
             (amoebum:handoff-between-users
              "session-author"
              "session-reviewer"
              "Compress this handoff"
              :context (list :conversation conversation
                             :memory-backend backend
                             :project-root project-root
                             :notes (make-string 400 :initial-element #\N))
              :budget (list :deadline-epoch-ms (+ (sw4rm-sdk::current-time-ms) 10000)
                            :context-max-bytes 1400))
             (let* ((request (first (amoebum:get-user-pending-handoffs "session-reviewer")))
                    (snapshot (getf request :context-snapshot))
                    (packet (getf request :context-packet))
                    (conversation-packet (getf packet :conversation)))
               (is (stringp snapshot))
               (is (<= (length snapshot) 1400))
               (is (listp packet))
               (is (string= "coding-task-context" (getf packet :packet-kind)))
               (is (<= (length (or (getf conversation-packet :entries) '()))
                       (getf conversation-packet :entry-count)))
               (is (<= (length (getf (getf packet :memory) :project))
                       (getf (getf packet :memory) :project-count)))))
        (%delete-directory-tree-safe tmp-root)))))
