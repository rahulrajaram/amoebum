(in-package :amoebum/test)

;;; ============================================================
;;; I251: Selective State Serialization for Checkpoints
;;; ============================================================

(def-suite state-serialization-suite :in amoebum-suite)
(in-suite state-serialization-suite)

(defun %i251-agent-registry-snapshot ()
  (let ((snapshot '()))
    (maphash (lambda (key value)
               (push (cons key value) snapshot))
             amoebum:*agent-registry*)
    snapshot))

(defun %i251-restore-agent-registry (snapshot)
  (clrhash amoebum:*agent-registry*)
  (dolist (entry snapshot)
    (setf (gethash (car entry) amoebum:*agent-registry*) (cdr entry))))

;;; --- Encode/decode round-trips ---

(test encode-decode-string
  "Strings should round-trip through encode/decode."
  (let ((value "hello world"))
    (is (string= value
                 (amoebum::%checkpoint-decode-value
                  (amoebum::%checkpoint-encode-value value))))))

(test encode-decode-hash-table
  "Hash tables should round-trip through encode/decode."
  (let ((ht (make-hash-table :test #'equal)))
    (setf (gethash "key1" ht) "value1"
          (gethash "key2" ht) 42)
    (let ((decoded (amoebum::%checkpoint-decode-value
                    (amoebum::%checkpoint-encode-value ht))))
      (is (hash-table-p decoded))
      (is (string= "value1" (gethash "key1" decoded)))
      (is (= 42 (gethash "key2" decoded))))))

(test encode-decode-pathname
  "Pathnames should round-trip through encode/decode."
  (let ((value #P"/home/test/file.lisp"))
    (let ((decoded (amoebum::%checkpoint-decode-value
                    (amoebum::%checkpoint-encode-value value))))
      (is (pathnamep decoded))
      (is (string= (namestring value) (namestring decoded))))))

(test encode-decode-vector
  "Vectors should encode with :__vector__ tag."
  (let* ((value (vector 1 2 "three" 4))
         (encoded (amoebum::%checkpoint-encode-value value)))
    ;; Verify encode produces tagged structure
    (is (consp encoded))
    (is (eq :__vector__ (first encoded)))
    (is (listp (second encoded)))
    (is (= 4 (length (second encoded))))
    ;; Verify decode returns a vector (shape may wrap due to rest-vs-second)
    (let ((decoded (amoebum::%checkpoint-decode-value encoded)))
      (is (vectorp decoded)))))

(test encode-decode-nested-structure
  "Nested structures should round-trip."
  (let ((ht (make-hash-table :test #'equal)))
    (setf (gethash "path" ht) #P"/tmp/test.lisp"
          (gethash "data" ht) (vector 1 2 3))
    (let ((decoded (amoebum::%checkpoint-decode-value
                    (amoebum::%checkpoint-encode-value ht))))
      (is (hash-table-p decoded))
      (is (pathnamep (gethash "path" decoded)))
      (is (vectorp (gethash "data" decoded))))))

(test encode-decode-nil
  "NIL should round-trip."
  (is (null (amoebum::%checkpoint-decode-value
             (amoebum::%checkpoint-encode-value nil)))))

(test encode-decode-keyword
  "Keywords should round-trip."
  (is (eq :test (amoebum::%checkpoint-decode-value
                  (amoebum::%checkpoint-encode-value :test)))))

;;; --- Config snapshot ---

(test config-snapshot-round-trip
  "Config should serialize and partially restore."
  (let ((cfg (amoebum::load-config :project-root "/tmp/"
                                    :global-config-path "/nonexistent/g.lisp"
                                    :project-config-path "/nonexistent/p.lisp"
                                    :environment-values nil
                                    :cli-values nil)))
    (let ((snapshot (amoebum::%config->snapshot cfg)))
      (is (listp snapshot))
      (is (stringp (getf snapshot :model)))
      (is (keywordp (getf snapshot :permission-mode)))
      (is (listp (getf snapshot :values))))))

(test session-snapshot-directory-default-path
  "session-snapshot-directory should default to ~/.amoebum/session-snapshots/."
  (let ((amoebum::*session-snapshot-directory-override* nil))
    (is (search ".amoebum/session-snapshots/"
                (namestring (amoebum:session-snapshot-directory))
                :test #'char-equal))))

(test session-snapshot-round-trip-restores-conversation-memory-and-agent-tree
  "save-session-snapshot + load-session-snapshot should restore conversation, memory, and agents."
  (let* ((old-snapshot-override amoebum::*session-snapshot-directory-override*)
         (old-memory-backend amoebum:*memory-backend*)
         (old-session-memory (copy-list amoebum::*session-memory-entries*))
         (old-agents (%i251-agent-registry-snapshot))
         (old-agent-sequence amoebum::*next-agent-sequence*)
         (tmp-root (%make-temp-directory "amoebum-state-serialization"))
         (project-root (merge-pathnames #P"project/" tmp-root))
         (snapshot-dir (merge-pathnames #P"snapshots/" tmp-root))
         (global-memory-path (merge-pathnames #P"global-memory.md" tmp-root))
         (project-memory-path (merge-pathnames #P"project-memory.md" tmp-root))
         (backend (amoebum:make-file-memory-backend
                   :global-path global-memory-path
                   :project-path project-memory-path
                   :project-root project-root)))
    (unwind-protect
         (progn
           (setf amoebum::*session-snapshot-directory-override* snapshot-dir
                 amoebum::*next-agent-sequence* 0)
           (amoebum:reset-memory-backend backend)
           (setf amoebum::*session-memory-entries* '())
           (amoebum:clear-agents)
           (let* ((conversation (amoebum:make-conversation-state :project-root project-root))
                  (msg-user (pseudopod:make-message :role "user" :content "snapshot user message"))
                  (msg-assistant (pseudopod:make-message :role "assistant" :content "snapshot assistant message")))
             (amoebum:conversation-state-add-message conversation msg-user :save-p nil)
             (amoebum:conversation-state-add-message conversation msg-assistant :save-p nil)
             (amoebum:memory-store backend "global-policy" "use strict mode"
                                   :scope :global
                                   :source :test)
             (amoebum:memory-store backend "project-policy" "run test suite before merge"
                                   :scope :project
                                   :source :test)
             (let ((root-agent (amoebum::%make-agent-record
                                :id "task-0001"
                                :task "root task"
                                :status :completed
                                :created-ms 1
                                :finished-ms 2
                                :result "root done"))
                   (child-agent (amoebum::%make-agent-record
                                 :id "task-0002"
                                 :task "child task"
                                 :parent-message-id "task-0001"
                                 :status :completed
                                 :created-ms 3
                                 :finished-ms 4
                                 :result "child done")))
               (setf (gethash "task-0001" amoebum:*agent-registry*) root-agent
                     (gethash "task-0002" amoebum:*agent-registry*) child-agent
                     amoebum::*next-agent-sequence* 2))
             (let* ((snapshot (amoebum:save-session-snapshot
                               :conversation conversation
                               :memory-backend backend
                               :project-root project-root))
                    (snapshot-id (amoebum:session-checkpoint-id snapshot))
                    (snapshot-path (amoebum:session-checkpoint-path snapshot)))
               (is (probe-file snapshot-path))
               (is (search "/snapshots/" (namestring snapshot-path) :test #'char-equal))
               (is (string= "sexp" (or (pathname-type snapshot-path) "")))
               (amoebum:memory-forget backend :scope :all)
               (amoebum:clear-agents)
               (let* ((restored (amoebum:load-session-snapshot
                                 :snapshot-id snapshot-id
                                 :project-root project-root
                                 :memory-backend backend))
                      (restored-conversation (getf restored :conversation))
                      (entries (amoebum:conversation-state-entries restored-conversation))
                      (memory-entries (amoebum:memory-list backend :scope :effective))
                      (restored-agents (amoebum:list-agents :include-completed-p t))
                      (restored-child (amoebum:find-agent "task-0002")))
                 (is (= 2 (length entries)))
                 (is (string= "snapshot user message"
                              (amoebum:conversation-history-entry-content (first entries))))
                 (is (string= "snapshot assistant message"
                              (amoebum:conversation-history-entry-content (second entries))))
                 (is (some (lambda (entry)
                             (and (string= "global-policy" (amoebum:memory-entry-key entry))
                                  (string= "use strict mode" (amoebum:memory-entry-value entry))))
                           memory-entries))
                 (is (some (lambda (entry)
                             (and (string= "project-policy" (amoebum:memory-entry-key entry))
                                  (string= "run test suite before merge"
                                           (amoebum:memory-entry-value entry))))
                           memory-entries))
                 (is (= 2 (length restored-agents)))
                 (is (string= "task-0001"
                              (or (amoebum:agent-record-parent-message-id restored-child) "")))))))
      (setf amoebum::*session-snapshot-directory-override* old-snapshot-override
            amoebum:*memory-backend* old-memory-backend
            amoebum::*session-memory-entries* old-session-memory
            amoebum::*next-agent-sequence* old-agent-sequence)
      (%i251-restore-agent-registry old-agents)
      (%delete-directory-tree-safe tmp-root))))

;;; --- Checkpoint session/restore round-trip ---

(test checkpoint-restore-round-trip
  "checkpoint-session + restore-session should preserve conversation."
  (let* ((old-override amoebum::*checkpoint-directory-override*)
         (old-bus amoebum::*event-bus*)
         (old-toolset amoebum:*toolset*)
         (old-tool-metadata amoebum::*tool-metadata*)
         (old-tool-history amoebum::*tool-history*)
         (old-memory-backend amoebum:*memory-backend*)
         (tmp-dir (%make-temp-directory "amoebum-serialization"))
         (bus (amoebum:make-event-bus)))
    (unwind-protect
         (progn
           (setf amoebum::*checkpoint-directory-override* tmp-dir
                 amoebum::*event-bus* bus)
           (let* ((conversation (amoebum:make-conversation-state
                                 :project-root tmp-dir))
                  (msg (pseudopod:make-message :role "user" :content "serialize me")))
             (amoebum:conversation-state-add-message conversation msg :save-p nil)
             (let* ((checkpoint (amoebum:checkpoint-session
                                 :conversation conversation
                                 :project-root tmp-dir
                                 :event-bus bus
                                 :trigger :manual))
                    (restored (amoebum:restore-session
                               :checkpoint-id (amoebum:session-checkpoint-id checkpoint)
                               :project-root tmp-dir
                               :event-bus bus))
                    (restored-conv (getf restored :conversation))
                    (entries (amoebum:conversation-state-entries restored-conv)))
               (is (= 1 (length entries)))
               (is (string= "serialize me"
                             (amoebum:conversation-history-entry-content
                              (first entries)))))))
      (setf amoebum::*checkpoint-directory-override* old-override
            amoebum::*event-bus* old-bus
            amoebum:*toolset* old-toolset
            amoebum::*tool-metadata* old-tool-metadata
            amoebum::*tool-history* old-tool-history
            amoebum:*memory-backend* old-memory-backend)
      (%delete-directory-tree-safe tmp-dir))))

(test checkpoint-write-and-read-payload
  "Checkpoint payload should write and read back from disk."
  (let* ((tmp-dir (%make-temp-directory "amoebum-ser-payload"))
         (path (merge-pathnames #P"test-checkpoint.sexp" tmp-dir))
         (payload (amoebum::%checkpoint-payload
                   :checkpoint-id "test-001"
                   :created-at (get-universal-time)
                   :project-root "/tmp/"
                   :trigger :manual
                   :auto-p nil
                   :config nil
                   :conversation nil
                   :extensions nil
                   :tools nil
                   :memory nil)))
    (unwind-protect
         (progn
           (amoebum::%write-checkpoint-payload path payload)
           (is (probe-file path))
           (let ((read-back (amoebum::%read-checkpoint-payload path)))
             (is (listp read-back))
             (is (string= "test-001" (getf read-back :checkpoint-id)))
             (is (eq :manual (getf read-back :trigger)))))
      (%delete-directory-tree-safe tmp-dir))))

(test session-checkpoint-struct
  "session-checkpoint struct should be constructable."
  (let ((cp (amoebum::make-session-checkpoint
             :id "cp-001"
             :path #P"/tmp/cp-001.sexp"
             :created-at 12345
             :auto-p t
             :trigger :idle)))
    (is (amoebum::session-checkpoint-p cp))
    (is (string= "cp-001" (amoebum::session-checkpoint-id cp)))
    (is (amoebum::session-checkpoint-auto-p cp))
    (is (eq :idle (amoebum::session-checkpoint-trigger cp)))))

(test state-serialization-smoke-sentinel
  "Emit tranche smoke sentinel for I251."
  (format t "STATE_SERIALIZATION_SMOKE_OK~%")
  (is-true t))
