;;;; amoebum/test/conversation-unit-test.lisp
;;;;
;;;; NXT-286 — Unit tests for conversation.lisp invariants that are not
;;;; exercised by conversation-roundtrip-test.lisp (which only covers
;;;; save/load serialization). These tests target in-memory invariants:
;;;;
;;;;   * append-only semantics for conversation-state-entries
;;;;   * monotonic ordering (index + timestamp) of successive adds
;;;;   * tool-call/tool-result pairing by tool-call-id (string match)
;;;;   * detection of orphan tool-results (no matching prior tool-call)
;;;;   * conversation length tracking
;;;;   * conversation-reset! clears history back to empty
;;;;
;;;; These tests DO NOT touch disk: every add is called with :save-p nil,
;;;; so no session fixture is required. They also do not mutate any
;;;; amoebum globals listed in amoebum/test/CLAUDE.md, so no
;;;; save/restore dance is needed.

(in-package :amoebum/test)

;; -------------------------------------------------------------------------
;; Local helpers — kept test-file-local so they don't leak into other suites.

(defun %cu-fresh-conversation ()
  "Make an in-memory conversation. Every test calls add/reset with
:save-p nil, so nothing is written to disk and no fixture is needed."
  (amoebum.sessions:make-conversation-state
   :project-root (uiop:temporary-directory)))

(defun %cu-add-user (conversation text)
  (amoebum.sessions:conversation-state-add-message
   conversation
   (pseudopod:make-message :role "user" :content text)
   :save-p nil))

(defun %cu-add-assistant (conversation text &key tool-call-id)
  (amoebum.sessions:conversation-state-add-message
   conversation
   (pseudopod:make-message :role "assistant"
                           :content text
                           :tool-call-id tool-call-id)
   :save-p nil))

(defun %cu-add-tool (conversation content tool-call-id &key (name "mock_tool"))
  (amoebum.sessions:conversation-state-add-message
   conversation
   (pseudopod:make-message :role "tool"
                           :name name
                           :content content
                           :tool-call-id tool-call-id)
   :save-p nil))

(defun %cu-snapshot-entries (conversation)
  "Take a shallow, stable snapshot of role/content/tool-call-id per entry."
  (mapcar (lambda (entry)
            (list :role (amoebum.sessions:conversation-history-entry-role entry)
                  :content (amoebum.sessions:conversation-history-entry-content entry)
                  :tool-call-id (amoebum.sessions:conversation-history-entry-tool-call-id entry)))
          (amoebum.sessions:conversation-state-entries conversation)))

(defun %cu-entry-at (conversation index)
  (nth index (amoebum.sessions:conversation-state-entries conversation)))

(defun %cu-tool-result-orphan-p (conversation entry-index)
  "Return T when the entry at ENTRY-INDEX is a tool result whose
tool-call-id has no matching earlier assistant tool-call-id.
Returns NIL when the entry is properly paired or is not a tool result."
  (let* ((entries (amoebum.sessions:conversation-state-entries conversation))
         (entry (nth entry-index entries)))
    (and entry
         (string= "tool"
                  (amoebum.sessions:conversation-history-entry-role entry))
         (let ((target (amoebum.sessions:conversation-history-entry-tool-call-id entry)))
           (and target
                (not (loop for e in (subseq entries 0 entry-index)
                           thereis
                             (and (string=
                                   "assistant"
                                   (amoebum.sessions:conversation-history-entry-role e))
                                  (let ((id (amoebum.sessions:conversation-history-entry-tool-call-id e)))
                                    (and id (string= id target)))))))))))

;; -------------------------------------------------------------------------
;; Tests

(test nxt-286-conversation-starts-empty
  "A fresh conversation has zero entries and zero messages."
  (let ((c (%cu-fresh-conversation)))
    (is (= 0 (length (amoebum.sessions:conversation-state-entries c))))
    (is (= 0 (length (amoebum.sessions:conversation-state-messages c))))))

(test nxt-286-add-message-is-append-only
  "Adding a new message must not mutate prior entries' observable fields."
  (let ((c (%cu-fresh-conversation)))
    (%cu-add-user c "first")
    (%cu-add-assistant c "second")
    (let ((snapshot-before (%cu-snapshot-entries c)))
      (%cu-add-user c "third")
      ;; Re-read the first two entries and compare to the snapshot.
      (let ((snapshot-after-prefix
              (subseq (%cu-snapshot-entries c) 0 2)))
        (is (equalp snapshot-before snapshot-after-prefix)
            "Prior entries must not change after a new message is appended.")))))

(test nxt-286-add-message-grows-length-by-one
  "Each conversation-state-add-message call grows entries by exactly 1."
  (let ((c (%cu-fresh-conversation)))
    (is (= 0 (length (amoebum.sessions:conversation-state-entries c))))
    (%cu-add-user c "a")
    (is (= 1 (length (amoebum.sessions:conversation-state-entries c))))
    (%cu-add-assistant c "b")
    (is (= 2 (length (amoebum.sessions:conversation-state-entries c))))
    (%cu-add-user c "c")
    (is (= 3 (length (amoebum.sessions:conversation-state-entries c))))
    (is (= (length (amoebum.sessions:conversation-state-entries c))
           (length (amoebum.sessions:conversation-state-messages c)))
        "messages and entries must stay length-synchronized.")))

(test nxt-286-message-order-is-monotonic
  "The index of the newest entry strictly increases after every add,
and the entry at index N holds the N'th added content."
  (let ((c (%cu-fresh-conversation)))
    (%cu-add-user c "m0")
    (%cu-add-assistant c "m1")
    (%cu-add-user c "m2")
    (%cu-add-assistant c "m3")
    (let ((entries (amoebum.sessions:conversation-state-entries c)))
      (is (= 4 (length entries)))
      (is (string= "m0" (amoebum.sessions:conversation-history-entry-content (nth 0 entries))))
      (is (string= "m1" (amoebum.sessions:conversation-history-entry-content (nth 1 entries))))
      (is (string= "m2" (amoebum.sessions:conversation-history-entry-content (nth 2 entries))))
      (is (string= "m3" (amoebum.sessions:conversation-history-entry-content (nth 3 entries)))))))

(test nxt-286-timestamps-are-non-decreasing
  "Conversation entries carry non-decreasing timestamps and
updated-at tracks the most recent add."
  (let ((c (%cu-fresh-conversation)))
    (%cu-add-user c "t0")
    (%cu-add-assistant c "t1")
    (%cu-add-user c "t2")
    (let* ((entries (amoebum.sessions:conversation-state-entries c))
           (stamps (mapcar #'amoebum.sessions:conversation-history-entry-timestamp
                           entries)))
      (is (every #'integerp stamps))
      (is (equal stamps (sort (copy-list stamps) #'<=))
          "Entry timestamps must be non-decreasing in insertion order.")
      (is (>= (amoebum.sessions:conversation-state-updated-at c)
              (first (last stamps)))
          "conversation-state-updated-at must be >= latest entry timestamp."))))

(test nxt-286-tool-call-pairing-matches-by-id
  "A tool result whose tool-call-id equals an earlier assistant entry's
tool-call-id is considered paired; %cu-tool-result-orphan-p returns NIL."
  (let ((c (%cu-fresh-conversation))
        (id "call-42"))
    (%cu-add-user c "please run the tool")
    (%cu-add-assistant c "calling" :tool-call-id id)
    (%cu-add-tool c "result payload" id)
    (let* ((entries (amoebum.sessions:conversation-state-entries c))
           (tool-entry (nth 2 entries)))
      (is (string= "tool"
                   (amoebum.sessions:conversation-history-entry-role tool-entry)))
      (is (string= id
                   (amoebum.sessions:conversation-history-entry-tool-call-id tool-entry))
          "Tool entry must keep the tool-call-id it was added with.")
      (is (not (%cu-tool-result-orphan-p c 2))
          "Tool result with a matching earlier assistant call must not be orphaned."))))

(test nxt-286-orphan-tool-result-is-detected
  "A tool result with a tool-call-id that has no preceding assistant
match is flagged by the orphan-detection helper."
  (let ((c (%cu-fresh-conversation)))
    (%cu-add-user c "hello")
    ;; Note: assistant does NOT carry tool-call-id "ghost".
    (%cu-add-assistant c "sure")
    (%cu-add-tool c "dangling result" "ghost")
    (is (%cu-tool-result-orphan-p c 2)
        "Orphan tool result must be flagged (no matching tool-call-id).")))

(test nxt-286-reset-clears-history
  "conversation-reset! empties entries, returns to :idle, and leaves
subsequent adds starting from index 0 again."
  (let ((c (%cu-fresh-conversation)))
    (%cu-add-user c "one")
    (%cu-add-assistant c "two")
    (%cu-add-user c "three")
    (is (= 3 (length (amoebum.sessions:conversation-state-entries c))))
    (amoebum.sessions:conversation-reset! c :save-p nil)
    (is (= 0 (length (amoebum.sessions:conversation-state-entries c)))
        "After conversation-reset!, entries must be empty.")
    (is (eq :idle (amoebum.sessions:conversation-state-state c))
        "Reset must return state to :idle.")
    (%cu-add-user c "fresh start")
    (is (= 1 (length (amoebum.sessions:conversation-state-entries c))))
    (is (string= "fresh start"
                 (amoebum.sessions:conversation-history-entry-content
                  (%cu-entry-at c 0))))))

(test nxt-286-entries-are-not-shared-with-caller-mutable-list
  "The entries list returned by conversation-state-entries must reflect
the current length of the conversation even if a caller captured an
earlier reference. This guards against aliasing bugs where a test or
caller accidentally holds a stale snapshot."
  (let ((c (%cu-fresh-conversation)))
    (%cu-add-user c "first")
    (let ((before-ref (amoebum.sessions:conversation-state-entries c)))
      (%cu-add-user c "second")
      ;; The current entries accessor must now show 2 regardless of
      ;; what before-ref points at.
      (is (= 2 (length (amoebum.sessions:conversation-state-entries c))))
      ;; And the prefix must still match.
      (is (string= "first"
                   (amoebum.sessions:conversation-history-entry-content
                    (first before-ref)))
          "Previously-captured reference must still expose the original first entry."))))

(test nxt-286-tool-call-pairing-survives-interleaving
  "With multiple tool calls in flight, each tool result pairs only to
the matching tool-call-id, regardless of position."
  (let ((c (%cu-fresh-conversation))
        (id-a "call-a")
        (id-b "call-b"))
    (%cu-add-user c "run two tools")
    (%cu-add-assistant c "first call" :tool-call-id id-a)
    (%cu-add-assistant c "second call" :tool-call-id id-b)
    ;; Results arrive in reverse order.
    (%cu-add-tool c "result-b" id-b)
    (%cu-add-tool c "result-a" id-a)
    (is (not (%cu-tool-result-orphan-p c 3)) "result-b must pair with call-b.")
    (is (not (%cu-tool-result-orphan-p c 4)) "result-a must pair with call-a.")
    ;; Spot-check: a synthetic orphan id is still detected.
    (%cu-add-tool c "nobody" "call-c")
    (is (%cu-tool-result-orphan-p c 5) "unmatched call-c must be orphan.")))
