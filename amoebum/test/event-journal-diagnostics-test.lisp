(in-package :amoebum/test)

(def-suite event-journal-diagnostics-suite :in amoebum-suite)
(in-suite event-journal-diagnostics-suite)

(test journal-captures-handoff-partial-result-diagnostics
  "User handoff completion/failure events should persist structured diagnostics in JSONL."
  (let* ((tmp-root (%make-temp-directory "event-journal-handoff"))
         (dir (merge-pathnames #P"journal/" tmp-root))
         (bus (amoebum:make-event-bus :capacity 128))
         (amoebum:*event-bus* bus)
         (j (amoebum:make-event-journal-instance :directory dir))
         (amoebum::*user-session-registry* (sw4rm-sdk:make-local-registry))
         (amoebum::*user-session->agent-id* (make-hash-table :test #'equal))
         (amoebum::*user-session->user-id* (make-hash-table :test #'equal))
         (amoebum::*user-agent->session-id* (make-hash-table :test #'equal))
         (amoebum::*user-negotiation-room-participants* (make-hash-table :test #'equal))
         (amoebum::*user-handoff-client* nil)
         (amoebum::*user-negotiation-client* nil)
         (amoebum::*user-handoff-sequence* 0)
         (amoebum::*user-handoff-details* (make-hash-table :test #'equal))
         (amoebum::*user-coordination-lock*
           (bordeaux-threads:make-lock "event-journal-handoff-lock")))
    (unwind-protect
         (progn
           (amoebum:start-event-journal :journal j :event-bus bus)
           (amoebum:register-user-session-peer "session-from" :user-id "alice")
           (amoebum:register-user-session-peer "session-to" :user-id "bob")
           (let* ((response (amoebum:handoff-between-users
                             "session-from"
                             "session-to"
                             "Need a fallback reviewer"))
                  (handoff-id (getf response :handoff-id)))
             (amoebum:reject-user-handoff
              handoff-id
              "overloaded"
              :partial-result '(:summary "Collected partial notes.")
              :diagnostics '((:severity :error :message "delegate overloaded"))
              :provenance '(:worker-id "delegate-9")))
           (sleep 0.1)
           (amoebum:stop-event-journal j)
           (let ((paths (amoebum:journal-segment-paths j)))
             (is (>= (length paths) 1))
             (let ((content (uiop:read-file-string (first paths))))
               (is (search "USER-HANDOFF-REJECTED" (string-upcase content)))
               (is (search "partial-result" content :test #'char-equal))
               (is (search "delegate overloaded" content :test #'char-equal))
               (is (search "delegation-trace" content :test #'char-equal)))))
      (ignore-errors (amoebum:stop-event-journal j))
      (ignore-errors
        (%delete-directory-tree-safe tmp-root)))))
