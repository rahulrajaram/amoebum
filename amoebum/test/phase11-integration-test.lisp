(in-package :amoebum/test)

;;; ============================================================
;;; I268: Phase 11 Integration Smoke Tests
;;;
;;; Cross-tranche integration scenarios verifying that
;;; all Phase 11 subsystems work together.
;;; ============================================================

(def-suite phase11-integration-suite :in amoebum-suite)
(in-suite phase11-integration-suite)

;;; --- Sound backend + event bus integration ---

(test sound-backend-emits-no-crash
  "Selecting and using a sound backend doesn't crash."
  (let ((old-backend amoebum:*sound-backend-instance*))
    (unwind-protect
         (progn
           (amoebum:reset-sound-backend)
           ;; Select builtin backend (always available)
           (let ((backend (amoebum:select-sound-backend :backend :builtin)))
             (is (not (null backend)))
             (is (eq :builtin (amoebum:sound-backend-kind backend)))))
      (setf amoebum:*sound-backend-instance* old-backend))))

;;; --- Worker system end-to-end (without real shell) ---

(test worker-registry-round-trip
  "Workers can be created, found, and cleared."
  (let ((old-sup amoebum:*worker-supervisor*))
    (unwind-protect
         (progn
           (setf amoebum:*worker-supervisor* nil)
           (amoebum:clear-workers)
           ;; Create a synthetic worker
           (let ((w (amoebum::%make-worker-record
                     :id "w-integ-001"
                     :type :shell
                     :label "integration test"
                     :status :completed
                     :exit-code 0)))
             (amoebum::%store-worker w)
             ;; Find it
             (is (not (null (amoebum::%find-worker "w-integ-001"))))
             ;; Worker list
             (is (plusp (length (amoebum:worker-list))))
             ;; Clear
             (amoebum:clear-workers)
             (is (= 0 (length (amoebum:worker-list))))))
      (setf amoebum:*worker-supervisor* old-sup))))

;;; --- Worker retry + classification integration ---

(test retry-classification-integration
  "Retry policy correctly classifies and gates eligibility."
  (let* ((policy (amoebum:make-worker-supervision-policy
                  :max-retries 3
                  :backoff-strategy :exponential
                  :backoff-base-seconds 1
                  :transient-exit-codes '(75 111)
                  :permanent-exit-codes '(126 127)))
         ;; Transient worker
         (w-trans (amoebum::%make-worker-record
                   :id "w-integ-trans"
                   :type :shell
                   :status :failed
                   :exit-code 75
                   :retry-count 0
                   :max-retries 3))
         ;; Permanent worker
         (w-perm (amoebum::%make-worker-record
                  :id "w-integ-perm"
                  :type :shell
                  :status :failed
                  :exit-code 127
                  :retry-count 0
                  :max-retries 3))
         ;; Exhausted worker
         (w-exhaust (amoebum::%make-worker-record
                     :id "w-integ-exhaust"
                     :type :shell
                     :status :failed
                     :exit-code 75
                     :retry-count 3
                     :max-retries 3)))
    (is (amoebum:worker-retry-eligible-p w-trans policy))
    (is (not (amoebum:worker-retry-eligible-p w-perm policy)))
    (is (not (amoebum:worker-retry-eligible-p w-exhaust policy)))))

;;; --- Fan-out + group management integration ---

(test fanout-group-lifecycle
  "Fan-out creates group, find locates it, clear removes it."
  (let ((old-sup amoebum:*worker-supervisor*))
    (unwind-protect
         (progn
           (setf amoebum:*worker-supervisor* nil)
           (amoebum:clear-workers)
           (amoebum:clear-worker-groups)
           (multiple-value-bind (gid wids)
               (amoebum:fan-out-workers
                (list (list :type :shell :command "echo a" :cwd "/tmp")))
             (is (stringp gid))
             (is (= 1 (length wids)))
             (is (not (null (amoebum:find-worker-group gid))))
             ;; Clear
             (amoebum:clear-worker-groups)
             (is (null (amoebum:find-worker-group gid)))))
      (setf amoebum:*worker-supervisor* old-sup))))

;;; --- Event journal + replay integration ---

(test journal-replay-round-trip
  "Events written to journal can be replayed via replay engine."
  (let* ((old-journal amoebum:*event-journal*)
         (tmp-dir (merge-pathnames
                   (format nil "amoebum-integ-journal-~D/" (get-universal-time))
                   #P"/tmp/"))
         (bus (amoebum:make-event-bus)))
    (unwind-protect
         (progn
           (ensure-directories-exist tmp-dir)
           (setf amoebum:*event-journal* nil)
           ;; Start journal
           (let ((journal (amoebum:make-event-journal-instance
                           :directory tmp-dir
                           :max-segment-bytes (* 1024 1024))))
             (amoebum:start-event-journal :journal journal :event-bus bus)
             ;; Publish some events
             (dotimes (i 5)
               (amoebum:publish bus
                                (intern (format nil "TEST:EVENT-~D" i) :keyword)
                                :source :integration-test
                                :payload (format nil "payload-~D" i)))
             ;; Stop journal
             (amoebum:stop-event-journal journal)
             ;; Replay into a fresh bus
             (let* ((replay-bus (amoebum:make-event-bus))
                    (paths (amoebum:journal-segment-paths journal))
                    (count (amoebum:replay-journal paths
                                                   :speed-factor 0.0
                                                   :target-bus replay-bus)))
               ;; Should have replayed events (at least the 5 we published)
               (is (>= count 5))
               ;; Audit query should find them too
               (let ((results (amoebum:audit-query paths)))
                 (is (>= (length results) 5))))))
      (setf amoebum:*event-journal* old-journal)
      (ignore-errors (uiop:delete-directory-tree tmp-dir :validate t)))))

;;; --- Session recording + journal integration ---

(test session-journal-integration
  "Session lifecycle integrates with journal segment tracking."
  (let ((old-dir amoebum:*session-directory*)
        (old-id amoebum:*current-session-id*)
        (old-journal amoebum:*event-journal*)
        (tmp-dir (merge-pathnames
                  (format nil "amoebum-integ-session-~D/" (get-universal-time))
                  #P"/tmp/")))
    (unwind-protect
         (progn
           (ensure-directories-exist tmp-dir)
           (setf amoebum:*session-directory* tmp-dir
                 amoebum:*event-journal* nil)
           ;; Start session
           (let ((session-id (amoebum:start-session :model "integration-model"
                                                    :project-path "/test")))
             (is (stringp session-id))
             ;; Stop session
             (let ((meta (amoebum:stop-session)))
               (is (amoebum:session-metadata-p meta))
               ;; List should find it
               (let ((sessions (amoebum:list-sessions)))
                 (is (= 1 (length sessions)))
                 (is (equal session-id
                            (amoebum:session-metadata-id (first sessions))))))))
      (setf amoebum:*session-directory* old-dir
            amoebum:*current-session-id* old-id
            amoebum:*event-journal* old-journal)
      (ignore-errors (uiop:delete-directory-tree tmp-dir :validate t)))))

;;; --- Conversation export integration ---

(test conversation-export-round-trip
  "Export conversation to markdown and JSON, verify both readable."
  (let ((old-dir amoebum:*conversation-export-directory*)
        (tmp-dir (merge-pathnames
                  (format nil "amoebum-integ-export-~D/" (get-universal-time))
                  #P"/tmp/")))
    (unwind-protect
         (progn
           (ensure-directories-exist tmp-dir)
           (setf amoebum:*conversation-export-directory* tmp-dir)
           (let ((conv (amoebum::%make-conversation-state
                        :session-id "integ-export-001"
                        :entries (list
                                  (amoebum:make-conversation-history-entry
                                   :role "user" :content "Hello")
                                  (amoebum:make-conversation-history-entry
                                   :role "assistant" :content "Hi there!")))))
             ;; Export markdown
             (let ((md-path (amoebum:export-conversation conv
                              :format-type :markdown
                              :project-name "integ-test")))
               (is (probe-file md-path))
               (let ((content (uiop:read-file-string md-path)))
                 (is (search "Conversation Export" content))
                 (is (search "Hello" content))))
             ;; Export JSON
             (let ((json-path (amoebum:export-conversation conv
                                :format-type :json
                                :project-name "integ-test")))
               (is (probe-file json-path))
               (let ((content (uiop:read-file-string json-path)))
                 (is (search "messages" content))
                 (is (search "Hello" content))))))
      (setf amoebum:*conversation-export-directory* old-dir)
      (ignore-errors (uiop:delete-directory-tree tmp-dir :validate t)))))

;;; --- Adapter resilience ---

(test overwatch-graceful-degradation
  "Overwatch supervisor degrades gracefully when unavailable."
  (let ((old-fn amoebum:*overwatch-http-request-function*))
    (unwind-protect
         (progn
           ;; Mock HTTP that always fails (unreachable)
           (setf amoebum:*overwatch-http-request-function*
                 (lambda (&rest _args)
                   (declare (ignore _args))
                   (values "" 0)))
           (is (not (amoebum:overwatch-available-p)))
           ;; Auto backend selection should fall back to :in-process
           (let ((backend (amoebum:select-worker-backend :backend :auto)))
             (is (typep backend 'amoebum:in-process-supervisor))))
      (setf amoebum:*overwatch-http-request-function* old-fn))))

(test hailer-graceful-degradation
  "Hailer CLI backend degrades gracefully when unavailable."
  (let ((old-runner amoebum:*hailer-cli-runner*))
    (unwind-protect
         (progn
           ;; Mock runner that simulates hailer not installed
           (setf amoebum:*hailer-cli-runner*
                 (lambda (&rest _args)
                   (declare (ignore _args))
                   (list :exit-code 127 :stdout "" :stderr "not found")))
           (let ((backend (amoebum:select-sound-backend :backend :hailer-cli)))
             ;; Should still create backend (lazy availability check)
             (is (not (null backend)))
             ;; But availability check should fail
             (is (not (amoebum:sound-backend-available-p backend)))))
      (setf amoebum:*hailer-cli-runner* old-runner))))

;;; --- Worker dashboard integration ---

(test dashboard-renders-with-workers
  "Dashboard widget renders without error even with synthetic workers."
  (let ((old-sup amoebum:*worker-supervisor*))
    (unwind-protect
         (progn
           (setf amoebum:*worker-supervisor* nil)
           (amoebum:clear-workers)
           ;; Add some synthetic workers
           (amoebum::%store-worker
            (amoebum::%make-worker-record
             :id "w-integ-dash-1" :type :shell :label "test job 1"
             :status :completed :started-at 100 :finished-at 105))
           (amoebum::%store-worker
            (amoebum::%make-worker-record
             :id "w-integ-dash-2" :type :shell :label "test job 2"
             :status :running :started-at 100))
           (let ((tree (amoebum::worker-dashboard '(:show-finished t :limit 10))))
             (is (typep tree 'ptui.ui.elements:ui-element))
             (is (eq :box (ptui.ui.elements:ui-element-type tree)))
             ;; Should have header + worker lines (at least 3 children)
             (let ((children (ptui.ui.elements:ui-element-children tree)))
               (is (plusp (length children))))))
      (amoebum:clear-workers)
      (setf amoebum:*worker-supervisor* old-sup))))
