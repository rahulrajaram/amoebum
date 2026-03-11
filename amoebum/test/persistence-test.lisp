(in-package :amoebum/test)

;;; ============================================================
;;; I249: Image Persistence — Pre-save Cleanup / Post-restore Init
;;; ============================================================

(def-suite persistence-suite :in amoebum-suite)
(in-suite persistence-suite)

(test image-directory-resolvable
  "image-directory should return a pathname."
  (let ((dir (amoebum:image-directory)))
    (is (pathnamep dir))))

(test pre-save-closes-tracked-streams
  "Pre-save cleanup should close tracked file descriptors."
  (let* ((tmp-dir (%make-temp-directory "amoebum-i249-fd"))
         (path (merge-pathnames #P"tracked.log" tmp-dir))
         (stream nil))
    (unwind-protect
        (progn
          (ensure-directories-exist path)
          (setf stream (open path
                             :direction :output
                             :if-exists :supersede
                             :if-does-not-exist :create
                             :external-format :utf-8))
          (let ((amoebum::*image-tracked-streams* nil)
                (amoebum::*image-fd-cleanup-hooks* (list #'amoebum::%close-image-tracked-streams))
                (amoebum::*image-network-drain-hooks* nil)
                (amoebum::*image-agent-checkpoint-hooks* nil)
                (amoebum::*image-terminal-snapshot-hooks* nil)
                (amoebum::*image-pre-save-hooks* nil))
            (amoebum::register-image-tracked-stream stream)
            (let ((report (amoebum::%image-pre-save-cleanup)))
              (is-true (not (open-stream-p stream)))
              (is (>= (getf report :fd-cleanup-count 0) 1))
              (is (null amoebum::*image-tracked-streams*)))))
      (when (and stream (open-stream-p stream))
        (close stream))
      (%delete-directory-tree-safe tmp-dir))))

(test pre-save-phase-order-runs-extension-hooks-last
  "Pre-save phases should execute in tranche order with extensions last."
  (let ((order '()))
    (let ((amoebum::*image-fd-cleanup-hooks*
            (list (lambda ()
                    (push :fd order)
                    1)))
          (amoebum::*image-network-drain-hooks*
            (list (lambda ()
                    (push :network order)
                    1)))
          (amoebum::*image-agent-checkpoint-hooks*
            (list (lambda ()
                    (push :agent order)
                    1)))
          (amoebum::*image-terminal-snapshot-hooks*
            (list (lambda (snapshot)
                    (declare (ignore snapshot))
                    (push :terminal order)
                    (list :backend :ansi))))
          (amoebum::*image-pre-save-hooks*
            (list (lambda ()
                    (push :extension order)
                    t))))
      (finishes (amoebum::%image-pre-save-cleanup))
      (is (equal (reverse order)
                 '(:fd :network :agent :terminal :extension))))))

(test post-restore-rehydrates-subsystems-and-emits-event
  "Post-restore should reopen terminal, reconnect MCP/API, emit system:restored, then run extensions."
  (let* ((bus (amoebum:make-event-bus :capacity 16))
         (order '())
         (terminal-state (list :term "xterm-256color" :columns "120" :lines "40")))
    (amoebum:subscribe bus
                       "*"
                       (lambda (event)
                         (when (string= "SYSTEM:RESTORED"
                                        (symbol-name (amoebum:event-type event)))
                           (push :event order))))
    (let ((amoebum::*image-terminal-reopen-hooks*
            (list (lambda (state)
                    (declare (ignore state))
                    (push :terminal order)
                    1)))
          (amoebum::*image-mcp-reconnect-hooks*
            (list (lambda ()
                    (push :mcp order)
                    1)))
          (amoebum::*image-api-reauth-hooks*
            (list (lambda ()
                    (push :api order)
                    1)))
          (amoebum::*image-post-restore-hooks*
            (list (lambda (&optional _report)
                    (declare (ignore _report))
                    (push :extension order)
                    1))))
      (let ((report (amoebum::%image-post-restore-init
                     :event-bus bus
                     :terminal-state terminal-state)))
        (is (equal terminal-state (getf report :terminal-state)))
        (is (= 1 (getf report :terminal-reopen-count)))
        (is (= 1 (getf report :mcp-reconnect-count)))
        (is (= 1 (getf report :api-reauth-count)))
        (is (= 1 (getf report :extension-hook-count)))))
    (let ((events (amoebum:event-history bus)))
      (is (= 1 (length events)))
      (is (string= "SYSTEM:RESTORED"
                   (symbol-name (amoebum:event-type (first events))))))
    (is (equal (reverse order)
               '(:terminal :mcp :api :event :extension)))))

(test post-restore-hooks-receive-terminal-state
  "Terminal reopen hooks should receive the captured terminal state."
  (let* ((bus (amoebum:make-event-bus :capacity 8))
         (expected (list :term "screen" :columns "200" :lines "60"))
         (captured nil))
    (let ((amoebum::*image-terminal-reopen-hooks*
            (list (lambda (state)
                    (setf captured state)
                    1)))
          (amoebum::*image-mcp-reconnect-hooks* nil)
          (amoebum::*image-api-reauth-hooks* nil)
          (amoebum::*image-post-restore-hooks* nil))
      (finishes (amoebum::%image-post-restore-init
                 :event-bus bus
                 :terminal-state expected))
      (is (equal expected captured)))))

(test image-hooks-error-resilient
  "Errors in lifecycle hooks should not escape cleanup/init."
  (let ((amoebum::*image-fd-cleanup-hooks*
          (list (lambda () (error "fd-fail"))))
        (amoebum::*image-network-drain-hooks*
          (list (lambda () (error "network-fail"))))
        (amoebum::*image-agent-checkpoint-hooks*
          (list (lambda () (error "agent-fail"))))
        (amoebum::*image-terminal-snapshot-hooks*
          (list (lambda (_) (declare (ignore _)) (error "terminal-fail"))))
        (amoebum::*image-terminal-reopen-hooks*
          (list (lambda (_) (declare (ignore _)) (error "reopen-fail"))))
        (amoebum::*image-mcp-reconnect-hooks*
          (list (lambda () (error "mcp-fail"))))
        (amoebum::*image-api-reauth-hooks*
          (list (lambda () (error "auth-fail"))))
        (amoebum::*image-pre-save-hooks*
          (list (lambda () (error "pre-ext-fail"))))
        (amoebum::*image-post-restore-hooks*
          (list (lambda (&optional _) (declare (ignore _)) (error "post-ext-fail")))))
    (let ((*error-output* (make-string-output-stream)))
      (finishes (amoebum::%image-pre-save-cleanup))
      (finishes (amoebum::%image-post-restore-init
                 :event-bus (amoebum:make-event-bus :capacity 8)
                 :terminal-state (list :term "xterm"))))))

(test persistence-smoke-sentinel
  (is-true t)
  (format t "PERSISTENCE_SMOKE_OK~%"))
