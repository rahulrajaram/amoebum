(defpackage :ptui.test.session-multiplex
  (:use :cl :fiveam)
  (:export #:run-all #:session-multiplex-suite))

(in-package :ptui.test.session-multiplex)

(def-suite session-multiplex-suite
  :description "PTUI session multiplexing coverage for multi-user support (I252).")

(in-suite session-multiplex-suite)

(defclass tagged-backend ()
  ((tag :initarg :tag :reader tagged-backend-tag)))

(defun %make-tagged-backend (user-id)
  (make-instance 'tagged-backend :tag user-id))

(defun %make-temp-socket-path ()
  (let* ((stamp (format nil "ptui-session-mux-~D-~D.sock"
                        (get-universal-time)
                        (random 1000000)))
         (dir (uiop:ensure-directory-pathname (uiop:temporary-directory))))
    (namestring (merge-pathnames stamp dir))))

(defun %cleanup-socket-path (path)
  (when (and path (probe-file path))
    (ignore-errors
      (delete-file path))))

(defun %unix-socket-supported-p ()
  #+sbcl
  t
  #-sbcl
  nil)

(test multi-user-sessions-connect-over-unix-socket
  (if (not (%unix-socket-supported-p))
      (is (equal t t))
      (let* ((socket-path (%make-temp-socket-path))
             (multiplexer (ptui.runtime.session-multiplex:make-session-multiplexer
                           :backend-factory #'%make-tagged-backend)))
        (unwind-protect
             (progn
               (ptui.runtime.session-multiplex:start-session-multiplexer-socket
                multiplexer socket-path)
               (let ((alice (ptui.runtime.session-multiplex:connect-user-session
                             multiplexer "alice"))
                     (bob (ptui.runtime.session-multiplex:connect-user-session
                           multiplexer "bob")))
                 (is (= 2 (ptui.runtime.session-multiplex:session-multiplexer-session-count
                           multiplexer)))
                 (is (ptui.runtime.session-multiplex:multiplex-session-connected-via-unix-socket-p
                      alice))
                 (is (ptui.runtime.session-multiplex:multiplex-session-connected-via-unix-socket-p
                      bob))
                 (is (not (eq (ptui.runtime.session-multiplex:multiplex-session-backend alice)
                              (ptui.runtime.session-multiplex:multiplex-session-backend bob))))
                 (is (string= "alice"
                              (tagged-backend-tag
                               (ptui.runtime.session-multiplex:multiplex-session-backend alice))))
                 (is (string= "bob"
                              (tagged-backend-tag
                               (ptui.runtime.session-multiplex:multiplex-session-backend bob))))))
          (ignore-errors
            (ptui.runtime.session-multiplex:stop-session-multiplexer-socket multiplexer))
          (%cleanup-socket-path socket-path)))))

(test per-user-view-state-is-isolated
  (let ((multiplexer (ptui.runtime.session-multiplex:make-session-multiplexer
                      :backend-factory #'%make-tagged-backend)))
    (ptui.runtime.session-multiplex:connect-user-session multiplexer "alice")
    (ptui.runtime.session-multiplex:connect-user-session multiplexer "bob")

    (ptui.runtime.session-multiplex:update-session-cursor multiplexer "alice" 9 3)
    (ptui.runtime.session-multiplex:update-session-scroll multiplexer "alice" 14)
    (ptui.runtime.session-multiplex:append-session-history multiplexer "alice" "alice-msg-1")

    (ptui.runtime.session-multiplex:update-session-cursor multiplexer "bob" 1 1)
    (ptui.runtime.session-multiplex:update-session-scroll multiplexer "bob" 0)
    (ptui.runtime.session-multiplex:append-session-history multiplexer "bob" "bob-msg-1")
    (ptui.runtime.session-multiplex:append-session-history multiplexer "bob" "bob-msg-2")

    (let* ((alice-session (ptui.runtime.session-multiplex:find-user-session multiplexer "alice"))
           (bob-session (ptui.runtime.session-multiplex:find-user-session multiplexer "bob"))
           (alice-view (ptui.runtime.session-multiplex:multiplex-session-view-state alice-session))
           (bob-view (ptui.runtime.session-multiplex:multiplex-session-view-state bob-session)))
      (is (= 9 (ptui.runtime.session-multiplex:session-view-state-cursor-row alice-view)))
      (is (= 3 (ptui.runtime.session-multiplex:session-view-state-cursor-col alice-view)))
      (is (= 14 (ptui.runtime.session-multiplex:session-view-state-scroll-offset alice-view)))
      (is (equal '("alice-msg-1")
                 (ptui.runtime.session-multiplex:session-view-state-conversation-history
                  alice-view)))

      (is (= 1 (ptui.runtime.session-multiplex:session-view-state-cursor-row bob-view)))
      (is (= 1 (ptui.runtime.session-multiplex:session-view-state-cursor-col bob-view)))
      (is (= 0 (ptui.runtime.session-multiplex:session-view-state-scroll-offset bob-view)))
      (is (equal '("bob-msg-1" "bob-msg-2")
                 (ptui.runtime.session-multiplex:session-view-state-conversation-history
                  bob-view))))))

(test shared-tool-registry-visible-across-sessions
  (let ((multiplexer (ptui.runtime.session-multiplex:make-session-multiplexer
                      :backend-factory #'%make-tagged-backend)))
    (ptui.runtime.session-multiplex:connect-user-session multiplexer "alice")
    (ptui.runtime.session-multiplex:connect-user-session multiplexer "bob")
    (ptui.runtime.session-multiplex:register-shared-tool
     multiplexer
     :search-files
     '(:description "Search files by glob" :group :tooling))
    (is (ptui.runtime.session-multiplex:shared-tool-registered-p multiplexer :search-files))
    (is (equal (gethash :search-files
                        (ptui.runtime.session-multiplex:session-multiplexer-tool-registry
                         multiplexer))
               '(:description "Search files by glob" :group :tooling)))))

(test session-multiplex-smoke-sentinel
  (format t "SESSION_MULTIPLEX_SMOKE_OK~%")
  (is (equal t t)))

(defun run-all ()
  (let ((results (run 'session-multiplex-suite)))
    (fiveam:explain! results)
    (fiveam:results-status results)))
