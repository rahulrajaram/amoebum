(in-package :amoebum/test)

(def-suite entry-spine-suite
  :in amoebum-suite
  :description "NXT-318 entry-spine launch/runtime invariants.")

(in-suite entry-spine-suite)

(test resolve-chat-launch-state-demo-owns-demo-shell
  (let* ((interactive-state (amoebum::make-chat-ui-state
                             :conversation (amoebum::make-conversation-state
                                            :session-id "interactive")))
         (resolved (amoebum::%resolve-chat-launch-state
                    :initial-state interactive-state
                    :demo t)))
    (is (typep resolved 'amoebum::chat-ui-state))
    (is (not (eq resolved interactive-state)))
    (is-true (amoebum::chat-ui-state-demo-mode-p resolved))
    (is (null (amoebum::chat-ui-state-stream-client resolved)))
    (is (eq (amoebum::chat-ui-state-stream-runner resolved)
            #'amoebum::demo-stream-runner))))

(test resolve-interactive-chat-state-skips-conversation-in-demo
  (let ((state (amoebum::%resolve-interactive-chat-state :demo-mode-p t
                                                         :session-id "ignored"
                                                         :resume "ignored")))
    (is (null state))))

(test run-interactive-chat-ui-aligns-demo-and-session-launch
  (let ((original-run-chat-ui (symbol-function 'amoebum:run-chat-ui))
        (original-resolve (symbol-function 'amoebum::%resolve-cli-conversation))
        (calls '()))
    (unwind-protect
         (progn
           (setf (symbol-function 'amoebum::%resolve-cli-conversation)
                 (lambda (&key session-id resume)
                   (push (list :resolve session-id resume) calls)
                   (amoebum::make-conversation-state
                    :session-id (or session-id resume "resolved"))))
           (setf (symbol-function 'amoebum:run-chat-ui)
                 (lambda (&key backend fps initial-state demo)
                   (push (list :run backend fps initial-state demo) calls)
                   :ok))
           (is (eq :ok (amoebum::%run-interactive-chat-ui :demo-mode-p t)))
           (is (equal (first calls) (list :run :auto 60 nil t)))
           (is (= 1 (length calls)))
           (setf calls '())
           (is (eq :ok (amoebum::%run-interactive-chat-ui
                        :session-id "sess-42"
                        :resume "")))
           (destructuring-bind (_ backend fps initial-state demo)
               (first calls)
             (declare (ignore _))
             (is (eq backend :auto))
             (is (= fps 60))
             (is-false demo)
             (is (typep initial-state 'amoebum::chat-ui-state))
             (is (string= (amoebum::conversation-state-session-id
                           (amoebum::chat-ui-state-conversation initial-state))
                          "sess-42")))
           (is (equal (second calls) (list :resolve "sess-42" ""))))
      (setf (symbol-function 'amoebum:run-chat-ui) original-run-chat-ui
            (symbol-function 'amoebum::%resolve-cli-conversation) original-resolve))))

(test chat-runtime-shell-restores-approval-flag-after-body
  (let ((original-load-user-extensions (symbol-function 'amoebum::load-user-extensions))
        (original-enable-gc-telemetry (symbol-function 'amoebum::enable-gc-telemetry))
        (original-disable-gc-telemetry (symbol-function 'amoebum::disable-gc-telemetry))
        (original-open-chat-log-stream (symbol-function 'amoebum::%open-chat-log-stream))
        (original-make-thread (symbol-function 'bt:make-thread))
        (original-join-thread (symbol-function 'bt:join-thread))
        (original-checkpoint (symbol-function 'amoebum::checkpoint-session))
        (checkpoint-calls 0))
    (unwind-protect
         (progn
           (setf (symbol-function 'amoebum::load-user-extensions)
                 (lambda (&key project-root global-directory project-directory)
                   (declare (ignore project-root global-directory project-directory))
                   nil)
                 (symbol-function 'amoebum::enable-gc-telemetry)
                 (lambda () nil)
                 (symbol-function 'amoebum::disable-gc-telemetry)
                 (lambda () nil)
                 (symbol-function 'amoebum::%open-chat-log-stream)
                 (lambda () nil)
                 (symbol-function 'bt:make-thread)
                 (lambda (fn &key name)
                   (declare (ignore fn name))
                   :fake-thread)
                 (symbol-function 'bt:join-thread)
                 (lambda (thread)
                   (declare (ignore thread))
                   nil)
                 (symbol-function 'amoebum::checkpoint-session)
                 (lambda (&key conversation trigger auto-p)
                   (declare (ignore conversation trigger auto-p))
                   (incf checkpoint-calls)
                   :checkpoint))
           (let ((amoebum::*approval-ui-active-p* nil)
                 (state (amoebum::make-chat-ui-state
                         :conversation (amoebum::make-conversation-state
                                        :session-id "nxt-318"))))
             (is (eq state
                     (amoebum::%call-with-chat-runtime-shell
                      (lambda ()
                        (is-true amoebum::*approval-ui-active-p*)
                        state)
                      :demo nil)))
             (is-false amoebum::*approval-ui-active-p*)
             (is (= 1 checkpoint-calls)))
           (setf checkpoint-calls 0)
           (let ((amoebum::*approval-ui-active-p* nil))
             (is (null
                  (amoebum::%call-with-chat-runtime-shell
                   (lambda ()
                     (is-true amoebum::*approval-ui-active-p*)
                     nil)
                   :demo t)))
             (is-false amoebum::*approval-ui-active-p*)
             (is (= 0 checkpoint-calls))))
      (setf (symbol-function 'amoebum::load-user-extensions) original-load-user-extensions
            (symbol-function 'amoebum::enable-gc-telemetry) original-enable-gc-telemetry
            (symbol-function 'amoebum::disable-gc-telemetry) original-disable-gc-telemetry
            (symbol-function 'amoebum::%open-chat-log-stream) original-open-chat-log-stream
            (symbol-function 'bt:make-thread) original-make-thread
            (symbol-function 'bt:join-thread) original-join-thread
            (symbol-function 'amoebum::checkpoint-session) original-checkpoint))))
