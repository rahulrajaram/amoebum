(in-package :amoebum/test)

(def-suite tts-suite
  :description "I230 Kokoro TTS integration."
  :in amoebum-suite)

(in-suite tts-suite)

(defun %wait-until (predicate &key (timeout-seconds 1.0d0) (sleep-seconds 0.01d0))
  (let ((deadline (+ (get-internal-real-time)
                     (round (* timeout-seconds internal-time-units-per-second)))))
    (loop
      (when (funcall predicate)
        (return t))
      (when (>= (get-internal-real-time) deadline)
        (return nil))
      (sleep sleep-seconds))))

(test kokoro-backend-queues-segments-sequentially
  (let ((calls '()))
    (let ((backend
            (amoebum:make-kokoro-tts-backend
             :voice "af_test"
             :run-command-function
             (lambda (command)
               (push command calls)
               (list :exit-code 0 :stdout "" :stderr "")))))
      (is (= 3 (amoebum:speak-text backend (format nil "one~%two~%three"))))
      (is-true (%wait-until (lambda () (= (length calls) 3)) :timeout-seconds 2.0d0))
      (let ((ordered (reverse calls)))
        (is (search "one" (first ordered) :test #'char-equal))
        (is (search "two" (second ordered) :test #'char-equal))
        (is (search "three" (third ordered) :test #'char-equal)))
      (is-false (amoebum:speaking-p backend)))))

(test speak-slash-command-speaks-latest-assistant-response
  (let ((calls '())
        (old-backend amoebum:*tts-backend*))
    (unwind-protect
        (let* ((backend
                 (amoebum:make-kokoro-tts-backend
                  :voice "af_test"
                  :run-command-function
                  (lambda (command)
                    (push command calls)
                    (list :exit-code 0 :stdout "" :stderr ""))))
               (chat-state
                 (amoebum.ui:make-chat-ui-state
                  :messages (list (pseudopod:make-message :role "user" :content "hi")
                                  (pseudopod:make-message :role "assistant"
                                                          :content "latest reply")))))
          (setf amoebum:*tts-backend* backend)
          (multiple-value-bind (handled result)
              (amoebum:dispatch-slash-command "/speak"
                                              :config (amoebum.config:current-config)
                                              :chat-state chat-state)
            (is-true handled)
            (is-true (typep result 'amoebum.commands:slash-command-result))
            (is (search "Speaking last assistant response"
                        (or (amoebum.commands:slash-command-result-output result) "")
                        :test #'char-equal)))
          (is-true (%wait-until (lambda () (plusp (length calls))) :timeout-seconds 2.0d0))
          (is (search "latest reply" (first calls) :test #'char-equal)))
      (setf amoebum:*tts-backend* old-backend))))

(test auto-speak-hook-respects-configuration
  (let ((calls '())
        (old-backend amoebum:*tts-backend*)
        (cfg (amoebum.config:current-config))
        (old-auto nil))
    (unwind-protect
        (progn
          (setf old-auto (amoebum.config:config-value :tts-auto-speak cfg))
          (setf amoebum:*tts-backend*
                (amoebum:make-kokoro-tts-backend
                 :voice "af_test"
                 :run-command-function
                 (lambda (command)
                   (push command calls)
                   (list :exit-code 0 :stdout "" :stderr ""))))
          (amoebum:enable-tts-post-receive-hook)
          (amoebum.config:setconfig :tts-auto-speak nil)
          (amoebum:run-hooks :post-receive "do not speak")
          (sleep 0.05d0)
          (is (= 0 (length calls)))
          (amoebum.config:setconfig :tts-auto-speak t)
          (amoebum:run-hooks :post-receive "speak now")
          (is-true (%wait-until (lambda () (= (length calls) 1)) :timeout-seconds 2.0d0))
          (is (search "speak now" (first calls) :test #'char-equal)))
      (setf amoebum:*tts-backend* old-backend)
      (amoebum.config:setconfig :tts-auto-speak old-auto))))

(test tts-smoke-sentinel
  (is-true t)
  (format t "TTS_SMOKE_OK~%"))
