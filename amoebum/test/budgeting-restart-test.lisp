(in-package :amoebum/test)

(def-suite budgeting-restart-suite :in amoebum-suite
  :description "I364 budget exhaustion restarts and summarize-and-finish fallback.")

(in-suite budgeting-restart-suite)

(defun %i364-long-summary ()
  (with-output-to-string (out)
    (dotimes (index 80)
      (format out "segment-~D " index))))

(test i364-budget-exhaustion-default-selector-summarizes
  (let* ((summary (%i364-long-summary))
         (result
           (let ((amoebum:*budget-exhaustion-restart-selector*
                   #'amoebum:default-budget-exhaustion-restart-selector))
             (amoebum:handle-budget-exhaustion
              :kind :token
              :used 120
              :budget 100
              :context-summary summary
              :max-partial-output-chars 96))))
    (is (eq :summarize-and-finish (getf result :action)))
    (is (stringp (getf result :partial-output)))
    (is (<= (length (getf result :partial-output)) 96))
    (is-true (search "Budget exhausted"
                     (getf result :partial-output)
                     :test #'char-equal))))

(test i364-budget-exhaustion-selector-can-extend-budget
  (let ((amoebum:*budget-exhaustion-restart-selector*
          (lambda (_condition)
            (declare (ignore _condition))
            '(extend-budget 42))))
    (let ((result (amoebum:handle-budget-exhaustion
                   :kind :token
                   :used 81
                   :budget 80)))
      (is (eq :extend-budget (getf result :action)))
      (is (= 42 (getf result :extra-budget))))))

(test i364-budget-exhaustion-selector-can-abort-task
  (let ((amoebum:*budget-exhaustion-restart-selector*
          (lambda (_condition)
            (declare (ignore _condition))
            '(abort-task "manual abort"))))
    (let ((result (amoebum:handle-budget-exhaustion
                   :kind :token
                   :used 81
                   :budget 80)))
      (is (eq :abort-task (getf result :action)))
      (is (string= "manual abort" (getf result :reason))))))

(test i364-budget-exhausted-condition-exposes-restarts
  (let ((restart-names nil))
    (let ((amoebum:*budget-exhaustion-restart-selector* nil))
      (handler-bind
          ((amoebum:budget-exhausted-condition
             (lambda (condition)
               (setf restart-names
                     (mapcar #'restart-name (compute-restarts condition)))
               (invoke-restart 'amoebum::summarize-and-finish "fallback summary"))))
        (let ((result (amoebum:handle-budget-exhaustion
                       :kind :token
                       :used 11
                       :budget 10)))
          (is (eq :summarize-and-finish (getf result :action))))))
    (is-true (member 'amoebum::extend-budget restart-names :test #'eq))
    (is-true (member 'amoebum::summarize-and-finish restart-names :test #'eq))
    (is-true (member 'amoebum::abort-task restart-names :test #'eq))))

(test i364-stream-budget-enforcement-uses-summarize-fallback
  (let* ((state (amoebum:ensure-chat-ui-state
                 (amoebum:make-chat-ui-state
                  :status-bar-state (amoebum:make-status-bar-state))))
         (stream-state (amoebum:chat-ui-state-stream-state state)))
    (setf (amoebum:chat-ui-state-messages state)
          (list (amoebum:make-chat-message "user" "Summarize the architecture changes.")
                (amoebum:make-chat-message "assistant" "" :partial t)))
    (setf (amoebum:chat-ui-state-context-window-limit state) 10
          (amoebum:chat-ui-state-context-used-tokens state) 9)
    (setf (amoebum::token-stream-state-status stream-state) :running
          (amoebum::token-stream-state-token-count stream-state) 9
          (amoebum::token-stream-state-target-message-index stream-state) 1
          (amoebum::token-stream-state-aborted-p stream-state) nil)
    (let ((amoebum:*budget-exhaustion-restart-selector*
            #'amoebum:default-budget-exhaustion-restart-selector))
      (is-true (amoebum::%enforce-stream-token-budget-if-needed state)))
    (let* ((summary (amoebum:token-stream-progress-summary stream-state))
           (assistant (nth 1 (amoebum:chat-ui-state-messages state)))
           (content (and (typep assistant 'pseudopod:message)
                         (amoebum::%message-content->text assistant))))
      (is-true (getf summary :aborted-p))
      (is (eql :budget-exhausted (getf summary :abort-reason)))
      (is-true (search "budget exhausted"
                       (string-downcase (or content ""))
                       :test #'char=)))))
