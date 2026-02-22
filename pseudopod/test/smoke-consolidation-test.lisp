(in-package :pseudopod/test)

(def-suite smoke-consolidation-suite :in pseudopod-suite
  :description "Smoke assertions migrated into FiveAM suites (I141).")

(in-suite smoke-consolidation-suite)

(test chat-completion-extract-content-roundtrip
  "chat-completion responses should round-trip through extract-content."
  (with-stub-dex
      (:post (lambda (url &rest args &key content want-stream &allow-other-keys)
               (declare (ignore url args content want-stream))
               (let ((response (make-hash-table :test #'equal))
                     (choice (make-hash-table :test #'equal))
                     (message (make-hash-table :test #'equal)))
                 (setf (gethash "role" message) "assistant")
                 (setf (gethash "content" message) "MOONSHOT_OK")
                 (setf (gethash "message" choice) message)
                 (setf (gethash "choices" response) (vector choice))
                 (values (jonathan:to-json response) 200))))
    (let* ((client (pseudopod:make-client :api-key "stub"))
           (response (pseudopod:chat-completion
                      client
                      "Reply with EXACTLY: MOONSHOT_OK"
                      :system-prompt "You are a connectivity test assistant."))
           (choices (and (hash-table-p response) (gethash "choices" response)))
           (choice (first-item choices))
           (message (and (hash-table-p choice) (gethash "message" choice)))
           (content (and (hash-table-p message) (gethash "content" message)))
           (extracted-content (pseudopod:extract-content response)))
      (is-true (stringp content))
      (is-true (stringp extracted-content))
      (is (string= content extracted-content)))))
