(in-package :amoebum/test)

(def-suite multimodal-chat-suite
  :in amoebum-suite
  :description "I341 multimodal image input coverage for headless + interactive chat.")

(in-suite multimodal-chat-suite)

(defun %i341-write-sample-png (path)
  (ensure-directories-exist path)
  (with-open-file (stream path
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create
                          :element-type '(unsigned-byte 8))
    (dolist (octet '(137 80 78 71 13 10 26 10 0 0 0 0))
      (write-byte octet stream))))

(test i341-headless-image-arguments-build-multimodal-content
  (let ((tmp-root (%make-temp-directory "amoebum-i341-headless")))
    (unwind-protect
         (let* ((image-path (merge-pathnames #P"fixtures/headless.png" tmp-root))
                (_ (%i341-write-sample-png image-path))
                (parts (amoebum::%build-user-message-content
                        "triage this screenshot"
                        (list (namestring image-path))))
                (text-part (first parts))
                (image-part (second parts)))
           (declare (ignore _))
           (is (= 2 (length parts)))
           (is (pseudopod:content-part-p text-part))
           (is (string= "text" (or (pseudopod:content-part-type text-part) "")))
           (is (string= "triage this screenshot"
                        (or (pseudopod:content-part-text text-part) "")))
           (is (hash-table-p image-part))
           (is (string= "image" (or (gethash "type" image-part) "")))
           (is (string= "image/png" (or (gethash "media_type" image-part) "")))
           (is-true (plusp (length (or (gethash "data" image-part) "")))))
      (%delete-directory-tree-safe tmp-root))))

(test i341-interactive-submit-parses-markdown-image-attachment
  (let ((tmp-root (%make-temp-directory "amoebum-i341-interactive")))
    (unwind-protect
         (let* ((image-path (merge-pathnames #P"fixtures/interactive.png" tmp-root))
                (_ (%i341-write-sample-png image-path))
                (chat-state (amoebum:make-chat-ui-state :stream-runner nil))
                (prompt (format nil "Please triage this ![screenshot](~A) now."
                                (namestring image-path))))
           (declare (ignore _))
           (amoebum:chat-ui-set-input chat-state prompt)
           (let* ((submitted (amoebum:chat-ui-submit-input chat-state))
                  (parts (and (pseudopod:message-p submitted)
                              (pseudopod:message-content submitted)))
                  (image-content (find "image"
                                       parts
                                       :test #'string=
                                       :key (lambda (part)
                                              (string-downcase
                                               (or (pseudopod:content-part-type part)
                                                   "")))))
                  (image-part (and image-content
                                   (pseudopod:content-part-to-hash image-content))))
             (is (pseudopod:message-p submitted))
             (is (string= "user" (or (pseudopod:message-role submitted) "")))
             (is-true image-content)
             (is (hash-table-p image-part))
             (is (string= "image/png" (or (gethash "media_type" image-part) "")))
             (is (string= (namestring image-path)
                          (or (gethash "path" image-part) "")))
             (is-true (search "[image interactive.png]"
                              (amoebum::%message-content->text submitted)
                              :test #'char=))
             (is (string= "" (amoebum:chat-ui-state-input-text chat-state)))))
      (%delete-directory-tree-safe tmp-root))))
