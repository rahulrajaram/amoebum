(load "/home/rahul/Documents/amoebum/ptui/.tools/quicklisp/setup.lisp")
(require :asdf)

(let* ((asdf-pkg (or (find-package "ASDF")
                     (error "Missing package ASDF")))
       (load-asd-sym (or (find-symbol "LOAD-ASD" asdf-pkg)
                         (error "Missing symbol LOAD-ASD in ASDF package")))
       (load-system-sym (or (find-symbol "LOAD-SYSTEM" asdf-pkg)
                            (error "Missing symbol LOAD-SYSTEM in ASDF package")))
       (load-asd-fn (symbol-function load-asd-sym))
       (load-system-fn (symbol-function load-system-sym)))
  (funcall load-asd-fn #P"/home/rahul/Documents/amoebum/pseudopod/pseudopod.asd")
  (funcall load-system-fn "pseudopod"))

(defun first-item (sequence)
  (cond
    ((null sequence) nil)
    ((listp sequence) (first sequence))
    ((vectorp sequence)
     (when (> (length sequence) 0)
       (aref sequence 0)))
    (t nil)))

(handler-case
    (let* ((client (pseudopod:make-client))
           (response (pseudopod:chat-completion
                      client
                      "Reply with EXACTLY: MOONSHOT_OK"
                      :system-prompt "You are a connectivity test assistant."))
           (choices (and (hash-table-p response) (gethash "choices" response)))
           (choice (first-item choices))
           (message (and (hash-table-p choice) (gethash "message" choice)))
           (content (and (hash-table-p message) (gethash "content" message))))
      (unless (stringp content)
        (error "Missing assistant content in response: ~S" response))
      (format t "PSEUDOPOD_SMOKE_OK~%")
      (format t "assistant=~A~%" content))
  (error (e)
    (format *error-output* "PSEUDOPOD_SMOKE_ERROR: ~A~%" e)
    (sb-ext:exit :code 1)))
