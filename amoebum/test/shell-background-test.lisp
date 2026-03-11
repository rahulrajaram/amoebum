(in-package :amoebum/test)

;;; ---------------------------------------------------------------------------
;;; Background shell execution tests (I345)
;;; ---------------------------------------------------------------------------

(def-suite shell-background-suite :in amoebum-suite
  :description "Background shell execution lifecycle, retrieval, timeout, and cleanup.")

(in-suite shell-background-suite)

(defun %i345-bash-exec-function ()
  (let ((tool (pseudopod:find-tool amoebum:*toolset* "bash-exec")))
    (unless tool
      (error "Tool bash-exec is not registered."))
    (pseudopod:tool-definition-fn tool)))

(defun %i345-make-args (&rest key-values)
  (let ((args (make-hash-table :test #'equal)))
    (loop for (key value) on key-values by #'cddr do
          (setf (gethash key args) value))
    args))

(defun %i345-invoke-bash-exec (&rest key-values)
  (funcall (%i345-bash-exec-function)
           (apply #'%i345-make-args key-values)))

(defun %i345-wait-for-task (task-id &key (retries 120) (sleep-seconds 0.05))
  (loop repeat retries
        for snapshot = (%i345-invoke-bash-exec "task-id" task-id)
        for status = (getf snapshot :status)
        when (member status '(:completed :timeout :failed :cancelled) :test #'eq)
          do (return snapshot)
        do (sleep sleep-seconds)
        finally (error "Timed out waiting for shell background task ~A." task-id)))

(defun %i345-reset-shell-background-state ()
  (%i345-invoke-bash-exec "cleanup-completed" t "include-running" t))

(defmacro %with-i345-shell-state (&body body)
  `(let ((old-mode (amoebum:config-permission-mode (amoebum:current-config)))
         (old-shell-working-directory amoebum::*shell-working-directory*))
     (unwind-protect
          (progn
            (amoebum:setconfig :permission-mode :full-auto)
            (setf amoebum::*shell-working-directory* (%amoebum-system-root))
            (%i345-reset-shell-background-state)
            ,@body)
       (%i345-reset-shell-background-state)
       (setf amoebum::*shell-working-directory* old-shell-working-directory)
       (amoebum:setconfig :permission-mode old-mode))))

(test i345-background-shell-output-and-exit-status
  "Background bash-exec jobs can be polled and expose deterministic output/exit status."
  (%with-i345-shell-state
    (let* ((launch (%i345-invoke-bash-exec
                    "command" "echo i345-out; echo i345-err 1>&2; exit 7"
                    "background" t
                    "timeout-seconds" 10))
           (task-id (getf launch :task-id)))
      (is (stringp task-id))
      (is (member (getf launch :status) '(:running :completed) :test #'eq))
      (let* ((snapshot (%i345-wait-for-task task-id))
             (result (getf snapshot :result)))
        (is (eq :completed (getf snapshot :status)))
        (is (= 7 (or (getf snapshot :exit-code) -1)))
        (is (stringp (getf snapshot :stdout)))
        (is (search "i345-out" (or (getf snapshot :stdout) "") :test #'char-equal))
        (is (search "i345-err" (or (getf snapshot :stderr) "") :test #'char-equal))
        (is (= 7 (or (getf result :exit-code) -1)))
        (is (search "i345-out" (or (getf result :stdout) "") :test #'char-equal))
        (is (search "i345-err" (or (getf result :stderr) "") :test #'char-equal))))))

(test i345-background-shell-timeout
  "Background bash-exec timeout transitions to :timeout with timed-out metadata."
  (%with-i345-shell-state
    (let* ((launch (%i345-invoke-bash-exec
                    "command" "sleep 2"
                    "background" t
                    "timeout-seconds" 1))
           (task-id (getf launch :task-id))
           (snapshot (%i345-wait-for-task task-id))
           (result (getf snapshot :result)))
      (is (stringp task-id))
      (is (eq :timeout (getf snapshot :status)))
      (is-true (getf snapshot :timed-out))
      (is (null (getf snapshot :exit-code)))
      (is-true (getf result :timed-out))
      (is (null (getf result :exit-code))))))

(test i345-background-shell-list-and-cleanup
  "Background shell APIs can list tracked tasks and cleanup terminal tasks."
  (%with-i345-shell-state
    (let* ((launch (%i345-invoke-bash-exec
                    "command" "echo cleanup-target"
                    "background" t
                    "timeout-seconds" 5))
           (task-id (getf launch :task-id)))
      (is (stringp task-id))
      (%i345-wait-for-task task-id)
      (let* ((listed (%i345-invoke-bash-exec
                      "list-tasks" t
                      "include-finished" t))
             (tasks (or (getf listed :tasks) '())))
        (is (>= (getf listed :count) 1))
        (is (find task-id tasks :key (lambda (task) (getf task :task-id))
                  :test #'string=)))
      (let* ((cleanup (%i345-invoke-bash-exec "cleanup-completed" t))
             (removed-ids (or (getf cleanup :removed-task-ids) '())))
        (is (>= (getf cleanup :removed-count) 1))
        (is (find task-id removed-ids :test #'string=)))
      (let ((listed-active (%i345-invoke-bash-exec
                            "list-tasks" t
                            "include-finished" nil)))
        (is (= 0 (getf listed-active :count))))
      (let ((missing-error-signaled nil))
        (handler-case
            (%i345-invoke-bash-exec "task-id" task-id)
          (error ()
            (setf missing-error-signaled t)))
        (is-true missing-error-signaled)))))
