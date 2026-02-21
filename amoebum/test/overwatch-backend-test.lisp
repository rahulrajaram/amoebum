(in-package :amoebum/test)

;;; ============================================================
;;; I259: Overwatch Worker Backend — Smoke Tests
;;; ============================================================

(def-suite overwatch-backend-suite :in amoebum-suite)
(in-suite overwatch-backend-suite)

;;; --- Protocol completeness ---

(test overwatch-supervisor-class-exists
  "overwatch-supervisor class is defined."
  (is (find-class 'amoebum:overwatch-supervisor nil)))

(test overwatch-available-p-function-exists
  "overwatch-available-p is a bound function."
  (is (fboundp 'amoebum:overwatch-available-p)))

(test select-worker-backend-function-exists
  "select-worker-backend is a bound function."
  (is (fboundp 'amoebum:select-worker-backend)))

;;; --- Configuration specials ---

(test overwatch-config-specials-exist
  "Overwatch configuration specials are bound."
  (is (boundp 'amoebum:*overwatch-host*))
  (is (boundp 'amoebum:*overwatch-port*))
  (is (boundp 'amoebum:*overwatch-poll-interval-seconds*))
  (is (boundp 'amoebum:*overwatch-connect-timeout-seconds*))
  (is (boundp 'amoebum:*overwatch-http-request-function*)))

;;; --- Backend selection ---

(test select-worker-backend-in-process
  "select-worker-backend :in-process returns in-process-supervisor."
  (let ((sup (amoebum:select-worker-backend :backend :in-process)))
    (is (typep sup 'amoebum:in-process-supervisor))))

(test select-worker-backend-overwatch
  "select-worker-backend :overwatch returns overwatch-supervisor."
  (let ((sup (amoebum:select-worker-backend :backend :overwatch)))
    (is (typep sup 'amoebum:overwatch-supervisor))))

;;; --- Mock HTTP transport ---

(test overwatch-available-p-with-mock-404
  "overwatch-available-p returns T when mock returns 404 (server alive)."
  (let ((old amoebum:*overwatch-http-request-function*))
    (unwind-protect
         (progn
           (setf amoebum:*overwatch-http-request-function*
                 (lambda (method url &key content)
                   (declare (ignore method url content))
                   (values "{\"error\": \"not found\"}" 404)))
           (is (amoebum:overwatch-available-p)))
      (setf amoebum:*overwatch-http-request-function* old))))

(test overwatch-available-p-with-mock-unreachable
  "overwatch-available-p returns NIL when mock returns status 0 (unreachable)."
  (let ((old amoebum:*overwatch-http-request-function*))
    (unwind-protect
         (progn
           (setf amoebum:*overwatch-http-request-function*
                 (lambda (method url &key content)
                   (declare (ignore method url content))
                   (values "" 0)))
           (is (not (amoebum:overwatch-available-p))))
      (setf amoebum:*overwatch-http-request-function* old))))

;;; --- Overwatch supervisor with mock (submit success) ---

(test overwatch-supervisor-spawn-with-mock
  "overwatch-supervisor spawn delegates to HTTP and creates worker record."
  (let ((old-http amoebum:*overwatch-http-request-function*)
        (old-sup amoebum:*worker-supervisor*)
        (submitted-url nil))
    (unwind-protect
         (progn
           (setf amoebum:*overwatch-http-request-function*
                 (lambda (method url &key content)
                   (declare (ignore content))
                   (cond
                     ;; POST /run
                     ((and (eq method :post) (search "/run" url))
                      (setf submitted-url url)
                      (values "{\"task_id\": \"ow-task-001\", \"status\": \"PENDING\"}" 200))
                     ;; GET /status/ow-task-001
                     ((and (eq method :get) (search "/status/" url))
                      (values "{\"status\": \"COMPLETED\", \"reason\": \"exit_zero\"}" 200))
                     ;; GET /output/ow-task-001
                     ((and (eq method :get) (search "/output/" url))
                      (values "{\"stdout\": \"hello from overwatch\", \"stderr\": \"\", \"exit_code\": 0}" 200))
                     (t (values "" 404)))))
           (setf amoebum:*worker-supervisor* nil)
           (amoebum:clear-workers)
           (let* ((sup (make-instance 'amoebum:overwatch-supervisor))
                  (worker (amoebum:supervisor-spawn sup :shell "echo hello"
                                                   :label "ow test"
                                                   :timeout-seconds 10
                                                   :cwd "/tmp")))
             (is (amoebum:worker-record-p worker))
             (is (stringp (amoebum:worker-record-id worker)))
             (is (not (null submitted-url)))
             (is (search "/run" submitted-url))
             ;; Wait for poller thread to complete
             (amoebum:await-worker (amoebum:worker-record-id worker)
                                   :timeout-seconds 5)
             (let ((status (amoebum:worker-record-status worker)))
               (is (eq :completed status)))))
      (setf amoebum:*overwatch-http-request-function* old-http
            amoebum:*worker-supervisor* old-sup))))

;;; --- Overwatch supervisor fallback on submission failure ---

(test overwatch-supervisor-falls-back-on-failure
  "overwatch-supervisor falls back to in-process when submit fails."
  (let ((old-http amoebum:*overwatch-http-request-function*)
        (old-sup amoebum:*worker-supervisor*))
    (unwind-protect
         (progn
           (setf amoebum:*overwatch-http-request-function*
                 (lambda (method url &key content)
                   (declare (ignore method url content))
                   (values "" 0)))  ; Simulate unreachable
           (setf amoebum:*worker-supervisor* nil)
           (amoebum:clear-workers)
           (let* ((sup (make-instance 'amoebum:overwatch-supervisor))
                  (worker (amoebum:supervisor-spawn sup :shell "echo fallback"
                                                   :label "fallback test"
                                                   :timeout-seconds 10
                                                   :cwd "/tmp")))
             ;; Should fall back to in-process backend
             (is (amoebum:worker-record-p worker))
             ;; Backend should be :in-process after fallback
             (amoebum:await-worker (amoebum:worker-record-id worker)
                                   :timeout-seconds 15)
             (is (eq :in-process (amoebum:worker-record-backend worker)))))
      (setf amoebum:*overwatch-http-request-function* old-http
            amoebum:*worker-supervisor* old-sup))))
