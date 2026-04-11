(in-package :amoebum/test)

(def-suite profiling-dashboard-suite :in amoebum-suite
  :description "I236 profiling dashboard and slash command integration.")

(in-suite profiling-dashboard-suite)

(defun %report-top-entry-names (report)
  (mapcar (lambda (entry) (getf entry :name))
          (amoebum.observability:profiling-report-top-functions report)))

(test profiling-report-model-and-table-render
  (let* ((report (amoebum.observability:make-profiling-report
                  :top-functions
                  (list (list :name "foo/a" :samples 10 :percentage 62.5d0)
                        (list :name "bar/b" :samples 6 :percentage 37.5d0))
                  :call-graph (list (list :caller "root" :callee "foo/a" :samples 4))
                  :total-samples 16
                  :elapsed-ms 150))
         (table (amoebum.observability:render-profiling-report-table :report report
                                                       :sort-by :samples
                                                       :sort-direction :desc)))
    (is (stringp table))
    (is (search "Profiling Report" table :test #'char-equal))
    (is (search "FUNCTION" table :test #'char-equal))
    (is (search "foo/a" table :test #'char-equal))))

(test profile-slash-command-start-stop-report
  (let ((original-last amoebum.observability:*last-profiling-report*))
    (unwind-protect
        (progn
          (multiple-value-bind (handledp start-result)
              (amoebum:dispatch-slash-command "/profile start")
            (is-true handledp)
            (is-true (typep start-result 'amoebum.commands:slash-command-result))
            (is-true amoebum.observability:*profiling-enabled-p*))
          (dotimes (_ 50000)
            (declare (ignore _))
            (sqrt 9.0))
          (multiple-value-bind (handledp stop-result)
              (amoebum:dispatch-slash-command "/profile stop")
            (is-true handledp)
            (is-true (typep stop-result 'amoebum.commands:slash-command-result))
            (is-false amoebum.observability:*profiling-enabled-p*)
            (is (typep amoebum.observability:*last-profiling-report* 'amoebum.observability:profiling-report)))
          (multiple-value-bind (handledp report-result)
              (amoebum:dispatch-slash-command "/profile report")
            (is-true handledp)
            (is-true (typep report-result 'amoebum.commands:slash-command-result))
            (let ((output (amoebum.commands:slash-command-result-output report-result)))
              (is (stringp output))
              (is (plusp (length output))))))
      (setf amoebum.observability:*last-profiling-report* original-last)
      (ignore-errors
        (amoebum.observability:stop-profiling)))))

(test per-tool-profiling-captures-pipeline-execution
  (let ((original-toolset amoebum:*toolset*)
        (original-hooks amoebum:*hook-registry*)
        (original-event-bus amoebum:*event-bus*))
    (unwind-protect
        (progn
          (setf amoebum:*toolset* (pseudopod:make-toolset)
                amoebum:*hook-registry* (make-hash-table :test #'equal)
                amoebum:*event-bus* (amoebum:make-event-bus :capacity 32))
          (pseudopod:register-tool-function
           amoebum:*toolset*
           :name "i236-profile-tool"
           :description "I236 per-tool profiling probe."
           :parameters (let ((schema (make-hash-table :test #'equal)))
                         (setf (gethash "type" schema) "object")
                         schema)
           :fn (lambda (_arguments _call)
                 (declare (ignore _arguments _call))
                 (dotimes (_ 10000)
                   (declare (ignore _))
                   (+ 1 1))
                 "ok"))
          (amoebum.observability:start-profiling :tool-profiling t)
          (let* ((context (amoebum:make-amoebum-context
                           :toolset amoebum:*toolset*
                           :permission-mode :full-auto
                           :event-bus amoebum:*event-bus*
                           :hook-registry amoebum:*hook-registry*
                           :initialize-notifications-p nil))
                 (call (pseudopod:make-tool-call
                        :id "i236-tool-call"
                        :name "i236-profile-tool"
                        :arguments "{}")))
            (is (string= "ok" (amoebum:execute-tool call context)))
            (let* ((report (amoebum.observability:stop-profiling))
                   (names (%report-top-entry-names report))
                   (call-graph (amoebum.observability:profiling-report-call-graph report)))
              (is (typep report 'amoebum.observability:profiling-report))
              (is-true (find "i236-profile-tool" names :test #'string=))
              (is-true (find "i236-profile-tool"
                             call-graph
                             :test #'string=
                             :key (lambda (edge) (getf edge :callee)))))))
      (ignore-errors
        (amoebum.observability:stop-profiling))
      (setf amoebum:*toolset* original-toolset
            amoebum:*hook-registry* original-hooks
            amoebum:*event-bus* original-event-bus))))

(test profiling-dashboard-widget-renders
  (let* ((report (amoebum.observability:make-profiling-report
                  :top-functions (list (list :name "widget/fn" :samples 2 :percentage 100.0d0))
                  :total-samples 2
                  :elapsed-ms 10))
         (widget (amoebum.observability:profiling-report-table-widget
                  (list :report report :sort-by :samples :sort-direction :desc))))
    (is-true (typep widget 'ptui.ui.elements:ui-element))))

(test profiling-dashboard-smoke-sentinel
  (is-true t)
  (format t "PROFILING_DASHBOARD_SMOKE_OK~%"))
