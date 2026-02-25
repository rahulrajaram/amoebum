(in-package :amoebum/test)

(def-suite pipeline-context-suite :in amoebum-suite
  :description "amoebum context construction and inheritance coverage (I206).")

(in-suite pipeline-context-suite)

(test amoebum-context-inherits-from-pseudopod-context
  (is-true (find-class 'amoebum:amoebum-context nil))
  (is-true (find-class 'amoebum:tool-execution-context nil))
  (is-true (find-class 'pseudopod::tool-execution-context nil))
  (is (subtypep 'amoebum:tool-execution-context
                'pseudopod::tool-execution-context))
  (is (subtypep 'amoebum:amoebum-context
                'pseudopod::tool-execution-context)))

(test make-amoebum-context-populates-required-slots
  (let* ((metrics (make-hash-table :test #'equal))
         (hook-registry (list :hook-registry))
         (context (amoebum:make-amoebum-context
                   :permission-mode :supervised
                   :metrics metrics
                   :hook-registry hook-registry)))
    (is-true (typep context 'amoebum:amoebum-context))
    (is (eq (amoebum:context-permission-mode context) :supervised))
    (is (eq (amoebum:context-metrics context) metrics))
    (is (equal (amoebum:context-hook-registry context) hook-registry))))
