(defpackage :ptui.test.event-filters
  (:use :cl :fiveam)
  (:export #:run-all #:event-filters-suite))

(in-package :ptui.test.event-filters)

(def-suite event-filters-suite
  :description "Composable PTUI event filter protocol coverage (I213).")

(in-suite event-filters-suite)

(defun %event (&key type severity source payload)
  (list :type type :severity severity :source source :payload payload))

(test filter-by-type-severity-and-source
  (let* ((filter
           (ptui.runtime.event-filters:filter-and
            (ptui.runtime.event-filters:filter-by-type :tool-invoked)
            (ptui.runtime.event-filters:filter-by-severity :info)
            (ptui.runtime.event-filters:filter-by-source :amoebum)))
         (match (%event :type :tool-invoked :severity :info :source :amoebum))
         (wrong-source (%event :type :tool-invoked :severity :info :source :external)))
    (is-true (ptui.runtime.event-filters:filter-matches-p filter match))
    (is-false (ptui.runtime.event-filters:filter-matches-p filter wrong-source))))

(test filter-or-and-not-compose-correctly
  (let* ((filter
           (ptui.runtime.event-filters:filter-and
            (ptui.runtime.event-filters:filter-or
             (ptui.runtime.event-filters:filter-by-type :tool-invoked)
             (ptui.runtime.event-filters:filter-by-type :tool-error))
            (ptui.runtime.event-filters:filter-not
             (ptui.runtime.event-filters:filter-by-severity :debug))))
         (tool-invoked (%event :type :tool-invoked :severity :info :source :amoebum))
         (tool-error-debug (%event :type :tool-error :severity :debug :source :amoebum))
         (tool-completed (%event :type :tool-completed :severity :info :source :amoebum)))
    (is-true (ptui.runtime.event-filters:filter-matches-p filter tool-invoked))
    (is-false (ptui.runtime.event-filters:filter-matches-p filter tool-error-debug))
    (is-false (ptui.runtime.event-filters:filter-matches-p filter tool-completed))))

(test compiled-filter-function-is-callable
  (let* ((filter
           (ptui.runtime.event-filters:filter-or
            (ptui.runtime.event-filters:filter-by-source :amoebum)
            (ptui.runtime.event-filters:filter-by-severity :error)))
         (fn (ptui.runtime.event-filters:coerce-filter-function filter)))
    (is (functionp fn))
    (is-true (funcall fn (%event :type :x :severity :warning :source :amoebum)))
    (is-true (funcall fn (%event :type :x :severity :error :source :external)))
    (is-false (funcall fn (%event :type :x :severity :info :source :external)))))

(defun run-all ()
  (run! 'event-filters-suite))
