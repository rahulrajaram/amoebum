(in-package :ptui.test.run)

(deftest event-bus-concurrent-publish-monotonic
  (let* ((bus (ptui.runtime.event-bus:make-event-bus))
         (publishers 4)
         (events-per-publisher 75)
         (expected-total (* publishers events-per-publisher))
         (captured-sequences '())
         (capture-lock (bordeaux-threads:make-lock "ptui-event-bus-test-capture"))
         (subscription-id
           (ptui.runtime.event-bus:subscribe
            bus
            (lambda (event)
              (bordeaux-threads:with-lock-held (capture-lock)
                (push (ptui.runtime.event-bus:event-envelope-sequence event)
                      captured-sequences)))))
         (threads
           (loop for publisher-id from 0 below publishers
                 collect
                 (bordeaux-threads:make-thread
                  (lambda ()
                    (loop for offset from 0 below events-per-publisher do
                      (ptui.runtime.event-bus:publish
                       bus
                       (list :publisher publisher-id :offset offset))))
                  :name (format nil "ptui-event-bus-publisher-~D" publisher-id)))))
    (dolist (thread threads)
      (bordeaux-threads:join-thread thread))
    (assert-true (ptui.runtime.event-bus:unsubscribe bus subscription-id)
                 "expected unsubscribe to succeed for subscription id ~S"
                 subscription-id)
    (let* ((sequence-copy (copy-list captured-sequences))
           (sorted-sequences (sort sequence-copy #'<))
           (max-sequence (if sorted-sequences (car (last sorted-sequences)) 0)))
      (assert-true (= (length captured-sequences) expected-total)
                   "expected ~D captured events, got ~D"
                   expected-total
                   (length captured-sequences))
      (assert-true (= (length (remove-duplicates captured-sequences)) expected-total)
                   "expected all sequence numbers to be unique")
      (assert-true (= max-sequence expected-total)
                   "expected monotonic sequence to reach ~D, got ~D"
                   expected-total
                   max-sequence)
      (assert-true (equal sorted-sequences
                          (loop for sequence from 1 to expected-total collect sequence))
                   "expected contiguous monotonic sequences 1..~D, got ~S"
                   expected-total
                   sorted-sequences))))
