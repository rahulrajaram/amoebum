(in-package :amoebum)

;;; Stream-event journal ownership extracted from ui/streaming.lisp as the
;;; NXT-383 fallback seam so chat-state/chat-stream can depend on a dedicated
;;; replayable journal module without pulling renderer logic through the same
;;; file.

(defun %stream-event-journal-now ()
  (get-universal-time))

(defstruct (stream-event-journal
            (:constructor make-stream-event-journal
                (&key (capacity 4096)
                      (started-at (%stream-event-journal-now)))))
  (entries (make-array 0 :adjustable t :fill-pointer 0) :type vector)
  (capacity 4096 :type fixnum)
  (session-id nil)
  (started-at nil))

(defun %stream-event-journal-entry (event)
  (cond
    ((policy-trace-entry-p event)
     (list :kind :policy-trace
           :phase (policy-trace-entry-phase event)
           :source (policy-trace-entry-source event)
           :decision (policy-trace-entry-decision event)
           :reason-code (policy-trace-entry-reason-code event)
           :reason (policy-trace-entry-reason event)
           :data (policy-trace-entry-data event)
           :source-timestamp (policy-trace-entry-timestamp event)
           :timestamp (%stream-event-journal-now)))
    (t
     (list :kind :stream-event
           :event-type (or (getf event :type) (getf event :kind))
           :event event
           :timestamp (%stream-event-journal-now)))))

(defun stream-event-journal-append! (journal event)
  "Append a stream EVENT or policy trace entry to JOURNAL.
Drops oldest entries when capacity exceeded."
  (check-type journal stream-event-journal)
  (let ((entries (stream-event-journal-entries journal))
        (cap (stream-event-journal-capacity journal))
        (entry (%stream-event-journal-entry event)))
    (when (>= (length entries) cap)
      (let* ((keep-start (floor cap 4))
             (new-entries (make-array (- (length entries) keep-start)
                                      :adjustable t
                                      :fill-pointer (- (length entries) keep-start))))
        (loop for i from keep-start below (length entries)
              for j from 0
              do (setf (aref new-entries j) (aref entries i)))
        (setf (stream-event-journal-entries journal) new-entries
              entries new-entries)))
    (vector-push-extend entry entries))
  journal)

(defun stream-event-journal-append-policy-trace! (journal structured-trace)
  "Append each policy trace entry in STRUCTURED-TRACE to JOURNAL."
  (check-type journal stream-event-journal)
  (dolist (entry structured-trace journal)
    (when (policy-trace-entry-p entry)
      (stream-event-journal-append! journal entry))))

(defun stream-event-journal-count (journal)
  "Return the number of entries in JOURNAL."
  (check-type journal stream-event-journal)
  (length (stream-event-journal-entries journal)))

(defun stream-event-journal-clear! (journal)
  "Clear all entries from JOURNAL."
  (check-type journal stream-event-journal)
  (setf (fill-pointer (stream-event-journal-entries journal)) 0)
  journal)

(defun stream-event-journal-entries-list (journal)
  "Return entries as a list (most recent last)."
  (check-type journal stream-event-journal)
  (coerce (stream-event-journal-entries journal) 'list))

(defun %stream-event-journal-entry-event (entry)
  (cond
    ((and (listp entry)
          (getf entry :event))
     (getf entry :event))
    ((listp entry) entry)
    (t nil)))

(defun %stream-turn-snapshot-from-events (events)
  (let ((snapshot (pseudopod:make-stream-turn-snapshot)))
    (dolist (event events snapshot)
      (when (listp event)
        (pseudopod:stream-turn-apply-event! snapshot event)))))

(defun stream-event-journal-replay-snapshot (journal)
  "Replay JOURNAL entries into a fresh stream-turn snapshot."
  (check-type journal stream-event-journal)
  (%stream-turn-snapshot-from-events
   (loop for entry in (stream-event-journal-entries-list journal)
         for event = (%stream-event-journal-entry-event entry)
         when (listp event)
           collect event)))
