(in-package :amoebum)

;;;; ---------------------------------------------------------------------------
;;;; Keymap registry and stack mutators.
;;;;
;;;; This module owns the stateful side of the defkeys subsystem: keymap
;;;; registry operations (`make-keymap`, `register-keymap`, `list-keymaps`,
;;;; `find-keymap`, `clear-keymaps`, `register-key-binding`), chord
;;;; registration (`define-chord`), and the keymap/overlay stack + sequence
;;;; buffer + pending-escape state helpers used by the dispatch pipeline.
;;;;
;;;; The parameter globals mutated here are defined in `defkeys/parser.lisp`
;;;; (alongside the `keymap` / `key-binding` / `key-chord` / `keymap-overlay`
;;;; structs). The dispatch pipeline lives in `defkeys/dispatch.lisp`.
;;;;
;;;; Behavior is preserved verbatim from the original
;;;; `amoebum/src/macros/defkeys.lisp`; only file boundaries change.
;;;; ---------------------------------------------------------------------------

(defun make-keymap (&key name description)
  (%make-keymap :name name :description description))

(defun register-keymap (keymap)
  (unless (typep keymap 'keymap)
    (error "Expected KEYMAP instance, got ~S." keymap))
  (let ((normalized-name (%normalize-keymap-name (keymap-name keymap))))
    (setf (gethash normalized-name *keymap-registry*) keymap)
    keymap))

(defun list-keymaps ()
  (let ((entries '()))
    (maphash (lambda (name keymap)
               (declare (ignore name))
               (push keymap entries))
             *keymap-registry*)
    (sort entries #'string<
          :key (lambda (keymap)
                 (%normalize-keymap-name (keymap-name keymap))))))

(defun find-keymap (designator &key (errorp nil))
  (%keymap-from-designator designator :errorp errorp))

(defun clear-keymaps ()
  (let ((count (hash-table-count *keymap-registry*)))
    (clrhash *keymap-registry*)
    (setf *keymap-stack* '()
          *keymap-overlay-stack* '()
          *key-sequence-buffer* '()
          *key-sequence-last-input-ms* 0
          *pending-escape-event* nil
          *pending-escape-start-ms* 0)
    count))

(defun register-key-binding (keymap key-spec handler
                             &key guard description source-file source-line)
  (unless (functionp handler)
    (error "Handler must be a function, got ~S." handler))
  (when (and guard (not (functionp guard)))
    (error "Guard must be NIL or a function, got ~S." guard))
  (let* ((map (%keymap-from-designator keymap))
         (signature (if (stringp key-spec)
                        (%parse-key-spec key-spec)
                        key-spec))
         (existing (gethash signature (keymap-bindings map))))
    (when existing
      (warn 'keymap-definition-warning
            :keymap (keymap-name map)
            :key-spec key-spec
            :reason "Duplicate binding shadowing earlier entry."))
    (setf (gethash signature (keymap-bindings map))
          (%make-key-binding :signature signature
                             :handler handler
                             :guard guard
                             :description description
                             :source-file source-file
                             :source-line source-line))
    signature))

(defun keymap-bindings-list (keymap)
  (let ((entries '()))
    (maphash (lambda (signature binding)
               (declare (ignore signature))
               (push binding entries))
             (keymap-bindings (%keymap-from-designator keymap)))
    entries))

(defun %sort-chords (chords)
  (sort chords
        (lambda (left right)
          (let ((left-len (length (key-chord-sequence left)))
                (right-len (length (key-chord-sequence right))))
            (if (= left-len right-len)
                (string< (or (key-chord-description left) "")
                         (or (key-chord-description right) ""))
                (> left-len right-len))))))

(defun define-chord (keymap sequence handler
                     &key when description source-file source-line)
  (unless (functionp handler)
    (error "Chord handler must be a function, got ~S." handler))
  (when (and when (not (functionp when)))
    (error "Chord :when guard must be NIL or a function, got ~S." when))
  (unless (and (listp sequence) (not (null sequence)))
    (error "Chord sequence must be a non-empty list, got ~S." sequence))
  (let* ((map (%keymap-from-designator keymap))
         (parsed-sequence
           (mapcar (lambda (step)
                     (if (stringp step)
                         (%parse-key-spec step)
                         step))
                   sequence))
         (entry (%make-key-chord :sequence parsed-sequence
                                 :handler handler
                                 :guard when
                                 :description description
                                 :source-file source-file
                                 :source-line source-line)))
    (setf (keymap-chords map)
          (%sort-chords (cons entry (keymap-chords map))))
    entry))

(defun current-keymap ()
  (first *keymap-stack*))

(defun current-keymap-overlay ()
  (first *keymap-overlay-stack*))

(defun %reset-key-sequence-buffer ()
  (setf *key-sequence-buffer* '()
        *key-sequence-last-input-ms* 0)
  *key-sequence-buffer*)

(defun %clear-pending-escape ()
  (setf *pending-escape-event* nil
        *pending-escape-start-ms* 0)
  nil)

(defun %set-pending-escape (event &optional (now (%now-milliseconds)))
  (setf *pending-escape-event* event
        *pending-escape-start-ms* now)
  event)

(defun %pending-escape-active-p ()
  (typep *pending-escape-event* 'ptui.core.events:key-event))

(defun %pending-escape-expired-p (&optional (now (%now-milliseconds)))
  (and (%pending-escape-active-p)
       (>= (- now *pending-escape-start-ms*)
           (max 0 *key-disambiguation-timeout-ms*))))

(defun %sequence-buffer-expired-p (&optional (now (%now-milliseconds)))
  (and (not (null *key-sequence-buffer*))
       (> *key-sequence-timeout-ms* 0)
       (> (- now *key-sequence-last-input-ms*)
          *key-sequence-timeout-ms*)))

(defun %expire-key-sequence-buffer (&optional (now (%now-milliseconds)))
  (when (%sequence-buffer-expired-p now)
    (%reset-key-sequence-buffer))
  *key-sequence-buffer*)

(defun %plain-escape-signature-p (signature)
  (and signature
       (eq (getf signature :key) :escape)
       (not (getf signature :ctrl))
       (not (getf signature :alt))
       (not (getf signature :shift))))

(defun %emit-keymap-overlay-event (event-type overlay reason)
  (when overlay
    (publish (current-event-bus)
             event-type
             :source :amoebum
             :severity :info
             :payload (list :keymap (keymap-name (keymap-overlay-keymap overlay))
                            :reason reason
                            :depth (length *keymap-overlay-stack*)
                            :auto-cleanup-on-escape-p
                            (keymap-overlay-auto-cleanup-on-escape-p overlay)))))

(defun push-keymap (designator)
  (let ((map (%keymap-from-designator designator :errorp t)))
    (setf *keymap-stack* (cons map (remove map *keymap-stack* :test #'eq))
          *pending-escape-event* nil
          *pending-escape-start-ms* 0)
    (%reset-key-sequence-buffer)
    map))

(defun pop-keymap ()
  (prog1 (first *keymap-stack*)
    (setf *keymap-stack* (rest *keymap-stack*)
          *pending-escape-event* nil
          *pending-escape-start-ms* 0)
    (%reset-key-sequence-buffer)))

(defun push-keymap-overlay (designator &key (auto-cleanup-on-escape-p t))
  (let* ((map (%keymap-from-designator designator :errorp t))
         (remaining (remove map
                            *keymap-overlay-stack*
                            :key #'keymap-overlay-keymap
                            :test #'eq))
         (overlay (%make-keymap-overlay :keymap map
                                        :auto-cleanup-on-escape-p
                                        (not (null auto-cleanup-on-escape-p))
                                        :entered-at (%now-milliseconds))))
    (setf *keymap-overlay-stack* (cons overlay remaining))
    (%reset-key-sequence-buffer)
    (%clear-pending-escape)
    (%emit-keymap-overlay-event +event-type-keymap-overlay-enter+
                                overlay
                                :push)
    overlay))

(defun pop-keymap-overlay (&key (reason :pop))
  (let ((overlay (first *keymap-overlay-stack*)))
    (when overlay
      (setf *keymap-overlay-stack* (rest *keymap-overlay-stack*))
      (%reset-key-sequence-buffer)
      (%clear-pending-escape)
      (%emit-keymap-overlay-event +event-type-keymap-overlay-exit+
                                  overlay
                                  reason))
    overlay))

(defun clear-keymap-overlays (&key (reason :clear))
  (let ((count 0))
    (loop while *keymap-overlay-stack* do
      (pop-keymap-overlay :reason reason)
      (incf count))
    count))

(defun reset-keymap-stack (&optional designators)
  (when *keymap-overlay-stack*
    (clear-keymap-overlays :reason :reset))
  (setf *keymap-stack* '()
        *pending-escape-event* nil
        *pending-escape-start-ms* 0)
  (%reset-key-sequence-buffer)
  (dolist (designator designators)
    (push-keymap designator))
  *keymap-stack*)

(defun %trim-key-sequence-buffer (max-length)
  (let ((length (length *key-sequence-buffer*)))
    (when (> length max-length)
      (setf *key-sequence-buffer*
            (subseq *key-sequence-buffer* (- length max-length)))))
  *key-sequence-buffer*)

(defun %append-key-signature (signature)
  (%expire-key-sequence-buffer)
  (setf *key-sequence-buffer*
        (append *key-sequence-buffer* (list signature))
        *key-sequence-last-input-ms* (%now-milliseconds))
  *key-sequence-buffer*)
