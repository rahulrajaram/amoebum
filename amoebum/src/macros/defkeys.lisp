(in-package :amoebum)

(define-condition malformed-key-binding (error)
  ((key-spec :initarg :key-spec
             :reader malformed-key-binding-key-spec)
   (reason :initarg :reason
           :reader malformed-key-binding-reason))
  (:report (lambda (condition stream)
             (format stream
                     "Malformed key binding ~S: ~A"
                     (malformed-key-binding-key-spec condition)
                     (malformed-key-binding-reason condition)))))

(define-condition keymap-definition-warning (style-warning)
  ((keymap :initarg :keymap
           :reader keymap-definition-warning-keymap)
   (key-spec :initarg :key-spec
             :reader keymap-definition-warning-key-spec)
   (reason :initarg :reason
           :reader keymap-definition-warning-reason))
  (:report (lambda (condition stream)
             (format stream
                     "Keymap ~S binding ~S: ~A"
                     (keymap-definition-warning-keymap condition)
                     (keymap-definition-warning-key-spec condition)
                     (keymap-definition-warning-reason condition)))))

(defstruct (key-binding
            (:constructor %make-key-binding
                (&key signature handler guard description source-file source-line)))
  signature
  handler
  guard
  description
  source-file
  source-line)

(defstruct (key-chord
            (:constructor %make-key-chord
                (&key sequence handler guard description source-file source-line)))
  sequence
  handler
  guard
  description
  source-file
  source-line)

(defstruct (keymap
            (:constructor %make-keymap (&key name description)))
  name
  description
  (bindings (make-hash-table :test #'equal))
  (chords '()))

(defparameter *keymap-registry* (make-hash-table :test #'equal))
(defparameter *keymap-stack* '())
(defparameter *key-sequence-buffer* '())

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %string-empty-p (value)
    (or (null value)
        (zerop (length value))))

  (defun %split-on-hyphen (value)
    (let ((parts '())
          (start 0)
          (length (length value)))
      (loop for index from 0 below length do
        (when (char= (aref value index) #\-)
          (push (subseq value start index) parts)
          (setf start (1+ index))))
      (push (subseq value start length) parts)
      (nreverse parts)))

  (defun %normalize-character-key (char)
    (if (alpha-char-p char)
        (char-downcase char)
        char))

  (defun %parse-function-key-token (token)
    (when (and (>= (length token) 2)
               (char-equal (aref token 0) #\F))
      (let ((index (ignore-errors (parse-integer token :start 1))))
        (when (and index (<= 1 index 12))
          (intern (format nil "F~D" index) :keyword)))))

  (defun %parse-key-token (token key-spec)
    (let ((normalized (string-upcase token)))
      (cond
        ((%string-empty-p normalized)
         (error 'malformed-key-binding
                :key-spec key-spec
                :reason "Key token is empty."))
        ((or (string= normalized "RET")
             (string= normalized "RETURN")
             (string= normalized "ENTER"))
         :enter)
        ((or (string= normalized "DEL")
             (string= normalized "BS")
             (string= normalized "BACKSPACE"))
         :backspace)
        ((string= normalized "TAB")
         :tab)
        ((string= normalized "ESC")
         :escape)
        ((or (string= normalized "SPC")
             (string= normalized "SPACE"))
         #\Space)
        ((string= normalized "UP")
         :up)
        ((string= normalized "DOWN")
         :down)
        ((string= normalized "LEFT")
         :left)
        ((string= normalized "RIGHT")
         :right)
        ((string= normalized "HOME")
         :home)
        ((string= normalized "END")
         :end)
        ((or (string= normalized "PGUP")
             (string= normalized "PAGEUP"))
         :pgup)
        ((or (string= normalized "PGDN")
             (string= normalized "PAGEDOWN"))
         :pgdn)
        ((%parse-function-key-token normalized))
        ((= (length token) 1)
         (%normalize-character-key (char token 0)))
        (t
         (error 'malformed-key-binding
                :key-spec key-spec
                :reason (format nil "Unsupported key token ~S." token))))))

  (defun %parse-key-spec (key-spec)
    (unless (and (stringp key-spec)
                 (not (%string-empty-p key-spec)))
      (error 'malformed-key-binding
             :key-spec key-spec
             :reason "Key specification must be a non-empty string."))
    (let* ((tokens (%split-on-hyphen key-spec))
           (key-token (car (last tokens)))
           (modifier-tokens (butlast tokens))
           (ctrl nil)
           (alt nil)
           (shift nil))
      (when (%string-empty-p key-token)
        (error 'malformed-key-binding
               :key-spec key-spec
               :reason "Missing terminal key token."))
      (dolist (token modifier-tokens)
        (let ((modifier (string-upcase token)))
          (cond
            ((or (string= modifier "C") (string= modifier "CTRL"))
             (setf ctrl t))
            ((or (string= modifier "M")
                 (string= modifier "META")
                 (string= modifier "ALT")
                 (string= modifier "A"))
             (setf alt t))
            ((or (string= modifier "S") (string= modifier "SHIFT"))
             (setf shift t))
            (t
             (error 'malformed-key-binding
                    :key-spec key-spec
                    :reason (format nil "Unknown key modifier ~S." token))))))
      (let ((key (%parse-key-token key-token key-spec)))
        (list :key key :ctrl ctrl :alt alt :shift shift))))

  (defun %event-key->normalized-key (event-key event)
    (cond
      ((characterp event-key)
       (%normalize-character-key event-key))
      ((eq event-key :text)
       (let ((text (ptui.core.events:key-event-text? event)))
         (when (and (stringp text) (> (length text) 0))
           (%normalize-character-key (char text 0)))))
      ((eq event-key :ctrl-c) #\c)
      ((eq event-key :ctrl-j) #\j)
      ((or (eq event-key :enter) (eq event-key :return))
       :enter)
      ((eq event-key :escape)
       :escape)
      ((eq event-key :tab)
       :tab)
      ((eq event-key :backspace)
       :backspace)
      ((member event-key
               '(:up :down :left :right :home :end :pgup :pgdn
                 :f1 :f2 :f3 :f4 :f5 :f6 :f7 :f8 :f9 :f10 :f11 :f12)
               :test #'eq)
       event-key)
      (t
       event-key)))

  (defun %key-event-signature (event)
    (unless (typep event 'ptui.core.events:key-event)
      (return-from %key-event-signature nil))
    (let ((key (%event-key->normalized-key (ptui.core.events:key-event-key event)
                                           event)))
      (when key
        (list :key key
              :ctrl (not (null (ptui.core.events:key-event-ctrlp event)))
              :alt (not (null (ptui.core.events:key-event-altp event)))
              :shift (not (null (ptui.core.events:key-event-shiftp event)))))))

  (defun %normalize-keymap-name (designator)
    (cond
      ((typep designator 'keymap)
       (%normalize-keymap-name (keymap-name designator)))
      ((symbolp designator)
       (string-downcase (symbol-name designator)))
      ((stringp designator)
       (string-downcase designator))
      (t
       (error "Unsupported keymap designator ~S." designator))))

  (defun %sequence-suffix-equal-p (buffer sequence)
    (let ((buffer-length (length buffer))
          (sequence-length (length sequence)))
      (and (<= sequence-length buffer-length)
           (equal (subseq buffer (- buffer-length sequence-length))
                  sequence))))

  (defun %sequence-prefix-length (buffer sequence)
    (let* ((buffer-length (length buffer))
           (sequence-length (length sequence))
           (max-length (min buffer-length (1- sequence-length))))
      (loop for candidate from max-length downto 1
            thereis (and (equal (subseq buffer (- buffer-length candidate))
                                (subseq sequence 0 candidate))
                         candidate))))

  (defun %invoke-key-handler (handler state key-event)
    (let ((results (multiple-value-list (funcall handler state key-event))))
      (if results
          (first results)
          state)))

  (defun %keymap-from-designator (designator &key (errorp t))
    (etypecase designator
      (keymap designator)
      ((or string symbol)
       (let* ((normalized-name (%normalize-keymap-name designator))
              (entry (gethash normalized-name *keymap-registry*)))
         (if (or entry (not errorp))
             entry
             (error "Unknown keymap ~S." designator)))))))

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
          *key-sequence-buffer* '())
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

(defun push-keymap (designator)
  (let ((map (%keymap-from-designator designator :errorp t)))
    (setf *keymap-stack* (cons map (remove map *keymap-stack* :test #'eq))
          *key-sequence-buffer* '())
    map))

(defun pop-keymap ()
  (prog1 (first *keymap-stack*)
    (setf *keymap-stack* (rest *keymap-stack*)
          *key-sequence-buffer* '())))

(defun reset-keymap-stack (&optional designators)
  (setf *keymap-stack* '()
        *key-sequence-buffer* '())
  (dolist (designator designators)
    (push-keymap designator))
  *keymap-stack*)

(defun %active-keymaps (&optional keymap-stack)
  (let ((stack (or keymap-stack *keymap-stack*)))
    (if (null stack)
        (let ((chat (find-keymap 'chat-mode)))
          (if chat
              (list chat)
              '()))
        stack)))

(defun %max-chord-length (&optional keymaps)
  (let ((max-length 1))
    (dolist (keymap keymaps max-length)
      (dolist (chord (keymap-chords keymap))
        (setf max-length
              (max max-length (length (key-chord-sequence chord))))))))

(defun %trim-key-sequence-buffer (max-length)
  (let ((length (length *key-sequence-buffer*)))
    (when (> length max-length)
      (setf *key-sequence-buffer*
            (subseq *key-sequence-buffer* (- length max-length)))))
  *key-sequence-buffer*)

(defun %append-key-signature (signature)
  (setf *key-sequence-buffer*
        (append *key-sequence-buffer* (list signature)))
  *key-sequence-buffer*)

(defun %dispatch-chords (keymap key-event state)
  (let ((pending nil))
    (dolist (chord (keymap-chords keymap))
      (let* ((sequence (key-chord-sequence chord))
             (guard (key-chord-guard chord))
             (guard-passed (or (null guard)
                               (not (null (funcall guard state key-event))))))
        (when guard-passed
          (when (%sequence-suffix-equal-p *key-sequence-buffer* sequence)
            (setf *key-sequence-buffer* '())
            (return-from %dispatch-chords
              (values (%invoke-key-handler (key-chord-handler chord)
                                           state
                                           key-event)
                      t
                      :chord)))
          (when (%sequence-prefix-length *key-sequence-buffer* sequence)
            (setf pending t)))))
    (if pending
        (values state t :pending)
        (values state nil :none))))

(defun dispatch-key-event (keymap key-event state &key (append-sequence t))
  (let* ((map (%keymap-from-designator keymap))
         (signature (%key-event-signature key-event)))
    (unless signature
      (return-from dispatch-key-event (values state nil :none)))
    (when append-sequence
      (%append-key-signature signature)
      (%trim-key-sequence-buffer (%max-chord-length (list map))))
    (multiple-value-bind (post-chord-state chord-handledp chord-kind)
        (%dispatch-chords map key-event state)
      (when chord-handledp
        (return-from dispatch-key-event
          (values post-chord-state t chord-kind)))
      (let ((binding (gethash signature (keymap-bindings map))))
        (if (null binding)
            (progn
              (when append-sequence
                (setf *key-sequence-buffer* '()))
              (values state nil :none))
            (let ((guard (key-binding-guard binding)))
              (if (or (null guard)
                      (not (null (funcall guard state key-event))))
                  (progn
                    (setf *key-sequence-buffer* '())
                    (values (%invoke-key-handler (key-binding-handler binding)
                                                 state
                                                 key-event)
                            t
                            :binding))
                  (progn
                    (when append-sequence
                      (setf *key-sequence-buffer* '()))
                    (values state nil :guard-skipped)))))))))

(defun dispatch-active-keymaps (key-event state &key keymap-stack)
  (let* ((keymaps (%active-keymaps keymap-stack))
         (signature (%key-event-signature key-event)))
    (unless signature
      (return-from dispatch-active-keymaps (values state nil nil)))
    (%append-key-signature signature)
    (%trim-key-sequence-buffer (%max-chord-length keymaps))
    (let ((current-state state))
      (dolist (keymap keymaps (values state nil nil))
        (multiple-value-bind (next-state handledp kind)
            (dispatch-key-event keymap key-event current-state :append-sequence nil)
          (setf current-state next-state)
          (when handledp
            (return-from dispatch-active-keymaps
              (values current-state t
                      (list :keymap (keymap-name keymap)
                            :kind kind))))))
      (setf *key-sequence-buffer* '())
      (values state nil nil))))

(defun make-key-dispatch-on-event (&key
                                     (fallback (lambda (state event)
                                                 (declare (ignore event))
                                                 state))
                                     (keymap-stack-fn (lambda () *keymap-stack*)))
  (unless (functionp fallback)
    (error "Fallback must be a function, got ~S." fallback))
  (unless (functionp keymap-stack-fn)
    (error "KEYMAP-STACK-FN must be a function, got ~S." keymap-stack-fn))
  (lambda (state event)
    (if (typep event 'ptui.core.events:key-event)
        (multiple-value-bind (next-state handledp)
            (dispatch-active-keymaps event state :keymap-stack (funcall keymap-stack-fn))
          (if handledp
              next-state
              (funcall fallback state event)))
        (funcall fallback state event))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %parse-binding-options (options key-spec)
    (unless (evenp (length options))
      (error "Binding options for key ~S must be key/value pairs." key-spec))
    (let ((guard nil)
          (description nil))
      (loop for (option value) on options by #'cddr do
        (ecase option
          (:when
           (setf guard value))
          (:description
           (setf description (princ-to-string value)))))
      (values guard description)))

  (defun %parse-defkeys-binding (form)
    (unless (and (consp form)
                 (stringp (first form))
                 (>= (length form) 2))
      (error "Invalid DEfKEYS binding form ~S." form))
    (let* ((key-spec (first form))
           (signature (%parse-key-spec key-spec))
           (handler-form (second form))
           (options (cddr form)))
      (multiple-value-bind (guard description)
          (%parse-binding-options options key-spec)
        (list :key-spec key-spec
              :signature signature
              :handler-form handler-form
              :guard-form guard
              :description description))))

  (defun %warn-on-duplicate-bindings (name parsed-bindings)
    (let ((seen (make-hash-table :test #'equal)))
      (dolist (binding parsed-bindings)
        (let ((signature (getf binding :signature))
              (key-spec (getf binding :key-spec)))
          (if (gethash signature seen)
              (warn 'keymap-definition-warning
                    :keymap name
                    :key-spec key-spec
                    :reason "Duplicate key binding in the same keymap.")
              (setf (gethash signature seen) t)))))))

(defmacro defkeys (name &body forms)
  (unless (symbolp name)
    (error "DEFKEYS name must be a symbol, got ~S." name))
  (let* ((docstring (and forms (stringp (first forms)) (first forms)))
         (binding-forms (if docstring (rest forms) forms))
         (parsed-bindings (mapcar #'%parse-defkeys-binding binding-forms))
         (state-symbol (or (find-symbol "STATE" *package*)
                           (intern "STATE" *package*)))
         (key-event-symbol (or (find-symbol "KEY-EVENT" *package*)
                               (intern "KEY-EVENT" *package*)))
         (keymap-symbol
           (intern (format nil "*~A-KEYMAP*" (string-upcase (symbol-name name)))
                   (find-package :amoebum)))
         (source-file (or *compile-file-truename* *load-truename*)))
    (%warn-on-duplicate-bindings name parsed-bindings)
    `(progn
       (defparameter ,keymap-symbol
         (let ((map (make-keymap :name ',name
                                 :description ,(or docstring
                                                   (format nil "Keymap ~A." name)))))
           ,@(mapcar
              (lambda (binding)
                (let ((signature (getf binding :signature))
                      (handler-form (getf binding :handler-form))
                      (guard-form (getf binding :guard-form))
                      (description (getf binding :description))
                      (key-spec (getf binding :key-spec))
                      (state-var (gensym "STATE-"))
                      (event-var (gensym "KEY-EVENT-")))
                  `(register-key-binding
                    map
                    ',signature
                    (lambda (,state-var ,event-var)
                      (let ((,state-symbol ,state-var)
                            (,key-event-symbol ,event-var))
                        (declare (ignorable ,state-symbol ,key-event-symbol))
                        ,handler-form))
                    :guard ,(if guard-form
                                `(lambda (,state-var ,event-var)
                                   (let ((,state-symbol ,state-var)
                                         (,key-event-symbol ,event-var))
                                     (declare (ignorable ,key-event-symbol))
                                     ,guard-form))
                                nil)
                    :description ,(or description (format nil "~A" key-spec))
                    :source-file ,source-file)))
              parsed-bindings)
           (register-keymap map)
           map))
       ',name)))

(defkeys chat-mode
  "Chat mode default keymap."
  ("RET" state :description "Submit or newline handler placeholder.")
  ("C-c" state :description "Quit placeholder.")
  ("TAB" state :description "Completion placeholder."))

(defkeys command-mode
  "Command mode keymap."
  ("ESC" state :description "Exit command mode placeholder.")
  (":" state :description "Enter command prefix placeholder.")
  ("RET" state :description "Execute command placeholder."))

(defkeys diff-mode
  "Diff mode keymap."
  ("up" state :description "Move diff selection up.")
  ("down" state :description "Move diff selection down.")
  ("q" state :description "Exit diff view placeholder."))

(defun activate-default-keymaps ()
  (reset-keymap-stack)
  (push-keymap 'chat-mode)
  *keymap-stack*)

(eval-when (:load-toplevel :execute)
  (when (null *keymap-stack*)
    (activate-default-keymaps)))
