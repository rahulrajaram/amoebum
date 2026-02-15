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

(defstruct (keymap-overlay
            (:constructor %make-keymap-overlay
                (&key keymap (auto-cleanup-on-escape-p t) entered-at)))
  keymap
  (auto-cleanup-on-escape-p t :type boolean)
  (entered-at 0 :type integer))

(defparameter *keymap-registry* (make-hash-table :test #'equal))
(defparameter *keymap-stack* '())
(defparameter *keymap-overlay-stack* '())
(defparameter *key-sequence-buffer* '())
(defparameter *key-sequence-timeout-ms* 500)
(defparameter *key-sequence-last-input-ms* 0)
(defparameter *key-disambiguation-timeout-ms* 50)
(defparameter *pending-escape-event* nil)
(defparameter *pending-escape-start-ms* 0)
(defparameter *terminal-key-normalization-profile* :auto)
(defparameter *terminal-key-normalization-table*
  '((:xterm . (("xterm-up" . "up")
               ("xterm-down" . "down")
               ("xterm-left" . "left")
               ("xterm-right" . "right")
               ("xterm-home" . "home")
               ("xterm-end" . "end")
               ("xterm-pgup" . "pgup")
               ("xterm-pgdn" . "pgdn")
               ("xterm-del" . "DEL")))
    (:kitty . (("kitty-up" . "up")
               ("kitty-down" . "down")
               ("kitty-left" . "left")
               ("kitty-right" . "right")
               ("kitty-home" . "home")
               ("kitty-end" . "end")
               ("kitty-pageup" . "pgup")
               ("kitty-pagedown" . "pgdn")
               ("kitty-delete" . "DEL")
               ("kitty-tab" . "TAB")))
    (:wezterm . (("wezterm-up" . "up")
                 ("wezterm-down" . "down")
                 ("wezterm-left" . "left")
                 ("wezterm-right" . "right")
                 ("wezterm-home" . "home")
                 ("wezterm-end" . "end")
                 ("wezterm-pageup" . "pgup")
                 ("wezterm-pagedown" . "pgdn")
                 ("wezterm-delete" . "DEL")
                 ("wezterm-tab" . "TAB")))))

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

  (defun %string-prefix-p (prefix value)
    (and (<= (length prefix) (length value))
         (string-equal prefix value :end2 (length prefix))))

  (defun %now-milliseconds ()
    (truncate (* 1000
                 (/ (coerce (get-internal-real-time) 'double-float)
                    (coerce internal-time-units-per-second 'double-float)))))

  (defun %terminal-profile-from-environment ()
    (let ((term-program (string-downcase (or (uiop:getenv "TERM_PROGRAM") "")))
          (term (string-downcase (or (uiop:getenv "TERM") "")))
          (tmux (uiop:getenv "TMUX")))
      (cond
        ((and (stringp tmux) (not (%string-empty-p tmux)))
         :tmux)
        ((search "wezterm" term-program)
         :wezterm)
        ((search "kitty" term-program)
         :kitty)
        ((or (search "xterm" term)
             (search "xterm" term-program))
         :xterm)
        (t
         :generic))))

  (defun resolve-terminal-key-normalization-profile ()
    (if (eq *terminal-key-normalization-profile* :auto)
        (%terminal-profile-from-environment)
        *terminal-key-normalization-profile*))

  (defun %event-key-token (event-key)
    (cond
      ((stringp event-key)
       (string-downcase event-key))
      ((symbolp event-key)
       (string-downcase (symbol-name event-key)))
      (t
       nil)))

  (defun %strip-tmux-prefix-token (token)
    (let* ((without-prefix (if (%string-prefix-p "tmux:" token)
                               (subseq token 5)
                               token))
           (space-position (position #\Space without-prefix :from-end t)))
      (if space-position
          (subseq without-prefix (1+ space-position))
          without-prefix)))

  (defun %lookup-terminal-normalized-spec (token profile)
    (let ((normalized-token (if (eq profile :tmux)
                                (%strip-tmux-prefix-token token)
                                token)))
      (or (cdr (assoc normalized-token
                      (cdr (assoc profile *terminal-key-normalization-table*))
                      :test #'string=))
          (loop for (name . rules) in *terminal-key-normalization-table*
                thereis (and (not (eq name profile))
                             (cdr (assoc normalized-token rules :test #'string=))))
          (when (and (eq profile :tmux)
                     (search "-" normalized-token))
            normalized-token))))

  (defun %event-key->base-signature (event-key event)
    (let* ((profile (resolve-terminal-key-normalization-profile))
           (ctrl (not (null (ptui.core.events:key-event-ctrlp event))))
           (alt (not (null (ptui.core.events:key-event-altp event))))
           (shift (not (null (ptui.core.events:key-event-shiftp event))))
           (token (%event-key-token event-key))
           (terminal-spec (and token (%lookup-terminal-normalized-spec token profile))))
      (flet ((result (key &key (ctrlp ctrl) (altp alt) (shiftp shift))
               (when key
                 (list :key key :ctrl (not (null ctrlp))
                       :alt (not (null altp))
                       :shift (not (null shiftp))))))
        (cond
          (terminal-spec
           (let ((parsed (%parse-key-spec terminal-spec)))
             (result (getf parsed :key)
                     :ctrlp (or ctrl (getf parsed :ctrl))
                     :altp (or alt (getf parsed :alt))
                     :shiftp (or shift (getf parsed :shift)))))
          ((characterp event-key)
           (result (%normalize-character-key event-key)))
          ((eq event-key :text)
           (let ((text (ptui.core.events:key-event-text? event)))
             (when (and (stringp text) (> (length text) 0))
               (result (%normalize-character-key (char text 0))))))
          ((eq event-key :ctrl-c)
           (result #\c :ctrlp t))
          ((eq event-key :ctrl-j)
           (result #\j :ctrlp t))
          ((or (eq event-key :enter) (eq event-key :return))
           (result :enter))
          ((eq event-key :escape)
           (result :escape))
          ((eq event-key :tab)
           (result :tab))
          ((eq event-key :backspace)
           (result :backspace))
          ((member event-key
                   '(:up :down :left :right :home :end :pgup :pgdn
                     :f1 :f2 :f3 :f4 :f5 :f6 :f7 :f8 :f9 :f10 :f11 :f12)
                   :test #'eq)
           (result event-key))
          ((and token (= (length token) 1))
           (result (%normalize-character-key (char token 0))))
          (t
           (result event-key))))))

  (defun %key-event-signature (event)
    (unless (typep event 'ptui.core.events:key-event)
      (return-from %key-event-signature nil))
    (%event-key->base-signature (ptui.core.events:key-event-key event)
                                event))

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

(defun %active-keymaps (&optional keymap-stack)
  (if (not (null *keymap-overlay-stack*))
      (mapcar #'keymap-overlay-keymap *keymap-overlay-stack*)
      (let ((stack (or keymap-stack *keymap-stack*)))
        (if (null stack)
            (let ((chat (find-keymap 'chat-mode)))
              (if chat
                  (list chat)
                  '()))
            stack))))

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
  (%expire-key-sequence-buffer)
  (setf *key-sequence-buffer*
        (append *key-sequence-buffer* (list signature))
        *key-sequence-last-input-ms* (%now-milliseconds))
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
            (%reset-key-sequence-buffer)
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
                (%reset-key-sequence-buffer))
              (values state nil :none))
            (let ((guard (key-binding-guard binding)))
              (if (or (null guard)
                      (not (null (funcall guard state key-event))))
                  (progn
                    (%reset-key-sequence-buffer)
                    (values (%invoke-key-handler (key-binding-handler binding)
                                                 state
                                                 key-event)
                            t
                            :binding))
                  (progn
                    (when append-sequence
                      (%reset-key-sequence-buffer))
                    (values state nil :guard-skipped)))))))))

(defun %make-alt-modified-key-event (event)
  (ptui.core.events:make-key-event (ptui.core.events:key-event-key event)
                                   :ctrlp (ptui.core.events:key-event-ctrlp event)
                                   :altp t
                                   :shiftp (ptui.core.events:key-event-shiftp event)
                                   :text? (ptui.core.events:key-event-text? event)))

(defun %dispatch-key-event-to-active-maps (key-event state &key keymap-stack)
  (let* ((keymaps (%active-keymaps keymap-stack))
         (signature (%key-event-signature key-event)))
    (unless signature
      (return-from %dispatch-key-event-to-active-maps (values state nil nil)))
    (%append-key-signature signature)
    (%trim-key-sequence-buffer (%max-chord-length keymaps))
    (let ((current-state state))
      (dolist (keymap keymaps (values state nil nil))
        (multiple-value-bind (next-state handledp kind)
            (dispatch-key-event keymap key-event current-state :append-sequence nil)
          (setf current-state next-state)
          (when handledp
            (return-from %dispatch-key-event-to-active-maps
              (values current-state t
                      (list :keymap (keymap-name keymap)
                            :kind kind))))))
      (let ((overlay (current-keymap-overlay)))
        (if (and overlay
                 (%plain-escape-signature-p signature)
                 (keymap-overlay-auto-cleanup-on-escape-p overlay))
            (progn
              (pop-keymap-overlay :reason :escape)
              (values state t
                      (list :keymap (keymap-name (keymap-overlay-keymap overlay))
                            :kind :overlay-exit)))
            (progn
              (%reset-key-sequence-buffer)
              (values state nil nil)))))))

(defun flush-key-dispatch-timeouts (state &key keymap-stack (force nil))
  (%expire-key-sequence-buffer)
  (if (and (%pending-escape-active-p)
           (or force (%pending-escape-expired-p)))
      (let ((pending *pending-escape-event*))
        (%clear-pending-escape)
        (%dispatch-key-event-to-active-maps pending state :keymap-stack keymap-stack))
      (values state nil nil)))

(defun dispatch-active-keymaps (key-event state &key keymap-stack)
  (unless (typep key-event 'ptui.core.events:key-event)
    (return-from dispatch-active-keymaps (values state nil nil)))
  (%expire-key-sequence-buffer)
  (multiple-value-bind (post-timeout-state timeout-handledp timeout-metadata)
      (flush-key-dispatch-timeouts state :keymap-stack keymap-stack)
    (let* ((signature (%key-event-signature key-event))
           (current-state post-timeout-state))
      (unless signature
        (return-from dispatch-active-keymaps
          (values current-state timeout-handledp timeout-metadata)))
      (when (%pending-escape-active-p)
        (if (and (not (%pending-escape-expired-p))
                 (not (%plain-escape-signature-p signature)))
            (progn
              (setf key-event (%make-alt-modified-key-event key-event)
                    signature (%key-event-signature key-event))
              (%clear-pending-escape))
            (progn
              (multiple-value-bind (post-escape-state escape-handledp escape-metadata)
                  (%dispatch-key-event-to-active-maps
                   *pending-escape-event*
                   current-state
                   :keymap-stack keymap-stack)
                (setf current-state post-escape-state
                      timeout-handledp escape-handledp
                      timeout-metadata escape-metadata))
              (%clear-pending-escape))))
      (if (%plain-escape-signature-p signature)
          (progn
            (%set-pending-escape key-event)
            (values current-state t
                    (or timeout-metadata
                        (list :kind :pending-escape))))
          (multiple-value-bind (next-state handledp metadata)
              (%dispatch-key-event-to-active-maps key-event current-state :keymap-stack keymap-stack)
            (if handledp
                (values next-state t metadata)
                (values next-state timeout-handledp timeout-metadata)))))))

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
    (let ((stack (funcall keymap-stack-fn)))
      (multiple-value-bind (state-after-timeouts)
          (flush-key-dispatch-timeouts state :keymap-stack stack)
        (if (typep event 'ptui.core.events:key-event)
            (multiple-value-bind (next-state handledp)
                (dispatch-active-keymaps event state-after-timeouts :keymap-stack stack)
              (if handledp
                  next-state
                  (funcall fallback next-state event)))
            (funcall fallback state-after-timeouts event))))))

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
