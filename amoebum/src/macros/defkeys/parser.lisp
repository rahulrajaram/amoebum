(in-package :amoebum)

;;;; ---------------------------------------------------------------------------
;;;; Keymap parser + types.
;;;;
;;;; Pure-function layer for the defkeys subsystem:
;;;;   * `malformed-key-binding` and `keymap-definition-warning` conditions
;;;;   * `key-binding`, `key-chord`, `keymap`, `keymap-overlay` structs
;;;;   * All parameter globals shared across defkeys submodules (registry,
;;;;     stack, overlay stack, sequence buffer, timeouts, pending escape,
;;;;     terminal normalization profile + table)
;;;;   * Compile-time-available helpers (eval-when) for parsing key
;;;;     specifications, resolving the terminal-key normalization profile,
;;;;     turning a ptui key-event into a normalized signature, resolving a
;;;;     keymap designator, and chord-sequence suffix/prefix matching.
;;;;
;;;; Behavior is preserved verbatim from the original
;;;; `amoebum/src/macros/defkeys.lisp`; only file boundaries change.
;;;; ---------------------------------------------------------------------------

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
