(defpackage :ptui.term.input
  (:use :cl)
  (:export #:input-parser #:make-input-parser
           #:input-feed #:input-drain-events))

(in-package :ptui.term.input)

(defstruct (input-parser (:constructor make-input-parser ()))
  ;; Incremental byte buffer so escape/UTF-8 sequences can span reads.
  (pending (make-array 0
                       :element-type '(unsigned-byte 8)
                       :adjustable t
                       :fill-pointer 0))
  (events '())
  ;; Used to disambiguate lone ESC from split escape sequences.
  (feed-seen-p nil :type boolean))

(defun %queue-event (parser event)
  (setf (input-parser-events parser)
        (nconc (input-parser-events parser) (list event)))
  nil)

(defun %push-octet (vector octet)
  (vector-push-extend octet vector)
  vector)

(defun %drop-leading (vector count)
  (let ((len (length vector)))
    (cond
      ((<= count 0) vector)
      ((>= count len)
       (setf (fill-pointer vector) 0)
       vector)
      (t
       (replace vector vector :start1 0 :start2 count :end2 len)
       (setf (fill-pointer vector) (- len count))
       vector))))

(defun %utf8-seq-length (b0)
  (cond
    ((<= b0 #x7F) 1)
    ((<= #xC2 b0 #xDF) 2)
    ((<= #xE0 b0 #xEF) 3)
    ((<= #xF0 b0 #xF4) 4)
    (t nil)))

(defun %valid-continuation-p (byte)
  (= (logand byte #xC0) #x80))

(defun %utf8-sequence-valid-p (pending need)
  (loop for index from 1 below need
        always (%valid-continuation-p (aref pending index))))

(defun %decode-utf8-value (pending need)
  (let ((b0 (aref pending 0)))
    (case need
      (2 (logior (ash (logand b0 #x1F) 6)
                 (logand (aref pending 1) #x3F)))
      (3 (logior (ash (logand b0 #x0F) 12)
                 (ash (logand (aref pending 1) #x3F) 6)
                 (logand (aref pending 2) #x3F)))
      (4 (logior (ash (logand b0 #x07) 18)
                 (ash (logand (aref pending 1) #x3F) 12)
                 (ash (logand (aref pending 2) #x3F) 6)
                 (logand (aref pending 3) #x3F))))))

(defun %invalid-utf8-codepoint-p (codepoint need)
  (or (> codepoint #x10FFFF)
      (and (= need 2) (< codepoint #x80))
      (and (= need 3) (< codepoint #x800))
      (and (= need 4) (< codepoint #x10000))
      (<= #xD800 codepoint #xDFFF)))

(defun %decode-utf8-codepoint (pending)
  (let* ((len (length pending)))
    (when (zerop len)
      (return-from %decode-utf8-codepoint (values nil 0 :incomplete)))
    (let* ((b0 (aref pending 0))
           (need (%utf8-seq-length b0)))
      (when (null need)
        (return-from %decode-utf8-codepoint (values nil 1 :invalid)))
      (when (< len need)
        (return-from %decode-utf8-codepoint (values nil 0 :incomplete)))
      (when (= need 1)
        (return-from %decode-utf8-codepoint (values b0 1 :ok)))
      (unless (%utf8-sequence-valid-p pending need)
        (return-from %decode-utf8-codepoint (values nil 1 :invalid)))
      (let ((codepoint (%decode-utf8-value pending need)))
        (when (%invalid-utf8-codepoint-p codepoint need)
          (return-from %decode-utf8-codepoint (values nil 1 :invalid)))
        (values codepoint need :ok)))))

(defun %printable-char-p (ch)
  (and ch (graphic-char-p ch)))

(defun %emit-text-event (parser text &key (altp nil))
  (%queue-event parser
                (ptui.core.events:make-key-event :text
                                                 :altp altp
                                                 :text? text)))

(defun %emit-key-event (parser key &key (ctrlp nil) (altp nil) (shiftp nil) (text? nil))
  (%queue-event parser
                (ptui.core.events:make-key-event key
                                                 :ctrlp ctrlp
                                                 :altp altp
                                                 :shiftp shiftp
                                                 :text? text?)))

(defun %emit-mouse-event (parser kind button x y)
  (%queue-event parser
                (ptui.core.events:make-mouse-event :kind kind
                                                   :button button
                                                   :x x
                                                   :y y)))

(defun %digit-p (byte)
  (and (<= 48 byte) (<= byte 57)))

(defun %byte-digit (byte)
  (- byte 48))

(defun %map-csi-fn-key (n)
  "Map CSI numeric code to function key. Standard xterm codes skip 16 and 22."
  (case n
    (3  :delete)
    (11 :f1)
    (12 :f2)
    (13 :f3)
    (14 :f4)
    (15 :f5)
    (17 :f6)
    (18 :f7)
    (19 :f8)
    (20 :f9)
    (21 :f10)
    (23 :f11)
    (24 :f12)
    (t nil)))

(defun %decode-csi-modifier (modifier-byte)
  "Return (values ctrlp altp shiftp) from xterm modifier digit."
  (let ((bits (1- (- modifier-byte 48))))
    (values (logbitp 2 bits) (logbitp 1 bits) (logbitp 0 bits))))

(defun %modified-arrow-key (base-key ctrlp)
  (if ctrlp
      (case base-key
        (:up :ctrl-up)
        (:down :ctrl-down)
        (:right :ctrl-right)
        (:left :ctrl-left)
        (otherwise base-key))
      base-key))

(defun %csi-final-key (final-byte)
  (case final-byte
    (65 :up)
    (66 :down)
    (67 :right)
    (68 :left)
    (72 :home)
    (70 :end)
    (t nil)))

(defun %csi-tilde-key (code)
  (or (%map-csi-fn-key code)
      (case code
        (1 :home)
        (4 :end)
        (5 :pgup)
        (6 :pgdn)
        (t nil))))

(defun %emit-csi-fallback-escape (parser)
  (%emit-key-event parser :escape)
  1)

(defun %emit-modified-csi-key (parser code modifier-byte final-byte final-index)
  (multiple-value-bind (ctrlp altp shiftp) (%decode-csi-modifier modifier-byte)
    (let ((arrow-key (%csi-final-key final-byte)))
      (when arrow-key
        (%emit-key-event parser
                         (%modified-arrow-key arrow-key ctrlp)
                         :ctrlp ctrlp
                         :altp altp
                         :shiftp shiftp)
        (return-from %emit-modified-csi-key (1+ final-index)))
      (when (= final-byte 126)
        (let ((key (%csi-tilde-key code)))
          (when key
            (%emit-key-event parser key :ctrlp ctrlp :altp altp :shiftp shiftp)
            (return-from %emit-modified-csi-key (1+ final-index)))))
      (%emit-csi-fallback-escape parser))))

(defun %emit-plain-csi-key (parser code terminator index)
  (declare (ignore parser))
  (unless (= terminator 126)
    (return-from %emit-plain-csi-key nil))
  (let ((key (%csi-tilde-key code)))
    (when key
      (values key (1+ index)))))

(defun %parse-csi-with-prefix (parser)
  (let* ((pending (input-parser-pending parser))
         (len (length pending))
         (code 0)
         (idx 2))
    (when (< len 4)
      (return-from %parse-csi-with-prefix 0))
    (loop while (< idx len)
          while (%digit-p (aref pending idx))
          do
            (setf code (+ (* code 10) (%byte-digit (aref pending idx))))
            (incf idx))
    (when (>= idx len)
      (return-from %parse-csi-with-prefix 0))
    (let ((terminator (aref pending idx)))
      (when (= terminator 59)
        (when (or (>= (+ idx 2) len)
                  (not (%digit-p (aref pending (1+ idx)))))
          (return-from %parse-csi-with-prefix 0))
        (return-from %parse-csi-with-prefix
          (%emit-modified-csi-key parser
                                  code
                                  (aref pending (1+ idx))
                                  (aref pending (+ idx 2))
                                  (+ idx 2))))
      (multiple-value-bind (key consumed)
          (%emit-plain-csi-key parser code terminator idx)
        (when key
          (%emit-key-event parser key)
          (return-from %parse-csi-with-prefix consumed)))
      (%emit-csi-fallback-escape parser))))

(defun %decode-sgr-button (cb final-byte)
  "Decode SGR button code CB and final byte (M=press, m=release) into (values kind button).
  Cb bits: 0-1=button, bit 5=motion, bit 6=scroll.
  Returns (values :press/:release/:move/:scroll-up/:scroll-down button-or-nil)."
  (let ((is-release (= final-byte 109))   ; lowercase m
        (is-scroll  (logbitp 6 cb))
        (is-motion  (logbitp 5 cb))
        (btn-bits   (logand cb 3)))
    (cond
      (is-scroll
       (if (= (logand cb 1) 0)
           (values :scroll-up nil)
           (values :scroll-down nil)))
      (is-motion
       (let ((button (case btn-bits
                       (0 :left)
                       (1 :middle)
                       (2 :right)
                       (otherwise nil))))
         (values :move button)))
      (is-release
       (let ((button (case btn-bits
                       (0 :left)
                       (1 :middle)
                       (2 :right)
                       (otherwise nil))))
         (values :release button)))
      (t
       (let ((button (case btn-bits
                       (0 :left)
                       (1 :middle)
                       (2 :right)
                       (otherwise nil))))
         (values :press button))))))

(defun %parse-sgr-mouse-sequence (parser)
  "Parse an SGR mouse sequence starting at byte 3 (after ESC [ <).
  Format: ESC [ < Cb ; Cx ; Cy M/m
  Returns number of bytes consumed, or 0 if incomplete, or 1 (fallback escape) if malformed."
  (let* ((pending (input-parser-pending parser))
         (len (length pending))
         ;; We have ESC [ < already confirmed at positions 0,1,2
         ;; digits start at idx 3
         (idx 3)
         (cb 0)
         (cx 0)
         (cy 0))
    ;; Parse Cb digits
    (unless (< idx len)
      (return-from %parse-sgr-mouse-sequence 0))
    (let ((has-cb-digit nil))
      (loop while (and (< idx len) (%digit-p (aref pending idx)))
            do
              (setf cb (+ (* cb 10) (%byte-digit (aref pending idx))))
              (setf has-cb-digit t)
              (incf idx))
      ;; Need at least one digit for Cb
      (unless has-cb-digit
        ;; Malformed: no digits after ESC [ < — need more data or fallback
        (if (>= idx len)
            (return-from %parse-sgr-mouse-sequence 0)
            ;; Got a non-digit character immediately: fallback
            (return-from %parse-sgr-mouse-sequence (%emit-csi-fallback-escape parser)))))
    ;; Expect semicolon separator
    (unless (< idx len)
      (return-from %parse-sgr-mouse-sequence 0))
    (unless (= (aref pending idx) 59)   ; semicolon
      (return-from %parse-sgr-mouse-sequence (%emit-csi-fallback-escape parser)))
    (incf idx)
    ;; Parse Cx digits
    (unless (< idx len)
      (return-from %parse-sgr-mouse-sequence 0))
    (let ((has-cx-digit nil))
      (loop while (and (< idx len) (%digit-p (aref pending idx)))
            do
              (setf cx (+ (* cx 10) (%byte-digit (aref pending idx))))
              (setf has-cx-digit t)
              (incf idx))
      (unless has-cx-digit
        (if (>= idx len)
            (return-from %parse-sgr-mouse-sequence 0)
            (return-from %parse-sgr-mouse-sequence (%emit-csi-fallback-escape parser)))))
    ;; Expect semicolon separator
    (unless (< idx len)
      (return-from %parse-sgr-mouse-sequence 0))
    (unless (= (aref pending idx) 59)
      (return-from %parse-sgr-mouse-sequence (%emit-csi-fallback-escape parser)))
    (incf idx)
    ;; Parse Cy digits
    (unless (< idx len)
      (return-from %parse-sgr-mouse-sequence 0))
    (let ((has-cy-digit nil))
      (loop while (and (< idx len) (%digit-p (aref pending idx)))
            do
              (setf cy (+ (* cy 10) (%byte-digit (aref pending idx))))
              (setf has-cy-digit t)
              (incf idx))
      (unless has-cy-digit
        (if (>= idx len)
            (return-from %parse-sgr-mouse-sequence 0)
            (return-from %parse-sgr-mouse-sequence (%emit-csi-fallback-escape parser)))))
    ;; Expect final byte: M (77) for press/motion or m (109) for release
    (unless (< idx len)
      (return-from %parse-sgr-mouse-sequence 0))
    (let ((final-byte (aref pending idx)))
      (unless (or (= final-byte 77) (= final-byte 109))
        (return-from %parse-sgr-mouse-sequence (%emit-csi-fallback-escape parser)))
      (multiple-value-bind (kind button) (%decode-sgr-button cb final-byte)
        ;; Convert 1-based terminal coordinates to 0-based internal coords
        (%emit-mouse-event parser kind button (1- cx) (1- cy)))
      (1+ idx))))

(defun %parse-escape-sequence (parser)
  (let* ((pending (input-parser-pending parser))
         (len (length pending)))
    (when (zerop len)
      (return-from %parse-escape-sequence 0))
    (unless (= (aref pending 0) 27)
      (return-from %parse-escape-sequence 0))
    (cond
      ;; Need more data to decide lone ESC vs sequence.
      ((= len 1)
       0)
      ;; CSI sequences: ESC [ ...
      ((= (aref pending 1) 91)
       (if (< len 3)
           0
         (case (aref pending 2)
           ;; Arrows: ESC [ A/B/C/D
           (65 (%emit-key-event parser :up) 3)
           (66 (%emit-key-event parser :down) 3)
           (67 (%emit-key-event parser :right) 3)
           (68 (%emit-key-event parser :left) 3)
           ;; Home/End: ESC [ H / ESC [ F
           (72 (%emit-key-event parser :home) 3)
           (70 (%emit-key-event parser :end) 3)
           ;; SGR mouse protocol: ESC [ < Cb ; Cx ; Cy M/m
           (60 (%parse-sgr-mouse-sequence parser))
           ;; Numeric CSI: ESC [ <digit> ...
           ;; Handles: ESC [ N ~ (Home=1~, End=4~, PgUp=5~, PgDn=6~)
           ;; and:     ESC [ 1 ; 5 <letter> (Ctrl+Arrow)
           (otherwise (%parse-csi-with-prefix parser)))))
      ;; Alt-modified byte (ESC + UTF-8 printable)
      (t
       (let ((b1 (aref pending 1)))
         (cond
           ((and (<= 32 b1) (<= b1 126))
            (%emit-text-event parser (string (code-char b1)) :altp t)
            2)
           (t
            (%emit-key-event parser :escape)
            1)))))))

(defun %parse-leading-token (parser &key (alt-prefix nil))
  (let ((pending (input-parser-pending parser)))
    (when (zerop (length pending))
      (return-from %parse-leading-token 0))
    (let ((b0 (aref pending 0)))
      (cond
        ((= b0 27)
         (%parse-escape-sequence parser))
        ((= b0 13)
         (%emit-key-event parser :enter)
         1)
        ((= b0 10)
         (%emit-key-event parser :ctrl-j :ctrlp t)
         1)
        ((= b0 9)
         (%emit-key-event parser :tab)
         1)
        ((or (= b0 8) (= b0 127))
         (%emit-key-event parser :backspace)
         1)
        ((= b0 3)
         (%emit-key-event parser :ctrl-c :ctrlp t)
         1)
        ((= b0 1)
         (%emit-key-event parser :ctrl-a :ctrlp t)
         1)
        ((= b0 5)
         (%emit-key-event parser :ctrl-e :ctrlp t)
         1)
        ((= b0 11)
         (%emit-key-event parser :ctrl-k :ctrlp t)
         1)
        ((= b0 21)
         (%emit-key-event parser :ctrl-u :ctrlp t)
         1)
        ((= b0 23)
         (%emit-key-event parser :ctrl-w :ctrlp t)
         1)
        ((= b0 14)  ; Ctrl-N
         (%emit-key-event parser :ctrl-n :ctrlp t)
         1)
        ((= b0 16)  ; Ctrl-P
         (%emit-key-event parser :ctrl-p :ctrlp t)
         1)
        ((= b0 18)  ; Ctrl-R
         (%emit-key-event parser :ctrl-r :ctrlp t)
         1)
        (t
         (multiple-value-bind (codepoint consumed status)
             (%decode-utf8-codepoint pending)
           (declare (ignore status))
           (cond
             ((= consumed 0)
              0)
             ((null codepoint)
              ;; Invalid lead/continuation: consume one byte and continue.
              1)
             (t
              (let ((ch (code-char codepoint)))
                (if (%printable-char-p ch)
                    (progn
                      (%emit-text-event parser (string ch) :altp alt-prefix)
                      consumed)
                  (progn
                    (%emit-key-event parser :unknown :altp alt-prefix)
                    consumed)))))))))))

(defun input-feed (parser octets)
  (let ((pending (input-parser-pending parser)))
    (setf (input-parser-feed-seen-p parser) t)
    (loop for byte across octets do
      (%push-octet pending byte))
    (loop
      for consumed = (%parse-leading-token parser)
      while (> consumed 0) do
        (%drop-leading pending consumed)))
  nil)

(defun input-drain-events (parser)
  (let ((pending (input-parser-pending parser)))
    ;; If ESC has been pending for a whole cycle with no new input,
    ;; treat it as an ESC keypress.
    (when (and (= (length pending) 1)
               (= (aref pending 0) 27)
               (not (input-parser-feed-seen-p parser)))
      (%emit-key-event parser :escape)
      (%drop-leading pending 1))
    (setf (input-parser-feed-seen-p parser) nil)
    (let ((events (input-parser-events parser)))
      (setf (input-parser-events parser) '())
      (values events (length events)))))
