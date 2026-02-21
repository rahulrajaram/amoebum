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

(defun %decode-utf8-codepoint (pending)
  (let* ((len (length pending)))
    (when (zerop len)
      (return-from %decode-utf8-codepoint (values nil 0 :incomplete)))
    (let* ((b0 (aref pending 0))
           (need (%utf8-seq-length b0)))
      (cond
        ((null need)
         (values nil 1 :invalid))
        ((< len need)
         (values nil 0 :incomplete))
        ((= need 1)
         (values b0 1 :ok))
        (t
         (dotimes (i (1- need))
           (unless (%valid-continuation-p (aref pending (1+ i)))
             (return-from %decode-utf8-codepoint (values nil 1 :invalid))))
         (let ((codepoint
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
           ;; Basic validity checks to avoid overlong/surrogate ranges.
           (when (or (> codepoint #x10FFFF)
                     (and (= need 2) (< codepoint #x80))
                     (and (= need 3) (< codepoint #x800))
                     (and (= need 4) (< codepoint #x10000))
                     (<= #xD800 codepoint #xDFFF))
             (return-from %decode-utf8-codepoint (values nil 1 :invalid)))
           (values codepoint need :ok)))))))

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
    ;; xterm modifier encoding: ";M" where M = 1 + bitmask
    ;; bit 0 = Shift, bit 1 = Alt/Meta, bit 2 = Ctrl
    (flet ((%decode-modifier (m)
             "Return (values ctrlp altp shiftp) from xterm modifier digit."
             (let ((bits (1- (- m 48))))
               (values (logbitp 2 bits) (logbitp 1 bits) (logbitp 0 bits))))
           (%modified-arrow-key (base-key ctrlp altp shiftp)
             (declare (ignore altp shiftp))
             ;; Emit ctrl-<dir> for backward compat when ctrl is set.
             (if ctrlp
                 (case base-key
                   (:up :ctrl-up) (:down :ctrl-down)
                   (:right :ctrl-right) (:left :ctrl-left)
                   (otherwise base-key))
                 base-key)))
      (cond
        ;; ESC [ code ; modifier <final>  — arrow/fn with modifiers
        ((and (= (aref pending idx) 59)       ; semicolon
              (< (+ idx 2) len)
              (%digit-p (aref pending (1+ idx))))
         (let ((mod-byte (aref pending (1+ idx)))
               (final-idx (+ idx 2)))
           (if (>= final-idx len)
               0  ; need more data
               (let ((final (aref pending final-idx)))
                 (multiple-value-bind (ctrlp altp shiftp) (%decode-modifier mod-byte)
                   (let ((base-key (case final
                                     (65 :up) (66 :down) (67 :right) (68 :left)
                                     (72 :home) (70 :end)
                                     (t nil))))
                     (cond
                       (base-key
                        (let ((key (%modified-arrow-key base-key ctrlp altp shiftp)))
                          (%emit-key-event parser key :ctrlp ctrlp :altp altp :shiftp shiftp)
                          (1+ final-idx)))
                       ((= final 126)
                        ;; ESC [ code ; modifier ~ — modified fn/nav key
                        (let ((fkey (or (%map-csi-fn-key code)
                                        (case code (1 :home) (4 :end) (5 :pgup) (6 :pgdown) (t nil)))))
                          (if fkey
                              (progn (%emit-key-event parser fkey :ctrlp ctrlp :altp altp :shiftp shiftp)
                                     (1+ final-idx))
                              (progn (%emit-key-event parser :escape) 1))))
                       (t (%emit-key-event parser :escape) 1))))))))
        ;; ESC [ code ~  — unmodified fn/nav key
        ((= (aref pending idx) 126)
         (case code
           (1 (%emit-key-event parser :home) (1+ idx))
           (3 (%emit-key-event parser :delete) (1+ idx))
           (4 (%emit-key-event parser :end) (1+ idx))
           (5 (%emit-key-event parser :pgup) (1+ idx))
           (6 (%emit-key-event parser :pgdown) (1+ idx))
           (otherwise
             (let ((fkey (%map-csi-fn-key code)))
               (if fkey
                   (progn (%emit-key-event parser fkey) (1+ idx))
                   (progn (%emit-key-event parser :escape) 1))))))
        ;; Incomplete semicolon — need more data
        ((= (aref pending idx) 59)
         0)
        (t
         (%emit-key-event parser :escape)
         1)))))

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
