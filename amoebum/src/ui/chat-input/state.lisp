(in-package :amoebum)

;;; NXT-543: state and text-manipulation helpers extracted from
;;; ui/chat-input.lisp. Owns input-text mutation primitives, cluster-aware
;;; grapheme helpers, parsed input content parts, and small string-shape
;;; predicates used across the chat-input modules. No rendering or event
;;; dispatch lives here.

(defun chat-ui-set-input (state text &key cursor-position)
  "Set the input text. CURSOR-POSITION: nil = end, integer = grapheme index."
  (let ((chat-state (ensure-chat-ui-state state)))
    (setf (chat-ui-state-input-text chat-state)
          (if (stringp text)
              text
              (princ-to-string text))
          (chat-ui-state-prompt-scroll-offset chat-state) nil
          (chat-ui-state-cursor-position chat-state) cursor-position)
    (%chat-sync-fuzzy-picker! chat-state :step-p nil)
    (chat-ui-state-input-text chat-state)))

(defun %blank-string-p (text)
  (every (lambda (char)
           (member char '(#\Space #\Tab #\Newline #\Return) :test #'char=))
         text))

(defun %chat-push-input-text-part (parts text)
  (if (and (stringp text) (plusp (length text)))
      (cons (pseudopod:make-text-part text) parts)
      parts))

(defun %chat-parse-input-content-parts (input)
  (let ((source (if (stringp input) input (princ-to-string input)))
        (cursor 0)
        (parts '()))
    (labels ((emit-text (from to)
               (when (< from to)
                 (setf parts (%chat-push-input-text-part parts (subseq source from to))))))
      (loop
        for marker = (search "![" source :start2 cursor)
        while marker do
          (let* ((alt-end (position #\] source :start (+ marker 2)))
                 (open-paren (and alt-end
                                  (< (1+ alt-end) (length source))
                                  (char= (char source (1+ alt-end)) #\()
                                  (1+ alt-end)))
                 (close-paren (and open-paren
                                   (position #\) source :start (1+ open-paren)))))
            (unless (and alt-end open-paren close-paren)
              (loop-finish))
            (emit-text cursor marker)
            (let ((path (string-trim '(#\Space #\Tab #\Newline #\Return)
                                     (subseq source (+ open-paren 1) close-paren))))
              (if (plusp (length path))
                  (push (%make-image-content-part path) parts)
                  (emit-text marker (1+ close-paren))))
            (setf cursor (1+ close-paren))))
      (emit-text cursor (length source)))
    (nreverse parts)))

(defun chat-ui-submit-input (state)
  (let* ((chat-state (ensure-chat-ui-state state))
         (conversation (%ensure-chat-conversation-state chat-state))
         (input (chat-ui-state-input-text chat-state)))
    (setf (chat-ui-state-agentic-iteration-count chat-state) 0)
    (if (or (null input) (zerop (length input)) (%blank-string-p input))
        nil
        (handler-case
            (let ((content (%chat-parse-input-content-parts input)))
              (if (null content)
                  nil
                  (prog1
                      (progn
                        (conversation-transition! conversation :user-input)
                        (chat-ui-add-message chat-state "user" content))
                    (setf (chat-ui-state-input-text chat-state) ""
                          (chat-ui-state-prompt-scroll-offset chat-state) nil)
                    (fuzzy-picker-deactivate! (%ensure-chat-fuzzy-picker-state chat-state)))))
          (error (condition)
            (chat-ui-add-message
             chat-state
             "system"
             (format nil "Unable to attach image input: ~A" condition))
            nil)))))

(defun %pop-last-grapheme (text)
  (let ((clusters (ptui.text.grapheme:split-graphemes text)))
    (if (null clusters)
        ""
        (with-output-to-string (out)
          (dolist (cluster (butlast clusters))
            (write-string cluster out))))))

(defun %delete-word-backward (text)
  "Delete the last word from TEXT, following readline Ctrl+W semantics:
skip trailing whitespace, then delete back to the next whitespace boundary."
  (when (or (null text) (zerop (length text)))
    (return-from %delete-word-backward ""))
  (let ((end (length text))
        (pos (1- (length text))))
    ;; Skip trailing whitespace
    (loop while (and (>= pos 0)
                     (member (char text pos) '(#\Space #\Tab #\Newline #\Return)
                             :test #'char=))
          do (decf pos))
    ;; Delete back through non-whitespace (the word)
    (loop while (and (>= pos 0)
                     (not (member (char text pos) '(#\Space #\Tab #\Newline #\Return)
                                  :test #'char=)))
          do (decf pos))
    (if (< pos 0)
        ""
        (subseq text 0 (1+ pos)))))

;;; ---------------------------------------------------------------------------
;;; Cursor-aware grapheme helpers
;;; ---------------------------------------------------------------------------

(defun %grapheme-length (text)
  "Return the number of grapheme clusters in TEXT."
  (if (or (null text) (zerop (length text)))
      0
      (length (ptui.text.grapheme:split-graphemes text))))

(defun %word-boundary-p (cluster)
  "Return T if CLUSTER is a word-boundary character (whitespace or punctuation)."
  (and (= (length cluster) 1)
       (let ((ch (char cluster 0)))
         (or (member ch '(#\Space #\Tab #\Newline #\Return) :test #'char=)
             (member ch '(#\( #\) #\[ #\] #\{ #\} #\< #\>
                          #\. #\, #\; #\: #\! #\? #\/ #\\
                          #\- #\+ #\= #\* #\& #\| #\^ #\~
                          #\' #\" #\` #\@ #\# #\$ #\%)
                     :test #'char=)))))

(defun %word-boundary-backward (text cursor-pos)
  "Return the grapheme offset for the word boundary to the left of CURSOR-POS."
  (let* ((clusters (ptui.text.grapheme:split-graphemes text))
         (pos (min (max 0 cursor-pos) (length clusters))))
    (when (<= pos 0)
      (return-from %word-boundary-backward 0))
    ;; Skip whitespace/punctuation backward
    (loop while (and (> pos 0)
                     (%word-boundary-p (nth (1- pos) clusters)))
          do (decf pos))
    ;; Skip word characters backward
    (loop while (and (> pos 0)
                     (not (%word-boundary-p (nth (1- pos) clusters))))
          do (decf pos))
    pos))

(defun %word-boundary-forward (text cursor-pos)
  "Return the grapheme offset for the word boundary to the right of CURSOR-POS."
  (let* ((clusters (ptui.text.grapheme:split-graphemes text))
         (len (length clusters))
         (pos (min (max 0 cursor-pos) len)))
    (when (>= pos len)
      (return-from %word-boundary-forward len))
    ;; Skip word characters forward
    (loop while (and (< pos len)
                     (not (%word-boundary-p (nth pos clusters))))
          do (incf pos))
    ;; Skip whitespace/punctuation forward
    (loop while (and (< pos len)
                     (%word-boundary-p (nth pos clusters)))
          do (incf pos))
    pos))

(defun %grapheme-insert-at (text cursor-pos new-text)
  "Insert NEW-TEXT at grapheme position CURSOR-POS in TEXT."
  (let* ((clusters (ptui.text.grapheme:split-graphemes text))
         (pos (min (max 0 cursor-pos) (length clusters))))
    (with-output-to-string (out)
      (loop for cluster in (subseq clusters 0 pos)
            do (write-string cluster out))
      (write-string new-text out)
      (loop for cluster in (nthcdr pos clusters)
            do (write-string cluster out)))))

(defun %grapheme-delete-before (text cursor-pos)
  "Delete grapheme before CURSOR-POS. Returns (values new-text new-cursor)."
  (if (or (<= cursor-pos 0) (zerop (length text)))
      (values text 0)
      (let* ((clusters (ptui.text.grapheme:split-graphemes text))
             (pos (min cursor-pos (length clusters))))
        (values
         (with-output-to-string (out)
           (loop for cluster in (subseq clusters 0 (1- pos))
                 do (write-string cluster out))
           (loop for cluster in (nthcdr pos clusters)
                 do (write-string cluster out)))
         (1- pos)))))

(defun %grapheme-delete-at (text cursor-pos)
  "Delete grapheme at CURSOR-POS (Delete key). Returns new text."
  (let* ((clusters (ptui.text.grapheme:split-graphemes text))
         (pos (min cursor-pos (length clusters))))
    (if (>= pos (length clusters))
        text
        (with-output-to-string (out)
          (loop for cluster in (subseq clusters 0 pos)
                do (write-string cluster out))
          (loop for cluster in (nthcdr (1+ pos) clusters)
                do (write-string cluster out))))))

(defun %delete-word-backward-at (text cursor-pos)
  "Delete word backward from CURSOR-POS. Returns (values new-text new-cursor)."
  (if (or (<= cursor-pos 0) (zerop (length text)))
      (values text 0)
      (let* ((clusters (ptui.text.grapheme:split-graphemes text))
             (pos (min cursor-pos (length clusters)))
             (before (with-output-to-string (out)
                       (loop for cluster in (subseq clusters 0 pos)
                             do (write-string cluster out))))
             (after-delete (%delete-word-backward before))
             (new-cursor (%grapheme-length after-delete)))
        (values
         (with-output-to-string (out)
           (write-string after-delete out)
           (loop for cluster in (nthcdr pos clusters)
                 do (write-string cluster out)))
         new-cursor))))

(defun %ensure-cursor-pos (text cursor-pos)
  "Coerce NIL cursor-position to end-of-text grapheme count."
  (if (null cursor-pos)
      (%grapheme-length text)
      cursor-pos))

(defun %chat-trim-text (text)
  (if (stringp text)
      (string-trim '(#\Space #\Tab #\Newline #\Return) text)
      ""))

(defun %normalize-instruction-text (text)
  (let ((trimmed (%chat-trim-text text)))
    (with-output-to-string (out)
      (loop with previous-space-p = t
            for char across (string-downcase trimmed) do
              (if (or (alphanumericp char) (char= char #\Space))
                  (progn
                    (write-char char out)
                    (setf previous-space-p (char= char #\Space)))
                  (unless previous-space-p
                    (write-char #\Space out)
                    (setf previous-space-p t)))))))
