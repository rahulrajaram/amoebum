(in-package :amoebum)

;;; Thin facade for streaming markdown rendering and event journal helpers.
;;; Token-stream state ownership now lives in src/ui/streaming/token-stream.lisp,
;;; and provider orchestration lives in src/ui/streaming/provider-runtime.lisp.

(defstruct (string-builder
            (:constructor make-string-builder (&optional (initial-size 256)))
            (:copier nil))
  "Mutable string builder to avoid repeated CONCATENATE allocation."
  (buffer (make-array (max 1 (or initial-size 256))
                      :element-type 'character
                      :adjustable t
                      :fill-pointer 0)
   :type (array character (*)))
  (length 0 :type fixnum))

(defun string-builder-append (sb string)
  "Append STRING to the string builder SB. Returns SB."
  (declare (type string-builder sb)
           (type string string))
  (let* ((buf (string-builder-buffer sb))
         (len (length string))
         (new-len (+ (string-builder-length sb) len))
         (current-cap (array-dimension buf 0)))
    (declare (type fixnum len new-len current-cap))
    (when (> new-len current-cap)
      ;; Double the buffer size or use new-len, whichever is larger
      (let ((new-cap (max (* current-cap 2) new-len)))
        (adjust-array buf new-cap)))
    (setf (fill-pointer buf) new-len)
    (replace buf string :start1 (string-builder-length sb))
    (setf (string-builder-length sb) new-len)
    sb))

(defun string-builder-get (sb)
  "Return the accumulated string from SB. Returns a fresh string."
  (declare (type string-builder sb))
  (subseq (string-builder-buffer sb) 0 (string-builder-length sb)))

(defun string-builder-clear (sb)
  "Clear the string builder for reuse. Returns SB."
  (declare (type string-builder sb))
  (setf (string-builder-length sb) 0)
  (setf (fill-pointer (string-builder-buffer sb)) 0)
  sb)

(defparameter +stream-cursor-glyph+ "█")
(defparameter +stream-cursor-blink-ms+ 450)

(defun %stream-markdown-split-lines (text)
  (let ((value (if (stringp text)
                   text
                   (princ-to-string (or text "")))))
    (if (zerop (length value))
        (list "")
        (let ((lines '())
              (start 0)
              (length (length value)))
          (loop for index from 0 below length do
            (when (char= (char value index) #\Newline)
              (push (subseq value start index) lines)
              (setf start (1+ index))))
          (push (subseq value start length) lines)
          (nreverse lines)))))

(defun %stream-markdown-heading-prefix-length (line)
  (let* ((length (length line))
         (count 0))
    (loop while (and (< count length)
                     (< count 6)
                     (char= (char line count) #\#))
          do (incf count))
    (if (and (> count 0)
             (or (= count length)
                 (member (char line count) '(#\Space #\Tab) :test #'char=)))
        count
        nil)))

(defun %stream-markdown-line-style (line)
  (let ((heading-prefix-length (%stream-markdown-heading-prefix-length line)))
    (if heading-prefix-length
        (values (string-left-trim '(#\Space #\Tab)
                                  (subseq line heading-prefix-length))
                :assistant-heading
                t)
        (values line :assistant nil))))

(defun %stream-markdown-default-role (segments)
  (if (and (listp segments) segments)
      (let ((seg (first segments)))
        (if (compact-segment-p seg)
            (style-entry-role (lookup-style (compact-segment-style-id seg)))
            (getf seg :role :assistant)))
      :assistant))

(defun %stream-markdown-fence-line-p (line)
  (let ((trimmed (string-trim '(#\Space #\Tab) line)))
    (and (>= (length trimmed) 3)
         (char= (char trimmed 0) #\`)
         (char= (char trimmed 1) #\`)
         (char= (char trimmed 2) #\`))))

(defun %stream-markdown-fence-language (line)
  (let* ((trimmed (string-trim '(#\Space #\Tab) line))
         (length (length trimmed)))
    (when (> length 3)
      (let ((language (string-trim '(#\Space #\Tab)
                                   (subseq trimmed 3 length))))
        (when (plusp (length language))
          (string-downcase language))))))

(defun %stream-markdown-leading-indent-length (line)
  (let ((length (length line))
        (index 0))
    (loop while (and (< index length)
                     (member (char line index) '(#\Space #\Tab) :test #'char=))
          do (incf index))
    index))

(defun %stream-markdown-list-marker-end (line start)
  (let ((length (length line)))
    (cond
      ((>= start length)
       nil)
      ((and (< (+ start 1) length)
            (member (char line start) '(#\- #\+ #\*) :test #'char=)
            (member (char line (1+ start)) '(#\Space #\Tab) :test #'char=))
       (+ start 2))
      (t
       (let ((index start))
         (loop while (and (< index length)
                          (digit-char-p (char line index)))
               do (incf index))
         (when (and (> index start)
                    (< (1+ index) length)
                    (char= (char line index) #\.)
                    (member (char line (1+ index)) '(#\Space #\Tab) :test #'char=))
           (+ index 2)))))))

;;; ---------------------------------------------------------------------------
;;; Language-aware code tokenizer
;;; ---------------------------------------------------------------------------

(defparameter *%stream-code-language-keywords*
  (let ((table (make-hash-table :test #'equal)))
    (setf (gethash "lisp" table)
          '("defun" "defmacro" "defvar" "defparameter" "defstruct" "defclass"
            "defmethod" "defgeneric" "defconstant" "defpackage" "define-condition"
            "let" "let*" "flet" "labels" "lambda" "progn" "block" "tagbody"
            "if" "when" "unless" "cond" "case" "typecase" "etypecase"
            "loop" "do" "dolist" "dotimes" "mapcar" "mapc"
            "setf" "setq" "push" "pop" "incf" "decf"
            "funcall" "apply" "multiple-value-bind" "values"
            "declare" "the" "locally" "in-package" "use-package"
            "handler-case" "handler-bind" "restart-case" "unwind-protect"
            "with-open-file" "with-slots" "with-accessors"
            "nil" "t" "null"))
    (setf (gethash "common-lisp" table) (gethash "lisp" table))
    (setf (gethash "cl" table) (gethash "lisp" table))
    (setf (gethash "python" table)
          '("def" "class" "return" "yield" "import" "from" "as"
            "if" "elif" "else" "for" "while" "break" "continue" "pass"
            "try" "except" "finally" "raise" "with" "assert"
            "and" "or" "not" "in" "is" "lambda" "global" "nonlocal"
            "True" "False" "None" "self" "async" "await"))
    (setf (gethash "py" table) (gethash "python" table))
    (setf (gethash "javascript" table)
          '("function" "return" "var" "let" "const" "if" "else"
            "for" "while" "do" "switch" "case" "break" "continue"
            "try" "catch" "finally" "throw" "new" "delete" "typeof" "instanceof"
            "class" "extends" "import" "export" "default" "from"
            "async" "await" "yield" "this" "super"
            "true" "false" "null" "undefined"))
    (setf (gethash "js" table) (gethash "javascript" table))
    (setf (gethash "typescript" table) (gethash "javascript" table))
    (setf (gethash "ts" table) (gethash "javascript" table))
    (setf (gethash "bash" table)
          '("if" "then" "else" "elif" "fi" "for" "while" "do" "done"
            "case" "esac" "in" "function" "return" "local" "export"
            "source" "eval" "exec" "set" "unset" "shift" "trap"
            "true" "false"))
    (setf (gethash "sh" table) (gethash "bash" table))
    (setf (gethash "shell" table) (gethash "bash" table))
    (setf (gethash "zsh" table) (gethash "bash" table))
    (setf (gethash "go" table)
          '("func" "return" "var" "const" "type" "struct" "interface"
            "if" "else" "for" "range" "switch" "case" "default" "break" "continue"
            "package" "import" "defer" "go" "select" "chan" "map" "make" "new"
            "true" "false" "nil" "iota"))
    (setf (gethash "rust" table)
          '("fn" "let" "mut" "const" "static" "struct" "enum" "impl" "trait"
            "if" "else" "for" "while" "loop" "match" "break" "continue" "return"
            "use" "mod" "pub" "crate" "self" "super" "where" "as" "in" "ref"
            "async" "await" "move" "unsafe" "dyn" "type"
            "true" "false" "Some" "None" "Ok" "Err"))
    (setf (gethash "json" table) '("true" "false" "null"))
    table)
  "Per-language keyword lists for syntax highlighting.")

(defun %stream-code-comment-prefix (language)
  "Return the single-line comment prefix for LANGUAGE, or NIL."
  (cond
    ((member language '("lisp" "common-lisp" "cl") :test #'string-equal) ";")
    ((member language '("python" "py" "bash" "sh" "shell" "zsh") :test #'string-equal) "#")
    ((member language '("javascript" "js" "typescript" "ts" "go" "rust" "java" "c" "cpp" "c++")
             :test #'string-equal) "//")
    (t nil)))

(defun %stream-code-string-chars (language)
  "Return list of string delimiter characters for LANGUAGE."
  (declare (ignore language))
  '(#\" #\'))

(defun %stream-markdown-tokenize-code-line (line language)
  "Tokenize LINE for LANGUAGE, returning a list of (text role) pairs.
   Recognizes comments, strings, numbers, and keywords."
  (let* ((lang (and (stringp language) (string-downcase language)))
         (keywords (and lang (gethash lang *%stream-code-language-keywords*)))
         (comment-prefix (and lang (%stream-code-comment-prefix lang)))
         (string-chars (and lang (%stream-code-string-chars lang)))
         (len (length line))
         (segments nil)
         (i 0))
    (when (or (null lang) (zerop len) (null keywords))
      (return-from %stream-markdown-tokenize-code-line
        (list (list line :assistant-code))))
    (labels
        ((flush-plain (start end)
           (when (> end start)
             (push (list (subseq line start end) :assistant-code) segments)))
         (word-boundary-p (pos)
           (or (>= pos len)
               (let ((ch (char line pos)))
                 (not (or (alphanumericp ch) (char= ch #\_) (char= ch #\-))))))
         (word-start-p (pos)
           (or (zerop pos)
               (let ((ch (char line (1- pos))))
                 (not (or (alphanumericp ch) (char= ch #\_) (char= ch #\-)))))))
      ;; Check for comment (after leading whitespace)
      (let ((trimmed-start (position-if-not
                            (lambda (c) (member c '(#\Space #\Tab))) line)))
        (when (and comment-prefix trimmed-start
                   (<= (+ trimmed-start (length comment-prefix)) len)
                   (string= line comment-prefix
                            :start1 trimmed-start
                            :end1 (+ trimmed-start (length comment-prefix))))
          (return-from %stream-markdown-tokenize-code-line
            (list (list line :assistant-code-comment)))))
      ;; Lisp: line starting with ; is a comment
      (when (and (member lang '("lisp" "common-lisp" "cl") :test #'string-equal)
                 (let ((ts (position-if-not
                            (lambda (c) (member c '(#\Space #\Tab))) line)))
                   (and ts (char= (char line ts) #\;))))
        (return-from %stream-markdown-tokenize-code-line
          (list (list line :assistant-code-comment))))
      ;; Token-by-token scan
      (let ((plain-start 0))
        (loop while (< i len) do
          (let ((ch (char line i)))
            (cond
              ;; String literal
              ((and string-chars (member ch string-chars))
               (flush-plain plain-start i)
               (let ((quote-char ch)
                     (j (1+ i)))
                 (loop while (< j len) do
                   (let ((c (char line j)))
                     (cond
                       ((char= c #\\) (incf j 2))  ; skip escape
                       ((char= c quote-char) (incf j) (return))
                       (t (incf j)))))
                 (push (list (subseq line i j) :assistant-code-string) segments)
                 (setf i j plain-start j)))
              ;; Number (digit at word boundary)
              ((and (digit-char-p ch) (word-start-p i))
               (flush-plain plain-start i)
               (let ((j i))
                 (loop while (and (< j len)
                                  (let ((c (char line j)))
                                    (or (digit-char-p c) (char= c #\.) (char= c #\x)
                                        (char= c #\X) (char= c #\e) (char= c #\E)
                                        (char= c #\_))))
                       do (incf j))
                 (push (list (subseq line i j) :assistant-code-number) segments)
                 (setf i j plain-start j)))
              ;; Word — check if keyword
              ((and (or (alpha-char-p ch) (char= ch #\_))
                    (word-start-p i))
               (let ((j i))
                 (loop while (and (< j len)
                                  (let ((c (char line j)))
                                    (or (alphanumericp c) (char= c #\_) (char= c #\-))))
                       do (incf j))
                 (let ((word (subseq line i j)))
                   (cond
                     ((member word keywords :test #'string=)
                      (flush-plain plain-start i)
                      (push (list word :assistant-code-keyword) segments)
                      (setf i j plain-start j))
                     (t (setf i j))))))
              (t (incf i)))))
        (flush-plain plain-start len)))
    (nreverse segments)))

(defun %stream-markdown-code-line-segments (line language)
  (let ((code-role :assistant-code)
        (keyword-role :assistant-code-keyword)
        (fence-role :assistant-code-fence)
        (left-rule (%stream-markdown-make-segment "│ " :assistant-code-fence :dimp t)))
    (let ((content-segments
            (cond
              ;; Markdown-in-code: existing heading/list detection
              ((and (stringp language)
                    (or (string= language "markdown")
                        (string= language "md")))
               (let* ((indent-end (%stream-markdown-leading-indent-length line))
                      (length (length line))
                      (heading-prefix-length
                        (and (< indent-end length)
                             (%stream-markdown-heading-prefix-length
                              (subseq line indent-end length))))
                      (marker-end
                        (or (and heading-prefix-length
                                 (+ indent-end heading-prefix-length))
                            (%stream-markdown-list-marker-end line indent-end))))
                 (if (and marker-end
                          (> marker-end 0)
                          (<= marker-end length))
                     (let* ((indent-text (subseq line 0 indent-end))
                            (marker-text (subseq line indent-end marker-end))
                            (body-text (subseq line marker-end length))
                            (segments '()))
                       (when (plusp (length indent-text))
                         (push (%stream-markdown-make-segment indent-text code-role) segments))
                       (when (plusp (length marker-text))
                         (push (%stream-markdown-make-segment marker-text keyword-role
                                                              :boldp t)
                               segments))
                       (when (plusp (length body-text))
                         (push (%stream-markdown-make-segment body-text code-role) segments))
                       (or (nreverse segments)
                           (list (%stream-markdown-make-segment line code-role))))
                     (list (%stream-markdown-make-segment line code-role)))))
              ;; Language-aware syntax highlighting
              ((and (stringp language)
                    (plusp (length language))
                    (gethash (string-downcase language)
                             *%stream-code-language-keywords*))
               (let ((tokens (%stream-markdown-tokenize-code-line line language)))
                 (mapcar (lambda (tok)
                           (%stream-markdown-make-segment
                            (first tok) (second tok)
                            :boldp (eq (second tok) :assistant-code-keyword)
                            :italicp (eq (second tok) :assistant-code-comment)))
                         tokens)))
              ;; Fallback: plain code
              (t (list (%stream-markdown-make-segment line code-role))))))
      (declare (ignore fence-role))
      (cons left-rule content-segments))))

(defun %stream-markdown-style-key (segment)
  "Return the style-id for a compact segment, or compute one for legacy plist."
  (if (compact-segment-p segment)
      (compact-segment-style-id segment)
      (list (getf segment :role :assistant)
            (not (null (getf segment :boldp)))
            (not (null (getf segment :italicp)))
            (not (null (getf segment :underlinep)))
            (not (null (getf segment :invertp)))
            (not (null (getf segment :dimp)))
            (not (null (getf segment :strikep))))))

(defun %stream-markdown-make-segment (text role
                                      &key
                                        (boldp nil)
                                        (italicp nil)
                                        (underlinep nil)
                                        (invertp nil)
                                        (dimp nil)
                                        (strikep nil))
  "Return a compact segment: (text . style-id)."
  (cons (or text "")
        (intern-style role
                      :boldp boldp :italicp italicp
                      :underlinep underlinep :invertp invertp
                      :dimp dimp :strikep strikep)))

(defun %stream-markdown-push-segment (segments-rev text role
                                      &key
                                        (boldp nil)
                                        (italicp nil)
                                        (underlinep nil)
                                        (invertp nil)
                                        (dimp nil)
                                        (strikep nil))
  (if (%token-stream-empty-string-p text)
      segments-rev
      (let* ((style-id (intern-style role
                                     :boldp boldp :italicp italicp
                                     :underlinep underlinep :invertp invertp
                                     :dimp dimp :strikep strikep))
             (head (first segments-rev)))
        (if (and head
                 (compact-segment-p head)
                 (eql (compact-segment-style-id head) style-id))
            (progn
              (setf (car head)
                    (concatenate 'string (car head) text))
              segments-rev)
            (cons (cons text style-id) segments-rev)))))

(defstruct (stream-markdown-inline-state
            (:constructor %make-stream-markdown-inline-state
                (&key (line "") role (headingp nil) (length 0))))
  (line "" :type string)
  role
  (headingp nil :type boolean)
  (length 0 :type fixnum)
  (index 0 :type fixnum)
  (boldp nil :type boolean)
  (italicp nil :type boolean)
  (mode :text)
  (buffer (make-string-output-stream))
  (segments-rev '()))

(defun %make-stream-markdown-inline-parser (line role headingp)
  (%make-stream-markdown-inline-state :line line
                                      :role role
                                      :headingp (not (null headingp))
                                      :length (length line)))

(defun %stream-markdown-inline-code-mode-p (state)
  (eq (stream-markdown-inline-state-mode state) :code))

(defun %stream-markdown-inline-current-char (state)
  (let ((index (stream-markdown-inline-state-index state)))
    (when (< index (stream-markdown-inline-state-length state))
      (char (stream-markdown-inline-state-line state) index))))

(defun %stream-markdown-inline-next-char (state)
  (let ((next-index (1+ (stream-markdown-inline-state-index state))))
    (when (< next-index (stream-markdown-inline-state-length state))
      (char (stream-markdown-inline-state-line state) next-index))))

(defun %stream-markdown-inline-advance (state &optional (count 1))
  (incf (stream-markdown-inline-state-index state) count)
  state)

(defun %stream-markdown-inline-write-char (state char)
  (write-char char (stream-markdown-inline-state-buffer state))
  (%stream-markdown-inline-advance state)
  state)

(defun %stream-markdown-inline-flush-buffer (state)
  (let ((text (get-output-stream-string (stream-markdown-inline-state-buffer state))))
    (setf (stream-markdown-inline-state-segments-rev state)
          (%stream-markdown-push-segment
           (stream-markdown-inline-state-segments-rev state)
           text
           (if (%stream-markdown-inline-code-mode-p state)
               :assistant-inline-code
               (stream-markdown-inline-state-role state))
           :boldp (or (stream-markdown-inline-state-headingp state)
                      (stream-markdown-inline-state-boldp state))
           :italicp (and (not (%stream-markdown-inline-code-mode-p state))
                         (stream-markdown-inline-state-italicp state)))))
  state)

(defun %stream-markdown-inline-toggle-style (state style-key &key (advance 1))
  (%stream-markdown-inline-flush-buffer state)
  (ecase style-key
    (:bold
     (setf (stream-markdown-inline-state-boldp state)
           (not (stream-markdown-inline-state-boldp state))))
    (:italic
     (setf (stream-markdown-inline-state-italicp state)
           (not (stream-markdown-inline-state-italicp state))))
    (:code
     (setf (stream-markdown-inline-state-mode state)
           (if (%stream-markdown-inline-code-mode-p state)
               :text
               :code))))
  (%stream-markdown-inline-advance state advance))

(defun %stream-markdown-inline-link-bounds (state)
  (let* ((line (stream-markdown-inline-state-line state))
         (index (stream-markdown-inline-state-index state))
         (length (stream-markdown-inline-state-length state))
         (label-end (position #\] line :start (1+ index)))
         (open-paren-p (and label-end
                            (< (1+ label-end) length)
                            (char= (char line (1+ label-end)) #\()))
         (url-end (and open-paren-p
                       (position #\) line :start (+ label-end 2)))))
    (values label-end open-paren-p url-end)))

(defun %stream-markdown-inline-handle-link (state)
  (multiple-value-bind (label-end open-paren-p url-end)
      (%stream-markdown-inline-link-bounds state)
    (when (and label-end open-paren-p url-end)
      (let* ((line (stream-markdown-inline-state-line state))
             (label (subseq line
                            (1+ (stream-markdown-inline-state-index state))
                            label-end)))
        (%stream-markdown-inline-flush-buffer state)
        (setf (stream-markdown-inline-state-segments-rev state)
              (%stream-markdown-push-segment
               (stream-markdown-inline-state-segments-rev state)
               label
               (stream-markdown-inline-state-role state)
               :boldp (or (stream-markdown-inline-state-headingp state)
                          (stream-markdown-inline-state-boldp state))
               :italicp (stream-markdown-inline-state-italicp state)
               :underlinep t)
              (stream-markdown-inline-state-index state) (1+ url-end))
        t))))

(defun %stream-markdown-inline-handle-char (state char)
  (cond
    ((and (not (%stream-markdown-inline-code-mode-p state))
          (member char '(#\* #\_) :test #'char=)
          (let ((next-char (%stream-markdown-inline-next-char state)))
            (and next-char (char= char next-char))))
     (%stream-markdown-inline-toggle-style state :bold :advance 2))
    ((and (not (%stream-markdown-inline-code-mode-p state))
          (char= char #\[)
          (%stream-markdown-inline-handle-link state))
     state)
    ((char= char #\`)
     (%stream-markdown-inline-toggle-style state :code))
    ((and (not (%stream-markdown-inline-code-mode-p state))
          (member char '(#\* #\_) :test #'char=))
     (%stream-markdown-inline-toggle-style state :italic))
    (t
     (%stream-markdown-inline-write-char state char))))

(defun %stream-markdown-inline-step (state)
  (let ((char (%stream-markdown-inline-current-char state)))
    (when char
      (%stream-markdown-inline-handle-char state char)))
  state)

(defun %stream-markdown-inline-finish (state)
  (%stream-markdown-inline-flush-buffer state)
  (let ((segments (stream-markdown-inline-state-segments-rev state)))
    (if segments
        (nreverse segments)
        (list (%stream-markdown-make-segment
               ""
               (stream-markdown-inline-state-role state)
               :boldp (stream-markdown-inline-state-headingp state))))))

(defun %stream-markdown-parse-inline (line role &key (headingp nil))
  (let ((state (%make-stream-markdown-inline-parser line role headingp)))
    (loop while (< (stream-markdown-inline-state-index state)
                   (stream-markdown-inline-state-length state))
          do (%stream-markdown-inline-step state))
    (%stream-markdown-inline-finish state)))

(defun %stream-markdown-space-grapheme-p (grapheme)
  "Return T if GRAPHEME is a single space or tab character."
  (and (= (length grapheme) 1)
       (let ((ch (char grapheme 0)))
         (or (char= ch #\Space)
             (char= ch #\Tab)))))

(defun %segment-text (seg)
  "Return text from a segment, compact or legacy plist."
  (if (compact-segment-p seg)
      (compact-segment-text seg)
      (getf seg :text "")))

(defun %stream-markdown-strip-trailing-spaces (segments-rev)
  "Remove trailing space-only text from reversed segment list."
  (loop while segments-rev
        for text = (%segment-text (first segments-rev))
        while (and (plusp (length text))
                   (every (lambda (ch) (or (char= ch #\Space) (char= ch #\Tab))) text))
        do (pop segments-rev))
  segments-rev)

(defstruct (stream-markdown-wrap-state
            (:constructor %make-stream-markdown-wrap-state
                (&key (safe-width 1) (default-role :assistant))))
  (safe-width 1 :type fixnum)
  (default-role :assistant)
  (lines-rev '())
  (line-segments-rev '())
  (line-width 0 :type fixnum)
  (break-segments-rev nil)
  (break-width 0 :type fixnum)
  (break-valid-p nil :type boolean)
  (word-graphemes '())
  (word-width 0 :type fixnum))

(defun %stream-markdown-wrap-grapheme-width (grapheme)
  (max 0 (ptui.text.width:grapheme-width grapheme)))

(defun %stream-markdown-wrap-reset-line (state)
  (setf (stream-markdown-wrap-state-line-segments-rev state) '()
        (stream-markdown-wrap-state-line-width state) 0
        (stream-markdown-wrap-state-break-segments-rev state) nil
        (stream-markdown-wrap-state-break-width state) 0
        (stream-markdown-wrap-state-break-valid-p state) nil
        (stream-markdown-wrap-state-word-graphemes state) '()
        (stream-markdown-wrap-state-word-width state) 0)
  state)

(defun %stream-markdown-wrap-push-line (state segments-rev)
  (let ((stripped (%stream-markdown-strip-trailing-spaces segments-rev)))
    (push (if stripped
              (nreverse stripped)
              (list (%stream-markdown-make-segment
                     ""
                     (stream-markdown-wrap-state-default-role state))))
          (stream-markdown-wrap-state-lines-rev state)))
  (%stream-markdown-wrap-reset-line state))

(defun %stream-markdown-wrap-emit-current-line (state)
  (%stream-markdown-wrap-push-line state
                                   (stream-markdown-wrap-state-line-segments-rev state)))

(defun %stream-markdown-wrap-push-grapheme (state style-segment grapheme)
  (let ((style-id (if (compact-segment-p style-segment)
                      (compact-segment-style-id style-segment)
                      (intern-style
                       (getf style-segment :role
                             (stream-markdown-wrap-state-default-role state))
                       :boldp (getf style-segment :boldp)
                       :italicp (getf style-segment :italicp)
                       :underlinep (getf style-segment :underlinep)
                       :invertp (getf style-segment :invertp)
                       :dimp (getf style-segment :dimp)
                       :strikep (getf style-segment :strikep)))))
    (let* ((segs (stream-markdown-wrap-state-line-segments-rev state))
           (head (first segs)))
      (if (and head
               (compact-segment-p head)
               (eql (compact-segment-style-id head) style-id))
          (progn
            (setf (car head) (concatenate 'string (car head) grapheme))
            (setf (stream-markdown-wrap-state-line-segments-rev state) segs))
          (setf (stream-markdown-wrap-state-line-segments-rev state)
                (cons (cons grapheme style-id) segs)))))
  state)

(defun %stream-markdown-wrap-append-grapheme (state style-segment grapheme &key (breakp nil))
  (let ((grapheme-width (%stream-markdown-wrap-grapheme-width grapheme)))
    (%stream-markdown-wrap-push-grapheme state style-segment grapheme)
    (incf (stream-markdown-wrap-state-line-width state) grapheme-width)
    (if breakp
        (setf (stream-markdown-wrap-state-break-segments-rev state)
              (mapcar #'copy-list
                      (stream-markdown-wrap-state-line-segments-rev state))
              (stream-markdown-wrap-state-break-width state)
              (stream-markdown-wrap-state-line-width state)
              (stream-markdown-wrap-state-break-valid-p state) t
              (stream-markdown-wrap-state-word-graphemes state) '()
              (stream-markdown-wrap-state-word-width state) 0)
        (progn
          (push (cons style-segment grapheme)
                (stream-markdown-wrap-state-word-graphemes state))
          (incf (stream-markdown-wrap-state-word-width state) grapheme-width))))
  state)

(defun %stream-markdown-wrap-replay-word (state saved-word)
  (dolist (pair saved-word)
    (%stream-markdown-wrap-push-grapheme state (car pair) (cdr pair))
    (incf (stream-markdown-wrap-state-line-width state)
          (%stream-markdown-wrap-grapheme-width (cdr pair))))
  (setf (stream-markdown-wrap-state-word-graphemes state)
        (copy-list saved-word)
        (stream-markdown-wrap-state-word-width state)
        (reduce #'+ saved-word
                :key (lambda (pair)
                       (%stream-markdown-wrap-grapheme-width (cdr pair)))
                :initial-value 0))
  state)

(defun %stream-markdown-wrap-can-rewind-word-p (state grapheme-width)
  (and (stream-markdown-wrap-state-break-valid-p state)
       (> (stream-markdown-wrap-state-break-width state) 0)
       (<= (+ (stream-markdown-wrap-state-word-width state) grapheme-width)
           (stream-markdown-wrap-state-safe-width state))))

(defun %stream-markdown-wrap-overflow-space (state)
  (%stream-markdown-wrap-emit-current-line state))

(defun %stream-markdown-wrap-overflow-rewind (state style-segment grapheme)
  (let ((saved-word (nreverse (stream-markdown-wrap-state-word-graphemes state))))
    (%stream-markdown-wrap-push-line state
                                     (stream-markdown-wrap-state-break-segments-rev state))
    (%stream-markdown-wrap-replay-word state saved-word)
    (%stream-markdown-wrap-append-grapheme state style-segment grapheme))
  state)

(defun %stream-markdown-wrap-overflow-hard-break (state style-segment grapheme grapheme-width)
  (%stream-markdown-wrap-emit-current-line state)
  (%stream-markdown-wrap-push-grapheme state style-segment grapheme)
  (incf (stream-markdown-wrap-state-line-width state) grapheme-width)
  (setf (stream-markdown-wrap-state-word-graphemes state) (list (cons style-segment grapheme))
        (stream-markdown-wrap-state-word-width state) grapheme-width)
  state)

(defun %stream-markdown-wrap-handle-overflow (state style-segment grapheme grapheme-width breakp)
  (cond
    (breakp
     (%stream-markdown-wrap-overflow-space state))
    ((%stream-markdown-wrap-can-rewind-word-p state grapheme-width)
     (%stream-markdown-wrap-overflow-rewind state style-segment grapheme))
    (t
     (%stream-markdown-wrap-overflow-hard-break state style-segment grapheme grapheme-width))))

(defun %stream-markdown-wrap-process-grapheme (state style-segment grapheme)
  (let ((grapheme-width (%stream-markdown-wrap-grapheme-width grapheme))
        (breakp (%stream-markdown-space-grapheme-p grapheme)))
    (if (and (> (stream-markdown-wrap-state-line-width state) 0)
             (> (+ (stream-markdown-wrap-state-line-width state) grapheme-width)
                (stream-markdown-wrap-state-safe-width state)))
        (%stream-markdown-wrap-handle-overflow
         state style-segment grapheme grapheme-width breakp)
        (%stream-markdown-wrap-append-grapheme state style-segment grapheme :breakp breakp))))

(defun %stream-markdown-wrap-process-segment (state segment)
  (dolist (grapheme (ptui.text.grapheme:split-graphemes (%segment-text segment)))
    (%stream-markdown-wrap-process-grapheme state segment grapheme))
  state)

(defun %stream-markdown-wrap-finish (state)
  (when (or (stream-markdown-wrap-state-line-segments-rev state)
            (null (stream-markdown-wrap-state-lines-rev state)))
    (%stream-markdown-wrap-emit-current-line state))
  (nreverse (stream-markdown-wrap-state-lines-rev state)))

(defun %stream-markdown-wrap-segments (segments width &key (default-role :assistant))
  "Wrap styled segments into lines respecting word boundaries where possible."
  (let ((state (%make-stream-markdown-wrap-state
                :safe-width (max 1 (if (integerp width) width 1))
                :default-role default-role)))
    (dolist (segment segments)
      (%stream-markdown-wrap-process-segment state segment))
    (%stream-markdown-wrap-finish state)))

(defun %stream-markdown-line-width (segments)
  (reduce #'+ segments
          :initial-value 0
          :key (lambda (segment)
                 (ptui.text.width:string-width (%segment-text segment)))))

(defun %stream-markdown-render-line-segments (raw-line in-fenced-code-p fenced-language)
  (cond
    ((%stream-markdown-fence-line-p raw-line)
     (values (list (%stream-markdown-make-segment "│ " :assistant-code-fence :dimp t)
                   (%stream-markdown-make-segment raw-line
                                                  :assistant-code-fence
                                                  :boldp t
                                                  :dimp t))
             (not in-fenced-code-p)
             (if in-fenced-code-p
                 nil
                 (%stream-markdown-fence-language raw-line))))
    (in-fenced-code-p
     (values (%stream-markdown-code-line-segments raw-line fenced-language)
             t
             fenced-language))
    (t
     (multiple-value-bind (line-text line-role headingp)
         (%stream-markdown-line-style raw-line)
       (values (%stream-markdown-parse-inline line-text line-role :headingp headingp)
               in-fenced-code-p
               fenced-language)))))

(defstruct (streaming-markdown-renderer
            (:constructor make-streaming-markdown-renderer
                (&key
                  (width 0)
                  (pending-line "")
                  (logical-lines '())
                  (wrapped-lines '())
                  (in-fenced-code-p nil)
                  (fenced-language nil))))
  (width 0 :type integer)
  (pending-line "" :type string)
  (logical-lines '() :type list)
  (wrapped-lines '() :type list)
  (in-fenced-code-p nil :type boolean)
  (fenced-language nil)
  ;; P0 #3 FIX: Tail pointers for O(1) list append instead of O(n) traversal.
  ;; Without these, every append walks the entire accumulated list.
  (logical-lines-tail nil :type list)
  (wrapped-lines-tail nil :type list))

(defun streaming-markdown-renderer-reset (renderer)
  (check-type renderer streaming-markdown-renderer)
  (setf (streaming-markdown-renderer-width renderer) 0
        (streaming-markdown-renderer-pending-line renderer) ""
        (streaming-markdown-renderer-logical-lines renderer) '()
        (streaming-markdown-renderer-wrapped-lines renderer) '()
        (streaming-markdown-renderer-in-fenced-code-p renderer) nil
        (streaming-markdown-renderer-fenced-language renderer) nil
        (streaming-markdown-renderer-logical-lines-tail renderer) nil
        (streaming-markdown-renderer-wrapped-lines-tail renderer) nil)
  renderer)

(defun %renderer-append-logical-line! (renderer segments)
  "Append SEGMENTS to the renderer's logical-lines in O(1) via tail pointer."
  (let ((new-cell (list segments)))
    (if (streaming-markdown-renderer-logical-lines-tail renderer)
        (setf (cdr (streaming-markdown-renderer-logical-lines-tail renderer)) new-cell)
        (setf (streaming-markdown-renderer-logical-lines renderer) new-cell))
    (setf (streaming-markdown-renderer-logical-lines-tail renderer) new-cell)))

(defun %renderer-append-wrapped-lines! (renderer new-lines)
  "Append NEW-LINES to the renderer's wrapped-lines in O(1) via tail pointer."
  (when new-lines
    (if (streaming-markdown-renderer-wrapped-lines-tail renderer)
        (setf (cdr (streaming-markdown-renderer-wrapped-lines-tail renderer)) new-lines)
        (setf (streaming-markdown-renderer-wrapped-lines renderer) new-lines))
    (setf (streaming-markdown-renderer-wrapped-lines-tail renderer) (last new-lines))))

(defun %stream-markdown-renderer-rewrap! (renderer width)
  (let ((safe-width (max 1 (if (integerp width) width 1))))
    ;; Full rewrap: clear and rebuild wrapped-lines with tail pointer
    (setf (streaming-markdown-renderer-wrapped-lines renderer) '()
          (streaming-markdown-renderer-wrapped-lines-tail renderer) nil)
    (dolist (segments (streaming-markdown-renderer-logical-lines renderer))
      (%renderer-append-wrapped-lines!
       renderer
       (%stream-markdown-wrap-segments
        segments
        safe-width
        :default-role (%stream-markdown-default-role segments))))
    (setf (streaming-markdown-renderer-width renderer) safe-width)
    renderer))

(defun streaming-markdown-renderer-append-chunk (renderer chunk)
  (check-type renderer streaming-markdown-renderer)
  (let* ((chunk-text (if (stringp chunk) chunk (princ-to-string (or chunk "")))))
    (when (plusp (length chunk-text))
      ;; P1 #5 FIX: Use string-builder instead of CONCATENATE to avoid
      ;; allocating intermediate strings during streaming
      (let* ((sb (make-string-builder (+ (length (streaming-markdown-renderer-pending-line renderer))
                                         (length chunk-text)
                                         256)))
             (_ (string-builder-append sb (streaming-markdown-renderer-pending-line renderer)))
             (_ (string-builder-append sb chunk-text))
             (combined (string-builder-get sb))
             (start 0)
             (length (length combined))
             (safe-width (streaming-markdown-renderer-width renderer)))
        (declare (ignore _))
        (loop for index from 0 below length do
          (when (char= (char combined index) #\Newline)
            (let ((raw-line (subseq combined start index)))
              (multiple-value-bind (segments next-fenced-p next-language)
                  (%stream-markdown-render-line-segments
                   raw-line
                   (streaming-markdown-renderer-in-fenced-code-p renderer)
                   (streaming-markdown-renderer-fenced-language renderer))
                ;; P0 #3 FIX: O(1) tail-append instead of O(n) list walk
                (%renderer-append-logical-line! renderer segments)
                (setf (streaming-markdown-renderer-in-fenced-code-p renderer) next-fenced-p
                      (streaming-markdown-renderer-fenced-language renderer) next-language)
                (when (> safe-width 0)
                  (%renderer-append-wrapped-lines!
                   renderer
                   (%stream-markdown-wrap-segments
                    segments safe-width
                    :default-role (%stream-markdown-default-role segments))))))
            (setf start (1+ index))))
        (setf (streaming-markdown-renderer-pending-line renderer)
              (subseq combined start length)))))
  renderer)

(defun streaming-markdown-renderer-render-lines (renderer width
                                                 &key
                                                   (partialp nil)
                                                   (cursor-visible-p nil)
                                                   (cursor-glyph +stream-cursor-glyph+))
  (check-type renderer streaming-markdown-renderer)
  (let* ((safe-width (max 1 (if (integerp width) width 1))))
    (when (/= safe-width (streaming-markdown-renderer-width renderer))
      (%stream-markdown-renderer-rewrap! renderer safe-width))
    ;; P0 #2 FIX: Avoid copy-list by exploiting list-sharing semantics.
    ;; - Between prompts: no pending, no cursor → return wrapped-lines directly
    ;; - During streaming with pending text: (append ...) already creates a fresh
    ;;   list spine, so cursor can safely mutate the result without copy-list
    ;; - Cursor only, no pending (rare): must copy-list to protect wrapped-lines
    (let* ((base-lines (streaming-markdown-renderer-wrapped-lines renderer))
           (pending-line (streaming-markdown-renderer-pending-line renderer))
           (has-pending (plusp (length pending-line)))
           (needs-cursor (and partialp cursor-visible-p
                              (stringp cursor-glyph) (plusp (length cursor-glyph))))
           (styled-lines base-lines))
      (when has-pending
        (multiple-value-bind (pending-segments pending-fenced-p pending-language)
            (%stream-markdown-render-line-segments
             pending-line
             (streaming-markdown-renderer-in-fenced-code-p renderer)
             (streaming-markdown-renderer-fenced-language renderer))
          (declare (ignore pending-fenced-p pending-language))
          ;; append creates a FRESH spine — safe for cursor mutation below
          (setf styled-lines
                (append styled-lines
                        (%stream-markdown-wrap-segments
                         pending-segments
                         safe-width
                         :default-role (%stream-markdown-default-role pending-segments))))))
      (unless styled-lines
        (setf styled-lines (list (list (%stream-markdown-make-segment "" :assistant)))))
      (when needs-cursor
        ;; If no pending-line was appended, styled-lines still shares structure
        ;; with base-lines. Must copy before mutating.
        (unless has-pending
          (setf styled-lines (copy-list styled-lines)))
        (let* ((cursor-segment (%stream-markdown-make-segment cursor-glyph :assistant
                                                              :boldp t
                                                              :invertp t))
               (last-line (car (last styled-lines)))
               (last-width (%stream-markdown-line-width last-line))
               (cursor-width (ptui.text.width:string-width cursor-glyph)))
          (if (and (> cursor-width 0)
                   (> (+ last-width cursor-width) safe-width))
              (setf styled-lines
                    (append styled-lines (list (list cursor-segment))))
              (setf (car (last styled-lines))
                    (append last-line (list cursor-segment))))))
      styled-lines)))

(defun stream-markdown-styled-lines (text width
                                     &key
                                       (partialp nil)
                                       (cursor-visible-p nil)
                                       (cursor-glyph +stream-cursor-glyph+))
  (let ((renderer (make-streaming-markdown-renderer)))
    (streaming-markdown-renderer-append-chunk renderer text)
    (streaming-markdown-renderer-render-lines renderer width
                                              :partialp partialp
                                              :cursor-visible-p cursor-visible-p
                                              :cursor-glyph cursor-glyph)))

;;; -----------------------------------------------------------------------------
;;; NXT-129: Stream Event Journal (append-only ring buffer)
;;; -----------------------------------------------------------------------------

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
      ;; Drop the oldest quarter
      (let* ((keep-start (floor cap 4))
             (new-entries (make-array (- (length entries) keep-start)
                                     :adjustable t :fill-pointer (- (length entries) keep-start))))
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
