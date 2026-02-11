(defpackage :ptui.text.width
  (:use :cl)
  (:export
   #:codepoint-width
   #:grapheme-width
   #:string-width
   #:east-asian-wide-codepoint-p
   #:combining-codepoint-p))

(in-package :ptui.text.width)

(defun %codepoint-in-range-p (cp start end)
  (and (<= start cp) (<= cp end)))

(defun combining-codepoint-p (cp)
  (or (%codepoint-in-range-p cp #x0300 #x036F)
      (%codepoint-in-range-p cp #x1AB0 #x1AFF)
      (%codepoint-in-range-p cp #x1DC0 #x1DFF)
      (%codepoint-in-range-p cp #x20D0 #x20FF)
      (%codepoint-in-range-p cp #xFE20 #xFE2F)))

(defun %variation-selector-codepoint-p (cp)
  (or (%codepoint-in-range-p cp #xFE00 #xFE0F)
      (%codepoint-in-range-p cp #xE0100 #xE01EF)))

(defun %control-codepoint-p (cp)
  (or (%codepoint-in-range-p cp #x0000 #x001F)
      (%codepoint-in-range-p cp #x007F #x009F)))

(defun %regional-indicator-codepoint-p (cp)
  (%codepoint-in-range-p cp #x1F1E6 #x1F1FF))

(defun %emoji-codepoint-p (cp)
  ;; Keep this conservative: only ranges that are consistently emoji-style
  ;; across modern terminals. Do not classify the entire Dingbats block
  ;; as width-2 because many symbols render as width-1 text glyphs.
  (or (%codepoint-in-range-p cp #x1F000 #x1FAFF)
      (%codepoint-in-range-p cp #x2600 #x26FF)))

(defun east-asian-wide-codepoint-p (cp)
  (or (%codepoint-in-range-p cp #x1100 #x115F)
      (%codepoint-in-range-p cp #x2329 #x232A)
      (%codepoint-in-range-p cp #x2E80 #xA4CF)
      (%codepoint-in-range-p cp #xAC00 #xD7A3)
      (%codepoint-in-range-p cp #xF900 #xFAFF)
      (%codepoint-in-range-p cp #xFE10 #xFE19)
      (%codepoint-in-range-p cp #xFE30 #xFE6F)
      (%codepoint-in-range-p cp #xFF00 #xFF60)
      (%codepoint-in-range-p cp #xFFE0 #xFFE6)
      (%codepoint-in-range-p cp #x20000 #x2FFFD)
      (%codepoint-in-range-p cp #x30000 #x3FFFD)))

(defun codepoint-width (ch)
  "Return terminal-cell width for CH using deterministic fallback rules."
  (check-type ch character)
  (let ((cp (char-code ch)))
    (cond
      ((%control-codepoint-p cp) 0)
      ((or (combining-codepoint-p cp)
           (%variation-selector-codepoint-p cp)
           (= cp #x200D))
       0)
      ((or (east-asian-wide-codepoint-p cp)
           (%emoji-codepoint-p cp))
       2)
      (t
       1))))

(defun %contains-emoji-p (cluster)
  (loop for ch across cluster
        thereis (%emoji-codepoint-p (char-code ch))))

(defun %contains-joiner-p (cluster)
  (loop for ch across cluster
        thereis (= (char-code ch) #x200D)))

(defun %all-regional-indicators-p (cluster)
  (and (> (length cluster) 0)
       (loop for ch across cluster
             always (%regional-indicator-codepoint-p (char-code ch)))))

(defun %single-cluster-width (cluster)
  (cond
    ((zerop (length cluster))
     0)
    ((string= cluster (string #\Newline))
     0)
    ((and (%contains-joiner-p cluster) (%contains-emoji-p cluster))
     2)
    ((and (= (length cluster) 2) (%all-regional-indicators-p cluster))
     2)
    (t
     (loop for ch across cluster sum (codepoint-width ch)))))

(defun grapheme-width (grapheme &key (engine :auto))
  "Return display width of GRAPHEME (or string containing graphemes)."
  (check-type grapheme string)
  (loop for cluster in (ptui.text.grapheme:split-graphemes grapheme :engine engine)
        sum (%single-cluster-width cluster)))

(defun string-width (text &key (engine :auto))
  "Return display width of TEXT by grapheme cluster."
  (check-type text string)
  (loop for cluster in (ptui.text.grapheme:split-graphemes text :engine engine)
        sum (%single-cluster-width cluster)))
