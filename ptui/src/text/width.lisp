(defpackage :ptui.text.width
  (:use :cl)
  (:export
   #:codepoint-width
   #:grapheme-width
   #:string-width
   #:east-asian-wide-codepoint-p
   #:combining-codepoint-p))

(in-package :ptui.text.width)

(defun combining-codepoint-p (cp)
  (ptui.text.adapter.fallback:combining-codepoint-p cp))

(defun %variation-selector-codepoint-p (cp)
  (ptui.text.adapter.fallback:variation-selector-codepoint-p cp))

(defun %regional-indicator-codepoint-p (cp)
  (ptui.text.adapter.fallback:regional-indicator-codepoint-p cp))

(defun %emoji-codepoint-p (cp)
  (ptui.text.adapter.fallback:emoji-codepoint-p cp))

(defun %joiner-codepoint-p (cp)
  (ptui.text.adapter.fallback:joiner-codepoint-p cp))

(defun %keycap-codepoint-p (cp)
  (ptui.text.adapter.fallback:keycap-codepoint-p cp))

(defun east-asian-wide-codepoint-p (cp)
  (ptui.text.adapter.fallback:east-asian-wide-codepoint-p cp))

(defun codepoint-width (ch &key (engine :auto))
  "Return terminal-cell width for CH via selected text engine adapter."
  (check-type ch character)
  (ptui.text.engine:call-codepoint-width ch :engine engine))

(defun %contains-emoji-p (cluster)
  (loop for ch across cluster
        thereis (%emoji-codepoint-p (char-code ch))))

(defun %contains-joiner-p (cluster)
  (loop for ch across cluster
        thereis (%joiner-codepoint-p (char-code ch))))

(defun %contains-keycap-p (cluster)
  (loop for ch across cluster
        thereis (%keycap-codepoint-p (char-code ch))))

(defun %all-regional-indicators-p (cluster)
  (and (> (length cluster) 0)
       (loop for ch across cluster
             always (%regional-indicator-codepoint-p (char-code ch)))))

(defun %single-cluster-width (cluster engine)
  (cond
    ((zerop (length cluster))
     0)
    ((string= cluster (string #\Newline))
     0)
    ((%contains-keycap-p cluster)
     2)
    ((and (%contains-joiner-p cluster) (%contains-emoji-p cluster))
     2)
    ((and (= (length cluster) 2) (%all-regional-indicators-p cluster))
     2)
    (t
     (loop for ch across cluster
           sum (codepoint-width ch :engine engine)))))

(defun grapheme-width (grapheme &key (engine :auto))
  "Return display width of GRAPHEME (or string containing graphemes)."
  (check-type grapheme string)
  (loop for cluster in (ptui.text.grapheme:split-graphemes grapheme :engine engine)
        sum (%single-cluster-width cluster engine)))

(defun string-width (text &key (engine :auto))
  "Return display width of TEXT by grapheme cluster."
  (check-type text string)
  (loop for cluster in (ptui.text.grapheme:split-graphemes text :engine engine)
        sum (%single-cluster-width cluster engine)))
