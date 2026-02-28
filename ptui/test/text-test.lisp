(defpackage :ptui.test.text
  (:use :cl :fiveam)
  (:export #:run-all
           #:text-suite))

(in-package :ptui.test.text)

(def-suite text-suite
  :description "PTUI text pipeline coverage (I308): width, grapheme, and layout.")

(in-suite text-suite)

(defun string-from-codepoints (&rest codepoints)
  (coerce (mapcar #'code-char codepoints) 'string))

;;; ===================================================================
;;; I308 text width
;;; ===================================================================

(test width-classifies-ascii-wide-and-combining
  (is (= 1 (ptui.text.width:codepoint-width #\A)))
  (is (= 2 (ptui.text.width:codepoint-width #\界)))
  (is (= 0 (ptui.text.width:codepoint-width #\́))))

(test width-mixed-string-aggregates-grapheme-rules
  (let ((text (concatenate 'string "A" "e" (string-from-codepoints #x0301) "界" "B")))
    (is (= 5 (ptui.text.width:string-width text)))
    (is (= 5 (ptui.text.width:string-width text :engine :fallback)))))

(test width-predicate-mappings
  (is (ptui.text.width:east-asian-wide-codepoint-p (char-code #\界)))
  (is (not (ptui.text.width:east-asian-wide-codepoint-p (char-code #\A))))
  (is (ptui.text.width:combining-codepoint-p (char-code #\́)))
  (is (not (ptui.text.width:combining-codepoint-p (char-code #\A)))))

;;; ===================================================================
;;; I308 grapheme segmentation
;;; ===================================================================

(test grapheme-splits-combining-sequences
  (let ((cluster (concatenate 'string "e" (string-from-codepoints #x0301))))
    (is (equal (ptui.text.grapheme:split-graphemes cluster :engine :fallback)
               (list cluster)))
    (is (= 1 (length (ptui.text.grapheme:split-graphemes cluster :engine :fallback))))))

(test grapheme-splits-regional-indicators
  (let* ((flag (string-from-codepoints #x1F1FA #x1F1F8))
         (flags (concatenate 'string flag flag)))
    (is (equal (ptui.text.grapheme:split-graphemes flag :engine :fallback)
               (list flag)))
    (is (equal (ptui.text.grapheme:split-graphemes flags :engine :fallback)
               (list flag flag)))
    (is (= 2 (length (ptui.text.grapheme:split-graphemes flags :engine :fallback))))))

;;; ===================================================================
;;; I308 layout wrapping/truncation/edges
;;; ===================================================================

(test layout-wrap-respects-grapheme-clusters
  (let ((cluster (concatenate 'string "A" (string-from-codepoints #x0301))))
    (is (equal (ptui.text.layout:wrap-by-width
                (concatenate 'string "AA" #1=(string-from-codepoints #x754C #x754C) "BB")
                3
                :engine :fallback)
               (list "AA" "界" "BB")))
    (is (equal (ptui.text.layout:wrap-by-width
                (concatenate 'string "A" cluster "界")
                2
                :engine :fallback)
               (list cluster "界")))))

(test layout-truncates-with-ellipsis
  (let ((text (concatenate 'string "A" (string-from-codepoints #x754C) "Bcd"))
        (ellipsis (string-from-codepoints #x2026)))
    (is (string= (ptui.text.layout:truncate-to-width text 4 :ellipsis t :engine :fallback)
                 (concatenate 'string "A" (string-from-codepoints #x754C) ellipsis)))
    (is (= 4 (ptui.text.width:string-width
              (ptui.text.layout:truncate-to-width text 4 :ellipsis t :engine :fallback)
              :engine :fallback)))))

(test layout-edge-cases
  (is (equal (ptui.text.layout:wrap-by-width "" 4 :engine :fallback) '("")))
  (is (string= (ptui.text.layout:truncate-to-width "abcdef" 0 :engine :fallback) ""))
  (signals error (ptui.text.layout:wrap-by-width "abc" 0 :engine :fallback))
  (signals error (ptui.text.layout:truncate-to-width "abc" -1 :engine :fallback)))

(defun run-all ()
  (run! 'text-suite))
