(defpackage :ptui.test.text-contract
  (:use :cl :fiveam)
  (:export #:run-all #:ptui-text-contract-suite))

(in-package :ptui.test.text-contract)

(def-suite ptui-text-contract-suite
  :description "PTUI foundational text contracts: grapheme, width, and layout.")

(in-suite ptui-text-contract-suite)

(defun string-from-codepoints (&rest codepoints)
  (coerce (mapcar #'code-char codepoints) 'string))

(test text-grapheme-map-and-do-graphemes-preserve-clusters
  (let* ((cluster (concatenate 'string "e" (string-from-codepoints #x0301)))
         (text (concatenate 'string "A" cluster))
         (mapped (ptui.text.grapheme:map-graphemes #'identity text :engine :fallback))
         (seen '()))
    (ptui.text.grapheme:do-graphemes (grapheme text :engine :fallback)
      (push grapheme seen))
    (is (equal '("A" nil) (list (first mapped) nil)) :test #'equal)
    (is (string= cluster (second mapped)))
    (is (equal mapped (nreverse seen)))))

(test text-width-handles-special-clusters
  (let ((flag (string-from-codepoints #x1F1FA #x1F1F8))
        (family (string-from-codepoints #x1F468 #x200D #x1F469 #x200D #x1F467 #x200D #x1F466))
        (keycap (string-from-codepoints #x0023 #xFE0F #x20E3)))
    (is (= 0 (ptui.text.width:grapheme-width (string #\Newline) :engine :fallback)))
    (is (= 2 (ptui.text.width:grapheme-width flag :engine :fallback)))
    (is (= 2 (ptui.text.width:grapheme-width family :engine :fallback)))
    (is (= 2 (ptui.text.width:grapheme-width keycap :engine :fallback)))))

(test text-width-remains-consistent-between-graphemes-and-strings
  (let* ((cluster (concatenate 'string "e" (string-from-codepoints #x0301)))
         (text (concatenate 'string "A" cluster (string-from-codepoints #x754C))))
    (is (= (ptui.text.width:string-width text :engine :fallback)
           (loop for grapheme in (ptui.text.grapheme:split-graphemes text :engine :fallback)
                 sum (ptui.text.width:grapheme-width grapheme :engine :fallback))))))

(test text-layout-width-safe-slice-respects-grapheme-boundaries
  (let* ((cluster (concatenate 'string "e" (string-from-codepoints #x0301)))
         (text (concatenate 'string "A" cluster "B")))
    (is (string= cluster
                 (ptui.text.layout:width-safe-slice text 1 2 :engine :fallback)))
    (is (string= (concatenate 'string cluster "B")
                 (ptui.text.layout:width-safe-slice text 1 3 :engine :fallback)))
    (is (string= ""
                 (ptui.text.layout:width-safe-slice text 1 1 :engine :fallback)))))

(test text-layout-wrap-preserve-spaces-controls-trailing-space-retention
  (is (equal '("A")
             (ptui.text.layout:wrap-by-width "A " 4 :engine :fallback)))
  (is (equal '("A ")
             (ptui.text.layout:wrap-by-width "A " 4
                                             :engine :fallback
                                             :preserve-spaces t))))

(test text-layout-wrap-handles-newlines-and-long-words
  (is (equal '("A" "B")
             (ptui.text.layout:wrap-by-width
              (concatenate 'string "A" (string #\Newline) "B")
              4
              :engine :fallback)))
  (is (equal '("AB" "CD" "E")
             (ptui.text.layout:wrap-by-width "ABCDE" 2 :engine :fallback))))

(test text-layout-truncate-slices-ellipsis-when-needed
  (is (string= "A…"
               (ptui.text.layout:truncate-to-width "ABCD" 2
                                                  :ellipsis t
                                                  :engine :fallback)))
  (is (string= ".."
               (ptui.text.layout:truncate-to-width "abcdef" 2
                                                  :ellipsis "..."
                                                  :engine :fallback))))

(test text-layout-width-safe-slice-rejects-inverted-ranges
  (signals error
    (ptui.text.layout:width-safe-slice "abc" 3 2 :engine :fallback)))

(defun run-all ()
  (run! 'ptui-text-contract-suite))
