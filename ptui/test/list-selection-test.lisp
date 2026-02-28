(defpackage :ptui.test.list-selection
  (:use :cl :fiveam)
  (:export #:list-selection-suite))

(in-package :ptui.test.list-selection)

(def-suite list-selection-suite
  :description "PTUI list-selection shared logic tests.")

(in-suite list-selection-suite)

;;; --- clamp-index ---

(test clamp-index-empty-list
  (is (= 0 (ptui.components.list-selection:clamp-index 5 0))))

(test clamp-index-in-range
  (is (= 3 (ptui.components.list-selection:clamp-index 3 10))))

(test clamp-index-out-of-range
  (is (= 9 (ptui.components.list-selection:clamp-index 15 10))))

(test clamp-index-negative
  (is (= 0 (ptui.components.list-selection:clamp-index -3 10))))

;;; --- move-selection ---

(test move-selection-up
  (is (= 2 (ptui.components.list-selection:move-selection 3 10 :up))))

(test move-selection-up-at-zero
  (is (= 0 (ptui.components.list-selection:move-selection 0 10 :up))))

(test move-selection-down
  (is (= 4 (ptui.components.list-selection:move-selection 3 10 :down))))

(test move-selection-down-at-end
  (is (= 9 (ptui.components.list-selection:move-selection 9 10 :down))))

(test move-selection-home
  (is (= 0 (ptui.components.list-selection:move-selection 5 10 :home))))

(test move-selection-end
  (is (= 9 (ptui.components.list-selection:move-selection 0 10 :end))))

(test move-selection-empty-list
  (is (= 0 (ptui.components.list-selection:move-selection 0 0 :down))))

;;; --- visible-window ---

(test visible-window-items-less-than-visible-count
  (multiple-value-bind (visible start)
      (ptui.components.list-selection:visible-window '(a b c) 0 10)
    (is (equal '(a b c) visible))
    (is (= 0 start))))

(test visible-window-items-more-than-visible-count
  (let ((items (loop for i from 0 below 20 collect i)))
    (multiple-value-bind (visible start)
        (ptui.components.list-selection:visible-window items 15 5)
      (is (= 5 (length visible)))
      (is (<= start 15))
      (is (< 15 (+ start 5))))))

(test visible-window-selected-near-end
  (let ((items (loop for i from 0 below 20 collect i)))
    (multiple-value-bind (visible start)
        (ptui.components.list-selection:visible-window items 19 5)
      (is (= 5 (length visible)))
      (is (= 15 start)))))
