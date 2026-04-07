(defpackage :ptui.test.lower-dispatch
  (:use :cl :fiveam)
  (:export #:run-all #:lower-dispatch-suite))

(in-package :ptui.test.lower-dispatch)

(def-suite lower-dispatch-suite
  :description "Lower PTUI layer dispatch table tests (FP-Refine Phase 3b).")

(in-suite lower-dispatch-suite)

;;; ====================================================================
;;; Signal number mapping table (ptui.term.signals)
;;; ====================================================================

(test signo-table-has-three-entries
  (is (= 3 (length ptui.term.signals::+signo-keyword-map+))))

(test signo-table-maps-2-to-int
  (is (eq :int (cdr (assoc 2 ptui.term.signals::+signo-keyword-map+ :test #'=)))))

(test signo-table-maps-15-to-term
  (is (eq :term (cdr (assoc 15 ptui.term.signals::+signo-keyword-map+ :test #'=)))))

(test signo-table-maps-28-to-winch
  (is (eq :winch (cdr (assoc 28 ptui.term.signals::+signo-keyword-map+ :test #'=)))))

(test signo-to-keyword-known-signals
  (is (eq :int (ptui.term.signals::%signo->keyword 2)))
  (is (eq :term (ptui.term.signals::%signo->keyword 15)))
  (is (eq :winch (ptui.term.signals::%signo->keyword 28))))

(test signo-to-keyword-unknown-falls-back
  (let ((result (ptui.term.signals::%signo->keyword 99)))
    (is (keywordp result))
    (is (string= "SIG-99" (symbol-name result)))))

;;; ====================================================================
;;; Event-filter field accessor table (ptui.runtime.event-filters)
;;; ====================================================================

(test event-field-accessor-table-has-three-entries
  (is (= 3 (length ptui.runtime.event-filters::+event-field-accessor-names+))))

(test event-field-accessor-table-maps-type
  (is (string= "EVENT-TYPE"
               (cdr (assoc :type ptui.runtime.event-filters::+event-field-accessor-names+
                           :test #'eq)))))

(test event-field-accessor-table-maps-severity
  (is (string= "EVENT-SEVERITY"
               (cdr (assoc :severity ptui.runtime.event-filters::+event-field-accessor-names+
                           :test #'eq)))))

(test event-field-accessor-table-maps-source
  (is (string= "EVENT-SOURCE"
               (cdr (assoc :source ptui.runtime.event-filters::+event-field-accessor-names+
                           :test #'eq)))))

;; The %event-field function should still resolve fields from plists
(test event-field-resolves-plist-type
  (is (eq :error (ptui.runtime.event-filters::%event-field
                  '(:type :error :severity :high) :type))))

(test event-field-resolves-plist-severity
  (is (eq :high (ptui.runtime.event-filters::%event-field
                 '(:type :error :severity :high) :severity))))

(test event-field-resolves-hash-table
  (let ((ht (make-hash-table :test 'eq)))
    (setf (gethash :type ht) :warning)
    (is (eq :warning (ptui.runtime.event-filters::%event-field ht :type)))))

;;; ====================================================================
;;; Test-backend draw-op table (ptui.backend.test)
;;; ====================================================================

(test test-backend-op-table-has-three-entries
  (is (= 3 (length ptui.backend.test::+test-backend-draw-op-handlers+))))

(test test-backend-op-table-entries-are-fboundp
  (dolist (entry ptui.backend.test::+test-backend-draw-op-handlers+)
    (is (fboundp (cdr entry))
        "Handler ~S should be fboundp." (cdr entry))))

(test test-backend-op-table-contains-expected-ops
  (let ((ops (mapcar #'car ptui.backend.test::+test-backend-draw-op-handlers+)))
    (dolist (k '(:draw-text :fill-rect :clear))
      (is (member k ops :test #'eq)
          "Expected ~S in test-backend op table." k))))

;; Integration: commit draw-ops through the table-dispatched backend
(test test-backend-commit-draws-text
  (let ((backend (ptui.backend.test:make-test-backend :cols 10 :rows 3)))
    (ptui.backend.protocol:backend-commit
     backend (list (list :draw-text 0 0 "ABC")))
    (let ((buf (ptui.backend.test:test-backend-buffer backend)))
      (is (string= "A" (ptui.core.types:cell-glyph
                         (svref (ptui.core.types:cell-buffer-cells buf) 0)))))))

(test test-backend-commit-clears
  (let ((backend (ptui.backend.test:make-test-backend :cols 5 :rows 2)))
    (ptui.backend.protocol:backend-commit
     backend (list (list :draw-text 0 0 "XY")))
    (ptui.backend.protocol:backend-commit
     backend (list (list :clear)))
    (let ((buf (ptui.backend.test:test-backend-buffer backend)))
      (is (string= " " (ptui.core.types:cell-glyph
                          (svref (ptui.core.types:cell-buffer-cells buf) 0)))))))

(test test-backend-commit-returns-op-count
  (let ((backend (ptui.backend.test:make-test-backend :cols 5 :rows 2)))
    (is (= 2 (ptui.backend.protocol:backend-commit
              backend (list (list :draw-text 0 0 "A")
                            (list :clear)))))))

;;; ====================================================================
;;; ncurses draw-op table structure (only when #+ptui-ncurses is active)
;;; ====================================================================

#+ptui-ncurses
(test ncurses-op-table-has-nine-entries
  (is (= 9 (length ptui.backend.ncurses::+ncurses-draw-op-handlers+))))

#+ptui-ncurses
(test ncurses-op-table-entries-are-fboundp
  (dolist (entry ptui.backend.ncurses::+ncurses-draw-op-handlers+)
    (is (fboundp (cdr entry))
        "ncurses handler ~S should be fboundp." (cdr entry))))

#+ptui-ncurses
(test ncurses-op-table-contains-expected-kinds
  (let ((kinds (mapcar #'car ptui.backend.ncurses::+ncurses-draw-op-handlers+)))
    (dolist (k '(:move :style :write :clear-eol :clear-screen
                 :hide-cursor :show-cursor :enter-alt :exit-alt))
      (is (member k kinds :test #'eq)
          "Expected ~S in ncurses draw-op table." k))))

#+ptui-ncurses
(test ncurses-alt-ops-map-to-noop
  (is (eq (cdr (assoc :enter-alt ptui.backend.ncurses::+ncurses-draw-op-handlers+ :test #'eq))
          (cdr (assoc :exit-alt ptui.backend.ncurses::+ncurses-draw-op-handlers+ :test #'eq)))))

(defun run-all ()
  (run! 'lower-dispatch-suite))
