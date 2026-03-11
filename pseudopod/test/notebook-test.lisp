(in-package :pseudopod/test)

;;; ---- I343 Tests: Notebook normalization and output policy ----

(defun %i343-parse-notebook (payload)
  (jonathan:parse payload :as :hash-table))

(defun %i343-cell (notebook index)
  (let ((cells (gethash "cells" notebook)))
    (if (vectorp cells)
        (aref cells index)
        (nth index cells))))

(defun %i343-cell-output-text (cell)
  (let* ((outputs (gethash "outputs" cell))
         (output (cond
                   ((and (vectorp outputs) (> (length outputs) 0))
                    (aref outputs 0))
                   ((and (listp outputs) outputs)
                    (first outputs))
                   (t
                    nil)))
         (text (and (hash-table-p output) (gethash "text" output))))
    (cond
      ((and (vectorp text) (> (length text) 0))
       (aref text 0))
      ((and (listp text) text)
       (first text))
      ((stringp text)
       text)
      (t
       ""))))

(test notebook-preserve-policy-keeps-baseline-outputs
  (let* ((baseline
           "{\"cells\":[{\"cell_type\":\"code\",\"source\":[\"print('old')\\n\"],\"outputs\":[{\"output_type\":\"stream\",\"name\":\"stdout\",\"text\":[\"baseline\\n\"]}],\"execution_count\":9}],\"metadata\":{},\"nbformat\":4,\"nbformat_minor\":5}")
         (updated
           "{\"cells\":[{\"cell_type\":\"code\",\"source\":[\"print('new')\\n\"],\"outputs\":[{\"output_type\":\"stream\",\"name\":\"stdout\",\"text\":[\"updated\\n\"]}],\"execution_count\":1}],\"metadata\":{},\"nbformat\":4,\"nbformat_minor\":5}"))
    (multiple-value-bind (normalized metadata)
        (pseudopod:prepare-notebook-content updated
                                            :baseline-content baseline
                                            :output-policy :preserve)
      (let* ((notebook (%i343-parse-notebook normalized))
             (cell (%i343-cell notebook 0)))
        (is (eq :preserve (getf metadata :output-policy)))
        (is (= 1 (getf metadata :cell-count)))
        (is (string= (format nil "baseline~%")
                     (or (%i343-cell-output-text cell) "")))
        (is (= 9 (or (gethash "execution_count" cell) -1)))))))

(test notebook-strip-policy-clears-code-cell-outputs
  (let ((updated
          "{\"cells\":[{\"cell_type\":\"code\",\"source\":[\"print('x')\\n\"],\"outputs\":[{\"output_type\":\"stream\",\"name\":\"stdout\",\"text\":[\"x\\n\"]}],\"execution_count\":3}],\"metadata\":{},\"nbformat\":4,\"nbformat_minor\":5}"))
    (multiple-value-bind (normalized metadata)
        (pseudopod:prepare-notebook-content updated :output-policy :strip)
      (let* ((notebook (%i343-parse-notebook normalized))
             (cell (%i343-cell notebook 0))
             (outputs (gethash "outputs" cell)))
        (is (eq :strip (getf metadata :output-policy)))
        (is (or (null outputs)
                (and (vectorp outputs) (= 0 (length outputs)))
                (and (listp outputs) (null outputs))))
        (is (null (gethash "execution_count" cell)))))))

(test notebook-replace-policy-retains-updated-outputs
  (let ((updated
          "{\"cells\":[{\"cell_type\":\"code\",\"source\":[\"print('x')\\n\"],\"outputs\":[{\"output_type\":\"stream\",\"name\":\"stdout\",\"text\":[\"updated\\n\"]}],\"execution_count\":12}],\"metadata\":{},\"nbformat\":4,\"nbformat_minor\":5}"))
    (multiple-value-bind (normalized metadata)
        (pseudopod:prepare-notebook-content updated :output-policy "replace")
      (let* ((notebook (%i343-parse-notebook normalized))
             (cell (%i343-cell notebook 0)))
        (is (eq :replace (getf metadata :output-policy)))
        (is (string= (format nil "updated~%")
                     (or (%i343-cell-output-text cell) "")))
        (is (= 12 (or (gethash "execution_count" cell) -1)))))))

(test notebook-invalid-json-signals-notebook-error
  (signals pseudopod:pseudopod-notebook-error
    (pseudopod:prepare-notebook-content "{not-json")))
