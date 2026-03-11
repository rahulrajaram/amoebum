(in-package :pseudopod)

;;; ---------------------------------------------------------------------------
;;; Notebook tooling helpers (I343)
;;;
;;; Validates and normalizes Jupyter notebook JSON payloads so callers can
;;; perform notebook-aware edits while preserving cell structure and outputs
;;; policy.
;;; ---------------------------------------------------------------------------

(define-condition pseudopod-notebook-error (pseudopod-error)
  ()
  (:report (lambda (condition stream)
             (format stream "Notebook error: ~A"
                     (or (pseudopod-error-message condition)
                         "invalid notebook payload")))))

(defun %notebook-error (format-string &rest args)
  (error 'pseudopod-notebook-error
         :message (apply #'format nil format-string args)))

(defun %json-object-value (object key &optional default)
  (if (hash-table-p object)
      (multiple-value-bind (value present-p)
          (gethash key object)
        (if present-p value default))
      default))

(defun %sequence-elements (value)
  (cond
    ((null value) nil)
    ((stringp value) (list value))
    ((listp value) value)
    ((vectorp value) (loop for element across value collect element))
    ((typep value 'sequence) (coerce value 'list))
    (t nil)))

(defun %copy-json-value (value)
  (cond
    ((stringp value)
     (copy-seq value))
    ((hash-table-p value)
     (let ((copy (make-hash-table :test (hash-table-test value))))
       (maphash (lambda (key item)
                  (setf (gethash key copy) (%copy-json-value item)))
                value)
       copy))
    ((vectorp value)
     (let ((copy (make-array (length value))))
       (loop for idx from 0 below (length value) do
             (setf (aref copy idx) (%copy-json-value (aref value idx))))
       copy))
    ((listp value)
     (mapcar #'%copy-json-value value))
    (t
     value)))

(defun %parse-notebook-content (content label)
  (unless (stringp content)
    (%notebook-error "~A must be a JSON string." label))
  (handler-case
      (jonathan:parse content :as :hash-table)
    (error (condition)
      (%notebook-error "Unable to parse ~A JSON: ~A" label condition))))

(defun %validate-source-shape (source label index)
  (unless (or (stringp source)
              (vectorp source)
              (listp source))
    (%notebook-error
     "~A cell ~D has invalid source type ~S; expected string or sequence."
     label
     (1+ index)
     (type-of source))))

(defun %validate-notebook-shape (notebook label)
  (unless (hash-table-p notebook)
    (%notebook-error "~A must decode to a JSON object." label))
  (let ((cells (%json-object-value notebook "cells" :missing))
        (nbformat (%json-object-value notebook "nbformat" :missing)))
    (when (eq cells :missing)
      (%notebook-error "~A is missing required \"cells\" field." label))
    (unless (or (listp cells) (vectorp cells))
      (%notebook-error "~A field \"cells\" must be an array." label))
    (when (eq nbformat :missing)
      (%notebook-error "~A is missing required \"nbformat\" field." label))
    (unless (integerp nbformat)
      (%notebook-error "~A field \"nbformat\" must be an integer." label))
    (loop for cell in (%sequence-elements cells)
          for index from 0 do
            (unless (hash-table-p cell)
              (%notebook-error "~A cell ~D must be a JSON object."
                               label
                               (1+ index)))
            (let ((cell-type (%json-object-value cell "cell_type" :missing))
                  (source (%json-object-value cell "source" :missing)))
              (unless (and (stringp cell-type) (> (length cell-type) 0))
                (%notebook-error "~A cell ~D is missing required cell_type."
                                 label
                                 (1+ index)))
              (when (eq source :missing)
                (%notebook-error "~A cell ~D is missing required source."
                                 label
                                 (1+ index)))
              (%validate-source-shape source label index))))
  notebook)

(defun %normalize-output-policy (policy)
  (cond
    ((or (null policy)
         (eq policy :preserve)
         (and (stringp policy)
              (string-equal (string-trim '(#\Space #\Tab #\Newline #\Return) policy)
                            "preserve")))
     :preserve)
    ((or (eq policy :replace)
         (and (stringp policy)
              (string-equal (string-trim '(#\Space #\Tab #\Newline #\Return) policy)
                            "replace")))
     :replace)
    ((or (eq policy :strip)
         (and (stringp policy)
              (member (string-downcase
                       (string-trim '(#\Space #\Tab #\Newline #\Return) policy))
                      '("strip" "clear")
                      :test #'string=)))
     :strip)
    (t
     (%notebook-error
      "Unsupported notebook output policy ~S; expected preserve, replace, or strip."
      policy))))

(defun %code-cell-p (cell)
  (and (hash-table-p cell)
       (string= (or (%json-object-value cell "cell_type") "") "code")))

(defun %apply-output-policy (updated-notebook baseline-notebook policy)
  (let ((updated-cells (%sequence-elements (%json-object-value updated-notebook "cells")))
        (baseline-cells (and baseline-notebook
                             (%sequence-elements (%json-object-value baseline-notebook "cells")))))
    (ecase policy
      (:replace
       nil)
      (:strip
       (dolist (cell updated-cells)
         (when (%code-cell-p cell)
           (setf (gethash "outputs" cell) (make-array 0))
           (setf (gethash "execution_count" cell) nil))))
      (:preserve
       (loop for updated-cell in updated-cells
             for index from 0 do
               (let ((baseline-cell (and baseline-cells
                                         (< index (length baseline-cells))
                                         (nth index baseline-cells))))
                 (when (and (%code-cell-p updated-cell)
                            (%code-cell-p baseline-cell))
                   (multiple-value-bind (baseline-outputs has-outputs-p)
                       (gethash "outputs" baseline-cell)
                     (when has-outputs-p
                       (setf (gethash "outputs" updated-cell)
                             (%copy-json-value baseline-outputs))))
                   (multiple-value-bind (baseline-count has-count-p)
                       (gethash "execution_count" baseline-cell)
                     (when has-count-p
                       (setf (gethash "execution_count" updated-cell)
                             baseline-count))))))))))

(defun prepare-notebook-content (updated-content
                                 &key baseline-content (output-policy :preserve))
  "Validate UPDATED-CONTENT as notebook JSON and apply OUTPUT-POLICY.

Returns two values:
1. Canonical JSON notebook string.
2. Metadata plist containing :output-policy, :cell-count, and :nbformat.

OUTPUT-POLICY supports:
- :PRESERVE (default): carry forward baseline outputs by cell index for code cells.
- :REPLACE: keep outputs present in UPDATED-CONTENT.
- :STRIP: clear all code-cell outputs and execution_count fields."
  (let* ((policy (%normalize-output-policy output-policy))
         (updated-notebook (%parse-notebook-content updated-content "updated notebook"))
         (baseline-notebook (when (and baseline-content
                                      (plusp (length (string-trim
                                                      '(#\Space #\Tab #\Newline #\Return)
                                                      baseline-content))))
                             (%parse-notebook-content baseline-content "baseline notebook"))))
    (%validate-notebook-shape updated-notebook "updated notebook")
    (when baseline-notebook
      (%validate-notebook-shape baseline-notebook "baseline notebook"))
    (%apply-output-policy updated-notebook baseline-notebook policy)
    (values (jonathan:to-json updated-notebook)
            (list :output-policy policy
                  :cell-count (length (%sequence-elements
                                       (%json-object-value updated-notebook "cells")))
                  :nbformat (%json-object-value updated-notebook "nbformat")))))
