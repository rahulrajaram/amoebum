(in-package :amoebum/test)

(def-suite notebook-edit-toolchain-suite
  :in amoebum-suite
  :description "I343 notebook-aware read/edit/write toolchain tests.")

(in-suite notebook-edit-toolchain-suite)

(defparameter +i343-baseline-notebook-json+
  "{\"cells\":[{\"cell_type\":\"markdown\",\"source\":[\"# Title\\n\",\"Notebook intro\\n\"]},{\"cell_type\":\"code\",\"source\":[\"print('hello')\\n\"],\"outputs\":[{\"output_type\":\"stream\",\"name\":\"stdout\",\"text\":[\"baseline output\\n\"]}],\"execution_count\":7}],\"metadata\":{\"language_info\":{\"name\":\"python\"}},\"nbformat\":4,\"nbformat_minor\":5}")

(defparameter +i343-updated-notebook-json+
  "{\"cells\":[{\"cell_type\":\"markdown\",\"source\":[\"# Title\\n\",\"Notebook intro\\n\"]},{\"cell_type\":\"code\",\"source\":[\"print('updated')\\n\"],\"outputs\":[{\"output_type\":\"stream\",\"name\":\"stdout\",\"text\":[\"updated output\\n\"]}],\"execution_count\":1}],\"metadata\":{\"language_info\":{\"name\":\"python\"}},\"nbformat\":4,\"nbformat_minor\":5}")

(defun %i343-tool-args (&rest key-values)
  (let ((args (make-hash-table :test #'equal)))
    (loop for (key value) on key-values by #'cddr do
          (setf (gethash key args) value))
    args))

(defun %i343-invoke-tool (tool-name &rest key-values)
  (let ((tool (pseudopod:find-tool amoebum:*toolset* tool-name)))
    (unless tool
      (error "Expected tool ~S to be registered." tool-name))
    (funcall (pseudopod:tool-definition-fn tool)
             (apply #'%i343-tool-args key-values))))

(defun %i343-parse-notebook-file (path)
  (jonathan:parse (uiop:read-file-string path :external-format :utf-8)
                  :as :hash-table))

(defun %i343-cell (notebook index)
  (let ((cells (gethash "cells" notebook)))
    (if (vectorp cells)
        (aref cells index)
        (nth index cells))))

(defun %i343-join-text (value)
  (cond
    ((stringp value) value)
    ((vectorp value)
     (with-output-to-string (stream)
       (loop for part across value do
             (write-string (%i343-join-text part) stream))))
    ((listp value)
     (with-output-to-string (stream)
       (dolist (part value)
         (write-string (%i343-join-text part) stream))))
    (t
     "")))

(defun %i343-cell-source-text (cell)
  (%i343-join-text (gethash "source" cell)))

(defun %i343-cell-output-text (cell)
  (let ((outputs (gethash "outputs" cell)))
    (cond
      ((and (vectorp outputs) (> (length outputs) 0))
       (%i343-join-text (gethash "text" (aref outputs 0))))
      ((and (listp outputs) outputs)
       (%i343-join-text (gethash "text" (first outputs))))
      (t
       ""))))

(test i343-read-file-renders-notebook-content
  (let* ((tmp-root (%make-temp-directory "amoebum-i343-read"))
         (config (amoebum:current-config))
         (old-mode (amoebum:config-permission-mode config))
         (old-rules amoebum:*permission-rules*)
         (path (merge-pathnames #P"sample.ipynb" tmp-root)))
    (unwind-protect
        (progn
          (setf amoebum:*permission-rules* nil)
          (amoebum:setconfig :permission-mode :full-auto)
          (%write-text-file path +i343-baseline-notebook-json+)
          (let ((result (%i343-invoke-tool "read-file" "path" (namestring path))))
            (is (stringp result))
            (is-true (search "## Cell 1 (markdown)" result :test #'char-equal))
            (is-true (search "```python" result :test #'char-equal))
            (is-true (search "baseline output" result :test #'char-equal))))
      (setf amoebum:*permission-rules* old-rules)
      (amoebum:setconfig :permission-mode old-mode)
      (%delete-directory-tree-safe tmp-root))))

(test i343-edit-file-preserves-notebook-validity-and-outputs-by-default
  (let* ((tmp-root (%make-temp-directory "amoebum-i343-edit"))
         (config (amoebum:current-config))
         (old-mode (amoebum:config-permission-mode config))
         (old-rules amoebum:*permission-rules*)
         (path (merge-pathnames #P"sample.ipynb" tmp-root)))
    (unwind-protect
        (progn
          (setf amoebum:*permission-rules* nil)
          (amoebum:setconfig :permission-mode :full-auto)
          (%write-text-file path +i343-baseline-notebook-json+)
          (%i343-invoke-tool "read-file" "path" (namestring path))
          (let ((result (%i343-invoke-tool "edit-file"
                                           "path" (namestring path)
                                           "old-string" "print('hello')"
                                           "new-string" "print('updated')")))
            (let ((notebook-meta (getf result :notebook)))
              (is-true notebook-meta)
              (is (eq :preserve (getf notebook-meta :output-policy)))))
          (let* ((notebook (%i343-parse-notebook-file path))
                 (code-cell (%i343-cell notebook 1)))
            (is (search "print('updated')" (%i343-cell-source-text code-cell)))
            (is (string= (format nil "baseline output~%")
                         (%i343-cell-output-text code-cell)))
            (is (= 7 (or (gethash "execution_count" code-cell) -1)))))
      (setf amoebum:*permission-rules* old-rules)
      (amoebum:setconfig :permission-mode old-mode)
      (%delete-directory-tree-safe tmp-root))))

(test i343-write-file-strip-policy-clears-notebook-outputs
  (let* ((tmp-root (%make-temp-directory "amoebum-i343-write"))
         (config (amoebum:current-config))
         (old-mode (amoebum:config-permission-mode config))
         (old-rules amoebum:*permission-rules*)
         (path (merge-pathnames #P"sample.ipynb" tmp-root)))
    (unwind-protect
        (progn
          (setf amoebum:*permission-rules* nil)
          (amoebum:setconfig :permission-mode :full-auto)
          (%write-text-file path +i343-baseline-notebook-json+)
          (%i343-invoke-tool "read-file" "path" (namestring path))
          (let ((result (%i343-invoke-tool "write-file"
                                           "path" (namestring path)
                                           "content" +i343-updated-notebook-json+
                                           "notebook-output-policy" "strip")))
            (let ((notebook-meta (getf result :notebook)))
              (is-true notebook-meta)
              (is (eq :strip (getf notebook-meta :output-policy)))))
          (let* ((notebook (%i343-parse-notebook-file path))
                 (code-cell (%i343-cell notebook 1))
                 (outputs (gethash "outputs" code-cell)))
            (is (search "print('updated')" (%i343-cell-source-text code-cell)))
            (is (or (null outputs)
                    (and (vectorp outputs) (= 0 (length outputs)))
                    (and (listp outputs) (null outputs))))
            (is (null (gethash "execution_count" code-cell)))))
      (setf amoebum:*permission-rules* old-rules)
      (amoebum:setconfig :permission-mode old-mode)
      (%delete-directory-tree-safe tmp-root))))
