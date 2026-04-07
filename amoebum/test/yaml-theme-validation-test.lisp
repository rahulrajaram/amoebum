(in-package :amoebum/test)

(def-suite yaml-theme-validation-suite :in amoebum-suite
  :description "Malformed YAML theme fixtures should fail before runtime load.")

(in-suite yaml-theme-validation-suite)

(defmacro with-isolated-yaml-theme-state (&body body)
  `(let ((old-loaded-p amoebum::*yaml-theme-loaded-p*)
         (old-source-path amoebum::*yaml-theme-source-path*)
         (old-last-modified amoebum::*yaml-theme-last-modified*)
         (old-layout amoebum::*yaml-layout-loaded*)
         (old-behavior amoebum::*yaml-behavior-loaded*)
         (old-active-theme ptui.core.theme:*active-theme*))
     (unwind-protect
          (progn
            (setf amoebum::*yaml-theme-loaded-p* nil
                  amoebum::*yaml-theme-source-path* nil
                  amoebum::*yaml-theme-last-modified* nil
                  amoebum::*yaml-layout-loaded* nil
                  amoebum::*yaml-behavior-loaded* nil)
            ,@body)
       (setf amoebum::*yaml-theme-loaded-p* old-loaded-p
             amoebum::*yaml-theme-source-path* old-source-path
             amoebum::*yaml-theme-last-modified* old-last-modified
             amoebum::*yaml-layout-loaded* old-layout
             amoebum::*yaml-behavior-loaded* old-behavior
             ptui.core.theme:*active-theme* old-active-theme))))

(defmacro with-temp-yaml-theme ((path-var content) &body body)
  `(let* ((tmp-root (%make-temp-directory "amoebum-yaml-theme-validation"))
          (,path-var (merge-pathnames #P"invalid-theme.tui-spec.yaml" tmp-root)))
     (unwind-protect
          (progn
            (%write-text-file ,path-var ,content)
            ,@body)
       (%delete-directory-tree-safe tmp-root))))

(test yaml-theme-loader-rejects-invalid-palette-entry
  (let ((*default-pathname-defaults*
          (pathname "/home/rahul/Documents/amoebum/"))
        (amoebum::*current-config* nil))
    (with-isolated-yaml-theme-state
      (with-temp-yaml-theme
          (theme-path
           (format nil
"metadata:
  name: invalid-palette
palette:
  bg: \"16 18 24\"
  text: \"220 226 236\"
  amber: \"not-a-color\"
roles:
  user-label:
    fg: text
layout:
  direction: column
  children:
    - name: history
      height: fill
    - name: prompt
      height: 3
      focusable: true
      focus-order: 1
    - name: status
      height: 1
"))
        (multiple-value-bind (ok error-message)
            (amoebum.ui:load-yaml-theme :cli-path theme-path)
          (is-false ok)
          (is (search "Invalid palette color"
                      (princ-to-string error-message)
                      :test #'char-equal))
          (is-false amoebum::*yaml-theme-loaded-p*))))))

(test yaml-theme-loader-rejects-unknown-role-color-reference
  (let ((*default-pathname-defaults*
          (pathname "/home/rahul/Documents/amoebum/"))
        (amoebum::*current-config* nil))
    (with-isolated-yaml-theme-state
      (with-temp-yaml-theme
          (theme-path
           (format nil
"metadata:
  name: invalid-role-ref
palette:
  bg: \"16 18 24\"
  text: \"220 226 236\"
roles:
  user-label:
    fg: missing-accent
layout:
  direction: column
  children:
    - name: history
      height: fill
    - name: prompt
      height: 3
      focusable: true
      focus-order: 1
    - name: status
      height: 1
"))
        (multiple-value-bind (ok error-message)
            (amoebum.ui:load-yaml-theme :cli-path theme-path)
          (is-false ok)
          (is (search "Unknown role color reference"
                      (princ-to-string error-message)
                      :test #'char-equal))
          (is-false amoebum::*yaml-theme-loaded-p*))))))

(test yaml-theme-loader-rejects-duplicate-focus-order
  (let ((*default-pathname-defaults*
          (pathname "/home/rahul/Documents/amoebum/"))
        (amoebum::*current-config* nil))
    (with-isolated-yaml-theme-state
      (with-temp-yaml-theme
          (theme-path
           (format nil
"metadata:
  name: duplicate-focus
palette:
  bg: \"16 18 24\"
  text: \"220 226 236\"
roles:
  user-label:
    fg: text
layout:
  direction: column
  children:
    - name: history
      height: fill
    - name: prompt
      height: 3
      focusable: true
      focus-order: 1
    - name: status
      height: 1
      focus-order: 1
"))
        (multiple-value-bind (ok error-message)
            (amoebum.ui:load-yaml-theme :cli-path theme-path)
          (is-false ok)
          (is (search "Duplicate focus-order"
                      (princ-to-string error-message)
                      :test #'char-equal))
          (is-false amoebum::*yaml-theme-loaded-p*))))))
