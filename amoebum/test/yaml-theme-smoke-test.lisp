;;; yaml-theme-smoke-test.lisp --- Smoke test for YAML theme loading

(in-package :cl-user)

(defun %yaml-theme-smoke-test-path ()
  "Resolve the bundled theme fixture independent of the caller's cwd."
  (asdf:system-relative-pathname "amoebum"
                                 "resources/themes/amoebum.tui-spec.yaml"))

(defun %yaml-theme-example-path ()
  "Resolve the example theme fixture independent of the caller's cwd."
  (asdf:system-relative-pathname "amoebum"
                                 "examples/amoebum-theme.yaml"))

(defun %yaml-theme-render-snapshot ()
  "Render an empty chat UI and return its serialized snapshot."
  (let* ((*default-pathname-defaults*
           (pathname "/home/rahul/Documents/amoebum/"))
         (amoebum::*current-config* nil)
         (event-bus (amoebum:make-event-bus :capacity 16))
         (state (amoebum.ui:make-chat-ui-state
                 :status-bar-state
                 (amoebum.ui:make-status-bar-state
                  :branch-name "yaml-theme-smoke"
                  :model-name "gpt-4o-mini"
                  :permission-mode :full-auto
                  :context-window-limit 128000
                  :event-bus event-bus)))
         (buffer (amoebum:render-chat-ui-buffer
                  state
                  (ptui.core.types:make-size 84 20))))
    (ptui.test-support.snapshot:buffer-to-snapshot buffer)))

(defun run-yaml-theme-smoke-test ()
  "Test that the YAML theme file loads correctly."
  (format t "~%=== YAML Theme Smoke Test ===~%")
  
  ;; Check theme file exists
  (let ((theme-path (%yaml-theme-smoke-test-path)))
    (format t "Checking theme file: ~A~%" theme-path)
    (assert (probe-file theme-path) ()
            "Theme file not found: ~A" theme-path)
    (format t "✓ Theme file exists~%"))
  
  ;; Check YAML is valid
  (format t "~%Validating YAML syntax...~%")
  (let ((content (uiop:read-file-string (%yaml-theme-smoke-test-path)))
        (yaml-data nil))
    (setf yaml-data (cl-yaml:parse content))
    (assert (hash-table-p yaml-data) ()
            "YAML did not parse to hash-table: ~A" (type-of yaml-data))
    (format t "✓ YAML parses successfully~%")
    (format t "  Keys found: ~A~%" 
            (loop for k being the hash-keys of yaml-data collect k)))
  
  ;; Check required sections
  (format t "~%Checking required sections...~%")
  (let* ((content (uiop:read-file-string (%yaml-theme-smoke-test-path)))
         (yaml-data (cl-yaml:parse content)))
    
    ;; Check metadata
    (let ((metadata (gethash "metadata" yaml-data)))
      (assert metadata () "Missing 'metadata' section")
      (format t "✓ Metadata section present~%")
      (format t "  name: ~A~%" (gethash "name" metadata))
      (format t "  version: ~A~%" (gethash "version" metadata)))
    
    ;; Check palette
    (let ((palette (gethash "palette" yaml-data)))
      (assert palette () "Missing 'palette' section")
      (assert (> (hash-table-count palette) 0) ()
              "Palette is empty")
      (format t "✓ Palette section present (~A colors)~%"
              (hash-table-count palette)))
    
    ;; Check roles
    (let ((roles (gethash "roles" yaml-data)))
      (assert roles () "Missing 'roles' section")
      (assert (> (hash-table-count roles) 0) ()
              "Roles is empty")
      (format t "✓ Roles section present (~A roles)~%"
              (hash-table-count roles))))

  ;; Check that amoebum actually loads the theme, not just parses the file
  (format t "~%Loading theme through amoebum...~%")
  (multiple-value-bind (ok theme-name &rest ignored)
      (amoebum.ui:load-yaml-theme :cli-path (%yaml-theme-smoke-test-path))
    (declare (ignore ignored))
    (assert ok () "amoebum.ui:load-yaml-theme failed: ~A" theme-name)
    (format t "✓ Theme loaded through amoebum (~A)~%" theme-name))

  ;; Check that the canonical YAML layout/behavior sections are actually consumed
  (let ((status-fn (find-symbol "YAML-THEME-STATUS-EXTENDED" :amoebum)))
    (when (and status-fn (fboundp status-fn))
      (let ((status (funcall status-fn)))
        (assert (getf status :layout-loaded-p) ()
                "Canonical YAML layout did not load: ~S" status)
        (assert (getf status :behavior-loaded-p) ()
                "Canonical YAML behavior did not load: ~S" status)
        (assert (plusp (or (getf status :layout-children-count) 0)) ()
                "Canonical YAML layout loaded with no children: ~S" status)
        (format t "✓ Layout and behavior sections loaded (~D panels)~%"
                (getf status :layout-children-count)))))

  ;; Check that the example layout changes the live renderer, not just parsing state.
  (format t "~%Loading example theme through amoebum...~%")
  (multiple-value-bind (ok theme-name &rest ignored)
      (amoebum.ui:load-yaml-theme :cli-path (%yaml-theme-example-path))
    (declare (ignore ignored))
    (assert ok () "Example amoebum theme failed to load: ~A" theme-name)
    (let ((snapshot (%yaml-theme-render-snapshot)))
      (assert (search "┌" snapshot) ()
              "Example YAML theme did not affect prompt-box border rendering:~%~A"
              snapshot)
      (format t "✓ Example layout affects live chat rendering (square prompt border)~%")))

  ;; Restore the canonical bundled theme so subsequent suites observe the
  ;; default Amoebum visual baseline rather than the example override.
  (multiple-value-bind (ok theme-name &rest ignored)
      (amoebum.ui:load-yaml-theme :cli-path (%yaml-theme-smoke-test-path))
    (declare (ignore ignored))
    (assert ok () "Failed to restore canonical YAML theme after example render: ~A"
            theme-name))
  
  (format t "~%=== All tests passed! ===~%")
  t)

;; Run the test when loaded
(run-yaml-theme-smoke-test)
