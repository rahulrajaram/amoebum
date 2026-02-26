(in-package :amoebum/test)

;;; ============================================================
;;; I257: Hailer Sound Adapter Protocol — Smoke Tests
;;; ============================================================

(def-suite hailer-adapter-suite :in amoebum-suite)
(in-suite hailer-adapter-suite)

;;; --- Protocol completeness ---

(test sound-backend-protocol-generics-exist
  "All sound-backend protocol generics are defined."
  (is (fboundp 'amoebum:sound-play))
  (is (fboundp 'amoebum:sound-stop))
  (is (fboundp 'amoebum:sound-list-themes))
  (is (fboundp 'amoebum:sound-resolve-category))
  (is (fboundp 'amoebum:sound-backend-available-p))
  (is (fboundp 'amoebum:sound-backend-kind)))

;;; --- Builtin backend ---

(test builtin-backend-kind
  "Builtin backend reports :builtin kind."
  (let ((backend (make-instance 'amoebum:builtin-sound-backend)))
    (is (eq :builtin (amoebum:sound-backend-kind backend)))))

(test builtin-backend-list-themes
  "Builtin backend lists themes from the sound-theme registry."
  (let ((backend (make-instance 'amoebum:builtin-sound-backend)))
    (is (listp (amoebum:sound-list-themes backend)))))

(test builtin-backend-resolve-unmapped-category
  "Builtin backend returns NIL for an unmapped category."
  (let ((backend (make-instance 'amoebum:builtin-sound-backend)))
    (is (null (amoebum:sound-resolve-category
               backend :nonexistent-category-xyzzy)))))

(test builtin-backend-play-unmapped-returns-nil
  "Playing an unmapped category returns NIL."
  (let ((backend (make-instance 'amoebum:builtin-sound-backend)))
    (is (null (amoebum:sound-play backend :nonexistent-category-xyzzy)))))

;;; --- Hailer CLI backend (with mock runner) ---

(test hailer-cli-backend-kind
  "Hailer CLI backend reports :hailer-cli kind."
  (let ((backend (make-instance 'amoebum:hailer-cli-sound-backend)))
    (is (eq :hailer-cli (amoebum:sound-backend-kind backend)))))

(test hailer-cli-backend-available-check
  "Hailer CLI backend checks availability via --version."
  (let ((amoebum:*hailer-cli-runner*
          (lambda (args)
            (declare (ignore args))
            (list :exit-code 0 :stdout "hailer 1.0.0" :stderr "")))
        (backend (make-instance 'amoebum:hailer-cli-sound-backend)))
    (is (amoebum:sound-backend-available-p backend))))

(test hailer-cli-backend-unavailable
  "Hailer CLI backend reports unavailable when command fails."
  (let ((amoebum:*hailer-cli-runner*
          (lambda (args)
            (declare (ignore args))
            (list :exit-code 127 :stdout "" :stderr "not found")))
        (backend (make-instance 'amoebum:hailer-cli-sound-backend)))
    (is (not (amoebum:sound-backend-available-p backend)))))

(test hailer-cli-backend-list-themes-via-cli
  "Hailer CLI backend parses theme list from CLI output."
  (let ((amoebum:*hailer-cli-runner*
          (lambda (args)
            (if (equal (second args) "themes")
                (list :exit-code 0
                      :stdout (format nil "minimal~%standard~%verbose~%")
                      :stderr "")
                (list :exit-code 0 :stdout "hailer 1.0" :stderr ""))))
        (backend (make-instance 'amoebum:hailer-cli-sound-backend)))
    (let ((themes (amoebum:sound-list-themes backend)))
      (is (= 3 (length themes)))
      (is (member :minimal themes))
      (is (member :standard themes))
      (is (member :verbose themes)))))

(test hailer-cli-backend-resolve-silence
  "Hailer CLI backend recognizes 'silence' resolve response."
  (let ((amoebum:*hailer-cli-runner*
          (lambda (args)
            (if (equal (second args) "resolve")
                (list :exit-code 0 :stdout "silence" :stderr "")
                (list :exit-code 0 :stdout "hailer 1.0" :stderr ""))))
        (backend (make-instance 'amoebum:hailer-cli-sound-backend)))
    (is (eq :silence (amoebum:sound-resolve-category backend :keystroke)))))

(test hailer-cli-backend-play-delegates
  "Hailer CLI backend delegates play to CLI."
  (let* ((called-args nil)
         (amoebum:*hailer-cli-runner*
           (lambda (args)
             (when (equal (second args) "play")
               (setf called-args args))
             (list :exit-code 0 :stdout "" :stderr "")))
         (backend (make-instance 'amoebum:hailer-cli-sound-backend)))
    (amoebum:sound-play backend :keystroke)
    (is (not (null called-args)))
    (is (equal "play" (second called-args)))
    (is (equal "keystroke" (third called-args)))))

(test hailer-cli-backend-play-with-theme
  "Hailer CLI backend passes --theme flag."
  (let* ((called-args nil)
         (amoebum:*hailer-cli-runner*
           (lambda (args)
             (when (equal (second args) "play")
               (setf called-args args))
             (list :exit-code 0 :stdout "" :stderr "")))
         (backend (make-instance 'amoebum:hailer-cli-sound-backend)))
    (amoebum:sound-play backend :keystroke :theme-name :verbose)
    (is (member "--theme" called-args :test #'equal))
    (is (member "verbose" called-args :test #'equal))))

;;; --- Hailer MCP backend (stub) ---

(test hailer-mcp-backend-kind
  "Hailer MCP backend reports :hailer-mcp kind."
  (let ((backend (make-instance 'amoebum:hailer-mcp-sound-backend)))
    (is (eq :hailer-mcp (amoebum:sound-backend-kind backend)))))

(test hailer-mcp-backend-unavailable
  "Hailer MCP backend is always unavailable (stub)."
  (let ((backend (make-instance 'amoebum:hailer-mcp-sound-backend)))
    (is (not (amoebum:sound-backend-available-p backend)))))

;;; --- Backend selection ---

(test select-sound-backend-builtin
  "select-sound-backend :builtin returns builtin backend."
  (let ((backend (amoebum:select-sound-backend :backend :builtin)))
    (is (eq :builtin (amoebum:sound-backend-kind backend)))))

(test select-sound-backend-hailer-cli
  "select-sound-backend :hailer-cli returns hailer-cli backend."
  (let ((backend (amoebum:select-sound-backend :backend :hailer-cli)))
    (is (eq :hailer-cli (amoebum:sound-backend-kind backend)))))

(test select-sound-backend-hailer-mcp
  "select-sound-backend :hailer-mcp returns hailer-mcp backend."
  (let ((backend (amoebum:select-sound-backend :backend :hailer-mcp)))
    (is (eq :hailer-mcp (amoebum:sound-backend-kind backend)))))

;;; --- Convenience API ---

(test play-sound-convenience-api
  "play-sound function works through the backend."
  (let ((amoebum:*sound-backend-instance*
          (make-instance 'amoebum:builtin-sound-backend)))
    ;; Should not error, just return nil for unmapped category
    (is (null (amoebum:play-sound :nonexistent-category-xyzzy)))))

(test reset-sound-backend-clears-instance
  "reset-sound-backend clears the cached instance."
  (let ((amoebum:*sound-backend-instance*
          (make-instance 'amoebum:builtin-sound-backend)))
    (amoebum:reset-sound-backend)
    (is (null amoebum:*sound-backend-instance*))))
