(in-package :amoebum/test)

(def-suite repo-surface-suite
  :description "Repo-surface regressions for removed stale integrations."
  :in amoebum-suite)

(in-suite repo-surface-suite)

(defun %repo-root ()
  (truename (merge-pathnames #P"../"
                             (asdf:system-source-directory :amoebum))))

(defun %read-file-text (path)
  (uiop:read-file-string path))

(test removed-watch-nudge-surface-stays-gone
  (let* ((repo-root (or (%repo-root)
                        (error "Unable to resolve repo root from test file.")))
         (readme-path (merge-pathnames #P"README.md" repo-root))
         (nudge-script-path (merge-pathnames #P"bin/nudge-idle-backlog-check.sh" repo-root))
         (readme-text (%read-file-text readme-path)))
    (is-false (search "watch_nudge_http_server.py" readme-text :test #'char-equal))
    (is-false (search "watch_nudge_server/static/" readme-text :test #'char-equal))
    (is-false (search "/api/nudges" readme-text :test #'char-equal))
    (is-false (probe-file nudge-script-path))))

(test repo-surface-smoke-sentinel
  (is-true t))
