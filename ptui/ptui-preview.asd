(asdf:defsystem "ptui-preview"
  :description "PTUI live-reloading YAML TUI preview tool"
  :author "Ralph"
  :license "MIT"
  :depends-on ("ptui" "cl-yaml")
  :serial t
  :components
  ((:file "src/preview/sample-content")
   (:file "src/preview/yaml-translator")
   (:file "src/preview/preview-app")))
