#!/bin/bash
cd /home/rahul/Documents/amoebum/amoebum
echo "=== Theme Debug ==="
sbcl --noinform --eval '(load "/home/rahul/Documents/amoebum/ptui/.tools/quicklisp/setup.lisp")' \
     --eval '(push "/home/rahul/Documents/amoebum/amoebum/" asdf:*central-registry*)' \
     --eval '(push "/home/rahul/Documents/amoebum/ptui/" asdf:*central-registry*)' \
     --eval '(push "/home/rahul/Documents/amoebum/pseudopod/" asdf:*central-registry*)' \
     --eval '(ql:quickload :amoebum :silent t)' \
     --eval '(in-package :amoebum)' \
     --eval '(format t "~%=== BEFORE LOAD ===~%")' \
     --eval '(format t "YAML loaded-p: ~S~%" *yaml-theme-loaded-p*)' \
     --eval '(format t "Bundled path: ~S~%" *yaml-bundled-theme-path*)' \
     --eval '(format t "File exists: ~S~%" (probe-file *yaml-bundled-theme-path*))' \
     --eval '(format t "Active theme: ~S~%" ptui.core.theme:*active-theme*)' \
     --eval '(format t "~%=== LOADING YAML THEME ===~%")' \
     --eval '(multiple-value-bind (ok result) (load-yaml-theme :if-not-loaded t) (format t "Success: ~S, Result: ~S~%" ok result))' \
     --eval '(format t "~%=== AFTER LOAD ===~%")' \
     --eval '(format t "YAML loaded-p: ~S~%" *yaml-theme-loaded-p*)' \
     --eval '(format t "Source path: ~S~%" *yaml-theme-source-path*)' \
     --eval '(format t "Active theme: ~S~%" ptui.core.theme:*active-theme*)' \
     --quit 2>&1 | tail -30
