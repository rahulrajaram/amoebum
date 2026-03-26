(in-package :amoebum)

(defun %heap-capture-room ()
  "Run (room t) and capture its output as a string."
  (with-output-to-string (*standard-output*)
    (room t)))

(defun %heap-capture-instance-usage (&key (top-n 30))
  "Run sb-vm::instance-usage and capture output."
  (with-output-to-string (*standard-output*)
    (sb-vm::instance-usage :dynamic :top-n top-n)))

(defun %heap-handler (_invocation arguments _context)
  (declare (ignore _invocation _context))
  (let* ((args (or (gethash :ARGS arguments) ""))
         (top-n (or (ignore-errors
                      (parse-integer (string-trim '(#\Space #\Tab) args)))
                    30)))
    (sb-ext:gc :full t)
    (let ((room-output (%heap-capture-room))
          (usage-output (%heap-capture-instance-usage :top-n top-n)))
      (make-slash-command-result
       :output (format nil "~
## Heap Snapshot (after full GC)

### Memory Summary
```
~A
```

### Top ~D Instance Types (dynamic space)
```
~A
```"
                       room-output top-n usage-output)
       :echo-input-p t))))
