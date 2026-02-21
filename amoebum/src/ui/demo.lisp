(in-package :amoebum)

;;; Demo stream runner — simulates LLM responses for TUI testing
;;; without requiring an API key or network connection.

(defparameter *demo-chunk-delay-seconds* 0.02
  "Delay in seconds between emitting chunks in demo mode.")

(defun %demo-emit-text (stream-state text)
  "Emit TEXT as individual word-chunks with realistic streaming delays."
  (let ((words (cl-ppcre:split "( )" text :with-registers-p t)))
    (dolist (word words)
      (token-stream-emit-chunk stream-state word)
      (sleep *demo-chunk-delay-seconds*))))

(defun %demo-response-default ()
  "A multi-paragraph markdown response exercising various formatting."
  (format nil "~
## Demo Response

This is a **demo mode** response. No API key is required — everything ~
you see is generated locally for TUI testing purposes.

### Features Being Tested

- **Bold text** and *italic text* rendering
- Inline `code spans` within prose
- Nested list items with varying content lengths
- Paragraph wrapping across terminal widths

> This is a blockquote. It should be visually distinct from ~
> regular paragraph text and properly indented.

Here is a numbered list:

1. First item with some detail
2. Second item — slightly longer to test wrapping behavior
3. Third item

That concludes the default demo response. Try typing **tool**, **code**, ~
**long**, or **error** to see other response types."))

(defun %demo-response-code ()
  "A response containing fenced code blocks."
  (concatenate 'string
    "Here's an example of a Common Lisp function:" (string #\Newline)
    (string #\Newline)
    "```lisp" (string #\Newline)
    "(defun fibonacci (n)" (string #\Newline)
    "  \"Return the Nth Fibonacci number.\"" (string #\Newline)
    "  (if (<= n 1)" (string #\Newline)
    "      n" (string #\Newline)
    "      (+ (fibonacci (- n 1))" (string #\Newline)
    "         (fibonacci (- n 2)))))" (string #\Newline)
    "```" (string #\Newline)
    (string #\Newline)
    "And a shell command:" (string #\Newline)
    (string #\Newline)
    "```bash" (string #\Newline)
    "sbcl --eval '(format t \"Hello ~A~%\" (lisp-implementation-version))' --quit" (string #\Newline)
    "```" (string #\Newline)
    (string #\Newline)
    "Code blocks should preserve indentation and use a monospace font with "
    "a distinct background."))

(defun %demo-response-long ()
  "A very long response to test scrolling behavior."
  (with-output-to-string (out)
    (format out "# Long Response Test~%~%")
    (format out "This response is intentionally long to test scroll behavior.~%~%")
    (dotimes (i 20)
      (format out "## Section ~D~%~%" (1+ i))
      (format out "Lorem ipsum dolor sit amet, consectetur adipiscing elit. ~
Sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. ~
Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris ~
nisi ut aliquip ex ea commodo consequat.~%~%")
      (format out "- Item A in section ~D~%" (1+ i))
      (format out "- Item B in section ~D~%" (1+ i))
      (format out "- Item C in section ~D~%~%" (1+ i)))))

(defun %demo-emit-tool-call (stream-state)
  "Simulate a tool call (started + argument-complete) then emit text."
  (let ((tool-call (pseudopod:make-tool-call
                    :id "demo-tc-001"
                    :name "read_file"
                    :arguments "{\"path\": \"/tmp/demo.txt\"}")))
    (token-stream-emit-tool-call-started stream-state tool-call)
    (sleep 0.3)
    (token-stream-emit-tool-call-argument-complete stream-state tool-call)
    (sleep 0.2))
  (%demo-emit-text stream-state
                   (format nil "~%~%I read the file. Here are the contents:~%~%~
```~%Hello from demo mode!~%```~%~%~
The tool call above was simulated — no file was actually read.")))

(defun %demo-detect-response-type (prompt)
  "Return a keyword indicating which demo response to generate based on PROMPT."
  (let ((lower (string-downcase (or prompt ""))))
    (cond
      ((search "tool"  lower) :tool)
      ((search "error" lower) :error)
      ((search "long"  lower) :long)
      ((search "code"  lower) :code)
      (t                      :default))))

(defun demo-stream-runner (stream-state prompt messages
                           &key system-prompt client tools)
  "Mock stream runner for demo mode. Examines PROMPT for keywords to choose
a canned response type, then streams chunks with small delays."
  (declare (ignore messages system-prompt client tools))
  (let ((response-type (%demo-detect-response-type prompt)))
    (case response-type
      (:error
       (sleep 0.5)
       (token-stream-mark-failed stream-state
                                 (make-condition 'simple-error
                                                 :format-control "Demo error: simulated stream failure"
                                                 :format-arguments nil))
       (return-from demo-stream-runner nil))
      (:tool
       (%demo-emit-tool-call stream-state))
      (:long
       (%demo-emit-text stream-state (%demo-response-long)))
      (:code
       (%demo-emit-text stream-state (%demo-response-code)))
      (otherwise
       (%demo-emit-text stream-state (%demo-response-default))))))
