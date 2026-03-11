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
  "Simulate a tool call then emit text.
Emits only tool-call-started (skips argument-complete) to avoid entering the
real tool-execution pipeline, which blocks on permission approval in supervised
mode.  A post-text sleep keeps the stream open so the TOOL> preview line
remains visible long enough for test captures."
  (let ((tool-call (pseudopod:make-tool-call
                    :id "demo-tc-001"
                    :name "read-file"
                    :arguments "{\"path\": \"/tmp/demo.txt\", \"encoding\": \"utf-8\"}")))
    (token-stream-emit-tool-call-started stream-state tool-call)
    (sleep 0.3))
  (%demo-emit-text stream-state
                   (format nil "~%~%I read the file. Here are the contents:~%~%~
```~%Hello from demo mode!~%```~%~%~
The tool call above was simulated — no file was actually read."))
  ;; Keep stream alive so TOOL> [streaming] line stays visible for captures.
  (sleep 5))

(defun %demo-emit-stress-tool-calls (stream-state)
  "Simulate a realistic multi-tool flow: reasoning text, then tool calls with
streamed argument deltas, mimicking what a real LLM conversation looks like."
  ;; Phase 1: Reasoning text (what the model 'thinks' before tool calls)
  (%demo-emit-text stream-state
    (format nil "Let me explore the project structure to understand what's going on.~%~%~
I'll start by checking the directory layout and then read key files."))
  (sleep 0.3)
  ;; Phase 2: First tool call — bash with long command, streamed as deltas
  (let* ((tc1 (pseudopod:make-tool-call
               :id "stress-tc-001"
               :name "bash-exec"
               :arguments ""))
         (full-args "{\"command\": \"find . -maxdepth 2 -type f -name \\\"*.md\\\" -o -name \\\"*.json\\\" -o -name \\\"*.toml\\\" -o -name \\\"*.yaml\\\"\", \"timeout\": 30}"))
    (token-stream-emit-tool-call-started stream-state tc1)
    ;; Stream arguments in chunks to simulate delta events
    (loop for i from 1 to (length full-args) do
      (let ((partial (subseq full-args 0 i)))
        (ptui.runtime.queue:queue-push
         (token-stream-state-events stream-state)
         (list :kind :tool-call-delta
               :type :tool-call-delta
               :tool-call (pseudopod:make-tool-call
                           :id "stress-tc-001"
                           :name "bash-exec"
                           :arguments partial)
               :tool-name "bash-exec"
               :arguments partial
               :index 0))
        (when (zerop (mod i 8))
          (sleep *demo-chunk-delay-seconds*)))))
  (sleep 0.5)
  ;; Phase 3: Second tool call — read-file
  (let ((tc2 (pseudopod:make-tool-call
              :id "stress-tc-002"
              :name "read-file"
              :arguments "{\"path\": \"/home/rahul/Documents/amoebum/README.md\"}")))
    (token-stream-emit-tool-call-started stream-state tc2)
    (sleep 0.3))
  ;; Phase 4: More reasoning text after tool calls
  (%demo-emit-text stream-state
    (format nil "~%~%Based on the files I found, this is a Common Lisp project ~
with multiple subsystems. Let me summarize what I see."))
  ;; Keep stream alive for visual inspection
  (sleep 8))

(defun %demo-emit-approval-flow (stream-state)
  "Simulate a tool call that requires user approval, exercising the full
approval dialog UI without needing a real tool execution pipeline."
  (%demo-emit-text stream-state
    (format nil "Let me read that file for you."))
  (sleep 0.3)
  ;; Show the tool-call preview line
  (let ((tool-call (pseudopod:make-tool-call
                    :id "demo-approval-001"
                    :name "read-file"
                    :arguments "{\"path\": \"/etc/passwd\"}")))
    (token-stream-emit-tool-call-started stream-state tool-call))
  (sleep 0.2)
  ;; Directly trigger the approval dialog from a background thread,
  ;; simulating what the pipeline does when permission is :prompt.
  (let ((pa (wait-for-pending-approval
             "read-file"
             "{\"path\": \"/etc/passwd\"}"
             :path "/etc/passwd"
             :reason "Reads a sensitive system file"
             :decision-id "demo-approval-001")))
    (let ((decision (pending-approval-decision pa)))
      (case decision
        (:allow
         (%demo-emit-text stream-state
           (format nil "~%~%Tool approved! Here are the contents:~%~%~
```~%root:x:0:0:root:/root:/bin/bash~%nobody:x:65534:65534:nobody:/nonexistent:/usr/sbin/nologin~%```~%~%~
(This is simulated — the file was not actually read.)")))
        (otherwise
         (%demo-emit-text stream-state
           (format nil "~%~%Tool was denied by the user. ~
I'll proceed without reading that file.")))))))

(defun %demo-emit-multi-approval-flow (stream-state)
  "Simulate two sequential tool calls requiring approval."
  (%demo-emit-text stream-state
    (format nil "I need to check a couple of files."))
  (sleep 0.3)
  ;; First tool call
  (let ((tc1 (pseudopod:make-tool-call
              :id "demo-multi-001"
              :name "bash-exec"
              :arguments "{\"command\": \"cat /etc/hostname\"}")))
    (token-stream-emit-tool-call-started stream-state tc1))
  (sleep 0.2)
  (let ((pa1 (wait-for-pending-approval
              "bash-exec" "{\"command\": \"cat /etc/hostname\"}"
              :command "cat /etc/hostname"
              :reason "Runs a shell command"
              :decision-id "demo-multi-001")))
    (%demo-emit-text stream-state
      (format nil "~%~%First tool ~A."
              (if (eq (pending-approval-decision pa1) :allow) "approved" "denied"))))
  (sleep 0.5)
  ;; Second tool call
  (let ((tc2 (pseudopod:make-tool-call
              :id "demo-multi-002"
              :name "read-file"
              :arguments "{\"path\": \"/tmp/notes.txt\"}")))
    (token-stream-emit-tool-call-started stream-state tc2))
  (sleep 0.2)
  (let ((pa2 (wait-for-pending-approval
              "read-file" "{\"path\": \"/tmp/notes.txt\"}"
              :path "/tmp/notes.txt"
              :reason "Reads a file"
              :decision-id "demo-multi-002")))
    (%demo-emit-text stream-state
      (format nil "~%Second tool ~A. Done!"
              (if (eq (pending-approval-decision pa2) :allow) "approved" "denied")))))

(defun %demo-detect-response-type (prompt)
  "Return a keyword indicating which demo response to generate based on PROMPT."
  (let ((lower (string-downcase (or prompt ""))))
    (cond
      ((search "approve" lower) :approval)
      ((search "multi"   lower) :multi-approval)
      ((search "stress"  lower) :stress)
      ((search "tool"    lower) :tool)
      ((search "error"   lower) :error)
      ((search "long"    lower) :long)
      ((search "code"    lower) :code)
      (t                        :default))))

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
      (:approval
       (%demo-emit-approval-flow stream-state))
      (:multi-approval
       (%demo-emit-multi-approval-flow stream-state))
      (:stress
       (%demo-emit-stress-tool-calls stream-state))
      (:tool
       (%demo-emit-tool-call stream-state))
      (:long
       (%demo-emit-text stream-state (%demo-response-long)))
      (:code
       (%demo-emit-text stream-state (%demo-response-code)))
      (otherwise
       (%demo-emit-text stream-state (%demo-response-default))))))
