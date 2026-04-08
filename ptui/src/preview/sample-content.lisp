(defpackage :ptui.preview.sample-content
  (:use :cl)
  (:export #:make-sample-history-content
           #:make-sample-status-content
           #:make-sample-prompt-content))

(in-package :ptui.preview.sample-content)

(defun %preview-scroll-debug-log (fmt &rest args)
  (let ((pkg (find-package :ptui.preview.app)))
    (when pkg
      (multiple-value-bind (sym status)
          (find-symbol "%PREVIEW-SCROLL-DEBUG-LOG" pkg)
        (when (and sym (eq status :internal) (fboundp sym))
          (apply (symbol-function sym) fmt args))))))

;;; ===================================================================
;;; Rich Sample Content for YAML TUI Preview
;;; ===================================================================
;;;
;;; Generates a realistic conversation (markdown, code blocks, diffs,
;;; tool calls, errors) styled using the YAML's palette and roles so
;;; theme changes are immediately visible.

;;; --- Helpers ---

(defun %resolve-color (palette-entries name)
  "Resolve a palette color name to a color-rgb, or NIL."
  (let ((entry (assoc name palette-entries :test #'equal)))
    (when (and entry (cdr entry))
      (ptui.core.color:make-color-rgb
       (first (cdr entry)) (second (cdr entry)) (third (cdr entry))))))

(defun %role-cell (role-entries palette-entries role-name
                   &key fallback-fg fallback-bg)
  "Resolve a YAML role to a cell struct for styled-segments.
ROLE-NAME is a string matching a roles: key. Falls back to palette
colors or defaults."
  (let* ((role (find role-name role-entries :key #'first :test #'equal))
         (fg-rgb (when role (second role)))
         (bg-rgb (when role (third role)))
         (attrs-plist (when role (fourth role)))
         (fg (if fg-rgb
                 (ptui.core.color:make-color-rgb
                  (first fg-rgb) (second fg-rgb) (third fg-rgb))
                 (when fallback-fg
                   (%resolve-color palette-entries fallback-fg))))
         (bg (if bg-rgb
                 (ptui.core.color:make-color-rgb
                  (first bg-rgb) (second bg-rgb) (third bg-rgb))
                 (when fallback-bg
                   (%resolve-color palette-entries fallback-bg))))
         (attrs (if attrs-plist
                    (ptui.core.types:make-attrs
                     :boldp (getf attrs-plist :bold)
                     :dimp (getf attrs-plist :dim)
                     :italicp (getf attrs-plist :italic)
                     :underlinep (getf attrs-plist :underline)
                     :invertp (getf attrs-plist :inverse)
                     :strikep (getf attrs-plist :strike))
                    (ptui.core.types:make-attrs))))
    (ptui.core.types:make-cell " " fg bg attrs)))

(defun %seg (text cell)
  "Shorthand for a styled segment pair."
  (list text cell))

(defun %plain-cell (palette-entries &optional fg-name bg-name)
  "Make a plain cell from palette color names."
  (ptui.core.types:make-cell
   " "
   (when fg-name (%resolve-color palette-entries fg-name))
   (when bg-name (%resolve-color palette-entries bg-name))
   (ptui.core.types:make-attrs)))

(defun %bold-cell (palette-entries fg-name &optional bg-name)
  "Make a bold cell from palette color names."
  (ptui.core.types:make-cell
   " "
   (when fg-name (%resolve-color palette-entries fg-name))
   (when bg-name (%resolve-color palette-entries bg-name))
   (ptui.core.types:make-attrs :boldp t)))

(defun %dim-cell (palette-entries fg-name &optional bg-name)
  "Make a dim cell from palette color names."
  (ptui.core.types:make-cell
   " "
   (when fg-name (%resolve-color palette-entries fg-name))
   (when bg-name (%resolve-color palette-entries bg-name))
   (ptui.core.types:make-attrs :dimp t)))

(defun %text-line (segments)
  "Make a text-widget from styled segments."
  (let ((full-text (format nil "~{~A~}" (mapcar #'first segments))))
    (ptui.widgets.core:make-text-widget
     full-text
     :styled-segments segments)))

(defun %detect-prefix (segments)
  "Detect the leading whitespace prefix from the first segment.
Returns (prefix-string cell) for use on continuation lines, or NIL."
  (when segments
    (let* ((first-text (first (first segments)))
           (first-cell (second (first segments)))
           (prefix-len (position-if-not
                        (lambda (ch) (char= ch #\Space))
                        first-text)))
      (when (and prefix-len (> prefix-len 0))
        (list (make-string prefix-len :initial-element #\Space)
              first-cell)))))

(defun %wrap-segments (segments width)
  "Wrap styled segments into multiple lines, each fitting within WIDTH columns.
Continuation lines preserve the leading whitespace prefix of the original line.
Returns a list of segment-lists. Each segment-list represents one output line."
  (when (or (null segments) (null width))
    (return-from %wrap-segments (list segments)))
  (let* ((prefix-seg (%detect-prefix segments))
         (prefix-len (if prefix-seg (length (first prefix-seg)) 0))
         (lines '())
         (current-line '())
         (current-len 0))
    (dolist (seg segments)
      (let* ((text (first seg))
             (cell (second seg))
             (remaining text))
        (loop while (> (length remaining) 0)
              do (let ((budget (- width current-len)))
                   (cond
                     ;; Rest fits on current line
                     ((<= (length remaining) budget)
                      (push (%seg remaining cell) current-line)
                      (incf current-len (length remaining))
                      (setf remaining ""))
                     ;; Take what fits, start new line
                     ((> budget 0)
                      (push (%seg (subseq remaining 0 budget) cell) current-line)
                      (setf remaining (subseq remaining budget))
                      (push (nreverse current-line) lines)
                      ;; Start continuation with prefix indent
                      (if (and prefix-seg (> prefix-len 0))
                          (setf current-line (list (list (first prefix-seg)
                                                         (second prefix-seg)))
                                current-len prefix-len)
                          (setf current-line '()
                                current-len 0)))
                     ;; Current line already full, flush it
                     (t
                      (when current-line
                        (push (nreverse current-line) lines))
                      (if (and prefix-seg (> prefix-len 0))
                          (setf current-line (list (list (first prefix-seg)
                                                         (second prefix-seg)))
                                current-len prefix-len)
                          (setf current-line '()
                                current-len 0))))))))
    (when current-line
      (push (nreverse current-line) lines))
    (nreverse lines)))

(defun %blank-line (palette-entries)
  "An empty line."
  (ptui.widgets.core:make-text-widget
   ""
   :styled-segments (list (%seg " " (%plain-cell palette-entries)))))

;;; --- Conversation section builders ---

(defun %user-message-lines (palette-entries role-entries)
  "Build the user message: label + request text."
  (let ((label-cell (%role-cell role-entries palette-entries "user-label"))
        (text-cell (%role-cell role-entries palette-entries "user"
                               :fallback-fg "text")))
    (list
     (%text-line (list (%seg " [U]    " label-cell)
                       (%seg "Show me the git diff for the auth module" text-cell))))))

(defun %assistant-markdown-lines (palette-entries role-entries)
  "Build the assistant response with markdown formatting."
  (let ((label-cell (%role-cell role-entries palette-entries "assistant-label"))
        (text-cell (%role-cell role-entries palette-entries "assistant"
                               :fallback-fg "text"))
        (heading-cell (%role-cell role-entries palette-entries "heading"
                                  :fallback-fg "blue"))
        (code-cell (%role-cell role-entries palette-entries "inline-code"
                               :fallback-fg "text" :fallback-bg "code-bg"))
        (bold-text (ptui.core.types:make-cell
                    " "
                    (%resolve-color palette-entries "text")
                    nil
                    (ptui.core.types:make-attrs :boldp t)))
        (italic-text (ptui.core.types:make-cell
                      " "
                      (%resolve-color palette-entries "text")
                      nil
                      (ptui.core.types:make-attrs :italicp t))))
    (list
     (%text-line (list (%seg " [A]    " label-cell)
                       (%seg "## Changes in " heading-cell)
                       (%seg "auth.lisp" code-cell)))
     (%blank-line palette-entries)
     ;; Single long paragraph — wrapper will break at width
     (%text-line (list (%seg "        The authentication module was updated to support " text-cell)
                       (%seg "OAuth 2.0" bold-text)
                       (%seg " tokens alongside the existing " text-cell)
                       (%seg "API key" bold-text)
                       (%seg " mechanism." text-cell)))
     (%blank-line palette-entries)
     (%blank-line palette-entries)
     (%text-line (list (%seg "        " text-cell)
                       (%seg "### Key changes:" heading-cell)))
     (%text-line (list (%seg "        - Added " text-cell)
                       (%seg "oauth-provider" bold-text)
                       (%seg " dynamic variable for configuring the upstream identity provider" text-cell)))
     (%text-line (list (%seg "        - New function " text-cell)
                       (%seg "validate-oauth-token" bold-text)
                       (%seg " with " text-cell)
                       (%seg "PKCE" italic-text)
                       (%seg " support for secure authorization code exchange" text-cell)))
     (%text-line (list (%seg "        - Updated " text-cell)
                       (%seg "authenticate-request" bold-text)
                       (%seg " to check both OAuth and API key methods before rejecting" text-cell))))))

(defun %code-block-lines (palette-entries role-entries)
  "Build a Lisp code block with fence, syntax-highlighted body."
  (let ((fence-cell (%role-cell role-entries palette-entries "code-fence"
                                :fallback-fg "border"))
        (code-cell (%role-cell role-entries palette-entries "code-body"
                               :fallback-fg "text"))
        (kw-cell (%role-cell role-entries palette-entries "code-keyword"
                              :fallback-fg "rose"))
        (fn-cell (%role-cell role-entries palette-entries "code-function"
                              :fallback-fg "blue")))
    (list
     (%blank-line palette-entries)
     (%text-line (list (%seg "        " fence-cell)
                       (%seg "│ " fence-cell)
                       (%seg "```lisp" fence-cell)))
     ;; (defun validate-oauth-token (token &key (verify-pkce t))
     (%text-line (list (%seg "        " fence-cell)
                       (%seg "│ " fence-cell)
                       (%seg "(" code-cell)
                       (%seg "defun" kw-cell)
                       (%seg " " code-cell)
                       (%seg "validate-oauth-token" fn-cell)
                       (%seg " (token " code-cell)
                       (%seg "&key" kw-cell)
                       (%seg " (verify-pkce " code-cell)
                       (%seg "t" kw-cell)
                       (%seg "))" code-cell)))
     ;; (let ((claims (decode-jwt token)))
     (%text-line (list (%seg "        " fence-cell)
                       (%seg "│ " fence-cell)
                       (%seg "  (" code-cell)
                       (%seg "let" kw-cell)
                       (%seg " ((claims (decode-jwt token)))" code-cell)))
     ;; (when verify-pkce
     (%text-line (list (%seg "        " fence-cell)
                       (%seg "│ " fence-cell)
                       (%seg "    (" code-cell)
                       (%seg "when" kw-cell)
                       (%seg " verify-pkce" code-cell)))
     (%text-line (list (%seg "        " fence-cell)
                       (%seg "│ " fence-cell)
                       (%seg "      (check-pkce-challenge claims))" code-cell)))
     ;; (values (jwt-subject claims) claims)))
     (%text-line (list (%seg "        " fence-cell)
                       (%seg "│ " fence-cell)
                       (%seg "    (" code-cell)
                       (%seg "values" kw-cell)
                       (%seg " (jwt-subject claims) claims)))" code-cell)))
     (%text-line (list (%seg "        " fence-cell)
                       (%seg "│ " fence-cell)
                       (%seg "```" fence-cell))))))

(defun %tool-call-lines (palette-entries role-entries)
  "Build a tool call header line, nested inside AI content column (col 9)."
  (let ((tool-label-cell (%role-cell role-entries palette-entries "tool-label"
                                     :fallback-fg "cyan"))
        (meta-cell (%role-cell role-entries palette-entries "meta"
                               :fallback-fg "meta")))
    ;; 8 spaces to align with AI content, then [tool] label
    (list
     (%blank-line palette-entries)
     (%text-line (list (%seg "        " tool-label-cell)
                       (%seg "[tool] " tool-label-cell)
                       (%seg "bash-exec " meta-cell)
                       (%seg "[complete]" (%plain-cell palette-entries "green"))
                       (%seg " " meta-cell)
                       (%seg "(git diff HEAD~1 -- src/auth.lisp)" meta-cell))))))

(defun %tool-result-lines (palette-entries role-entries)
  "Build tool result output with a git diff, nested under [tool] label (col 16)."
  (let ((tool-cell (%role-cell role-entries palette-entries "tool"
                               :fallback-fg "cyan"))
        (meta-cell (%role-cell role-entries palette-entries "meta"
                               :fallback-fg "meta"))
        (hint-cell (%role-cell role-entries palette-entries "tool-hint"
                               :fallback-fg "muted"))
        (green-cell (%plain-cell palette-entries "green"))
        (rose-cell (%plain-cell palette-entries "rose"))
        (indent "               "))  ; 15 spaces = 8 (AI col) + 7 ([tool] )
    (list
     (%text-line (list (%seg indent tool-cell)
                       (%seg "diff --git a/src/auth.lisp b/src/auth.lisp" meta-cell)))
     (%text-line (list (%seg indent tool-cell)
                       (%seg "--- a/src/auth.lisp" rose-cell)))
     (%text-line (list (%seg indent tool-cell)
                       (%seg "+++ b/src/auth.lisp" green-cell)))
     (%text-line (list (%seg indent tool-cell)
                       (%seg "@@ -42,6 +42,15 @@" meta-cell)))
     (%text-line (list (%seg indent tool-cell)
                       (%seg " (defvar *api-key* nil)" tool-cell)))
     (%text-line (list (%seg indent tool-cell)
                       (%seg "+(defvar *oauth-provider* nil)" green-cell)))
     (%text-line (list (%seg indent tool-cell)
                       (%seg "+" green-cell)))
     (%text-line (list (%seg indent tool-cell)
                       (%seg "+(defun validate-oauth-token (token &key (verify-pkce t))" green-cell)))
     (%text-line (list (%seg indent tool-cell)
                       (%seg "+  (let ((claims (decode-jwt token)))" green-cell)))
     (%blank-line palette-entries)
     (%text-line (list (%seg indent hint-cell)
                       (%seg "(ctrl+o for full output)" hint-cell))))))

(defun %json-block-lines (palette-entries role-entries)
  "Build a second assistant response with JSON code block and a warning."
  (let ((label-cell (%role-cell role-entries palette-entries "assistant-label"))
        (text-cell (%role-cell role-entries palette-entries "assistant"
                               :fallback-fg "text"))
        (fence-cell (%role-cell role-entries palette-entries "code-fence"
                                :fallback-fg "border"))
        (code-cell (%role-cell role-entries palette-entries "code-body"
                               :fallback-fg "text"))
        (str-cell (%role-cell role-entries palette-entries "code-string"
                               :fallback-fg "green"))
        (warn-cell (%role-cell role-entries palette-entries "warning"
                               :fallback-fg "yellow")))
    (list
     (%blank-line palette-entries)
     (%text-line (list (%seg " [A]    " label-cell)
                       (%seg "The response also includes a JSON config for the OAuth provider setup:" text-cell)))
     (%blank-line palette-entries)
     (%text-line (list (%seg "        " fence-cell)
                       (%seg "│ " fence-cell)
                       (%seg "```json" fence-cell)))
     (%text-line (list (%seg "        " fence-cell)
                       (%seg "│ " fence-cell)
                       (%seg "{" code-cell)))
     ;; "provider": "github",
     (%text-line (list (%seg "        " fence-cell)
                       (%seg "│ " fence-cell)
                       (%seg "  " code-cell)
                       (%seg "\"provider\"" str-cell)
                       (%seg ": " code-cell)
                       (%seg "\"github\"" str-cell)
                       (%seg "," code-cell)))
     ;; "scopes": ["read:user", "repo"],
     (%text-line (list (%seg "        " fence-cell)
                       (%seg "│ " fence-cell)
                       (%seg "  " code-cell)
                       (%seg "\"scopes\"" str-cell)
                       (%seg ": [" code-cell)
                       (%seg "\"read:user\"" str-cell)
                       (%seg ", " code-cell)
                       (%seg "\"repo\"" str-cell)
                       (%seg "]," code-cell)))
     ;; "redirect_uri": "http://localhost:3000/callback"
     (%text-line (list (%seg "        " fence-cell)
                       (%seg "│ " fence-cell)
                       (%seg "  " code-cell)
                       (%seg "\"redirect_uri\"" str-cell)
                       (%seg ": " code-cell)
                       (%seg "\"http://localhost:3000/callback\"" str-cell)))
     (%text-line (list (%seg "        " fence-cell)
                       (%seg "│ " fence-cell)
                       (%seg "}" code-cell)))
     (%text-line (list (%seg "        " fence-cell)
                       (%seg "│ " fence-cell)
                       (%seg "```" fence-cell)))
     (%blank-line palette-entries)
     (%text-line (list (%seg "        > " text-cell)
                       (%seg "Warning:" warn-cell)
                       (%seg " OAuth tokens expire after 1 hour. You will need to implement a refresh token flow to maintain persistent sessions." text-cell))))))

(defun %error-lines (palette-entries role-entries)
  "Build a sample agent-level error line (429/503/etc) to showcase the error role.
Flush-left, red — visually breaks out of the conversation indent to signal
a system-level issue rather than LLM response content."
  (let ((error-cell (%role-cell role-entries palette-entries "error"
                                :fallback-fg "red")))
    (list
     (%blank-line palette-entries)
     (%text-line (list (%seg "429 Too Many Requests — rate limit exceeded, retrying in 12s" error-cell))))))

;;; --- Second conversation volley ---

(defun %user-followup-lines (palette-entries role-entries)
  "Build a second user message asking about tests."
  (let ((label-cell (%role-cell role-entries palette-entries "user-label"))
        (text-cell (%role-cell role-entries palette-entries "user"
                               :fallback-fg "text")))
    (list
     (%blank-line palette-entries)
     (%text-line (list (%seg " [U]    " label-cell)
                       (%seg "Can you also add tests for the new OAuth flow? I want to make sure the" text-cell)))
     (%text-line (list (%seg "        " text-cell)
                       (%seg "PKCE challenge verification works correctly with both valid and expired tokens." text-cell))))))

(defun %assistant-followup-lines (palette-entries role-entries)
  "Build the assistant's response to the test request — markdown + code."
  (let ((label-cell (%role-cell role-entries palette-entries "assistant-label"))
        (text-cell (%role-cell role-entries palette-entries "assistant"
                               :fallback-fg "text"))
        (heading-cell (%role-cell role-entries palette-entries "heading"
                                  :fallback-fg "blue"))
        (bold-text (ptui.core.types:make-cell
                    " "
                    (%resolve-color palette-entries "text")
                    nil
                    (ptui.core.types:make-attrs :boldp t)))
        (code-cell (%role-cell role-entries palette-entries "inline-code"
                               :fallback-fg "text" :fallback-bg "code-bg")))
    (list
     (%blank-line palette-entries)
     (%text-line (list (%seg " [A]    " label-cell)
                       (%seg "## Test Plan" heading-cell)))
     (%blank-line palette-entries)
     (%text-line (list (%seg "        I'll add a test suite covering three scenarios for " text-cell)
                       (%seg "validate-oauth-token" code-cell)
                       (%seg ":" text-cell)))
     (%blank-line palette-entries)
     (%text-line (list (%seg "        1. " text-cell)
                       (%seg "Valid token" bold-text)
                       (%seg " with correct PKCE challenge — should return subject + claims" text-cell)))
     (%text-line (list (%seg "        2. " text-cell)
                       (%seg "Expired token" bold-text)
                       (%seg " — should signal " text-cell)
                       (%seg "oauth-token-expired" code-cell)
                       (%seg " condition" text-cell)))
     (%text-line (list (%seg "        3. " text-cell)
                       (%seg "Invalid PKCE" bold-text)
                       (%seg " — should signal " text-cell)
                       (%seg "pkce-challenge-failed" code-cell)
                       (%seg " when " text-cell)
                       (%seg ":verify-pkce t" code-cell))))))

(defun %test-tool-call-lines (palette-entries role-entries)
  "Build a tool call for writing the test file."
  (let ((tool-label-cell (%role-cell role-entries palette-entries "tool-label"
                                     :fallback-fg "cyan"))
        (meta-cell (%role-cell role-entries palette-entries "meta"
                               :fallback-fg "meta")))
    (list
     (%blank-line palette-entries)
     (%text-line (list (%seg "        " tool-label-cell)
                       (%seg "[tool] " tool-label-cell)
                       (%seg "file-write " meta-cell)
                       (%seg "[complete]" (%plain-cell palette-entries "green"))
                       (%seg " " meta-cell)
                       (%seg "(tests/auth-oauth-test.lisp)" meta-cell))))))

(defun %test-code-block-lines (palette-entries role-entries)
  "Build a Lisp test code block for the OAuth test suite."
  (let ((fence-cell (%role-cell role-entries palette-entries "code-fence"
                                :fallback-fg "border"))
        (code-cell (%role-cell role-entries palette-entries "code-body"
                               :fallback-fg "text"))
        (kw-cell (%role-cell role-entries palette-entries "code-keyword"
                              :fallback-fg "rose"))
        (fn-cell (%role-cell role-entries palette-entries "code-function"
                              :fallback-fg "blue"))
        (str-cell (%role-cell role-entries palette-entries "code-string"
                               :fallback-fg "green")))
    (list
     (%text-line (list (%seg "        " fence-cell)
                       (%seg "│ " fence-cell)
                       (%seg "```lisp" fence-cell)))
     (%text-line (list (%seg "        " fence-cell)
                       (%seg "│ " fence-cell)
                       (%seg "(" code-cell)
                       (%seg "def-suite" kw-cell)
                       (%seg " " code-cell)
                       (%seg ":oauth-token-tests" fn-cell)
                       (%seg ")" code-cell)))
     (%text-line (list (%seg "        " fence-cell)
                       (%seg "│ " fence-cell)
                       (%seg "" code-cell)))
     (%text-line (list (%seg "        " fence-cell)
                       (%seg "│ " fence-cell)
                       (%seg "(" code-cell)
                       (%seg "test" kw-cell)
                       (%seg " " code-cell)
                       (%seg "valid-token-returns-subject" fn-cell)))
     (%text-line (list (%seg "        " fence-cell)
                       (%seg "│ " fence-cell)
                       (%seg "  (" code-cell)
                       (%seg "let" kw-cell)
                       (%seg " ((token (make-test-jwt " code-cell)
                       (%seg ":sub" kw-cell)
                       (%seg " " code-cell)
                       (%seg "\"user-42\"" str-cell)
                       (%seg ")))" code-cell)))
     (%text-line (list (%seg "        " fence-cell)
                       (%seg "│ " fence-cell)
                       (%seg "    (" code-cell)
                       (%seg "multiple-value-bind" kw-cell)
                       (%seg " (subject claims)" code-cell)))
     (%text-line (list (%seg "        " fence-cell)
                       (%seg "│ " fence-cell)
                       (%seg "        (validate-oauth-token token)" code-cell)))
     (%text-line (list (%seg "        " fence-cell)
                       (%seg "│ " fence-cell)
                       (%seg "      (" code-cell)
                       (%seg "is" kw-cell)
                       (%seg " (" code-cell)
                       (%seg "string=" fn-cell)
                       (%seg " subject " code-cell)
                       (%seg "\"user-42\"" str-cell)
                       (%seg "))" code-cell)))
     (%text-line (list (%seg "        " fence-cell)
                       (%seg "│ " fence-cell)
                       (%seg "      (" code-cell)
                       (%seg "is" kw-cell)
                       (%seg " (" code-cell)
                       (%seg "not" fn-cell)
                       (%seg " (" code-cell)
                       (%seg "null" fn-cell)
                       (%seg " claims))))))" code-cell)))
     (%text-line (list (%seg "        " fence-cell)
                       (%seg "│ " fence-cell)
                       (%seg "" code-cell)))
     (%text-line (list (%seg "        " fence-cell)
                       (%seg "│ " fence-cell)
                       (%seg "(" code-cell)
                       (%seg "test" kw-cell)
                       (%seg " " code-cell)
                       (%seg "expired-token-signals-condition" fn-cell)))
     (%text-line (list (%seg "        " fence-cell)
                       (%seg "│ " fence-cell)
                       (%seg "  (" code-cell)
                       (%seg "let" kw-cell)
                       (%seg " ((token (make-test-jwt " code-cell)
                       (%seg ":exp" kw-cell)
                       (%seg " (- (get-universal-time) 3600))))" code-cell)))
     (%text-line (list (%seg "        " fence-cell)
                       (%seg "│ " fence-cell)
                       (%seg "    (" code-cell)
                       (%seg "signals" kw-cell)
                       (%seg " (oauth-token-expired)" code-cell)))
     (%text-line (list (%seg "        " fence-cell)
                       (%seg "│ " fence-cell)
                       (%seg "      (validate-oauth-token token))))" code-cell)))
     (%text-line (list (%seg "        " fence-cell)
                       (%seg "│ " fence-cell)
                       (%seg "```" fence-cell)))
     (%blank-line palette-entries)
     (%text-line (list (%seg "        I've written the first two test cases. The PKCE challenge test" (%role-cell role-entries palette-entries "assistant" :fallback-fg "text"))))
     (%text-line (list (%seg "        requires a mock identity provider — I'll add that next." (%role-cell role-entries palette-entries "assistant" :fallback-fg "text")))))))

;;; --- Public API ---

(defun %wrap-widget-to-width (widget width)
  "Wrap a text widget's content to fit within WIDTH columns.
Returns a list of text-widgets (one per wrapped line)."
  (let* ((props (ptui.ui.elements:ui-element-props widget))
         (segments (getf props :styled-segments)))
    (if (and segments width)
        (let ((wrapped-lines (%wrap-segments segments width)))
          (mapcar (lambda (segs)
                    (let ((text (format nil "~{~A~}" (mapcar #'first segs))))
                      (ptui.widgets.core:make-text-widget
                       text
                       :styled-segments segs)))
                  wrapped-lines))
        (list widget))))

(defun make-sample-history-content (palette-entries role-entries width
                                    &key (scroll-offset 0))
  "Build a scroll widget containing a realistic styled conversation.
PALETTE-ENTRIES and ROLE-ENTRIES come from the YAML translator.
WIDTH controls how many columns each line uses — long lines wrap.
SCROLL-OFFSET controls the vertical scroll position."
  (let* ((raw-lines (append
                     ;; First volley: user asks about auth diff
                     (%user-message-lines palette-entries role-entries)
                     (list (%blank-line palette-entries))
                     (%assistant-markdown-lines palette-entries role-entries)
                     (%code-block-lines palette-entries role-entries)
                     (%tool-call-lines palette-entries role-entries)
                     (%tool-result-lines palette-entries role-entries)
                     (%json-block-lines palette-entries role-entries)
                     (%error-lines palette-entries role-entries)
                     ;; Second volley: user asks for tests
                     (%user-followup-lines palette-entries role-entries)
                     (%assistant-followup-lines palette-entries role-entries)
                     (%test-tool-call-lines palette-entries role-entries)
                     (%test-code-block-lines palette-entries role-entries)))
         (lines (if width
                    (mapcan (lambda (w) (%wrap-widget-to-width w width))
                            raw-lines)
                    raw-lines))
         (line-count (length lines))
         (viewport-height
           (let ((pkg (find-package :ptui.preview.app)))
             (if pkg
                 (multiple-value-bind (sym status)
                     (find-symbol "*HISTORY-VIEWPORT-HEIGHT*" pkg)
                   (if (and sym (eq status :internal) (boundp sym))
                       (max 1 (symbol-value sym))
                       1))
                 1)))
         (widget-offset
           (ptui.util.scroll:render-scroll-offset
            scroll-offset
            line-count
            viewport-height
            :origin :bottom)))
    (%preview-scroll-debug-log
     "WIDGET shared_offset=~D widget_offset=~D lines=~D viewport=~D max=~D width=~D"
     scroll-offset
     widget-offset
     line-count
     viewport-height
     (ptui.util.scroll:max-scroll-offset line-count viewport-height)
     (or width 0))
    ;; Publish content height so the preview app can clamp scroll offset
    (let ((pkg (find-package :ptui.preview.app)))
      (when pkg
        (multiple-value-bind (sym status) (find-symbol "*CONTENT-HEIGHT*" pkg)
          (when (and sym (eq status :internal) (boundp sym))
            (setf (symbol-value sym) line-count)))))
    (ptui.widgets.core:make-scroll-widget
     (ptui.widgets.core:make-stack-widget lines :id :sample-history)
     :id :history-scroll
     :scroll-bar :auto
     :offset widget-offset)))

(defun make-sample-status-content (palette-entries role-entries name-str
                                   &optional status-hints)
  "Build status bar content as a single full-width padded line.
STATUS-HINTS, when provided, is appended after the file path.
When STATUS-HINTS starts with \"(ctrl+c)\", render with orange bg / black fg."
  (let ((bar-cell (%role-cell role-entries palette-entries "status-bar"
                              :fallback-fg "muted" :fallback-bg "surface"))
        (ok-cell (%role-cell role-entries palette-entries "context-ok"
                             :fallback-fg "green" :fallback-bg "surface")))
    (cond
      ;; Ctrl+C banner: orange background, black foreground, full-width
      ((and (string-equal name-str "status")
            status-hints
            (>= (length status-hints) 8)
            (string-equal (subseq status-hints 0 8) "(ctrl+c)"))
       (let ((banner-cell (ptui.core.types:make-cell
                           " "
                           (ptui.core.color:make-color-rgb 0 0 0)
                           (ptui.core.color:make-color-rgb 224 175 104)
                           (ptui.core.types:make-attrs :boldp t))))
         (%text-line (list (%seg (format nil " ~A" status-hints) banner-cell)
                           (%seg (make-string 200 :initial-element #\Space)
                                 banner-cell)))))
      ((string-equal name-str "status")
       ;; Single unified status bar: 42% remaining │ file │ hints   [padded]
       (let* ((segments (list (%seg " " bar-cell)
                              (%seg "42%" ok-cell)
                              (%seg " │ amoebum/src/auth.lisp" bar-cell)
                              (%seg (if status-hints
                                        (format nil " │ ~A" status-hints)
                                        "")
                                    bar-cell)
                              ;; Pad to fill width so background is continuous
                              (%seg (make-string 200 :initial-element #\Space)
                                    bar-cell))))
         (%text-line segments)))
      (t
       (%text-line (list (%seg (format nil " ~A " name-str) bar-cell)))))))

(defun make-sample-prompt-content (palette-entries role-entries)
  "Build a prompt placeholder with cursor indicator.
Pads to 200 columns so the box widget stretches to fill available width."
  (declare (ignore role-entries))
  (let* ((text-cell (%plain-cell palette-entries "text"))
         (cursor-cell (ptui.core.types:make-cell
                       " "
                       (%resolve-color palette-entries "bg")
                       (%resolve-color palette-entries "text")
                       (ptui.core.types:make-attrs)))
         ;; Pad with spaces so box stretches to fill width
         (pad-cell (%plain-cell palette-entries nil nil))
         (pad-width 200))
    (%text-line (list (%seg " > " text-cell)
                      (%seg " " cursor-cell)
                      (%seg (make-string pad-width :initial-element #\Space)
                            pad-cell)))))
