#.(progn (require :asdf) nil)

(let* ((smoke-file (or *load-truename* *compile-file-truename*))
       (amoebum-dir (and smoke-file (make-pathname :name nil :type nil :defaults smoke-file)))
       (repo-root (and amoebum-dir (truename (merge-pathnames #P"../" amoebum-dir)))))
  (unless repo-root
    (error "Unable to resolve repository root from ~S" smoke-file))

  (load (merge-pathnames #P"ptui/.tools/quicklisp/setup.lisp" repo-root))
  (require :asdf)

  (let* ((asdf-pkg (or (find-package "ASDF")
                       (error "Missing package ASDF")))
         (load-asd-sym (or (find-symbol "LOAD-ASD" asdf-pkg)
                           (error "Missing symbol LOAD-ASD in ASDF package")))
         (load-system-sym (or (find-symbol "LOAD-SYSTEM" asdf-pkg)
                              (error "Missing symbol LOAD-SYSTEM in ASDF package")))
         (load-asd-fn (symbol-function load-asd-sym))
         (load-system-fn (symbol-function load-system-sym)))
    (funcall load-asd-fn (merge-pathnames #P"pseudopod/pseudopod.asd" repo-root))
    (funcall load-asd-fn (merge-pathnames #P"sw4rm-sdk/sw4rm-sdk.asd" repo-root))
    (funcall load-asd-fn (merge-pathnames #P"ptui/ptui.asd" repo-root))
    (funcall load-asd-fn (merge-pathnames #P"amoebum/amoebum.asd" repo-root))
    (funcall load-system-fn "amoebum"))

  (let* ((amoebum-pkg (or (find-package "AMOEBUM")
                          (error "Missing package AMOEBUM after load.")))
         (pseudopod-pkg (or (find-package "PSEUDOPOD")
                            (error "Missing package PSEUDOPOD after load.")))
         (uiop-pkg (or (find-package "UIOP")
                       (find-package "ASDF/UTILITY")
                       (error "Missing UIOP package after requiring ASDF.")))
         (symbol-in
           (lambda (name package)
             (or (find-symbol name package)
                 (error "Missing symbol ~A in package ~A." name (package-name package)))))
         (fn-in
           (lambda (name package)
             (symbol-function (funcall symbol-in name package))))
         (temporary-directory-fn (funcall fn-in "TEMPORARY-DIRECTORY" uiop-pkg))
         (ensure-directory-pathname-fn (funcall fn-in "ENSURE-DIRECTORY-PATHNAME" uiop-pkg))
         (find-tool-fn (funcall fn-in "FIND-TOOL" pseudopod-pkg))
         (tool-definition-fn-fn (funcall fn-in "TOOL-DEFINITION-FN" pseudopod-pkg))
         (toolset-sym (funcall symbol-in "*TOOLSET*" amoebum-pkg))
         (setconfig-fn (funcall fn-in "SETCONFIG" amoebum-pkg))
         (web-fetch-cache-sym (funcall symbol-in "*WEB-FETCH-CACHE*" amoebum-pkg)))
    (labels ((assert-true (condition format-string &rest format-args)
               (unless condition
                 (error (apply #'format nil format-string format-args))))
             (contains-substring-p (needle haystack)
               (and (stringp haystack)
                    (search needle haystack :test #'char-equal)))
             (write-text-file (path content)
               (ensure-directories-exist path)
               (with-open-file (stream path
                                       :direction :output
                                       :if-exists :supersede
                                       :if-does-not-exist :create
                                       :external-format :utf-8)
                 (write-string content stream)))
             (make-args (&rest key-values)
               (let ((args (make-hash-table :test #'equal)))
                 (loop for (key value) on key-values by #'cddr do
                       (setf (gethash key args) value))
                 args))
             (invoke-tool (tool-name &rest key-values)
               (let* ((toolset (symbol-value toolset-sym))
                      (tool (funcall find-tool-fn toolset tool-name)))
                 (assert-true tool "Expected tool ~S to be registered." tool-name)
                 (funcall (funcall tool-definition-fn-fn tool)
                          (apply #'make-args key-values))))
             (command-available-p (command)
               (handler-case
                   (progn
                     (uiop:run-program (list command "--version")
                                       :ignore-error-status t
                                       :output :string
                                       :error-output :string)
                     t)
                 (error ()
                   nil)))
             (resolve-python-command ()
               (or (and (command-available-p "python3") "python3")
                   (and (command-available-p "python") "python")
                   (error "Web fetch smoke test requires python3 or python.")))
             (wait-until (predicate timeout-seconds &optional (poll-seconds 0.05d0))
               (let ((deadline (+ (get-internal-real-time)
                                  (ceiling (* timeout-seconds
                                              internal-time-units-per-second)))))
                 (loop
                   do
                      (when (funcall predicate)
                        (return t))
                      (when (>= (get-internal-real-time) deadline)
                        (return nil))
                      (sleep poll-seconds))))
             (read-json-lines (path)
               (when (probe-file path)
                 (with-open-file (stream path :direction :input :external-format :utf-8)
                   (let* ((jonathan-pkg (or (find-package :jonathan)
                                            (error "Missing package JONATHAN.")))
                          (parse-symbol (or (find-symbol "PARSE" jonathan-pkg)
                                            (error "Missing JONATHAN:PARSE.")))
                          (parse-fn (symbol-function parse-symbol)))
                     (loop for line = (read-line stream nil nil)
                           while line
                           collect (funcall parse-fn line :as :hash-table))))))
             (path-call-count (entries path)
               (count path entries
                      :test #'string=
                      :key (lambda (entry) (gethash "path" entry))))
             (write-server-script (path)
               (write-text-file
                path
                (format nil
                        (concatenate
                         'string
                         "import http.server~%"
                         "import json~%"
                         "import urllib.parse~%"
                         "import time~%"
                         "import sys~%"
                         "~%"
                         "port_path = sys.argv[1]~%"
                         "log_path = sys.argv[2]~%"
                         "~%"
                         "LONG_PARAGRAPH = ' '.join(['alpha beta gamma delta'] * 2500)~%"
                         "~%"
                         "class Handler(http.server.BaseHTTPRequestHandler):~%"
                         "    def log_message(self, format, *args):~%"
                         "        return~%"
                         "~%"
                         "    def _write(self, status, body, content_type='text/html; charset=utf-8', headers=None):~%"
                         "        payload = body.encode('utf-8')~%"
                         "        self.send_response(status)~%"
                         "        self.send_header('Content-Type', content_type)~%"
                         "        self.send_header('Content-Length', str(len(payload)))~%"
                         "        if headers:~%"
                         "            for key, value in headers.items():~%"
                         "                self.send_header(key, value)~%"
                         "        self.end_headers()~%"
                         "        self.wfile.write(payload)~%"
                         "~%"
                         "    def do_GET(self):~%"
                         "        parsed = urllib.parse.urlparse(self.path)~%"
                         "        with open(log_path, 'a', encoding='utf-8') as handle:~%"
                         "            handle.write(json.dumps({'ts': time.time(), 'path': parsed.path}) + '\\n')~%"
                         "~%"
                         "        if parsed.path == '/article':~%"
                         "            html = \"\"\"<html><head><title>Example Article Title</title></head><body>~%"
                         "<nav>Navigation should be removed</nav>~%"
                         "<article>~%"
                         "<h1>Example Article Title</h1>~%"
                         "<p>This is the primary paragraph with meaningful content.</p>~%"
                         "<p>Second paragraph for readability extraction checks.</p>~%"
                         "</article>~%"
                         "<aside>Advertisement sidebar text</aside>~%"
                         "</body></html>\"\"\"~%"
                         "            self._write(200, html)~%"
                         "            return~%"
                         "~%"
                         "        if parsed.path == '/long':~%"
                         "            html = f\"<html><head><title>Long Content</title></head><body><article><h1>Long Content</h1><p>{LONG_PARAGRAPH}</p></article></body></html>\"~%"
                         "            self._write(200, html)~%"
                         "            return~%"
                         "~%"
                         "        if parsed.path == '/needs-auth':~%"
                         "            location = f'http://localhost:{self.server.server_address[1]}/login'~%"
                         "            self._write(302, 'redirecting', headers={'Location': location})~%"
                         "            return~%"
                         "~%"
                         "        if parsed.path == '/login':~%"
                         "            html = \"\"\"<html><head><title>Sign In</title></head><body>~%"
                         "<main><h1>Sign In</h1><form><input type='text' name='email'/><input type='password' name='password'/></form></main>~%"
                         "</body></html>\"\"\"~%"
                         "            self._write(200, html)~%"
                         "            return~%"
                         "~%"
                         "        self._write(404, 'not found', 'text/plain; charset=utf-8')~%"
                         "~%"
                         "server = http.server.ThreadingHTTPServer(('127.0.0.1', 0), Handler)~%"
                         "with open(port_path, 'w', encoding='utf-8') as port_file:~%"
                         "    port_file.write(str(server.server_address[1]))~%"
                         "server.serve_forever()~%")))))
      (let* ((python-command (resolve-python-command))
             (tmp-root
               (funcall ensure-directory-pathname-fn
                        (merge-pathnames
                         (make-pathname :directory `(:relative ,(format nil "amoebum-i58-~A"
                                                                        (get-universal-time))))
                         (funcall temporary-directory-fn))))
             (server-script (merge-pathnames #P"mock-web-fetch-server.py" tmp-root))
             (port-file (merge-pathnames #P"server.port" tmp-root))
             (log-file (merge-pathnames #P"requests.jsonl" tmp-root))
             (process nil))
        (write-server-script server-script)
        (setf process
              (uiop:launch-program (list python-command
                                         (namestring server-script)
                                         (namestring port-file)
                                         (namestring log-file))
                                   :output :stream
                                   :error-output :stream
                                   :ignore-error-status t))
        (unwind-protect
            (progn
              (assert-true (wait-until (lambda () (probe-file port-file)) 5.0d0)
                           "Timed out waiting for mock web fetch server startup.")
              (let* ((port (parse-integer
                            (string-trim '(#\Space #\Tab #\Newline #\Return)
                                         (uiop:read-file-string port-file
                                                                :external-format :utf-8))))
                     (base-url (format nil "http://127.0.0.1:~D" port))
                     (article-url (format nil "~A/article" base-url))
                     (long-url (format nil "~A/long" base-url))
                     (auth-url (format nil "~A/needs-auth" base-url)))
                (funcall setconfig-fn :permission-mode :full-auto)
                (funcall setconfig-fn :web-fetch-timeout-seconds 20)
                (funcall setconfig-fn :web-fetch-cache-ttl-seconds 900)
                (funcall setconfig-fn :web-fetch-max-markdown-bytes 10240)
                (funcall setconfig-fn :web-fetch-user-agent "amoebum-web-fetch-smoke/0.1")
                (clrhash (symbol-value web-fetch-cache-sym))

                (let* ((article-result (invoke-tool "web-fetch"
                                                    "url" article-url
                                                    "cache-ttl-seconds" 900))
                       (article-markdown (getf article-result :markdown)))
                  (assert-true (not (getf article-result :cached))
                               "Expected first fetch to be uncached.")
                  (assert-true (eq :pseudopod (getf article-result :fetch-engine))
                               "Expected fetch orchestration to report pseudopod engine.")
                  (assert-true (integerp (getf article-result :fetched-at))
                               "Expected normalized fetched-at timestamp from pseudopod fetch response.")
                  (assert-true (contains-substring-p "# Example Article Title" article-markdown)
                               "Expected extracted markdown title, got ~S."
                               article-markdown)
                  (assert-true (contains-substring-p "primary paragraph with meaningful content"
                                                     article-markdown)
                               "Expected markdown body from article extraction.")
                  (assert-true (not (contains-substring-p "Navigation should be removed"
                                                          article-markdown))
                               "Expected nav text to be excluded from readable markdown."))

                (let ((cached-result (invoke-tool "web-fetch"
                                                  "url" article-url
                                                  "cache-ttl-seconds" 900)))
                  (assert-true (getf cached-result :cached)
                               "Expected second fetch to be served from cache."))

                (let* ((entries (or (read-json-lines log-file) '()))
                       (article-calls (path-call-count entries "/article")))
                  (assert-true (= article-calls 1)
                               "Expected one /article request due to cache hit, got ~D."
                               article-calls))

                (let* ((long-result (invoke-tool "web-fetch"
                                                 "url" long-url
                                                 "max-markdown-bytes" 600
                                                 "cache-ttl-seconds" 60))
                       (long-markdown (getf long-result :markdown)))
                  (assert-true (getf long-result :truncated-p)
                               "Expected oversized page markdown to be truncated.")
                  (assert-true (<= (length long-markdown) 600)
                               "Expected truncated markdown length <= 600, got ~D."
                               (length long-markdown)))

                (let ((auth-result (invoke-tool "web-fetch"
                                                "url" auth-url
                                                "cache-ttl-seconds" 30)))
                  (assert-true (getf auth-result :host-changed)
                               "Expected redirect host change detection for login redirect.")
                  (assert-true (contains-substring-p "authentication wall"
                                                     (or (getf auth-result :authentication-warning)
                                                         ""))
                               "Expected authentication warning for login-wall redirect, got ~S."
                               auth-result)))))
          (when process
            (ignore-errors (uiop:terminate-process process))
            (ignore-errors (uiop:wait-process process))))))
    (format t "AMOEBUM_WEB_FETCH_SMOKE_OK~%"))
