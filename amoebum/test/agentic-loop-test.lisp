(in-package :amoebum/test)

;;; ---------------------------------------------------------------------------
;;; Agentic tool loop integration test — exercises 20+ iterations of the
;;; tool-call → execute → result → continuation cycle using REAL tools
;;; (write_file, bash_exec, read_file) to build an Instagram app skeleton
;;; in a temp directory.
;;; ---------------------------------------------------------------------------

(defparameter +instagram-target-iterations+ 20
  "Number of tool-call iterations the mock runner should produce.")

(defun %count-tool-result-messages (messages)
  "Count messages with role \"tool\" in a message list."
  (count-if (lambda (msg)
              (and (pseudopod:message-p msg)
                   (string= (pseudopod:message-role msg) "tool")))
            messages))

(defun %count-assistant-messages (messages)
  "Count messages with role \"assistant\" in a message list."
  (count-if (lambda (msg)
              (and (pseudopod:message-p msg)
                   (string= (pseudopod:message-role msg) "assistant")))
            messages))

(defun %instagram-tool-call-script (project-dir)
  "Return a list of 20 (tool-name . arguments-json) pairs that build an
Instagram app skeleton using real tools."
  (let ((d (namestring project-dir)))
    (list
     ;; 1. Create project directory
     (cons "bash-exec"
           (format nil "{\"command\": \"mkdir -p ~A\"}" d))
     ;; 2. package.json
     (cons "write-file"
           (format nil "{\"path\": \"~Apackage.json\", \"content\": ~S}"
                   d "{\"name\":\"instagram-clone\",\"version\":\"1.0.0\",\"scripts\":{\"start\":\"node server.js\",\"dev\":\"node server.js\"},\"dependencies\":{\"express\":\"^4.18.0\"}}"))
     ;; 3. server.js
     (cons "write-file"
           (format nil "{\"path\": \"~Aserver.js\", \"content\": ~S}"
                   d "const express = require('express');
const path = require('path');
const app = express();
app.use(express.json());
app.use(express.static(path.join(__dirname, 'public')));
const posts = [];
app.get('/api/posts', (req, res) => res.json(posts));
app.post('/api/posts', (req, res) => { const post = { id: Date.now(), ...req.body, likes: 0, comments: [], createdAt: new Date() }; posts.unshift(post); res.status(201).json(post); });
app.post('/api/posts/:id/like', (req, res) => { const post = posts.find(p => p.id === parseInt(req.params.id)); if (post) { post.likes++; res.json(post); } else { res.status(404).json({error:'Not found'}); } });
app.post('/api/posts/:id/comment', (req, res) => { const post = posts.find(p => p.id === parseInt(req.params.id)); if (post) { post.comments.push({ user: req.body.user || 'anon', text: req.body.text, createdAt: new Date() }); res.json(post); } else { res.status(404).json({error:'Not found'}); } });
app.listen(3333, () => console.log('Instagram clone on http://localhost:3333'));"))
     ;; 4. Create public directory
     (cons "bash-exec"
           (format nil "{\"command\": \"mkdir -p ~Apublic\"}" d))
     ;; 5. index.html
     (cons "write-file"
           (format nil "{\"path\": \"~Apublic/index.html\", \"content\": ~S}"
                   d "<!DOCTYPE html><html><head><meta charset='utf-8'><meta name='viewport' content='width=device-width,initial-scale=1'><title>Instagram Clone</title><link rel='stylesheet' href='style.css'></head><body><div id='app'><header><h1>Instagram Clone</h1></header><div id='new-post'><textarea id='caption' placeholder='Write a caption...'></textarea><button onclick='createPost()'>Post</button></div><div id='feed'></div></div><script src='app.js'></script></body></html>"))
     ;; 6. style.css
     (cons "write-file"
           (format nil "{\"path\": \"~Apublic/style.css\", \"content\": ~S}"
                   d "* { margin: 0; padding: 0; box-sizing: border-box; }
body { font-family: -apple-system, BlinkMacSystemFont, sans-serif; background: #fafafa; }
#app { max-width: 600px; margin: 0 auto; padding: 20px; }
header { text-align: center; padding: 20px 0; border-bottom: 1px solid #dbdbdb; margin-bottom: 20px; }
header h1 { font-size: 24px; }
#new-post { background: white; border: 1px solid #dbdbdb; border-radius: 8px; padding: 16px; margin-bottom: 20px; }
#new-post textarea { width: 100%; height: 60px; border: 1px solid #dbdbdb; border-radius: 4px; padding: 8px; resize: none; font-size: 14px; }
#new-post button { margin-top: 8px; padding: 8px 24px; background: #0095f6; color: white; border: none; border-radius: 4px; cursor: pointer; font-weight: 600; }
.post { background: white; border: 1px solid #dbdbdb; border-radius: 8px; margin-bottom: 20px; overflow: hidden; }
.post-header { padding: 12px 16px; font-weight: 600; }
.post-caption { padding: 8px 16px; }
.post-actions { padding: 8px 16px; display: flex; gap: 16px; }
.post-actions button { background: none; border: none; cursor: pointer; font-size: 14px; }
.post-likes { padding: 4px 16px; font-weight: 600; font-size: 14px; }
.post-comments { padding: 8px 16px; }
.comment { font-size: 14px; margin-bottom: 4px; }
.comment-form { display: flex; padding: 8px 16px; border-top: 1px solid #efefef; }
.comment-form input { flex: 1; border: none; font-size: 14px; padding: 8px 0; }
.comment-form button { background: none; border: none; color: #0095f6; font-weight: 600; cursor: pointer; }
.post-time { padding: 4px 16px 12px; font-size: 10px; color: #8e8e8e; text-transform: uppercase; }"))
     ;; 7. app.js
     (cons "write-file"
           (format nil "{\"path\": \"~Apublic/app.js\", \"content\": ~S}"
                   d "async function createPost() {
  const caption = document.getElementById('caption');
  if (!caption.value.trim()) return;
  const res = await fetch('/api/posts', { method: 'POST', headers: {'Content-Type':'application/json'}, body: JSON.stringify({caption: caption.value, user: 'user'}) });
  caption.value = '';
  loadFeed();
}
async function likePost(id) {
  await fetch(`/api/posts/${id}/like`, { method: 'POST' });
  loadFeed();
}
async function addComment(id) {
  const input = document.getElementById(`comment-${id}`);
  if (!input.value.trim()) return;
  await fetch(`/api/posts/${id}/comment`, { method: 'POST', headers: {'Content-Type':'application/json'}, body: JSON.stringify({text: input.value, user: 'user'}) });
  input.value = '';
  loadFeed();
}
function timeAgo(date) { const s = Math.floor((Date.now()-new Date(date))/1000); if(s<60) return s+'s'; if(s<3600) return Math.floor(s/60)+'m'; if(s<86400) return Math.floor(s/3600)+'h'; return Math.floor(s/86400)+'d'; }
function renderPost(post) {
  const comments = post.comments.map(c => `<div class='comment'><b>${c.user}</b> ${c.text}</div>`).join('');
  return `<div class='post'><div class='post-header'>${post.user||'user'}</div><div class='post-caption'>${post.caption}</div><div class='post-actions'><button onclick='likePost(${post.id})'>♥ Like</button></div><div class='post-likes'>${post.likes} likes</div><div class='post-comments'>${comments}</div><div class='comment-form'><input id='comment-${post.id}' placeholder='Add a comment...'><button onclick='addComment(${post.id})'>Post</button></div><div class='post-time'>${timeAgo(post.createdAt)}</div></div>`;
}
async function loadFeed() { const res = await fetch('/api/posts'); const posts = await res.json(); document.getElementById('feed').innerHTML = posts.map(renderPost).join(''); }
loadFeed();"))
     ;; 8. Create models directory
     (cons "bash-exec"
           (format nil "{\"command\": \"mkdir -p ~Amodels\"}" d))
     ;; 9. User model
     (cons "write-file"
           (format nil "{\"path\": \"~Amodels/user.js\", \"content\": ~S}"
                   d "class User { constructor(username, email) { this.id = Date.now(); this.username = username; this.email = email; this.bio = ''; this.avatar = null; this.followers = []; this.following = []; this.createdAt = new Date(); } follow(userId) { if (!this.following.includes(userId)) this.following.push(userId); } unfollow(userId) { this.following = this.following.filter(id => id !== userId); } } module.exports = User;"))
     ;; 10. Post model
     (cons "write-file"
           (format nil "{\"path\": \"~Amodels/post.js\", \"content\": ~S}"
                   d "class Post { constructor(userId, caption, imageUrl) { this.id = Date.now(); this.userId = userId; this.caption = caption; this.imageUrl = imageUrl || null; this.likes = []; this.comments = []; this.createdAt = new Date(); } addLike(userId) { if (!this.likes.includes(userId)) this.likes.push(userId); } removeLike(userId) { this.likes = this.likes.filter(id => id !== userId); } addComment(userId, text) { this.comments.push({id: Date.now(), userId, text, createdAt: new Date()}); } } module.exports = Post;"))
     ;; 11. Create routes directory
     (cons "bash-exec"
           (format nil "{\"command\": \"mkdir -p ~Aroutes\"}" d))
     ;; 12. User routes
     (cons "write-file"
           (format nil "{\"path\": \"~Aroutes/users.js\", \"content\": ~S}"
                   d "const express = require('express'); const router = express.Router(); const users = []; router.post('/register', (req, res) => { const {username, email} = req.body; if (users.find(u => u.username === username)) return res.status(400).json({error:'Username taken'}); const user = {id: Date.now(), username, email, bio:'', followers:[], following:[], createdAt: new Date()}; users.push(user); res.status(201).json(user); }); router.get('/:username', (req, res) => { const user = users.find(u => u.username === req.params.username); user ? res.json(user) : res.status(404).json({error:'Not found'}); }); router.post('/:id/follow', (req, res) => { const user = users.find(u => u.id === parseInt(req.params.id)); if (!user) return res.status(404).json({error:'Not found'}); if (!user.followers.includes(req.body.followerId)) user.followers.push(req.body.followerId); res.json(user); }); module.exports = router;"))
     ;; 13. Create middleware directory
     (cons "bash-exec"
           (format nil "{\"command\": \"mkdir -p ~Amiddleware\"}" d))
     ;; 14. Auth middleware
     (cons "write-file"
           (format nil "{\"path\": \"~Amiddleware/auth.js\", \"content\": ~S}"
                   d "function authMiddleware(req, res, next) { const token = req.headers.authorization; if (!token) { req.user = {id: 1, username: 'anonymous'}; } else { req.user = {id: parseInt(token), username: 'user_' + token}; } next(); } module.exports = authMiddleware;"))
     ;; 15. Create config directory and config file
     (cons "bash-exec"
           (format nil "{\"command\": \"mkdir -p ~Aconfig\"}" d))
     ;; 16. Config file
     (cons "write-file"
           (format nil "{\"path\": \"~Aconfig/default.js\", \"content\": ~S}"
                   d "module.exports = { port: process.env.PORT || 3333, maxPostLength: 2200, maxCommentLength: 500, maxFileSize: 10 * 1024 * 1024, allowedImageTypes: ['image/jpeg', 'image/png', 'image/webp'] };"))
     ;; 17. Read back server.js to verify it was written
     (cons "read-file"
           (format nil "{\"path\": \"~Aserver.js\"}" d))
     ;; 18. Read back index.html
     (cons "read-file"
           (format nil "{\"path\": \"~Apublic/index.html\"}" d))
     ;; 19. List all files created
     (cons "bash-exec"
           (format nil "{\"command\": \"find ~A -type f | sort\"}" d))
     ;; 20. Count lines of code
     (cons "bash-exec"
           (format nil "{\"command\": \"wc -l ~Aserver.js ~Apublic/app.js ~Apublic/style.css ~Apublic/index.html ~Amodels/*.js ~Aroutes/users.js ~Amiddleware/auth.js ~Aconfig/default.js\"}"
                   d d d d d d d d)))))

(defun %mock-instagram-runner (stream-state prompt messages
                               &key system-prompt client tools)
  "Mock stream runner that scripts 20 real tool calls to build an Instagram
app skeleton, then emits a text summary to terminate the loop."
  (declare (ignore prompt system-prompt client tools))
  (let* ((completed (%count-tool-result-messages messages))
         (project-dir (or (getf (symbol-plist '%instagram-project-dir) :path)
                          (error "Project dir not set on %instagram-project-dir plist")))
         (script (%instagram-tool-call-script project-dir)))
    (if (< completed (length script))
        (let* ((step (nth completed script))
               (tool-name (car step))
               (arguments (cdr step))
               (call-id (format nil "call_~D" (1+ completed)))
               (tool-call (pseudopod:make-tool-call
                           :id call-id
                           :name tool-name
                           :arguments arguments)))
          (amoebum::token-stream-emit-tool-call-started stream-state tool-call)
          (amoebum::token-stream-emit-tool-call-argument-complete
           stream-state tool-call))
        ;; Done — emit final text
        (amoebum::token-stream-emit-chunk
         stream-state
         (format nil "Built Instagram clone with ~D tool calls. Project at ~A"
                 (length script) (namestring project-dir))))))

(test agentic-loop-builds-instagram-app
  "Exercise 20 iterations of the agentic tool loop using REAL tools
(write_file, bash_exec, read_file) to build an Instagram app in /tmp."
  (let ((original-toolset amoebum:*toolset*)
        (original-metadata amoebum:*tool-metadata*)
        (original-hooks amoebum:*hook-registry*)
        (original-event-bus amoebum:*event-bus*)
        (original-rules amoebum:*permission-rules*)
        (project-dir nil))
    (unwind-protect
        (progn
          ;; Create temp directory
          (setf project-dir
                (uiop:ensure-directory-pathname
                 (merge-pathnames
                  (make-pathname
                   :directory `(:relative
                                ,(format nil "instagram-agentic-~D-~D"
                                         (get-universal-time)
                                         (random 1000000))))
                  (uiop:ensure-directory-pathname
                   (uiop:temporary-directory)))))
          ;; Stash project dir where mock runner can find it
          (setf (getf (symbol-plist '%instagram-project-dir) :path) project-dir)
          ;; Isolate globals — but keep real toolset so real tools execute
          (let ((real-toolset amoebum:*toolset*))
            (setf amoebum:*tool-metadata* (make-hash-table :test #'equal)
                  amoebum:*hook-registry* (make-hash-table :test #'equal)
                  amoebum:*event-bus* (amoebum:make-event-bus :capacity 256)
                  amoebum:*permission-rules* nil))
          ;; Build chat-state with mock runner
          (let* ((bus amoebum:*event-bus*)
                 (chat-state
                   (amoebum:ensure-chat-ui-state
                    (amoebum:make-chat-ui-state
                     :stream-runner #'%mock-instagram-runner
                     :status-bar-state
                     (amoebum:make-status-bar-state
                      :event-bus bus
                      :model-name "test-model"
                      :branch-name "test-branch")))))
            (let ((old-mode (amoebum:config-permission-mode
                             (amoebum:current-config))))
              (amoebum:setconfig :permission-mode :full-auto)
              (unwind-protect
                  (progn
                    (setf (amoebum:chat-ui-state-input-text chat-state)
                          "Build me a full-stack Instagram clone.")
                    (let ((user-msg (amoebum:chat-ui-submit-input chat-state)))
                      (amoebum::%start-streaming-assistant-response
                       chat-state user-msg))
                    ;; Drain events until idle or timeout
                    (let ((max-polls 600)
                          (poll-count 0)
                          (idle-p nil))
                      (loop while (and (< poll-count max-polls) (not idle-p))
                            do (sleep 0.05)
                               (amoebum::%drain-stream-events chat-state)
                               (incf poll-count)
                               (let* ((conv (amoebum::chat-ui-state-conversation
                                             chat-state))
                                      (state (and conv
                                                  (amoebum::conversation-state-state
                                                   conv))))
                                 (when (eq state :idle)
                                   (setf idle-p t))))
                      ;; --- Assertions ---
                      (let* ((messages (amoebum:chat-ui-state-messages chat-state))
                             (tool-msgs (%count-tool-result-messages messages))
                             (iteration-count
                               (amoebum::chat-ui-state-agentic-iteration-count
                                chat-state)))
                        ;; Reached idle
                        (is-true idle-p
                                 "Expected conversation to reach :idle within timeout.")
                        ;; 20 tool result messages
                        (is (= tool-msgs +instagram-target-iterations+)
                            "Expected ~D tool result messages, got ~D."
                            +instagram-target-iterations+ tool-msgs)
                        ;; Iteration counter
                        (is (= iteration-count +instagram-target-iterations+)
                            "Expected iteration count ~D, got ~D."
                            +instagram-target-iterations+ iteration-count)
                        ;; --- Verify real files exist on disk ---
                        (is-true (probe-file (merge-pathnames "package.json" project-dir))
                                 "Expected package.json to exist.")
                        (is-true (probe-file (merge-pathnames "server.js" project-dir))
                                 "Expected server.js to exist.")
                        (is-true (probe-file (merge-pathnames "public/index.html" project-dir))
                                 "Expected public/index.html to exist.")
                        (is-true (probe-file (merge-pathnames "public/style.css" project-dir))
                                 "Expected public/style.css to exist.")
                        (is-true (probe-file (merge-pathnames "public/app.js" project-dir))
                                 "Expected public/app.js to exist.")
                        (is-true (probe-file (merge-pathnames "models/user.js" project-dir))
                                 "Expected models/user.js to exist.")
                        (is-true (probe-file (merge-pathnames "models/post.js" project-dir))
                                 "Expected models/post.js to exist.")
                        (is-true (probe-file (merge-pathnames "routes/users.js" project-dir))
                                 "Expected routes/users.js to exist.")
                        (is-true (probe-file (merge-pathnames "middleware/auth.js" project-dir))
                                 "Expected middleware/auth.js to exist.")
                        (is-true (probe-file (merge-pathnames "config/default.js" project-dir))
                                 "Expected config/default.js to exist.")
                        ;; Verify file content is non-empty
                        (let ((server-content
                                (uiop:read-file-string
                                 (merge-pathnames "server.js" project-dir))))
                          (is (> (length server-content) 100)
                              "Expected server.js to have substantial content, got ~D chars."
                              (length server-content))
                          (is-true (search "express" server-content)
                                   "Expected server.js to contain 'express'."))
                        ;; Final message should reference the project
                        (let* ((final-msg (car (last messages)))
                               (final-text
                                 (and (pseudopod:message-p final-msg)
                                      (amoebum::%message-content->text final-msg))))
                          (is-true (and final-text
                                        (search "Instagram" final-text))
                                   "Expected final message to reference Instagram.")))))
                (amoebum:setconfig :permission-mode old-mode)))))
      ;; Cleanup
      (when project-dir
        (ignore-errors
          (uiop:delete-directory-tree project-dir
                                     :validate t
                                     :if-does-not-exist :ignore)))
      (setf (getf (symbol-plist '%instagram-project-dir) :path) nil)
      (setf amoebum:*toolset* original-toolset
            amoebum:*tool-metadata* original-metadata
            amoebum:*hook-registry* original-hooks
            amoebum:*event-bus* original-event-bus
            amoebum:*permission-rules* original-rules))))
