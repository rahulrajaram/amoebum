(in-package :amoebum)

(defparameter *web-fetch-default-cache-ttl-seconds* 900)
(defparameter *web-fetch-cache* (make-hash-table :test #'equal))
(defparameter *web-fetch-cache-lock*
  (bordeaux-threads:make-lock "amoebum-web-fetch-cache"))

(defun %web-resolve-fetch-cache-ttl-seconds (override)
  (%web-resolve-positive-integer
   override
   (%web-resolve-positive-integer (cfg :web-fetch-cache-ttl-seconds)
                                  *web-fetch-default-cache-ttl-seconds*)))

(defun %web-fetch-cache-key (url max-markdown-bytes)
  (list (%web-trim url) max-markdown-bytes))

(defun %web-copy-plist (plist)
  (loop for (key value) on plist by #'cddr append (list key value)))

(defun %web-fetch-cache-get (cache-key now)
  (bordeaux-threads:with-lock-held (*web-fetch-cache-lock*)
    (let ((entry (gethash cache-key *web-fetch-cache*)))
      (when entry
        (let ((expires-at (or (getf entry :expires-at) 0.0d0)))
          (if (and (> expires-at 0.0d0) (> now expires-at))
              (progn
                (remhash cache-key *web-fetch-cache*)
                nil)
              (%web-copy-plist (getf entry :result))))))))

(defun %web-fetch-cache-put (cache-key result now ttl-seconds)
  (when (and (integerp ttl-seconds) (> ttl-seconds 0))
    (bordeaux-threads:with-lock-held (*web-fetch-cache-lock*)
      (setf (gethash cache-key *web-fetch-cache*)
            (list :expires-at (+ now ttl-seconds)
                  :result (%web-copy-plist result))))))
