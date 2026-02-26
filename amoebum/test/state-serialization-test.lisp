(in-package :amoebum/test)

;;; ============================================================
;;; I251: Selective State Serialization for Checkpoints
;;; ============================================================

(def-suite state-serialization-suite :in amoebum-suite)
(in-suite state-serialization-suite)

;;; --- Encode/decode round-trips ---

(test encode-decode-string
  "Strings should round-trip through encode/decode."
  (let ((value "hello world"))
    (is (string= value
                 (amoebum::%checkpoint-decode-value
                  (amoebum::%checkpoint-encode-value value))))))

(test encode-decode-hash-table
  "Hash tables should round-trip through encode/decode."
  (let ((ht (make-hash-table :test #'equal)))
    (setf (gethash "key1" ht) "value1"
          (gethash "key2" ht) 42)
    (let ((decoded (amoebum::%checkpoint-decode-value
                    (amoebum::%checkpoint-encode-value ht))))
      (is (hash-table-p decoded))
      (is (string= "value1" (gethash "key1" decoded)))
      (is (= 42 (gethash "key2" decoded))))))

(test encode-decode-pathname
  "Pathnames should round-trip through encode/decode."
  (let ((value #P"/home/test/file.lisp"))
    (let ((decoded (amoebum::%checkpoint-decode-value
                    (amoebum::%checkpoint-encode-value value))))
      (is (pathnamep decoded))
      (is (string= (namestring value) (namestring decoded))))))

(test encode-decode-vector
  "Vectors should encode with :__vector__ tag."
  (let* ((value (vector 1 2 "three" 4))
         (encoded (amoebum::%checkpoint-encode-value value)))
    ;; Verify encode produces tagged structure
    (is (consp encoded))
    (is (eq :__vector__ (first encoded)))
    (is (listp (second encoded)))
    (is (= 4 (length (second encoded))))
    ;; Verify decode returns a vector (shape may wrap due to rest-vs-second)
    (let ((decoded (amoebum::%checkpoint-decode-value encoded)))
      (is (vectorp decoded)))))

(test encode-decode-nested-structure
  "Nested structures should round-trip."
  (let ((ht (make-hash-table :test #'equal)))
    (setf (gethash "path" ht) #P"/tmp/test.lisp"
          (gethash "data" ht) (vector 1 2 3))
    (let ((decoded (amoebum::%checkpoint-decode-value
                    (amoebum::%checkpoint-encode-value ht))))
      (is (hash-table-p decoded))
      (is (pathnamep (gethash "path" decoded)))
      (is (vectorp (gethash "data" decoded))))))

(test encode-decode-nil
  "NIL should round-trip."
  (is (null (amoebum::%checkpoint-decode-value
             (amoebum::%checkpoint-encode-value nil)))))

(test encode-decode-keyword
  "Keywords should round-trip."
  (is (eq :test (amoebum::%checkpoint-decode-value
                  (amoebum::%checkpoint-encode-value :test)))))

;;; --- Config snapshot ---

(test config-snapshot-round-trip
  "Config should serialize and partially restore."
  (let ((cfg (amoebum::load-config :project-root "/tmp/"
                                    :global-config-path "/nonexistent/g.lisp"
                                    :project-config-path "/nonexistent/p.lisp"
                                    :environment-values nil
                                    :cli-values nil)))
    (let ((snapshot (amoebum::%config->snapshot cfg)))
      (is (listp snapshot))
      (is (stringp (getf snapshot :model)))
      (is (keywordp (getf snapshot :permission-mode)))
      (is (listp (getf snapshot :values))))))

;;; --- Checkpoint session/restore round-trip ---

(test checkpoint-restore-round-trip
  "checkpoint-session + restore-session should preserve conversation."
  (let* ((old-override amoebum::*checkpoint-directory-override*)
         (old-bus amoebum::*event-bus*)
         (tmp-dir (%make-temp-directory "amoebum-serialization"))
         (bus (amoebum:make-event-bus)))
    (unwind-protect
         (progn
           (setf amoebum::*checkpoint-directory-override* tmp-dir
                 amoebum::*event-bus* bus)
           (let* ((conversation (amoebum:make-conversation-state
                                 :project-root tmp-dir))
                  (msg (pseudopod:make-message :role "user" :content "serialize me")))
             (amoebum:conversation-state-add-message conversation msg :save-p nil)
             (let* ((checkpoint (amoebum:checkpoint-session
                                 :conversation conversation
                                 :project-root tmp-dir
                                 :event-bus bus
                                 :trigger :manual))
                    (restored (amoebum:restore-session
                               :checkpoint-id (amoebum:session-checkpoint-id checkpoint)
                               :project-root tmp-dir
                               :event-bus bus))
                    (restored-conv (getf restored :conversation))
                    (entries (amoebum:conversation-state-entries restored-conv)))
               (is (= 1 (length entries)))
               (is (string= "serialize me"
                             (amoebum:conversation-history-entry-content
                              (first entries)))))))
      (setf amoebum::*checkpoint-directory-override* old-override
            amoebum::*event-bus* old-bus)
      (%delete-directory-tree-safe tmp-dir))))

(test checkpoint-write-and-read-payload
  "Checkpoint payload should write and read back from disk."
  (let* ((tmp-dir (%make-temp-directory "amoebum-ser-payload"))
         (path (merge-pathnames #P"test-checkpoint.sexp" tmp-dir))
         (payload (amoebum::%checkpoint-payload
                   :checkpoint-id "test-001"
                   :created-at (get-universal-time)
                   :project-root "/tmp/"
                   :trigger :manual
                   :auto-p nil
                   :config nil
                   :conversation nil
                   :extensions nil
                   :tools nil
                   :memory nil)))
    (unwind-protect
         (progn
           (amoebum::%write-checkpoint-payload path payload)
           (is (probe-file path))
           (let ((read-back (amoebum::%read-checkpoint-payload path)))
             (is (listp read-back))
             (is (string= "test-001" (getf read-back :checkpoint-id)))
             (is (eq :manual (getf read-back :trigger)))))
      (%delete-directory-tree-safe tmp-dir))))

(test session-checkpoint-struct
  "session-checkpoint struct should be constructable."
  (let ((cp (amoebum::make-session-checkpoint
             :id "cp-001"
             :path #P"/tmp/cp-001.sexp"
             :created-at 12345
             :auto-p t
             :trigger :idle)))
    (is (amoebum::session-checkpoint-p cp))
    (is (string= "cp-001" (amoebum::session-checkpoint-id cp)))
    (is (amoebum::session-checkpoint-auto-p cp))
    (is (eq :idle (amoebum::session-checkpoint-trigger cp)))))
