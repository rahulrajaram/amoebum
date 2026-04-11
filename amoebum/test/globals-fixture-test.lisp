(in-package :amoebum/test)

(def-suite globals-fixture-suite
  :description "NXT-263 with-clean-amoebum-globals test isolation fixture."
  :in amoebum-suite)

(in-suite globals-fixture-suite)

(defun %capture-amoebum-globals ()
  "Return a plist of the six test-sensitive amoebum globals' current values."
  (list :toolset amoebum:*toolset*
        :tool-metadata amoebum:*tool-metadata*
        :tool-history amoebum:*tool-history*
        :memory-backend amoebum:*memory-backend*
        :event-bus amoebum:*event-bus*
        :checkpoint-directory-override amoebum.sessions:*checkpoint-directory-override*))

(defun %globals-match-p (snapshot)
  "Return T if every saved value in SNAPSHOT is `eq' to its live counterpart."
  (and (eq (getf snapshot :toolset) amoebum:*toolset*)
       (eq (getf snapshot :tool-metadata) amoebum:*tool-metadata*)
       (eq (getf snapshot :tool-history) amoebum:*tool-history*)
       (eq (getf snapshot :memory-backend) amoebum:*memory-backend*)
       (eq (getf snapshot :event-bus) amoebum:*event-bus*)
       (eq (getf snapshot :checkpoint-directory-override)
           amoebum.sessions:*checkpoint-directory-override*)))

(test with-clean-amoebum-globals-restores-on-success
  "Mutating every guarded global inside the fixture leaves them untouched after exit."
  (let ((snapshot (%capture-amoebum-globals)))
    (amoebum.test-support.globals-fixture:with-clean-amoebum-globals
      (setf amoebum:*toolset* (cons :mutated-toolset nil)
            amoebum:*tool-metadata* (cons :mutated-tool-metadata nil)
            amoebum:*tool-history* (cons :mutated-tool-history nil)
            amoebum:*memory-backend* (cons :mutated-memory-backend nil)
            amoebum:*event-bus* (cons :mutated-event-bus nil)
            amoebum.sessions:*checkpoint-directory-override* (cons :mutated-checkpoint-dir nil))
      ;; Sanity: inside the body the mutations are visible.
      (is (not (%globals-match-p snapshot))))
    (is (eq (getf snapshot :toolset) amoebum:*toolset*))
    (is (eq (getf snapshot :tool-metadata) amoebum:*tool-metadata*))
    (is (eq (getf snapshot :tool-history) amoebum:*tool-history*))
    (is (eq (getf snapshot :memory-backend) amoebum:*memory-backend*))
    (is (eq (getf snapshot :event-bus) amoebum:*event-bus*))
    (is (eq (getf snapshot :checkpoint-directory-override)
            amoebum.sessions:*checkpoint-directory-override*))
    (is (%globals-match-p snapshot))))

(test with-clean-amoebum-globals-restores-on-error
  "Globals are still restored when BODY signals an error."
  (let ((snapshot (%capture-amoebum-globals)))
    (handler-case
        (amoebum.test-support.globals-fixture:with-clean-amoebum-globals
          (setf amoebum:*toolset* (cons :mutated-toolset nil)
                amoebum:*tool-metadata* (cons :mutated-tool-metadata nil)
                amoebum:*tool-history* (cons :mutated-tool-history nil)
                amoebum:*memory-backend* (cons :mutated-memory-backend nil)
                amoebum:*event-bus* (cons :mutated-event-bus nil)
                amoebum.sessions:*checkpoint-directory-override*
                (cons :mutated-checkpoint-dir nil))
          (error "intentional failure inside with-clean-amoebum-globals"))
      (error (c)
        (declare (ignore c))
        nil))
    (is (eq (getf snapshot :toolset) amoebum:*toolset*))
    (is (eq (getf snapshot :tool-metadata) amoebum:*tool-metadata*))
    (is (eq (getf snapshot :tool-history) amoebum:*tool-history*))
    (is (eq (getf snapshot :memory-backend) amoebum:*memory-backend*))
    (is (eq (getf snapshot :event-bus) amoebum:*event-bus*))
    (is (eq (getf snapshot :checkpoint-directory-override)
            amoebum.sessions:*checkpoint-directory-override*))
    (is (%globals-match-p snapshot))))
