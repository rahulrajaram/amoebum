(in-package :amoebum/test)

(def-suite deftool-dangerous-permission-suite
  :description "DEFTool dangerous/permission compile-time validation tests (I203)."
  :in amoebum-suite)

(in-suite deftool-dangerous-permission-suite)

(defun %warning-mentions-dangerous-auto-permission-p (condition)
  (let ((text (princ-to-string condition)))
    (or (and (search "dangerous" text :test #'char-equal)
             (search ":permission :auto" text :test #'char-equal))
        (and (search "Dangerous tool declared with :permission :auto" text :test #'char-equal)
             (search ":supervised" text :test #'char-equal)))))

(test dangerous-auto-permission-signals-style-warning
  (amoebum::reset-deftool-compile-validation-state)
  (let ((warning-seen nil))
    (handler-bind
        ((warning
           (lambda (condition)
             (when (and (typep condition 'amoebum::dangerous-auto-permission)
                        (%warning-mentions-dangerous-auto-permission-p condition))
               (setf warning-seen t))
             (let ((restart (find-restart 'muffle-warning condition)))
               (when restart
                 (invoke-restart restart))))))
      (macroexpand-1
       '(amoebum:deftool i203-dangerous-auto-warning
         ((path pathname :description "Path to process" :required t))
         "I203 dangerous/auto warning probe."
         (:permission :auto)
         (:dangerous t)
         path)))
    (is-true warning-seen
             "Expected DEFTTOOL to warn when :dangerous t is combined with :permission :auto.")))

(test dangerous-tools-default-to-supervised-permission
  (let ((original-toolset amoebum:*toolset*)
        (original-metadata (alexandria:copy-hash-table amoebum::*tool-metadata*)))
    (unwind-protect
         (progn
           (amoebum::reset-deftool-compile-validation-state)
           (setf amoebum::*toolset* (pseudopod:make-toolset))
           (clrhash amoebum::*tool-metadata*)
           (eval
            '(amoebum:deftool i203-dangerous-default-supervised
              ((path pathname :description "Path to process" :required t))
              "I203 dangerous default permission probe."
              (:dangerous t)
              path))
           (let ((metadata (gethash "i203-dangerous-default-supervised" amoebum::*tool-metadata*)))
             (is-true metadata
                      "Expected tool metadata to be registered for i203-dangerous-default-supervised.")
             (is (eq (amoebum::tool-metadata-permission metadata) :supervised)
                 "Expected dangerous DEFTTOOL declaration to default permission to :supervised.")))
      (setf amoebum:*toolset* original-toolset
            amoebum::*tool-metadata* original-metadata))))
