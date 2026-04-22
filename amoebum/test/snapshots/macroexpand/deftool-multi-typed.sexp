
(progn
 (defun amoebum.tools::%exec-r013-golden-tool-multi-typed (arguments &optional tool-call)
   (declare (ignorable tool-call))
   (let* ((#:%raw-files (%extract-tool-argument arguments #A((5) base-char . "files")))
          (common-lisp-user::files
           (if (eq #:%raw-files +missing-tool-argument+)
               nil
               (%coerce-tool-argument #:%raw-files '(or null list string)
                                      'common-lisp-user::files)))
          (#:%check-files (or (not (eq #:%raw-files +missing-tool-argument+)) t nil))
          (#:%raw-model (%extract-tool-argument arguments #A((5) base-char . "model")))
          (common-lisp-user::model
           (if (eq #:%raw-model +missing-tool-argument+)
               nil
               (%coerce-tool-argument #:%raw-model '(or null string) 'common-lisp-user::model)))
          (#:%check-model (or (not (eq #:%raw-model +missing-tool-argument+)) t nil))
          (#:%raw-amend (%extract-tool-argument arguments #A((5) base-char . "amend")))
          (common-lisp-user::amend
           (if (eq #:%raw-amend +missing-tool-argument+)
               nil
               (%coerce-tool-argument #:%raw-amend 'boolean 'common-lisp-user::amend)))
          (#:%check-amend (or (not (eq #:%raw-amend +missing-tool-argument+)) t nil))
          (#:%raw-allow-amend
           (%extract-tool-argument arguments #A((11) base-char . "allow-amend")))
          (common-lisp-user::allow-amend
           (if (eq #:%raw-allow-amend +missing-tool-argument+)
               nil
               (%coerce-tool-argument #:%raw-allow-amend 'boolean 'common-lisp-user::allow-amend)))
          (#:%check-allow-amend (or (not (eq #:%raw-allow-amend +missing-tool-argument+)) t nil)))
     (when #:%check-files (check-type common-lisp-user::files (or null list string)))
     (when #:%check-model (check-type common-lisp-user::model (or null string)))
     (when #:%check-amend (check-type common-lisp-user::amend boolean))
     (when #:%check-allow-amend (check-type common-lisp-user::allow-amend boolean))
     (sb-ext:with-timeout 180
       (progn
        (list :files common-lisp-user::files :model common-lisp-user::model :amend
              common-lisp-user::amend :allow-amend common-lisp-user::allow-amend)))))
 (defparameter amoebum.tools::*tool-schema-r013-golden-tool-multi-typed*
   (%tool-schema-from-parameter-specs
    '((:name common-lisp-user::files :type (or null list string) :description
       #A((24) base-char . "Optional explicit files.") :required nil :default nil
       :default-supplied-p t)
      (:name common-lisp-user::model :type (or null string) :description
       #A((15) base-char . "Model override.") :required nil :default nil :default-supplied-p t)
      (:name common-lisp-user::amend :type boolean :description
       #A((19) base-char . "Request amend mode.") :required nil :default nil :default-supplied-p t)
      (:name common-lisp-user::allow-amend :type boolean :description
       #A((14) base-char . "Confirm amend.") :required nil :default nil :default-supplied-p t))))
 (let* ((toolset (%ensure-toolset))
        (previous-definition
         (pseudopod:find-tool toolset #A((28) base-char . "r013-golden-tool-multi-typed")))
        (previous-metadata
         (gethash #A((28) base-char . "r013-golden-tool-multi-typed") *tool-metadata*)))
   (%push-tool-version-to-history #A((28) base-char . "r013-golden-tool-multi-typed")
                                  previous-definition previous-metadata)
   (pseudopod:register-tool toolset
                            (pseudopod:make-tool-definition :name
                                                            #A((28) base-char
                                                               . "r013-golden-tool-multi-typed")
                                                            :description
                                                            "Dangerous multi-typed tool."
                                                            :parameters
                                                            amoebum.tools::*tool-schema-r013-golden-tool-multi-typed*
                                                            :fn
                                                            #'amoebum.tools::%exec-r013-golden-tool-multi-typed))
   (let ((new-metadata
          (make-tool-metadata :name #A((28) base-char . "r013-golden-tool-multi-typed") :permission
                              ':full-auto :dangerous-p t :category ':smoke :timeout-seconds 180
                              :source-file #P"/dev/stdin" :source-line nil :parameter-specs
                              '((:name common-lisp-user::files :type (or null list string)
                                 :description #A((24) base-char . "Optional explicit files.")
                                 :required nil :default nil :default-supplied-p t)
                                (:name common-lisp-user::model :type (or null string) :description
                                 #A((15) base-char . "Model override.") :required nil :default nil
                                 :default-supplied-p t)
                                (:name common-lisp-user::amend :type boolean :description
                                 #A((19) base-char . "Request amend mode.") :required nil :default
                                 nil :default-supplied-p t)
                                (:name common-lisp-user::allow-amend :type boolean :description
                                 #A((14) base-char . "Confirm amend.") :required nil :default nil
                                 :default-supplied-p t))
                              :defined-at (get-universal-time) :mcp-server nil)))
     (setf (gethash #A((28) base-char . "r013-golden-tool-multi-typed") *tool-metadata*)
             new-metadata)
     (when previous-definition
       (%emit-tool-redefined #A((28) base-char . "r013-golden-tool-multi-typed") previous-metadata
                             new-metadata))))
 'common-lisp-user::r013-golden-tool-multi-typed)