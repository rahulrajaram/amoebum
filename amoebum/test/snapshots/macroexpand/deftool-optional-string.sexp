
(progn
 (defun amoebum.tools::%exec-r013-golden-tool-optional-string (arguments &optional tool-call)
   (declare (ignorable tool-call))
   (let* ((#:%raw-base-branch
           (%extract-tool-argument arguments #A((11) base-char . "base-branch")))
          (common-lisp-user::base-branch
           (if (eq #:%raw-base-branch +missing-tool-argument+)
               nil
               (%coerce-tool-argument #:%raw-base-branch '(or null string)
                                      'common-lisp-user::base-branch)))
          (#:%check-base-branch (or (not (eq #:%raw-base-branch +missing-tool-argument+)) t nil)))
     (when #:%check-base-branch (check-type common-lisp-user::base-branch (or null string)))
     (sb-ext:with-timeout 60
       (progn (list :base-branch common-lisp-user::base-branch)))))
 (defparameter amoebum.tools::*tool-schema-r013-golden-tool-optional-string*
   (%tool-schema-from-parameter-specs
    '((:name common-lisp-user::base-branch :type (or null string) :description
       #A((30) base-char . "Optional base branch override.") :required nil :default nil
       :default-supplied-p t))))
 (let* ((toolset (%ensure-toolset))
        (previous-definition
         (pseudopod:find-tool toolset #A((32) base-char . "r013-golden-tool-optional-string")))
        (previous-metadata
         (gethash #A((32) base-char . "r013-golden-tool-optional-string") *tool-metadata*)))
   (%push-tool-version-to-history #A((32) base-char . "r013-golden-tool-optional-string")
                                  previous-definition previous-metadata)
   (pseudopod:register-tool toolset
                            (pseudopod:make-tool-definition :name
                                                            #A((32) base-char
                                                               . "r013-golden-tool-optional-string")
                                                            :description
                                                            "Single optional string tool."
                                                            :parameters
                                                            amoebum.tools::*tool-schema-r013-golden-tool-optional-string*
                                                            :fn
                                                            #'amoebum.tools::%exec-r013-golden-tool-optional-string))
   (let ((new-metadata
          (make-tool-metadata :name #A((32) base-char . "r013-golden-tool-optional-string")
                              :permission ':auto :dangerous-p nil :category ':smoke
                              :timeout-seconds 60 :source-file #P"/dev/stdin" :source-line nil
                              :parameter-specs
                              '((:name common-lisp-user::base-branch :type (or null string)
                                 :description #A((30) base-char . "Optional base branch override.")
                                 :required nil :default nil :default-supplied-p t))
                              :defined-at (get-universal-time) :mcp-server nil)))
     (setf (gethash #A((32) base-char . "r013-golden-tool-optional-string") *tool-metadata*)
             new-metadata)
     (when previous-definition
       (%emit-tool-redefined #A((32) base-char . "r013-golden-tool-optional-string")
                             previous-metadata new-metadata))))
 'common-lisp-user::r013-golden-tool-optional-string)