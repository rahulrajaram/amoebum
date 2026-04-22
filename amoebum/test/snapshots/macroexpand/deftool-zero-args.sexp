
(progn
 (defun amoebum.tools::%exec-r013-golden-tool-zero-args (arguments &optional tool-call)
   (declare (ignorable tool-call))
   (let* ()
     (sb-ext:with-timeout 30
       (progn (list :ok t)))))
 (defparameter amoebum.tools::*tool-schema-r013-golden-tool-zero-args*
   (%tool-schema-from-parameter-specs 'nil))
 (let* ((toolset (%ensure-toolset))
        (previous-definition
         (pseudopod:find-tool toolset #A((26) base-char . "r013-golden-tool-zero-args")))
        (previous-metadata
         (gethash #A((26) base-char . "r013-golden-tool-zero-args") *tool-metadata*)))
   (%push-tool-version-to-history #A((26) base-char . "r013-golden-tool-zero-args")
                                  previous-definition previous-metadata)
   (pseudopod:register-tool toolset
                            (pseudopod:make-tool-definition :name
                                                            #A((26) base-char
                                                               . "r013-golden-tool-zero-args")
                                                            :description "Zero-argument tool."
                                                            :parameters
                                                            amoebum.tools::*tool-schema-r013-golden-tool-zero-args*
                                                            :fn
                                                            #'amoebum.tools::%exec-r013-golden-tool-zero-args))
   (let ((new-metadata
          (make-tool-metadata :name #A((26) base-char . "r013-golden-tool-zero-args") :permission
                              ':auto :dangerous-p nil :category ':smoke :timeout-seconds 30
                              :source-file #P"/dev/stdin" :source-line nil :parameter-specs 'nil
                              :defined-at (get-universal-time) :mcp-server nil)))
     (setf (gethash #A((26) base-char . "r013-golden-tool-zero-args") *tool-metadata*)
             new-metadata)
     (when previous-definition
       (%emit-tool-redefined #A((26) base-char . "r013-golden-tool-zero-args") previous-metadata
                             new-metadata))))
 'common-lisp-user::r013-golden-tool-zero-args)