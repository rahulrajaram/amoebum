
(progn
 (defun amoebum.tools::%exec-r013-golden-tool (arguments &optional tool-call)
   (declare (ignorable tool-call))
   (let* ((#:%raw-path (%extract-tool-argument arguments #A((4) base-char . "path")))
          (common-lisp-user::path
           (if (eq #:%raw-path +missing-tool-argument+)
               nil
               (%coerce-tool-argument #:%raw-path 'pathname 'common-lisp-user::path)))
          (#:%check-path (or (not (eq #:%raw-path +missing-tool-argument+)) nil t))
          (#:%raw-limit (%extract-tool-argument arguments #A((5) base-char . "limit")))
          (common-lisp-user::limit
           (if (eq #:%raw-limit +missing-tool-argument+)
               nil
               (%coerce-tool-argument #:%raw-limit '(or null integer) 'common-lisp-user::limit)))
          (#:%check-limit (or (not (eq #:%raw-limit +missing-tool-argument+)) t nil))
          (#:%raw-dry-run (%extract-tool-argument arguments #A((7) base-char . "dry-run")))
          (common-lisp-user::dry-run
           (if (eq #:%raw-dry-run +missing-tool-argument+)
               nil
               (%coerce-tool-argument #:%raw-dry-run 'boolean 'common-lisp-user::dry-run)))
          (#:%check-dry-run (or (not (eq #:%raw-dry-run +missing-tool-argument+)) t nil))
          (#:%raw-mode (%extract-tool-argument arguments #A((4) base-char . "mode")))
          (common-lisp-user::mode
           (if (eq #:%raw-mode +missing-tool-argument+)
               :fast
               (%coerce-tool-argument #:%raw-mode '(member :fast :slow) 'common-lisp-user::mode)))
          (#:%check-mode (or (not (eq #:%raw-mode +missing-tool-argument+)) t nil)))
     (when (eq #:%raw-path +missing-tool-argument+)
       (error "Missing required tool argument ~S for tool ~A." 'common-lisp-user::path
              #A((16) base-char . "r013-golden-tool")))
     (when #:%check-path (check-type common-lisp-user::path pathname))
     (when #:%check-limit (check-type common-lisp-user::limit (or null integer)))
     (when #:%check-dry-run (check-type common-lisp-user::dry-run boolean))
     (when #:%check-mode (check-type common-lisp-user::mode (member :fast :slow)))
     (sb-ext:with-timeout 9
       (progn
        (list :path common-lisp-user::path :limit common-lisp-user::limit :dry-run
              common-lisp-user::dry-run :mode common-lisp-user::mode)))))
 (defparameter amoebum.tools::*tool-schema-r013-golden-tool*
   (%tool-schema-from-parameter-specs
    '((:name common-lisp-user::path :type pathname :description
       #A((13) base-char . "Absolute path") :required t :default nil :default-supplied-p nil)
      (:name common-lisp-user::limit :type (or null integer) :description
       #A((10) base-char . "Line limit") :required nil :default nil :default-supplied-p t)
      (:name common-lisp-user::dry-run :type boolean :description #A((7) base-char . "Dry run")
       :required nil :default nil :default-supplied-p t)
      (:name common-lisp-user::mode :type (member :fast :slow) :description
       #A((4) base-char . "Mode") :required nil :default :fast :default-supplied-p t))))
 (let* ((toolset (%ensure-toolset))
        (previous-definition (pseudopod:find-tool toolset #A((16) base-char . "r013-golden-tool")))
        (previous-metadata (gethash #A((16) base-char . "r013-golden-tool") *tool-metadata*)))
   (%push-tool-version-to-history #A((16) base-char . "r013-golden-tool") previous-definition
                                  previous-metadata)
   (pseudopod:register-tool toolset
                            (pseudopod:make-tool-definition :name
                                                            #A((16) base-char . "r013-golden-tool")
                                                            :description "Golden macroexpand tool"
                                                            :parameters
                                                            amoebum.tools::*tool-schema-r013-golden-tool*
                                                            :fn
                                                            #'amoebum.tools::%exec-r013-golden-tool))
   (let ((new-metadata
          (make-tool-metadata :name #A((16) base-char . "r013-golden-tool") :permission ':auto
                              :dangerous-p t :category ':smoke :timeout-seconds 9 :source-file
                              #P"/dev/stdin" :source-line nil :parameter-specs
                              '((:name common-lisp-user::path :type pathname :description
                                 #A((13) base-char . "Absolute path") :required t :default nil
                                 :default-supplied-p nil)
                                (:name common-lisp-user::limit :type (or null integer) :description
                                 #A((10) base-char . "Line limit") :required nil :default nil
                                 :default-supplied-p t)
                                (:name common-lisp-user::dry-run :type boolean :description
                                 #A((7) base-char . "Dry run") :required nil :default nil
                                 :default-supplied-p t)
                                (:name common-lisp-user::mode :type (member :fast :slow)
                                 :description #A((4) base-char . "Mode") :required nil :default
                                 :fast :default-supplied-p t))
                              :defined-at (get-universal-time) :mcp-server nil)))
     (setf (gethash #A((16) base-char . "r013-golden-tool") *tool-metadata*) new-metadata)
     (when previous-definition
       (%emit-tool-redefined #A((16) base-char . "r013-golden-tool") previous-metadata
                             new-metadata))))
 'common-lisp-user::r013-golden-tool)
