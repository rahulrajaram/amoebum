(in-package :amoebum/test)

(def-suite compile-validation-conditions-suite :in amoebum-suite
  :description "Compile-time macro condition coverage (I205).")

(in-suite compile-validation-conditions-suite)

(test compile-validation-condition-types-are-defined
  (is (subtypep 'amoebum::tool-definition-warning 'style-warning))
  (is (subtypep 'amoebum::unmapped-type-warning 'amoebum::tool-definition-warning))
  (is (subtypep 'amoebum::missing-tool-description 'amoebum::tool-definition-warning))
  (is (subtypep 'amoebum::duplicate-tool-name 'amoebum::tool-definition-warning))
  (is (subtypep 'amoebum::dangerous-auto-permission 'amoebum::tool-definition-warning))
  (is (subtypep 'amoebum::unknown-tool-reference 'amoebum::tool-definition-warning))
  (is (subtypep 'amoebum::invalid-permission-mode 'error)))

(test deftool-compile-validation-signals-consolidated-conditions
  (let ((saw-missing nil)
        (saw-duplicate nil)
        (saw-dangerous-auto nil))
    (handler-bind ((amoebum::missing-tool-description
                     (lambda (condition)
                       (setf saw-missing t)
                       (let ((restart (find-restart 'muffle-warning condition)))
                         (when restart
                           (invoke-restart restart)))))
                   (amoebum::duplicate-tool-name
                     (lambda (condition)
                       (setf saw-duplicate t)
                       (let ((restart (find-restart 'muffle-warning condition)))
                         (when restart
                           (invoke-restart restart)))))
                   (amoebum::dangerous-auto-permission
                     (lambda (condition)
                       (setf saw-dangerous-auto t)
                       (let ((restart (find-restart 'muffle-warning condition)))
                         (when restart
                           (invoke-restart restart))))))
      (amoebum::reset-deftool-compile-validation-state)
      (macroexpand-1
       '(amoebum:deftool i205-condition-tool-1 ((path pathname :required t))
          (:permission :auto)
          (:dangerous t)
          :ok))
      (macroexpand-1
       '(amoebum:deftool i205-condition-tool-1 ((path pathname :required t))
          "duplicate declaration"
          :ok)))
    (is-true saw-missing)
    (is-true saw-duplicate)
    (is-true saw-dangerous-auto)))

(test deftool-invalid-permission-signals-invalid-permission-mode
  (signals amoebum::invalid-permission-mode
    (macroexpand-1
     '(amoebum:deftool i205-invalid-permission ((arg string))
        (:permission :not-a-mode)
        "invalid permission"
        :ok))))

(test defhook-unknown-tool-reference-signals-consolidated-warning
  (let ((saw-warning nil))
    (handler-bind ((amoebum::unknown-tool-reference
                     (lambda (condition)
                       (setf saw-warning t)
                       (let ((restart (find-restart 'muffle-warning condition)))
                         (when restart
                           (invoke-restart restart))))))
      (amoebum::reset-deftool-compile-validation-state)
      (macroexpand-1
       '(amoebum:defhook :pre-tool-use (tool-name args)
          (:match (:tool "tool-not-defined-yet")
            :ok))))
    (is-true saw-warning)))
