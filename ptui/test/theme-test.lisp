(defpackage :ptui.test.theme
  (:use :cl :fiveam)
  (:export #:run-all #:theme-suite))

(in-package :ptui.test.theme)

(def-suite theme-suite
  :description "PTUI theme infrastructure tests.")

(in-suite theme-suite)

;;; --- Struct creation ---

(test role-style-creation
  (let ((style (ptui.core.theme:make-role-style
                :fg (ptui.core.color:make-color-rgb 100 200 150)
                :boldp t)))
    (is (ptui.core.theme:role-style-boldp style))
    (is (not (ptui.core.theme:role-style-italicp style)))
    (is (typep (ptui.core.theme:role-style-fg style) 'ptui.core.color:color-rgb))
    (is (null (ptui.core.theme:role-style-bg style)))))

(test gradient-spec-creation
  (let ((g (ptui.core.theme:make-gradient-spec
            :start (ptui.core.color:make-color-rgb 0 0 0)
            :stop (ptui.core.color:make-color-rgb 255 255 255))))
    (is (typep (ptui.core.theme:gradient-spec-start g) 'ptui.core.color:color-rgb))
    (is (typep (ptui.core.theme:gradient-spec-stop g) 'ptui.core.color:color-rgb))))

(test markdown-config-creation
  (let ((mc (ptui.core.theme:make-markdown-config
             :code-background (ptui.core.color:make-color-rgb 36 42 54)
             :heading-color (ptui.core.color:make-color-rgb 122 162 247))))
    (is (typep (ptui.core.theme:markdown-config-code-background mc) 'ptui.core.color:color-rgb))
    (is (typep (ptui.core.theme:markdown-config-heading-color mc) 'ptui.core.color:color-rgb))
    (is (null (ptui.core.theme:markdown-config-fence-color mc)))))

;;; --- Theme creation and registry ---

(test theme-manual-creation-and-registry
  (let ((roles (make-hash-table :test #'eq)))
    (setf (gethash :test-role roles)
          (ptui.core.theme:make-role-style
           :fg (ptui.core.color:make-color-rgb 255 0 0) :boldp t))
    (let ((theme (ptui.core.theme:make-theme
                  :name 'test-manual-theme
                  :background (ptui.core.color:make-color-rgb 10 10 10)
                  :foreground (ptui.core.color:make-color-rgb 200 200 200)
                  :roles roles)))
      (ptui.core.theme:register-theme theme)
      (is (eq theme (ptui.core.theme:find-theme 'test-manual-theme)))
      (is (eq (ptui.core.theme:theme-name theme) 'test-manual-theme))
      ;; cleanup
      (remhash 'test-manual-theme ptui.core.theme:*theme-registry*))))

;;; --- Parent inheritance ---

(test theme-parent-role-resolution
  (let ((parent-roles (make-hash-table :test #'eq))
        (child-roles (make-hash-table :test #'eq)))
    (setf (gethash :shared parent-roles)
          (ptui.core.theme:make-role-style
           :fg (ptui.core.color:make-color-rgb 100 100 100)))
    (setf (gethash :only-parent parent-roles)
          (ptui.core.theme:make-role-style
           :fg (ptui.core.color:make-color-rgb 50 50 50)))
    (setf (gethash :shared child-roles)
          (ptui.core.theme:make-role-style
           :fg (ptui.core.color:make-color-rgb 200 200 200)))
    (let* ((parent (ptui.core.theme:make-theme :name 'test-parent :roles parent-roles))
           (child (ptui.core.theme:make-theme :name 'test-child
                                              :parent parent
                                              :roles child-roles)))
      ;; child overrides :shared
      (let ((resolved (ptui.core.theme:theme-resolve-role child :shared)))
        (is (= 200 (ptui.core.color:color-rgb-r
                     (ptui.core.theme:role-style-fg resolved)))))
      ;; child inherits :only-parent from parent
      (let ((resolved (ptui.core.theme:theme-resolve-role child :only-parent)))
        (is (= 50 (ptui.core.color:color-rgb-r
                    (ptui.core.theme:role-style-fg resolved)))))
      ;; unknown role returns nil
      (is (null (ptui.core.theme:theme-resolve-role child :nonexistent)))
      ;; cleanup
      (remhash 'test-parent ptui.core.theme:*theme-registry*)
      (remhash 'test-child ptui.core.theme:*theme-registry*))))

;;; --- Gradient interpolation ---

(test gradient-color-at-midpoint
  (let ((g (ptui.core.theme:make-gradient-spec
            :start (ptui.core.color:make-color-rgb 0 0 0)
            :stop (ptui.core.color:make-color-rgb 200 100 50))))
    (let ((mid (ptui.core.theme:gradient-color-at g 0.5)))
      (is (= 100 (ptui.core.color:color-rgb-r mid)))
      (is (= 50 (ptui.core.color:color-rgb-g mid)))
      (is (= 25 (ptui.core.color:color-rgb-b mid))))))

(test gradient-color-at-boundaries
  (let ((g (ptui.core.theme:make-gradient-spec
            :start (ptui.core.color:make-color-rgb 10 20 30)
            :stop (ptui.core.color:make-color-rgb 200 210 220))))
    (let ((at-zero (ptui.core.theme:gradient-color-at g 0.0))
          (at-one (ptui.core.theme:gradient-color-at g 1.0)))
      (is (= 10 (ptui.core.color:color-rgb-r at-zero)))
      (is (= 200 (ptui.core.color:color-rgb-r at-one))))))

;;; --- theme-role-cell ---

(test theme-role-cell-returns-cell
  (let ((roles (make-hash-table :test #'eq)))
    (setf (gethash :test-role roles)
          (ptui.core.theme:make-role-style
           :fg (ptui.core.color:make-color-rgb 255 128 64)
           :boldp t))
    (let ((theme (ptui.core.theme:make-theme :name 'test-cell-theme :roles roles)))
      (let ((cell (ptui.core.theme:theme-role-cell theme :test-role)))
        (is (typep cell 'ptui.core.types:cell))
        (is (= 255 (ptui.core.color:color-rgb-r (ptui.core.types:cell-fg cell))))
        (is (ptui.core.types:attrs-boldp (ptui.core.types:cell-attrs cell))))
      ;; unknown role uses fallback
      (let ((cell (ptui.core.theme:theme-role-cell theme :unknown)))
        (is (typep cell 'ptui.core.types:cell))
        (is (= 175 (ptui.core.color:color-rgb-r (ptui.core.types:cell-fg cell)))))
      ;; cleanup
      (remhash 'test-cell-theme ptui.core.theme:*theme-registry*))))

;;; --- define-theme macro ---

(test define-theme-macro-expansion
  (let ((expanded (macroexpand-1
                   '(ptui.core.theme:define-theme test-macro-theme
                      (:base :dark)
                      (:background (rgb 20 20 20))
                      (:roles
                        (:test-role :fg (rgb 255 0 0) :boldp t))))))
    (is (consp expanded))
    (is (eq 'progn (first expanded)))))

;;; --- Built-in :dark theme ---

(test dark-base-theme-exists
  (let ((dark (ptui.core.theme:find-theme :dark)))
    (is (not (null dark)))
    (is (eq :dark (ptui.core.theme:theme-name dark)))
    ;; has some basic roles
    (is (not (null (ptui.core.theme:theme-resolve-role dark :system))))
    (is (not (null (ptui.core.theme:theme-resolve-role dark :warning))))))

(test dark-theme-primitives-include-row-styling
  (let ((dark (ptui.core.theme:find-theme :dark)))
    (is (not (null (ptui.core.theme:theme-resolve-role dark :badge))))
    (is (not (null (ptui.core.theme:theme-resolve-role dark :badge-muted))))
    (is (not (null (ptui.core.theme:theme-resolve-role dark :gutter))))
    (is (not (null (ptui.core.theme:theme-resolve-role dark :gutter-selected))))
    (is (not (null (ptui.core.theme:theme-resolve-role dark :tree-row))))
    (is (not (null (ptui.core.theme:theme-resolve-role dark :tree-connector))))
    (is (not (null (ptui.core.theme:theme-resolve-role dark :block-key))))
    (is (not (null (ptui.core.theme:theme-resolve-role dark :block-value))))))

;;; --- Suite runner ---

(defun run-all ()
  (let ((results (run 'theme-suite)))
    (explain! results)
    (results-status results)))
