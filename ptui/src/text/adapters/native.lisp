(defpackage :ptui.text.adapter.native
  (:use :cl)
  (:export
   #:+native-enable-env-var+
   #:+native-require-parity-env-var+
   #:*native-grapheme-support-p*
   #:*native-width-support-p*
   #:native-feature-enabled-p
   #:native-parity-required-p
   #:native-runtime-contract-satisfied-p
   #:native-parity-check-p
   #:native-engine-available-p
   #:split-graphemes
   #:codepoint-width))

(in-package :ptui.text.adapter.native)

(defparameter +native-enable-env-var+ "PTUI_TEXT_NATIVE_ENABLE"
  "Feature flag that allows :native activation when contract checks pass.")

(defparameter +native-require-parity-env-var+ "PTUI_TEXT_NATIVE_REQUIRE_PARITY"
  "If truthy, :native requires fallback/native parity checks to pass before activation.")

(defparameter *native-grapheme-support-p* nil
  "Set true when a native grapheme segmenter is wired.")

(defparameter *native-width-support-p* nil
  "Set true when a native width engine is wired.")

(defparameter *native-enable-override* :unset
  "Test override for PTUI_TEXT_NATIVE_ENABLE (:unset means env-driven).")

(defparameter *native-require-parity-override* :unset
  "Test override for PTUI_TEXT_NATIVE_REQUIRE_PARITY (:unset means env-driven).")

(defun %string-from-codepoints (&rest codepoints)
  (coerce (mapcar #'code-char codepoints) 'string))

(defparameter *native-parity-corpus*
  (list
   "abc"
   (%string-from-codepoints #x0065 #x0301)
   (%string-from-codepoints #x1F1FA #x1F1F8)
   (%string-from-codepoints #x1F468 #x200D #x1F469 #x200D #x1F467 #x200D #x1F466)
   (%string-from-codepoints #x0041 #x754C #x0042)
   (%string-from-codepoints #x0023 #xFE0F #x20E3))
  "Fixed corpus used to smoke-test fallback/native parity before activation.")

(defun %truthy-env-value-p (value)
  (and value
       (member (string-downcase (string-trim '(#\Space #\Tab #\Newline #\Return) value))
               '("1" "true" "yes" "on")
               :test #'string=)))

(defun native-feature-enabled-p (&key (env #'uiop:getenv))
  (if (eq *native-enable-override* :unset)
      (%truthy-env-value-p (funcall env +native-enable-env-var+))
      (not (null *native-enable-override*))))

(defun native-parity-required-p (&key (env #'uiop:getenv))
  (if (eq *native-require-parity-override* :unset)
      (%truthy-env-value-p (funcall env +native-require-parity-env-var+))
      (not (null *native-require-parity-override*))))

(defun native-runtime-contract-satisfied-p ()
  (and *native-grapheme-support-p*
       *native-width-support-p*))

(defun split-graphemes (text)
  "Temporary native adapter placeholder: delegates to fallback behavior."
  (ptui.text.adapter.fallback:split-graphemes text))

(defun codepoint-width (ch)
  "Temporary native adapter placeholder: delegates to fallback behavior."
  (ptui.text.adapter.fallback:codepoint-width ch))

(defun native-parity-check-p (&key (corpus *native-parity-corpus*))
  (every (lambda (text)
           (check-type text string)
           (and (equal (split-graphemes text)
                       (ptui.text.adapter.fallback:split-graphemes text))
                (loop for ch across text
                      always (= (codepoint-width ch)
                                (ptui.text.adapter.fallback:codepoint-width ch)))))
         corpus))

(defun native-engine-available-p (&key (env #'uiop:getenv))
  "Activation contract:
1) PTUI_TEXT_NATIVE_ENABLE must be truthy.
2) Native grapheme + width hooks must both be wired.
3) Optional parity gate (PTUI_TEXT_NATIVE_REQUIRE_PARITY) must pass."
  (and (native-feature-enabled-p :env env)
       (native-runtime-contract-satisfied-p)
       (or (not (native-parity-required-p :env env))
           (native-parity-check-p))))

(eval-when (:load-toplevel :execute)
  (ptui.text.engine:register-text-engine
   (ptui.text.engine:make-text-engine-adapter
    :native
    #'split-graphemes
    #'codepoint-width
    :available-p-fn #'native-engine-available-p)))
