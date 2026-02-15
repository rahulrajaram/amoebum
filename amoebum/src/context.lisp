(in-package :amoebum)

(defparameter +default-context-window-limit+ 128000)

(defparameter +model-context-window-limits+
  '(("moonshot-v1-8k" . 8000)
    ("moonshot-v1-32k" . 32000)
    ("moonshot-v1-128k" . 128000)))

(defparameter +context-budget-green-threshold-percent+ 50)
(defparameter +context-budget-yellow-threshold-percent+ 80)
(defparameter +context-budget-compress-threshold-percent+ 80)

(defparameter *context-window-limit* nil
  "Optional runtime override for context window size. NIL defers to model limits.")

(defun %safe-model-name (model-name)
  (cond
    ((and (stringp model-name) (plusp (length model-name)))
     (string-downcase model-name))
    ((null model-name)
     "")
    (t
     (string-downcase (princ-to-string model-name)))))

(defun %context-digit-run-end (text start)
  (let ((end start)
        (length (length text)))
    (loop while (and (< end length)
                     (digit-char-p (char text end)))
          do (incf end))
    end))

(defun %infer-limit-from-model-name (model-name)
  (let* ((text (%safe-model-name model-name))
         (length (length text)))
    (loop for start from 0 below length do
      (when (digit-char-p (char text start))
        (let ((end (%context-digit-run-end text start)))
          (when (< end length)
            (let ((unit (char text end)))
              (when (member unit '(#\k #\m) :test #'char=)
                (handler-case
                    (let ((number (parse-integer text :start start :end end))
                          (factor (if (char= unit #\k) 1000 1000000)))
                      (return (* number factor)))
                  (error ()
                    nil))))))))))

(defun context-window-limit-for-model (model-name
                                       &key (default +default-context-window-limit+))
  (let* ((normalized (%safe-model-name model-name))
         (exact (assoc normalized +model-context-window-limits+ :test #'string=)))
    (or (and exact (cdr exact))
        (%infer-limit-from-model-name normalized)
        default)))

(defun resolve-context-window-limit (&key model config-limit)
  (cond
    ((and (integerp config-limit) (> config-limit 0))
     config-limit)
    ((and (integerp *context-window-limit*) (> *context-window-limit* 0))
     *context-window-limit*)
    (t
     (context-window-limit-for-model model))))

(defun context-usage-percent (used-tokens limit-tokens)
  (let ((used (if (and (integerp used-tokens) (> used-tokens 0))
                  used-tokens
                  0))
        (limit (if (and (integerp limit-tokens) (> limit-tokens 0))
                   limit-tokens
                   0)))
    (if (<= limit 0)
        0
        (min 999 (floor (/ (* 100.0d0 used) limit))))))

(defun context-usage-level (used-tokens limit-tokens)
  (let ((percent (context-usage-percent used-tokens limit-tokens)))
    (cond
      ((< percent +context-budget-green-threshold-percent+)
       :green)
      ((<= percent +context-budget-yellow-threshold-percent+)
       :yellow)
      (t
       :red))))

(defun context-compression-required-p (used-tokens limit-tokens)
  (let ((used (if (and (integerp used-tokens) (> used-tokens 0))
                  used-tokens
                  0))
        (limit (if (and (integerp limit-tokens) (> limit-tokens 0))
                   limit-tokens
                   0)))
    (and (> limit 0)
         (> (* used 100) (* limit +context-budget-compress-threshold-percent+)))))

(defun %context-blank-string-p (text)
  (or (null text)
      (zerop (length text))
      (every (lambda (char)
               (member char '(#\Space #\Tab #\Newline #\Return) :test #'char=))
             text)))

(defun %count-string-tokens (text)
  (if (%context-blank-string-p text)
      0
      (ceiling (length text) 4)))

(defun %context-content-part-text (part)
  (let ((type (string-downcase (or (pseudopod:content-part-type part) "text"))))
    (cond
      ((string= type "text")
       (or (pseudopod:content-part-text part) ""))
      ((string= type "think")
       (or (pseudopod:content-part-think part) ""))
      (t
       (or (pseudopod:content-part-text part)
           (pseudopod:content-part-think part)
           "")))))

(defun %message-content-text (message)
  (let ((parts (pseudopod:message-content message)))
    (if (null parts)
        ""
        (with-output-to-string (stream)
          (loop for part in parts
                for index from 0 do
                  (when (> index 0)
                    (write-char #\Newline stream))
                  (write-string (%context-content-part-text part) stream))))))

(defun count-tokens (value &key model)
  (declare (ignore model))
  (cond
    ((null value)
     0)
    ((stringp value)
     (%count-string-tokens value))
    ((pseudopod:message-p value)
     (%count-string-tokens (%message-content-text value)))
    ((listp value)
     (loop for entry in value
           sum (count-tokens entry)))
    (t
     (%count-string-tokens (princ-to-string value)))))
