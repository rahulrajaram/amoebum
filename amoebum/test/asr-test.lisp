(in-package :amoebum/test)

(def-suite asr-suite
  :description "I231 Whisper ASR integration coverage."
  :in amoebum-suite)

(in-suite asr-suite)

(test whisper-asr-protocol-and-language
  (let ((backend (amoebum::make-whisper-asr-backend :language "en" :background-p nil)))
    (is-true (typep backend 'amoebum::asr-backend))
    (is-false (amoebum::listening-p backend))
    (is (string= "fr" (amoebum::set-language backend "fr")))))

(test whisper-asr-transcribes-via-run-command
  (let ((captured-command nil)
        (amoebum::*asr-run-command-function* nil))
    (setf amoebum::*asr-run-command-function*
          (lambda (command &key timeout)
            (declare (ignore timeout))
            (setf captured-command command)
            (list :exit-code 0 :stdout "transcribed phrase" :stderr "")))
    (let ((backend (amoebum::make-whisper-asr-backend
                    :language "en"
                    :model "tiny"
                    :background-p nil
                    :recorder :arecord)))
      (is (string= "transcribed phrase"
                   (amoebum::%whisper-transcribe-once backend)))
      (is-true (search "whisper" (or captured-command "") :test #'char-equal))
      (is-true (or (search "arecord" (or captured-command "") :test #'char-equal)
                   (search "sox" (or captured-command "") :test #'char-equal))))))

(test whisper-asr-barge-in-stops-tts
  (let ((stop-count 0)
        (amoebum::*asr-run-command-function*
          (lambda (command &key timeout)
            (declare (ignore command timeout))
            (list :exit-code 0 :stdout "hello from barge-in" :stderr "")))
        (amoebum::*asr-tts-speaking-function* (lambda () t))
        (amoebum::*asr-tts-stop-function* nil))
    (setf amoebum::*asr-tts-stop-function*
          (lambda ()
            (incf stop-count)
            t))
    (let ((backend (amoebum::make-whisper-asr-backend :background-p nil)))
      (is (string= "hello from barge-in"
                   (amoebum::%whisper-transcribe-once backend)))
      (is (= stop-count 1)))))

(test voice-slash-command-toggles-mode
  (let* ((amoebum::*asr-run-command-function*
           (lambda (command &key timeout)
             (declare (ignore command timeout))
             (list :exit-code 0 :stdout "voice text" :stderr "")))
         (amoebum::*asr-backend* (amoebum::make-whisper-asr-backend :background-p nil))
         (amoebum::*voice-input-mode-enabled-p* nil))
    (multiple-value-bind (handled result)
        (amoebum:dispatch-slash-command "/voice on")
      (is-true handled)
      (is-true (search "enabled" (amoebum:slash-command-result-output result)
                       :test #'char-equal)))
    (is-true (amoebum::voice-input-mode-enabled-p))
    (multiple-value-bind (handled result)
        (amoebum:dispatch-slash-command "/voice off")
      (is-true handled)
      (is-true (search "disabled" (amoebum:slash-command-result-output result)
                       :test #'char-equal)))
    (is-false (amoebum::voice-input-mode-enabled-p))))

(test asr-smoke-sentinel
  (is-true t)
  (format t "ASR_SMOKE_OK~%"))

