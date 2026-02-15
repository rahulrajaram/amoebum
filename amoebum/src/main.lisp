(in-package :amoebum)

(defun main (&rest argv)
  (declare (ignore argv))
  (run-chat-ui :backend :auto :fps 20))
