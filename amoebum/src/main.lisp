(in-package :amoebum)

(defun main (&rest argv)
  (activate-amoebum-readtable)
  (reload-config :cli-arguments argv)
  (run-chat-ui :backend :auto :fps 20))
