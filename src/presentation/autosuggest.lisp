(in-package #:nshell.presentation)
(defun compute-suggestion (history input)
  (let ((suggestion (nshell.application:history-suggestion history input)))
    (when suggestion
      (format t "~C[2m~a~C[0m" #\Esc suggestion #\Esc))
    suggestion))
(defun accept-suggestion (input suggestion)
  (concatenate 'string input suggestion))
