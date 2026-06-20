;;; REPL input-state helpers
(in-package #:nshell.presentation)

(defun make-repl-input-state (&key (buffer "") cursor-pos)
  (make-input-state :buffer buffer
                    :cursor-pos (or cursor-pos (length buffer))
                    :abbreviation-expander
                    (lambda (token)
                      (gethash token *abbreviations*))))
