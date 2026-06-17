;;; REPL input-state helpers
(in-package #:nshell.presentation)

(defun lookup-abbreviation (token)
  (gethash token *abbreviations*))

(defun make-repl-input-state (&key (buffer "") cursor-pos)
  (make-input-state :buffer buffer
                    :cursor-pos (or cursor-pos (length buffer))
                    :abbreviation-expander #'lookup-abbreviation))
