(in-package #:nshell.presentation)
(defun render-completions (candidates)
  (when candidates
    (format t "~%~{~a  ~}" (mapcar #'nshell.domain.completion:candidate-text (subseq candidates 0 (min 8 (length candidates)))))
    (format t "~%")))
(defun cycle-completion (candidates current)
  (declare (ignore candidates current)) 0)
(defun apply-completion (input candidate)
  (concatenate 'string (nshell.domain.completion:candidate-text candidate)))
