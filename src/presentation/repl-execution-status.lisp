(in-package #:nshell.presentation)

(defun %update-status (code)
  (setf *environment*
        (nshell.domain.environment:env-set
         *environment* "status" (write-to-string code) nil))
  code)
