;;; REPL parse diagnostics
(in-package #:nshell.presentation)

(defun format-parse-diagnostic (diagnostic)
  (format nil "~a at column ~d"
          (nshell.domain.parsing:parse-diagnostic-message diagnostic)
          (1+ (nshell.domain.parsing:parse-diagnostic-start diagnostic))))

(defun format-parse-diagnostic-lines (result)
  (mapcar (lambda (diagnostic)
            (format nil "nshell: syntax error: ~a"
                    (format-parse-diagnostic diagnostic)))
          (nshell.domain.parsing:parse-errors result)))

(defun report-parse-diagnostics (result &optional (stream *error-output*))
  (dolist (line (format-parse-diagnostic-lines result))
    (format stream "~a~%" line)))
