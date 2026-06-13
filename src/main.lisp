(in-package #:nshell)

(defun main ()
  "Entry point for the nshell binary."
  (format t "nshell v0.1.0 - fish-inspired interactive shell in Common Lisp~%")
  (format t "Built with SBCL ~a~%" (lisp-implementation-version))
  (handler-case
      (nshell.presentation:run-repl)
    (error (e)
      (format *error-output* "Fatal error: ~a~%" e)
      (sb-ext:exit :code 1)))
  (sb-ext:exit :code 0))
