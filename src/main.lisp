;;; nshell main entry point
;;; Wave 0: minimal binary that prints banner and exits

(in-package #:nshell)

(defun main ()
  "Entry point for the nshell binary.
Prints version banner and exits cleanly.
Interactive REPL will be added in Wave 7."
  (format t "nshell v0.1.0 - fish-inspired interactive shell in Common Lisp~%")
  (format t "Built with SBCL ~a~%" (lisp-implementation-version))
  (format t "Startup OK. Interactive REPL not yet implemented.~%")
  (sb-ext:exit :code 0))
