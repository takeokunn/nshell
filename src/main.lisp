(in-package #:nshell)

(defun tty-p ()
  "Return T if standard input is a terminal (interactive mode)."
  #+sbcl (= 1 (sb-unix:unix-isatty 0))
  #-sbcl nil)

(defun main ()
  "Entry point for the nshell binary."
  (if (tty-p)
      (progn
        (format t "nshell v0.1.0 - fish-inspired shell in Common Lisp (SBCL ~a)~%"
                (lisp-implementation-version))
        (handler-case
            (nshell.presentation:run-repl)
          (error (e)
            (format *error-output* "Fatal error: ~a~%" e)
            (sb-ext:exit :code 1))))
      (handler-case
          (nshell.presentation::run-repl-batch)
        (error (e)
          (format *error-output* "Fatal error: ~a~%" e)
          (sb-ext:exit :code 1))))
  (sb-ext:quit :unix-status 0))
