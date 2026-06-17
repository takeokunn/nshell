(in-package #:nshell)

(defun tty-p ()
  "Return T if standard input is a terminal (interactive mode)."
  #+sbcl (= 1 (sb-unix:unix-isatty 0))
  #-sbcl nil)

(defun main ()
  "Entry point for the nshell binary."
  (let ((exit-code
          (if (tty-p)
              (progn
                (format t "nshell v0.1.0 - fish-inspired shell in Common Lisp (SBCL ~a)~%"
                        (lisp-implementation-version))
                (handler-case
                    (progn
                      (nshell.presentation:run-repl)
                      0)
                  (error (e)
                    (format *error-output* "Fatal error: ~a~%" e)
                    (sb-ext:exit :code 1))))
              (handler-case
                  (nshell.presentation::run-repl-batch)
                (error (e)
                  (format *error-output* "Fatal error: ~a~%" e)
                  (sb-ext:exit :code 1))))))
    (sb-ext:quit :unix-status (or exit-code 0))))
