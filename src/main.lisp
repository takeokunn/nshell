(in-package #:nshell)

(defun tty-p ()
  "Return T if standard input is a terminal (interactive mode)."
  #+sbcl (= 1 (sb-unix:unix-isatty 0))
  #-sbcl nil)

(defun %command-line-arguments ()
  "Return the command-line arguments passed to nshell."
  #+sbcl (rest sb-ext:*posix-argv*)
  #-sbcl nil)

(defun %cli-action (arguments)
  "Classify top-level CLI arguments."
  (cond ((or (member "--help" arguments :test #'string=)
             (member "-h" arguments :test #'string=))
         :help)
        ((or (member "--version" arguments :test #'string=)
             (member "-V" arguments :test #'string=))
         :version)
        ((and (= (length arguments) 2)
              (or (string= (first arguments) "-c")
                  (string= (first arguments) "--command")))
         :command)
        ((null arguments)
         :run)
        (t
         :invalid)))

(defun %cli-command (arguments)
  "Return the command string for command mode."
  (second arguments))

(defun %print-usage (&optional (stream *standard-output*))
  (format stream "Usage: nshell [--help] [--version] [-c COMMAND]~%")
  (format stream "~%")
  (format stream "Without arguments, nshell starts an interactive shell when~%")
  (format stream "stdin is a terminal and reads batch input from stdin otherwise.~%")
  (format stream "With -c/--command, nshell executes COMMAND once in batch mode.~%"))

(defun %print-version (&optional (stream *standard-output*))
  (format stream "nshell v0.2.2 - fish-inspired shell in Common Lisp (SBCL ~a)~%"
          (lisp-implementation-version)))

(defun %fatal-error (error)
  (format *error-output* "Fatal error: ~a~%" error)
  1)

(defun main ()
  "Entry point for the nshell binary."
  (let* ((arguments (%command-line-arguments))
         (exit-code
           (handler-case
               (case (%cli-action arguments)
                 (:help
                  (%print-usage)
                  0)
                 (:version
                  (%print-version)
                  0)
                 (:command
                  (nshell.presentation::run-repl-batch
                   :line (%cli-command arguments)))
                 (:invalid
                  (%print-usage *error-output*)
                  1)
                 (:run
                  (if (tty-p)
                      (progn
                        (%print-version)
                        (nshell.presentation:run-repl)
                        0)
                      (nshell.presentation::run-repl-batch))))
             (error (error)
               (%fatal-error error)))))
    (sb-ext:quit :unix-status (or exit-code 0))))
