(in-package #:nshell.domain.execution)

;;; Command value object
(defstruct (command
            (:constructor %make-command (name-str args-list))
            (:conc-name command-))
  "A shell command: an executable name and its arguments.
NAME is the command/program name (string).
ARGS is a list of argument strings."
  (name-str "" :type string :read-only t)
  (args-list nil :type list :read-only t))

(defun make-command (name &optional args)
  "Create a command from NAME and optional ARGS."
  (%make-command name args))

(defun command-name (cmd)
  "Return the command name string."
  (command-name-str cmd))

(defun command-args (cmd)
  "Return the command arguments as a list of strings."
  (command-args-list cmd))

(defun command-to-list (cmd)
  "Convert command to a flat list of strings (name + args)."
  (cons (command-name-str cmd) (command-args-list cmd)))

