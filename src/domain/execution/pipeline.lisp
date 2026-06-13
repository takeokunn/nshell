(in-package #:nshell.domain.execution)

;;; Pipeline value object: cmd1 | cmd2 | cmd3
(defstruct (pipeline
            (:constructor %make-pipeline (commands-list))
            (:conc-name pipeline-))
  "A pipeline of commands connected by pipes (|).
COMMANDS is a list of command structs in execution order."
  (commands-list nil :type list :read-only t))

(defun make-pipeline (&rest commands)
  "Create a pipeline from COMMANDS in execution order."
  (%make-pipeline commands))

(defun pipeline-commands (pipe)
  "Return the list of commands in the pipeline."
  (pipeline-commands-list pipe))

(defun pipeline-single-command-p (pipe)
  "True if pipeline contains exactly one command."
  (= (length (pipeline-commands-list pipe)) 1))

(defun pipeline-empty-p (pipe)
  "True if pipeline has no commands."
  (null (pipeline-commands-list pipe)))

(defun pipeline-length (pipe)
  "Number of commands in the pipeline."
  (length (pipeline-commands-list pipe)))

(export '(pipeline-single-command-p
          pipeline-empty-p
          pipeline-length))
