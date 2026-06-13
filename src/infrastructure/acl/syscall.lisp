(in-package #:nshell.infrastructure.acl)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-posix))

(defun spawn-command (command)
  (let* ((args (nshell.domain.execution:command-args command))
         (cmd-name (nshell.domain.execution:command-name command))
         (all-args (cons cmd-name args)))
    (sb-ext:run-program (first all-args) (rest all-args)
                        :output :stream :error :stream :wait nil)))

(defun spawn-pipeline (pipeline)
  (let ((cmds (nshell.domain.execution:pipeline-commands pipeline)))
    (dolist (cmd cmds)
      (spawn-command cmd))))

(defun wait-job (process)
  (sb-ext:process-wait process)
  (sb-ext:process-exit-code process))

(defun kill-process (process signal)
  (declare (ignore signal))
  (sb-ext:process-kill process sb-posix:sigterm 0))
