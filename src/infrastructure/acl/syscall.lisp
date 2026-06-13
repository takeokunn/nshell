(in-package #:nshell.infrastructure.acl)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-posix))

(defun spawn-command (command)
  (let* ((args (nshell.domain.execution:command-args command))
         (cmd-name (nshell.domain.execution:command-name command)))
    (sb-ext:run-program cmd-name args :output :stream :error :output :wait nil :search t)))

(defun run-external (cmd args)
  "Run an external command, print its output, return exit code."
  (handler-case
      (let ((proc (sb-ext:run-program cmd args
                    :output :stream :error :output :wait t :search t)))
        (when proc
          (let ((out (sb-ext:process-output proc)))
            (when out
              (loop for line = (read-line out nil nil)
                    while line do (write-line line))))
          (sb-ext:process-exit-code proc)))
    (error (err)
      (format *error-output* "nshell: ~a: ~a~%" cmd err)
      1)))

(defun spawn-pipeline (commands)
  "Execute commands sequentially."
  (let ((exit 0))
    (dolist (cmd-node commands)
      (let ((cmd (nshell.domain.parsing:command-node-command cmd-node))
            (args (nshell.domain.parsing:command-node-args cmd-node)))
        (setf exit (run-external cmd args))))
    exit))

(defun wait-job (process)
  (sb-ext:process-wait process)
  (sb-ext:process-exit-code process))

(defun kill-process (process signal)
  (declare (ignore signal))
  (sb-ext:process-kill process sb-posix:sigterm 0))
