(in-package #:nshell.application)

(defun execute-command-line (line history dispatcher)
  (declare (ignore dispatcher))
  (let ((result (nshell.domain.parsing:parse-command-line line)))
    (when (nshell.domain.parsing:parse-complete-p result)
      (nshell.domain.history:history-add history line)
      (let ((ast (nshell.domain.parsing:parse-result-ast result)))
        (values ast result)))))

(defun execute-pipeline (pipeline-ast)
  "Execute a pipeline AST, piping stdout of each command to stdin of the next.
Uses sb-ext:run-program with :input/:output stream connections."
  (let ((commands (if (nshell.domain.parsing:pipeline-node-p pipeline-ast)
                      (nshell.domain.parsing:pipeline-node-commands pipeline-ast)
                      (list pipeline-ast))))
    (when commands
      (execute-pipeline-commands commands))))

(defun execute-pipeline-commands (commands)
  "Execute a list of command-nodes as a pipeline."
  (let ((prev-output nil)
        (processes '())
        (last-exit 0))
    (dolist (cmd-node commands)
      (let* ((cmd (nshell.domain.parsing:command-node-command cmd-node))
             (args (nshell.domain.parsing:command-node-args cmd-node))
             (proc (handler-case
                       (sb-ext:run-program cmd args
                         :input (if prev-output :stream :t)
                         :output :stream
                         :error :output
                         :wait nil)
                     (error (err)
                       (format *error-output* "nshell: ~a: ~a~%" cmd err)
                       (return-from execute-pipeline-commands 1)))))
        (when prev-output
          ;; Read from previous output and feed to current input
          (let ((input-stream (sb-ext:process-input proc)))
            (handler-case
                (loop for line = (read-line prev-output nil nil)
                      while line
                      do (write-line line input-stream))
              (error ()))))
        (push proc processes)
        (setf prev-output (sb-ext:process-output proc))))
    ;; Wait for all processes and get exit code
    (dolist (proc (reverse processes))
      (sb-ext:process-wait proc)
      (setf last-exit (sb-ext:process-exit-code proc)))
    last-exit))

(defun execute-pipeline-use-case (pipeline dispatcher)
  (declare (ignore dispatcher))
  (execute-pipeline pipeline))
