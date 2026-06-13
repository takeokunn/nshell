(in-package #:nshell.application)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-posix))

(defun execute-command-line (line history dispatcher)
  (declare (ignore dispatcher))
  (let ((result (nshell.domain.parsing:parse-command-line line)))
    (when (nshell.domain.parsing:parse-complete-p result)
      (nshell.domain.history:history-add history line)
      (let ((ast (nshell.domain.parsing:parse-result-ast result)))
        (values ast result)))))

(defun execute-external (cmd args)
  "Execute an external command, print its output, return exit code."
  (handler-case
      (let ((proc (sb-ext:run-program cmd args
                    :output :stream :error :output :wait t :search t)))
        (when proc
          (let ((out (sb-ext:process-output proc)))
            (when out
              (loop for line = (read-line out nil nil)
                    while line
                    do (write-line line))))
          (sb-ext:process-exit-code proc)))
    (error (err)
      (format *error-output* "nshell: ~a: ~a~%" cmd err)
      1)))

(defun execute-pipeline (pipeline-ast)
  "Execute a pipeline AST."
  (let ((commands (if (nshell.domain.parsing:pipeline-node-p pipeline-ast)
                      (nshell.domain.parsing:pipeline-node-commands pipeline-ast)
                      (list pipeline-ast))))
    (if (null (cdr commands))
        (let ((cmd-node (first commands)))
          (execute-external
           (nshell.domain.parsing:command-node-command cmd-node)
           (nshell.domain.parsing:command-node-args cmd-node)))
        ;; For multi-command pipelines, use sequential execution via temp files
        (let ((last-exit 0))
          (dolist (cmd-node commands)
            (let ((cmd (nshell.domain.parsing:command-node-command cmd-node))
                  (args (nshell.domain.parsing:command-node-args cmd-node)))
              (setf last-exit (execute-external cmd args))))
          last-exit))))

(defun execute-pipeline-use-case (pipeline dispatcher)
  (declare (ignore dispatcher))
  (execute-pipeline pipeline))
