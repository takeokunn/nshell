(in-package #:nshell.application)

(defun execute-command-line (line history dispatcher)
  (declare (ignore dispatcher))
  (let ((result (nshell.domain.parsing:parse-command-line line)))
    (when (nshell.domain.parsing:parse-complete-p result)
      (nshell.domain.history:history-add history line)
      (let ((ast (nshell.domain.parsing:parse-result-ast result)))
        (values ast result)))))

(defun %ast-command->domain-command (cmd-node)
  (nshell.domain.execution:make-command
   (nshell.domain.parsing:command-node-command cmd-node)
   (mapcar #'nshell.domain.parsing:arg-value
           (nshell.domain.parsing:command-node-args cmd-node))))

(defun %ast->pipeline (pipeline-ast)
  (let ((commands (if (nshell.domain.parsing:pipeline-node-p pipeline-ast)
                      (nshell.domain.parsing:pipeline-node-commands pipeline-ast)
                      (list pipeline-ast))))
    (apply #'nshell.domain.execution:make-pipeline
           (mapcar #'%ast-command->domain-command commands))))

(defun %read-stream-to-string (stream)
  (with-output-to-string (out)
    (loop for char = (read-char stream nil nil)
          while char
          do (write-char char out))))

(defun %run-stage-process (command input)
  (let* ((stdin (if input (make-string-input-stream input) *standard-input*))
         (proc (handler-case
                   (sb-ext:run-program
                    (nshell.domain.execution:command-name command)
                    (nshell.domain.execution:command-args command)
                    :input stdin
                    :output :stream
                    :error :output
                    :wait nil
                    :search t)
                 (error (err)
                   (format *error-output* "nshell: ~a: ~a~%"
                           (nshell.domain.execution:command-name command) err)
                   nil))))
    (if proc
        (progn
          (ignore-errors
            (let ((pid (sb-ext:process-pid proc)))
              (when (plusp pid)
                (nshell.infrastructure.acl:set-process-group pid pid))))
          (let ((output (%read-stream-to-string (sb-ext:process-output proc))))
            (sb-ext:process-wait proc)
            (values (or (sb-ext:process-exit-code proc) 0) output)))
        (values 127 ""))))

(defun execute-pipeline-cps (plan kont)
  "Return a continuation that executes PLAN stage-by-stage, then calls KONT.

KONT is called as (KONT EXIT-CODE OUTPUT) after the final stage completes. Each
stage continuation captures its upstream output and returns the next stage
continuation."
  (labels ((stage-cont (stages input last-exit)
             (lambda ()
               (if (null stages)
                   (funcall kont last-exit (or input ""))
                   (let* ((stage (first stages))
                          (command (nshell.domain.execution:pipeline-stage-command stage)))
                     (multiple-value-bind (exit output)
                         (%run-stage-process command input)
                       (stage-cont (rest stages) output exit)))))))
    (stage-cont (nshell.domain.execution:pipeline-plan-stages plan) nil 0)))

(defun %run-continuation (kont)
  (loop for next = (funcall kont) then (funcall next)
        while (functionp next)
        finally (return next)))

(defun execute-pipeline (pipeline-ast)
  "Execute a pipeline AST using OS-level pipes through the infrastructure layer."
  (if (nshell.domain.parsing:pipeline-node-p pipeline-ast)
      (nshell.infrastructure.acl:spawn-pipeline
       (nshell.domain.parsing:pipeline-node-commands pipeline-ast))
      (let ((commands (list pipeline-ast)))
        (nshell.infrastructure.acl:spawn-pipeline commands))))

(defun execute-pipeline-use-case (pipeline dispatcher)
  (declare (ignore dispatcher))
  (execute-pipeline pipeline))
