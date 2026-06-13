(in-package #:nshell.application)

(defun execute-command-line (line history dispatcher)
  (declare (ignore dispatcher))
  (let ((result (nshell.domain.parsing:parse-command-line line)))
    (when (nshell.domain.parsing:parse-complete-p result)
      (nshell.domain.history:history-add history line)
      (let ((ast (nshell.domain.parsing:parse-result-ast result)))
        (values ast result)))))

(defun execute-pipeline (pipeline-ast)
  "Execute a pipeline AST via infrastructure ACL."
  (let ((commands (if (nshell.domain.parsing:pipeline-node-p pipeline-ast)
                      (nshell.domain.parsing:pipeline-node-commands pipeline-ast)
                      (list pipeline-ast))))
    (if (null (cdr commands))
        (let ((cmd-node (first commands)))
          (nshell.infrastructure.acl:run-external
           (nshell.domain.parsing:command-node-command cmd-node)
           (nshell.domain.parsing:command-node-args cmd-node)))
        (nshell.infrastructure.acl:spawn-pipeline commands))))

(defun execute-pipeline-use-case (pipeline dispatcher)
  (declare (ignore dispatcher))
  (execute-pipeline pipeline))
