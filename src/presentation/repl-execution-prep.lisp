;;; REPL command preparation helpers
(in-package #:nshell.presentation)

(defun %reap-before-builtin-p (command)
  (not (null (member command '("fg" "bg" "jobs" "disown") :test #'string=))))

(defun make-job-from-ast (ast text)
  (let* ((node (cond
                 ((or (nshell.domain.parsing:command-node-p ast)
                      (nshell.domain.parsing:pipeline-node-p ast))
                  ast)
                 ((nshell.domain.parsing:sequence-node-p ast)
                  (first (nshell.domain.parsing:sequence-node-commands ast)))))
         (cmds (if (nshell.domain.parsing:pipeline-node-p node)
                   (nshell.domain.parsing:pipeline-node-commands node)
                   (list node)))
         (dom-cmds
           (mapcar (lambda (cmd)
                     (nshell.domain.execution:make-command
                      (nshell.domain.parsing:command-node-command cmd)
                      (nshell.domain.parsing:command-node-arg-values cmd)))
                   cmds))
         (pipe (apply #'nshell.domain.execution:make-pipeline dom-cmds))
         (job (nshell.domain.execution:make-job 0 pipe)))
    (setf (nshell.domain.execution:job-command-line job) text)
    job))

(defun expand-arg-list (args)
  (loop for arg in args
        for value = (if (consp arg) (car arg) arg)
        for quoted-p = (and (consp arg) (cdr arg))
        if quoted-p
          append (list value)
        else
          append (nshell.domain.expansion:expand-all value (ensure-environment))))
