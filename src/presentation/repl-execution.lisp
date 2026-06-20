(in-package #:nshell.presentation)

(defun execute-ast (ast)
  (cond
    ((nshell.domain.parsing:sequence-node-p ast)
     (let* ((cmds (nshell.domain.parsing:sequence-node-commands ast))
            (seps (nshell.domain.parsing:sequence-node-separators ast))
            (code 0))
       (loop for cmd in cmds
             for index from 0
             for sep = (and (< index (length seps))
                            (nth index seps))
             do (cond
                  ((eq :amp sep)
                   (%execute-background-ast cmd))
                  (t
                   (setf code (%update-status (or (execute-ast cmd) 0)))
                   (when (or (and (eq :and sep) (/= code 0))
                             (and (eq :or sep) (= code 0)))
                     (return code)))))
       code))
    ((nshell.domain.parsing:command-node-p ast)
     (execute-command-node ast))
    ((or (nshell.domain.parsing:pipeline-node-p ast)
         (nshell.domain.parsing:if-node-p ast)
         (nshell.domain.parsing:for-node-p ast)
         (nshell.domain.parsing:while-node-p ast)
         (nshell.domain.parsing:case-node-p ast)
         (nshell.domain.parsing:begin-end-node-p ast))
     (%execute-foreground-ast-in-context ast))
    (t
     (format t "nshell: cannot execute~%")
     1)))
