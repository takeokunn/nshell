(in-package #:nshell.presentation)

(defun execute-builtin (ast)
  (let* ((cmd (nshell.domain.parsing:command-node-command ast))
         (args (nshell.domain.parsing:command-node-arg-values ast))
         (handler (nshell.application:lookup-builtin cmd)))
    (if handler
        (let ((context (%make-repl-shell-context)))
          (when (%reap-before-builtin-p cmd)
            (reap-background-jobs))
          (multiple-value-bind (output code) (funcall handler context args)
            (%sync-repl-shell-context context code)
            (when output
              (write-string output))
            (values t code)))
        (values nil nil))))

(defun execute-command-node (ast)
  (let* ((expanded-ast (nshell.application:expand-command-alias-node
                        ast
                        *aliases*))
         (cmd (nshell.domain.parsing:command-node-command expanded-ast))
         (context (%make-repl-shell-context)))
    (when (%reap-before-builtin-p cmd)
      (reap-background-jobs))
    (multiple-value-bind (output code)
        (nshell.application:execute-command-node-in-context context expanded-ast)
      (%sync-repl-shell-context context (or code 0))
      (when output
        (write-string output))
      (or code 0))))
