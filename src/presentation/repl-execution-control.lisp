(in-package #:nshell.presentation)

(defun execute-ast-list (nodes)
  (let ((code 0))
    (dolist (node nodes code)
      (setf code (%update-status (or (execute-ast node) 0))))))

(defun execute-if-node (ast)
  (if (= 0 (or (execute-ast (nshell.domain.parsing:if-node-condition ast)) 0))
      (execute-ast-list (nshell.domain.parsing:if-node-then-branch ast))
      (when (nshell.domain.parsing:if-node-else-branch ast)
        (execute-ast-list (nshell.domain.parsing:if-node-else-branch ast)))))

(defun execute-for-node (ast)
  (let* ((var-name (nshell.domain.parsing:for-node-var-name ast))
         (in-values (expand-arg-list
                     (nshell.domain.parsing:for-node-in-values ast)))
         (body (nshell.domain.parsing:for-node-body ast))
         (code 0))
    (dolist (value in-values)
      (setf *environment*
            (nshell.domain.environment:env-set
             *environment* var-name value nil))
      (setf code (execute-ast-list body)))
    code))

(defun execute-while-node (ast)
  (let ((code 0))
    (loop while (= 0 (or (execute-ast (nshell.domain.parsing:while-node-condition ast)) 0))
          do (setf code (execute-ast-list (nshell.domain.parsing:while-node-body ast))))
    code))

(defun execute-case-node (ast)
  (let* ((raw-value (nshell.domain.parsing:case-node-value ast))
         (expanded (expand-arg-list (list raw-value)))
         (value (or (first expanded) raw-value)))
    (loop for clause in (nshell.domain.parsing:case-node-clauses ast)
          for pattern = (car clause)
          when (or (string= pattern "*") (string= pattern value))
            do (return (execute-ast-list (cdr clause)))
          finally (return 0))))
